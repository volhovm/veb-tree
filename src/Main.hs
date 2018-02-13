{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Control.Monad    (forM, forM_, mapM, replicateM, replicateM_, unless,
                                   when)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits        (Bits (..))
import           Data.Bool        (bool)
import           Data.Int         (Int16, Int64, Int8)
import           Data.List        (intersperse, nub, sort)
import           Data.Maybe       (isNothing)
import           Data.STRef
import           Data.Word
import           Debug.Trace      (trace, traceShow, traceShowM)
import           System.Random

-- could be Word8 instead of Int here, it's used for depth only
class (Ord i, Integral i, Ix i) => BRep i where
    -- | Return total number of bits.
    totalBits :: Int
    -- | Take high as we're k bit number.
    takeHigh :: Int -> i -> i
    -- | Take low as we're k bit number.
    takeLow :: Int -> i -> i
    -- | Create a k bit number from k/2 high and low
    fromHighLow :: Int -> i -> i -> i

    default takeHigh :: Bits i => Int -> i -> i
    takeHigh k x = x `shiftR` (k `div` 2)
    default takeLow :: Bits i => Int -> i -> i
    takeLow k x = x .&. ((2^k-1) `shiftR` (k`div`2))
    default fromHighLow :: Bits i => Int -> i -> i -> i
    fromHighLow k h l = l .|. (h `shiftL` (k`div`2))

instance BRep Int64 where
    totalBits = 64

instance BRep Int16 where
    totalBits = 16

instance BRep Int8 where
    totalBits = 8

instance BRep Word8 where
    totalBits = 8

-- for testing
newtype Int4 = Int4 Int16 deriving (Eq,Ord,Real,Num,Enum,Integral,Bits,Ix)
instance Show Int4 where
    show (Int4 x) = show x
instance BRep Int4 where
    totalBits = 4

data VEB s i
    = VNode { vChildren :: STArray s i (VEB s i)
            , vAux      :: STRef s (VEB s i)
            , vMinMax   :: STRef s (Maybe (i, i)) }
    | VLeaf { vMinMax :: STRef s (Maybe (i, i)) }

isEmpty :: (BRep i) => VEB s i -> ST s Bool
isEmpty v = isNothing <$> readSTRef (vMinMax v)

getMinMax :: ((i,i) -> i) -> VEB s i -> ST s (Maybe i)
getMinMax g v = fmap g <$> readSTRef (vMinMax v)

getMin, getMax :: VEB s i -> ST s (Maybe i)
getMin = getMinMax fst
getMax = getMinMax snd

member :: forall s i. (BRep i) => VEB s i -> i -> ST s Bool
member = memberGo (totalBits @i)

memberGo :: forall s i. (BRep i) => Int -> VEB s i -> i -> ST s Bool
memberGo 0 _ _ = error "memberGo 0"
memberGo k v e = do
    let checkChildren (VNode {..}) = do
            aux <- readSTRef vAux
            let h = takeHigh k e
            let k' = k `div` 2
            b <- memberGo k' aux h
            if b then do vc <- readArray vChildren h
                         memberGo (k `div` 2) vc (takeLow k e)
                 else pure False -- child doesn't even exist
        checkChildren _            = pure False

    mm <- readSTRef (vMinMax v)
    maybe (pure False) (\(mn,mx) -> if e == mn || e == mx
                                    then pure True
                                    else checkChildren v) mm

toList :: forall i s. (BRep i) => VEB s i -> ST s [i]
toList = toListK (totalBits @i)
  where
    toListMM :: STRef s (Maybe (i,i)) -> ST s [i]
    toListMM s = maybe ([]) (\(a,b) -> nub [a,b]) <$> readSTRef s

    toListK :: Int -> VEB s i -> ST s [i]
    toListK 0 = error "toListK 0"
    toListK k = \case
        VLeaf {..} -> toListMM vMinMax
        VNode {..} -> do
            auxFlat <- toListK (k `div` 2) =<< readSTRef vAux
            (childrenFlat :: [i]) <-
                fmap concat $ forM auxFlat $ \iHigh -> do
                    c <- readArray vChildren iHigh
                    -- C♭, hehe
                    (cFlat :: [i]) <- toListK (k `div` 2) c
                    pure $ map (fromHighLow k iHigh) cFlat
            mmList <- toListMM vMinMax
            pure $ childrenFlat ++ mmList

printVeb :: forall i. (BRep i, Show i) => VEB RealWorld i -> IO ()
printVeb v0 = putStrLn =<< stToIO (printGo (totalBits @i) v0)
  where
    printMinMax s = maybe "\"mm0\"" (show . show) <$> readSTRef s
    printGo :: Int -> VEB RealWorld i -> ST RealWorld String
    printGo _ VLeaf {..} = do
        e <- printMinMax vMinMax
        pure $ concat [ "{ \"leaf\": ", e, " }" ]
    printGo k VNode {..} = do
        let printChild j = do
                b <- readSTRef vAux >>= \aux -> memberGo (k `div` 2) aux j
                if b
                    then printGo (k `div` 2) =<< readArray vChildren j
                    else pure "null"
        e1 <- concat . intersperse ", " <$> mapM printChild [0..(fromIntegral k)-1]
        (e2 :: String) <- printGo (k `div` 2) =<< readSTRef vAux
        e3 <- printMinMax vMinMax
        pure $ concat ["{ \"children\": [", e1, "], \"aux\": ", e2 ,", \"mm\": ", e3, "}"]

newVEB :: forall i s. BRep i => ST s (VEB s i)
newVEB = newVEBGo $ totalBits @i

newVEBGo :: BRep i => Int -> ST s (VEB s i)
newVEBGo i | i <= 0 = error "newVEBGo"
newVEBGo 1 = VLeaf <$> newSTRef Nothing
newVEBGo n = do
    vChildren <- newArray_ (0, fromIntegral n)
    vAux <- newSTRef =<< newVEBGo (n `div` 2)
    vMinMax <- newSTRef Nothing
    pure $ VNode {..}

insert :: forall i s. (BRep i, Show i) => VEB s i -> i -> ST s ()
insert v00 e00 = insertDo (totalBits @i) v00 e00
  where
    insertDo :: Int -> VEB s i -> i -> ST s ()
    insertDo 0 _v _e0 = error "insertDo 0"
    insertDo k v e0 = do
        let onEmptyMM = writeSTRef (vMinMax v) (Just (e0,e0))
            onFullMM (tmin,tmax) | e0 == tmin || e0 == tmax = pure ()
            onFullMM (tmin,tmax) | tmin == tmax = do
                let [newmin,newmax] = sort [tmin, e0]
                writeSTRef (vMinMax v) $ Just (newmin,newmax)
            onFullMM (tmin,tmax) = do
                let [newmin,e,newmax] = sort [tmin, tmax, e0]
                writeSTRef (vMinMax v) $ Just (newmin,newmax)
                let ehigh = takeHigh k e
                let elow = takeLow k e
                let k' = k `div` 2
                case v of
                    VLeaf{} -> pure ()
                    VNode{..} -> do
                        aux <- readSTRef vAux
                        childExists <- memberGo k' aux ehigh
                        if childExists
                           then readArray vChildren ehigh >>= \v' -> insertDo k' v' elow
                           else do
                               insertDo k' aux ehigh
                               newTree <- newVEBGo k'
                               insertDo k' newTree elow
                               writeArray vChildren ehigh newTree

        maybe onEmptyMM onFullMM =<< readSTRef (vMinMax v)

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

testInt4 :: IO ()
testInt4 = replicateM_ 100000 $ do
    (iter :: Int) <- randomRIO (0, 20)
    toInsert <- nub . map Int4 <$> replicateM iter (randomRIO (0,15))
    v <- stToIO $ do
        v <- newVEB @Int4
        mapM_ (insert v) toInsert
        pure v
    forM_ [0..15] $ \i -> do
        m <- stToIO (member v i)
        when (m && (not $ i `elem` toInsert)) $ do
            print $ sort $ toInsert
            print i
            error "A"
        when (not m && (i `elem` toInsert)) $ do
            print $ sort $ toInsert
            print i
            printVeb v
            error "B"
    asList <- stToIO (toList v)
    unless (sort asList == sort toInsert) $ do
        print $ sort $ toInsert
        print asList
        print toInsert
        printVeb v
        error "C"

testBug = do
    let toInsert = [1,5,10,14,15]
    v <- stToIO $ do
        v <- newVEB @Int4
        mapM_ (insert v) toInsert
        pure v
    printVeb v
    print =<< stToIO (toList v)
    forM_ [0..15] $ \i -> do
        print =<< stToIO (member v i)

-- debug this
testRandom :: forall i. (BRep i, Random i, Show i, Bounded i) => IO ()
testRandom = do
    (iter :: Int) <- randomRIO (0, 400)
    vals <- nub <$> replicateM iter (randomRIO (0, maxBound @i))
    putStrLn "Generated, inserting"
    v <- stToIO $ do
        v <- newVEB @i
        mapM_ (insert v) vals
        pure v
    putStrLn "Inserted, checking"
    r <- stToIO $ fmap and $ forM [0..maxBound @i] $ member v
    print r

main :: IO ()
main = testRandom @Word8

{-
{ "children": [ { "children": [null, null], "mm": "(3,3)"}
              , { "children": [null, null], "mm": "(0,0)"}
              , null, null]
, "mm": "(0,7)"}


{ "children": [ { "children": [null, { "leaf": "(0,0)" }]
                , "aux": { "leaf": "(1,1)" }
                , "mm": "(1,3)"}
              , { "children": [null, null]
                , "aux": { "leaf": "mm0" }
                , "mm": "(1,1)"}
              , null
              , { "children": [null, null]
                , "aux": { "leaf": "mm0" }
                , "mm": "(2,2)"}
              ]
, "aux": { "children": [{ "leaf": "(1,1)" }, null]
         , "aux": { "leaf": "(0,0)" }
         , "mm": "(0,3)"}
, "mm": "(0,15)"}


Doesn't have 10 but must
[1,5,10,14,15]
10

{ "children": [ null
              , { "children": [null, null]
                , "aux": { "leaf": "mm0" }
                , "mm": "(1,1)"}
              , null
              , { "children": [null, null]
                , "aux": { "leaf": "mm0" }
                , "mm": "(2,2)"}
              ]
, "aux": { "children": [null, { "leaf": "(0,0)" }]
         , "aux": { "leaf": "(1,1)" }
         , "mm": "(1,3)"}
, "mm": "(1,15)"}

POSTFIX:

{ "children": [ null
              , { "children": [null, null], "aux": { "leaf": "mm0" }, "mm": "(1,1)"}
              , { "children": [null, null], "aux": { "leaf": "mm0" }, "mm": "(2,2)"}
              , { "children": [null, null], "aux": { "leaf": "mm0" }, "mm": "(2,2)"}]
, "aux": { "children": [null, { "leaf": "(0,0)" }]
         , "aux": { "leaf": "(1,1)" }
         , "mm": "(1,3)"}
, "mm": "(1,15)"
}

-}
