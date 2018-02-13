{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Control.Monad    (forM, forM_, mapM, replicateM, replicateM_, unless,
                                   when)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits        (Bits (..), FiniteBits (..))
import           Data.Int
import           Data.List        (intersperse, nub, sort)
import           Data.Maybe       (isNothing)
import           Data.STRef
import           Data.Word
import           System.Random

----------------------------------------------------------------------------
-- BRep
----------------------------------------------------------------------------

-- could be Word8 instead of Int here, it's used for depth only
class (Show i, Ord i, Integral i, Ix i) => BRep i where
    -- | Return total number of bits.
    totalBits :: Int
    -- | Take high as we're k bit number.
    takeHigh :: Int -> i -> i
    -- | Take low as we're k bit number.
    takeLow :: Int -> i -> i
    -- | Create a k bit number from k/2 high and low
    fromHighLow :: Int -> i -> i -> i

    default totalBits :: FiniteBits i => Int
    totalBits = finiteBitSize (0 :: i)
    default takeHigh :: Bits i => Int -> i -> i
    takeHigh k x = x `shiftR` (k `div` 2)
    default takeLow :: Bits i => Int -> i -> i
    takeLow k x = x .&. (2^(k`div`2)-1)
    default fromHighLow :: Bits i => Int -> i -> i -> i
    fromHighLow k h l = l .|. (h `shiftL` (k`div`2))

instance BRep Int8 where
instance BRep Int16 where
instance BRep Int32 where
instance BRep Int64 where
instance BRep Word8 where
instance BRep Word16 where

-- for testing
newtype Int4 = Int4 Int16 deriving (Eq,Ord,Real,Num,Enum,Integral,Bits,Ix)
instance Show Int4 where
    show (Int4 x) = show x
instance BRep Int4 where
    totalBits = 4

-- :thinking_face:
testBRep :: forall i. (BRep i, Random i, Show i) => IO ()
testBRep = replicateM_ 100000 $ do
    let b = totalBits @i
    let log2 :: Int -> Int
        log2 (fromIntegral -> x) = round $ (log x :: Double) / log 2
    kp <- randomRIO (0,log2 b)
    let k = 2 ^ kp
    x <- randomIO @i
    let h = takeHigh k x
    let l = takeLow k x
    let x2 = fromHighLow k h l
    unless (x2 == x) $ error $ "testBRep: " ++ show k ++ " " ++ show x

    let hb = takeHigh b x
    let lb = takeLow b x
    let twoP = 2^(b-1)
    when (lb > twoP) $ error $ "low > twoP: " ++ show b ++ " " ++ show x ++ " " ++ show lb
    when (hb > twoP) $ error $ "high > twoP: " ++ show b ++ " " ++ show x ++ " " ++ show hb

----------------------------------------------------------------------------
-- VEB
----------------------------------------------------------------------------

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
member = memberK (totalBits @i)

memberK :: forall s i. (BRep i) => Int -> VEB s i -> i -> ST s Bool
memberK 0 _ _ = error "memberK 0"
memberK k v e = do
    let checkChildren (VNode {..}) = do
            aux <- readSTRef vAux
            let h = takeHigh k e
            let k' = k `div` 2
            b <- memberK k' aux h
            if b then do vc <- readArray vChildren h
                         memberK (k `div` 2) vc (takeLow k e)
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
                b <- readSTRef vAux >>= \aux -> memberK (k `div` 2) aux j
                if b
                    then printGo (k `div` 2) =<< readArray vChildren j
                    else pure "null"
        e1 <- concat . intersperse ", " <$> mapM printChild [0..(fromIntegral k)-1]
        (e2 :: String) <- printGo (k `div` 2) =<< readSTRef vAux
        e3 <- printMinMax vMinMax
        pure $ concat ["{ \"children\": [", e1, "], \"aux\": ", e2 ,", \"mm\": ", e3, "}"]

newVEB :: forall i s. BRep i => ST s (VEB s i)
newVEB = newVEBK $ totalBits @i

-- | Creates k-tree
newVEBK :: forall i s. BRep i => Int -> ST s (VEB s i)
newVEBK i | i <= 0 = error "newVEBK"
newVEBK 1 = VLeaf <$> newSTRef Nothing
newVEBK k = do
    when (odd k) $ error $ "NewVebGo " ++ show k
    vChildren <- newArray_ (0, 2 ^ (fromIntegral k `div` (2 :: Int)) - 1)
    vAux <- newSTRef =<< newVEBK (k `div` 2)
    vMinMax <- newSTRef Nothing
    pure $ VNode {..}

insert :: forall i s. BRep i => VEB s i -> i -> ST s ()
insert v00 e00 = insertK (totalBits @i) v00 e00
  where
    insertK :: Int -> VEB s i -> i -> ST s ()
    insertK 0 _v _e0 = error "insertK 0"
    insertK k v e0 = do
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
                        childExists <- memberK k' aux ehigh
                        if childExists
                           then readArray vChildren ehigh >>= \v' -> insertK k' v' elow
                           else do
                               insertK k' aux ehigh
                               newTree <- newVEBK k'
                               insertK k' newTree elow
                               writeArray vChildren ehigh newTree

        maybe onEmptyMM onFullMM =<< readSTRef (vMinMax v)

fromList :: forall i s. BRep i => [i] -> ST s (VEB s i)
fromList toInsert = do
    v <- newVEB @i
    mapM_ (insert v) toInsert
    pure v

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

testInt4 :: IO ()
testInt4 = replicateM_ 10000 $ do
    (iter :: Int) <- randomRIO (0, 20)
    toInsert <- nub . map Int4 <$> replicateM iter (randomRIO (0,15))
    v <- stToIO $ fromList toInsert
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
        print $ sort toInsert
        print $ sort asList
        printVeb v
        error "C"

testBug :: IO ()
testBug = do
    v <- stToIO $ fromList [38::Int8,92,64,79,77]
    printVeb v
    print =<< stToIO (toList v)

-- debug this
testRandom :: forall i. (BRep i, Random i, Show i, Bounded i) => IO ()
testRandom = replicateM_ 20 $ do
    (iter :: Int) <- randomRIO (0, 20000)
    vals <- nub <$> replicateM iter (randomRIO (0, maxBound @i))
    !v <- stToIO $ fromList vals

    forM_ vals $ \i -> do
        m <- stToIO (member v i)
        unless m $ do
            print $ sort vals
            print i
            printVeb v
            error "B"

--    !v2 <- stToIO $ toList v
--    when (sort v2 /= sort vals) $ do
--        print $ sort vals
--        print $ sort v2
--        printVeb v
--        error "testRandom"

main :: IO ()
main = do
    putStrLn "word8"
    testRandom @Word8
    putStrLn "word16"
    testRandom @Word16
    putStrLn "int8"
    testRandom @Int8
    putStrLn "int16"
    testRandom @Int16
    putStrLn "int32"
    testRandom @Int32
    putStrLn "int64"
    testRandom @Int64
