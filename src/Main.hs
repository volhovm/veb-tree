{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Control.Monad    (forM, forM_, mapM, replicateM)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits        (Bits (..))
import           Data.Bool        (bool)
import           Data.Int         (Int16, Int64)
import           Data.List        (intersperse, nub)
import           Data.Maybe       (isNothing)
import           Data.STRef
import           System.Random

-- could be Word8 instead of Int here, it's used for depth only
class (Ord i, Integral i, Ix i) => BRep i where
    -- | Return total number of bits.
    totalBits :: Int
    -- | Take high as we're k bit number.
    takeHigh :: Int -> i -> i
    -- | Take low as we're k bit number.
    takeLow :: Int -> i -> i

    default takeHigh :: Bits i => Int -> i -> i
    takeHigh k x = x `shiftR` (k `div` 2)
    default takeLow :: Bits i => Int -> i -> i
    takeLow k x = x .&. ((2^k-1) `shiftR` (k`div`2))

instance BRep Int64 where
    totalBits = 64

instance BRep Int16 where
    totalBits = 16

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
  where
    memberGo :: Int -> VEB s i -> i -> ST s Bool
    memberGo k v e = do
        let checkChildren (VNode {..}) =
                getChild v (takeHigh k e) >>= \case
                    Nothing -> pure False -- child doesn't even exist
                    Just vc -> memberGo (k `div` 2) vc (takeLow k e)
            checkChildren _            = pure False

        mm <- readSTRef (vMinMax v)
        maybe (pure False) (\(mn,mx) -> if e == mn || e == mx
                                        then pure True
                                        else checkChildren v) mm

-- inline
getChild :: (BRep i) => VEB s i -> i -> ST s (Maybe (VEB s i))
getChild VLeaf{} _ = error "getChild leaf"
getChild VNode{..} j = do
    b <- readSTRef vAux >>= \aux -> member aux j
    if b
        then Just <$> readArray vChildren j
        else pure Nothing

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
                b <- readSTRef vAux >>= \aux -> member aux j
                if b
                    then printGo (k `div` 2) =<< readArray vChildren j
                    else pure "null"
        e1 <- concat . intersperse ", " <$> mapM printChild [0..(fromIntegral k)-1]
        --(e2 :: String) <- printGo (k `div` 2) =<< readSTRef vAux
        e3 <- printMinMax vMinMax
        pure $ concat ["{ \"children\": [", e1, "], \"mm\": ", e3, "}"]

newVEB :: forall i s. BRep i => ST s (VEB s i)
newVEB = newVEBGo $ totalBits @i

newVEBGo :: BRep i => Int -> ST s (VEB s i)
newVEBGo 1 = VLeaf <$> newSTRef Nothing
newVEBGo n = do
    vChildren <- newArray_ (0, fromIntegral n)
    vAux <- newSTRef =<< newVEBGo (n `div` 2)
    vMinMax <- newSTRef Nothing
    pure $ VNode {..}

insert :: forall i s. (BRep i) => VEB s i -> i -> ST s ()
insert = insertDo (totalBits @i)
  where
    insertDo :: Int -> VEB s i -> i -> ST s ()
    insertDo k v e = do
        let onEmptyMM = writeSTRef (vMinMax v) (Just (e,e))
            onFullMM (tmin,tmax) = do
                let newmin = bool tmin e (tmin > e)
                let newmax = bool tmax e (tmax < e)
                writeSTRef (vMinMax v) $ Just (newmin,newmax)
                let ehigh = takeHigh k e
                let elow = takeLow k e
                let k' = k `div` 2
                case v of
                    VLeaf{} -> pure ()
                    VNode{..} -> do
                        aux <- readSTRef vAux
                        childExists <- member aux ehigh
                        if childExists
                           then readArray vChildren ehigh >>= \v' -> insertDo k' v' elow
                           else do
                               insertDo k' aux ehigh
                               newTree <- newVEBGo k'
                               insertDo k' newTree elow
                               writeArray vChildren ehigh newTree

        maybe onEmptyMM onFullMM =<< readSTRef (vMinMax v)

testInt4 :: IO ()
testInt4 = do
    v <- stToIO $ do
        v <- newVEB @Int4
        mapM_ (insert v) [0, 1, 2, 3, 5, 14, 15]
        pure v
    printVeb v
    forM_ [0..15] $ \i -> do
        putStr (show i ++ ": ")
        print =<< stToIO (member v i)

-- debug this
testInt16 :: IO ()
testInt16 = do
    (iter :: Int) <- randomRIO (200, 400)
    vals <- nub <$> replicateM iter (randomIO @Int16)
    putStrLn "kek"
    v <- stToIO $ do
        v <- newVEB @Int16
        mapM_ (insert v) vals
        pure v
    putStrLn "mda"
    r <- stToIO $ fmap and $ forM [0..maxBound @Int16] $ member v
    print r

main :: IO ()
main = testInt16
