{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Control.Monad    (mapM)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits        (Bits (..))
import           Data.Int         (Int16, Int64)
import           Data.List        (intersperse)
import           Data.Maybe       (isNothing)
import           Data.STRef

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

data VEB s i
    = VNode { vChildren :: STArray s i (VEB s i)
            , vAux      :: STRef s (VEB s i)
            , vMinMax   :: Maybe (STRef s (i, i)) }
    | VLeaf { vMinMax :: Maybe (STRef s (i, i)) }

isEmpty :: (BRep i) => VEB s i -> Bool
isEmpty v = isNothing $ vMinMax v

getMinMax :: ((i,i) -> i) -> VEB s i -> ST s (Maybe i)
getMinMax g = maybe (pure Nothing) (fmap (Just . g) . readSTRef) . vMinMax

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
            checkChildren _            = error "member: doesn't make sense"

        maybe (pure False) (\mm -> readSTRef mm >>= \(mn,mx) ->
                               if e == mn || e == mx
                               then pure True
                               else checkChildren v) $ vMinMax v

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
    printMinMax = maybe (pure "mm0") (\s -> show <$> readSTRef s)
    printGo :: Int -> VEB RealWorld i -> ST RealWorld String
    printGo _ VLeaf {..} = do
        e <- printMinMax vMinMax
        pure $ concat [ "{ leaf ", e, " }" ]
    printGo k VNode {..} = do
        let printChild j = do
                b <- readSTRef vAux >>= \aux -> member aux j
                if b
                    then printGo (k `div` 2) =<< readArray vChildren j
                    else pure "N"
        e1 <- concat . intersperse ", " <$> mapM printChild [0..(fromIntegral k)-1]
        (e2 :: String) <- printGo (k `div` 2) =<< readSTRef vAux
        e3 <- printMinMax vMinMax
        pure $ concat ["{ children: (", e1, "), aux: (", e2, "), mm: ", e3, " }"]

newVEB :: forall i s. BRep i => ST s (VEB s i)
newVEB = newVEBGo $ totalBits @i
  where
    newVEBGo 1 = pure $ VLeaf Nothing
    newVEBGo n = do
        vChildren <- newArray_ (0, fromIntegral n)
        vAux <- newSTRef =<< newVEBGo (n `div` 2)
        let vMinMax = Nothing
        pure $ VNode {..}

{-
insert :: forall a s. (BRep a) => VEB s a -> a -> ST s ()
insert t0 e0 = insertDo (totalBits @a) t0 e0
  where
    insertDo :: Integer -> VEB a -> a -> IO ()
    insertDo s t e
      | isEmpty t = writeIORef (vMinMax t) (e,e)
      | otherwise = do
            (tmin,tmax) <- readIORef $ vMinMax t
            if tmin == tmax
            then writeIORef (vMinMax t) $ bool (tmin,e) (e,tmax) (tmin < e)
            else do let newmin = bool tmin e (tmin > e)
                    let newmax = bool tmax e (tmax < e)
                    writeIORef (vMinMax t) (newmin,newmax)
                    let ehigh = takeHigh @a s
                    let elow = takeLow @a s
                    let s' = s `div` 2
                    child <- readIORef (vChildren t !! (fromIntegral ehigh))
                    when (isEmpty child) $
                        readIORef (vAux t) >>= (\aux -> insertDo s' aux ehigh)
                    insertDo (s `div` 2) child elow

-}

main :: IO ()
main = do
    !x <- stToIO (newVEB @Int16)
    printVeb x
