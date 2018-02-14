{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Data.VEB.Internal where

import Control.Monad (forM, mapM, void, when)
import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.Array.ST (Ix, STArray, newArray_, readArray, writeArray)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.List (intersperse, nub, sort)
import Data.Maybe (fromMaybe, isNothing)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word16, Word32, Word64, Word8)

----------------------------------------------------------------------------
-- BRep
----------------------------------------------------------------------------

-- could be Word8 instead of Int here, it's used for depth only
-- | Types that can be used as elements of 'VEB'.
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
    default takeLow :: Bits i => Int -> i -> i
    takeLow k x = x .&. (2^(k`div`2)-1)
    default fromHighLow :: Bits i => Int -> i -> i -> i
    fromHighLow k h l = l .|. (h `shiftL` (k`div`2))

instance BRep Word8 where
    takeHigh k x = x `shiftR` (k `div` 2)
instance BRep Word16 where
    takeHigh k x = x `shiftR` (k `div` 2)
instance BRep Word32 where
    takeHigh k x = x `shiftR` (k `div` 2)
instance BRep Word64 where
    takeHigh k x = x `shiftR` (k `div` 2)

-- for testing
newtype Int4 = Int4 Word16 deriving (Eq,Ord,Real,Num,Enum,Integral,Bits,Ix)
instance Show Int4 where
    show (Int4 x) = show x
instance BRep Int4 where
    totalBits = 4
    takeHigh k x = x `shiftR` (k `div` 2)

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

notMember :: forall i s. (BRep i) => VEB s i -> i -> ST s Bool
notMember v = fmap not . member v

member :: forall i s. (BRep i) => VEB s i -> i -> ST s Bool
member = memberK (totalBits @i)

memberK :: forall i s. (BRep i) => Int -> VEB s i -> i -> ST s Bool
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

printVEB :: forall i. (BRep i, Show i) => VEB RealWorld i -> IO ()
printVEB v0 = putStrLn =<< stToIO (printGo (totalBits @i) v0)
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

delete :: forall i s. BRep i => VEB s i -> i -> ST s ()
delete v00 e00 = void $ deleteK (totalBits @i) v00 e00
  where
    -- returns True if tree became empty
    deleteK :: Int -> VEB s i -> i -> ST s Bool
    deleteK 0 _v _e = error "insertK 0"
    deleteK k v e = do
        let k' = k `div` 2
        let onFullMM (tmin,tmax) | tmin == tmax && tmin == e =
                True <$ writeSTRef (vMinMax v) Nothing
            onFullMM (tmin,tmax) | tmin == e = do
                let maxIsMin = False <$ writeSTRef (vMinMax v) (Just (tmax,tmax))
                case v of
                    VLeaf{}   -> maxIsMin
                    VNode{..} -> do
                        aux <- readSTRef vAux
                        getMin aux >>= \case
                            Nothing -> maxIsMin
                            Just auxMinHigh -> do
                                c <- readArray vChildren auxMinHigh
                                cMinLow <- fromMaybe (error "getMin: impossible") <$> getMin c
                                nowEmpty <- deleteK k' c cMinLow
                                when nowEmpty $ void $ deleteK k' aux auxMinHigh
                                let fullMin = fromHighLow k auxMinHigh cMinLow
                                False <$ writeSTRef vMinMax (Just (fullMin,tmax))
            onFullMM (tmin,tmax) | tmax == e = do
                let minIsMax = False <$ writeSTRef (vMinMax v) (Just (tmin,tmin))
                case v of
                    VLeaf{}   -> minIsMax
                    VNode{..} -> do
                        aux <- readSTRef vAux
                        getMax aux >>= \case
                            Nothing -> minIsMax
                            Just auxMaxHigh -> do
                                c <- readArray vChildren auxMaxHigh
                                cMaxLow <- fromMaybe (error "getMax: impossible") <$> getMax c
                                nowEmpty <- deleteK k' c cMaxLow
                                when nowEmpty $ void $ deleteK k' aux auxMaxHigh
                                let fullMax = fromHighLow k auxMaxHigh cMaxLow
                                False <$ writeSTRef vMinMax (Just (tmin,fullMax))

            onFullMM _ = case v of
                VLeaf{} -> pure False -- nothing more we can do here
                VNode{..} -> do
                    aux <- readSTRef vAux
                    let h = takeHigh k e
                    memberK k' aux h >>= \case
                        False -> pure False -- nothing to delete from :shrug:
                        True -> do
                            c <- readArray vChildren h
                            nowEmpty <- deleteK k' c (takeLow k e)
                            when nowEmpty $ void $ deleteK k' aux h
                            pure False

        maybe (pure False) onFullMM =<< readSTRef (vMinMax v)
