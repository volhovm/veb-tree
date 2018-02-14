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
import Control.Monad.ST (ST)
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
-- | Types that can be used as elements of 'VEB' (can be split in
-- high/low).
class (Show i, Ord i, Integral i, Ix i) => BRep i where
    -- | Return total number of bits.
    totalBits :: Int
    -- | Take high as we're @k@ bit number.
    takeHigh :: Int -> i -> i
    -- | Take low as we're @k@ bit number.
    takeLow :: Int -> i -> i
    -- | Create a @k@ bit number from @k/2@ high and low parts.
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
newtype Word4 = Word4 Word16 deriving (Eq,Ord,Real,Num,Enum,Integral,Bits,Ix)
instance Show Word4 where
    show (Word4 x) = show x
instance BRep Word4 where
    totalBits = 4
    takeHigh k x = x `shiftR` (k `div` 2)

----------------------------------------------------------------------------
-- VEB
----------------------------------------------------------------------------

-- | Van Emde Boas tree.
data VEB s i
    = VNode { vChildren :: STArray s i (VEB s i)
            , vAux      :: STRef s (VEB s i)
            , vMinMax   :: STRef s (Maybe (i, i)) }
      -- ^ Trees of k > 1, have children, aux and minmax.
    | VLeaf { vMinMax :: STRef s (Maybe (i, i)) }
      -- ^ Trees of k = 1, always have zero children and empty aux, so
      -- it's ommited.

-- | Checks if tree is empty.
isEmpty :: (BRep i) => VEB s i -> ST s Bool
isEmpty v = isNothing <$> readSTRef (vMinMax v)

getMinMax :: ((i,i) -> i) -> VEB s i -> ST s (Maybe i)
getMinMax g v = fmap g <$> readSTRef (vMinMax v)

-- | Gets minimal element from the tree.
getMin :: VEB s i -> ST s (Maybe i)
getMin = getMinMax fst

-- | Gets maximum element from the tree.
getMax :: VEB s i -> ST s (Maybe i)
getMax = getMinMax snd

-- | Checks if element is a member of a tree.
member :: forall i s. (BRep i) => VEB s i -> i -> ST s Bool
member = memberK (totalBits @i)

-- | Opposite of 'member'.
notMember :: forall i s. (BRep i) => VEB s i -> i -> ST s Bool
notMember v = fmap not . member v

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

-- | Converts 'VEB' to list.
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

-- | Prints 'VEB' in json-like format.
printVEB :: forall i s. (BRep i, Show i) => VEB s i -> ST s String
printVEB v0 = printGo (totalBits @i) v0
  where
    printMinMax s = maybe "\"mm0\"" (show . show) <$> readSTRef s
    printGo :: Int -> VEB s i -> ST s String
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

-- | Creates new 'VEB'.
newVEB :: forall i s. BRep i => ST s (VEB s i)
newVEB = newVEBK $ totalBits @i

-- Creates VEB k-tree.
newVEBK :: forall i s. BRep i => Int -> ST s (VEB s i)
newVEBK i | i <= 0 = error "newVEBK"
newVEBK 1 = VLeaf <$> newSTRef Nothing
newVEBK k = do
    when (odd k) $ error $ "NewVebGo " ++ show k
    vChildren <- newArray_ (0, 2 ^ (fromIntegral k `div` (2 :: Int)) - 1)
    vAux <- newSTRef =<< newVEBK (k `div` 2)
    vMinMax <- newSTRef Nothing
    pure $ VNode {..}

-- | Inserts an element into the 'VEB'.
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

-- | Converts a list to a 'VEB' by creating a new tree and inserting all
-- elements into it.
fromList :: forall i s. BRep i => [i] -> ST s (VEB s i)
fromList toInsert = do
    v <- newVEB @i
    mapM_ (insert v) toInsert
    pure v

-- | Deletes an element from a list.
delete :: forall i s. BRep i => VEB s i -> i -> ST s ()
delete v00 e00 = void $ deleteK (totalBits @i) v00 e00
  where
    -- returns True if tree became empty
    deleteK :: Int -> VEB s i -> i -> ST s Bool
    deleteK 0 _v _e = error "insertK 0"
    deleteK k v e = do
        let k' = k `div` 2
        let cornerCase (tmin,tmax) isMin = do
                let isOther = False <$ writeSTRef (vMinMax v)
                                                  (Just $ if isMin then (tmax,tmax)
                                                                   else (tmin,tmin))
                let getter = if isMin then getMin else getMax
                case v of
                    VLeaf{}   -> isOther
                    VNode{..} -> do
                        aux <- readSTRef vAux
                        getter aux >>= \case
                            Nothing -> isOther
                            Just auxMHigh -> do
                                c <- readArray vChildren auxMHigh
                                cMLow <-
                                    fromMaybe (error $ "cornerCase: impossible " ++ show isMin) <$>
                                    getter c
                                nowEmpty <- deleteK k' c cMLow
                                when nowEmpty $ void $ deleteK k' aux auxMHigh
                                let fullM = fromHighLow k auxMHigh cMLow
                                False <$ writeSTRef vMinMax (Just $ if isMin then (fullM,tmax)
                                                                             else (tmin,fullM))
        let onFullMM (tmin,tmax) | tmin == tmax && tmin == e =
                True <$ writeSTRef (vMinMax v) Nothing
            onFullMM t@(tmin,_) | tmin == e = cornerCase t True
            onFullMM t@(_,tmax) | tmax == e = cornerCase t False
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
