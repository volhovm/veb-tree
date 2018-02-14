{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | VEB spec.

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.ST (stToIO)
import Data.List (nub, sort, (\\))
import Data.Word (Word16, Word32, Word64, Word8)
import System.Random
import Test.Hspec (Spec, describe, hspec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.VEB.Internal (BRep (..))
import qualified Data.VEB.Internal as V

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
    describe "BRep instances for: " $ do
        describe "Word8" $ brepSpec @Word8
        describe "Word16" $ brepSpec @Word16
        describe "Word32" $ brepSpec @Word32
        describe "Word64" $ brepSpec @Word64
    describe "VEB Word32" $ vebSpec @Word32

genBRepK :: forall i. (BRep i) => Gen Int
genBRepK = do
   let log2 :: Int -> Int
       log2 (fromIntegral -> x) = round $ (log x :: Double) / log 2
   kp <- choose (0,log2 (totalBits @i))
   pure $ 2 ^ kp

brepSpec :: forall i. (BRep i, Random i, Show i, Arbitrary i) => Spec
brepSpec = do
    prop "takeHigh,takeLow ~= fromHighLow" $
        forAll ((,) <$> genBRepK @i <*> arbitrary @i) $ \(k,x) ->
            fromHighLow k (takeHigh k x) (takeLow k x) == x
    let halfLess g = forAll (arbitrary @i) $ \x -> let b = (totalBits @i) in g b x < (2^(b-1))
    prop "low < 2^(k-1)" $ halfLess takeLow
    prop "high < 2^(k-1)" $ halfLess takeHigh

vebSpec :: forall i. (BRep i, Random i, Show i, Bounded i, Arbitrary i) => Spec
vebSpec = do
    prop "newVEB doesn't fail" $ stProp $ True <$ (V.newVEB @i)
    prop "fromList . toList = id" $ forAll (listOf (arbitrary @i)) $ \(nub -> xs) ->
        stProp $ do
            v <- V.fromList @i xs
            asList <- V.toList v
            pure $ sort xs == sort asList
    prop "every inserted elem is a member" $ forAll (listOf (arbitrary @i)) $ \xs ->
        stProp $ do
            v <- V.fromList @i xs
            fmap and $ forM xs $ V.member v

    let deletionScenario act =
            forAll (listOf (arbitrary @i)) $ \(nub -> xs) ->
            forAll (sublistOf xs) $ \(nub -> ys) ->
            stProp $ do
                v <- V.fromList @i xs
                forM_ ys $ V.delete v
                act (xs,ys,v)
    prop "all deleted elements are absent" $ deletionScenario $ \(_xs,ys,v) ->
        fmap and $ forM ys $ V.notMember v
    prop "all undeleted elements are present" $ deletionScenario $ \(xs,ys,v) ->
        fmap and $ forM (xs \\ ys) $ V.member v
  where
    stProp = ioProperty . stToIO
