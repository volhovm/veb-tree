{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Gauge-based benchmarks.

module Main where

import Control.DeepSeq (NFData (..))
import Control.Monad (forM, replicateM)
import Control.Monad.ST (RealWorld, stToIO)
import Data.List (nub)
import Data.VEB.Internal
import Data.Word (Word16, Word32, Word8)
import System.Random

import Gauge

vbToInsert :: forall i. (Random i, BRep i, Bounded i) => Int -> IO ([i],VEB RealWorld i)
vbToInsert size = do
    (iter :: Int) <- randomRIO (0, size)
    elems <- nub <$> replicateM iter (randomRIO (0, maxBound @i))
    v <- stToIO $ fromList elems
    pure (elems, v)

vebBench :: [Benchmark]
vebBench =
    [ bgroup "Word8" $ vb @Word8
    , bgroup "Word16" $ vb @Word16
    , bgroup "Word32" $ vb @Word32
    ]
  where
    vb :: forall i. (BRep i, Bounded i, Random i, NFData i) => [Benchmark]
    vb =
        let vbNumbered ~n = env (vbToInsert @i n) $ allBenches n
            allBenches n ~(elems,tree) =
                bgroup (show n ++ "elems")
                   [ bench "fromList" $ whnfIO $ stToIO $ fromList elems
                   , bench "lookupAll" $ whnfIO $ stToIO $ forM elems (member tree)
                   , bench "toList" $ whnfIO $ stToIO $ toList tree
                   , bench "deleteAll" $ whnfIO $ stToIO $ forM elems (delete tree)
                   ]
        in [ vbNumbered 2000
           , vbNumbered 5000
           , vbNumbered 10000
           ]

-- VEB is mutable -- it only contains three pointers in the worst
-- case. All operations are done strictly in ST.
instance NFData (VEB s i) where
    rnf = const ()

{-
Output of stack bench --benchmark-arguments "--small"

Word8/2000elems/fromList                  mean 376.5 μs  ( +- 35.99 μs  )
Word8/2000elems/lookupAll                 mean 133.6 μs  ( +- 3.801 μs  )
Word8/2000elems/toList                    mean 18.14 μs  ( +- 488.5 ns  )
Word8/2000elems/deleteAll                 mean 4.248 μs  ( +- 187.7 ns  )
Word8/5000elems/fromList                  mean 371.1 μs  ( +- 15.13 μs  )
Word8/5000elems/lookupAll                 mean 134.1 μs  ( +- 5.434 μs  )
Word8/5000elems/toList                    mean 18.62 μs  ( +- 1.209 μs  )
Word8/5000elems/deleteAll                 mean 4.346 μs  ( +- 378.5 ns  )
Word8/10000elems/fromList                 mean 386.4 μs  ( +- 30.29 μs  )
Word8/10000elems/lookupAll                mean 135.7 μs  ( +- 9.273 μs  )
Word8/10000elems/toList                   mean 18.65 μs  ( +- 1.574 μs  )
Word8/10000elems/deleteAll                mean 4.276 μs  ( +- 291.0 ns  )
Word16/2000elems/fromList                 mean 5.906 ms  ( +- 514.1 μs  )
Word16/2000elems/lookupAll                mean 1.080 ms  ( +- 61.59 μs  )
Word16/2000elems/toList                   mean 262.1 μs  ( +- 27.55 μs  )
Word16/2000elems/deleteAll                mean 22.93 μs  ( +- 2.728 μs  )
Word16/5000elems/fromList                 mean 8.546 ms  ( +- 514.2 μs  )
Word16/5000elems/lookupAll                mean 1.601 ms  ( +- 228.6 μs  )
Word16/5000elems/toList                   mean 412.9 μs  ( +- 94.65 μs  )
Word16/5000elems/deleteAll                mean 32.79 μs  ( +- 3.785 μs  )
Word16/10000elems/fromList                mean 20.67 ms  ( +- 2.317 ms  )
Word16/10000elems/lookupAll               mean 5.792 ms  ( +- 940.7 μs  )
Word16/10000elems/toList                  mean 2.013 ms  ( +- 370.7 μs  )
Word16/10000elems/deleteAll               mean 107.6 μs  ( +- 39.08 μs  )
Word32/2000elems/fromList                 mean 2.072 ms  ( +- 96.57 μs  )
Word32/2000elems/lookupAll                mean 81.80 μs  ( +- 6.016 μs  )
Word32/2000elems/toList                   mean 65.38 μs  ( +- 8.125 μs  )
Word32/2000elems/deleteAll                mean 1.807 μs  ( +- 114.1 ns  )
Word32/5000elems/fromList                 mean 102.9 ms  ( +- 3.550 ms  )
Word32/5000elems/lookupAll                mean 8.518 ms  ( +- 441.6 μs  )
Word32/5000elems/toList                   mean 7.146 ms  ( +- 521.9 μs  )
Word32/5000elems/deleteAll                mean 206.4 μs  ( +- 23.30 μs  )
Word32/10000elems/fromList                mean 172.5 ms  ( +- 6.740 ms  )
Word32/10000elems/lookupAll               mean 14.84 ms  ( +- 1.065 ms  )
Word32/10000elems/toList                  mean 11.45 ms  ( +- 938.4 μs  )
Word32/10000elems/deleteAll               mean 393.2 μs  ( +- 33.29 μs  )
-}

main :: IO ()
main = defaultMain vebBench
