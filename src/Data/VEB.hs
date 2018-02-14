-- | Van Emde Boas tree implementation.

module Data.VEB
    ( BRep(..)
    , VEB
    , delete
    , fromList
    , getMax
    , getMin
    , insert
    , isEmpty
    , member
    , notMember
    , printVEB
    , toList
    ) where

import Data.VEB.Internal
