{-
   Copyright (c) Microsoft Corporation
   All rights reserved.

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
-}
-- | Sparse arrays
--
-- This provides constant time access read and write and constant time array
-- slicing, as well as efficient storage (only non-default elements are stored)
--
-- Sparse arrays are strict in the elements.
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module SparseArray (
    SparseArray -- Opaque
    -- * "Data.Array.MArray"-like features
  , newArray
  , newListArray
  , readArray
  , writeArray
  , writeDefault
  , getElems
    -- * "Data.IntMap"-like features
  , size
    -- * "Data.Vector"-like features
  , slice
  , update
    -- * Versions without bounds checking
  , unsafeReadArray
  , unsafeWriteArray
  , unsafeWriteDefault
  , unsafeSlice
  , unsafeUpdate
  ) where

import Data.Maybe (fromMaybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

-- | Sparse arrays
--
-- Sparse arrays are always indexed from 0.
data SparseArray a = SA {
       saElems  :: !(IntMap a)
     , saOffset :: !Int
     , saSize   :: !Int
     , saDef    :: a
     }
   deriving Show

instance Eq a => Eq (SparseArray a) where
  arr1 == arr2 = getElems arr1 == getElems arr2

{-------------------------------------------------------------------------------
  MArray-like features
-------------------------------------------------------------------------------}

-- | /O(1)/ Builds a new array, with every element initialised to the supplied
-- value.
newArray :: Int -> a -> SparseArray a
newArray saSize saDef = SA{ saElems = IM.empty, saOffset = 0, .. }

-- | /O(n)/ Construct from a list
newListArray :: a -> [a] -> SparseArray a
newListArray def as = SA {
      saElems  = IM.fromDistinctAscList (zip [0..] as)
    , saOffset = 0
    , saSize   = length as
    , saDef    = def
    }

-- | /O(min(n,W))/  Read an element from a mutable array
readArray :: Int -> SparseArray a -> a
readArray i sa@SA{..}
     | i < 0        = error "readArray: negative index"
     | i >= saSize  = error "readArray: out of bounds"
     | otherwise    = unsafeReadArray i sa

-- | Version of `readArray` without bounds checking
unsafeReadArray :: Int -> SparseArray a -> a
unsafeReadArray i SA{..} = fromMaybe saDef $ IM.lookup (i + saOffset) saElems

-- | /O(min(n,W)) Write an element in a mutable array
--
-- NOTE: We do not compare the element being written with the default; it may
-- be more efficient to use `writeDefault`.
writeArray :: Int -> a -> SparseArray a -> SparseArray a
writeArray i a sa@SA{..}
     | i < 0       = error "writeArray: negative index"
     | i >= saSize = error "writeArray: out of bounds"
     | otherwise   = unsafeWriteArray i a sa

-- | Version of `writeArray` without bounds checking
unsafeWriteArray :: Int -> a -> SparseArray a -> SparseArray a
unsafeWriteArray i a SA{..} = SA{ saElems = IM.insert (i + saOffset) a saElems, .. }

-- | /O(min(n,W))/ Set an element in the array to the default value
writeDefault :: Int -> SparseArray a -> SparseArray a
writeDefault i sa@SA{..}
     | i < 0       = error "writeArray: negative index"
     | i >= saSize = error "writeArray: out of bounds"
     | otherwise   = unsafeWriteDefault i sa

-- | Version of `writeDefault` without bounds checking
unsafeWriteDefault :: Int -> SparseArray a -> SparseArray a
unsafeWriteDefault i SA{..} = SA{ saElems = IM.delete (i + saOffset) saElems, .. }

-- | /O(n)/ Convert to a list
getElems :: forall a. SparseArray a -> [a]
getElems sa@SA{..} = aux 0 (toList sa)
  where
    aux :: Int -> [(Int, a)] -> [a]
    aux n _           | n == saSize = []
    aux n []                        = saDef : aux (n + 1) []
    aux n ((m, a):as) | n == m      = a     : aux (n + 1) as
                      | otherwise   = saDef : aux (n + 1) ((m, a):as)

{-------------------------------------------------------------------------------
  IntMap-like features
-------------------------------------------------------------------------------}

-- | /O(1)/ Number of elements in the map.
size :: SparseArray a -> Int
size = saSize

-- | /O(n)/ Convert the map to a list of key/value pairs.
--
-- NOTE: Elements equal to the default are NOT included in the result list
-- (see `getElems` if this is not desired)
toList :: SparseArray a -> [(Int, a)]
toList SA{..} = map adjustIndex
              $ filter inRange
              $ IM.toList saElems
  where
    adjustIndex (i, a) = (i - saOffset, a)
    inRange     (i, _) = i >= saOffset && i < saOffset + saSize

{-------------------------------------------------------------------------------
  Vector-like features
-------------------------------------------------------------------------------}

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least i+n elements.
slice :: Int -> Int -> SparseArray a -> SparseArray a
slice i n parent
    | i < 0               = error "slice: negative offset"
    | i + n > size parent = error "slice: size too large"
    | otherwise           = unsafeSlice i n parent

-- | Version of `slice` without bounds checking
unsafeSlice :: Int -> Int -> SparseArray a -> SparseArray a
unsafeSlice i n parent = SA{ saElems  = saElems parent
                           , saOffset = saOffset parent + i
                           , saSize   = n
                           , saDef    = saDef parent
                           }

-- | Overwrite all entries in the first array with entries in the second array
-- at a particular offset. That is,
--
-- > readArray j (update i arr2 arr1) == readArray (j - i) arr2
-- >   for (i <= j <= i + size arr2)
--
-- NOTE: This _only_ works for arrays with equal default elements. If this
-- is not the case, this throws a runtime exception (this is the role reason
-- for the equality constraint).
update :: Eq a => Int -> SparseArray a -> SparseArray a -> SparseArray a
update i arr2 arr1
    | saDef arr1 /= saDef arr2  = error "update: unequal defaults"
    | i < 0                     = error "update: negative index"
    | i + size arr2 > size arr1 = error "update: out of bounds"
    | otherwise                 = unsafeUpdate i arr2 arr1

-- | Version of `update` without bounds checking
--
-- NOTE: See comments for `update` about default elements.
unsafeUpdate :: forall a. Int -> SparseArray a -> SparseArray a -> SparseArray a
unsafeUpdate i arr2 = go 0 (toList arr2)
  where
    go :: Int -> [(Int, a)] -> SparseArray a -> SparseArray a
    go n _           arr | n == size arr2 = arr
    go n []          arr                  = go (n + 1) []          (writeDefault (n + i)   arr)
    go n ((m, a):as) arr | n == m         = go (n + 1) as          (writeArray   (n + i) a arr)
                         | otherwise      = go (n + 1) ((m, a):as) (writeDefault (n + i)   arr)
