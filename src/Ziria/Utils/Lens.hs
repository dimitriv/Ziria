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
-- | Very minimal lenses
--
-- We don't want to pull in the entire `lens` library and all its dependencies.
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
module Ziria.Utils.Lens (
    -- * Basic definitions
    Lens
  , get
  , put
  , modify
    -- * Standard lenses
  , mapAt
  , mapAtDef
    -- * Working with MonadState
  , getSt
  , putSt
  , modifySt
  ) where

import Control.Applicative
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as St
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

type Lens a b = forall f. Functor f => (b -> f b) -> (a -> f a)

get :: Lens a b -> a -> b
get lens = getConst . lens Const

modify :: Lens a b -> (b -> b) -> (a -> a)
modify lens f = runIdentity . lens (Identity . f)

put :: Lens a b -> b -> a -> a
put lens b = modify lens (const b)

{-------------------------------------------------------------------------------
  Standard lenses
-------------------------------------------------------------------------------}

mapAt :: Ord k => k -> Lens (Map k a) (Maybe a)
mapAt k f mp = (\a -> Map.alter (const a) k mp) <$> f (Map.lookup k mp)

mapAtDef :: Ord k => a -> k -> Lens (Map k a) a
mapAtDef def k f mp = (\a -> Map.insert k a mp) <$> f (Map.findWithDefault def k mp)

{-------------------------------------------------------------------------------
  Working with MonadSate
-------------------------------------------------------------------------------}

getSt :: MonadState s m => Lens s a -> m a
getSt lens = St.gets (get lens)

-- | Update the indexed by a lens
--
-- NOTE: Updates the state strictly.
modifySt :: MonadState s m => Lens s a -> (a -> a) -> m ()
modifySt lens f = St.modify (modify lens f)

-- | Overwrite the state indexed by a lens
--
-- NOTE: Updates the state strictly
putSt :: MonadState s m => Lens s a -> a -> m ()
putSt lens a = modifySt lens (const a)
