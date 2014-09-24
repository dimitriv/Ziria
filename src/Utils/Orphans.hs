{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module Orphans where

import Data.Map (Map)
import Text.Parsec.Pos (SourcePos)
import Text.Show.Pretty (PrettyVal(..))
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  PrettyVal orphans
-------------------------------------------------------------------------------}

instance PrettyVal a => PrettyVal (Maybe a)
instance (PrettyVal a, PrettyVal b) => PrettyVal (Either a b)
instance PrettyVal Bool
instance PrettyVal ()

instance PrettyVal SourcePos where
  prettyVal = prettyVal . show
instance (PrettyVal k, PrettyVal a) => PrettyVal (Map k a) where
  prettyVal = prettyVal . Map.toList
