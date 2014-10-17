{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Orphans where

import Data.Map (Map)
import Text.Parsec.Pos (SourcePos)
import Text.Show.Pretty (PrettyVal(..), Value(Con))
import Text.Parsec.Pos
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  PrettyVal orphans
-------------------------------------------------------------------------------}

instance PrettyVal a => PrettyVal (Maybe a)
instance (PrettyVal a, PrettyVal b) => PrettyVal (Either a b)
instance PrettyVal Bool
instance PrettyVal ()

instance PrettyVal SourcePos where
  prettyVal pos = Con (show 'newPos) [
                      prettyVal (sourceName pos)
                    , prettyVal (sourceLine pos)
                    , prettyVal (sourceColumn pos)
                    ]

instance (PrettyVal k, PrettyVal a) => PrettyVal (Map k a) where
  prettyVal mp = Con (show 'Map.toList) [prettyVal (Map.toList mp)]
