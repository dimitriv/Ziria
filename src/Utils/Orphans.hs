{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Orphans where

import Control.DeepSeq
import Data.Map (Map)
import Text.Parsec.Pos
import Text.Parsec.Pos
import Text.Show.Pretty (PrettyVal(..), Value(Con))
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

{-------------------------------------------------------------------------------
  NFData orphans
-------------------------------------------------------------------------------}

instance NFData SourcePos where
  rnf p = let !line = sourceLine p
              !col  = sourceColumn p
              !name = force sourceName
          in ()
