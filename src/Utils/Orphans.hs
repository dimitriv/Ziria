{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Orphans where

import Control.DeepSeq
import Data.Loc
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..), Value(Con))
import qualified Data.Map as Map

import qualified Text.PrettyPrint.Mainland as MainlandPretty (Doc(..), prettyS)

import qualified Text.PrettyPrint.HughesPJ as HughesPJ

{-------------------------------------------------------------------------------
  Generic orphans
-------------------------------------------------------------------------------}

deriving instance Generic Pos
deriving instance Generic Loc
deriving instance Generic SrcLoc

{-------------------------------------------------------------------------------
  PrettyVal orphans
-------------------------------------------------------------------------------}

instance PrettyVal ()

instance PrettyVal Pos
instance PrettyVal Loc
instance PrettyVal SrcLoc

instance (PrettyVal k, PrettyVal a) => PrettyVal (Map k a) where
  prettyVal mp = Con (show 'Map.toList) [prettyVal (Map.toList mp)]

{-------------------------------------------------------------------------------
  NFData orphans
-------------------------------------------------------------------------------}

deriving instance NFData Pos
deriving instance NFData Loc
deriving instance NFData SrcLoc

{-------------------------------------------------------------------------------
  Error orphans
-------------------------------------------------------------------------------}

instance Ord HughesPJ.Doc where 
  d1 <= d2 = show d1 <= show d2

{------------------------------------------------------------------------------
   Pretty Printer orphans
------------------------------------------------------------------------------}

instance Show MainlandPretty.Doc where
  showsPrec _ = MainlandPretty.prettyS 80

