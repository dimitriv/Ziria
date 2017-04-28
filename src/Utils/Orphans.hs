{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
module Orphans where

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Loc
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..), Value(Con))
import qualified Data.Map as Map

import qualified Text.PrettyPrint.HughesPJ as HughesPJ
import Control.Monad.Error.Class 

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

instance NFData Pos         where rnf = genericRnf
instance NFData Loc         where rnf = genericRnf
instance NFData SrcLoc      where rnf = genericRnf

{-------------------------------------------------------------------------------
  Error orphans
-------------------------------------------------------------------------------}

instance Error HughesPJ.Doc where
  noMsg  = HughesPJ.empty
  strMsg = HughesPJ.text
