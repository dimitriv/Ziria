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
{-# LANGUAGE ScopedTypeVariables #-}

module AutoLUT (runAutoLUT) where

import Opts
import GenSym

import AstComp
import AstExpr
import PpExpr
import CgLUT
import qualified Analysis.RangeAnal as RangeAnal
import LUTAnalysis 

import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Text.PrettyPrint.HughesPJ 

import Outputable
import PpExpr () 
import AstUnlabelled ( eLUT )
import Orphans ()

import qualified Analysis.DataFlow as DataFlow

{------------------------------------------------------------------
  The AutoLUT analysis
-------------------------------------------------------------------}

runAutoLUT :: DynFlags -> Sym -> Comp -> IO Comp
runAutoLUT dflags _ c = autolutC dflags c

autolutC :: DynFlags -> Comp -> IO Comp
autolutC dflags 
  = mapCompM return return return return (autolutE dflags) return

autolutE :: DynFlags -> Exp -> IO Exp
autolutE dflags = autoE
  where
    autoE e0@(MkExp e loc inf) = do
      -- | Calculate LUT statistics 
      stats <- calcLUTStats dflags e0
      case stats of 
        Left err -> do 
          verbose dflags $ 
            vcat [ text "Cannot autolut expresion:"
                 , nest 4 (ppr e0)
                 , text "Location:" <+> text (show loc)
                 , text "Reason:" <+> err ]
          go_inside e0
        Right stats -> autoE' e0 stats

    autoE' e0 stats
      | Right True <- lutShould stats
      = do verbose dflags $
             vcat [ text "Expression autolutted:" 
                  , nest 4 (ppr e0)
                  , text "Location:" <+> text (show (expLoc e0))
                  , ppr stats ]
           pure $ eLUT (expLoc e0) stats e0
    autoE' e0 stats = go_inside e0

    go_inside e0@(MkExp e loc inf) 
      = MkExp <$> go e <*> pure loc <*> pure inf
      where
        go :: Exp0 -> IO Exp0
        go e@(EVal {})       = pure e
        go e@(EValArr {})    = pure e
        go e@(EVar {})       = pure e
        go (EUnOp op e)      = EUnOp op <$> autoE e
        go (EBinOp op e1 e2) = EBinOp op <$> autoE e1 <*> autoE e2
        go (EAssign e1 e2)   = EAssign <$> autoE e1 <*> autoE e2
        go (EArrRead e1 e2 len) =
            EArrRead <$> autoE e1 <*> autoE e2 <*> pure len
        go (EArrWrite e1 e2 len e3) =
            EArrWrite <$> autoE e1 <*> autoE e2 <*> pure len <*> autoE e3
        go (EFor ui i e1 e2 e3) = do
            EFor ui i <$> autoE e1 <*> autoE e2 <*> autoE e3
        go (EWhile e1 e2) = do
            EWhile <$> autoE e1 <*> autoE e2
        go (ELet v fi e1 e2) = ELet v fi <$> autoE e1 <*> autoE e2
        go (ELetRef v (Just e1) e2) 
          = autoE e1 >>= \e1' -> ELetRef v (Just e1') <$> autoE e2
        go (ELetRef v Nothing e2) = ELetRef v Nothing <$> autoE e2
        go (ESeq e1 e2)    = ESeq <$> autoE e1 <*> autoE e2
        go (ECall f es)    = ECall f <$> mapM autoE es
        go (EIf e1 e2 e3)  = EIf <$> autoE e1 <*> autoE e2 <*> autoE e3
        go (EPrint nl es)  = EPrint nl <$> mapM autoE es
        go (EError _t str) = pure e
        go (ELUT s e)      = pure $ ELUT s e
        -- TODO: Revisit this, it seems defensive
        go (EStruct tn tfs) = pure e
        go (EProj _ fn)     = pure e
