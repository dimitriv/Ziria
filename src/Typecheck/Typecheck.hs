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
-- | Type check source code
{-# OPTIONS_GHC -Wall #-}
module Typecheck (
    -- * Top-level
    tyCheckProg
  , envOfDecls
    -- * Not top-level (for use by AstFreeMonad primarily)
  , tyCheckComp
  , tyCheckExpr
  ) where

import Control.Monad
import Text.PrettyPrint.HughesPJ
import qualified Data.Set as S

import AstComp
import AstExpr
import CtComp (ctComp)
import Lint (lint)
import Outputable
import Rename
import TcMonad
import TcUnify (defaultProg)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tyCheckProg :: SrcProg -> TcM Prog
tyCheckProg prog_src = do
  prog_ren <- renProg prog_src
  -- liftIO $ putStrLn $ "Renamed: " ++ show prog_ren
  prog_tc  <- lint prog_ren
  prog_def <- defaultProg prog_tc
  void $ checkUnresolved (progComp prog_def)
  return prog_def

-- TODO: Remove once we get rid of locals
envOfDecls :: [(GName Ty, Maybe Exp)] -> Env
envOfDecls = mkEnv . map (\(nm, _) -> (name nm, nm))

{-------------------------------------------------------------------------------
  Not top-level (for use by AstFreeMonad primarily)
-------------------------------------------------------------------------------}

tyCheckComp :: SrcComp -> TcM Comp
tyCheckComp = renComp >=> lint

tyCheckExpr :: SrcExp -> TcM Exp
tyCheckExpr = renExp >=> lint

{-------------------------------------------------------------------------------
  Auxiliary: check for unresolved type variables
-------------------------------------------------------------------------------}

-- | Check for unresolved type variables.
--
-- In principle we could just zonk and require that there aren't any, anywhere,
-- but that is problematic, since there may be parts of the code that are not
-- used, and where type inference did not really solve any unification
-- problems. However the "main" part of the code, which is supposed to run
-- should not have any unresolved variables.  Hence, we descend in the context
-- (with findMain, below) and only then we map through the computation with our
-- checker (with comp_combine, below).
checkUnresolved :: Comp -> TcM Comp
checkUnresolved
    = mapCompM return return return return return comp_combine . findMain
  where
    comp_combine c = do
      let (vars, cvars) = tyVarsOfCTy (ctComp c)
      if (S.null vars && S.null cvars)
        then return c
        else raiseErrNoVarCtx (compLoc c) $
               vcat [ text "Computation:"
                    , nest 2 $ ppr c
                    , text "has unresolved type:"
                    , nest 2 $ ppr (ctComp c)
                    ]

    -- Descend the context to find the main computation
    findMain :: Comp -> Comp
    findMain (MkComp (Let _ _         c) _ _) = findMain c
    findMain (MkComp (LetStruct _     c) _ _) = findMain c
    findMain (MkComp (LetE _ _ _      c) _ _) = findMain c
    findMain (MkComp (LetHeader _     c) _ _) = findMain c
    findMain (MkComp (LetFunC _ _ _ _ c) _ _) = findMain c
    findMain (MkComp (LetERef _ _     c) _ _) = findMain c
    findMain other_c                          = other_c
