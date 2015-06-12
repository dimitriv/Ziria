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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Ziria.Codegen.CgCmdDom
  ( cgDeref
  , cgAssign, cgAssignAliasing, lvalAlias
  , squashArrDeref
  , cgArrWrite, cgArrWrite_chk
  ) where

import Data.Loc
import qualified Language.C.Syntax as C
import Language.C.Quote.C

import Ziria.BasicTypes.AstExpr
import Ziria.Codegen.CgBoundsCheck
import Ziria.Codegen.CgMonad
import Ziria.Codegen.CgTypes
import Ziria.Codegen.CgValDom
import Ziria.ComputeType.CtExpr

import Opts
             
{-------------------------------------------------------------------------------
  CmdDom interface 
-------------------------------------------------------------------------------}

-- | Squash back-to-back GDArr. For example x[0,3][1,2] ~~> x[1,2] 
squashArrDeref :: LVal ArrIdx -> LVal ArrIdx
squashArrDeref = go 
 where
  go (GDVar x) = GDVar x
  -- NB: If we have x[1][1,2] then x[1] must be a nested array
  go (GDArr (GDArr d c0 (LILength {})) c1 l1) 
    = go (GDArr d (c0 `arrIdxPlus` c1) l1)
  go (GDArr d cidx linf) = GDArr (go d) cidx linf
  go (GDProj d f)        = GDProj (go d) f
 
cgDeref :: DynFlags -> SrcLoc -> LVal ArrIdx -> Cg C.Exp
cgDeref dfs loc d = go (squashArrDeref d)
  where
    go (GDVar x) = lookupVarEnv x
    go (GDProj d0 f) = do 
      cd <- go d0 
      return $ cgStrProj projty cd strty f
      where projty = ctDerefExp (GDProj d0 f)
            strty  = ctDerefExp d0
    go d1@(GDArr d0 aidx lnfo) = do   
      cd0 <- cgDeref dfs loc d0 
      cgArrRead dfs loc ret_ty cd0 arr_ty aidx lnfo
      where arr_ty = ctDerefExp d0
            ret_ty = ctDerefExp d1

-- In this case the RHS comes from an LVal which 
-- is aliasing the LHS and so we'd better create new storage.
cgAssignAliasing :: DynFlags -> SrcLoc -> LVal ArrIdx -> C.Exp -> Cg ()
cgAssignAliasing dfs loc d crhs_pre = do
  res <- freshName "anti_alias" lval_ty Mut
  appendCodeGenDeclGroup (name res) lval_ty ZeroOut
  let crhs = [cexp| $id:(name res)|]
  -- copy to fresh storage
  assignByVal lval_ty crhs crhs_pre
  cgAssign dfs loc d crhs
  where lval_ty = ctDerefExp d

lvalAlias :: LVal ArrIdx -> LVal ArrIdx -> Bool
lvalAlias d1 d2 = (lvalBase d1 == lvalBase d2)
   where lvalBase (GDVar x) = x
         lvalBase (GDArr d _ _) = lvalBase d
         lvalBase (GDProj d _)  = lvalBase d

cgAssign :: DynFlags -> SrcLoc -> LVal ArrIdx -> C.Exp -> Cg ()
cgAssign dfs loc d crhs = go (squashArrDeref d)
  where
    go (GDVar x) = do 
      cx <- lookupVarEnv x
      assignByVal lval_ty cx crhs

    go lval@(GDProj {}) = do 
      cx <- cgDeref dfs loc lval 
      assignByVal lval_ty cx crhs

    go d1@(GDArr d0 aidx lnfo) = do 
      cd0 <- cgDeref dfs loc d0 
      cgArrWrite dfs loc ret_ty cd0 arr_ty aidx lnfo crhs
      where arr_ty = ctDerefExp d0
            ret_ty = ctDerefExp d1

    lval_ty = ctDerefExp d

cgArrWrite :: DynFlags 
           -> SrcLoc
           -> Ty          -- ^ Return type
           -> C.Exp -> Ty -- ^ Array and array type
           -> ArrIdx      -- ^ Start
           -> LengthInfo  -- ^ Range to read from
           -> C.Exp 
           -> Cg ()
cgArrWrite dfs loc ty carr arr_ty start_idx li crhs = do 
  cgBoundsCheck dfs loc arr_ty (cexpOfArrIdx start_idx) li
  -- | Do bounds checking and call the 'checked' version (chk)
  cgArrWrite_chk ty carr start_idx li crhs

cgArrWrite_chk :: Ty         -- ^ return type 
               -> C.Exp      -- ^ ce
               -> ArrIdx     -- ^ aidx
               -> LengthInfo -- ^ rng
               -> C.Exp      -- ^ rhs
               -> Cg ()      -- ^ ce[aidx...aidx+rng-1] := rhs
-- | Bit arrays singletons
cgArrWrite_chk TBit ce aidx LISingleton crhs = 
  appendStmt $ [cstm| bitWrite($ce,$(cexpOfArrIdx aidx),$crhs);|]
-- | Bit arrays length reads 
cgArrWrite_chk (TArray _ TBit) ce aidx (LILength l) crhs
  | Just cidx <- isBitArrByteAligned aidx
  , l `mod` 8 == 0  
    -- assignByVal calls bitArrRead, which in turn can overwrite (see bit.c)
    -- hence we can call this fast path only when the length is aligned too.
  , let clhs = [cexp| & $ce[$cidx]|]
  = assignByVal (TArray (Literal l) TBit) clhs crhs
  | otherwise
  = appendStmt [cstm|bitArrWrite($crhs,$(cexpOfArrIdx aidx),$int:l,$ce);|]
-- | Non-bit arrays, just assign-by-val
cgArrWrite_chk ret_ty cd aidx lnfo crhs = do 
  clhs <- cgArrRead_chk ret_ty cd aidx lnfo
  assignByVal ret_ty clhs crhs

