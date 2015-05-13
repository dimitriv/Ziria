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
{-# LANGUAGE  QuasiQuotes, GADTs, ScopedTypeVariables, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module CgCmdDom
  ( cgDeref
  , cgAssign
  ) where

import Opts
import AstExpr
import CgMonad
import CgTypes
import CtExpr

import Outputable

import Data.Loc
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.HughesPJ
import Utils ( panic )
import CgValDom
import CgBoundsCheck
             
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
    go (GDArr d0 aidx lnfo) = do
      cd0 <- cgDeref dfs loc d0 
      cgBoundsCheck dfs loc arr_ty (cexpOfArrIdx aidx) lnfo
      cg_read_arr cd0 arr_ty aidx lnfo 
      where arr_ty = ctDerefExp d0

cgAssign :: DynFlags -> SrcLoc -> LVal ArrIdx -> C.Exp -> Cg ()
cgAssign dfs loc d crhs = go (squashArrDeref d)
  where
    go (GDVar x) = do 
      cx <- lookupVarEnv x
      assignByVal lval_ty cx crhs

    go lval@(GDProj {}) = do 
      cx <- cgDeref dfs loc lval 
      assignByVal lval_ty cx crhs

    go (GDArr d0 aidx lnfo) = do 
      cd0 <- cgDeref dfs loc d0 
      cgBoundsCheck dfs loc arr_ty (cexpOfArrIdx aidx) lnfo
      cg_asgn_arr cd0 arr_ty aidx lnfo crhs
      where arr_ty = ctDerefExp d0   

    lval_ty = ctDerefExp d

cg_read_arr :: C.Exp      -- ce
            -> Ty         -- array type (of ce)
            -> ArrIdx     -- aidx
            -> LengthInfo -- rng
            -> Cg C.Exp   -- ce[aidx...aidx+rng-1]
-- | Bit arrays singletons
cg_read_arr ce (TArray _ TBit) aidx LISingleton = do 
  res <- freshName "bitres" TBit Mut
  appendCodeGenDeclGroup (name res) TBit ZeroOut
  appendStmt $ [cstm| bitRead($ce,$(cexpOfArrIdx aidx),& $id:(name res)); |]
  return [cexp|$id:(name res)|]
-- | Bit arrays length reads 
cg_read_arr ce t@(TArray _ TBit) aidx (LILength l)
  | AIdxStatic i <- aidx
  , i `mod` 8 == 0 = return [cexp| & $ce[$int:(i `div` 8)]|]
  | AIdxMult i cebase <- aidx
  , i `mod` 8 == 0 = return [cexp| & $ce[$int:(i `div` 8)*$cebase]|]
  | otherwise = do
      res <- freshName "bitarrres" t Mut 
      appendCodeGenDeclGroup (name res) t ZeroOut
      appendStmt [cstm|
         bitArrRead($ce,$(cexpOfArrIdx aidx),$int:l,$id:(name res)); |]
      return [cexp| $id:(name res) |]
-- | Non-bit arrays
cg_read_arr ce (TArray _ tbase) aidx LISingleton = do 
   let b    = isPtrType tbase
   let cidx = cexpOfArrIdx aidx
   if b then return [cexp| &($ce[$cidx])|] else return [cexp| $ce[$cidx]|]
cg_read_arr ce (TArray {}) aidx (LILength _) =
   let cidx = cexpOfArrIdx aidx
   in return [cexp|& ($ce[$cidx])|]
cg_read_arr _ce ty _aidx li = panic $ 
   vcat [ text "cg_read_arr: non-array type" <+> ppr ty
        , text "li    :" <+> text (show li) ]

cg_asgn_arr :: C.Exp      -- ce
            -> Ty         -- array type (of ce)
            -> ArrIdx     -- aidx
            -> LengthInfo -- rng
            -> C.Exp      -- rhs
            -> Cg ()      -- ce[aidx...aidx+rng-1] := rhs
-- | Bit arrays singletons
cg_asgn_arr ce (TArray _ TBit) aidx LISingleton crhs = 
  appendStmt $ [cstm| bitRead($ce,$(cexpOfArrIdx aidx),& &($crhs));|]
-- | Bit arrays length reads 
cg_asgn_arr ce (TArray _ TBit) aidx (LILength l) crhs
  | AIdxStatic i <- aidx
  , i `mod` 8 == 0 
  = let clhs = [cexp| & $ce[$int:(i `div` 8)]|]
    in assignByVal (TArray (Literal l) TBit) clhs crhs
  | AIdxMult i cebase <- aidx
  , i `mod` 8 == 0 
  = let clhs = [cexp| & $ce[$int:(i `div` 8)*$cebase]|]
    in assignByVal (TArray (Literal l) TBit) clhs crhs
  | otherwise
  = appendStmt [cstm|bitArrWrite($crhs,$(cexpOfArrIdx aidx),$int:l,$ce);|]
-- | Non-bit arrays, just assign-by-val
cg_asgn_arr cd t@(TArray {}) aidx lnfo crhs = do 
  clhs <- cg_read_arr cd t aidx lnfo
  assignByVal t clhs crhs

cg_asgn_arr _cd ty _aidx li _crhs = panic $ 
   vcat [ text "cg_asgn_arr: non-array type" <+> ppr ty
        , text "li    :" <+> text (show li) ]

