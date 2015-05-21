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
-- | Compute the types of expressions
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module CtExpr (ctExp, ctFun, ctDerefExp) where

import Text.PrettyPrint.HughesPJ

import AstExpr
import CtCall
import Outputable
import PpExpr ()
import Utils

ctExp :: GExp Ty a -> Ty
ctExp MkExp{..} = ctExp0 unExp

-- | Compute the type of an expression
--
-- NOTE: Arrays must be non-empty so it's safe to take @(head elems)@
ctExp0 :: GExp0 Ty a -> Ty
ctExp0 (EVal ty _)         = ty
ctExp0 (EValArr elems)     = TArray (Literal (length elems)) (ctExp (head elems))
ctExp0 (EVar nm)           = nameTyp nm
ctExp0 (EUnOp op a)        = ctUnOp op (ctExp a)
ctExp0 (EBinOp op a b)     = ctBinOp op (ctExp a) (ctExp b)
ctExp0 (EAssign _ _)       = TUnit
ctExp0 (EArrRead a _ l)    = ctArrRead (show a) (ctExp a) l
ctExp0 (EFor _ _ _ _ _)    = TUnit
ctExp0 (EWhile _ _)        = TUnit
ctExp0 (ELet _ _ _ e2)     = ctExp e2
ctExp0 (ELetRef _ _ e2)    = ctExp e2
ctExp0 (ESeq _ e2)         = ctExp e2
ctExp0 (ECall f xs)
  | tfun@(TArrow fun_tys _fres) <- nameTyp f
  , checkArgMut fun_tys xs
  = ctECall f tfun (map ctExp xs)
  | otherwise              = panic $ text "ctExp: ECall ill-formed mutability"
ctExp0 (EIf _ a _)         = ctExp a
ctExp0 (EPrint _ _)        = TUnit
ctExp0 (EError ty _)       = ty
ctExp0 (ELUT _ e)          = ctExp e
ctExp0 (EStruct t _)       = t
ctExp0 (EProj s f)         = ctProj (ctExp s) f



ctUnOp :: GUnOp Ty -> Ty -> Ty
ctUnOp NatExp     _  = panic (text "ctUnOp: NatExp not supported")
ctUnOp Neg        ty = ty
ctUnOp Not        ty = ty
ctUnOp BwNeg      ty = ty
ctUnOp (Cast ty') _  = ty'
ctUnOp ALength    _  = tint

ctBinOp :: BinOp -> Ty -> Ty -> Ty
ctBinOp op a _b
  | isArithBinOp    op = a
  | isShiftBinOp    op = a
  | isLogicalBinOp  op = a
  | isEqualityBinOp op = TBool
  | isRelBinOp      op = TBool
  | isBoolBinOp     op = a
  | otherwise          = panic $ text "ctBinOp:" <+> ppr op

ctArrRead :: String -> Ty -> LengthInfo -> Ty
ctArrRead _err (TArray _ t) LISingleton  = t
ctArrRead _err (TArray _ t) (LILength n) = TArray (Literal n) t
ctArrRead err ty _ = panic $ text "ctExp0:" <+> text err <+> ppr ty

ctProj :: Ty -> FldName -> Ty
ctProj (TStruct _ fs) n = lookup' n fs
ctProj t _ = panic $ text "ctProj:" <+> ppr t

ctFun :: Fun -> Ty
ctFun fun = nameTyp (funName fun) 

ctDerefExp :: AGDerefExp exp Ty -> Ty
ctDerefExp (GDVar nm)        = nameTyp nm
ctDerefExp (GDProj de fld)   = ctProj (ctDerefExp de) fld
ctDerefExp (GDArr de _ li)   = ctArrRead "ctDerefExp" (ctDerefExp de) li

-- DELETEME
-- ctDerefExp (GDNewArray t _)  = t
-- ctDerefExp (GDNewStruct t _) = t
