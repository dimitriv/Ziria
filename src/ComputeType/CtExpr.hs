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
module CtExpr (ctExp) where

import Text.PrettyPrint.HughesPJ

import AstExpr
import CtCall
import Outputable
import PpExpr ()
import Utils

ctExp :: GExp Ty a -> Ty
ctExp MkExp{..} = ctExp0 unExp

ctExp0 :: GExp0 Ty a -> Ty
ctExp0 (EVal    ty _)      = ty
ctExp0 (EValArr ty _)      = ty
ctExp0 (EVar nm)           = nameTyp nm
ctExp0 (EUnOp op a)        = ctUnOp op (ctExp a)
ctExp0 (EBinOp op a b)     = ctBinOp op (ctExp a) (ctExp b)
ctExp0 (EAssign _ _)       = TUnit
ctExp0 (EArrRead a _ l)    = ctArrRead (ctExp a) l
ctExp0 (EArrWrite _ _ _ _) = TUnit
ctExp0 (EIter _ _ _ _)     = TUnit
ctExp0 (EFor _ _ _ _ _)    = TUnit
ctExp0 (EWhile _ _)        = TUnit
ctExp0 (ELet _ _ _ e2)     = ctExp e2
ctExp0 (ELetRef _ _ e2)    = ctExp e2
ctExp0 (ESeq _ e2)         = ctExp e2
ctExp0 (ECall f xs)        = ctECall (nameTyp f) (map ctExp xs)
ctExp0 (EIf _ a _)         = ctExp a
ctExp0 (EPrint _ _)        = TUnit
ctExp0 (EError ty _)       = ty
ctExp0 (ELUT _ e)          = ctExp e
ctExp0 (EBPerm e1 _)       = ctExp e1
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

ctArrRead :: Ty -> LengthInfo -> Ty
ctArrRead (TArray _ t) LISingleton  = t
ctArrRead (TArray _ t) (LILength n) = TArray (Literal n) t
ctArrRead ty _ = panic $ text "ctExp0:" <+> ppr ty

ctProj :: Ty -> FldName -> Ty
ctProj (TStruct _ fs) n = lookup' n fs
ctProj t _ = panic $ text "ctProj:" <+> ppr t
