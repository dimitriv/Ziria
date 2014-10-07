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
-- | Compute the type of function calls
--
-- Since functions can be length polymorphic this involves instantiating
-- type variables. Note that length polymorphism is the _only_ kind of
-- polymorphism that Ziria supports.
{-# OPTIONS_GHC -Wall #-}
module CtCall (ctECall, ctCall, lookup') where

import Text.PrettyPrint.HughesPJ

import AstComp
import AstExpr
import Outputable
import PpExpr ()
import PpComp ()
import Utils

ctECall :: Ty -> [Ty] -> Ty
ctECall (TArrow args res) args' = applyTy (matchAllTy (zip args args')) res
ctECall t _ = panic $ text "ctECall: Unexpected" <+> ppr t

ctCall :: CTy -> [CallArg Ty CTy] -> CTy
ctCall (CTArrow args res) args' = CTBase (applyCTy (matchAllCA (zip args args')) res)
ctCall t _ = panic $ text "ctCall: Unexpected" <+> ppr t

{-------------------------------------------------------------------------------
  Substitutions
-------------------------------------------------------------------------------}

type Subst = [(GName Ty, NumExpr)]

applyTy :: Subst -> Ty -> Ty
applyTy s (TArray n  t) = TArray (applyNumExpr s n)   (applyTy s t)
applyTy s (TArrow ts t) = TArrow (map (applyTy s) ts) (applyTy s t)
applyTy _ t             = t

applyNumExpr :: Subst -> NumExpr -> NumExpr
applyNumExpr s (NVar n _) = lookup' n s
applyNumExpr _ e          = e

applyCTy :: Subst -> CTy0 -> CTy0
applyCTy s (TComp u a b) = TComp (applyTy s u) (applyTy s a) (applyTy s b)
applyCTy s (TTrans  a b) = TTrans (applyTy s a) (applyTy s b)

{-------------------------------------------------------------------------------
  Expression types
-------------------------------------------------------------------------------}

matchAllTy :: [(Ty, Ty)] -> Subst
matchAllTy = concatMap (uncurry matchTy)

matchTy :: Ty -> Ty -> Subst
matchTy (TArray n  t) (TArray n'  t') = matchNumExpr n n' ++ matchTy t t'
matchTy (TArrow ts t) (TArrow ts' t') = matchAllTy (zip (t:ts) (t':ts'))
matchTy _             _               = []

matchNumExpr :: NumExpr -> NumExpr -> Subst
matchNumExpr (NVar n _) e = [(n, e)]
matchNumExpr _          _ = []

{-------------------------------------------------------------------------------
  Computation types
-------------------------------------------------------------------------------}

matchAllCA :: [(CallArg Ty CTy0, CallArg Ty CTy)] -> Subst
matchAllCA = concatMap (uncurry matchCA)

matchCA :: CallArg Ty CTy0 -> CallArg Ty CTy -> Subst
matchCA (CAExp  t) (CAExp  t') = matchTy  t t'
matchCA (CAComp t) (CAComp t') = matchCTy t t'
matchCA t t' = panic $ text "matchCA: Unexpected" <+> ppr t <+> text "and" <+> ppr t'

matchCTy :: CTy0 -> CTy -> Subst
matchCTy (TComp u a b) (CTBase (TComp u' a' b')) = matchAllTy [(u, u'), (a, a'), (b, b')]
matchCTy (TTrans  a b) (CTBase (TTrans   a' b')) = matchAllTy [(a, a'), (b, b')]
matchCTy t t' = panic $ text "matchCTy: Unexpected" <+> ppr t <+> text "and" <+> ppr t'

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

lookup' :: (Outputable a, Eq a) => a -> [(a, b)] -> b
lookup' a dict =
  case lookup a dict of
    Nothing -> panic $ text "lookup:" <+> ppr a <+> text "not found"
    Just b  -> b
