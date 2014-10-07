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
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
-- | Compute the type of computations
module CtComp (ctComp) where

import Text.PrettyPrint.HughesPJ

import AstComp
import AstExpr
import CtCall
import CtExpr (ctExp)
import Outputable
import PpComp ()
import Utils

ctComp :: GComp CTy Ty a b -> CTy
ctComp MkComp{..} = ctComp0 unComp

ctComp0 :: GComp0 CTy Ty a b -> CTy
ctComp0 (Var nm)             = nameTyp nm
ctComp0 (BindMany _ xcs)     = ctComp (snd (last xcs))
ctComp0 (Seq _ c2)           = ctComp c2
ctComp0 (Par _ c1 c2)        = ctPar (ctComp c1) (ctComp c2)
ctComp0 (Let _ _ c2)         = ctComp c2
ctComp0 (LetE _ _ _ c)       = ctComp c
ctComp0 (LetERef _ _ c)      = ctComp c
ctComp0 (LetHeader _ _ c)    = ctComp c
ctComp0 (LetFunC _ _ _ _ c2) = ctComp c2
ctComp0 (LetStruct _ c)      = ctComp c
ctComp0 (Call f xs)          = ctCall (nameTyp f) (map ctCallArg xs)
ctComp0 (Emit a e)           = CTBase (TComp TUnit a (ctExp e))
ctComp0 (Emits a e)          = ctEmits a (ctExp e)
ctComp0 (Return a b _ e)     = CTBase (TComp (ctExp e) a b)
ctComp0 (Interleave c1 _)    = ctComp c1
ctComp0 (Branch _ c1 _)      = ctComp c1
ctComp0 (Take1 a b)          = CTBase (TComp a a b)
ctComp0 (Take a b n)         = CTBase (TComp (TArray (Literal n) a) a b)
ctComp0 (Until _ c)          = ctComp c
ctComp0 (While _ c)          = ctComp c
ctComp0 (Times _ _ _ _ c)    = ctComp c
ctComp0 (Repeat _ c)         = ctRepeat (ctComp c)
ctComp0 (VectComp _ c)       = ctComp c
ctComp0 (Map _ f)            = ctMap (nameTyp f)
ctComp0 (Filter f)           = ctFilter (nameTyp f)
ctComp0 (ReadSrc a)          = CTBase (TTrans (TBuff (ExtBuf a)) a)
ctComp0 (WriteSnk a)         = CTBase (TTrans a (TBuff (ExtBuf a)))
ctComp0 (ReadInternal a _ _) = CTBase (TTrans (TBuff (IntBuf a)) a)
ctComp0 (WriteInternal a _)  = CTBase (TTrans a (TBuff (IntBuf a)))
ctComp0 (Standalone c)       = ctComp c
ctComp0 (Mitigate a n1 n2)   = ctMitigate a n1 n2

ctCallArg :: CallArg (GExp Ty b) (GComp CTy Ty a b) -> CallArg Ty CTy
ctCallArg (CAExp  e) = CAExp  $ ctExp  e
ctCallArg (CAComp c) = CAComp $ ctComp c

ctEmits :: Ty -> Ty -> CTy
ctEmits a (TArray _ b) = CTBase (TComp TUnit a b)
ctEmits _ b = panic $ text "ctEmits: Unexpected" <+> ppr b

ctPar :: CTy -> CTy -> CTy
ctPar (CTBase (TTrans  a _)) (CTBase (TTrans  _ c)) = CTBase (TTrans  a c)
ctPar (CTBase (TTrans  a _)) (CTBase (TComp u _ c)) = CTBase (TComp u a c)
ctPar (CTBase (TComp u a _)) (CTBase (TTrans  _ c)) = CTBase (TComp u a c)
ctPar t t' = panic $ text "ctPar: Unexpected" <+> ppr t <+> text "and" <+> ppr t'

ctRepeat :: CTy -> CTy
ctRepeat (CTBase (TComp _ a b)) = CTBase (TTrans a b)
ctRepeat t = panic $ text "ctRepeat: Unexpected" <+> ppr t

ctMap :: Ty -> CTy
ctMap (TArrow [a] b) = CTBase (TTrans a b)
ctMap t = panic $ text "ctMap: Unexpected" <+> ppr t

ctFilter :: Ty -> CTy
ctFilter (TArrow [a] TBool) = CTBase (TTrans a a)
ctFilter t = panic $ text "ctFilter: Unexpected" <+> ppr t

ctMitigate :: Ty -> Int -> Int -> CTy
ctMitigate a n1 n2 = CTBase (TTrans t1 t2)
  where
    t1 = if n1 == 1 then a else TArray (Literal n1) a
    t2 = if n2 == 1 then a else TArray (Literal n2) a
