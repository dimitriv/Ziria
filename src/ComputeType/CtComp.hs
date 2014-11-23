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
module CtComp ( ctComp, ctParMid, ctDoneTyOfComp ) where

import Text.PrettyPrint.HughesPJ

import AstComp
import AstExpr
import CtCall
import CtExpr (ctExp)
import Outputable
import Utils
import Data.Maybe ( fromJust )

ctComp :: GComp CTy Ty a b -> CTy
ctComp MkComp{..} = ctComp0 unComp

ctComp0 :: GComp0 CTy Ty a b -> CTy
ctComp0 (Var nm)             = nameTyp nm
ctComp0 (BindMany c0 xcs)    = ctBind $ map ctComp (c0 : map snd xcs)
ctComp0 (Seq c1 c2)          = ctBind $ map ctComp [c1,c2]
ctComp0 (Par _ c1 c2)        = ctPar (ctComp c1) (ctComp c2)
ctComp0 (Let _ _ c2)         = ctComp c2
ctComp0 (LetE _ _ _ c)       = ctComp c
ctComp0 (LetERef _ _ c)      = ctComp c
ctComp0 (LetHeader _ c)      = ctComp c
ctComp0 (LetFunC _ _ _ c2)   = ctComp c2
ctComp0 (LetStruct _ c)      = ctComp c
ctComp0 (Call f xs)          = ctCall (nameTyp f) (map ctCallArg xs)
ctComp0 (Emit e)             = CTComp TUnit TVoid (ctExp e)
ctComp0 (Emits e)            = ctEmits TVoid (ctExp e)
ctComp0 (Return _ e)         = CTComp (ctExp e) TVoid TVoid
ctComp0 (Interleave c1 _)    = ctComp c1
ctComp0 (Branch _ c1 _)      = ctComp c1
ctComp0 (Take1 a)            = CTComp a a TVoid
ctComp0 (Take a n)           = CTComp (TArray (Literal n) a) a TVoid
ctComp0 (Until _ c)          = ctComp c
ctComp0 (While _ c)          = ctComp c
ctComp0 (Times _ _ _ _ c)    = ctComp c
ctComp0 (Repeat _ c)         = ctRepeat (ctComp c)
ctComp0 (VectComp _ c)       = ctComp c
ctComp0 (Map _ f)            = ctMap (nameTyp f)
ctComp0 (Filter f)           = ctFilter (nameTyp f)
ctComp0 (ReadSrc a)          = CTTrans (TBuff (ExtBuf a)) a
ctComp0 (WriteSnk a)         = CTTrans a (TBuff (ExtBuf a))
ctComp0 (ReadInternal a _ _) = CTTrans (TBuff (IntBuf a)) a
ctComp0 (WriteInternal a _)  = CTTrans a (TBuff (IntBuf a))
ctComp0 (Standalone c)       = ctComp c
ctComp0 (Mitigate a n1 n2)   = ctMitigate a n1 n2

ctCallArg :: CallArg (GExp Ty b) (GComp CTy Ty a b) -> CallArg Ty CTy
ctCallArg (CAExp  e) = CAExp  $ ctExp  e
ctCallArg (CAComp c) = CAComp $ ctComp c

ctEmits :: Ty -> Ty -> CTy
ctEmits a (TArray _ b) = CTComp TUnit a b
ctEmits _ b = panic $ text "ctEmits: Unexpected" <+> ppr b

ctPar :: CTy -> CTy -> CTy
ctPar (CTTrans  a _) (CTTrans  _ c) = CTTrans  a c
ctPar (CTTrans  a _) (CTComp u _ c) = CTComp u a c
ctPar (CTComp u a _) (CTTrans  _ c) = CTComp u a c
ctPar t t' = panic $ 
             text "ctPar: Unexpected" <+> ppr t <+> text "and" <+> ppr t'

ctBind :: [CTy] -> CTy 
ctBind [] = panic $ text "ctBind: empty bind?"
ctBind cts 
  = case last cts of
       CTComp v _ _ -> CTComp v ax bx
       CTTrans _ _  -> CTTrans ax bx 
       ct -> panic $ 
             text "ctBind: Unexpected non-base computation type" <+> ppr ct
  where ax = join_many $ map inTyOfCTy cts
        bx = join_many $ map yldTyOfCTy cts
        join_many [t] = t
        join_many (t1:t2:ts) = join_many (ctJoin t1 t2 : ts)
        join_many _ = panic $ text "join_many: empty list!"


ctRepeat :: CTy -> CTy
ctRepeat (CTComp _ a b) = CTTrans a b
ctRepeat t = panic $ text "ctRepeat: Unexpected" <+> ppr t

ctMap :: Ty -> CTy
ctMap (TArrow [a] b) = CTTrans a b
ctMap t = panic $ text "ctMap: Unexpected" <+> ppr t

ctFilter :: Ty -> CTy
ctFilter (TArrow [a] TBool) = CTTrans a a
ctFilter t = panic $ text "ctFilter: Unexpected" <+> ppr t

ctMitigate :: Ty -> Int -> Int -> CTy
ctMitigate a n1 n2 = CTTrans t1 t2
  where
    t1 = if n1 == 1 then a else TArray (Literal n1) a
    t2 = if n2 == 1 then a else TArray (Literal n2) a


ctParMid :: Comp -> Comp -> Ty
-- Give the type of the queue in the middle of a par
ctParMid c1 c2 = ctJoin y1 i2
  where cty1 = ctComp c1
        cty2 = ctComp c2 
        y1   = yldTyOfCTy cty1
        i2   = inTyOfCTy cty2

ctJoin :: Ty -> Ty -> Ty 
-- Pre-condition: the two types are joinable
ctJoin TVoid t = t
ctJoin t TVoid = t
ctJoin t t'   
  | t == t' 
  = t
  | otherwise
  = panic $ text "ctJoin: Incompatible types!" <+> ppr t <+> text "and" <+> ppr t'


ctDoneTyOfComp :: Comp -> Ty 
-- Pre-condition: the computer is an ST (C v) a b
ctDoneTyOfComp comp
  = fromJust $ doneTyOfCTy (ctComp comp)


