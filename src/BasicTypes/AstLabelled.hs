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
-- | Smart constructors (that take labels as arguments)
module AstLabelled where

import Prelude hiding (pi)
import Text.Parsec.Pos (SourcePos)
import Data.Map (Map)

import AstExpr
import AstComp
import Analysis.Range (Range)

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

vint :: Int -> Val
-- Auxiliary function for use in the vectorizer
vint n = VInt (fromIntegral n)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

eVal :: Maybe SourcePos -> a -> t -> Val -> GExp t a
eVal loc a t v = MkExp (EVal t v) loc a

eValArr :: Maybe SourcePos -> a -> t -> [Val] -> GExp t a
eValArr loc a t v = MkExp (EValArr t v) loc a

eVar :: Maybe SourcePos -> a ->  GName t -> GExp t a
eVar loc a v = MkExp (EVar v) loc a

eUnOp :: Maybe SourcePos -> a -> GUnOp t -> GExp t a -> GExp t a
eUnOp loc a o v = MkExp (EUnOp o v) loc a

eBinOp :: Maybe SourcePos -> a -> BinOp -> GExp t a -> GExp t a -> GExp t a
eBinOp loc a b x y = MkExp (EBinOp b x y) loc a

eAssign :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a
eAssign loc a x y = MkExp (EAssign x y) loc a

eArrRead :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> LengthInfo -> GExp t a
eArrRead loc a x y l = MkExp (EArrRead x y l) loc a

eArrWrite :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> LengthInfo -> GExp t a -> GExp t a
eArrWrite loc a x y l e = MkExp (EArrWrite x y l e) loc a

eIter :: Maybe SourcePos -> a -> GName t -> GName t -> GExp t a -> GExp t a -> GExp t a
eIter loc a x y e1 e2 = MkExp (EIter x y e1 e2) loc a

eFor :: Maybe SourcePos -> a -> UnrollInfo -> GName t -> GExp t a -> GExp t a -> GExp t a -> GExp t a
eFor loc a ui n e1 e2 e3 = MkExp (EFor ui n e1 e2 e3) loc a

eLet :: Maybe SourcePos -> a ->  GName t -> ForceInline -> GExp t a -> GExp t a -> GExp t a
eLet loc a x fi e1 e2 = MkExp (ELet x fi e1 e2) loc a

eLetRef :: Maybe SourcePos -> a ->  GName t -> Maybe (GExp t a) -> GExp t a -> GExp t a
eLetRef loc a nm x e = MkExp (ELetRef nm x e) loc a

eLetHeader :: Maybe SourcePos -> a -> GFun t a -> GExp t a -> GExp t a
eLetHeader loc a fun e = MkExp (ELetHeader fun e) loc a

eSeq :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a
eSeq loc a e1 e2 = MkExp (ESeq e1 e2) loc a

eCall :: Maybe SourcePos -> a ->  GName t -> [GExp t a] -> GExp t a
eCall loc a f es = MkExp (ECall f es) loc a

eIf :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a -> GExp t a
eIf loc a e1 e2 e3 = MkExp (EIf e1 e2 e3) loc a

ePrint :: Maybe SourcePos -> a ->  Bool -> GExp t a -> GExp t a
ePrint loc a b e = MkExp (EPrint b e) loc a

eError :: Maybe SourcePos -> a -> t -> String -> GExp t a
eError loc a t s = MkExp (EError t s) loc a

eLUT :: Maybe SourcePos -> a ->  Map (GName t) Range -> GExp t a -> GExp t a
eLUT loc a m e = MkExp (ELUT m e) loc a

eBPerm :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a
eBPerm loc a e1 e2 = MkExp (EBPerm e1 e2) loc a

eStruct :: Maybe SourcePos -> a -> t -> [(String,GExp t a)] -> GExp t a
eStruct loc a tn es = MkExp (EStruct tn es) loc a

eProj :: Maybe SourcePos -> a ->  GExp t a -> String -> GExp t a
eProj loc a e s = MkExp (EProj e s) loc a

eWhile :: Maybe SourcePos -> a -> GExp t a -> GExp t a -> GExp t a
eWhile loc a econd ebody = MkExp (EWhile econd ebody) loc a

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

mkFunDefined :: Maybe SourcePos -> a
             -> GName t                       -- ^ name
             -> [GName t]                     -- ^ params
             -> GExp t a                      -- ^ body
             -> GFun t a
mkFunDefined loc a nm params body = MkFun {
      unFun   = MkFunDefined nm params body
    , funLoc  = loc
    , funInfo = a
    }

mkFunExternal :: Maybe SourcePos -> a
              -> GName t                      -- ^ name
              -> [GName t]                    -- ^ params
              -> t                            -- ^ return type
              -> GFun t a
mkFunExternal loc a nm params ret = MkFun {
      unFun   = MkFunExternal nm params ret
    , funLoc  = loc
    , funInfo = a
    }

{-------------------------------------------------------------------------------
  Computations
-------------------------------------------------------------------------------}

cVar :: Maybe SourcePos -> a -> GName tc -> GComp tc t a b
cVar loc a x = MkComp (Var x) loc a

cBindMany :: Maybe SourcePos -> a -> GComp tc t a b -> [(GName t,GComp tc t a b)] -> GComp tc t a b
cBindMany loc a c cs = MkComp (mkBindMany c cs) loc a -- NB: using smart constructor

cSeq :: Maybe SourcePos -> a -> GComp tc t a b -> GComp tc t a b -> GComp tc t a b
cSeq loc a c1 c2 = MkComp (Seq c1 c2) loc a

cPar :: Maybe SourcePos -> a -> ParInfo -> GComp tc t a b -> GComp tc t a b -> GComp tc t a b
cPar loc a pi c1 c2 = MkComp (Par pi c1 c2) loc a

cLet :: Maybe SourcePos -> a -> GName tc ->
        GComp tc t a b -> GComp tc t a b -> GComp tc t a b
cLet loc a x c1 c2 = MkComp (Let x c1 c2) loc a

cLetE :: Maybe SourcePos -> a -> GName t -> ForceInline ->
         GExp t b -> GComp tc t a b -> GComp tc t a b
cLetE loc a x fi e c = MkComp (LetE x fi e c) loc a

cLetERef :: Maybe SourcePos -> a -> GName t -> Maybe (GExp t b) -> GComp tc t a b -> GComp tc t a b
cLetERef loc a x y c = MkComp (LetERef x y c) loc a

cLetHeader :: Maybe SourcePos -> a -> GFun t b -> GComp tc t a b -> GComp tc t a b
cLetHeader loc a f c = MkComp (LetHeader f c) loc a

cLetFunC :: Maybe SourcePos -> a -> GName tc -> [(GName (CallArg t tc))]
         -> GComp tc t a b -> GComp tc t a b -> GComp tc t a b
cLetFunC loc a x args c1 c2 = MkComp (LetFunC x args c1 c2) loc a

cLetStruct :: Maybe SourcePos -> a -> GStructDef t -> GComp tc t a b -> GComp tc t a b
cLetStruct loc a sd c = MkComp (LetStruct sd c) loc a

cCall :: Maybe SourcePos -> a -> GName tc -> [CallArg (GExp t b) (GComp tc t a b)] -> GComp tc t a b
cCall loc a x es = MkComp (Call x es) loc a

cEmit :: Maybe SourcePos -> a -> GExp t b -> GComp tc t a b
cEmit loc a e = MkComp (Emit e) loc a

cEmits :: Maybe SourcePos -> a -> GExp t b -> GComp tc t a b
cEmits loc a e = MkComp (Emits e) loc a

cReturn :: Maybe SourcePos -> a -> ForceInline -> GExp t b -> GComp tc t a b
cReturn loc a fi e = MkComp (Return fi e) loc a

cInterleave :: Maybe SourcePos -> a -> GComp tc t a b -> GComp tc t a b -> GComp tc t a b
cInterleave loc a c1 c2 = MkComp (Interleave c1 c2) loc a

cBranch :: Maybe SourcePos -> a -> GExp t b -> GComp tc t a b -> GComp tc t a b -> GComp tc t a b
cBranch loc a e c1 c2 = MkComp (Branch e c1 c2) loc a

cTake1 :: Maybe SourcePos -> a -> t -> GComp tc t a b
cTake1 loc a t = MkComp (Take1 t) loc a

cTake :: Maybe SourcePos -> a -> t -> Int -> GComp tc t a b
cTake loc a t n = MkComp (Take t n) loc a

cUntil :: Maybe SourcePos -> a -> GExp t b -> GComp tc t a b -> GComp tc t a b
cUntil loc a e c = MkComp (Until e c) loc a

cWhile :: Maybe SourcePos -> a -> GExp t b -> GComp tc t a b -> GComp tc t a b
cWhile loc a e c = MkComp (While e c) loc a

cTimes :: Maybe SourcePos -> a -> UnrollInfo -> GExp t b -> GExp t b -> GName t -> GComp tc t a b -> GComp tc t a b
cTimes loc a ui es elen x c = MkComp (Times ui es elen x c) loc a

cRepeat :: Maybe SourcePos -> a -> Maybe VectAnn -> GComp tc t a b -> GComp tc t a b
cRepeat loc a ann c = MkComp (Repeat ann c) loc a

cVectComp :: Maybe SourcePos -> a -> (Int,Int) -> GComp tc t a b -> GComp tc t a b
cVectComp loc a ann c = MkComp (VectComp ann c) loc a

cMap :: Maybe SourcePos -> a -> Maybe VectAnn -> GName t -> GComp tc t a b
cMap loc a ann nm = MkComp (Map ann nm) loc a

cFilter :: Maybe SourcePos -> a -> GName t -> GComp tc t a b
cFilter loc a nm = MkComp (Filter nm) loc a

cReadSrc  :: Maybe SourcePos -> a -> t -> GComp tc t a b
cReadSrc loc a t = MkComp (ReadSrc t) loc a

cWriteSnk :: Maybe SourcePos -> a -> t -> GComp tc t a b
cWriteSnk loc a t = MkComp (WriteSnk t) loc a

cReadInternal  :: Maybe SourcePos -> a -> t -> BufId -> ReadType -> GComp tc t a b
cReadInternal  loc a t bid rt = MkComp (ReadInternal t bid rt) loc a

cWriteInternal :: Maybe SourcePos -> a -> t -> BufId -> GComp tc t a b
cWriteInternal loc a t bid = MkComp (WriteInternal t bid) loc a

cStandalone :: Maybe SourcePos -> a -> GComp tc t a b -> GComp tc t a b
cStandalone loc a c = MkComp (Standalone c) loc a

cMitigate :: Maybe SourcePos -> a -> t -> Int -> Int -> GComp tc t a b
cMitigate loc a t n1 n2 = MkComp (Mitigate t n1 n2) loc a
