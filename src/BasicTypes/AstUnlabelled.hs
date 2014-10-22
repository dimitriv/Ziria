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
module AstUnlabelled where

import Prelude hiding (pi)
import Text.Parsec.Pos (SourcePos)
import Data.Map (Map)

import AstExpr
import AstComp
import Analysis.Range (Range)

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

vint :: Integral a => a -> Val
-- Auxiliary function for use in the vectorizer
vint n = VInt (fromIntegral n)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

eVal :: Maybe SourcePos -> t -> Val -> GExp t ()
eVal loc t v = MkExp (EVal t v) loc ()

eValArr :: Maybe SourcePos -> t -> [Val] -> GExp t ()
eValArr loc t v = MkExp (EValArr t v) loc ()

eVar :: Maybe SourcePos ->  GName t -> GExp t ()
eVar loc v = MkExp (EVar v) loc ()

eUnOp :: Maybe SourcePos -> GUnOp t -> GExp t () -> GExp t ()
eUnOp loc o v = MkExp (EUnOp o v) loc ()

eBinOp :: Maybe SourcePos -> BinOp -> GExp t () -> GExp t () -> GExp t ()
eBinOp loc b x y = MkExp (EBinOp b x y) loc ()

eAssign :: Maybe SourcePos ->  GExp t () -> GExp t () -> GExp t ()
eAssign loc x y = MkExp (EAssign x y) loc ()

eArrRead :: Maybe SourcePos ->  GExp t () -> GExp t () -> LengthInfo -> GExp t ()
eArrRead loc x y l = MkExp (EArrRead x y l) loc ()

eArrWrite :: Maybe SourcePos ->  GExp t () -> GExp t () -> LengthInfo -> GExp t () -> GExp t ()
eArrWrite loc x y l e = MkExp (EArrWrite x y l e) loc ()

eIter :: Maybe SourcePos -> GName t -> GName t -> GExp t () -> GExp t () -> GExp t ()
eIter loc x y e1 e2 = MkExp (EIter x y e1 e2) loc ()

eFor :: Maybe SourcePos -> UnrollInfo -> GName t -> GExp t () -> GExp t () -> GExp t () -> GExp t ()
eFor loc ui n e1 e2 e3 = MkExp (EFor ui n e1 e2 e3) loc ()

eLet :: Maybe SourcePos ->  GName t -> ForceInline -> GExp t () -> GExp t () -> GExp t ()
eLet loc x fi e1 e2 = MkExp (ELet x fi e1 e2) loc ()

eLetRef :: Maybe SourcePos ->  GName t -> Maybe (GExp t ()) -> GExp t () -> GExp t ()
eLetRef loc nm x e = MkExp (ELetRef nm x e) loc ()

eSeq :: Maybe SourcePos ->  GExp t () -> GExp t () -> GExp t ()
eSeq loc e1 e2 = MkExp (ESeq e1 e2) loc ()

eCall :: Maybe SourcePos ->  GName t -> [GExp t ()] -> GExp t ()
eCall loc f es = MkExp (ECall f es) loc ()

eIf :: Maybe SourcePos ->  GExp t () -> GExp t () -> GExp t () -> GExp t ()
eIf loc e1 e2 e3 = MkExp (EIf e1 e2 e3) loc ()

ePrint :: Maybe SourcePos ->  Bool -> GExp t () -> GExp t ()
ePrint loc b e = MkExp (EPrint b e) loc ()

eError :: Maybe SourcePos -> t -> String -> GExp t ()
eError loc t s = MkExp (EError t s) loc ()

eLUT :: Maybe SourcePos ->  Map (GName t) Range -> GExp t () -> GExp t ()
eLUT loc m e = MkExp (ELUT m e) loc ()

eBPerm :: Maybe SourcePos ->  GExp t () -> GExp t () -> GExp t ()
eBPerm loc e1 e2 = MkExp (EBPerm e1 e2) loc ()

eStruct :: Maybe SourcePos -> t -> [(String,GExp t ())] -> GExp t ()
eStruct loc tn es = MkExp (EStruct tn es) loc ()

eProj :: Maybe SourcePos ->  GExp t () -> String -> GExp t ()
eProj loc e s = MkExp (EProj e s) loc ()

eWhile :: Maybe SourcePos -> GExp t () -> GExp t () -> GExp t ()
eWhile loc econd ebody = MkExp (EWhile econd ebody) loc ()

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

mkFunDefined :: Maybe SourcePos
             -> GName t                       -- ^ name
             -> [GName t]                     -- ^ params
             -> GExp t ()                     -- ^ body
             -> GFun t ()
mkFunDefined loc nm params body = MkFun {
      unFun   = MkFunDefined nm params body
    , funLoc  = loc
    , funInfo = ()
    }

mkFunExternal :: Maybe SourcePos
              -> GName t                      -- ^ name
              -> [GName t]                    -- ^ params
              -> t                            -- ^ return type
              -> GFun t ()
mkFunExternal loc nm params ret = MkFun {
      unFun   = MkFunExternal nm params ret
    , funLoc  = loc
    , funInfo = ()
    }

{-------------------------------------------------------------------------------
  Computations
-------------------------------------------------------------------------------}

cVar :: Maybe SourcePos -> GName tc -> GComp tc t () ()
cVar loc x = MkComp (Var x) loc ()

cBindMany :: Maybe SourcePos -> GComp tc t () () -> [(GName t,GComp tc t () ())] -> GComp tc t () ()
cBindMany loc c cs = MkComp (mkBindMany c cs) loc () -- NB: using smart constructor

cSeq :: Maybe SourcePos -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cSeq loc c1 c2 = MkComp (Seq c1 c2) loc ()

cPar :: Maybe SourcePos -> ParInfo -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cPar loc pi c1 c2 = MkComp (Par pi c1 c2) loc ()

cLet :: Maybe SourcePos -> GName tc ->
        GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cLet loc x c1 c2 = MkComp (Let x c1 c2) loc ()

cLetE :: Maybe SourcePos -> GName t -> ForceInline ->
         GExp t () -> GComp tc t () () -> GComp tc t () ()
cLetE loc x fi e c = MkComp (LetE x fi e c) loc ()

cLetERef :: Maybe SourcePos -> GName t -> Maybe (GExp t ()) -> GComp tc t () () -> GComp tc t () ()
cLetERef loc x y c = MkComp (LetERef x y c) loc ()

cLetHeader :: Maybe SourcePos -> GFun t () -> GComp tc t () () -> GComp tc t () ()
cLetHeader loc f c = MkComp (LetHeader f c) loc ()

cLetFunC :: Maybe SourcePos -> GName tc -> [(GName (CallArg t tc))]
         -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cLetFunC loc x args c1 c2 = MkComp (LetFunC x args c1 c2) loc ()

cLetStruct :: Maybe SourcePos -> GStructDef t -> GComp tc t () () -> GComp tc t () ()
cLetStruct loc sd c = MkComp (LetStruct sd c) loc ()

cCall :: Maybe SourcePos -> GName tc -> [CallArg (GExp t ()) (GComp tc t () ())] -> GComp tc t () ()
cCall loc x es = MkComp (Call x es) loc ()

cEmit :: Maybe SourcePos -> t -> GExp t () -> GComp tc t () ()
cEmit loc t e = MkComp (Emit t e) loc ()

cEmits :: Maybe SourcePos -> t -> GExp t () -> GComp tc t () ()
cEmits loc t e = MkComp (Emits t e) loc ()

cReturn :: Maybe SourcePos -> t -> t -> ForceInline -> GExp t () -> GComp tc t () ()
cReturn loc t t' fi e = MkComp (Return t t' fi e) loc ()

cInterleave :: Maybe SourcePos -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cInterleave loc c1 c2 = MkComp (Interleave c1 c2) loc ()

cBranch :: Maybe SourcePos -> GExp t () -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cBranch loc e c1 c2 = MkComp (Branch e c1 c2) loc ()

cTake1 :: Maybe SourcePos -> t -> t -> GComp tc t () ()
cTake1 loc t t' = MkComp (Take1 t t') loc ()

cTake :: Maybe SourcePos -> t -> t -> Int -> GComp tc t () ()
cTake loc t t' n = MkComp (Take t t' n) loc ()

cUntil :: Maybe SourcePos -> GExp t () -> GComp tc t () () -> GComp tc t () ()
cUntil loc e c = MkComp (Until e c) loc ()

cWhile :: Maybe SourcePos -> GExp t () -> GComp tc t () () -> GComp tc t () ()
cWhile loc e c = MkComp (While e c) loc ()

cTimes :: Maybe SourcePos -> UnrollInfo -> GExp t () -> GExp t () -> GName t -> GComp tc t () () -> GComp tc t () ()
cTimes loc ui es elen x c = MkComp (Times ui es elen x c) loc ()

cRepeat :: Maybe SourcePos -> Maybe VectAnn -> GComp tc t () () -> GComp tc t () ()
cRepeat loc ann c = MkComp (Repeat ann c) loc ()

cVectComp :: Maybe SourcePos -> (Int,Int) -> GComp tc t () () -> GComp tc t () ()
cVectComp loc ann c = MkComp (VectComp ann c) loc ()

cMap :: Maybe SourcePos -> Maybe VectAnn -> GName t -> GComp tc t () ()
cMap loc ann nm = MkComp (Map ann nm) loc ()

cFilter :: Maybe SourcePos -> GName t -> GComp tc t () ()
cFilter loc nm = MkComp (Filter nm) loc ()

cReadSrc  :: Maybe SourcePos -> t -> GComp tc t () ()
cReadSrc loc t = MkComp (ReadSrc t) loc ()

cWriteSnk :: Maybe SourcePos -> t -> GComp tc t () ()
cWriteSnk loc t = MkComp (WriteSnk t) loc ()

cReadInternal  :: Maybe SourcePos -> t -> BufId -> ReadType -> GComp tc t () ()
cReadInternal  loc t bid rt = MkComp (ReadInternal t bid rt) loc ()

cWriteInternal :: Maybe SourcePos -> t -> BufId -> GComp tc t () ()
cWriteInternal loc t bid = MkComp (WriteInternal t bid) loc ()

cStandalone :: Maybe SourcePos -> GComp tc t () () -> GComp tc t () ()
cStandalone loc c = MkComp (Standalone c) loc ()

cMitigate :: Maybe SourcePos -> t -> Int -> Int -> GComp tc t () ()
cMitigate loc t n1 n2 = MkComp (Mitigate t n1 n2) loc ()
