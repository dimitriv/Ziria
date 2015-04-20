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
import Data.Loc
import AstExpr
import AstComp
import {-# SOURCE #-} LUTAnalysis

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

vint :: Integral a => a -> Val
-- Auxiliary function for use in the vectorizer
vint n = VInt (fromIntegral n)

eint32 :: Integral a => a -> Exp
eint32 n = eVal noLoc tint (vint n) 

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

eVal :: SrcLoc -> t -> Val -> GExp t ()
eVal loc t v = MkExp (EVal t v) loc ()

eValArr :: SrcLoc -> [GExp t ()] -> GExp t ()
eValArr loc v = MkExp (EValArr v) loc ()

eVar :: SrcLoc ->  GName t -> GExp t ()
eVar loc v = MkExp (EVar v) loc ()

eUnOp :: SrcLoc -> GUnOp t -> GExp t () -> GExp t ()
eUnOp loc o v = MkExp (EUnOp o v) loc ()

eBinOp :: SrcLoc -> BinOp -> GExp t () -> GExp t () -> GExp t ()
eBinOp loc b x y = MkExp (EBinOp b x y) loc ()

eAssign :: SrcLoc ->  GExp t () -> GExp t () -> GExp t ()
eAssign loc x y = MkExp (EAssign x y) loc ()

eArrRead :: SrcLoc ->  GExp t () -> GExp t () -> LengthInfo -> GExp t ()
eArrRead loc x y l = MkExp (EArrRead x y l) loc ()

eArrWrite :: SrcLoc ->  GExp t () -> GExp t () -> LengthInfo -> GExp t () -> GExp t ()
eArrWrite loc x y l e = MkExp (EArrWrite x y l e) loc ()

eFor :: SrcLoc -> UnrollInfo -> GName t -> GExp t () -> GExp t () -> GExp t () -> GExp t ()
eFor loc ui n e1 e2 e3 = MkExp (EFor ui n e1 e2 e3) loc ()

eLet :: SrcLoc ->  GName t -> ForceInline -> GExp t () -> GExp t () -> GExp t ()
eLet loc x fi e1 e2 = MkExp (ELet x fi e1 e2) loc ()

eLetRef :: SrcLoc ->  GName t -> Maybe (GExp t ()) -> GExp t () -> GExp t ()
eLetRef loc nm x e = MkExp (ELetRef nm x e) loc ()

eSeq :: SrcLoc ->  GExp t () -> GExp t () -> GExp t ()
eSeq loc e1 e2 = MkExp (ESeq e1 e2) loc ()

eCall :: SrcLoc ->  GName t -> [GExp t ()] -> GExp t ()
eCall loc f es = MkExp (ECall f es) loc ()

eIf :: SrcLoc ->  GExp t () -> GExp t () -> GExp t () -> GExp t ()
eIf loc e1 e2 e3 = MkExp (EIf e1 e2 e3) loc ()

ePrint :: SrcLoc ->  Bool -> [GExp t ()] -> GExp t ()
ePrint loc b e = MkExp (EPrint b e) loc ()

eError :: SrcLoc -> t -> String -> GExp t ()
eError loc t s = MkExp (EError t s) loc ()

eLUT :: SrcLoc ->  LUTStats -> GExp t () -> GExp t ()
eLUT loc m e = MkExp (ELUT m e) loc ()

eStruct :: SrcLoc -> t -> [(String,GExp t ())] -> GExp t ()
eStruct loc tn es = MkExp (EStruct tn es) loc ()

eProj :: SrcLoc ->  GExp t () -> String -> GExp t ()
eProj loc e s = MkExp (EProj e s) loc ()

eWhile :: SrcLoc -> GExp t () -> GExp t () -> GExp t ()
eWhile loc econd ebody = MkExp (EWhile econd ebody) loc ()

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

mkFunDefined :: SrcLoc
             -> GName t                       -- ^ name
             -> [GName t]                     -- ^ params
             -> GExp t ()                     -- ^ body
             -> GFun t ()
mkFunDefined loc nm params body = MkFun {
      unFun   = MkFunDefined nm params body
    , funLoc  = loc
    , funInfo = ()
    }

mkFunExternal :: SrcLoc
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

cVar :: SrcLoc -> GName tc -> GComp tc t () ()
cVar loc x = MkComp (Var x) loc ()

cBindMany :: SrcLoc -> GComp tc t () () -> [(GName t,GComp tc t () ())] -> GComp tc t () ()
cBindMany loc c cs = MkComp (mkBindMany c cs) loc () -- NB: using smart constructor

cSeq :: SrcLoc -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cSeq loc c1 c2 = MkComp (Seq c1 c2) loc ()

cPar :: SrcLoc -> ParInfo -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cPar loc pi c1 c2 = MkComp (Par pi c1 c2) loc ()

cLet :: SrcLoc -> GName tc ->
        GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cLet loc x c1 c2 = MkComp (Let x c1 c2) loc ()

cLetE :: SrcLoc -> GName t -> ForceInline ->
         GExp t () -> GComp tc t () () -> GComp tc t () ()
cLetE loc x fi e c = MkComp (LetE x fi e c) loc ()

cLetERef :: SrcLoc -> GName t -> Maybe (GExp t ()) -> GComp tc t () () -> GComp tc t () ()
cLetERef loc x y c = MkComp (LetERef x y c) loc ()

cLetHeader :: SrcLoc -> GFun t () -> GComp tc t () () -> GComp tc t () ()
cLetHeader loc f c = MkComp (LetHeader f c) loc ()

cLetFunC :: SrcLoc -> GName tc -> [(GName (CallArg t tc))]
         -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cLetFunC loc x args c1 c2 = MkComp (LetFunC x args c1 c2) loc ()

cLetStruct :: SrcLoc -> GStructDef t -> GComp tc t () () -> GComp tc t () ()
cLetStruct loc sd c = MkComp (LetStruct sd c) loc ()

cCall :: SrcLoc -> GName tc -> [CallArg (GExp t ()) (GComp tc t () ())] -> GComp tc t () ()
cCall loc x es = MkComp (Call x es) loc ()

cEmit :: SrcLoc -> GExp t () -> GComp tc t () ()
cEmit loc e = MkComp (Emit e) loc ()

cEmits :: SrcLoc -> GExp t () -> GComp tc t () ()
cEmits loc e = MkComp (Emits e) loc ()

cReturn :: SrcLoc -> ForceInline -> GExp t () -> GComp tc t () ()
cReturn loc fi e = MkComp (Return fi e) loc ()

cInterleave :: SrcLoc -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cInterleave loc c1 c2 = MkComp (Interleave c1 c2) loc ()

cBranch :: SrcLoc -> GExp t () -> GComp tc t () () -> GComp tc t () () -> GComp tc t () ()
cBranch loc e c1 c2 = MkComp (Branch e c1 c2) loc ()

cTake1 :: SrcLoc -> t -> GComp tc t () ()
cTake1 loc t = MkComp (Take1 t) loc ()

cTake :: SrcLoc -> t -> Int -> GComp tc t () ()
cTake loc t n = MkComp (Take t n) loc ()

cUntil :: SrcLoc -> GExp t () -> GComp tc t () () -> GComp tc t () ()
cUntil loc e c = MkComp (Until e c) loc ()

cWhile :: SrcLoc -> GExp t () -> GComp tc t () () -> GComp tc t () ()
cWhile loc e c = MkComp (While e c) loc ()

cTimes :: SrcLoc -> UnrollInfo -> GExp t () -> GExp t () -> GName t -> GComp tc t () () -> GComp tc t () ()
cTimes loc ui es elen x c = MkComp (Times ui es elen x c) loc ()

cRepeat :: SrcLoc -> Maybe VectAnn -> GComp tc t () () -> GComp tc t () ()
cRepeat loc ann c = MkComp (Repeat ann c) loc ()

cVectComp :: SrcLoc -> (Int,Int) -> GComp tc t () () -> GComp tc t () ()
cVectComp loc ann c = MkComp (VectComp ann c) loc ()

cMap :: SrcLoc -> Maybe VectAnn -> GName t -> GComp tc t () ()
cMap loc ann nm = MkComp (Map ann nm) loc ()

cFilter :: SrcLoc -> GName t -> GComp tc t () ()
cFilter loc nm = MkComp (Filter nm) loc ()

cReadSrc  :: SrcLoc -> t -> GComp tc t () ()
cReadSrc loc t = MkComp (ReadSrc t) loc ()

cWriteSnk :: SrcLoc -> t -> GComp tc t () ()
cWriteSnk loc t = MkComp (WriteSnk t) loc ()

cReadInternal  :: SrcLoc -> t -> BufId -> ReadType -> GComp tc t () ()
cReadInternal  loc t bid rt = MkComp (ReadInternal t bid rt) loc ()

cWriteInternal :: SrcLoc -> t -> BufId -> GComp tc t () ()
cWriteInternal loc t bid = MkComp (WriteInternal t bid) loc ()

cStandalone :: SrcLoc -> GComp tc t () () -> GComp tc t () ()
cStandalone loc c = MkComp (Standalone c) loc ()

cMitigate :: SrcLoc -> String -> t -> Int -> Int -> GComp tc t () ()
cMitigate loc s t n1 n2 = MkComp (Mitigate s t n1 n2) loc ()


