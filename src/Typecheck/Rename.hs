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
-- | Renaming
--
-- The renamer has two responsibilities:
--
-- 1. Assigning uniqIds to names
-- 2. Translating (Maybe) SrcTy and (Maybe) SrcCTy to Ty and CTy. This involves:
--    a. Assigning type variables where there are no type annotations in the
--       source, primarily to free occurrences of names
--    b. Adding a unification constraint in the case of 'length(x)' in types
--       (where we need the type of x to be an array)
{-# OPTIONS_GHC -Wall #-}
module Rename (renProg, renComp, renExp) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Reader hiding (mapM)
import Data.Loc
import Data.Traversable (mapM)

import AstComp
import AstExpr
import AstUnlabelled
import TcMonad
import TcUnify
import Utils

{-------------------------------------------------------------------------------
  The heart of renaming: renaming variables

  NOTE: We do _NOT_ change the 'name' field, only the 'uniqId' field. We take
  advantage of this in the ext* functions, which only need to know the
  renamed name. Obviously, we also do name lookups by name rather than by
  uniqId (necessarily).
-------------------------------------------------------------------------------}

genRenBound :: MutKind 
            -> (SrcLoc -> ty -> TcM ty') -> GName ty -> TcM (GName ty')
genRenBound mk onTy nm = do
    uniqId'  <- genSym "_r"
    nameTyp' <- onTy (nameLoc nm) (nameTyp nm)
    return nm{uniqId = MkUniq $ uniqId', nameTyp = nameTyp', nameMut = mk}

renBound :: MutKind -> GName SrcTy -> TcM (GName Ty)
renBound mk = genRenBound mk renTyAnn

renCBound :: GName SrcCTy -> TcM (GName CTy)
renCBound = genRenBound Imm renCTyAnn

-- | Params are parsed with the right mutability kinds to start with
renParam :: GName SrcTy -> TcM (GName Ty)
renParam nm = genRenBound (nameMut nm) renTyAnn nm
renCAParam :: GName (CallArg SrcTy SrcCTy) -> TcM (GName (CallArg Ty CTy))
renCAParam nm = genRenBound (nameMut nm) renTyCTy_ann nm
  where 
    renTyCTy_ann :: SrcLoc -> CallArg SrcTy SrcCTy -> TcM (CallArg Ty CTy)
    renTyCTy_ann p = callArg (liftM CAExp . renTyAnn p) (liftM CAComp . renCTyAnn p)


genRenFree :: (GName ty -> TcM (GName ty')) -> GName ty -> TcM (GName ty')
genRenFree lookupTy nm = do
    nm' <- lookupTy nm
    -- NB: keep the mutability of the binding site but location of occurence
    return nm{uniqId = uniqId nm', nameTyp = nameTyp nm', nameMut = nameMut nm'}

renFree :: GName SrcTy -> TcM (GName Ty)
renFree = genRenFree $ \nm -> lookupEnv (name nm) (nameLoc nm)

renCFree :: GName SrcCTy -> TcM (GName CTy)
renCFree = genRenFree $ \nm -> lookupCEnv (name nm) (nameLoc nm)

{-------------------------------------------------------------------------------
  Recording newly renamed names

  NOTE: Computation function parameters are a bit of a special case: in the
  declaration of the function they will have type GName (CallArg ..), but when
  we rename the function body that CallArg should be resolved. For example, if
  the function header says

  > MkName { name == "x", nameTyp = CAExp TInt, .. }

  then we should record

  > MkName { name == "x", nameTyp = TInt, .. }

  when we rename the body.
-------------------------------------------------------------------------------}

recName :: GName Ty -> TcM x -> TcM x
recName  = recNames . (:[])

recNames :: [GName Ty] -> TcM x -> TcM x
recNames = extendEnv . map (name &&& id)

recCName :: GName CTy -> TcM x -> TcM x
recCName = recCNames . (:[])

recCNames :: [GName CTy] -> TcM x -> TcM x
recCNames = extendCEnv . map (name &&& id)

recCAName :: GName (CallArg Ty CTy) -> TcM x -> TcM x
recCAName = recCANames . (:[])

recCANames :: [GName (CallArg Ty CTy)] -> TcM x -> TcM x
recCANames params = let (names, cnames) = partitionParams params
                    in recNames names . recCNames cnames

{-------------------------------------------------------------------------------
  Renaming types
-------------------------------------------------------------------------------}


renReqTy :: SrcLoc -> SrcTy -> TcM Ty
renReqTy p = go
  where
    go SrcTUnit               = return TUnit
    go SrcTBit                = return TBit
    go SrcTBool               = return TBool
    go (SrcTArray numExpr ty) = TArray <$> renNumExpr p numExpr <*> go ty
    go (SrcTInt bw sg)        = flip TInt (renSignedness sg) <$> renBitWidth bw
    go SrcTDouble             = return TDouble
    go (SrcTStruct tn)        = do sdef <- lookupTDefEnv tn p
                                   return $ TStruct tn (struct_flds sdef)

    -- Internal types injected into the source language are used for
    -- quasi-quoting only, and therefore do not have to be rend.
    go (SrcInject ty)         = return ty

    go SrcTyUnknown           = panicStr "renReqTy: unknown type"

renReqGCTy :: SrcLoc -> GCTy SrcTy -> TcM CTy
renReqGCTy p = go
  where
    go (CTVar _)          = panicStr "Unexpected type variable in source type"
    go (CTComp u a b)     = CTComp  <$>
                            renReqTy p u <*> renReqTy p a <*> renReqTy p b
    go (CTTrans a b)      = CTTrans <$> renReqTy p a <*> renReqTy p b
    go (CTArrow args res) = CTArrow <$>
                            mapM (renTyCTy p) args <*> go res

renNumExpr :: SrcLoc -> SrcNumExpr -> TcM NumExpr
renNumExpr _ (SrcLiteral n) = return $ Literal n
renNumExpr _ (SrcNVar _)    = freshNumExpr "n"
renNumExpr p (SrcNArr nm)   = do
    nm' <- renFree nm
    (ne, _a) <- unifyTArray p Infer Infer (nameTyp nm')
    return ne

renBitWidth :: SrcBitWidth -> TcM BitWidth
renBitWidth SrcBW8  = return BW8
renBitWidth SrcBW16 = return BW16
renBitWidth SrcBW32 = return BW32
renBitWidth SrcBW64 = return BW64

renSignedness :: SrcSignedness -> Signedness
renSignedness SrcSigned    = Signed
renSignedness SrcUnsigned  = Unsigned

renStructDef :: SrcLoc -> GStructDef SrcTy -> TcM StructDef
renStructDef p (StructDef nm flds) = do
    flds' <- forM flds $ \(fld, ty) -> do
               ty' <- renReqTy p ty
               return (fld, ty')
    return StructDef { struct_name = nm
                     , struct_flds = flds'
                     }

renTyAnn :: SrcLoc -> SrcTy -> TcM Ty
renTyAnn _ SrcTyUnknown = freshTy "a"
renTyAnn p ty           = renReqTy p ty

renCTyAnn :: SrcLoc -> SrcCTy -> TcM CTy
renCTyAnn _ SrcCTyUnknown    = freshCTy "c"
renCTyAnn p (SrcCTyKnown ct) = renReqGCTy p ct


{-------------------------------------------------------------------------------
  Dealing with CallArg
-------------------------------------------------------------------------------}

renTyCTy :: SrcLoc -> CallArg (GArgTy SrcTy) (GCTy SrcTy) -> TcM (CallArg ArgTy CTy)
renTyCTy p = callArg on_exp (liftM CAComp . renReqGCTy p)
  where on_exp (GArgTy srcty m)
           = do ty <- renReqTy p srcty
                return (CAExp (GArgTy ty m))

renExpComp :: CallArg SrcExp SrcComp -> TcM (CallArg Exp Comp)
renExpComp = callArg (liftM CAExp . renExp) (liftM CAComp . renComp)

{-------------------------------------------------------------------------------
  Renaming computations
-------------------------------------------------------------------------------}

renProg :: SrcProg -> TcM Prog
renProg (MkProg comp) = MkProg <$> renComp comp

renComp :: SrcComp -> TcM Comp
renComp (MkComp comp0 cloc ()) = case comp0 of
      Var nm -> do
        nm' <- renCFree nm
        return $ cVar cloc nm'
      BindMany c1 xs_cs -> do
        c1'    <- renComp c1
        xs_cs' <- mapTelescope (recName . fst) renBind $ xs_cs
        return $ cBindMany cloc c1' xs_cs'
      Seq c1 c2 -> do
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cSeq cloc c1' c2'
      Par parInfo c1 c2 -> do
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cPar cloc parInfo c1' c2'
      Let x c1 c2 -> do
        c1' <- renComp c1
        x'  <- renCBound x
        c2' <- recCName x' $ renComp c2
        return $ cLet cloc x' c1' c2'
      LetStruct sdef c1 -> do
        sdef' <- renStructDef cloc sdef
        c1'   <- extendTDefEnv [sdef'] $ renComp c1
        return $ cLetStruct cloc sdef' c1'
      LetE x fi e c1 -> do
        e'  <- renExp e
        x'  <- renBound Imm x
        c1' <- recName x' $ renComp c1
        return $ cLetE cloc x' fi e' c1'
      LetERef x e c1 -> do
        e' <- mapM renExp e
        x' <- renBound Mut x
        recName x' $ do
          c1' <- renComp c1
          return $ cLetERef cloc x' e' c1'
      LetHeader fun c2 -> do
        fun' <- renFun fun
        c2'  <- recName (funName fun') $ renComp c2
        return $ cLetHeader cloc fun' c2'
      LetFunC nm params c1 c2 -> do
        nm'     <- renCBound nm
        params' <- mapTelescope recCAName renCAParam params
        c1'     <- recCANames params' $ renComp c1
        c2'     <- recCName nm' $ renComp c2
        return $ cLetFunC cloc nm' params' c1' c2'
      Call nm es -> do
        es' <- mapM renExpComp es
        nm' <- renCFree nm
        return $ cCall cloc nm' es'
      Emit e -> do
        e' <- renExp e
        return $ cEmit cloc e'
      Return fi e -> do
        e' <- renExp e
        return $ cReturn cloc fi e'
      Emits e -> do
        e' <- renExp e
        return $ cEmits cloc e'
      Interleave c1 c2 -> do
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cInterleave cloc c1' c2'
      Branch e c1 c2 -> do
        e'  <- renExp e
        c1' <- renComp c1
        c2' <- renComp c2
        return $ cBranch cloc e' c1' c2'
      Take1 a -> do
        a' <- renTyAnn cloc a
        return $ cTake1 cloc a'
      Take a n -> do
        a' <- renTyAnn cloc a
        return $ cTake cloc a' n
      Until e c' -> do
        e'  <- renExp e
        c'' <- renComp c'
        return $ cUntil cloc e' c''
      While e c' -> do
        e'  <- renExp e
        c'' <- renComp c'
        return $ cWhile cloc e' c''
      Times ui e elen nm c' -> do
        e'    <- renExp e
        elen' <- renExp elen
        nm'   <- renBound Imm nm -- NB: Immutable! 
        c''   <- recName nm' $ renComp c'
        return $ cTimes cloc ui e' elen' nm' c''
      Repeat wdth c' -> do
        c'' <- renComp c'
        return $ cRepeat cloc wdth c''
      VectComp wdth c' -> do
        c'' <- renComp c'
        return $ cVectComp cloc wdth c''
      Map wdth nm -> do
        nm' <- renFree nm
        return $ cMap cloc wdth nm'
      Filter f -> do
        f' <- renFree f
        return $ cFilter cloc f'
      ReadSrc ann -> do
        ann' <- renTyAnn cloc ann
        return $ cReadSrc cloc ann'
      WriteSnk ann -> do
        ann' <- renTyAnn cloc ann
        return $ cWriteSnk cloc ann'
      ReadInternal a s typ -> do
        a' <- renTyAnn cloc a
        return $ cReadInternal cloc a' s typ
      WriteInternal a s -> do
        a' <- renTyAnn cloc a
        return $ cWriteInternal cloc a' s
      Standalone l c' -> do
        c'' <- renComp c'
        return $ cStandalone cloc l c''
      Mitigate s ty n1 n2 -> do
        ty' <- renTyAnn cloc ty
        return $ cMitigate cloc s ty' n1 n2

{-------------------------------------------------------------------------------
  Renaming expressions
-------------------------------------------------------------------------------}

renExp :: SrcExp -> TcM Exp
renExp (MkExp exp0 eloc ()) = case exp0 of
      EVal t v -> do
        t' <- renTyAnn eloc t
        return $ eVal eloc t' v
      EValArr vs -> do
        vs' <- mapM renExp vs
        return $ eValArr eloc vs'
      EVar nm -> do
        nm' <- renFree nm
        return $ eVar eloc nm'
      EUnOp op e1 -> do
        op' <- renUnOp eloc op
        e1' <- renExp e1
        return $ eUnOp eloc op' e1'
      EBinOp op e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eBinOp eloc op e1' e2'
      EAssign e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eAssign eloc e1' e2'
      EArrRead e1 e2 r -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eArrRead eloc e1' e2' r
      EArrWrite e1 e2 r e3 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        e3' <- renExp e3
        return $ eArrWrite eloc e1' e2' r e3'
      EFor ui nm1 e1 e2 e3 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        nm1' <- renBound Imm nm1
        recName nm1' $ do
          e3' <- renExp e3
          return $ eFor eloc ui nm1' e1' e2' e3'
      EWhile e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eWhile eloc e1' e2'
      ELet nm1 fi e1 e2 -> do
        e1' <- renExp e1
        nm1' <- renBound Imm nm1
        recName nm1' $ do
          e2' <- renExp e2
          return $ eLet eloc nm1' fi e1' e2'
      ELetRef nm1 e1 e2 -> do
        e1'  <- mapM renExp e1
        nm1' <- renBound Mut nm1
        recName nm1' $ do
          e2' <- renExp e2
          return $ eLetRef eloc nm1' e1' e2'
      ESeq e1 e2 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        return $ eSeq eloc e1' e2'
      ECall f es -> do
        f'  <- renFree f
        es' <- mapM renExp es
        return $ eCall eloc f' es'
      EIf e1 e2 e3 -> do
        e1' <- renExp e1
        e2' <- renExp e2
        e3' <- renExp e3
        return $ eIf eloc e1' e2' e3'
      EPrint nl e1s -> do
        e1s' <- mapM renExp e1s
        return $ ePrint eloc nl e1s'
      EError a str -> do
        a' <- renTyAnn eloc a
        return $ eError eloc a' str
      ELUT r e1 -> do
        e1' <- renExp e1
        return $ eLUT eloc r e1'
      EStruct t tfs -> do
        t'   <- renReqTy eloc t
        tfs' <- mapM (\(f,e') -> renExp e' >>= \e'' -> return (f,e'')) tfs
        return $ eStruct eloc t' tfs'
      EProj e1 fn -> do
        e1' <- renExp e1
        return $ eProj eloc e1' fn

renUnOp :: SrcLoc -> GUnOp SrcTy -> TcM UnOp
renUnOp _ NatExp    = return NatExp
renUnOp _ Neg       = return Neg
renUnOp _ Not       = return Not
renUnOp _ BwNeg     = return BwNeg
renUnOp p (Cast ty) = Cast <$> renReqTy p ty
renUnOp _ ALength   = return ALength

renFun :: SrcFun -> TcM Fun
renFun (MkFun fun0 floc ()) = case fun0 of
    MkFunDefined nm params body -> do
      nm'     <- renBound Imm nm -- parameters are immutable ... revisit!
      params' <- mapTelescope recName renParam params
      body'   <- recNames params' $ renExp body
      return $ mkFunDefined floc nm' params' body'
    MkFunExternal nm params retTy -> do
      nm'     <- renBound Imm nm
      params' <- mapTelescope recName renParam params
      retTy'  <- recNames params' $ renReqTy floc retTy
      return $ mkFunExternal floc nm' params' retTy'

-- | The continuation of a monadic bind
renBind :: (GName SrcTy, SrcComp) -> TcM (GName Ty, Comp)
renBind (x, c) = do
    x' <- renBound Imm x
    c' <- recName x' $ renComp c
    return (x', c')
