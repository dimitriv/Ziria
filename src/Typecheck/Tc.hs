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
-- | Typecheck (and sanity check) core (rather than source)
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
module Tc ( tc ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif /* !MIN_VERSION_base(4,8,0) */
import Control.Monad
import Data.Loc
import Text.PrettyPrint.HughesPJ

import AstComp
import AstExpr
import Outputable
import TcMonad
import TcUnify
import Utils

import Data.Maybe ( isNothing, isJust, fromJust )

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

class Zonk a => Tc a where
  tc :: a -> TcM a

instance Tc (GProg CTy Ty a b) where
  tc prog = tcProg prog >> zonk prog

instance Tc (GComp CTy Ty a b) where
  tc comp = tcComp comp >> zonk comp

instance Tc (GExp Ty a) where
  tc expr = tcExp expr >> zonk expr

{-------------------------------------------------------------------------------
  The tcer proper
-------------------------------------------------------------------------------}

tcVal :: Val -> TcM Ty
tcVal = go
  where
    go (VBit _)    = return TBit
    go (VInt _ Signed)   = flip TInt Signed   <$> freshBitWidth "bw"
    go (VInt _ Unsigned) = flip TInt Unsigned <$> freshBitWidth "bw"    
    go (VDouble _) = return TDouble
    go (VBool _)   = return TBool
    go (VString _) = return TString
    go VUnit       = return TUnit

-- | Return the type of the result of applying a unary operator
--
-- TODO: The case for Neg is dubious: we should not check isScalarTy until all
-- unification constraints have been applied (where some of those constraints
-- might not be known at this point). See also comments for 'isScalarTy'.
tcUnOp :: SrcLoc -> GUnOp Ty -> Ty -> TcM Ty
tcUnOp loc uop = zonk >=> go uop
  where
    go Neg argTy = do
      let msg = text "Expected scalar type but got" <+> ppr argTy
      checkWith loc (isScalarTy argTy) msg
      return argTy
    go Not argTy = do
      unify loc argTy TBool
      return argTy
    go BwNeg argTy = do
      firstToSucceed (unifyTInt' loc argTy)
                     (unify loc argTy TBit)
      return argTy
    go (Cast targetTy) argTy = do
      compat_test <- case (targetTy, argTy) of
        (TBit,       TInt _ _)   -> return True
        (TInt _ _,     TBit)     -> return True
        (TDouble,    TInt _ _)   -> return True
        (TInt _ _,    TDouble)   -> return True
        (TInt _ _,     TInt _ _) -> return True
        (TStruct {}, TStruct {}) -> return $ isComplexTy targetTy && isComplexTy argTy
        -- Otherwise just try to unify
        (t1,         t2)         -> unify loc t1 t2 >> return True
      checkWith loc compat_test $
        text "Invalid cast from type" <+> ppr argTy <+> text "to" <+> ppr targetTy
      return targetTy
    go ALength argTy = do
      void $ unifyTArray loc Infer Infer argTy
      return tint
    go NatExp _argTy =
      error "typeCheckExpr: NatExp not supported!"

tcBinOp :: SrcLoc -> BinOp -> Ty -> Ty -> TcM Ty
tcBinOp loc bop = \argTy1 argTy2 -> do
    argTy1' <- zonk argTy1
    argTy2' <- zonk argTy2
    case () of
      () | isArithBinOp    bop -> goArith    argTy1' argTy2'
      () | isShiftBinOp    bop -> goShift    argTy1' argTy2'
      () | isLogicalBinOp  bop -> goLogical  argTy1' argTy2'
      () | isEqualityBinOp bop -> goEquality argTy1' argTy2'
      () | isRelBinOp      bop -> goRel      argTy1' argTy2'
      () | isBoolBinOp     bop -> goBool     argTy1' argTy2'
      () -> error $ "BUG: Forgot operator " ++ show bop ++ " in tcBinOp!"
  where
    -- Add / Sub / Mult / Div / Rem / Expon
    goArith argTy1 argTy2 = do
      -- liftIO $ putStrLn $ "argTy1 = " ++ show argTy1
      checkWith loc (supportsArithTy argTy1) mismatch
      unify loc argTy1 argTy2
      return argTy1

    -- ShL / ShR
    goShift argTy1 argTy2 = do
      unifyTInt' loc argTy1
      unifyTInt' loc argTy2
      return argTy1

    -- BwAnd / BwOr / BwXor (valid for either int or bit)
    goLogical argTy1 argTy2 = do
      unify loc argTy1 argTy2 -- order is important here!
      firstToSucceed (unifyTInt' loc argTy1)
                     (unify loc argTy1 TBit)
      return argTy1

    -- Eq / Neq
    goEquality argTy1 argTy2 = do
      checkWith loc (supportsEqTy argTy1) mismatch
      unify loc argTy1 argTy2
      return TBool

    -- Let / Leq / Gt / Geq
    goRel argTy1 argTy2 = do
      checkWith loc (supportsCmpTy argTy1) mismatch
      unify loc argTy1 argTy2
      return TBool

    -- And / Or
    goBool argTy1 argTy2 = do
      unify loc argTy1 TBool
      unify loc argTy2 TBool
      return TBool

    mismatch = text "Binary operator type mismatch:" <+> ppr bop

tcExp :: GExp Ty a -> TcM Ty
tcExp expr@(MkExp exp0 loc _) =
    pushErrCtx (ExprErrCtx expr) $ go exp0
  where
    go :: GExp0 Ty a -> TcM Ty
    go (EVal ty val) = do
      ty' <- tcVal val
      unify loc ty ty'
      return ty
    go (EValArr []) = do
      raiseErrNoVarCtx loc $ text "Empty array constant"
    go (EValArr vals) = do
      tys <- mapM tcExp vals
      unifyAll loc tys
      return $ TArray (Literal (length vals)) (head tys)
    go (EVar x) =
      return (nameTyp x)
    go (EUnOp uop e) = do
      ty <- tcExp e
      tcUnOp loc uop ty
    go (EBinOp bop e1 e2) = do
      ty1 <- tcExp e1
      ty2 <- tcExp e2
      tcBinOp loc bop ty1 ty2
    go (EAssign e1 e2) = do
      -- check e1 is a dereference expression
      when (isNothing $ isMutGDerefExp e1) $ do
        raiseErrNoVarCtx loc $
         text "Assignment to non-mutable dereference expression."
      -- otherwise type check and unify 
      ty1 <- tcExp e1
      ty2 <- tcExp e2
      unify loc ty1 ty2
      return TUnit
    go (EArrWrite arr idx li rhs) = do
      -- check arr is a dereference expression
      when (isNothing $ isMutGDerefExp arr) $ do
        raiseErrNoVarCtx loc $
         text "Assignment to non-mutable dereference expression."
      arrTy <- tcExp arr
      idxTy <- tcExp idx
      rhsTy <- tcExp rhs
      unifyTInt' loc idxTy
      (_n, a) <- unifyTArray loc Infer Infer arrTy
      case li of
        LISingleton 
          -> unify loc rhsTy a
        LILength m  
          -> void $ unifyTArray loc (Check (Literal m)) (Check a) rhsTy
        LIMeta x    
          -> raiseErrNoVarCtx loc $
               text "Unexpected meta-variable" <+> text (show x)
      return TUnit
    go (EArrRead arr idx li) = do
      arrTy <- tcExp arr
      idxTy <- tcExp idx
      unifyTInt' loc idxTy
      (_n, a) <- unifyTArray loc Infer Infer arrTy
      case li of
        LISingleton -> return a
        LILength m  -> return $ TArray (Literal m) a
        LIMeta   x  -> raiseErrNoVarCtx loc $
                         text "Unexpected meta-variable" <+> text (show x)
    go (EFor _ui x estart elen ebody) = do
      let xTy = nameTyp x
      unifyTInt' loc xTy
      estartTy <- tcExp estart
      elenTy   <- tcExp elen
      ebodyTy  <- tcExp ebody
      unifyAll loc [xTy, estartTy, elenTy]
      unify loc ebodyTy TUnit
      -- If type of x is not known after typecheck the body, default to tint32
      _ <- defaultTy loc xTy tint32
      return TUnit
    go (EWhile econd ebody) = do
      econdTy <- tcExp econd
      ebodyTy <- tcExp ebody
      unify loc econdTy TBool
      unify loc ebodyTy TUnit
      return TUnit
    go (ELet x _fi e1 e2) = do
      let xTy = nameTyp x
      e1Ty <- tcExp e1
      unify loc xTy e1Ty
      tcExp e2
    go (ELetRef x (Just e1) e2) = do
      let xTy = nameTyp x
      e1Ty <- tcExp e1
      unify loc xTy e1Ty
      tcExp e2
    go (ELetRef _x Nothing e2) =
      tcExp e2
    go (ESeq e1 e2) = do
      -- TODO: We might want to insist that e1 has type TUnit
      void $ tcExp e1
      tcExp e2
    go (ECall f args) = do
      -- The types of functions are always known before we call them
      -- (otherwise would not be able to call 'instantiateCall' here)
      fTy <- instantiateCall =<< zonk (nameTyp f)
      -- liftIO $ print $ vcat [ ppr f, ppr fTy ]
      case fTy of 
        TArrow funtys _funres -> do 
          tcCheckArgMuts loc f funtys args 
          checkWith loc (length funtys == length args) $
             text "Formal parameters vs. actual arguments length mismatch!"

          actual <- mapM tcExp args
          let actual' = zipWith (\t (GArgTy _ m) -> GArgTy t m) actual funtys
          res    <- freshTy "b"
          unify loc fTy $ TArrow actual' res
          return res
        _otherwise -> 
          -- Print variable context, as this is probably a shadowing bug
          raiseErr True loc $
            vcat [ text "Call to function" <+> ppr f 
                 , text "Which is of non-function type: " <+> ppr fTy 
                 , text "Maybe " <+> ppr f <+> 
                          text "is shadowed, or declared as non-function?" 
                 ]

    go (EIf be e1 e2) = do
      beTy <- tcExp be
      e1Ty <- tcExp e1
      e2Ty <- tcExp e2
      unify loc beTy TBool
      unify loc e1Ty e2Ty
      return e1Ty
    go (EPrint _ es) = do
      void $ mapM tcExp es
      return TUnit
    go (EError t _) =
      return t
    go (ELUT _ e) =
      -- TODO: Should we check anything about the types in the ranges?
      tcExp e
    go (EStruct t@(TStruct _ tys) flds) = do
      matchStructFields tys flds
      return t
    go (EStruct t _) = do
      panic $ text "EStruct with non-struct type" <+> ppr t
    go (EProj e fld) = do
      eTy <- zonk =<< tcExp e
      -- Since structs can share field names, we cannot infer a type here
      case eTy of
        TStruct nm flds ->
          case lookup fld flds of
            Nothing ->
              raiseErrNoVarCtx loc $
                text ("Unknown field " ++ fld ++ 
                          " projected out of type " ++ nm)
            Just fty ->
              return fty
        _other -> do
          raiseErrNoVarCtx loc $
            text "Field projection from non-struct type: " <+> ppr eTy

    -- TODO: This expects the struct fields to be in the same order as they
    -- are in the type defintion. This seems unnecessary?
    matchStructFields :: [(FldName, Ty)] -> [(FldName, GExp Ty a)] -> TcM ()
    matchStructFields [] [] =
      return ()
    matchStructFields ((fld, _):_) [] =
      raiseErrNoVarCtx loc $ text "Missing field" <+> text (show fld)
    matchStructFields [] ((fld', _):_) =
      raiseErrNoVarCtx loc $ text "Unexpected field" <+> text (show fld')
    matchStructFields ((fld, fldTy):tys) ((fld', fldExp):exps) = do
      unless (fld == fld') $
        raiseErrNoVarCtx loc $ vcat [
            text "Unexpected field" <+> text (show fld')
          , text "Expected field" <+> text (show fld)
          ]
      fldTy' <- tcExp fldExp
      unify (expLoc expr) fldTy fldTy'
      matchStructFields tys exps

tcComp :: GComp CTy Ty a b -> TcM CTy
tcComp comp@(MkComp comp0 loc _) =
    pushErrCtx (CompErrCtx comp) $ go comp0
  where
    go :: GComp0 CTy Ty a b -> TcM CTy
    go (Var c) =
      return (nameTyp c)

    go (Let x c1 c2) = do
      cty1 <- tcComp c1
      -- Important to check c1 before comparing to the type of x, because
      -- that might be a type variable.
      (mu, a, b) <- unifyCompOrTrans loc Infer Infer cty1
      case mu of
        Nothing -> void $ unifyTrans loc (Check a) (Check b) (nameTyp x)
        Just u -> void $ unifyComp loc (Check u) (Check a) (Check b) (nameTyp x)
      tcComp c2
    go (LetE x _fi e c) = do
      ety <- tcExp e
      unify loc (nameTyp x) ety
      tcComp c
    go (LetERef x (Just e) c) = do
      ety <- tcExp e
      unify loc (nameTyp x) ety
      tcComp c
    go (LetERef _x Nothing c) = do
      tcComp c
    go (LetHeader fun c) = do
      void $ tcFun fun
      tcComp c
    go (LetFunC f args body rhs) = do
      res <- tcComp body
      void $ ctyJoin loc (nameTyp f) $ CTArrow (map nameCallArgTy args) res
      tcComp rhs
    go (LetStruct _ c) =
      tcComp c
    go (Call f args) = do

      -- TODO: Why don't we need to instantiate here? Don't we support
      -- polymorphism in comp functions?

      fTy@(CTArrow funtys _funres) <- zonk (nameTyp f)

      -- Check that mutability agrees with deref exprs
      tcCheckCAArgMuts loc f funtys args

      checkWith loc (length funtys == length args) $ 
          text "Formal parameters vs. actual arguments length mismatch!"

      -- Get the actual types
      actual <- forM args $ \arg -> case arg of
                  CAExp  e -> CAExp  <$> tcExp e
                  CAComp c -> CAComp <$> tcComp c
      let munge (CAExp t) (CAExp (GArgTy _ m)) = return $ CAExp (GArgTy t m)
          munge (CAComp cty) _                 = return $ CAComp cty
          munge _ _ = raiseErrNoVarCtx loc $ 
            text "Expected computation but found expression argument."
      actual' <- zipWithM munge actual funtys

      res    <- freshCTy "b"
      void $ ctyJoin loc fTy $ CTArrow actual' res
      return res

    go (Until e c) = do
      ety <- tcExp e
      cty <- tcComp c
      unify loc ety TBool
      void $ unifyComp loc Infer Infer Infer cty
      return cty
    go (While e c) = do
      ety <- tcExp e
      cty <- tcComp c
      unify loc ety TBool
      void $ unifyComp loc Infer Infer Infer cty
      return cty
    go (Times _ui estart elen x c) = do
      estartTy <- tcExp estart
      elenTy   <- tcExp elen
      cty      <- tcComp c
      void $ unifyTInt loc Infer (nameTyp x)
      unifyAll loc [estartTy, elenTy, nameTyp x]
      void $ unifyComp loc Infer Infer Infer cty
      return cty
    go (Repeat _ann c) = do
      cty <- tcComp c
      -- The official typing rule says that the computation must return (),
      -- but we relax it here so that it's consistent with the typing of
      -- > c ; c ; c
      (_, a, b) <- unifyComp loc Infer Infer Infer cty
      return $ CTTrans a b
    go (VectComp _ c) =
      tcComp c
    go (Map _ f) = do
      a <- freshTy "a"
      b <- freshTy "b"
      let fTy = TArrow [(GArgTy a Imm)] b
      unify loc fTy (nameTyp f)
      return $ CTTrans a b
    go (Filter f) = do
      a <- freshTy "a"
      let fTy = TArrow [(GArgTy a Imm)] TBool
      unify loc fTy (nameTyp f)
      return $ CTTrans a a
    go (ReadSrc a) =
      return $ CTTrans (TBuff (ExtBuf a)) a
    go (WriteSnk a) =
      return $ CTTrans a (TBuff (ExtBuf a))
    go (ReadInternal a _bid _rt) =
      return $ CTTrans (TBuff (IntBuf a)) a
    go (WriteInternal a _bid) =
      return $ CTTrans a (TBuff (IntBuf a))
    go (Standalone c) =
      tcComp c
    go (Mitigate _ a n1 n2) =
      tcMitigator loc a n1 n2

    -- DV: Typing those in more restrictive ways
    go (Emit e) = do
      b <- tcExp e
      return $ CTComp TUnit TVoid b
    go (Emits e) = do
      ety <- tcExp e
      (_n, b) <- unifyTArray loc Infer Infer ety
      return $ CTComp TUnit TVoid b
    go (Return _fi e) = do
      u <- tcExp e
      return $ CTComp u TVoid TVoid
    go (Take1 a) = do
      return $ CTComp a a TVoid
    go (Take a n) = do
      return $ CTComp (TArray (Literal n) a) a TVoid

    -- DV: Combining the types of those here in less restrictive ways
    go (BindMany c []) =
      tcComp c
    go (BindMany c ((x, c'):cs)) = do
      -- Order here matters. See Note [Type checking order]
      cty1 <- tcComp c
      (_, a, b) <- unifyComp loc (Check (nameTyp x)) Infer Infer cty1
      cty2 <- go (BindMany c' cs)
      unifyCompOrTrans_CTy loc (Check a) (Check b) cty2
    go (Seq c1 c2) = do
      cty1 <- tcComp c1
      -- TODO: We might want to insist that c1 returns unit here
      --                         vvvvv
      (_, a, b) <- unifyComp loc Infer Infer Infer cty1
      cty2 <- tcComp c2
      unifyCompOrTrans_CTy loc (Check a) (Check b) cty2
    go (Par _pi c1 c2) = do
      cty1 <- tcComp c1
      cty2 <- tcComp c2
      (mu1, a, b) <- unifyCompOrTrans loc Infer Infer cty1
      case mu1 of
        Just u -> do -- c1 is a computer
          (_, c) <- unifyTrans loc (Check b) Infer cty2
          return $ CTComp u a c
        Nothing -> do -- c1 is a transformer
          (mu2, _, c) <- unifyCompOrTrans loc (Check b) Infer cty2
          case mu2 of
            Just u -> -- c2 is a computer
              return $ CTComp u a c
            Nothing ->
              return $ CTTrans a c

    go (Interleave c1 c2) = do
      cty1 <- tcComp c1
      cty2 <- tcComp c2
      (a, b) <- unifyTrans loc Infer Infer cty1
      unifyTrans_CTy loc (Check a) (Check b) cty2

    go (Branch e c1 c2) = do
      ety  <- tcExp e
      cty1 <- tcComp c1
      cty2 <- tcComp c2
      unify loc ety TBool
      ctyJoin loc cty1 cty2

tcCheckArgMut :: (Outputable w, Outputable t) 
              => SrcLoc -> GName w -> ArgTy -> GExp t a -> TcM ()
tcCheckArgMut loc fn (GArgTy _ Mut) earg = do 
  let md = isGDerefExp earg
  checkWith loc (isJust md) $ 
    vcat [ text "Function"  <+> ppr fn <+> text "expects mutable argument,"
         , text "but was given non-lvalue: " <+> ppr earg ]
  checkWith loc (isMutable (derefBase (fromJust md))) $
    vcat [ text "Function"  <+> ppr fn <+> text "expects mutable argument,"
         , text "but was given non-mutable argument:" <+> ppr earg ]
tcCheckArgMut _ _ _ _ = return ()


tcCheckArgMuts :: (Outputable w, Outputable t) 
               => SrcLoc -> GName w -> [ArgTy] -> [GExp t a] -> TcM ()
tcCheckArgMuts loc fn fun_tys args = mapM_ check_mut (zip fun_tys args)
  where check_mut (argty, earg) = tcCheckArgMut loc fn argty earg

tcCheckCAArgMuts :: (Outputable w, Outputable t) => SrcLoc
                 -> GName w
                 -> [CallArg ArgTy CTy]
                 -> [CallArg (GExp t b) (GComp tc t a b)] -> TcM ()
tcCheckCAArgMuts loc fn fun_tys args = mapM_ check_mut (zip fun_tys args)
  where check_mut (CAExp argty, CAExp earg) = tcCheckArgMut loc fn argty earg
        check_mut _ = return ()

tcFun :: GFun Ty a -> TcM Ty
tcFun (MkFun fun0 loc _) = go fun0
  where
    go :: GFun0 Ty a -> TcM Ty
    go (MkFunDefined f args body) = do
      res <- tcExp body
      unify loc (nameTyp f) $ TArrow (map nameArgTy args) res
      return (nameTyp f)
    go (MkFunExternal f args res) = do
      unify loc (nameTyp f) $ TArrow (map nameArgTy args) res
      return (nameTyp f)

tcMitigator :: SrcLoc -> Ty -> Int -> Int -> TcM CTy
tcMitigator loc a n1 n2 = do
    unless (n1 `divides` n2 || n2 `divides` n1) $
      raiseErrNoVarCtx loc $
             text "Invalid mitigator arguments"
         <+> parens (hsep (punctuate comma [int n1, int n2]))
    return $ CTTrans (toArray n1) (toArray n2)
  where
    toArray :: Int -> Ty
    toArray 1 = a
    toArray n = TArray (Literal n) a

tcProg :: GProg CTy Ty a b -> TcM CTy
tcProg (MkProg comp) = tcComp comp

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

divides :: Int -> Int -> Bool
n `divides` m = m `mod` n == 0

{-------------------------------------------------------------------------------
  Note [Order of type checking]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  In an ideal world the order of type checking would not matter. We would just
  create unification constraints and solve them in an arbitrary order. Sadly,
  the world is not ideal. For example, when we a field projection out of a
  struct

  > x.foo

  or when we add two scalars

  > x + y

  or in a number of other cases, we cannot create a unification constraint, but
  instead need to know the type of the variable _now_. For example, in
  EProj we do

  > go (EProj e fld) = do
  >   eTy <- zonk =<< tcExp e
  >   -- Since structs can share field names, we cannot infer a type here
  >   case eTy of
  >     TStruct nm flds -> ...
  >     _ -> raiseErrNoVarCtx ...

  this means that we need to make sure to collect as many constraints about 'e'
  as possible _before_ type checking the projection. For example, consider
  type checking

  > d <- foo ()
  > emit d.foo

  If in the case for 'bind' we check the continuation before unifying the type
  of the bound variable with the result type of the first computation, then
  at the point where we have to typecheck @d.foo@ we will not yet know the
  type of @d@ and hence we will give an error message such as

  > Field projection from non-struct type:  ?a17
  > When type checking expression:
  >   d.data
-------------------------------------------------------------------------------}
