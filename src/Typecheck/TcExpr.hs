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
module TcExpr (
    tyCheckExpr
  , tyCheckBound
  , tyCheckFree
  , trTy
  , trReqTy
  , unifyAnn
  ) where

import Control.Applicative
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJ

import AstExpr
import AstUnlabelled
import Outputable
import PpExpr ()
import TcErrors
import TcMonad
import TcUnify
import Lint -- We reuse some code from the linter to avoid duplication

{-------------------------------------------------------------------------------
  Variables, values, expressions
-------------------------------------------------------------------------------}

-- | Free variables
tyCheckFree :: GName (Maybe SrcTy) -> TcM (GName Ty, Ty)
tyCheckFree nm = do
  t <- lookupEnv (name nm) (nameLoc nm)
  unifyAnn (nameLoc nm) (nameTyp nm) t
  return (nm{nameTyp = t}, t)

-- | Bound variables
--
-- Bound variables are assigned a type variable as type unless they have an
-- explicit type annotation
tyCheckBound :: GName (Maybe SrcTy) -> TcM (GName Ty, Ty)
tyCheckBound nm = do
  a <- freshTy "a"
  unifyAnn (nameLoc nm) (nameTyp nm) a
  return (nm{nameTyp = a}, a)

-- | Expressions
--
-- TODO: In numerous places (here as well as in TcComp) we do something like
-- the following:
--
-- > ty' <- zonk ty
-- > case ty of
-- >   TArray n tbase -> ...
-- >   _              -> raiseErrNoVarCtx ..
--
-- Shouldn't we replace this pattern (everywhere) with
--
-- > a <- TVar <$> newTyVar "a"
-- > n <- NVar <$> newALenVar "n"
-- > unify loc ty (TArray a n)
--
-- And then proceed with `a` and `n`?
tyCheckExpr :: SrcExp -> TcM (Exp, Ty)
tyCheckExpr e
  = do { let loc = expLoc e
       ; pushErrCtx (ExprErrCtx e) $
         case unExp e of

          EVal ann v ->
            do { t <- lintVal v
               ; unifyAnn loc ann t
               ; return (eVal loc t v, t)
               }
          EValArr _ann a@(h:t) ->
            do { vtys <- mapM lintVal (h:t)
               ; unifyAll loc vtys
               ; let arrty = TArray (Literal (length a)) (head vtys)
               ; return $ (eValArr loc arrty a, arrty)
               }
          EValArr _ [] ->
            raiseErrNoVarCtx loc $ text ("Empty value array.")

          EVar x ->
            do { (x', ty) <- tyCheckFree x
               ; return $ (eVar loc x', ty)
               }

          EUnOp uop e0 ->
            do { uop'       <- trUnOp loc uop
               ; (e0', ty0) <- tyCheckExpr e0
               ; ty0'       <- lintUnOp loc uop' ty0
               ; return (eUnOp loc uop' e0', ty0')
               }

          EBinOp bop e1 e2 ->
            do { (e1', ty1) <- tyCheckExpr e1
               ; (e2', ty2) <- tyCheckExpr e2
               ; resTy <- lintBinOp loc bop ty1 ty2
               ; return (eBinOp loc bop e1' e2', resTy)
               }

          EAssign e1 e2 ->
            do { (e1', ty1) <- tyCheckExpr e1
               ; (e2', ty2) <- tyCheckExpr e2
               ; unify (expLoc e1') ty1 ty2
               ; return (eAssign loc e1' e2', TUnit)
               }

          EArrRead earr eix len ->
            do { (earr', tyarr) <- tyCheckExpr earr
               ; (eix',  tyix)  <- tyCheckExpr eix
               ; tyarr' <- zonk tyarr
               ; tyix'  <- zonk tyix -- TODO: Why is this zonking necessary?
               ; unifyTInt' loc tyix'
               ; case tyarr' of
                  TArray _n tbase -> -- TODO: support metavars for array size here
                   case len of
                     LISingleton
                       -> return (eArrRead loc earr' eix' LISingleton, tbase)

                     LILength r
                       -> return (eArrRead loc earr' eix' (LILength r), TArray (Literal r) tbase)

                  _ -> raiseErrNoVarCtx loc (expActualErr unknownTArr tyarr' earr)
               }

          EBPerm earr eperm ->
           do { (earr', tyarr) <- tyCheckExpr earr
              ; (eix',  tyix)  <- tyCheckExpr eperm
              ; tyarr' <- zonk tyarr
              ; tyix'  <- zonk tyix  -- TODO: Why is this zonking necessary?
              ; case tyarr' of
                  TArray n TBit ->
                    do { ti <- TInt <$> freshBitWidth "bw"
                       ; unify loc tyix' (TArray n ti)
                       ; return (eBPerm loc earr' eix', tyarr')
                       }
                  _ -> raiseErrNoVarCtx loc $
                       expActualErr (unknownTArrOfBase TBit) tyarr' earr
              }

          EArrWrite earr eix r eval ->
           do { (earr', tyarr) <- tyCheckExpr earr
              ; (eval', tyval) <- tyCheckExpr eval
              ; (eix',  tyix)  <- tyCheckExpr eix
              ; unifyTInt' loc tyix
              ; tyarr' <- zonk tyarr
              ; tyval' <- zonk tyval -- TODO: Why is this zonking necessary?
              ; case tyarr' of
                  TArray n tbase ->
                    case r of
                      LISingleton
                        -> do { unify loc tbase tyval'
                              ; return (eArrWrite loc earr' eix' r eval', TUnit)
                              }
                      LILength l -> case n of
                        Literal nn | nn < l
                          -> raiseErrNoVarCtx loc $
                               vcat [ text "Array write operation interval " <+> int l
                                    , text "greater than array size" <+> int nn
                                    ]
                        _ -> do { unify loc (TArray (Literal l) tbase) tyval'
                                ; return (eArrWrite loc earr' eix' r eval', TUnit)
                                }
                  _ ->
                    raiseErrNoVarCtx loc $
                    expActualErr unknownTArr tyarr earr
              }

          EFor ui ix estart elen ebody ->
            do (ix',     tyix)    <- tyCheckBound ix
               (estart', tystart) <- tyCheckExpr estart
               (elen',   tylen)   <- tyCheckExpr elen

               unifyAll loc [tyix, tystart, tylen]

               (ebody', tybody) <- extendEnv [ix'] $ tyCheckExpr ebody
               unify loc tybody TUnit
               return (eFor loc ui ix' estart' elen' ebody', TUnit)

          EWhile econd ebody ->
            do (econd', tycond) <- tyCheckExpr econd
               (ebody', tybody) <- tyCheckExpr ebody
               unify loc tycond TBool
               unify loc tybody TUnit
               return (eWhile loc econd' ebody', TUnit)

          EIter ix x earr ebody ->
            do { (ix',   tyix)  <- tyCheckBound ix
               ; (x',    tyx)   <- tyCheckBound x
               ; (earr', tyarr) <- tyCheckExpr earr
               ; tyarr' <- zonk tyarr
               ; case tyarr' of
                   TArray _n tbase ->
                      do { -- ix must range over an int type
                         ; unifyTInt' loc tyix
                           -- x must range of the base type of the array
                         ; unify loc tyx tbase
                         ; (ebody', tybody) <- extendEnv [ix', x'] $
                                                 tyCheckExpr ebody
                           -- If the type of ix is not yet known after we
                           -- typecheck the body, default to tint32
                         ; _tyix' <- defaultTy loc tyix tint32
                         ; unify loc tybody TUnit
                         ; return (eIter loc ix' x' earr' ebody', TUnit)
                         }
                   _ -> raiseErrNoVarCtx loc $
                        expActualErr unknownTArr tyarr earr
              }

          ELet x fi e1 e2 ->
            do { (x',  tyx)  <- tyCheckBound x
               ; (e1', tye1) <- tyCheckExpr e1
               ; unify loc tyx tye1
               ; (e2', tye2) <- extendEnv [x'] $ tyCheckExpr e2
               ; return (eLet loc x' fi e1' e2', tye2)
               }

          ELetRef x (Just e1) e2 ->
            do { (x',  tyx)  <- tyCheckBound x
               ; (e1', tye1) <- tyCheckExpr e1
               ; unify loc tyx tye1
               ; (e2', tye2) <- extendEnv [x'] $ tyCheckExpr e2
               ; return (eLetRef loc x' (Just e1') e2', tye2)
               }

          ELetRef x Nothing e2 ->
            do { (x',  _)    <- tyCheckBound x
               ; (e2', tye2) <- extendEnv [x'] $ tyCheckExpr e2
               ; return (eLetRef loc x' Nothing e2', tye2)
               }

          ESeq e1 e2 ->
            do { (e1', _ty1) <- tyCheckExpr e1
               ; (e2', ty2)  <- tyCheckExpr e2
               ; return (eSeq loc e1' e2', ty2)
               }

          ECall f actual ->
            do { (f', fty_poly) <- tyCheckFree f
               ; actual' <- tyCheckExprs actual
                  -- Zonking and then instantiating the function type at this
                  -- point is okay for two reasons:
                  --
                  -- (i)  Functions must be defined (and hence type checked)
                  --      before they are used
                  -- (ii) Ziria does does not support recursion
                  --
                  -- This means that if a function type still has unification
                  -- variables in it after type checking the function, then
                  -- the function is indeed polymorphic in these variables.
               ; fty_poly' <- zonk fty_poly
--               ; liftIO $ putStrLn $ "Before instantiation = " ++ show fty_poly'
               ; fty_inst  <- instantiateCall fty_poly'
--               ; liftIO $ putStrLn $ "After instantiation = " ++ show fty_inst
               ; case fty_inst of
                   TArrow formal res ->
                     do { checkWith loc (length formal == length actual') $
                          vcat [ text "Function" <+> ppr f
                               , text "Expecting" <+> int (length formal) <+>
                                                      text "arguments"
                               , text "but was given" <+> int (length actual')
                               ]

                          -- BOZIDAR: change below to enable polymorphic arrays
                          -- unify loc $ zip (tyListOfProd ta) (map info es2')

                        ; unifyMany loc formal (map snd actual')
                        ; return (eCall loc f' (map fst actual'), res)
                        }
                   _ -> raiseErrNoVarCtx loc $
                        expActualErr (unknownTFun (length actual')) fty_poly' e
               }

          EIf be e1 e2 ->
            do { (be', tyb) <- tyCheckExpr be
               ; (e1', ty1) <- tyCheckExpr e1
               ; (e2', ty2) <- tyCheckExpr e2
               ; unify loc tyb TBool
               ; unify loc ty1 ty2
               ; return (eIf loc be' e1' e2', ty1)
               }

          EPrint nl e1 ->
            do { (e1', _ty1) <- tyCheckExpr e1
               ; return (ePrint loc nl e1', TUnit)
               }

          EError ann str ->
            do { a <- freshTy "a"
               ; unifyAnn loc ann a
               ; return (eError loc a str, a)
               }

          ELUT _r _e1 ->
            do { -- Currently we don't expect any ELUT in source terms. If we
                 -- do add support for them, we need to tr the types
                 -- in the range keys.
               ; raiseErrNoVarCtx loc $ text "Unexpected LUT"
               }

          EProj e1 fn ->
            do { (e', ty1)  <- tyCheckExpr e1
               ; ty' <- zonk ty1
               ; case ty' of
                  TStruct nm flds
                    -> do { case lookup fn flds of
                              Nothing -> raiseErrNoVarCtx loc $
                                         text ("Unknown field " ++ fn ++ " projected out of type " ++ nm)
                              Just fty -> return (eProj loc e' fn, fty)
                          }
                  _other -> raiseErrNoVarCtx loc $
                            text "Field projection from non-struct type: " <+> ppr ty'
               }

          EStruct tn tfs ->
            do { struct <- lookupTDefEnv tn loc
                 -- TODO: This requires the order of the fields to match the
                 -- order in the definition. That seems unnecessary.
               ; let tc_field ((f,fe),(f',fty))
                       | f == f'
                       = do { (fe', fty') <- tyCheckExpr fe
                            ; unify loc fty' fty
                            ; return (f, fe')
                            }
                       | otherwise
                       = raiseErrNoVarCtx loc $
                         text ("Expecting field " ++ f' ++ " but got " ++ f)

               ; tfs' <- mapM tc_field (zip tfs (struct_flds struct))
               ; return (eStruct loc tn tfs', TStruct tn (struct_flds struct))
               }
       }

tyCheckExprs :: [SrcExp] -> TcM [(Exp, Ty)]
tyCheckExprs = mapM tyCheckExpr

{-------------------------------------------------------------------------------
  Translate from SrcTy to Ty
-------------------------------------------------------------------------------}

-- | Unify a type with an optional type annotation in the source
unifyAnn :: Maybe SourcePos -> Maybe SrcTy -> Ty -> TcM ()
unifyAnn _ Nothing     _   = return ()
unifyAnn p (Just ann) ty2 = do
  ty1 <- trTy p ann
  unify p ty1 ty2

-- | Translate source types to internal types
--
-- NOTE: Source int types are always fully specified. The only place where
-- we don't know the bitwidth in source terms is in literals.
trTy :: Maybe SourcePos -> SrcTy -> TcM Ty
trTy _ SrcTUnit        = return TUnit
trTy _ SrcTBit         = return TBit
trTy _ SrcTBool        = return TBool
trTy p (SrcTArray n t) = do n' <- trNumExpr p n
                            t' <- trTy p t
                            return (TArray n' t')
trTy _ (SrcTInt bw)    = do bw' <- trBitWidth bw
                            return $ TInt bw'
trTy _ SrcTDouble      = return TDouble
trTy p (SrcTStruct tn) = do sdef <- lookupTDefEnv tn p
                            return $ TStruct tn (struct_flds sdef)
trTy _ (SrcInject ty)  = return ty

trNumExpr :: Maybe SourcePos -> SrcNumExpr -> TcM NumExpr
trNumExpr _ (SrcLiteral n) = return $ Literal n
trNumExpr p (SrcNArr x)    = do (_x', xt) <- tyCheckFree x
                                (ne, _a) <- unifyTArray p Infer Infer xt
                                return ne
trNumExpr _ (SrcNVar _p')  = freshNumExpr "n"

trBitWidth :: SrcBitWidth -> TcM BitWidth
trBitWidth SrcBW8  = return BW8
trBitWidth SrcBW16 = return BW16
trBitWidth SrcBW32 = return BW32
trBitWidth SrcBW64 = return BW64

-- | Translate requried type annotations
--
-- The source language has Maybe SrcTy as type annotations, but in a few
-- places these type annotations are not in fact optional. The parser will
-- enforce that they are given, but we give a type error if somehow they
-- are missing anyway.
trReqTy :: String -> Maybe SourcePos -> Maybe SrcTy -> TcM Ty
trReqTy _    p (Just ty) = trTy p ty
trReqTy desc p Nothing   = raiseErrNoVarCtx p $ text ("Missing type for " ++ desc)

trUnOp :: Maybe SourcePos -> GUnOp (Maybe SrcTy) -> TcM (GUnOp Ty)
trUnOp _ NatExp    = return NatExp
trUnOp _ Neg       = return Neg
trUnOp _ Not       = return Not
trUnOp _ BwNeg     = return BwNeg
trUnOp p (Cast ty) = Cast <$> trReqTy "cast" p ty
trUnOp _ ALength   = return ALength
