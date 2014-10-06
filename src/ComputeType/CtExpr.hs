-- | Compute the types of expressions
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module CtExpr (ctExp) where

import Control.Arrow (second)
import Text.PrettyPrint.HughesPJ

import AstExpr
import Outputable
import PpExpr ()
import Utils

{-------------------------------------------------------------------------------
  Compute types of expressions
-------------------------------------------------------------------------------}

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
ctExp0 (ELetRef _ _ _ e2)  = ctExp e2
ctExp0 (ESeq _ e2)         = ctExp e2
ctExp0 (ECall f xs)        = ctCall (ctExp f) (map ctExp xs)
ctExp0 (EIf _ a _)         = ctExp a
ctExp0 (EPrint _ _)        = TUnit
ctExp0 (EError ty _)       = ty
ctExp0 (ELUT _ e)          = ctExp e
ctExp0 (EBPerm e1 _)       = ctExp e1
ctExp0 (EStruct tn fs)     = TStruct tn (map (second ctExp) fs)
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
ctArrRead (TArr _ t) LISingleton  = t
ctArrRead (TArr _ t) (LILength n) = TArr (Literal n) t
ctArrRead ty _ = panic $ text "ctExp0:" <+> ppr ty

ctCall :: Ty -> [Ty] -> Ty
ctCall (TArrow args res) args' = apply (matchAll (zip args args')) res
ctCall t _ = panic $ text "ctCall:" <+> ppr t

ctProj :: Ty -> FldName -> Ty
ctProj (TStruct _ fs) n = lookup' n fs
ctProj t _ = panic $ text "ctProj:" <+> ppr t

{-------------------------------------------------------------------------------
  Polymorphic instantation of function arguments

  Note that the only kind of polymorphism that still exists after type checking
  is length polymorphism.
-------------------------------------------------------------------------------}

type Subst = [(Name, NumExpr)]

matchAll :: [(Ty, Ty)] -> Subst
matchAll = concatMap (uncurry match)

match :: Ty -> Ty -> Subst
match (TArr n t)    (TArr n' t')    = matchNumExpr n n' ++ match t t'
match (TArrow ts t) (TArrow ts' t') = matchAll (zip (t:ts) (t':ts'))
match _             _               = []

matchNumExpr :: NumExpr -> NumExpr -> Subst
matchNumExpr (NVar n _) e = [(n, e)]
matchNumExpr _          _ = []

apply :: Subst -> Ty -> Ty
apply s (TArr n t)    = TArr (applyNumExpr s n) (apply s t)
apply s (TArrow ts t) = TArrow (map (apply s) ts) (apply s t)
apply _ t             = t

applyNumExpr :: Subst -> NumExpr -> NumExpr
applyNumExpr s (NVar n _) = lookup' n s
applyNumExpr _ e          = e

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

lookup' :: (Outputable a, Eq a) => a -> [(a, b)] -> b
lookup' a dict =
  case lookup a dict of
    Nothing -> panic $ text "lookup:" <+> ppr a <+> text "not found"
    Just b  -> b
