{-# LANGUAGE TupleSections, ScopedTypeVariables, InstanceSigs #-}
module AtomInstantiation where

import AtomComp -- simplified Ziria model
import AutomataModel
import AstExpr
import Data.Loc
import Control.Exception

import Data.Maybe
import Data.Tuple
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Outputable 
import Text.PrettyPrint.HughesPJ

import AstUnlabelled

data SymAtom = SAExp (AExp ())
             | SACast String (Int,Ty) (Int,Ty) 
             | SADiscard String (Int,Ty)
             | SAAssert Bool -- ^ assert true and assert false
             | SARollback EId Int
             | SAClear (Map EId Int)
  deriving Eq

instance Outputable SymAtom where
  ppr (SAExp e) = ppr e

  ppr (SACast _ (n1,t1) (n2,t2)) 
    | t1 == t2  = assert (n1 == n2) (text "ID")
    | otherwise 
    = parens (pprTy n1 t1) <> text "-CAST-" <> parens (pprTy n2 t2)
    where pprTy 1 t = ppr t
          pprTy n t = ppr n <> text "*" <> ppr t

  ppr (SADiscard _ _) = text "DISCARD"

  ppr (SAAssert b) = text "ASSERT_" <> text (show b)

  ppr (SARollback q n) = text "ROLLBACK"

  ppr (SAClear pipes) = text "CLEAR"

instance Show SymAtom where
  show sa = render (pprShort sa)

pprShort :: SymAtom -> Doc
pprShort (SAExp e) =  pprFileLoc (aexp_lbl e)
pprShort sa = ppr sa

pprFileLoc =
  text . takeWhile (/= '$') .
  reverse . takeWhile (/= '\\') . takeWhile (/= '/') . reverse .
  render . ppr


{--------------- Instantiation of Atom class -------------}


instance Atom SymAtom where
  atomInTy (SAExp e) = map ((1,) . nameTyp) (aexp_ivs e)
  atomInTy (SACast _ inty _)  = [inty]
  atomInTy (SADiscard _ inty) = [inty]
  atomInTy (SAAssert _) = [(1,TBool)]
  atomInTy (SARollback _ _) = []
  atomInTy (SAClear pipes) = map (\(ch,n) -> (n, nameTyp ch)) $ Map.toList pipes

  atomOutTy (SAExp e) = map ((1,) . nameTyp) (aexp_ovs e)
  atomOutTy (SACast _ _ outty) = [outty]
  atomOutTy (SADiscard _ _)    = []
  atomOutTy (SAAssert _) = []
  atomOutTy (SARollback q n) = [(n,nameTyp q)]
  atomOutTy (SAClear _) = []

  castAtom    = SACast
  discardAtom = SADiscard
  assertAtom = SAAssert
  rollbackAtom = SARollback
  clearAtom = SAClear

  isRollbackAtom (SARollback q n) = Just (n,q)
  isRollbackAtom _ = Nothing

  expToWiredAtom :: AExp () -> Maybe EId -> WiredAtom SymAtom
  expToWiredAtom e mb_out = 
    let loc = expLoc body 
        body = aexp_exp e
    in WiredAtom { 
        wires_in  = map (1,) (aexp_ivs e)
      , wires_out = map (1,) (maybeToList mb_out ++ aexp_ovs e)
      , the_atom  = 
            case (unExp body,mb_out) of 
              (EVar x, Just retvar) 
                 -> SACast (aexp_lbl e) (1,nameTyp x) (1,nameTyp x)
              (_, Nothing) 
                 -> SAExp e { aexp_exp = eSeq loc body (eVal loc TUnit VUnit)
                            , aexp_ret = TUnit }
              (_, Just retvar)
                 -> SAExp e { aexp_exp = eAssign loc (eVar loc retvar) body
                            , aexp_ovs = retvar : aexp_ovs e 
                            , aexp_ret = TUnit }
      }
