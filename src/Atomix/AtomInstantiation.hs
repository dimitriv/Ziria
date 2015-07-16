{-# LANGUAGE TupleSections, ScopedTypeVariables, InstanceSigs #-}
module AtomInstantiation where

import AtomComp -- simplified Ziria model
import AutomataModel
import AstExpr
import Data.Loc
import Control.Exception

import Data.Maybe
import qualified Data.List as List

import Outputable 
import Text.PrettyPrint.HughesPJ


data SymAtom = SAExp (AExp ())
             | SAExpIgnoreRet (AExp ())
             | SACast (Int,Ty) (Int,Ty) 
             | SADiscard (Int,Ty)
             | SAAssert Bool -- assert true and assert false
  deriving Eq

instance Outputable SymAtom where
  ppr (SAExp e) = ppr e
  ppr (SAExpIgnoreRet e) = ppr e

  ppr (SACast (n1,t1) (n2,t2)) 
    | t1 == t2  = assert (n1 == n2) (text "ID")
    | otherwise = parens (pprTy n1 t1) <> text "-CAST-" <> parens (pprTy n2 t2)
    where pprTy 1 t = ppr t
          pprTy n t = ppr n <> text "*" <> ppr t

  ppr (SADiscard _) = text "DISCARD"

  ppr (SAAssert b) = text "ASSERT_" <> text (show b)

instance Show SymAtom where
  show sa = render (pprShort sa)

pprShort :: SymAtom -> Doc
pprShort (SAExp e) =  pprFileLoc (aexp_lbl e)
pprShort (SAExpIgnoreRet e) = pprFileLoc (aexp_lbl e)
pprShort sa = ppr sa

pprFileLoc =
  text . takeWhile (/= '$') .
  reverse . takeWhile (/= '\\') . takeWhile (/= '/') . reverse .
  render . ppr


{--------------- Instantiation of Atom class -------------}


instance Atom SymAtom where
  atomInTy (SAExp e) = map ((1,) . nameTyp) (aexp_ivs e)
  atomInTy (SAExpIgnoreRet e) = map ((1,) . nameTyp) (aexp_ivs e)
  atomInTy (SACast inty _)  = [inty]
  atomInTy (SADiscard inty) = [inty]
  atomInTy (SAAssert _) = [(1,TBool)]

  atomOutTy (SAExp e) = (1, aexp_ret e) : map ((1,) . nameTyp) (aexp_ovs e)
  atomOutTy (SAExpIgnoreRet e) = map ((1,) . nameTyp) (aexp_ovs e)
  atomOutTy (SACast _ outty) = [outty]
  atomOutTy (SADiscard _)    = []
  atomOutTy (SAAssert _) = []

  castAtom    = SACast
  discardAtom = SADiscard
  assertAtom = SAAssert

  expToWiredAtom :: AExp () -> Maybe EId -> WiredAtom SymAtom
  expToWiredAtom e mb_out = 
      WiredAtom { wires_in  = map (1,) (aexp_ivs e)
                , wires_out = map (1,) (maybeToList mb_out ++ aexp_ovs e)
                , the_atom  = (if isNothing mb_out then SAExpIgnoreRet else SAExp) e
                }