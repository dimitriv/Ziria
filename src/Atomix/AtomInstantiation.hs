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

data SymAtom = SymAtom { atom_core :: !(Maybe Int)
                       , atom_kind :: !SymAtomKind
                       }
  deriving Eq

data SymAtomKind = SAExp !MbLocConstr !(AExp ())
                 | SACast !String !MbLocConstr !CastAtomOrigin !(Int,Ty) !(Int,Ty)
                 | SADiscard !String !MbLocConstr !(Int,Ty)
                 | SAAssert !Bool -- ^ assert true and assert false
                 | SARollback !EId !Int
                 | SAClear !(Map EId Int)
  deriving Eq

instance Outputable SymAtom where
  ppr = ppr . atom_kind

instance Outputable SymAtomKind where
  ppr (SAExp _ e) = ppr e

  ppr (SACast _ _ _ (n1,t1) (n2,t2)) 
    | t1 == t2  = assert (n1 == n2) (text "ID")
    | otherwise = parens (pprTy n1 t1) <> text "-CAST-" <> parens (pprTy n2 t2)
    where pprTy 1 t = ppr t
          pprTy n t = ppr n <> text "*" <> ppr t

  ppr (SADiscard _ _ _) = text "DISCARD"

  ppr (SAAssert b) = text "ASSERT_" <> text (show b)

  ppr (SARollback q n) = text "ROLLBACK"

  ppr (SAClear pipes) = text "CLEAR"

instance Show SymAtom where
  show sa = render (pprShort sa)

pprShort :: SymAtom -> Doc
pprShort (SymAtom _ (SAExp _ e)) = text $ cleanFileLoc $ aexp_lbl e
pprShort sa = ppr sa

cleanFileLoc :: String -> String
cleanFileLoc =
  takeWhile (/= '$') . dropWhile (== '$') .
  reverse .
  takeWhile (/= '\\') . dropWhile (== '\\') .
  takeWhile (/= '/') . dropWhile (== '/') .
  reverse


{--------------- Instantiation of Atom class -------------}

instance Atom SymAtom where
  atomInTy sa = case atom_kind sa of
    SAExp _ e           -> map ((1,) . nameTyp) (aexp_ivs e)
    SACast _ _ _ inty _ -> [inty]
    SADiscard _ _ inty  -> [inty]
    SAAssert _          -> [(1,TBool)]
    SARollback _ _      -> []
    SAClear pipes       -> map (\(ch,n) -> (n, nameTyp ch)) $ Map.toList pipes

  atomOutTy sa = case atom_kind sa of
    SAExp _ e            -> map ((1,) . nameTyp) (aexp_ovs e)
    SACast _ _ _ _ outty -> [outty]
    SADiscard _ _ _      -> []
    SAAssert _           -> []
    SARollback q n       -> [(n,nameTyp q)]
    SAClear _            -> []

  setCore coreId sa = SymAtom (Just coreId) (atom_kind sa)
  getCore = atom_core

  castAtom eid cstrt orig in_t out_t = SymAtom Nothing $ SACast eid cstrt orig in_t out_t
  discardAtom eid cstrt ty = SymAtom Nothing $ SADiscard eid cstrt ty
  assertAtom = SymAtom Nothing . SAAssert
  rollbackAtom ch n = SymAtom Nothing $ SARollback ch n
  clearAtom = SymAtom Nothing . SAClear

  isRollbackAtom (SymAtom _ (SARollback q n)) = Just (n,q)
  isRollbackAtom _ = Nothing

  wiredAtomId = wired_atom_id

  locConstrs (SymAtom _ (SAExp c _)) = c
  locConstrs (SymAtom _ (SACast _ c _ _ _)) = c
  locConstrs (SymAtom _ (SADiscard _ c _)) = c
  locConstrs _ = Nothing

  expToWiredAtom :: AExp () -> MbLocConstr -> Maybe EId -> WiredAtom SymAtom
  expToWiredAtom e cstrt mb_out = 
    let loc = expLoc body 
        body = aexp_exp e
    in WiredAtom { 
        wires_in  = map (1,) (aexp_ivs e)
      , wires_out = map (1,) (maybeToList mb_out ++ aexp_ovs e)
      , the_atom  = SymAtom Nothing $
          case (unExp body,mb_out) of
            (EVar x, Just retvar)
               -> SACast (aexp_lbl e) cstrt TakeOrEmitOrigin (1,nameTyp x) (1,nameTyp x)
            (_, Nothing)
               -> SAExp cstrt e { aexp_exp = eSeq loc body (eVal loc TUnit VUnit)
                                , aexp_ret = TUnit }
            (_, Just retvar)
               -> SAExp cstrt e { aexp_exp = eAssign loc (eVar loc retvar) body
                                , aexp_ovs = retvar : aexp_ovs e
                                , aexp_ret = TUnit }
      }



-- | The unique identifier of an atom
wired_atom_id :: WiredAtom SymAtom -> String
wired_atom_id (WiredAtom win _wout the_atom) = case atom_kind the_atom of
  SACast s _ _ _ _  -> "cast" ++ "$" ++ s
  SADiscard s _ _   -> "disc" ++ "$" ++ s
  SAExp _ ae        -> "aexp" ++ "$" ++ aexp_lbl ae
  SARollback qvar n -> "rbck" ++ "$" ++ show_uniq qvar ++ "$" ++ show n
  SAClear qsns      ->
     let qsns_list = Map.toList qsns
         u = concat $ 
             map (\(q,n) -> show_uniq q ++ "$" ++ show n ++ ":") qsns_list
     in "_clr" ++ "$" ++ u
  SAAssert b   -> let u = render (ppNameUniq (snd (head win)))
                  in "asrt" ++ "$" ++ u ++ "$" ++ show b
  where show_uniq = render . ppNameUniq
