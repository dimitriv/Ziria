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
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module TcMonad where

import Prelude hiding (exp)
import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad.State
import Text.Parsec.Pos
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as M
import qualified Data.Set as S

import AstComp
import AstExpr
import Outputable
import PpExpr (ppName)
import TcErrors
import qualified GenSym as GS


isArrowTy :: Ty -> Bool
isArrowTy t =
  case t of
    TArrow _ _ -> True
    _ -> False


-- Maps names to structure definitions
type TyDefEnv = M.Map String StructDef

mkTyDefEnv :: [(TyName,StructDef)] -> TyDefEnv
mkTyDefEnv = M.fromList


-- maps type variables to types
type TyEnv = M.Map String Ty

mkTyEnv :: [(String,Ty)] -> TyEnv
mkTyEnv = M.fromList

--- maps program variables to types
type Env = M.Map String Ty

mkEnv :: [(String,Ty)] -> Env
mkEnv = M.fromList

-- maps computation variables to computation types
type CEnv = M.Map String CTy

mkCEnv :: [(String,CTy)] -> CEnv
mkCEnv = M.fromList

-- maps array lengths to ints
type ALenEnv = M.Map LenVar NumExpr

mkALenEnv :: [(LenVar, NumExpr)] -> ALenEnv
mkALenEnv = M.fromList

-- Maps max array lengths to ints
-- This is used to store the largest array size used in a function call
-- When the function is generated, the buffer for the array will use this size
type AMaxLenEnv = M.Map (GName Ty) NumExpr

mkAMaxLenEnv :: [(GName Ty, NumExpr)] -> AMaxLenEnv
mkAMaxLenEnv = M.fromList

-- maps bitwidth precisions to BitWidths
type BWEnv = M.Map BWVar BitWidth

mkBWEnv :: [(BWVar,BitWidth)] -> BWEnv
mkBWEnv = M.fromList

data Ct
 = BaseTyCt { ct_pos  :: Maybe SourcePos
            , ct_ty   :: Ty
            , ct_base :: Ty }

-- The state of the type checker monad
data TcMState
  = TcMState { tcm_tyenv       :: TyEnv
             , tcm_alenenv     :: ALenEnv
             , tcm_amaxlenenv  :: AMaxLenEnv
             , tcm_bwenv       :: BWEnv
             }



emptyTcMState :: TcMState
emptyTcMState
  = TcMState { tcm_tyenv      = mkTyEnv []
             , tcm_alenenv    = mkALenEnv []
             , tcm_amaxlenenv = mkAMaxLenEnv []
             , tcm_bwenv      = mkBWEnv []
             }


data TcM a
  = TcM { runTcM :: TyDefEnv     -- type (struct) definitions
                 -> Env          -- term variables
                 -> CEnv         -- computation variables
                 -> GS.Sym
                 -> ErrCtx       -- A context for error reporting
                 -> TcMState     -- State
                 -> IO (Either Doc (a, TcMState)) }


instance Functor TcM where
  fmap f (TcM x) = TcM $ \tenv env cenv sym ctx st -> do
    res <- x tenv env cenv sym ctx st
    case res of
      Right (x', st') -> return $ Right (f x', st')
      Left err        -> return $ Left err

instance Applicative TcM where
  pure x = TcM $ \_tenv _env _cenv _sym _ctxt st -> return (Right (x, st))
  (TcM f) <*> (TcM x) = TcM $ \tenv env cenv sym ctx st -> do
    fres <- f tenv env cenv sym ctx st
    case fres of
      Left err -> return $ Left err
      Right (f', st') -> do
        xres <- x tenv env cenv sym ctx st'
        case xres of
          Left err -> return $ Left err
          Right (x', st'') -> pure $ Right (f' x', st'')

instance Monad TcM where
  (>>=) m1 m2 =
    TcM $ \tenv env cenv sym ctxt st ->
      do { res <- runTcM m1 tenv env cenv sym ctxt st
         ; case res of
             Left err -> return $ Left err
             Right (a, st') -> runTcM (m2 a) tenv env cenv sym ctxt st'
         }
  return = pure

raiseErr :: Bool -> Maybe SourcePos -> Doc -> TcM a
raiseErr print_vartypes p msg
  = do { ctx <- getErrCtx
       ; vartypes_msg <- ppVarTypes print_vartypes ctx
       ; let doc = ppTyErr $ TyErr ctx p msg vartypes_msg
       ; failTcM doc }
  where
    ppVarTypes :: Bool -> ErrCtx -> TcM Doc
    ppVarTypes False _
      = return empty
    ppVarTypes True (CompErrCtx comp)
      = pp_vars $ (S.elems *** S.elems) (compFVs comp)
    ppVarTypes True (ExprErrCtx exp)
      = pp_vars $ ([], S.elems (exprFVs exp))
    ppVarTypes _ _
      = return empty

    -- Since error contexts are _source_ level terms, they carry source types
    --
    pp_cvar :: GName (Maybe (GCTy SrcTy)) -> TcM Doc
    pp_cvar v = do
      cty  <- lookupCEnv (name v) p
      zcty <- zonkCTy cty
      return $ ppName v <+> colon <+> ppr zcty

    pp_var :: GName (Maybe SrcTy) -> TcM Doc
    pp_var v = do
      ty  <- lookupEnv (name v) p
      zty <- zonkTy ty
      return $ ppName v <+> colon <+> ppr zty

    pp_vars :: ([GName (Maybe (GCTy SrcTy))], [GName (Maybe SrcTy)]) -> TcM Doc
    pp_vars (cvars, vars)
     = do { cdocs <- mapM pp_cvar cvars
          ; docs  <- mapM pp_var  vars
          ; return $ vcat [ text "Variable bindings:"
                          , nest 2 $ vcat (cdocs ++ docs)
                          ] }

raiseErrNoVarCtx :: Maybe SourcePos -> Doc -> TcM a
raiseErrNoVarCtx = raiseErr False

failTcM :: Doc -> TcM a
failTcM doc
  = TcM $ \_ _ _ _ _ _ -> return $ Left doc


updBinds :: Ord x => [(x,a)] -> M.Map x a -> M.Map x a
updBinds newbinds binds = M.union (M.fromList newbinds) binds

-- Get the environments
getEnv :: TcM Env
getEnv = TcM $ \_ env _ _ _ st -> return $ Right (env, st)
getCEnv :: TcM CEnv
getCEnv = TcM $ \_ _ cenv _ _ st -> return $ Right (cenv, st)

getTDefEnv :: TcM TyDefEnv
getTDefEnv = TcM $ \tenv _ _ _ _ st -> return $ Right (tenv, st)

getErrCtx :: TcM ErrCtx
getErrCtx = TcM $ \_ _ _ _ ctx st -> return $ Right (ctx,st)


-- Get a stateful environment
getStEnv :: (TcMState -> a) -> TcM a
getStEnv f = TcM $ \_ _ _ _ _ st -> return $ Right(f st, st)
updStEnv :: (TcMState -> TcMState) -> TcM ()
updStEnv f = TcM $ \_ _ _ _ _ st -> return $ Right ((),f st)

-- Type environment (substitution)
getTyEnv :: TcM TyEnv
getTyEnv = getStEnv tcm_tyenv
setTyEnv :: TyEnv -> TcM ()
setTyEnv e = updStEnv $ \st -> st { tcm_tyenv = e }


updTyEnv :: [(String,Ty)] -> TcM ()
updTyEnv binds
  = updStEnv $ \st -> st { tcm_tyenv = updBinds binds (tcm_tyenv st) }

-- ALen environment
getALenEnv :: TcM ALenEnv
getALenEnv = getStEnv tcm_alenenv
setALenEnv :: ALenEnv -> TcM ()
setALenEnv e = updStEnv (\st -> st { tcm_alenenv = e })
updALenEnv :: [(LenVar, NumExpr)] -> TcM ()
updALenEnv binds
  = updStEnv $ \st -> st { tcm_alenenv = updBinds binds (tcm_alenenv st) }

-- AMAxLen environment
getAMaxLenEnv :: TcM AMaxLenEnv
getAMaxLenEnv = getStEnv tcm_amaxlenenv
setAMaxLenEnv :: AMaxLenEnv -> TcM ()
setAMaxLenEnv e = updStEnv (\st -> st { tcm_amaxlenenv = e })
updAMaxLenEnv :: [(GName Ty, NumExpr)] -> TcM ()
updAMaxLenEnv binds
  = updStEnv $ \st -> st { tcm_amaxlenenv = updBinds binds (tcm_amaxlenenv st) }

-- BitWidth environment
getBWEnv :: TcM BWEnv
getBWEnv = getStEnv tcm_bwenv
setBWEnv :: BWEnv -> TcM ()
setBWEnv e = updStEnv (\st -> st { tcm_bwenv = e })
updBWEnv :: [(BWVar,BitWidth)] -> TcM ()
updBWEnv binds
  = updStEnv $ \st -> st { tcm_bwenv = updBinds binds (tcm_bwenv st) }


-- Lookups
lookupTcM :: Ord a => a -> Maybe SourcePos -> Doc -> M.Map a b -> TcM b
lookupTcM s pos err env
  | Just res <- M.lookup s env
  = return res
  | otherwise = raiseErr False pos err

lookupEnv :: String -> Maybe SourcePos -> TcM Ty
lookupEnv s pos = getEnv >>= lookupTcM s pos msg
  where msg = text "Unbound variable:" <+> text s

lookupCEnv :: String -> Maybe SourcePos -> TcM CTy
lookupCEnv s pos = getCEnv >>= lookupTcM s pos msg
  where msg = text "Unbound computation variable:" <+> text s

lookupTDefEnv :: String -> Maybe SourcePos -> TcM StructDef
lookupTDefEnv s pos = getTDefEnv >>= lookupTcM s pos msg
  where msg = text "Unbound type definition:" <+> text s

lookupALenEnv :: LenVar -> Maybe SourcePos -> TcM NumExpr
lookupALenEnv s pos = getALenEnv >>= lookupTcM s pos msg
  where msg = text "Unbound array length:" <+> text s


-- Lifting an IO action
liftIO :: IO a -> TcM a
liftIO m = TcM $ \_ _ _ _ _ st -> m >>= \a -> return (Right (a,st))

genSym :: String -> TcM String
genSym prefix =
  TcM $ \_ _ _ sym _ st ->
    do { str <- GS.genSymStr sym
       ; return $ Right (prefix ++ str, st) }

extendEnv :: [GName Ty] -> TcM a -> TcM a
extendEnv binds m
  = TcM $ \tenv env cenv sym ctxt st ->
            runTcM m tenv (updBinds binds' env) cenv sym ctxt st
  where
    binds' = map (\nm -> (name nm, nameTyp nm)) binds


extendCEnv :: [GName CTy] -> TcM a -> TcM a
extendCEnv binds m
  = TcM $ \tenv env cenv sym ctxt st ->
            runTcM m tenv env (updBinds binds' cenv) sym ctxt st
  where
    binds' = map (\nm -> (name nm, nameTyp nm)) binds

extendTDefEnv :: [(String,StructDef)] -> TcM a -> TcM a
extendTDefEnv binds m
  = TcM $ \tenv env cenv sym ctxt st ->
            runTcM m (updBinds binds tenv) env cenv sym ctxt st


pushErrCtx :: ErrCtx -> TcM a -> TcM a
pushErrCtx ctxt m
  = TcM $ \tenv env cenv sym _ctxt st ->
              runTcM m tenv env cenv sym ctxt st


updInTy :: Ty -> CTy -> CTy
updInTy ta cty
  | CTBase (TComp v _ tb) <- cty = CTBase (TComp v ta tb)
  | CTBase (TTrans _ tb)  <- cty = CTBase (TTrans ta tb)
  | CTArrow _ _ <- cty = cty
  | otherwise = error "updInTy"


updYldTy :: Ty -> CTy -> CTy
updYldTy tb cty
  | CTBase (TComp v ta _) <- cty = CTBase (TComp v ta tb)
  | CTBase (TTrans ta _)  <- cty = CTBase (TTrans ta tb)
  | CTArrow _ _ <- cty = cty
  | otherwise = error "updYldTy"

updDoneTy :: Ty -> CTy -> CTy
updDoneTy v cty
  | CTBase (TComp _ ta tb) <- cty = CTBase (TComp v ta tb)
  | CTBase (TTrans _ _)   <- cty = cty
  | CTArrow _ _ <- cty = cty
  | otherwise = error "updDoneTy"


newTyVar :: String -> TcM TyVar
newTyVar prefix = genSym prefix

newBWVar :: String -> TcM BWVar
newBWVar prefix = genSym prefix

newALenVar :: String -> TcM LenVar
newALenVar prefix = genSym prefix

newTInt_BWUnknown :: TcM Ty
newTInt_BWUnknown =
  do { v <- newBWVar "bw"
     ; return $ TInt (BWUnknown v) }


tyVarsOfTy :: Ty -> S.Set TyVar
tyVarsOfTy t = snd $ runState (mapTyM collect_var t) S.empty
 where collect_var :: Ty -> State (S.Set TyVar) Ty
       collect_var (TVar x)
        = do { modify (S.union (S.singleton x))
             ; return (TVar x) }
       collect_var ty
        = return ty

tyVarsOfCTy :: CTy -> S.Set TyVar
tyVarsOfCTy (CTBase ct0)      = tvs_ct0 ct0
tyVarsOfCTy (CTArrow tys ct0) = tvs_args tys `S.union` tvs_ct0 ct0

tvs_ct0 :: GCTy0 Ty -> S.Set TyVar
tvs_ct0 (TTrans a b)  = tyVarsOfTy a `S.union` tyVarsOfTy b
tvs_ct0 (TComp v a b) = tyVarsOfTy a `S.union` tyVarsOfTy b
                                     `S.union` tyVarsOfTy v

tvs_arg :: CallArg Ty (GCTy0 Ty) -> S.Set TyVar
tvs_arg (CAExp t)    = tyVarsOfTy t
tvs_arg (CAComp ct0) = tvs_ct0 ct0

tvs_args :: [CallArg Ty (GCTy0 Ty)] -> S.Set TyVar
tvs_args = foldl (\s a -> s `S.union` tvs_arg a) S.empty



checkWith :: Maybe SourcePos -> Bool -> Doc -> TcM ()
checkWith _   True  _   = return ()
checkWith pos False err = raiseErr False pos err


firstToSucceed :: TcM a -> TcM a -> TcM a
firstToSucceed m1 m2
  = TcM $ \tenv env cenv sym ctx st ->
    do { mres <- runTcM m1 tenv env cenv sym ctx st
       ; case mres of
           Left _err -> runTcM m2 tenv env cenv sym ctx st
           Right res -> return (Right res)
       }


{- Zonking
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

zonkTy :: Ty -> TcM Ty
zonkTy = mapTyM do_zonk
  where
    do_zonk (TVar x)
      = do { tenv <- getTyEnv
           ; case M.lookup x tenv of
                Nothing  -> return (TVar x)
                Just xty -> zonkTy xty
           }
    do_zonk (TInt bw)
      = do { bw' <- zonkBitWidth bw
           ; return (TInt bw')
           }
    do_zonk (TArray n ty)
      = do { n' <- zonkALen n
             -- NB: No need to recurse, mapTyM will do it for us
           ; return (TArray n' ty)
           }
    do_zonk ty = return ty

zonkBitWidth :: BitWidth -> TcM BitWidth
zonkBitWidth (BWUnknown x)
  = do { benv <- getBWEnv
       ; case M.lookup x benv of
           Just bw -> zonkBitWidth bw
           Nothing -> return (BWUnknown x)
       }
zonkBitWidth other_bw = return other_bw

zonkALen :: NumExpr -> TcM NumExpr
zonkALen (NVar n)
  = do { env <- getALenEnv
       ; case M.lookup n env of
           Nothing -> return $ NVar n
           Just ne -> zonkALen ne
       }
zonkALen (Literal i)
  = return (Literal i)


zonkCTy :: CTy -> TcM CTy
zonkCTy cty
  = case cty of
      CTBase cty0 ->
         do { cty0' <- zonk_cty0 cty0
            ; return (CTBase cty0')
            }
      CTArrow ts cty0 ->
         do { ts' <- mapM zonk_arg ts
            ; cty0' <- zonk_cty0 cty0
            ; return (CTArrow ts' cty0')
            }
  where

    zonk_arg (CAExp t)
       = do { t' <- zonkTy t
            ; return (CAExp t')
            }

    zonk_arg (CAComp ct)
       = do { ct' <- zonk_cty0 ct
            ; return (CAComp ct')
            }

    zonk_cty0 :: CTy0 -> TcM CTy0
    zonk_cty0 (TComp u a b)
      = do { u' <- zonkTy u
           ; a' <- zonkTy a
           ; b' <- zonkTy b
           ; return (TComp u' a' b')
           }

    zonk_cty0 (TTrans a b)
      = do { a' <- zonkTy a
           ; b' <- zonkTy b
           ; return (TTrans a' b')
           }

-- | Zonking
--
-- NOTE: This used to also default the type of `EError` to `TUnit` when it
-- was still a free variable. However, we don't do this anymore here but we
-- do this in `defaultExpr` instead. This is important, because we should have
-- the invariant that we can zonk at any point during type checking without
-- changing the result.
zonkExpr :: Exp -> TcM Exp
zonkExpr = mapExpM zonkTy return return

zonkComp :: Comp -> TcM Comp
zonkComp = mapCompM zonkCTy zonkTy return return zonkExpr return
