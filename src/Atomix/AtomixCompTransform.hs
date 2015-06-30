

{-# OPTIONS -Wall -Werror #-}
module AtomixCompTransform (
  atomixCompTransform, 
  atomixCompToComp,
  RnSt ( .. ) 
  ) where

import AstExpr
import AstComp
import AstUnlabelled
import Utils

import NameEnv
import Control.Applicative ( (<$>) )
import Outputable 

import Data.Maybe ( fromJust, isJust )

import qualified Data.Set as S

import Control.Monad.Reader
import Control.Monad.State
import Data.Loc

import Text.PrettyPrint.HughesPJ

import qualified GenSym as GS

import Data.List ( nub )

import CtExpr 
import CtComp

{----------- Renaming expression variables bound in computations --------------}

data RnEnv = RnEnv { env_vars :: NameEnv Ty EId, env_sym  :: GS.Sym }

type Rn a = ReaderT RnEnv IO a

rnVar :: EId -> Rn EId
rnVar x = do 
  bound <- asks env_vars
  case neLookup x bound of { Nothing -> return x; Just x' -> return x' }

extEnv :: EId -> Rn a -> Rn a
extEnv x action = do 
  sym <- asks env_sym 
  uniq  <- liftIO $ GS.genSymStr sym
  let x' = x { uniqId = MkUniq uniq }
  local (\r -> r { env_vars = neExtend x x' (env_vars r) }) action

freshName :: String -> SrcLoc -> Ty -> MutKind -> Rn EId
freshName str loc ty mut = do 
  sym <- asks env_sym 
  uniq  <- liftIO $ GS.genSymStr sym
  return $ (toName str loc ty mut) { uniqId = MkUniq uniq }

alphaNorm :: Comp -> Rn Comp
alphaNorm = mapCompM return return return return return action
  where action c 
          | Emit e <- unComp c
          , let loc = compLoc c
          = do x <- freshName "emit_tmp" loc (ctExp e) Imm
               return $ cLetE loc x AutoInline e (cEmit loc (eVar loc x))

          | Emits e <- unComp c
          , let loc = compLoc c
          = do x <- freshName "emits_tmp" loc (ctExp e) Imm
               return $ cLetE loc x AutoInline e (cEmits loc (eVar loc x))

          | Branch e c1 c2 <- unComp c
          , let loc = compLoc c
          = do x <- freshName "branch_cond" loc (ctExp e) Imm
               return $ cLetE loc x AutoInline e (cBranch loc (eVar loc x) c1 c2)

          | While e c1 <- unComp c
          , let loc = compLoc c
          = do x <- freshName "while_cond" loc (ctExp e) Imm
               return $ cLetERef loc x (Just e) $
                        cWhile loc (eVar loc x) 
                          (cSeq loc c1 (cReturn loc AutoInline (eAssign loc (eVar loc x) e)))

          | Until e c1 <- unComp c
          , let loc = compLoc c
          = do x <- freshName "until_cond" loc (ctExp e) Imm
               return $ cLetERef loc x Nothing $
                        cUntil loc (eVar loc x) 
                          (cSeq loc c1 (cReturn loc AutoInline (eAssign loc (eVar loc x) e)))

          | Times _ui _estart elen _cnt _c1 <- unComp c
          , EVal {} <- unExp elen
          = return c -- Static times => translate to static loops in atomix
          | Times _ui estart elen cnt c1 <- unComp c
          , let loc = compLoc c 
          = do x_bound <- freshName "times_bound" loc (ctExp elen) Imm
               let cond_expr   = eBinOp loc Lt (eVar loc cnt) (eVar loc x_bound) -- cnt < x_bound
                   cnt_expr    = eVar loc cnt
                   TInt _bw sn = nameTyp cnt
                   one         = eVal loc (nameTyp cnt) (VInt 1 sn)
                   upd_cntr    = eAssign loc cnt_expr (eBinOp loc Add cnt_expr one)

               cLetERef loc cnt (Just estart) <$> 
                 cLetE loc x_bound AutoInline (eBinOp loc Add (eVar loc cnt) elen) <$>
                 action (cWhile loc cond_expr (cSeq loc c1 (cReturn loc AutoInline upd_cntr)))

          
          | otherwise
          = return c


renameComp :: GS.Sym -> Comp -> IO Comp 
renameComp sym comp
  = runReaderT (alphaNorm =<< ren_comp comp) $ RnEnv { env_vars = neEmpty, env_sym = sym }
  where 
    ren_comp = mapCompM_env return 
                           return 
                           return 
                           return 
                           ren_expr 
                           on_comp
                           extEnv 
                           (\_ m -> m)
    on_comp c 
      | Map v fn <- unComp c
      , TArrow [argty] _resty <- nameTyp fn
      , let loc = compLoc c 
      = do fn' <- rnVar fn
           x <- freshName (name fn ++ "_in") loc (argty_ty argty) Imm 
           return $ cRepeat loc v $ 
                    cBindMany loc (cTake1 loc (nameTyp x))
                                  [(x, cEmit loc (eCall loc fn' [eVar loc x]))]
     | Filter {} <- unComp c
     = panicStr "renameExpr: Filter not supported"
     | otherwise 
     = return c
           
    ren_expr = mapExpM_env return return ren_var extEnv
    ren_var e | EVar x <- unExp e 
              = eVar (expLoc e) <$> rnVar x 
              | ECall fn args <- unExp e
              = do { fn' <- rnVar fn
                   ; return $ eCall (expLoc e) fn' args }
              | otherwise = return e





{------------------- Lifting up functions with closure variables --------------}
{- The purpose of this transform is to lift definitions to the top-level
   and (a) create a bunch of global function definitions, 
       (b) a bunch of global letref definitions
       (c) a bunch of global struct definitions
 ------------------------------------------------------------------------------}

data RnSt = RnSt { st_letref_vars :: [EId]
                 , st_fundefs     :: [(EId,(Fun,[EId]))]
                 , st_structs     :: [(TyName, StructDef)]
                 }

type RnStM a = StateT RnSt IO a

recLetRef :: EId -> RnStM ()
recLetRef x = modify $ \s -> s { st_letref_vars = x : st_letref_vars s }

recFunDef :: EId -> Fun -> [EId] -> RnStM ()
recFunDef x fn clos 
  = modify $ \s -> s { st_fundefs = (x,(fn,clos)) : st_fundefs s }

recStruct :: TyName -> StructDef -> RnStM ()
recStruct sn sd 
  = modify $ \s -> s { st_structs = (sn,sd) : st_structs s }


emptyRnSt :: RnSt
emptyRnSt = RnSt { st_letref_vars = []
                 , st_fundefs     = []
                 , st_structs     = [] 
                 }

lkpFunDef :: EId -> RnStM (Maybe (Fun, [EId])) 
lkpFunDef f = lookup f <$> gets st_fundefs


-- Deal with Let/LetRef/LetStruct
liftBindsComp :: Comp -> RnStM Comp
liftBindsComp = mapCompM return return return return return action
  where action c 
          | LetE x _fi e c1 <- unComp c
          , let loc = compLoc c
          = return $ cBindMany loc (cReturn loc AutoInline e) [(x,c1)]

          | LetERef x Nothing c1 <- unComp c
          = recLetRef x >> return c1

          | LetERef x (Just e) c1 <- unComp c
          , let loc = compLoc c 
          = let easgn = eAssign loc (eVar loc x) e
                ret   = cReturn loc AutoInline easgn
            in recLetRef x >> return (cSeq loc ret c1)

          | LetStruct sdef c1 <- unComp c
          = recStruct (struct_name sdef) sdef >> return c1 
          
          | otherwise
          = return c


{---------------------------- Closure conversion ------------------------------}

closConvComp :: Comp -> RnStM Comp
closConvComp comp = clos_conv comp >>= fixup_call_sites
  where 
   fixup_call_sites = mapCompM return 
                               return
                               return
                               return
                               fixup_call_site
                               return
   fixup_call_site e
     | ECall fn eargs <- unExp e
     = do mb <- lkpFunDef fn
          case mb of 
            Nothing 
              -> panic $ text "closConvTop: function definition missing!" <+> ppr fn
            Just (fdef,clos_vars)
              -> let loc = expLoc e
                     clos_args = map (eVar loc) clos_vars
                 in return $ eCall loc (funName fdef) (eargs ++ clos_args)
     | otherwise = return e


clos_conv :: Comp -> RnStM Comp 
clos_conv comp = go $ unComp comp
  where 
    loc = compLoc comp 
    go (LetHeader fun c1) = do 
       cvs <- getClosureVars fun
       let new_fun = closConvFun fun cvs
       recFunDef (funName new_fun) new_fun cvs
       clos_conv c1
    go (BindMany c xs_cs) = do 
      c' <- clos_conv c
      xs_cs' <- mapM (\(x,cx) -> (\w -> (x,w)) <$> clos_conv cx) xs_cs
      return $ cBindMany loc c' xs_cs'
    go (Seq c1 c2) = do 
      c1' <- clos_conv c1
      c2' <- clos_conv c2
      return $ cSeq loc c1' c2'
    go (Par p c1 c2) = do 
      c1' <- clos_conv c1
      c2' <- clos_conv c2
      return $ cPar loc p c1' c2'
    go (Branch e c1 c2) = do 
      c1' <- clos_conv c1
      c2' <- clos_conv c2
      return $ cBranch loc e c1' c2'
    go (Standalone c) = cStandalone loc <$> clos_conv c
    go (VectComp v c) = cVectComp loc v <$> clos_conv c
    go (Repeat v c)   = cRepeat loc v   <$> clos_conv c
    go (Until e c)    = cUntil loc e    <$> clos_conv c
    go (While e c)    = cWhile loc e    <$> clos_conv c
    go (Times u es elen nm c) = cTimes loc u es elen nm <$> clos_conv c 

    go (Call fn cargs) = cCall loc fn <$> mapM go_arg cargs

    go _cother = return comp

    go_arg (CAExp e)  = return $ CAExp e
    go_arg (CAComp c) = CAComp <$> clos_conv c


closConvFun :: Fun -> [EId] -> Fun
closConvFun f@(MkFun (MkFunExternal {}) _ _) _closvars = f
closConvFun (MkFun (MkFunDefined fn prms body) loc _) closvars
  = mkFunDefined loc new_fn (prms ++ closvars) body
  where new_fn = fn { nameTyp = clos_conv_funty (nameTyp fn) }
        clos_conv_funty :: Ty -> Ty
        clos_conv_funty (TArrow argtys resty)
           = TArrow (argtys ++ closvartys) resty
           where closvartys = map (\x -> GArgTy (nameTyp x) (nameMut x)) closvars
        clos_conv_funty _ty = panicStr "clos_conv_funty: not an arrow!"


getClosureVars :: Fun -> RnStM [EId]
getClosureVars fdef = do
  -- | Get the free variables of this definition
  let clos_vars_from_body = S.toList (funFVsClos fdef)
  -- | Get all the closure parameters (recursively) of callees 
  called_funs <- map fromJust <$> filter isJust <$> 
                    (mapM lkpFunDef $ S.toList (funFVs fdef))
  let clos_vars_from_called = concat (map (\(_,fs) -> fs) called_funs)
  -- | Sum the two up and return
  return $ nub (clos_vars_from_body ++ clos_vars_from_called)

{------------------ Main transform -------------------------------------------}

atomixCompTransform :: GS.Sym -> Comp -> IO (Comp,RnSt)
atomixCompTransform sym c = do 
  ren_c <- renameComp sym c
  (final_comp,st) <- runStateT (liftBindsComp ren_c >>= closConvComp) emptyRnSt
  putStrLn "Atomix closure conversion phase finished, typechecking result ..."
  putStrLn $ "Typechecking finished: " ++ show (ctComp final_comp)
  return (final_comp, st)


atomixCompToComp :: Comp -> RnSt -> Comp
atomixCompToComp  = error "implement me!" 

