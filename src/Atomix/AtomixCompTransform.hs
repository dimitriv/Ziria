{-# OPTIONS -Wall -Werror #-}
module AtomixCompTransform (
  atomixCompTransform, 
  atomixCompToComp,
  zirToAtomZir,
  freshName, freshNameDoc, newBlockId,
  RnSt ( .. ) 
  ) where

import AstExpr
import AstComp
import AstUnlabelled
import Utils
import Control.Applicative ( (<$>) )
import Outputable 
import Data.Maybe ( fromJust, isJust )
import qualified Data.Set as S
import Control.Monad.Identity
import Control.Monad.State
import Data.Loc
import Text.PrettyPrint.HughesPJ
import qualified GenSym as GS
import Data.List ( nub )
import CtExpr 
import CtComp
import TcRename 
import AtomComp

import Analysis.DataFlow 

import Opts

{----------- A-normal forms --------------}

freshName :: GS.Sym -> String -> SrcLoc -> Ty -> MutKind -> IO EId
freshName sym str loc ty mut = do 
  uniq  <- GS.genSymStr sym
  return $ (toName str loc ty mut) { uniqId = MkUniq uniq }

freshNameDoc :: GS.Sym -> String -> SrcLoc -> Ty -> MutKind -> String -> IO EId
freshNameDoc sym str loc ty mut doc = do
  x <- freshName sym str loc ty mut
  return (x { nameDoc = doc })

alphaNorm :: GS.Sym -> Comp -> IO Comp
alphaNorm sym = mapCompM return return return return return action
  where 
    action c 
      -- | Emit e <- unComp c
      -- , let loc = compLoc c
      -- = do x <- freshName sym "emit_tmp" loc (ctExp e) Imm
      --      return $ cLetE loc x AutoInline e (cEmit loc (eVar loc x))
      | Emits evar <- unComp c
      , EVar {} <- unExp evar
      = return c

      | Emits e <- unComp c
      , let loc = compLoc c
      = do x <- freshName sym "emits_tmp" loc (ctExp e) Imm
           return $ cLetE loc x AutoInline e (cEmits loc (eVar loc x))

      | Branch e c1 c2 <- unComp c
      , let loc = compLoc c
      = do x <- freshNameDoc sym "cond_if" loc (ctExp e) Imm (show e)
           return $ cLetE loc x AutoInline e (cBranch loc (eVar loc x) c1 c2)

      | While e c1 <- unComp c
      , let loc = compLoc c
      = do x <- freshNameDoc sym "cond_while" loc (ctExp e) Mut (show e) -- needs to be refreshed in each iteration
           return $ 
             cLetERef loc x (Just e) $
               cWhile loc (eVar loc x) 
                  (cSeq loc c1 
                     (cReturn loc AutoInline (eAssign loc (eVar loc x) e)))

      | Until e c1 <- unComp c
      , let loc = compLoc c
      = do x <- freshNameDoc sym "cond_until" loc (ctExp e) Mut (show e) -- needs to be refreshed in each iteration
           return $ 
             cLetERef loc x Nothing $
               cUntil loc (eVar loc x) 
                  (cSeq loc c1 
                     (cReturn loc AutoInline (eAssign loc (eVar loc x) e)))

      -- | Times _ui _estart elen _cnt _c1 <- unComp c
      -- , EVal {} <- unExp elen
      -- = return c -- Static times => translate to static loops in atomix
      | Times _ui estart elen cnt c1 <- unComp c
      , let loc = compLoc c 
      = do x_bnd <- freshName sym "times_bound" loc (ctExp elen) Imm
           let cond_expr   = eBinOp loc Lt (eVar loc cnt) 
                                           (eVar loc x_bnd) -- cnt < x_bound
               -- It is a bit naughty to assign to a variable that started life
               -- as immutable -- however we will fix this in the next phase of
               -- translation to atomix (mkMutableBound)
               cnt_expr    = eVar loc cnt
               TInt _bw sn = nameTyp cnt
               one         = eVal loc (nameTyp cnt) (VInt 1 sn)
               upd_cntr    = eAssign loc cnt_expr (eBinOp loc Add cnt_expr one)

           cLetERef loc cnt (Just estart) <$> 
            cLetE loc x_bnd AutoInline (eBinOp loc Add (eVar loc cnt) elen) <$>
             action (cWhile loc cond_expr 
                       (cSeq loc c1 (cReturn loc AutoInline upd_cntr)))

      | Map v fn <- unComp c
      , TArrow [argty] _resty <- nameTyp fn
      , let loc = compLoc c 
      = do x <- freshName sym (alphaNumStr (name fn) ++ "_in") loc (argty_ty argty) Imm
           alphaNorm sym $ 
              cRepeat loc v $ 
              cBindMany loc (cTake1 loc (nameTyp x))
                            [(x, cEmit loc (eCall loc fn [eVar loc x]))]

      | otherwise  
      = return c


{------------------- Lifting up functions with closure variables --------------}
{- The purpose of this transform is to lift definitions to the top-level
   and (a) create a bunch of global function definitions, 
       (b) a bunch of global letref definitions
       (c) a bunch of global struct definitions
 ------------------------------------------------------------------------------}

data RnSt = RnSt { st_bound_vars  :: [EId]
                 , st_fundefs     :: [(EId,(Fun,[EId]))] -- NB: in reverse order
                 , st_structs     :: [(TyName, StructDef)]
                 }

instance Outputable RnSt where
  ppr (RnSt vars fundefs structs) 
      = vcat $
        [text "Variables:"] ++
        [text (show vars)] ++
        [text "Functions:"] ++
        map (pprFun . unFun . fst . snd) fundefs ++
        [text "Structs"] ++
        [text (show structs)]
    where
      pprFun (MkFunDefined f args body) 
         = ppr f <> (parens $ hsep $ punctuate comma $ map ppr args) <+> 
           text ":=" <+> ppr body
      pprFun (MkFunExternal f args _body) 
         = ppr f <> (parens $ hsep $ punctuate comma $ map ppr args) <+> 
           text ":=" <+> text "<EXTERNAL>"

type RnStM a = StateT RnSt IO a

recBound :: EId -> RnStM ()
recBound x 
  = modify $ \s -> s { st_bound_vars = x { nameMut = Mut } : st_bound_vars s }

recFunDef :: EId -> Fun -> [EId] -> RnStM ()
recFunDef x fn clos 
  = modify $ \s -> s { st_fundefs = (x,(fn,clos)) : st_fundefs s }

recStruct :: TyName -> StructDef -> RnStM ()
recStruct sn sd 
  = modify $ \s -> s { st_structs = (sn,sd) : st_structs s }


emptyRnSt :: RnSt
emptyRnSt = RnSt { st_bound_vars  = []
                 , st_fundefs     = []
                 , st_structs     = [] 
                 }

lkpFunDef :: EId -> RnStM (Maybe (Fun, [EId])) 
lkpFunDef f = lookup f <$> gets st_fundefs


-- Deal with Let/LetRef/LetStruct/Bind
liftBindsComp :: Comp -> RnStM Comp
liftBindsComp = mapCompM return return return return return action
  where action c 
          | LetE x _fi e c1 <- unComp c
          , let loc = compLoc c
          = do recBound x
               return $ cBindMany loc (cReturn loc AutoInline e) [(x,c1)]

          | LetERef x Nothing c1 <- unComp c
          = recBound x >> return c1

          | LetERef x (Just e) c1 <- unComp c
          , let loc = compLoc c 
          = let easgn = eAssign loc (eVar loc x) e
                ret   = cReturn loc AutoInline easgn
            in recBound x >> return (cSeq loc ret c1)

          | LetStruct sdef c1 <- unComp c
          = recStruct (struct_name sdef) sdef >> return c1 

          | BindMany c1 rest <- unComp c
          = go c1 rest
          | otherwise
          = return c
          where go c1 [] = return c1
                go c1 ((x,c2):xscs)
                 | x `S.notMember` (fst $ compFVs c2) -- not in c2 & not in xscs
                 , x `S.notMember` (S.unions (map (fst. compFVs. snd) xscs)) 
                 = cSeq (compLoc c) c1 <$> go c2 xscs
                 | otherwise 
                 = do recBound x
                      rest' <- go c2 xscs 
                      return $ cBindMany (compLoc c) c1 [(x, rest')]

mkMutableBound :: [EId] -> Comp -> IO Comp
-- | All 'recBound' variables should become mutable because we write
-- to them in the automata level (perhaps just once if they originate
-- in Ziria immutables, but still)
mkMutableBound bound 
  = mapCompM return return return return exp_action comp_action
  where exp_action = mapExpM return return action
        action e   = return $ case unExp e of
          EVar x | x `elem` bound -> eVar (expLoc e) (x { nameMut = Mut })
          _otherwise -> e

        comp_action c = return $ case unComp c of 
          BindMany c1 xs_cs -> 
            let xs_cs' = map (\(x,d) -> (x { nameMut = Mut },d)) xs_cs
            in cBindMany (compLoc c) c1 xs_cs'
          _ -> c

{---------------------------- Closure conversion ------------------------------}

closConvComp :: Comp -> RnStM Comp
closConvComp comp = clos_conv comp >>= fixup_call_sites
  where 
   fixup_call_sites 
      = mapCompM return 
                 return
                 return
                 return
                 fixup_call_site
                 return
   fixup_call_site e
     | ECall fn eargs <- unExp e
     = do mb <- lkpFunDef fn
          case mb of
            Nothing -> 
              panic $ text "closConvTop: unbound function " <+> ppr fn
            Just (fdef,clos_vars) -> 
              let loc = expLoc e
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
    go (Standalone l c) = cStandalone loc l <$> clos_conv c
    go (VectComp v c)   = cVectComp loc v   <$> clos_conv c
    go (Repeat v c)     = cRepeat loc v     <$> clos_conv c
    go (Until e c)      = cUntil loc e      <$> clos_conv c
    go (While e c)      = cWhile loc e      <$> clos_conv c
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
           = TArrow (argtys ++ clos_vartys) resty
        clos_conv_funty _ty = panicStr "clos_conv_funty: not an arrow!"
        clos_vartys = map (\x -> GArgTy (nameTyp x) (nameMut x)) closvars


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
  -- Rename 
  ren_c <- tcRenComp sym c

  -- Alpha convert 
  ren_alpha <- alphaNorm sym ren_c

  -- Closure convert / lift 
  (closure_comp,st) <- runStateT (liftBindsComp ren_alpha >>= closConvComp) $
                       emptyRnSt

  final_comp <- mkMutableBound (st_bound_vars st) closure_comp

  putStrLn $ "Closure conversion phase finished, result type: " ++ 
             show (ctComp final_comp)
  return (final_comp, st)


atomixCompToComp :: Comp -> RnSt -> Comp
atomixCompToComp comp (RnSt { st_bound_vars  = letrefs
                            , st_fundefs     = fundefs
                            , st_structs     = strdefs }) = str_c 
 where let_c = foldl (\c x -> cLetERef loc x Nothing c) comp letrefs
       fun_c = foldl (\c f -> cLetHeader loc f c) let_c funs 
       str_c = foldl (\c s -> cLetStruct loc s c) fun_c (map snd strdefs)
       funs  = map (\(_,(fun,_)) -> fun) fundefs
       loc   = compLoc comp



zirToAtomZir :: DynFlags -> GS.Sym -> Comp -> IO (AComp MbLocConstr (), RnSt)
zirToAtomZir dfs sym comp = do
  -- Closure convert and lift
  (comp0,rnst) <- atomixCompTransform sym comp 
  -- Transform lifted
  (acomp,xs) <- runStateT (transLifted dfs sym comp0) []
  print $ vcat [ text "AComp structure:"
               , nest 2 $ ppr acomp
               ]
  return (acomp,rnst { st_bound_vars = st_bound_vars rnst ++ xs } )


newBlockId :: GS.Sym -> SrcLoc -> String -> IO String
newBlockId sym loc pref = do
  u <- GS.genSymStr sym
  let block_id = pref ++ "$" ++ (render $ ppr $ loc) ++ "$" ++ u
  return block_id

transLiftedExp :: DynFlags -> GS.Sym -> Exp -> IO (AExp ())
transLiftedExp dfs sym e = do
  vupkg <- inOutVarsDefinite dfs e
  block_id <- newBlockId sym (expLoc e) ""

  return $ MkAExp { aexp_lbl = block_id
                  , aexp_exp = e
                  , aexp_ivs = vu_invars vupkg
                  , aexp_ovs = vu_outvars vupkg
                  , aexp_nfo = ()
                  , aexp_ret = ctExp e 
                  }

transLifted :: DynFlags -> GS.Sym -> Comp -> StateT [EId] IO (AComp MbLocConstr ())
transLifted dfs sym = go_comp Nothing
  where 
    go_comp lconstr comp = go lconstr (unComp comp)
      where 
        loc = compLoc comp
        go lc (Return _ e)  = aReturn loc lc <$> (liftIO $ transLiftedExp dfs sym e)

        go _lc (Async lc' e) = aAsync loc (Just lc') <$> (liftIO $ transLiftedExp dfs sym e) 
        go lc (Await x)      = return $ aAwait loc lc x

        -- go (Times _ estrt (MkExp (EVal _ (VInt i _)) _ _) cnt c) = do
        --   let cnt_expr    = eVar loc cnt
        --       TInt _bw sn = nameTyp cnt
        --       one         = eVal loc (nameTyp cnt) (VInt 1 sn)
        --       upd_cntr   = eAssign loc cnt_expr (eBinOp loc Add cnt_expr one)
        --       asg_cntr   = eAssign loc cnt_expr estrt

        --       c_upd      = cSeq loc c (cReturn loc AutoInline upd_cntr)
           
        --   a1 <- go_comp (cReturn loc AutoInline asg_cntr)
        --   a2 <- aRepeatN loc () (fromIntegral i) <$> go_comp c_upd
        --   -- Record the counter variable 
        --   modify (\xs -> cnt:xs)        
        --   -- and return 
        --   return (aBind loc () Nothing a1 a2)


        go lc (BindMany c1 []) = go_comp lc c1
        go lc (BindMany c1 ((x,c2):xscs)) 
           | x `S.notMember` (fst $ compFVs c2) -- not in c2 and not in xscs
           , x `S.notMember` (S.unions (map (fst. compFVs. snd) xscs)) 
           = liftM2 (aBind loc lc Nothing) (go_comp lc c1) (go lc (BindMany c2 xscs))
           | otherwise
           = liftM2 (aBind loc lc (Just x)) (go_comp lc c1) (go lc (BindMany c2 xscs))

        go lc (Seq c1 c2)
           = liftM2 (aBind loc lc Nothing) (go_comp lc c1) (go_comp lc c2)


        go lc (Par p c1 c2)
          | ReadSrc {} <- unComp c1 = go_comp lc c2
          | WriteSnk {} <- unComp c2 = go_comp lc c1
          | otherwise
          = liftM3 (aPar loc lc p) (go_comp lc c1) (return t) (go_comp lc c2)
            where t = yldTyOfCTy (ctComp c1)

        go lc (Emit e)                     = aEmit1 loc lc <$> (liftIO $ transLiftedExp dfs sym e)
        go lc (Emits (MkExp (EVar x) _ _)) = do s <- liftIO $ newBlockId sym loc "tk1"
                                                return $ aEmitN loc lc s t n x
          where TArray (Literal n) t = nameTyp x

        go lc (Take1 t)  = do s <- liftIO $ newBlockId sym loc "tk1"
                              return $ aTake1 loc lc s t
        go lc (Take t n) = do s <- liftIO $ newBlockId sym loc "tkN"
                              return $ aTakeN loc lc s t n

        go lc (Until (MkExp (EVar x) _ _) c) = aUntil loc lc x <$> go_comp lc c
        go lc (While (MkExp (EVar x) _ _) c) = aWhile loc lc x <$> go_comp lc c 

        go lc (Repeat _ c) = aRepeat loc lc <$> go_comp lc c
        go lc (VectComp _ c) = go_comp lc c

        go lc (Mitigate s t 1 1)
          = do b <- liftIO $ newBlockId sym loc s
               return $ aRepeat loc lc $ aCast loc lc b (1,t) (1,t)
        go lc (Mitigate s t n1 1) 
          = do b <- liftIO $ newBlockId sym loc s
               return $ aRepeat loc lc $ 
                 aCast loc lc b (1, TArray (Literal n1) t) (n1,t)
        go lc (Mitigate s t 1 n2) 
          = do b <- liftIO $ newBlockId sym loc s 
               return $ aRepeat loc lc $
                 aCast loc lc b (n2,t) (1,TArray (Literal n2) t)
        go lc (Mitigate s t n1 n2)
          | n1 `mod` n2 == 0 -- n1 = k*n2
          , let k = n1 `div` n2
          = do b <- liftIO $ newBlockId sym loc s 
               return $ aRepeat loc lc $ 
                 aCast loc lc b (1,TArray (Literal n1) t) (k,TArray (Literal n2) t)

          | n2 `mod` n1 == 0
          , let k = n2 `div` n1
          = do b <- liftIO $ newBlockId sym loc s 
               return $ aRepeat loc lc $ 
                 aCast loc lc b (k,TArray (Literal n1) t) (1,TArray (Literal n2) t)
   
          | otherwise
          = panicStr "Ill typed mitigate node during Atomix translation!"
                   
        -- NB: Change from lc to lc'!
        go _lc (Standalone lc' c) = go_comp (Just lc') c

        go lc (Branch (MkExp (EVar x) _ _) c1 c2)
          = liftM2 (aBranch loc lc x) (go_comp lc c1) (go_comp lc c2)

        go _lc (ReadSrc _t)  = panicStr "Standalone ReadSrc!?"  
        go _lc (WriteSnk _t) = panicStr "Standalone WriteSnk!?"

        go _lc _other = panic $ 
                    vcat [ text "Unexpected comp in transLifted!"
                         , nest 2 $ ppr comp
                         ] 


