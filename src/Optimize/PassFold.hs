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
{-# OPTIONS_GHC -Wall -Wwarn #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, GeneralizedNewtypeDeriving, MultiWayIf, QuasiQuotes #-}
module PassFold (runFold, elimMitigsIO) where

import Prelude hiding (exp)
import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (isJust)
import Data.Monoid
import System.CPUTime
import Text.Parsec.Pos (SourcePos)
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as S

import AstComp
import AstExpr
import AstUnlabelled
import CtExpr ( ctExp  )
import Eval
import Opts
import PassFoldDebug
import PpComp ()
import PpExpr ()
import qualified GenSym as GS

{-------------------------------------------------------------------------------
  Rewriting monad
-------------------------------------------------------------------------------}

data IsRewritten = NotRewritten | Rewritten

-- The rewriting monad keeps track of whether any rewriting actually happened
newtype RwM a = RwM { unRwM :: StateT IsRewritten (ReaderT (GS.Sym, DynFlags) IO) a }
  deriving (Functor, Applicative, Monad)

runRwM :: RwM a -> (GS.Sym, DynFlags) -> IO (a, IsRewritten)
runRwM act = runReaderT (runStateT (unRwM act) NotRewritten)

rewrite :: a -> RwM a
rewrite a = RwM $ do
    put Rewritten
    return a

genSym :: String -> RwM String
genSym prefix = RwM $ do
    gs   <- fst `liftM` ask
    str' <- liftIO $ GS.genSymStr gs
    return (prefix ++ str')

getDynFlags :: RwM DynFlags
getDynFlags = RwM $ snd `liftM` ask

logStep :: String -> Maybe SourcePos -> String -> RwM ()
logStep pass pos str = do
    flags <- getDynFlags
    when (isDynFlagSet flags DebugFold) $ RwM $ do
      liftIO $ putStrLn $ pass ++ ": " ++ ppr' pos ++ "\n  " ++ str
  where
    ppr' (Just p) = show p ++ ": "
    ppr' Nothing  = ""

{-------------------------------------------------------------------------------
  Rewriting statistics
-------------------------------------------------------------------------------}

newtype RwStats = RwStats { getRwStats :: Map.Map String RwStepStats }

data RwStepStats = RwStepStats {
    rw_invoked  :: Int
  , rw_rewrote  :: Int
  , rw_inv_time :: Double
  , rw_rew_time :: Double
  }

printRwStats :: RwStats -> IO ()
printRwStats mp =
    mapM_ print_one (Map.toList $ getRwStats mp)
  where
    print_one (pn, RwStepStats{..}) =
      printf "%20s:%d/%d/%f, %f\n" pn rw_invoked
                                      rw_rewrote
                                      rw_inv_time
                                      rw_rew_time

incInvokes :: RwStats -> Double -> String -> RwStats
incInvokes mp d s = RwStats (Map.alter aux s $ getRwStats mp)
  where
    aux Nothing                       = Just (RwStepStats 1 0 d 0)
    aux (Just (RwStepStats i r d0 t)) = Just (RwStepStats (i+1) r (d0+d) t)

incRewrites :: RwStats -> Double -> String -> RwStats
incRewrites mp d s = RwStats (Map.alter aux s $ getRwStats mp)
  where
    aux Nothing                       = Just (RwStepStats 1 1 0 d)
    aux (Just (RwStepStats i r _t f)) = Just (RwStepStats i (r+1) d (f+d))

{-------------------------------------------------------------------------------
  Top-level: definition of a pass, and infrastructure to run passes
-------------------------------------------------------------------------------}

-- | A computation pass is defined only on each node of computations;
-- it gets lifted to the whole computation in `runTypedCompPass`.
--
-- We pass in the location of the computation as a separate argument for
-- convenience.
newtype TypedCompPass = TypedCompPass (Maybe SourcePos -> Comp -> RwM Comp)

-- | An expression pass is defined only on each node of expressions;
-- it gets lifted to the whole computation in `runTypedExpPass`.
newtype TypedExpPass = TypedExpPass (Maybe SourcePos -> Exp -> RwM Exp)

-- | Note: when composing passes you compose their action _on each node_, not
-- on the tree as a whole.
instance Monoid TypedCompPass where
  mempty = TypedCompPass $ \_cloc comp ->
    return comp
  TypedCompPass f `mappend` TypedCompPass g = TypedCompPass $ \cloc comp -> do
    comp' <- f cloc comp
    g (compLoc comp') comp'

instance Monoid TypedExpPass where
  mempty = TypedExpPass $ \_eloc exp ->
    return exp
  TypedExpPass f `mappend` TypedExpPass g = TypedExpPass $ \eloc exp -> do
    exp' <- f eloc exp
    g (expLoc exp') exp'

runTypedCompPass :: TypedCompPass -> Comp -> RwM Comp
runTypedCompPass (TypedCompPass f) =
    mapCompM return return return return return f'
  where
    f' comp = f (compLoc comp) comp

runTypedExpPass :: TypedExpPass -> Comp -> RwM Comp
runTypedExpPass (TypedExpPass f) =
    mapCompM return return return return (mapExpM return return f') return
  where
    f' exp = f (expLoc exp) exp

-- | Run a set of passes
--
-- We run each pass in sequence; when a pass rewrites the term in some way,
-- we run it again, until it does no further rewriting.
runPasses :: (GS.Sym, DynFlags)
          -> RwStats
          -> [(String, Comp -> RwM Comp)]
          -> Comp
          -> IO (Bool, Comp, RwStats)
runPasses (sym, flags) = go False
  where
    go :: Bool                          -- ^ Did we rewrite at all?
       -> RwStats                       -- ^ Statistics so far
       -> [(String, Comp -> RwM Comp)]  -- ^ Passes left to run
       -> Comp                          -- ^ Computation to rewrite
       -> IO (Bool, Comp, RwStats)
    go b mp [] comp =
      return (b, comp, mp)
    go b mp ((pn,p):ps) comp = do
--    printf "Pass: %10s" pn
      ((comp', rewritten), time) <- measure $ runRwM (p comp) (sym, flags)
      let mp' = incInvokes mp time pn
      case rewritten of
        NotRewritten -> do
          -- printf "... not rewritten :-( \n"
          go b mp' ps comp
        Rewritten -> do
          -- printf "... rewritten     :-) \n"
          -- ; printf "comp = %s\n" (show comp)
          go True (incRewrites mp' time pn) ((pn,p):ps) comp'

-- | Perform folding (run all standard passes)
runFold :: DynFlags -> GS.Sym -> Comp -> IO Comp
runFold flags sym = \comp -> do
     (comp', mp') <- go (RwStats Map.empty) 0 comp

     when (isDynFlagSet flags Verbose) $ do
       putStrLn "Optimizer statistics:"
       printRwStats mp'

     return comp'
  where
    compPasses = map (second runTypedCompPass) (foldCompPasses flags)
    expPasses  = map (second runTypedExpPass)  (foldExpPasses  flags)
    passes     = compPasses ++ expPasses

    go :: RwStats -> Integer -> Comp -> IO (Comp, RwStats)
    go mp depth comp
      | depth >= 10  = return (comp,mp)
      | otherwise    = do
          (rw_happened,comp1,mp1) <- runPasses (sym, flags) mp passes comp
          if rw_happened then go mp1 (depth+1) comp1
                         else return (comp1,mp1)

foldCompPasses :: DynFlags -> [(String,TypedCompPass)]
foldCompPasses flags
  | isDynFlagSet flags NoFold
  = []
  | otherwise
  = -- Standard good things
    [ ("fold"          , fold_step         )
    , ("purify"        , purify_step       )
    , ("purify-letref" , purify_letref_step)
    , ("elim-times"    , elim_times_step   )
    , ("letfunc"       , letfunc_step      )
    , ("letfun-times"  , letfun_times_step )
    , ("times-unroll"  , times_unroll_step )
    , ("inline"        , inline_step       )

    -- More aggressive optimizations
    , ("push-comp-locals"       , push_comp_locals_step   )
    , ("take-emit"              , take_emit_step          )
    , ("float-letfun-repeat"    , float_letfun_repeat_step)
    , ("float-let-par-step"     , float_let_par_step      )
    , ("ifdead"                 , ifdead_step             )
    , ("if-return-step"         , if_return_step          )
    , ("elim-automapped-mitigs" , elim_automapped_mitigs  )

    -- Don't use: not wrong but does not play nicely with LUT
    --  , ("float-top-letref"   , float_top_letref_step )
    ]

foldExpPasses :: DynFlags -> [(String,TypedExpPass)]
foldExpPasses flags
  | isDynFlagSet flags NoFold || isDynFlagSet flags NoExpFold
  = []
  | otherwise
  = [ ("for-unroll"         , for_unroll_step   )
    , ("exp-inlining-steps" , exp_inlining_steps)
    , ("asgn-letref-step"   , asgn_letref_step  )
    , ("exp_let_push_step"  , exp_let_push_step )
    , ("rest-chain"         , rest_chain        )
    ]

{-------------------------------------------------------------------------------
  Comp passes
-------------------------------------------------------------------------------}

-- | Just a single step of converting a return to a let
--
-- NOTE: We have to manually the entire bind structure, because although the
-- rewriting will happen at every node in the AST, there may only be a single
-- bind node for a sequence of binds. The same is not true for a sequence of
-- 'seq' nodes though. This is rather subtle, and fold_step is the _only_ pass
-- that actually does this. I'm not sure if there are other subtle bugs due to
-- this.
fold_step :: TypedCompPass
fold_step = TypedCompPass $ \_ -> go
  where
    go comp = do
      let cloc = compLoc comp
      case bindSeqView comp of
        BindView nm (MkComp (Return fi e) _ ()) c12 -> do
          logStep "fold_step/bind" cloc [step| nm <- return e ; .. ~~> let nm = e in .. |]
          c12' <- go c12
          rewrite $ cLetE cloc nm fi e c12'

        BindView nm c c12 -> do
          c12' <- go c12
          return $ cBindMany cloc c [(nm, c12')]

        SeqView (MkComp (Return fi e) _ ()) c12 -> do
          let nm = mkUniqNm cloc (ctExp e)
          logStep "fold_step/seq" cloc [step| return e ; .. ~~> let nm = e in .. |]
          c12' <- go c12
          rewrite $ cLetE cloc nm fi e c12'

        _otherwise -> do
          return comp

    mkUniqNm :: Maybe SourcePos -> Ty -> GName Ty
    mkUniqNm loc tp = toName ("__fold_unused_" ++ getLnNumInStr loc) loc tp

{-------------------------------------------------------------------------------
  TODO: Not yet cleaned up
-------------------------------------------------------------------------------}

{- The main rewriter individual steps
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}


float_letfun_repeat_step :: TypedCompPass
-- Rewriting of the form:
--
-- repeat (letfun f(params) = c in f(args))
-- ~~~>
-- letfun f(params) =
--     uninitialized-locals;
--     repeat c
-- in f(args)
-- This will give the opportunity to take-emit to kick in and rewrite the whole thing to map!
float_letfun_repeat_step = TypedCompPass $ \cloc comp -> if
    | Repeat wdth rcomp <- unComp comp
      , LetFunC nm params cbody ccont <- unComp rcomp
      , Call nm' args <- unComp ccont
      , nm' == nm
      , all is_simpl_call_arg args  -- Not sure this is necessary
     -> rewrite $ cLetFunC cloc nm params (cRepeat cloc wdth cbody) ccont
    | otherwise
     -> return comp



float_let_par_step :: TypedCompPass
float_let_par_step = TypedCompPass $ \cloc comp -> if
    | Par p c1 c2 <- unComp comp
      , LetE x fi e1 c1' <- unComp c1
     -> rewrite $ cLetE cloc x fi e1 (cPar cloc p c1' c2)
    | Par p c1 c2 <- unComp comp
      , LetE x fi e2 c2' <- unComp c2
     -> rewrite $ cLetE cloc x fi e2 (cPar cloc p c1 c2')
    | otherwise
     -> return comp



is_simpl_call_arg :: CallArg Exp Comp -> Bool
is_simpl_call_arg (CAExp e) = is_simpl_expr e
is_simpl_call_arg _         = False

from_simpl_call_param :: GName (CallArg Ty CTy) -> Maybe (GName Ty)
from_simpl_call_param nm =
  case nameTyp nm of CAExp  t -> Just nm{nameTyp = t}
                     CAComp _ -> Nothing

push_comp_locals_step :: TypedCompPass
-- let comp f(x) = var ... x <- take ; emit e
-- ~~~>
-- let comp f(x) = x <- take; emit (letref locals in e)
push_comp_locals_step = TypedCompPass $ \cloc comp -> if
    | LetFunC nm params cbody_with_locals ccont <- unComp comp
      , (locals, cbody) <- extractCMutVars cbody_with_locals
      , not (null locals)
      , BindMany tk [(x,emt)] <- unComp cbody
      , Take1 {} <- unComp tk
      , Emit e <- unComp emt
     -> do { let comp'  = cLetFunC cloc nm params cbody' ccont
                 cbody' = cBindMany cloc tk [(x,emt')]
                 emt'   = cEmit cloc e'
                 e'     = insertEMutVars locals e
           ; rewrite comp'
           }
    | otherwise
     -> return comp


take_emit_step :: TypedCompPass
-- repeat (x <- take ; emit e) ~~~> let f(x) = e in map(f)
take_emit_step = TypedCompPass $ \cloc comp -> if
    | Repeat nfo bm <- unComp comp
      , BindMany tk [(x,emt)] <- unComp bm
      , Take1 ain <- unComp tk
      , Emit e    <- unComp emt
     -> do { let xty  = ain
                 ety  = ctExp e
                 eloc = expLoc e
                 fty  = TArrow [xty] ety

           ; fname <- do { fr <- genSym "auto_map_"
                         ; return $ toName fr eloc fty }

           ; let letfun  = cLetHeader cloc fun mapcomp
                 mapcomp = cMap cloc nfo fname
                         -- NB: We pass the nfo thing,
                         -- to preserve vectorization hints!
                 fun = mkFunDefined eloc fname [x] e

           ; rewrite letfun
           }
    | otherwise
     -> return comp

{-

Not wrong, by evil, we lose the letref-structure and LUT can't do a
very good job!

float_top_letref_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
float_top_letref_step fgs comp
  | LetFun nm f c <- unComp comp
  , MkFunDefined nm params locals body <- unFun f
  , let (extra_locals,rem_body) = strip_letrefs body
  , not (null extra_locals)
  = do { let f' = f { unFun = fdef' }
             fdef' = MkFunDefined nm params (locals ++ extra_locals) rem_body
       ; rewrite $
         cLetFun (compLoc comp) (compInfo comp) nm f' c
       }
  | otherwise
  = return comp
  where strip_letrefs e = go [] e
        go defs e
          = go0 defs (unExp e)
          where
             go0 defs (ELetRef nm (Left ty) e')
               = go ((nm,ty,Nothing):defs) e'
             go0 defs (ELetRef nm (Right einit) e')
               = go ((nm, info einit, Just einit):defs) e'
             go0 defs _other = (reverse defs, e)
-}


inline_step :: TypedCompPass
inline_step = TypedCompPass $ \cloc comp -> if
    | Let nm c1 c2 <- unComp comp
     -> rewrite $ substComp [] [] [(nm,c1)] c2

{- Too   much inlining!
    | LetE nm e1 c2 <- unComp comp
      , isDynFlagSet fgs AutoLUT
      , Just rgs <- varRanges e1
      , Right True <- shouldLUT [] rgs e1    -- and the expression is luttable
      , Just (_, [], _) <- inOutVars [] Map.empty e1
     -> substExpComp (nm,e1) c2 >>= rewrite
-}
    | LetE nm ForceInline e1 c2 <- unComp comp
     -> rewrite $ substComp [] [(nm,e1)] [] c2

    | LetE _nm NoInline _e1 _c2 <- unComp comp
     -> return comp

    | LetE nm AutoInline e1 c2 <- unComp comp
      , is_simpl_expr e1
     -> rewrite $ substComp [] [(nm,e1)] [] c2


    | LetHeader f@(MkFun (MkFunDefined {}) _ _) c2 <- unComp comp
      , MkFunDefined nm params body' <- unFun f -- Defined
      , (locals, body) <- extractEMutVars body'
      , no_lut_inside body                      -- Not already lutted body

     -> do { c2' <- inline_exp_fun_in_comp (nm,params,locals,body) c2
             -- Inlining functions not always completely eliminates
             -- them (e.g. args to map, or non-simple arguments)
           ; if S.member nm (compEFVs c2')
               then return $ cLetHeader cloc f c2'
               else return c2'
           }

    | LetFunC nm _params _c1 c2 <- unComp comp
      , not (S.member nm (compCFVs c2)) -- Completely unused
     -> rewrite c2

    | LetFunC nm params c1 c2 <- unComp comp
     -> do { -- liftIO $ putStrLn $ "Inlining comp fun      = " ++ show nm
           ; c2' <- inline_comp_fun (nm,params,c1) c2
             -- liftIO $
             -- putStrLn $
             -- "nm member of rewritten = " ++ show (S.member nm (compFVs c2'))
             -- ; liftIO $ putStrLn $ "LFC-after = " ++ show c2'
           ; if S.member nm (compCFVs c2')
               then return $ cLetFunC cloc nm params c1 c2'
               else return $ c2'
           }

    | otherwise
     -> return comp

-- Just a heuristic for inlining: what are 'simple' expressions that
-- are safe and likely beneficial to just inline before code generation.
is_simpl_expr :: Exp -> Bool
is_simpl_expr = is_simpl_expr0 . unExp

is_simpl_expr0 :: Exp0 -> Bool
is_simpl_expr0 (EVal _ _)       = True
is_simpl_expr0 (EValArr _ _)    = True
is_simpl_expr0 (EVar _)         = True
is_simpl_expr0 (EUnOp _ e)      = is_simpl_expr e
is_simpl_expr0 (EStruct _ fses) = all is_simpl_expr (map snd fses)
is_simpl_expr0 _                = False

no_lut_inside :: Exp -> Bool
no_lut_inside x
    = isJust (mapExpM return return elut_nothing x)
  where
    elut_nothing :: Exp -> Maybe Exp
    elut_nothing (MkExp (ELUT {}) _ ()) = Nothing
    elut_nothing other                  = Just other



-- if e then return e1 else return e2 ~~> return (if e then e1 else e2)
if_return_step :: TypedCompPass
if_return_step = TypedCompPass $ \_cloc comp -> if
    | Branch eguard c1 c2 <- unComp comp
      , Return f1 e1  <- unComp c1
      , Return _f2 e2 <- unComp c2
   -- , f1 == f2
      , let cloc = compLoc comp
     -> rewrite $ cReturn cloc f1 $
                  eIf cloc eguard e1 e2
    | otherwise
     -> return comp


inline_exp_fun :: (GName Ty, [GName Ty], [MutVar], Exp)
               -> Exp -> RwM Exp
-- True means that it is safe to just get rid of the function
inline_exp_fun (nm,params,locals,body)
    = mapExpM return return replace_call
  where replace_call (MkExp (ECall nm' args) loc ())
--          | all is_simpl_expr args -- Like what we do for LetE/ELet
          | nm == nm'
          = do { let xs                        = zip params args
                     (subst, locals_from_args) = arg_subst xs
                     subst_len                 = len_subst xs
               ; rewrite $ mk_local_lets subst subst_len (locals_from_args ++ locals) body
               }
         where
            -- arg_subst will return (a) a substitution and (b) some local bindings
            arg_subst :: [(GName Ty, Exp)] -> ([(GName Ty, Exp)], [MutVar])
            arg_subst [] = ([],[])
            arg_subst ((prm_nm,arg):rest)
              -- An array of variable size.
              -- Here we must substitute for the size too
              | TArray (NVar siz_nm) _ <- nameTyp prm_nm
              , let (rest_subst, rest_locals) = arg_subst rest
                -- TODO: this size substitution can (should?) go once we
                -- disallow direct term-level references to type-level length
                -- variables (#76)
              , let siz_bnd = (toName siz_nm Nothing tint, arg_size (ctExp arg))
              = case how_to_inline prm_nm arg of
                  Left bnd  -> (bnd:siz_bnd:rest_subst, rest_locals)
                  Right bnd -> (siz_bnd:rest_subst, bnd:rest_locals)

              | otherwise
              , let (rest_subst, rest_locals) = arg_subst rest
              = case how_to_inline prm_nm arg of
                  Left bnd  -> (bnd:rest_subst, rest_locals)
                  Right bnd -> (rest_subst, bnd:rest_locals)

            -- Delicate: classifies which arrays are passed by
            -- reference. Should revisit.  But at the moment this is
            -- consistent with the semantics implemented in CgExpr.
            -- See for example codeGenArrRead in CgExpr.hs

            how_to_inline :: GName Ty -> Exp
                          -> Either (GName Ty, Exp) MutVar
            -- The choice is: either with a substitution (Left), or
            --                with a let-binding (Right)
            how_to_inline prm_nm arg
              = if is_simpl_expr arg
                then Left (prm_nm, arg)
                else case isArrayTy_maybe (nameTyp prm_nm) of
                       Just bty
                         | (bty /= TBit && is_array_ref arg)
                         -> Left (prm_nm, arg)
                       _otherwise -> Right (MutVar prm_nm (Just arg) (expLoc arg) ())

            is_array_ref (MkExp (EVar _)      _ ()) = True
            is_array_ref (MkExp (EArrRead {}) _ ()) = True
            is_array_ref _ = False

            len_subst :: [(GName Ty, Exp)] -> [(LenVar, NumExpr)]
            len_subst [] = []
            len_subst ((prm_nm,arg):rest)
              | TArray (NVar siz_nm) _ <- nameTyp prm_nm
              = (siz_nm, arg_numexpr (ctExp arg)):(len_subst rest)
              | otherwise
              = len_subst rest

            arg_numexpr :: Ty -> NumExpr
            arg_numexpr (TArray ne _t) = ne
            arg_numexpr _
              = error $
                "Could not deduce param size during inlining of function:" ++
                    show nm ++ ", at call location:" ++ show loc

            arg_size :: Ty -> Exp
            arg_size (TArray (NVar siz_nm) _) = eVar loc (toName siz_nm Nothing tint)
            arg_size (TArray (Literal siz) _) = eVal loc tint (vint siz)
            arg_size _
              = error $
                "Could not deduce param size during inlining of function:" ++
                    show nm ++ ", at call location:" ++ show loc

            mk_local_lets :: [(GName Ty, Exp)]
                          -> [(LenVar, NumExpr)]
                          -> [MutVar]
                          -> Exp
                          -> Exp
            mk_local_lets subst subst_len extended_locals
                = substExp subst_len subst . insertEMutVars extended_locals

        replace_call other = return other


inline_comp_fun :: (GName CTy, [GName (CallArg Ty CTy)], Comp) -> Comp -> RwM Comp
inline_comp_fun (nm,params,cbody) c
  = do { -- liftIO $ putStrLn $ "inline_comp_fun (before) = " ++ show c
       ; r <- mapCompM return return return return return replace_call c
       ; -- liftIO $ putStrLn $ "inline_comp_fun (after) = " ++ show r
       ; return r
       }
  where
    replace_call :: Comp -> RwM Comp
    replace_call (MkComp (Call nm' args) _ ())
      | all is_simpl_call_arg args
      , nm == nm'
      = do { -- liftIO $ putStrLn "Matching!"
           ; r <- rewrite $ substComp [] (mk_expr_subst params args) [] cbody
           ; -- liftIO $ putStrLn $ "Substituted =" ++ (show r)
           ; return r
           }
    replace_call other = return other

    mk_expr_subst :: [GName (CallArg Ty CTy)]
                  -> [CallArg Exp Comp]
                  -> [(GName Ty, Exp)]
    mk_expr_subst [] [] = []
    mk_expr_subst (p:ps1) (a:as1)
      = let CAExp ty = nameTyp p
            CAExp e1 = a
        in (p{nameTyp = ty}, e1) : mk_expr_subst ps1 as1
    mk_expr_subst _ _ = error "BUG: inline_comp_fun!"


inline_exp_fun_in_comp :: (GName Ty, [GName Ty], [MutVar], Exp)
                       -> Comp -> RwM Comp
inline_exp_fun_in_comp fun
  = mapCompM return return return return (inline_exp_fun fun) return


purify_step :: TypedCompPass
-- Returns Just if we managed to rewrite
purify_step = TypedCompPass $ \cloc comp ->
    -- rwMIO $ putStrLn "purify_step, comp = "
    -- rwMIO $ print (ppComp comp)
    case isMultiLet_maybe (unComp comp) of
      Just (binds, Return fi e) ->
        rewrite $ cReturn cloc fi (mkMultiLetExp (reverse binds) e)

      Just (binds, Emit e) ->
        rewrite $ cEmit cloc (mkMultiLetExp (reverse binds) e)

      Just (binds, Emits e) ->
        rewrite $ cEmits cloc (mkMultiLetExp (reverse binds) e)

      _otherwise ->
        return comp



purify_letref_step :: TypedCompPass
-- Returns Just if we managed to rewrite
purify_letref_step = TypedCompPass $ \cloc comp -> do
    -- rwMIO $ putStrLn "purify_step, comp = "
    -- rwMIO $ print (ppComp comp)
    case isMultiLetRef_maybe (unComp comp) of
      Just (binds, Return fi e) ->
        rewrite $ cReturn cloc fi (mkMultiLetRefExp (reverse binds) e)

      Just (binds, Emit e) ->
        rewrite $ cEmit cloc (mkMultiLetRefExp (reverse binds) e)

      Just (binds, Emits e) ->
        rewrite $ cEmits cloc (mkMultiLetRefExp (reverse binds) e)

      _otherwise ->
        return comp



ifdead_step :: TypedCompPass
ifdead_step = TypedCompPass $ \cloc comp -> do
    case unComp comp of
      Branch e c1 c2
        | Just b <- evalBool e
        -> rewrite $ if b then c1 else c2

      Branch e (MkComp (Branch e' c1 c2) _ ()) c3
        | e `impliesBool` e'
        -> rewrite $ cBranch cloc e c1 c3
        | e `impliesBoolNeg` e'
        -> rewrite $ cBranch cloc e c2 c3

      Branch e c1 (MkComp (Branch e' c2 c3) _ ())
        | eneg e `impliesBool` e'
        -> rewrite $ cBranch cloc e c1 c2
        | eneg e `impliesBoolNeg` e'
        -> rewrite $ cBranch cloc e c1 c3

      _otherwise -> return comp
  where

    eneg :: Exp -> Exp
    eneg e = eUnOp (expLoc e) Neg e

    impliesBool :: Exp -> Exp -> Bool
    impliesBool (MkExp (EBinOp Eq e  (MkExp (EVal _ (VInt j )) _ ())) _ ())
                (MkExp (EBinOp Eq e' (MkExp (EVal _ (VInt j')) _ ())) _ ())
       | e `expEq` e'
       = j == j'
    impliesBool _ _ = False

    impliesBoolNeg :: Exp -> Exp -> Bool
    impliesBoolNeg (MkExp (EBinOp Eq e  (MkExp (EVal _ (VInt j )) _ ())) _ ())
                   (MkExp (EBinOp Eq e' (MkExp (EVal _ (VInt j')) _ ())) _ ())
       | e `expEq` e'
       = j /= j'
    impliesBoolNeg _ _ = False




letfunc_step :: TypedCompPass
-- Rewriting of the form:
--
--   letfunc f(..) = return e
--   in ... f(...)
--
--   ~~>
--
--   let f(..) = e
--   in ... return (f(..)) ...
--
-- Which will allow more drastic inlining in the inlining step.
--
letfunc_step = TypedCompPass $ \cloc comp -> do
    case unComp comp of
      LetFunC nm params (MkComp (Return _fi e) _ ()) cont
         | Just params' <- mapM from_simpl_call_param params
         -> do { let fun_ty :: Ty
                     fun_ty = TArrow (map nameTyp params') (ctExp e)

                     f :: GName Ty
                     f = nm { nameTyp = fun_ty }

                     fun :: Fun
                     fun = mkFunDefined cloc f params' e

                     replace_call :: Comp -> RwM Comp
                     replace_call (MkComp (Call f' es) xloc ())
                       | uniqId f == uniqId f'
                       = let es'  = map unCAExp es
                             call = eCall xloc f es'
                         in rewrite $ cReturn xloc AutoInline call
                     replace_call other = return other

                     purify_calls :: Comp -> RwM Comp
                     purify_calls = mapCompM return return return return return replace_call

               ; cont' <- purify_calls cont
               ; rewrite $ cLetHeader cloc fun cont' -- Same type!
               }
      _ -> return comp

-- > for i in [e,elen] { let f(params) = ... in cont }
-- > ~~~>
-- > let f(i,params) = ... in for i in [e,elen] { cont }
letfun_times_step :: TypedCompPass
letfun_times_step = TypedCompPass $ \_cloc comp ->
    case unComp comp of
      Times ui e elen i (MkComp (LetHeader def cont) cloc ())
       | MkFun (MkFunDefined f params body) floc () <- def
       -> do { let fty' = TArrow (map nameTyp (i:params)) (fun_ret_ty (nameTyp f))
                   f'   = f { nameTyp = fty' }
                   def' = mkFunDefined floc f' (i:params) body
                   iexp = eVar cloc i -- The counter variable
             ; cont' <- augment_calls f' iexp cont
             ; rewrite $ cLetHeader cloc def' (cTimes cloc ui e elen i cont')
             }
      _otherwise -> return comp

  where
    fun_ret_ty :: Ty -> Ty
    fun_ret_ty (TArrow _ r) = r
    fun_ret_ty  _           = error "Function not having an arrow type!?"

    augment_calls :: GName Ty -> Exp -> Comp -> RwM Comp
    augment_calls f' iexp = mapCompM return return return return replace_ecall return
      where
        replace_ecall (MkExp (ECall f es) xloc ())
          | uniqId f' == uniqId f
          = rewrite $ eCall xloc f' (iexp:es)
        replace_ecall other = return other

-- | Loop unrolling
--
-- > for i in [0,n] { c }
-- > ~~~>
-- > c ; ... ; c
times_unroll_step :: TypedCompPass
times_unroll_step = TypedCompPass $ \cloc comp -> do
    let unused :: GName Ty
        unused = toName ("__unroll_unused_" ++ getLnNumInStr cloc) Nothing TUnit

        mk_bind_many :: [Comp] -> Comp
        mk_bind_many []     = error "times_unroll_step: can't happen!"
        mk_bind_many [x]    = x
        mk_bind_many (x:xs) = cBindMany (compLoc x) x [(unused, mk_bind_many xs)]

    case unComp comp of
      Times ui e elen i c
       | EVal _ (VInt n') <- unExp elen
       , let n = fromIntegral n'
       , n > 0
       , EVal valTy (VInt 0) <- unExp e
       , (n < 3 && ui == AutoUnroll) || (ui == Unroll)
-- BOZIDAR: this will currently fail perf test for TX/test_encoding_34
--         , ui == Unroll -- || (n < 3 && n > 0 && ui == AutoUnroll)
       -> let idxs = [0..n-1]
              comps = replicate n c
              unrolled = zipWith (\curr xc ->
                substComp [] [(i, eVal (expLoc e) valTy (vint curr))] [] xc) idxs comps
          in rewrite $ mk_bind_many unrolled
      _ -> return comp




mkMultiLetExp :: [(GName Ty, Exp, ForceInline)] -> Exp -> Exp
mkMultiLetExp []             e = e
mkMultiLetExp ((x,e1,fi):bs) e = eLet (expLoc e) x fi e1 (mkMultiLetExp bs e)

mkMultiLetRefExp :: [(GName Ty, Maybe Exp)] -> Exp -> Exp
mkMultiLetRefExp []          e = e
mkMultiLetRefExp ((x,b1):bs) e = eLetRef (expLoc e) x b1 (mkMultiLetRefExp bs e)

isMultiLet_maybe :: Comp0 -> Maybe ([(GName Ty, Exp, ForceInline)], Comp0)
isMultiLet_maybe = go []
  where
    go acc (LetE x fi e c) = go ((x,e,fi):acc) (unComp c)

    go [] (Return {}) = Nothing
    go [] (Emit {})   = Nothing
    go [] (Emits {})  = Nothing

    -- We must have some accumulated bindings!
    go acc c@(Return {}) = Just (acc,c)
    go acc c@(Emit {})   = Just (acc,c)
    go acc c@(Emits {})  = Just (acc,c)
    go _   _             = Nothing

isMultiLetRef_maybe :: Comp0 -> Maybe ([(GName Ty, Maybe Exp)], Comp0)
isMultiLetRef_maybe = go []
  where
    go acc (LetERef x e c) = go ((x,e):acc) (unComp c)

    go [] (Return {}) = Nothing
    go [] (Emit {})   = Nothing
    go [] (Emits {})  = Nothing

    -- We must have some accumulated bindings!
    go acc c@(Return {}) = Just (acc,c)
    go acc c@(Emit {})   = Just (acc,c)
    go acc c@(Emits {})  = Just (acc,c)
    go _   _             = Nothing

-- > for cnt in [estart, ebound] { return ebody }
-- > ~~~>
-- > return (for cnt in [estart, ebound] ebody)
elim_times_step :: TypedCompPass
elim_times_step = TypedCompPass $ \cloc comp -> do
    case unComp comp of
      Times ui estart ebound cnt (MkComp (Return _ ebody) _cloc ()) ->
        rewrite $ cReturn cloc AutoInline
                    (eFor cloc ui cnt estart ebound ebody)

      _ -> return comp




elim_automapped_mitigs :: TypedCompPass
elim_automapped_mitigs = TypedCompPass $ \_cloc c -> if
  | MkComp c0 _cloc _ <- c
    , Par _p c1 c2 <- c0
    , Par _p c11 c12 <- unComp c1
    , LetHeader fun (MkComp (Map _v f) _ _) <- unComp c12  -- (c1 - map f) - c2
    , Mitigate ty1 i1 j1 <- unComp c11
    , Mitigate ty2 i2 j2 <- unComp c2           -- (c11' -- mitigate(i2,j2) -- map f) - mitigate(i2,j2) - c2'
    , i1 >= j1 && i2 <= j2                      -- down mit and up mit
    , let d1 = i1 `div` j1
    , let d2 = j2 `div` i2
    , d1 == d2                                  -- exactly the same rate
    , funName fun == f
   -> rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f, fun)

  | MkComp c0 _cloc _ <- c
    , Par _p c1 c2 <- c0
    , Par _p c21 c22 <- unComp c2
    , LetHeader fun (MkComp (Map _v f) _ _) <- unComp c21  -- c1 - (map f - c2)
    , Mitigate ty1 i1 j1 <- unComp c1      -- (c11' -- mitigate(i1,j1) -- map f) - c2
    , Mitigate ty2 i2 j2 <- unComp c22     -- (c11' -- mitigate(i2,j2) -- map f) - mitigate(i2,j2) - c2'
    , i1 >= j1 && i2 <= j2                      -- down mit and up mit
    , let d1 = i1 `div` j1
    , let d2 = j2 `div` i2
    , d1 == d2                                  -- exactly the same rate
    , funName fun == f
   -> rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f, fun)

  | otherwise
   -> return c



rewrite_mit_map :: Ty -> (Int,Int) -> Ty -> (Int,Int) -> (EId, Fun) -> RwM Comp
-- Precondition:  i1 `div` j1 == j2 `div` i2
rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f_name, fun)
  = do { let rng1 = if j1 == 1 then LISingleton else LILength j1
       ; let rng2 = if i2 == 1 then LISingleton else LILength i2
       ; let d = i1 `div` j1
       ; let floc = funLoc fun

         -- input variable
       ; x <- genSym "x"
       ; let x_ty   = TArray (Literal i1) ty1   -- input type
       ; let x_name = toName x floc x_ty
       ; let x_exp  = eVar floc x_name

         -- output variable
       ; y <- genSym "y"
       ; let y_ty   = TArray (Literal j2) ty2      -- output type
       ; let y_name = toName y floc y_ty
       ; let y_exp  = eVar floc y_name

         -- name of new map function
       ; new_f <- genSym "auto_map_mit"
       ; let f_ty = TArrow [x_ty] y_ty
       ; let new_f_name = toName new_f floc f_ty


         -- new counter
       ; i <- genSym "i"
       ; let i_name = toName i floc tint
       ; let i_exp  = eVar floc i_name

         -- zero and 'd'
       ; let ezero = eVal floc tint (vint (0 :: Int))
       ; let e_d   = eVal floc tint (vint d)


       ; let new_body
               = eLetRef floc y_name Nothing $
                 eSeq floc
                  (eFor floc AutoUnroll i_name ezero e_d ekernel)
                  -- do the for loop
                  y_exp -- and return y

             write_idx = eBinOp floc Mult (eVal floc tint (vint i2)) i_exp
             read_idx  = eBinOp floc Mult (eVal floc tint (vint j1)) i_exp

             earrrd    = eArrRead floc x_exp read_idx rng1
             ekernel   = eArrWrite floc y_exp write_idx rng2 $
                         eCall floc f_name [earrrd]

      ; let new_mapper = cMap floc Nothing new_f_name
      ; let new_fun = MkFun (MkFunDefined new_f_name [x_name] new_body) floc ()

      ; rewrite $
        cLetHeader floc fun $   -- original function
        cLetHeader floc new_fun new_mapper
      }








arrinit_step :: TypedExpPass
-- Statically initialize as many arrays as possible
arrinit_step = TypedExpPass $ \_ e1 -> case evalArrInt e1 of
    Nothing   -> return e1
    Just vals -> rewrite $ eValArr (expLoc e1) (ctExp e1) (map VInt vals)

exp_inlining_steps :: TypedExpPass
exp_inlining_steps = TypedExpPass $ \_ e -> do
  fgs <- getDynFlags

  if | ELet nm ForceInline e1 e2 <- unExp e
      -> rewrite $ substExp [] [(nm,e1)] e2 -- Forced Inline!

     | ELet _nm NoInline _e1 _e2 <- unExp e
      -> return e

     | ELet nm AutoInline e1 e2 <- unExp e
       , let fvs = exprFVs e2
       , let b = nm `S.member` fvs
      -> if not b then
            if not (mutates_state e) then rewrite e2
            else return e
         else if is_simpl_expr e1 && not (isDynFlagSet fgs NoExpFold)
         then rewrite $ substExp [] [(nm,e1)] e2
         else return e

     | otherwise
      -> return e

exp_let_push_step :: TypedExpPass
exp_let_push_step = TypedExpPass $ \_ e -> if
 | ELet nm fi e1 e2 <- unExp e
   , EArrWrite e0 estart0 elen0 erhs <- unExp e2
   , EArrRead evals estart rlen <- unExp erhs
   , let fvs = foldr (S.union . exprFVs) S.empty [e0, estart0, evals]
   , not (nm `S.member` fvs)
  -> let estart' = eLet (expLoc estart) nm fi e1 estart
     in rewrite $
        eArrWrite (expLoc e2) e0 estart0 elen0 $
        eArrRead (expLoc erhs) evals estart' rlen
 | otherwise
  -> return e



asgn_letref_step :: TypedExpPass
asgn_letref_step = TypedExpPass $ \_ exp -> if
  | EArrWrite e0 estart elen erhs <- unExp exp
    , TArray _ ty <- ctExp e0
    , not (ty == TBit)
     -- It has to be LILength so we can just take a pointer
    , LILength _ <- elen
    , EVar _ <- unExp e0
     -- Just a simple expression with no side-effects
    , not (mutates_state estart)
    , Just (y, residual_erhs) <- returns_letref_var erhs
   -> rewrite $ substExp [] [(y, eArrRead (expLoc exp) e0 estart elen)] residual_erhs
  | otherwise
   -> return exp
  where
    returns_letref_var :: Exp -> Maybe (GName Ty, Exp)
    returns_letref_var = go []

    go :: [GName Ty] -> Exp -> Maybe (GName Ty, Exp)
    go letrefs e
     = let loc = expLoc e
       in
       case unExp e of
          EVar v ->
            if v `elem` letrefs
              then Just (v, eVal loc TUnit VUnit)
              else Nothing

          ELet y fi e1 e2 -> do
            (w, e2') <- go letrefs e2
            return (w, eLet loc y fi e1 e2')

          ELetRef y Nothing e2 -> do
            (w, e2') <- go (y:letrefs) e2
            if w == y
              then return (w, e2')
              else return (w, eLetRef loc y Nothing e2')

          ESeq e1 e2 -> do
            (w, e2') <- go letrefs e2
            return (w, eSeq loc e1 e2')

          _ -> Nothing

-- NOTE: There was a (seemingly outdated) comment above `proj_inline_step` that
--
-- This leads to too much inlining and seems to have unstable effects to
-- performance so I am keeping it commented for now:
rest_chain :: TypedExpPass
rest_chain = mconcat [
      alength_elim
    , eval_arith
    , const_fold
    , arrinit_step
    , subarr_inline_step
    , proj_inline_step
    ]

alength_elim :: TypedExpPass
alength_elim = TypedExpPass $ \_ e -> if
  | EUnOp ALength e0 <- unExp e
    , (TArray nexp _)  <- ctExp e0
    , let loc = expLoc e
   -> rewrite $ numexp_to_exp loc nexp
  | otherwise
   -> return e
  where
    numexp_to_exp loc (Literal i) = eVal loc tint (vint i)
    numexp_to_exp loc (NVar nm)   = eVar loc (toName nm Nothing tint)


eval_arith :: TypedExpPass
eval_arith = TypedExpPass $ \_ e -> if
  | arith_ty (ctExp e)      -- of arithmetic type
    , not (isEVal e)          -- not already a value
    , Just v <- evalArith e   -- evaluate it!
   -> rewrite $ eVal (expLoc e) (ctExp e) v
  | otherwise
   -> return e
  where
    arith_ty :: Ty -> Bool
    arith_ty (TInt _) = True
    arith_ty TDouble  = True
    arith_ty _        = False

subarr_inline_step :: TypedExpPass
subarr_inline_step = TypedExpPass $ \_ e -> if
  | EArrRead evals estart LISingleton <- unExp e
    , EValArr _ vals <- unExp evals
    , EVal _ (VInt n') <- unExp estart
    , let n = fromIntegral n'
   -> rewrite $ eVal (expLoc e) (ctExp e) (vals!!n)

  | EArrRead evals estart (LILength n) <- unExp e
    , EValArr _ vals <- unExp evals
    , EVal _ (VInt s') <- unExp estart
    , let s = fromIntegral s'
   -> rewrite $ eValArr (expLoc e) (ctExp e) (take n $ drop s vals)

    -- x[0,length(x)] == x
  | EArrRead evals estart (LILength n) <- unExp e
    , EVal _ (VInt 0) <- unExp estart
    , TArray (Literal m) _ <- ctExp evals
    , n == m
   -> rewrite evals

  | EArrWrite eval_tgt estart (LILength n) erhs <- unExp e
    , EVal _ (VInt 0) <- unExp estart
    , TArray (Literal m) _ <- ctExp eval_tgt
    , n == m
   -> rewrite $ eAssign (expLoc e) eval_tgt erhs

  | otherwise
   -> return e

proj_inline_step :: TypedExpPass
proj_inline_step = TypedExpPass $ \_ e -> if
  | EProj e' fn <- unExp e
    , EStruct _s fs_es <- unExp e'
    , all (is_simpl_expr . snd) fs_es -- no side effects
    , Just ep <- lookup fn fs_es
   -> rewrite ep
  | otherwise
   -> return e


for_unroll_step :: TypedExpPass
for_unroll_step = TypedExpPass $ \_ e -> if
  | EFor ui nm estart elen ebody  <- unExp e
    , EVal _ (VInt 0) <- unExp estart
    , EVal _ (VInt n') <- unExp elen
    , let n = fromIntegral n'
    , (n < 8 && n > 0 && ui == AutoUnroll) || ui == Unroll
   -> -- liftIO (putStrLn "for_unroll_step, trying ...") >>
      let idxs = [0..n-1]
          exps = replicate n ebody
          unrolled = zipWith (\curr xe -> substExp [] [(nm, eVal (expLoc e) tint (vint curr))] xe) idxs exps
      in case unrolled of
           [] -> return $ eVal (expLoc e) TUnit VUnit
           xs -> rewrite $ mk_eseq_many xs
  | otherwise
   -> return e
  where
    mk_eseq_many :: [Exp] -> Exp
    mk_eseq_many []     = error "for_unroll_step: can't happen!"
    mk_eseq_many [x]    = x
    mk_eseq_many (x:xs) = eSeq (expLoc x) x (mk_eseq_many xs)




const_fold :: TypedExpPass
const_fold = TypedExpPass $ \loc e ->
    case go (unExp e) of
      Nothing -> return e
      Just e' -> rewrite $ MkExp e' loc ()
  where
    go :: Exp0 -> Maybe Exp0
    go (EBinOp Add e1 e2) | EVal _ (VInt 0) <- unExp e1 =
        return $ unExp e2

    go (EBinOp Add e1 e2) | EVal _ (VInt 0) <- unExp e2 =
        return $ unExp e1

    go (EBinOp Add e1 e2) | EVal a   (VInt i1) <- unExp e1
                          , EVal _a' (VInt i2) <- unExp e2 =
        return $ EVal a (VInt (i1+i2))

    go (EBinOp Sub e1 e2) | EVal a   (VInt i1) <- unExp e1
                          , EVal _a' (VInt i2) <- unExp e2 =
        return $ EVal a (VInt (i1-i2))

    go (EBinOp Mult e1 e2) | EVal _ (VInt 1) <- unExp e1 =
        return $ unExp e2

    go (EBinOp Mult e1 e2) | EVal _ (VInt 1) <- unExp e2 =
        return $ unExp e1

    go (EBinOp Mult e1 e2) | EVal a  (VInt i1) <- unExp e1
                           , EVal _a' (VInt i2) <- unExp e2 =
        return $ EVal a (VInt (i1*i2))

    go (EBinOp Div e1 e2) | EVal a   (VInt i1) <- unExp e1
                          , EVal _a' (VInt i2) <- unExp e2 =
        return $ EVal a (VInt (i1 `quot` i2))

    go (EBinOp Rem e1 e2) | EVal a  (VInt i1) <- unExp e1
                          , EVal _a' (VInt i2) <- unExp e2 =
        return $ EVal a (VInt (i1 `rem` i2))

    go (EBinOp Eq e1 e2) | EVal _ (VInt i1) <- unExp e1
                         , EVal _ (VInt i2) <- unExp e2 =
        return $ if i1 == i2 then EVal TBool (VBool True) else EVal TBool (VBool False)

    go (EBinOp Neq e1 e2) | EVal _ (VInt i1) <- unExp e1
                          , EVal _ (VInt i2) <- unExp e2 =
        return $ if i1 /= i2 then EVal TBool (VBool True) else EVal TBool (VBool False)

    go (EBinOp Lt e1 e2) | EVal _ (VInt i1) <- unExp e1
                         , EVal _ (VInt i2) <- unExp e2 =
        return $ if i1 < i2 then EVal TBool (VBool True) else EVal TBool (VBool False)

    go (EBinOp Gt e1 e2) | EVal _ (VInt i1) <- unExp e1
                         , EVal _ (VInt i2) <- unExp e2 =
        return $ if i1 > i2 then EVal TBool (VBool True) else EVal TBool (VBool False)

    go (EIf e1 e2 e3) | EVal _ (VBool flag) <- unExp e1 =
        return $ if flag then unExp e2 else unExp e3

    go _ = Nothing






{- Elimination of mitigators
   ~~~~~~~~~~~~~~~~~~~~~~~~~ -}

elimMitigsIO :: DynFlags -> GS.Sym -> Comp -> IO Comp
elimMitigsIO flags sym = go
  where
    go comp = do { (comp', rewritten) <- runRwM (elimMitigs comp) (sym, flags)
                 ; case rewritten of
                     NotRewritten -> return comp
                     Rewritten    -> go comp'
                 }


frm_mit :: Comp -> Maybe ((Ty,Int,Int), Comp)
-- returns (mitigator,residual-comp)
frm_mit c
  | Par _p0 c1 c2 <- unComp c
  , Mitigate ty i1 i2  <- unComp c2
  = Just ((ty,i1,i2), c1)

  | Par p0 c1 c2 <- unComp c
  = case frm_mit c2 of
      Nothing -> Nothing
      Just (mit,c2') -> Just (mit, cPar (compLoc c) p0 c1 c2')

  | LetHeader fdef@(MkFun (MkFunDefined {}) _ _) cont <- unComp c
    -- Needed because of AutoMaps! Yikes!
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetHeader loc fdef cont')

  -- Special case for code emitted by the vectorizer
  | LetFunC fn prms body cont <- unComp c
  , Call fn' _args <- unComp cont
  , fn == fn'
  , let loc = compLoc c
  = case frm_mit body of
      Nothing -> Nothing
      Just (mit,body') -> Just (mit, cLetFunC loc fn prms body' cont)

  -- fallthrough general case
  | LetFunC fn prms body cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetFunC loc fn prms body cont')

  | Let n c1 cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLet loc n c1 cont')

  | LetE n f e cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetE loc n f e cont')

  | LetERef n me cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetERef loc n me cont')


  | otherwise
  = Nothing

flm_mit :: Comp -> Maybe ((Ty,Int,Int), Comp)
-- returns (mitigator,residual-comp)
flm_mit c
  | Par _p0 c1 c2 <- unComp c
  , Mitigate ty i1 i2  <- unComp c1
  = Just ((ty,i1,i2), c2)

  | Par p0 c1 c2 <- unComp c
  = case flm_mit c1 of
      Nothing -> Nothing
      Just (mit,c1') -> Just (mit, cPar (compLoc c) p0 c1' c2)

  | LetHeader fdef@(MkFun (MkFunDefined {}) _ _) cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit, cLetHeader loc fdef cont')

  -- Special case for code emitted by the vectorizer
  | LetFunC fn prms body cont <- unComp c
  , Call fn' _ <- unComp cont
  , fn == fn'
  , let loc = compLoc c
  = case flm_mit body of
      Nothing -> Nothing
      Just (mit,body') -> Just (mit, cLetFunC loc fn prms body' cont)

  -- fallthrough general case
  | LetFunC fn prms body cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetFunC loc fn prms body cont')

  | Let n c1 cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLet loc n c1 cont')

  | LetE n f e cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetE loc n f e cont')

  | LetERef n me cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetERef loc n me cont')

  | otherwise
  = Nothing




elimMitigs :: Comp -> RwM Comp
-- Vectorizer-specific
elimMitigs comp
  = mapCompM return return return return return mitig comp
  where
   mitig c
      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if (i1 `mod` j2 == 0) || (j2 `mod` i1) == 0 then
             do { rewrite $
                  cPar cloc p c1' $
                  cPar cloc (mkParInfo NeverPipeline)
                            (cMitigate cloc ty1 i1 j2) c2'
                }
             else
             if l /= j1 then
                 rewrite $
                 cPar cloc p (cPar cloc pnever c1' (cMitigate cloc ty1 i1 l))
                             (cPar cloc pnever (cMitigate cloc ty2 l j2) c2')
             else return c
           }

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , Mitigate ty2 i2 j2 <- unComp c2
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if i1 `mod` j2 == 0 || j2 `mod` i1 == 0 then
             do { rewrite $
                  cPar cloc p c1' (cMitigate cloc ty1 i1 j2)
                }
             else
             if l /= j1 then
                 rewrite $
                 cPar cloc p (cPar cloc pnever c1' (cMitigate cloc ty1 i1 l))
                             (cMitigate cloc ty2 l j2)
             else return c
           }

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , Mitigate ty1 i1 j1 <- unComp c1
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if i1 `mod` j2 == 0 || j2 `mod` i1 == 0 then
             do { rewrite $
                  cPar cloc p (cMitigate cloc ty1 i1 j2) c2'
                }
             else if l /= j1 then
                 rewrite $
                 cPar cloc p (cMitigate cloc ty1 i1 l)
                                (cPar cloc pnever (cMitigate cloc ty2 l j2) c2')
             else return c
           }

        -- throw away useless mitigators!
      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c1') <- frm_mit c1
      , WriteSnk {} <- unComp c2
      = rewrite $ cPar cloc p c1' c2

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c1') <- frm_mit c1
      , WriteInternal {} <- unComp c2
      = rewrite $ cPar cloc p c1' c2

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c2') <- flm_mit c2
      , ReadSrc {} <- unComp c1
      = rewrite $ cPar cloc p c1 c2'

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c2') <- flm_mit c2
      , ReadInternal {} <- unComp c1
      = rewrite $ cPar cloc p c1 c2'

      -- trivial mitigators
      | MkComp c0 _cloc () <- c
      , Par _p c1 c2 <- c0
      , Mitigate _ty i1 i2 <- unComp c1
      , i1 == i2
      = rewrite c2

      | MkComp c0 _cloc () <- c
      , Par _p c1 c2 <- c0
      , Mitigate _ty i1 i2 <- unComp c2
      , i1 == i2
      = rewrite c1

      | otherwise
      = return c

{-------------------------------------------------------------------------------
  Some view patterns

  TODO: I think the readability of some of the passes could be improved if
  we introduced more views like these.
-------------------------------------------------------------------------------}

data BindView
  = BindView (GName Ty) Comp Comp
  | SeqView Comp Comp
  | NotSeqOrBind Comp

bindSeqView :: Comp -> BindView
bindSeqView = mk_view
  where
    mk_view c@(MkComp (BindMany c1 c2s) cloc ()) =
      case c2s of
        (nm,c2):rest -> BindView nm c1 (MkComp (mkBindMany c2 rest) cloc ())
        []           -> NotSeqOrBind c
    mk_view (MkComp (Seq c1 c2) _cloc ()) = SeqView c1 c2
    mk_view c = NotSeqOrBind c

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Measure how long an action takes
measure :: IO a -> IO (a, Double)
measure act = do
    st <- getCPUTime
    a  <- act
    en <- getCPUTime
    return (a, fromIntegral (en - st) / (10 ^ (12 :: Integer)))
