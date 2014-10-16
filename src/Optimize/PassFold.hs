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
{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module PassFold (runFold, elimMitigsIO) where

import Prelude hiding (exp)
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.Maybe (fromJust, isJust)
import System.CPUTime
import Text.Parsec.Pos (SourcePos)
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as S

import AstComp
import AstExpr
import AstUnlabelled
import CtComp (ctComp)
import CtExpr (ctExp)
import Eval
import Opts
import PpComp ()
import PpExpr ()
import qualified GenSym as GS

{- A Rewritting monad
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

data RewriteRes a = NotRewritten a | Rewritten a

newtype RwM a = RwM { runRwM :: GS.Sym -> IO (RewriteRes a) }

instance Monad RwM where
 (RwM m) >>= f = RwM $ \sym ->
        do { r <- m sym
           ; case r of
               NotRewritten c -> runRwM (f c) sym
               Rewritten c    ->
                 do { r' <- runRwM (f c) sym
                    ; case r' of
                        NotRewritten c2 -> return (Rewritten c2)
                        Rewritten c2    -> return (Rewritten c2)
                    }
           }

 return a = RwM (\_sym -> return (NotRewritten a))

instance Functor RwM where
    fmap f x = x >>= return . f

instance Applicative RwM where
    pure   = return
    (<*>)  = ap

rewrite :: a -> RwM a
rewrite a = RwM (\_sym -> return (Rewritten a))

rwMIO :: IO a -> RwM a
rwMIO m = RwM (\_sym -> m >>= (return . NotRewritten))

instance MonadIO RwM where
  liftIO = rwMIO

genSym :: String -> RwM String
genSym prefix =
  RwM $ \gs ->
    do { str' <- GS.genSymStr gs
       ; return (NotRewritten $ prefix ++ str') }


{- Rewriter statistics
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

newtype RwStats = RwStats { getRwStats :: Map.Map String RwStepStats }

data RwStepStats
       = RwStepStats { rw_invoked  :: Int
                     , rw_rewrote  :: Int
                     , rw_inv_time :: Double
                     , rw_rew_time :: Double }


printRwStats :: RwStats -> IO ()
printRwStats mp
  = mapM_ print_one (Map.toList $ getRwStats mp)
  where
    print_one (pn, RwStepStats{..})
      = printf "%20s:%d/%d/%f, %f\n" pn rw_invoked
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


{- The main rewriter individual steps
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

fold_step :: DynFlags -> Comp -> RwM Comp
-- Just a single step of converting a return to a let
fold_step fgs comp =
  case bindSeqView comp of

    BindView (MkComp (Return _a _b fi e) _ ()) nm c12 ->
     do { -- rwMIO $ putStrLn ("Found BindView" ++ compShortName comp)
        ; fold_step fgs c12 >>= \c12' -> rewrite $ cLetE cloc nm fi e c12'
        }

    BindView (MkComp (LetHeader fun@(MkFun (MkFunDefined {}) _ _) c) _ ()) nm c12 ->
        fold_step fgs c12 >>= \c12' ->
          rewrite $ cLetHeader cloc fun (cBindMany cloc c [(nm, c12')])

    -- Don't forget to go inside!
    BindView c nm  c12 ->
     fold_step fgs c12 >>= \c12' -> return $ cBindMany cloc c [(nm, c12')]

    SeqView (MkComp (Return _a _b fi e) _ ()) c12 ->
      do { -- rwMIO $ putStrLn ("Found BindView" ++ compShortName comp)
         ; let nm = toName ("__fold_unused_" ++ getLnNumInStr cloc)
                           Nothing (ctExp e)
         ; fold_step fgs c12 >>= \c12' -> rewrite $ cLetE cloc nm fi e c12'
         }

    _otherwise ->
       do { -- rwMIO $ putStrLn "fold_step not kicking in for term = "
            -- ; rwMIO $ print (ppCompAst comp)
            return comp
          }
  where
    cloc = compLoc comp


float_letfun_repeat_step :: DynFlags -> Comp -> RwM Comp
-- Rewriting of the form:
--
-- repeat (letfun f(params) = uninitialized-locals ; c in f(args))
-- ~~~>
-- letfun f(params) =
--     uninitialized-locals;
--     repeat c
-- in f(args)
-- This will give the opportunity to take-emit to kick in and rewrite the whole thing to map!
float_letfun_repeat_step _fgs comp
    | Repeat wdth rcomp <- unComp comp
    , LetFunC nm params locals cbody ccont <- unComp rcomp
    , null locals
    , Call nm' args <- unComp ccont
    , nm' == nm
    , all is_simpl_call_arg args  -- Not sure this is necessary
    = rewrite $ cLetFunC cloc nm params locals (cRepeat cloc wdth cbody) ccont
    | otherwise
    = return comp
  where
    cloc = compLoc comp


float_let_par_step :: DynFlags -> Comp -> RwM Comp
float_let_par_step _fgs comp
    | Par p c1 c2 <- unComp comp
    , LetE x fi e1 c1' <- unComp c1
    = rewrite $ cLetE cloc x fi e1 (cPar cloc p c1' c2)
    | Par p c1 c2 <- unComp comp
    , LetE x fi e2 c2' <- unComp c2
    = rewrite $ cLetE cloc x fi e2 (cPar cloc p c1 c2')
    | otherwise
    = return comp
  where
    cloc = compLoc comp



is_simpl_call_arg :: CallArg Exp Comp -> Bool
is_simpl_call_arg (CAExp e) = is_simpl_expr e
is_simpl_call_arg _         = False

from_simpl_call_param :: GName (CallArg Ty CTy) -> Maybe (GName Ty)
from_simpl_call_param nm =
  case nameTyp nm of CAExp  t -> Just nm{nameTyp = t}
                     CAComp _ -> Nothing

push_comp_locals_step :: DynFlags -> Comp -> RwM Comp
-- let comp f(x) = var ... x <- take ; emit e
-- ~~~>
-- let comp f(x) = x <- take; emit (letref locals in e)
push_comp_locals_step _fgs comp
    | LetFunC nm params locals cbody ccont <- unComp comp
    , not (null locals)
    , BindMany tk [(x,emt)] <- unComp cbody
    , Take1 _a' _b' <- unComp tk
    , Emit a e <- unComp emt
    = do { let comp'  = cLetFunC cloc nm params [] cbody' ccont
               cbody' = cBindMany cloc tk [(x,emt')]
               emt'   = cEmit cloc a e'
               e'     = mk_letrefs (expLoc e) locals e
         ; rewrite comp'
         }
    | otherwise
    = return comp
  where
    mk_letrefs :: Maybe SourcePos -> [(GName Ty, Maybe Exp)] -> Exp -> Exp
    mk_letrefs eloc = go
      where
        go [] e = e
        go ((lcl,Nothing):lcls) e
          = eLetRef eloc lcl Nothing (go lcls e)
        go ((lcl,Just einit):lcls) e
          = eLetRef eloc lcl (Just einit) (go lcls e)

    cloc = compLoc comp


take_emit_step :: DynFlags -> Comp -> RwM Comp
-- repeat (x <- take ; emit e) ~~~> let f(x) = e in map(f)
take_emit_step _fgs comp
    | Repeat nfo bm <- unComp comp
    , BindMany tk [(x,emt)] <- unComp bm
    , Take1 _a _b <- unComp tk
    , Emit _a e <- unComp emt
    = do { let xty  = fromJust $ doneTyOfCTyBase (ctComp tk)
               ety  = ctExp e
               eloc = expLoc e
               fty  = TArrow [xty] ety

         ; fname <- do { fr <- genSym "auto_map_"
                       ; return $ toName fr eloc fty }

         ; let letfun  = cLetHeader cloc fun mapcomp
               mapcomp = cMap cloc nfo fname
                       -- NB: We pass the nfo thing,
                       -- to preserve vectorization hints!
               fun = mkFunDefined eloc fname [x] [] e

         ; rewrite letfun
         }

    | otherwise
    = return comp
  where
    cloc = compLoc comp

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


inline_step :: DynFlags -> Comp -> RwM Comp
inline_step fgs comp
   = inline_step_aux fgs comp

inline_step_aux :: DynFlags -> Comp -> RwM Comp
inline_step_aux _fgs comp
    | Let nm c1 c2 <- unComp comp
    = substComp (nm,c1) c2 >>= rewrite

{- Too much inlining!
    | LetE nm e1 c2 <- unComp comp
    , isDynFlagSet fgs AutoLUT
    , Just rgs <- varRanges e1
    , Right True <- shouldLUT [] rgs e1    -- and the expression is luttable
    , Just (_, [], _) <- inOutVars [] Map.empty e1
    = substExpComp (nm,e1) c2 >>= rewrite
-}
    | LetE nm ForceInline e1 c2 <- unComp comp
    = substExpComp (nm,e1) c2 >>= rewrite

    | LetE _nm NoInline _e1 _c2 <- unComp comp
    = return comp

    | LetE nm AutoInline e1 c2 <- unComp comp
    , is_simpl_expr e1
    = substExpComp (nm,e1) c2 >>= rewrite


    | LetHeader f@(MkFun (MkFunDefined {}) _ _) c2 <- unComp comp
    , MkFunDefined nm params locals body <- unFun f -- Defined
    , no_lut_inside body                            -- Not already lutted body

    = do { c2' <- inline_exp_fun_in_comp (nm,params,locals,body) c2
           -- Inlining functions not always completely eliminates
           -- them (e.g. args to map, or non-simple arguments)
         ; if S.member nm (compEFVs c2')
             then return $ cLetHeader cloc f c2'
             else return c2'
         }

    | LetFunC nm _params _locals _c1 c2 <- unComp comp
    , not (S.member nm (compCFVs c2)) -- Completely unused
    = rewrite c2

    | LetFunC nm params locals c1 c2 <- unComp comp
    -- NB: for now, we only inline LetFunC's with empty local environments
    , [] <- locals
    = do { -- liftIO $ putStrLn $ "Inlining comp fun      = " ++ show nm
         ; c2' <- inline_comp_fun (nm,params,c1) c2
           -- liftIO $
           -- putStrLn $
           -- "nm member of rewritten = " ++ show (S.member nm (compFVs c2'))
           -- ; liftIO $ putStrLn $ "LFC-after = " ++ show c2'
         ; if S.member nm (compCFVs c2')
             then return $ cLetFunC cloc nm params locals c1 c2'
             else return $ c2'
         }

    | otherwise
    = return comp
  where
    cloc = compLoc comp

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





inline_exp_fun :: (GName Ty, [GName Ty], [(GName Ty, Maybe Exp)], Exp)
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
               ; e' <- mk_local_lets subst subst_len (locals_from_args ++ locals) body
               ; rewrite e'
               }
         where
            -- arg_subst will return (a) a substitution and (b) some local bindings
            arg_subst :: [(GName Ty, Exp)] -> ([(GName Ty, Exp)], [(GName Ty, Maybe Exp)])
            arg_subst [] = ([],[])
            arg_subst ((prm_nm,arg):rest)
              -- An array of variable size.
              -- Here we must substitute for the size too
              | TArray (NVar siz_nm) _ <- nameTyp prm_nm
              , let (rest_subst, rest_locals) = arg_subst rest
                -- TODO: this size substitution can (should?) go once we disallow
                -- direct term-level references to type-level length variables
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
                          -> Either (GName Ty, Exp) (GName Ty, Maybe Exp)
            -- The choice is: either with a substitution (Left), or 
            --                with a let-binding (Right)
            how_to_inline prm_nm arg
              = if is_simpl_expr arg
                then Left (prm_nm, arg)
                else case isArrayTy_maybe (nameTyp prm_nm) of
                       Just bty 
                         | (bty /= TBit && is_array_ref arg)
                         -> Left (prm_nm, arg)
                       _otherwise -> Right (prm_nm, Just arg)

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
                          -> [(GName Ty, Maybe Exp)]
                          -> Exp
                          -> RwM Exp
            mk_local_lets subst subst_len extended_locals
                = \e -> go extended_locals e >>= substAllTyped subst subst_len
              where
                go [] e = return e
                go ((x,Nothing):xs) e
                  = do { e'  <- go xs e
                       ; ty' <- substAllLengthTy subst_len (nameTyp x)
                       ; let x' = x { nameTyp = ty' }
                       ; return $ eLetRef (expLoc e) x' Nothing e'
                       }
                go ((x,Just xe):xs) e
                  = do { e'  <- go xs e
                       ; xe' <- substAllTyped subst subst_len xe
                       ; ty' <- substAllLengthTy subst_len (nameTyp x)
                       ; let x' = x { nameTyp = ty' }
                       ; return $ eLetRef (expLoc e) x' (Just xe') e'
                       }

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
           ; r <- substAllComp (mk_expr_subst params args) cbody >>= rewrite
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


inline_exp_fun_in_comp :: (GName Ty, [GName Ty], [(GName Ty, Maybe Exp)], Exp)
                       -> Comp -> RwM Comp
inline_exp_fun_in_comp fun
  = mapCompM return return return return (inline_exp_fun fun) return


purify_step :: DynFlags -> Comp -> RwM Comp
-- Returns Just if we managed to rewrite
purify_step _fgs comp =
 do -- rwMIO $ putStrLn "purify_step, comp = "
    -- rwMIO $ print (ppComp comp)
    case isMultiLet_maybe (unComp comp) of
      Just (binds, Return a b fi e) ->
        rewrite $ cReturn cloc a b fi (mkMultiLetExp (reverse binds) e)

      Just (binds, Emit a e) ->
        rewrite $ cEmit cloc a (mkMultiLetExp (reverse binds) e)

      Just (binds, Emits a e) ->
        rewrite $ cEmits cloc a (mkMultiLetExp (reverse binds) e)

      _otherwise ->
        return comp
  where
    cloc = compLoc comp


purify_letref_step :: DynFlags -> Comp -> RwM Comp
-- Returns Just if we managed to rewrite
purify_letref_step _fgs comp =
 do -- rwMIO $ putStrLn "purify_step, comp = "
    -- rwMIO $ print (ppComp comp)
    case isMultiLetRef_maybe (unComp comp) of
      Just (binds, Return a b fi e) ->
        rewrite $ cReturn cloc a b fi (mkMultiLetRefExp (reverse binds) e)

      Just (binds, Emit a e) ->
        rewrite $ cEmit cloc a (mkMultiLetRefExp (reverse binds) e)

      Just (binds, Emits a e) ->
        rewrite $ cEmits cloc a (mkMultiLetRefExp (reverse binds) e)

      _otherwise ->
        return comp
  where
    cloc = compLoc comp


ifdead_step :: DynFlags -> Comp -> RwM Comp
ifdead_step _fgs comp
  = case unComp comp of
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
    -- TODO, convert this to a proper evaluator
    evalBool :: Exp -> Maybe Bool
    evalBool (MkExp (EBinOp Eq (MkExp (EVal _ (VInt i)) _ ())
                               (MkExp (EVal _ (VInt j)) _ ())) _ ())
       = Just (i==j)
    evalBool (MkExp (EVal _ (VBool b)) _ ()) = Just b
    evalBool _ = Nothing

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

    cloc = compLoc comp



letfunc_step :: DynFlags -> Comp -> RwM Comp
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
letfunc_step _fgs comp =
  case unComp comp of
    LetFunC nm params locals (MkComp (Return a b _fi e) _ ()) cont
       | Just params' <- mapM from_simpl_call_param params
       -> do { let fun_ty :: Ty
                   fun_ty = TArrow (map nameTyp params') (ctExp e)

                   f :: GName Ty
                   f = nm { nameTyp = fun_ty }

                   fun :: Fun
                   fun = mkFunDefined cloc f params' locals e

                   replace_call :: Comp -> RwM Comp
                   replace_call (MkComp (Call f' es) xloc ())
                     | uniqId f == uniqId f'
                     = let es'  = map unCAExp es
                           call = eCall xloc f es'
                       in rewrite $ cReturn xloc a b AutoInline call
                   replace_call other = return other

                   purify_calls :: Comp -> RwM Comp
                   purify_calls = mapCompM return return return return return replace_call

             ; cont' <- purify_calls cont
             ; rewrite $ cLetHeader cloc fun cont' -- Same type!
             }
    _ -> return comp
  where
    cloc = compLoc comp

-- > for i in [e,elen] { let f(params) = ... in cont }
-- > ~~~>
-- > let f(i,params) = ... in for i in [e,elen] { cont }
letfun_times_step :: DynFlags -> Comp -> RwM Comp
letfun_times_step _fgs comp =
  case unComp comp of
    Times ui e elen i (MkComp (LetHeader def cont) cloc ())
     | MkFun (MkFunDefined f params locals body) floc () <- def
     -> do { let fty' = TArrow (map nameTyp (i:params)) (fun_ret_ty (nameTyp f))
                 f'   = f { nameTyp = fty' }
                 def' = mkFunDefined floc f' (i:params) locals body
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
times_unroll_step :: DynFlags -> Comp -> RwM Comp
times_unroll_step _fgs comp = case unComp comp of
    Times ui e elen i c
     | EVal _ (VInt n') <- unExp elen
     , let n = fromIntegral n'
     , EVal valTy (VInt 0) <- unExp e
     , (n < 3 && n > 0 && ui == AutoUnroll) || ui == Unroll
-- BOZIDAR: this will currently fail perf test for TX/test_encoding_34
--         , ui == Unroll -- || (n < 3 && n > 0 && ui == AutoUnroll)
     -> let idxs = [0..n-1]
            comps = replicate n c
            unrolled = zipWithM (\curr xc ->
              substExpComp (i, eVal (expLoc e) valTy (vint curr)) xc) idxs comps
        in case unrolled of
             Nothing ->
               return comp
             Just [] -> do
               -- The loop doesn't get executed at all
               -- (TODO: This case is currently excluded by the @n > 0@ condition)
               -- In this case we just return unit, but we have to find the
               -- right input and output stream types, which we set to be
               -- equal the the input and output stream types of the original
               -- computation.
               let compTy = ctComp comp
                   a      =  inTyOfCTyBase compTy
                   b      = yldTyOfCTyBase compTy
               return $ cReturn cloc a b ForceInline (eVal cloc TUnit VUnit)
             Just xs ->
               rewrite $ mk_bind_many xs
    _ -> return comp
  where
    mk_bind_many :: [Comp] -> Comp
    mk_bind_many []     = error "times_unroll_step: can't happen!"
    mk_bind_many [x]    = x
    mk_bind_many (x:xs) = cBindMany (compLoc x) x [(unused, mk_bind_many xs)]

    unused :: GName Ty
    unused = toName ("__unroll_unused_" ++ getLnNumInStr cloc) Nothing TUnit

    cloc = compLoc comp


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

    go [] (Return _ _ _ _) = Nothing
    go [] (Emit _ _)       = Nothing
    go [] (Emits _ _)      = Nothing

    -- We must have some accumulated bindings!
    go acc (Return a b fi e) = Just (acc, Return a b fi e)
    go acc (Emit a e)        = Just (acc, Emit a e)
    go acc (Emits a e)       = Just (acc, Emits a e)
    go _   _                 = Nothing

isMultiLetRef_maybe :: Comp0 -> Maybe ([(GName Ty, Maybe Exp)], Comp0)
isMultiLetRef_maybe = go []
  where
    go acc (LetERef x e c) = go ((x,e):acc) (unComp c)

    go [] (Return _ _ _ _) = Nothing
    go [] (Emit _ _)       = Nothing
    go [] (Emits _ _)      = Nothing

    -- We must have some accumulated bindings!
    go acc (Return a b fi e) = Just (acc, Return a b fi e)
    go acc (Emit a e)        = Just (acc, Emit a e)
    go acc (Emits a e)       = Just (acc, Emits a e)
    go _   _                 = Nothing

-- > for cnt in [estart, ebound] { return ebody }
-- > ~~~>
-- > return (for cnt in [estart, ebound] ebody)
elim_times_step :: DynFlags -> Comp -> RwM Comp
elim_times_step _fgs comp =
  case unComp comp of
    Times ui estart ebound cnt (MkComp (Return a b _ ebody) _cloc ()) ->
      rewrite $ cReturn cloc a b AutoInline
                  (eFor cloc ui cnt estart ebound ebody)

    _ -> return comp
  where
    cloc = compLoc comp



arrinit_step :: DynFlags -> TypedExpPass
-- Statically initialize as many arrays as possible
arrinit_step _fgs e1 = case evalArrInt e1 of
    Nothing   -> return e1
    Just vals -> rewrite $ eValArr (expLoc e1) (ctExp e1) (map VInt vals)

exp_inlining_steps :: DynFlags -> TypedExpPass
exp_inlining_steps fgs e
   -- Forced Inline!
 | ELet nm ForceInline e1 e2 <- unExp e
 = substExp (nm,e1) e2 >>= rewrite

 | ELet _nm NoInline _e1 _e2 <- unExp e
 = return e

 | ELet nm AutoInline e1 e2 <- unExp e
 , let fvs = exprFVs e2
 , let b = nm `S.member` fvs
 = if not b then
      if not (mutates_state e) then rewrite e2
      else return e
   else if is_simpl_expr e1 && not (isDynFlagSet fgs NoExpFold)
   then substExp (nm,e1) e2 >>= rewrite
   else return e
 | otherwise
 = return e

exp_let_push_step :: DynFlags -> TypedExpPass
exp_let_push_step _fgs e
 | ELet nm fi e1 e2 <- unExp e
 , EArrWrite e0 estart0 elen0 erhs <- unExp e2
 , EArrRead evals estart rlen <- unExp erhs
 , let fvs = foldr (S.union . exprFVs) S.empty [e0, estart0, evals]
 , not (nm `S.member` fvs)
 = let estart' = eLet (expLoc estart) nm fi e1 estart
   in rewrite $
      eArrWrite (expLoc e2) e0 estart0 elen0 $
      eArrRead (expLoc erhs) evals estart' rlen
 | otherwise
 = return e



asgn_letref_step :: DynFlags -> TypedExpPass
asgn_letref_step _fgs exp
  | EArrWrite e0 estart elen erhs <- unExp exp
  , TArray _ ty <- ctExp e0
  , not (ty == TBit)
   -- It has to be LILength so we can just take a pointer
  , LILength _ <- elen
  , EVar _ <- unExp e0
   -- Just a simple expression with no side-effects
  , not (mutates_state estart)
  , Just (y, residual_erhs) <- returns_letref_var erhs
  = substExp (y, eArrRead (expLoc exp) e0 estart elen) residual_erhs >>= rewrite
  | otherwise = return exp
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

rest_chain :: DynFlags -> TypedExpPass
rest_chain fgs e
     = alength_elim fgs e >>=
       eval_arith fgs >>=
       const_fold fgs >>=
       arrinit_step fgs >>=
       subarr_inline_step fgs
       -- This leads to too much inlining and seems to have
       -- unstable effects to performance so I am keeping it
       -- commented for now:
          >>= proj_inline_step fgs


alength_elim :: DynFlags -> TypedExpPass
alength_elim _fgs e
 | EUnOp ALength e0 <- unExp e
 , (TArray nexp _)  <- ctExp e0
 , let loc = expLoc e
 = rewrite $ numexp_to_exp loc nexp
 | otherwise
 = return e
 where numexp_to_exp loc (Literal i) = eVal loc tint (vint i)
       numexp_to_exp loc (NVar nm)   = eVar loc (toName nm Nothing tint)


eval_arith :: DynFlags -> TypedExpPass
eval_arith _fgs e
  | arith_ty (ctExp e)      -- of arithmetic type
  , not (isEVal e)          -- not already a value
  , Just v <- evalArith e   -- evaluate it!
  = rewrite $ eVal (expLoc e) (ctExp e) v
  | otherwise
  = return e
  where
    arith_ty :: Ty -> Bool
    arith_ty (TInt _) = True
    arith_ty TDouble  = True
    arith_ty _        = False

subarr_inline_step :: DynFlags -> TypedExpPass
subarr_inline_step _fgs e
  | EArrRead evals estart LISingleton <- unExp e
  , EValArr _ vals <- unExp evals
  , EVal _ (VInt n') <- unExp estart
  , let n = fromIntegral n'
  = rewrite $ eVal (expLoc e) (ctExp e) (vals!!n)

  | EArrRead evals estart (LILength n) <- unExp e
  , EValArr _ vals <- unExp evals
  , EVal _ (VInt s') <- unExp estart
  , let s = fromIntegral s'
  = rewrite $ eValArr (expLoc e) (ctExp e) (take n $ drop s vals)

    -- x[0,length(x)] == x
  | EArrRead evals estart (LILength n) <- unExp e
  , EVal _ (VInt 0) <- unExp estart
  , TArray (Literal m) _ <- ctExp evals
  , n == m
  = rewrite evals

  | EArrWrite eval_tgt estart (LILength n) erhs <- unExp e
  , EVal _ (VInt 0) <- unExp estart
  , TArray (Literal m) _ <- ctExp eval_tgt
  , n == m
  = rewrite $ eAssign (expLoc e) eval_tgt erhs

  | otherwise
  = return e

proj_inline_step :: DynFlags -> TypedExpPass
proj_inline_step _fgs e
 | EProj e' fn <- unExp e
 , EStruct _s fs_es <- unExp e'
 , all (is_simpl_expr . snd) fs_es -- no side effects
 , Just ep <- lookup fn fs_es
 = rewrite ep
 | otherwise
 = return e


for_unroll_step :: DynFlags -> TypedExpPass
for_unroll_step _fgs e
  | EFor ui nm estart elen ebody  <- unExp e
  , EVal _ (VInt 0) <- unExp estart
  , EVal _ (VInt n') <- unExp elen
  , let n = fromIntegral n'
  , (n < 8 && n > 0 && ui == AutoUnroll) || ui == Unroll
  = -- liftIO (putStrLn "for_unroll_step, trying ...") >>
    let idxs = [0..n-1]
        exps = replicate n ebody
        unrolled = zipWith (\curr xe -> runIdentity $
                                        substExp (nm, eVal (expLoc e) tint (vint curr)) xe) idxs exps
    in case unrolled of
         [] -> return $ eVal (expLoc e) TUnit VUnit
         xs -> rewrite $ mk_eseq_many xs

  where
    mk_eseq_many :: [Exp] -> Exp
    mk_eseq_many []     = error "for_unroll_step: can't happen!"
    mk_eseq_many [x]    = x
    mk_eseq_many (x:xs) = eSeq (expLoc e) x (mk_eseq_many xs)

for_unroll_step _fgs e
  = return e




const_fold :: DynFlags -> TypedExpPass
const_fold _ e@(MkExp e0 loc ())
  = case go e0 of
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

type TypedCompPass = Comp -> RwM Comp
type TypedExpPass  = Exp  -> RwM Exp


-- The Boolean value returned indicates that some rewriting happened (if True)
runPasses :: GS.Sym
          -> RwStats
          -> [(String,TypedCompPass)]
          -> Comp
          -> IO (Bool, Comp, RwStats)
runPasses sym = go False
  where
    go b mp [] comp
      = return (b, comp, mp)
    go b mp ((pn,p):ps) comp
        = do { st <- getCPUTime
--             ; printf "Pass: %10s" pn
             ; r <- runRwM (p comp) sym
             ; en <- getCPUTime
             ; let diff = fromIntegral (en - st) / (10 ^ (12 :: Integer))
             ; let mp' = incInvokes mp diff pn
             ; case r of
                 NotRewritten _c
                   -> do { -- printf "... not rewritten :-( \n"
                           go b mp' ps comp
                         }
                 Rewritten comp'
                   -> do { -- printf "... rewritten     :-) \n"
                           -- ; printf "comp = %s\n" (show comp)
                           go True (incRewrites mp' diff pn) ((pn,p):ps) comp'
                         }

             }


foldCompPasses :: DynFlags -> [(String,TypedCompPass)]
foldCompPasses flags
  | isDynFlagSet flags NoFold
  = []
  | otherwise
  = -- Standard good things
    [ ("fold"         , fold_step  flags         )
    , ("purify"       , purify_step  flags       )
    , ("purify-letref", purify_letref_step  flags)

    , ("elim-times"  , elim_times_step flags  )
    , ("letfunc"     , letfunc_step flags     )
    , ("letfun-times", letfun_times_step flags)

    , ("times-unroll", times_unroll_step flags)

    , ("inline", inline_step flags)

    -- More aggressive optimizations
    , ("push-comp-locals"   , push_comp_locals_step flags   )

    , ("take-emit"          , take_emit_step flags          )

    , ("float-letfun-repeat", float_letfun_repeat_step flags)

    , ("float-let-par-step", float_let_par_step flags)


    , ("ifdead"             , ifdead_step flags             )

    -- Don't use: not wrong but does not play nicely with LUT
    --  , ("float-top-letref"   , float_top_letref_step flags )

    ]

foldExpPasses :: DynFlags -> [(String,TypedExpPass)]
foldExpPasses flags
  | isDynFlagSet flags NoFold || isDynFlagSet flags NoExpFold
  = []
  | otherwise
  = [ ("for-unroll", for_unroll_step flags)
    , ("exp-inlining-steps", exp_inlining_steps flags)
    , ("asgn-letref-step", asgn_letref_step flags)
    , ("exp_let_push_step", exp_let_push_step flags)
    , ("rest-chain", rest_chain flags)
    ]

runFold :: DynFlags -> GS.Sym -> Comp -> IO Comp
runFold flags sym = \comp ->
     do { (comp',mp') <- go (RwStats Map.empty) 0 comp

        ; when (isDynFlagSet flags Verbose) $
          do { putStrLn "Optimizer statistics:"
             ; printRwStats mp' }

        ; return comp'
        }
 where
  comp_passes
    = map (\(nm, step) -> (nm, mapCompM return return return return return step))
          (foldCompPasses flags)
  exp_passes
    = map (\(nm,step) -> (nm, mapCompM return return return return (mapExpM return return step) return))
          (foldExpPasses flags)
  passes = comp_passes ++ exp_passes

  go :: RwStats -> Integer -> Comp -> IO (Comp, RwStats)
  go mp depth comp
    | depth >= 10  = return (comp,mp)
    | otherwise
    = do { (rw_happened,comp1,mp1) <- runPasses sym mp passes comp
         ; if rw_happened then go mp1 (depth+1) comp1
                          else return (comp1,mp1)
         }


{- Elimination of mitigators
   ~~~~~~~~~~~~~~~~~~~~~~~~~ -}

elimMitigsIO :: GS.Sym -> Comp -> IO Comp
elimMitigsIO sym = go
  where
    go comp = do { r <- runRwM (elimMitigs comp) sym
                 ; case r of NotRewritten {} -> return comp
                             Rewritten comp' -> go comp'
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
        Just (mit,c2') -> Just (mit, cPar cloc p0 c1 c2')

    | LetHeader fdef@(MkFun (MkFunDefined {}) _ ()) cont <- unComp c -- Needed because of AutoMaps! Yikes!
    = case frm_mit cont of
        Nothing -> Nothing
        Just (mit,cont') -> Just (mit, cLetHeader cloc fdef cont')

    -- Special case for code emitted by the vectorizer
    | LetFunC fn prms locs body cont <- unComp c
    , Call fn' _args <- unComp cont
    , fn == fn'
    = case frm_mit body of
        Nothing -> Nothing
        Just (mit,body') -> Just (mit, cLetFunC cloc fn prms locs body' cont)

    -- fallthrough general case
    | LetFunC fn prms locs body cont <- unComp c
    = case frm_mit cont of
        Nothing -> Nothing
        Just (mit,cont') -> Just (mit, cLetFunC cloc fn prms locs body cont')

    | Let n c1 cont <- unComp c
    = case frm_mit cont of
        Nothing -> Nothing
        Just (mit,cont') -> Just (mit, cLet cloc n c1 cont')

    | otherwise
    = Nothing
  where
    cloc = compLoc c

flm_mit :: Comp -> Maybe ((Ty,Int,Int), Comp)
-- returns (mitigator,residual-comp)
flm_mit c
    | Par _p0 c1 c2 <- unComp c
    , Mitigate ty i1 i2  <- unComp c1
    = Just ((ty,i1,i2), c2)

    | Par p0 c1 c2 <- unComp c
    = case flm_mit c1 of
        Nothing -> Nothing
        Just (mit,c1') -> Just (mit, cPar cloc p0 c1' c2)

    | LetHeader fdef@(MkFun (MkFunDefined {}) _ ()) cont <- unComp c
    = case flm_mit cont of
        Nothing -> Nothing
        Just (mit,cont') -> Just (mit,cLetHeader cloc fdef cont')

    -- Special case for code emitted by the vectorizer
    | LetFunC fn prms locs body cont <- unComp c
    , Call fn' _args <- unComp cont
    , fn == fn'
    = case flm_mit body of
        Nothing -> Nothing
        Just (mit,body') -> Just (mit, cLetFunC cloc fn prms locs body' cont)

    -- fallthrough general case
    | LetFunC fn prms locs body cont <- unComp c
    = case flm_mit cont of
        Nothing -> Nothing
        Just (mit,cont') -> Just (mit, cLetFunC cloc fn prms locs body cont')


    | Let n c1 cont <- unComp c
    = case flm_mit cont of
        Nothing -> Nothing
        Just (mit,cont') -> Just (mit, cLet cloc n c1 cont')

    | otherwise
    = Nothing
  where
    cloc = compLoc c



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
  = BindView Comp (GName Ty) Comp
  | SeqView Comp Comp
  | NotSeqOrBind Comp

bindSeqView :: Comp -> BindView
bindSeqView = mk_view
  where
    mk_view c@(MkComp (BindMany c1 c2s) cloc ()) =
      case c2s of
        (nm,c2):rest -> BindView c1 nm (MkComp (mkBindMany c2 rest) cloc ())
        []           -> NotSeqOrBind c
    mk_view (MkComp (Seq c1 c2) _cloc ()) = SeqView c1 c2
    mk_view c = NotSeqOrBind c
