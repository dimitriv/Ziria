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
{-# LANGUAGE ScopedTypeVariables #-}

module PassFold where

import AstExpr
import AstComp

import Text.PrettyPrint.HughesPJ

import PpExpr
import PpComp
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map


import Data.Time
import System.CPUTime

import Text.Printf
import Text.Parsec.Pos (SourcePos)

import CgLUT ( shouldLUT )
import Analysis.Range ( varRanges )
import Analysis.UseDef ( inOutVars )

import Opts

import qualified GenSym as GS

import TcExpr ( tyOfParams )

import Data.Functor.Identity

import Eval

import Data.Maybe ( fromJust, isJust, isNothing )

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

rewrite a = RwM (\_sym -> return (Rewritten a))

rwMIO :: IO a -> RwM a
rwMIO m = RwM (\_sym -> m >>= (return . NotRewritten))

instance MonadIO RwM where
  liftIO = rwMIO

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
    print_one (pn,(RwStepStats invs rws tm1 tm2))
      = printf "%20s:%d/%d/%f, %f\n" pn invs rws tm1 tm2


incInvokes :: RwStats -> Double -> String -> RwStats
incInvokes mp d s = RwStats (Map.alter aux s $ getRwStats mp)
  where aux Nothing                       = Just (RwStepStats 1 0 d 0)
        aux (Just (RwStepStats i r d0 t)) = Just (RwStepStats (i+1) r (d0+d) t)

incRewrites :: RwStats -> Double -> String -> RwStats
incRewrites mp d s = RwStats (Map.alter aux s $ getRwStats mp)
  where aux Nothing                      = Just (RwStepStats 1 1 0 d)
        aux (Just (RwStepStats i r t f)) = Just (RwStepStats i (r+1) d (f+d))


{- The main rewriter individual steps
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

fold_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- Just a single step of converting a return to a let
fold_step fgs comp =
  case bindSeqView comp of

    BindView (MkComp (Return fi e) rloc rinfo) nm c12 ->
     do { -- rwMIO $ putStrLn ("Found BindView" ++ compShortName comp)
        ; fold_step fgs c12 >>= \c12' ->
                rewrite $ MkComp (LetE nm fi e c12') cloc cinfo
        }

    -- CL
    BindView (MkComp (LetHeader f fun@(MkFun (MkFunDefined {}) _ _) c) floc finfo) nm c12 ->
        fold_step fgs c12 >>= \c12' ->
          rewrite $
          MkComp (LetHeader f fun (MkComp (mkBindMany c [(nm,c12')]) cloc cinfo)) cloc cinfo
    --
    -- Don't forget to go inside!
    BindView c nm  c12 ->
     fold_step fgs c12 >>= \c12'
         -> return (MkComp (mkBindMany c [(nm,c12')]) cloc cinfo)

    SeqView (MkComp (Return fi e) rloc rinfo) c12 ->
      do { -- rwMIO $ putStrLn ("Found BindView" ++ compShortName comp)
         ; let nm = toName ("__fold_unused_" ++ getLnNumInStr cloc)
                           Nothing Nothing
         ; fold_step fgs c12 >>= \c12' ->
               rewrite $ MkComp (LetE nm fi e c12) cloc cinfo }
    _otherwise ->
       do { -- rwMIO $ putStrLn "fold_step not kicking in for term = "
            -- ; rwMIO $ print (ppCompAst comp)
            return comp
          }

  where cloc   = compLoc comp
        cinfo  = compInfo comp



float_letfun_repeat_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- Rewriting of the form:
--
-- repeat (letfun f(params) = uninitialized-locals ; c in f(args))
-- ~~~>
-- letfun f(params) =
--     uninitialized-locals;
--     repeat c
-- in f(args)
-- This will give the opportunity to take-emit to kick in and rewrite the whole thing to map!
float_letfun_repeat_step fgs comp
 | Repeat wdth rcomp <- unComp comp
 , LetFunC nm params locals cbody ccont <- unComp rcomp
 , null locals
 , Call nm' args <- unComp ccont
 , nm' == nm
 , all is_simpl_call_arg args  -- Not sure this is necessary
 = rewrite $
   MkComp (LetFunC nm params locals (MkComp (Repeat wdth cbody)
                                            (compLoc comp)
                                            (compInfo comp)) ccont)
          (compLoc comp) (compInfo comp)
 | otherwise
 = return comp


float_let_par_step fgs comp
  | Par p c1 c2 <- unComp comp
  , LetE x fi e1 c1' <- unComp c1
  = rewrite $ cLetE loc cty x fi e1 (cPar loc cty p c1' c2)
  | Par p c1 c2 <- unComp comp
  , LetE x fi e2 c2' <- unComp c2
  = rewrite $ cLetE loc cty x fi e2 (cPar loc cty p c1 c2')
  | otherwise
  = return comp
  where loc = compLoc comp
        cty = compInfo comp




is_simpl_call_arg (CAExp e) = is_simpl_expr e
is_simpl_call_arg _         = False

is_simpl_call_param (CAExp _) = True
is_simpl_call_param _         = False



push_comp_locals_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- let comp f(x) = var ... x <- take ; emit e
-- ~~~>
-- let comp f(x) = x <- take; emit (letref locals in e)
push_comp_locals_step fgs comp
  | LetFunC nm params locals cbody ccont <- unComp comp
  , not (null locals)
  , BindMany tk [(x,emt)] <- unComp cbody
  , Take1 <- unComp tk
  , Emit e <- unComp emt
  , let loc = compLoc comp
  , let cty = compInfo comp
  = do { let comp'  = cLetFunC loc cty nm params [] cbody' ccont
             cbody' = cBindMany loc (compInfo cbody) tk [(x,emt')]
             emt'   = cEmit loc (compInfo emt) e'
             e'     = mk_letrefs (expLoc e) (info e) locals e
       ; rewrite comp'
       }
  | otherwise
  = return comp
  where
    mk_letrefs :: Maybe SourcePos -> a -> [(Name, Ty, Maybe (Exp a))] -> Exp a -> Exp a
    mk_letrefs eloc ety lcls e = go lcls e
      where
        go [] e = e
        go ((lcl,ty,Nothing):lcls) e
          = eLetRef eloc ety lcl ty Nothing (go lcls e)
        go ((lcl,ty,Just einit):lcls) e
          = eLetRef eloc ety lcl ty (Just einit) (go lcls e)


take_emit_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- repeat (x <- take ; emit e) ~~~> let f(x) = e in map(f)
take_emit_step fgs comp
  | Repeat nfo bm <- unComp comp
  , BindMany tk [(x,emt)] <- unComp bm
  , Take1 <- unComp tk
  , Emit e <- unComp emt
  = do { let xty = fromJust $ doneTyOfCTyBase (compInfo tk)
             ety = info e
             eloc = expLoc e
             fty = TArrow [xty] ety
             cloc = compLoc comp
             cty  = compInfo comp

       ; fname <- do { fr <- genSym "auto_map_"
                     ; return $ toName fr eloc (Just fty) }

       ; let letfun  = MkComp (LetHeader fname fun mapcomp) cloc cty
             mapcomp = MkComp (Map nfo fname) cloc cty
                     -- NB: We pass the nfo thing,
                     -- to preserve vectorization hints!
             fun = MkFun (MkFunDefined fname [(x,xty)] [] e) eloc fty

       ; rewrite letfun
       }

  | otherwise
  = return comp

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


inline_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
inline_step fgs comp
   = inline_step_aux fgs comp

inline_step_aux fgs comp
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

  | LetE nm NoInline e1 c2 <- unComp comp
  = return comp

  | LetE nm AutoInline e1 c2 <- unComp comp
  , is_simpl_expr e1
  = substExpComp (nm,e1) c2 >>= rewrite


  | LetHeader nm f@(MkFun (MkFunDefined {}) _ _) c2 <- unComp comp
  , MkFunDefined nm params locals body <- unFun f -- Defined
  , no_lut_inside body                            -- Not already lutted body

  = do { c2' <- inline_exp_fun_in_comp (nm,params,locals,body) c2
         -- Inlining functions not always completely eliminates
         -- them (e.g. args to map, or non-simple arguments)
       ; if S.member nm (compFVs c2') then
           return $ MkComp (LetHeader nm f c2')
                           (compLoc comp)
                           (compInfo comp)
         else return c2'
       }

  | LetFunC nm params locals c1 c2 <- unComp comp
  , not (S.member nm (compFVs c2)) -- Completely unused
  = return c2

  | LetFunC nm params locals c1 c2 <- unComp comp
  -- NB: for now, we only inline LetFunC's with empty local environments
  , [] <- locals
  = do { -- liftIO $ putStrLn $ "Inlining comp fun      = " ++ show nm
       ; c2' <- inline_comp_fun (nm,params,c1) c2
         -- liftIO $
         -- putStrLn $
         -- "nm member of rewritten = " ++ show (S.member nm (compFVs c2'))
         -- ; liftIO $ putStrLn $ "LFC-after = " ++ show c2'
       ; return $
         if S.member nm (compFVs c2')
         then MkComp (LetFunC nm params locals c1 c2')
                     (compLoc comp)
                     (compInfo comp)
         else c2' }

  | otherwise
  = return comp

is_arg_to_map nm comp
  = isNothing (mapCompM_ return aux comp)
  where aux c@(MkComp (Map _ nm') _ _)
          = if nm == nm' then Nothing else Just c
        aux other = Just other

-- Just a heuristic for inlining: what are 'simple' expressions that
-- are safe and likely beneficial to just inline before code generation.
is_simpl_expr = is_simpl_expr0 . unExp
is_simpl_expr0 :: Exp0 Ty -> Bool
is_simpl_expr0 (EVal _)    = True
is_simpl_expr0 (EValArr _) = True
is_simpl_expr0 (EVar _)    = True
is_simpl_expr0 (EUnOp u e)          = is_simpl_expr e
is_simpl_expr0 (EStruct _ fses) = all is_simpl_expr (map snd fses)


is_simpl_expr0 _ = False

no_lut_inside x
  = isJust (mapExpM_ elut_nothing x)
  where elut_nothing :: Exp Ty -> Maybe (Exp Ty)
        elut_nothing (MkExp (ELUT {}) _ _) = Nothing
        elut_nothing other                 = Just other



-- if e then return e1 else return e2 ~~> return (if e then e1 else e2)
if_return_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
if_return_step dynflags comp
  | Branch eguard c1 c2 <- unComp comp
  , Return f1 e1 <- unComp c1
  , Return f2 e2 <- unComp c2
  -- , f1 == f2
  , let cty  = compInfo comp
  , let cloc = compLoc comp
  , Just dty <- doneTyOfCTyBase (compInfo comp)
  = rewrite $ cReturn (compLoc comp) (compInfo comp) f1 (eIf (compLoc comp) dty eguard e1 e2)
  | otherwise
  = return comp



inline_exp_fun :: (Name,[(Name,Ty)],[(Name,Ty,Maybe (Exp Ty))],Exp Ty)
               -> Exp Ty -> RwM (Exp Ty)
-- True means that it is safe to just get rid of the function
inline_exp_fun (nm,params,locals,body) e
  = mapExpM_ replace_call e
  where replace_call e@(MkExp (ECall (MkExp (EVar nm') _ _) args) _ _)
--          | all is_simpl_expr args -- Like what we do for LetE/ELet
          | nm == nm'
          = do { let xs                        = zip params args
                     (subst, locals_from_args) = arg_subst xs
                     subst_len                 = len_subst xs
               ; e' <- mk_local_lets subst subst_len (locals_from_args ++ locals) body
               ; rewrite e'
               }
         where
            loc = expLoc e
            -- arg_subst will return (a) a substitution and (b) some local bindings
            arg_subst [] = ([],[])
            arg_subst (((prm_nm,prm_ty),arg):rest)
              -- An array of variable size.
              -- Here we must substitute for the size too
              | TArr (NVar siz_nm _m) _ <- prm_ty
              , let (rest_subst, rest_locals) = arg_subst rest
              , let siz_bnd = (siz_nm, arg_size (info arg))
              = case how_to_inline prm_nm prm_ty arg of
                  Left bnd  -> (bnd:siz_bnd:rest_subst, rest_locals)
                  Right bnd -> (siz_bnd:rest_subst, bnd:rest_locals)

              | otherwise
              , let (rest_subst, rest_locals) = arg_subst rest
              = case how_to_inline prm_nm prm_ty arg of
                  Left bnd  -> (bnd:rest_subst, rest_locals)
                  Right bnd -> (rest_subst, bnd:rest_locals)

            -- Delicate: classifies which arrays are passed by
            -- reference. Should revisit.  But at the moment this is
            -- consistent with the semantics implemented in CgExpr.
            -- See for example codeGenArrRead in CgExpr.hs

            how_to_inline :: Name -> Ty -> Exp Ty -> Either (Name,Exp Ty) (Name, Ty, Maybe (Exp Ty))
            -- The choice is: either with a substitution (Left) or with a let-binding (Right)
            how_to_inline prm_nm prm_ty arg
              = if is_simpl_expr arg
                then Left (prm_nm, arg)
                else if isArrTy prm_ty && getArrTy prm_ty /= TBit && is_array_ref arg
                     then Left (prm_nm,arg)
                else Right (prm_nm, prm_ty, Just arg)

            is_array_ref (MkExp (EVar _) _ _)      = True
            is_array_ref (MkExp (EArrRead {}) _ _) = True
            is_array_ref _ = False


            len_subst [] = []
            len_subst (((prm_nm,prm_ty),arg):rest)
              | TArr (NVar siz_nm _m) _ <- prm_ty
              = (siz_nm, arg_numexpr (info arg)):(len_subst rest)
              | otherwise
              = len_subst rest

            arg_numexpr (TArr ne _t) = ne
            arg_numexpr _
              = error $
                "Could not deduce param size during inlining of function:" ++
                    show nm ++ ", at call location:" ++ show loc

            arg_size (TArr (NVar siz_nm _m) _) = eVar loc tint siz_nm
            arg_size (TArr (Literal siz) _)    = eVal loc tint (vint siz)
            arg_size _
              = error $
                "Could not deduce param size during inlining of function:" ++
                    show nm ++ ", at call location:" ++ show loc

            mk_local_lets subst len_subst locals e
              = go locals e >>= substAllTyped subst len_subst
              where go [] e = return e
                    go ((x,ty,Nothing):xs) e
                      = do { e' <- go xs e
                           ; ty' <- substAllLengthTy len_subst ty
                           ; let x' = x { mbtype = Just ty }
                           ; return $
                             eLetRef (expLoc e) (info e) x' ty' Nothing e'
                           }
                    go ((x,ty,Just xe):xs) e
                      = do { e' <- go xs e
                           ; xe' <- substAllTyped subst len_subst xe
                           ; ty' <- substAllLengthTy len_subst ty
                           ; let x' = x { mbtype = Just ty' }
                           ; return $
                             eLetRef (expLoc e) (info e) x' ty' (Just xe') e'
                           }

        replace_call other = return other


inline_comp_fun :: (Name,[(Name,CallArg Ty CTy0)], Comp CTy Ty) -> Comp CTy Ty -> RwM (Comp CTy Ty)
inline_comp_fun (nm,params,cbody) c
  = do { -- liftIO $ putStrLn $ "inline_comp_fun (before) = " ++ show c
       ; r <- mapCompM_ return replace_call c
       ; -- liftIO $ putStrLn $ "inline_comp_fun (after) = " ++ show r
       ; return r
       }
  where replace_call c@(MkComp (Call nm' args) _ _)
          | all is_simpl_call_arg args
          , nm == nm'
          = do { -- liftIO $ putStrLn "Matching!"
               ; r <- substAllComp (mk_expr_subst params args) cbody >>= rewrite
               ; -- liftIO $ putStrLn $ "Substituted =" ++ (show r)
               ; return r
               }

        replace_call other = return other

        mk_expr_subst [] [] = []
        mk_expr_subst ((nm, CAExp _ty):ps1) ((CAExp e1):as1)
          = (nm,e1): mk_expr_subst ps1 as1
        mk_expr_subst _ _ = error "BUG: inline_comp_fun!"


inline_exp_fun_in_comp :: (Name,[(Name,Ty)],
                                [(Name,Ty,Maybe (Exp Ty))],Exp Ty)
                       -> Comp CTy Ty -> RwM (Comp CTy Ty)
inline_exp_fun_in_comp finfo comp
  = mapCompM_ (inline_exp_fun finfo) return comp


purify_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- Returns Just if we managed to rewrite
purify_step fgs comp =
 do -- rwMIO $ putStrLn "purify_step, comp = "
    -- rwMIO $ print (ppComp comp)
    case isMultiLet_maybe (unComp comp) of
      Just (binds, Return fi e) ->
          rewrite $ MkComp (Return fi (mkMultiLetExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      Just (binds, Emit e) ->
          rewrite $ MkComp (Emit (mkMultiLetExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      Just (binds, Emits e) ->
          rewrite $ MkComp (Emits (mkMultiLetExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      _otherwise -> return comp


purify_letref_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- Returns Just if we managed to rewrite
purify_letref_step fgs comp =
 do -- rwMIO $ putStrLn "purify_step, comp = "
    -- rwMIO $ print (ppComp comp)
    case isMultiLetRef_maybe (unComp comp) of
      Just (binds, Return fi e) ->
          rewrite $ MkComp (Return fi (mkMultiLetRefExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      Just (binds, Emit e) ->
          rewrite $ MkComp (Emit (mkMultiLetRefExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      Just (binds, Emits e) ->
          rewrite $ MkComp (Emits (mkMultiLetRefExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      _otherwise -> return comp


ifdead_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
ifdead_step fgs comp
  = case unComp comp of
      Branch e c1 c2
        | Just b <- evalBool e
        -> rewrite $ if b then c1 else c2

      Branch e (MkComp (Branch e' c1 c2) _ _) c3
        | e `impliesBool` e'
        -> rewrite $ MkComp (Branch e c1 c3) (compLoc comp) (compInfo comp)
        | e `impliesBoolNeg` e'
        -> rewrite $ MkComp (Branch e c2 c3) (compLoc comp) (compInfo comp)

      Branch e c1 (MkComp (Branch e' c2 c3) _ _)
        | eneg e `impliesBool` e'
        -> rewrite $ MkComp (Branch e c1 c2) (compLoc comp) (compInfo comp)
        | eneg e `impliesBoolNeg` e'
        -> rewrite $ MkComp (Branch e c1 c3) (compLoc comp) (compInfo comp)

      _otherwise -> return comp

        -- TODO, convert this to a proper evaluator
  where evalBool (MkExp (EBinOp Eq (MkExp (EVal (VInt i)) _ _)
                                   (MkExp (EVal (VInt j)) _ _)) _ _)
           = Just (i==j)
        evalBool (MkExp (EVal (VBool b)) _ _) = Just b
        evalBool _ = Nothing

        eneg e = MkExp (EUnOp Neg e) (expLoc e) (info e)

        impliesBool (MkExp (EBinOp Eq e
                                      (MkExp (EVal (VInt j)) _ _)) _ _)
                    (MkExp (EBinOp Eq e'
                                      (MkExp (EVal (VInt j')) _ _)) _ _)
           | e `expEq` e'
           = j == j'
        impliesBool _ _ = False

        impliesBoolNeg (MkExp (EBinOp Eq e
                                         (MkExp (EVal (VInt j)) _ _)) _ _)
                       (MkExp (EBinOp Eq e'
                                         (MkExp (EVal (VInt j')) _ _)) _ _)
           | e `expEq` e'
           = j /= j'
        impliesBoolNeg _ _ = False




letfunc_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
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
letfunc_step fgs comp =
  case unComp comp of
    LetFunC nm params locals (MkComp (Return _fi e) _ _) cont
       | all (is_simpl_call_param . snd ) params
       -> do { let (fun_ty :: Ty) = TArrow (map (unCAExp . snd) params) (info e)
             ; cont' <- purify_calls fun_ty nm cont
             ; let new_params = map (\(n,t) -> (n, unCAExp t)) params
             ; let (fun :: Fun Ty) = MkFun (MkFunDefined nm new_params locals e) (compLoc comp) fun_ty
             ; rewrite $ MkComp (LetHeader nm fun cont') (compLoc comp) (compInfo comp) -- Same type!
             }
    _ -> return comp
  where
        purify_calls fun_ty nm = mapCompM_ return (replace_call fun_ty nm)
        replace_call fun_ty nm (MkComp (Call f es) xloc xnfo)
          | nm == f
          , let es' = map unCAExp es
          = let f_exp = MkExp (EVar f) xloc fun_ty
                call  = MkExp (ECall f_exp es') xloc (fromJust $ doneTyOfCTyBase xnfo)
                        -- Since original was return, we know it's a computer, hence fromJust works.
            in rewrite $ MkComp (Return AutoInline call) xloc xnfo
        replace_call _ _ other = return other

letfun_times_step fgs comp =
  case unComp comp of
    Times ui e elen i (MkComp (LetHeader f def cont) cloc cinfo)
     | MkFun (MkFunDefined n params locals body) floc fty <- def
     -> do { let fty' = TArrow (tyOfParams ((i,tint):params)) (fun_ret_ty fty)
                 def' = MkFun (MkFunDefined n ((i,tint):params) locals body) floc fty'
                 iexp = MkExp (EVar i) cloc tint -- The counter variable
           ; cont' <- augment_calls (n,fty') iexp cont
           ; rewrite $
             MkComp (LetHeader f def' (MkComp (Times ui e elen i cont') cloc cinfo)) cloc cinfo }
    _otherwise -> return comp

  where fun_ret_ty (TArrow _ r) = r
        fun_ret_ty  _           = error "Function not having an arrow type!?"

        augment_calls (fname,new_fty) iexp = mapCompM_ replace_ecall return
          where replace_ecall (MkExp (ECall (MkExp (EVar f) floc fty) es) xloc xnfo)
                  | fname == f
                  = rewrite $ MkExp (ECall (MkExp (EVar f) floc new_fty) (iexp:es)) xloc xnfo
                replace_ecall other = return other

times_unroll_step fgs comp
  = case unComp comp of
        Times ui e elen i c
         | EVal (VInt n') <- unExp elen
         , let n = fromIntegral n'
         , EVal (VInt 0) <- unExp e
         , (n < 3 && n > 0 && ui == AutoUnroll) || ui == Unroll
-- BOZIDAR: this will currently fail perf test for TX/test_encoding_34
--         , ui == Unroll -- || (n < 3 && n > 0 && ui == AutoUnroll)

         -> let idxs = [0..n-1]
                comps = replicate n c
                unrolled = zipWithM (\curr xc ->
                  substExpComp (i, MkExp (EVal (vint curr)) (expLoc e) tint) xc) idxs comps
            in case unrolled of
                 Nothing -> return comp
                 Just [] -> return $ MkComp (Return ForceInline (MkExp (EVal VUnit) (compLoc comp) TUnit))
                                            (compLoc comp)
                                            (compInfo comp)
                 Just xs -> rewrite $ mk_bind_many xs
        _ -> return comp

  where mk_bind_many :: [Comp CTy Ty] -> Comp CTy Ty
        mk_bind_many [] = error "times_unroll_step: can't happen!"
        mk_bind_many [x] = x
        mk_bind_many (x:xs) = MkComp (mkBind x (unused, mk_bind_many xs))
                                     (compLoc comp)
                                     (compInfo comp)
        unused = toName ("__unroll_unused_" ++ getLnNumInStr (compLoc comp)) Nothing Nothing


mkMultiLetExp :: [(Name,Exp a, ForceInline)] -> Exp a -> Exp a
mkMultiLetExp [] e = e
mkMultiLetExp ((x,e1,fi):bnds) e
  = MkExp (ELet x fi e1 (mkMultiLetExp bnds e)) (expLoc e) (info e)

mkMultiLetRefExp :: [(Name, Ty, Maybe (Exp a))] -> Exp a -> Exp a
mkMultiLetRefExp [] e = e
mkMultiLetRefExp ((x,ty,b1):bnds) e
  = MkExp (ELetRef x ty b1 (mkMultiLetRefExp bnds e)) (expLoc e) (info e)


isMultiLet_maybe comp
  = go comp []
  where go (LetE x fi e c) acc = go (unComp c) ((x,e,fi):acc)
        go (Return _fu e)  []  = Nothing
        go (Emit e)    []   = Nothing
        go (Emits e)   []   = Nothing

        go (Return fi e) acc   = Just (acc, Return fi e)
              -- We must have some accumulated bindings!
        go (Emit e) acc     = Just (acc, Emit e)
        go (Emits e) acc    = Just (acc, Emits e)
        go _ _ = Nothing


isMultiLetRef_maybe comp
  = go comp []
  where go (LetERef x ty e c) acc = go (unComp c) ((x,ty,e):acc)
        go (Return _fi e)  []    = Nothing
        go (Emit e)    []   = Nothing
        go (Emits e)   []   = Nothing

        go (Return fi e) acc   = Just (acc, Return fi e)
              -- We must have some accumulated bindings!
        go (Emit e) acc     = Just (acc, Emit e)
        go (Emits e) acc    = Just (acc, Emits e)
        go _ _ = Nothing


elim_times_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
elim_times_step fgs comp =
  case unComp comp of
    Times ui estart ebound cnt (MkComp (Return _ ebody) cloc cty) ->
        do { let efor = EFor ui cnt estart ebound ebody
           ; rewrite $ MkComp (Return AutoInline (MkExp efor cloc (info ebody)))
                              (compLoc comp)
                              (compInfo comp)
           }

    _ -> return comp




elim_automapped_mitigs :: DynFlags -> TypedCompPass
elim_automapped_mitigs dflags c
  | MkComp c0 cloc _ <- c          
  , Par p c1 c2 <- c0
  , Par p c11 c12 <- unComp c1 
  , LetHeader _f fun (MkComp (Map v f) _ _) <- unComp c12  -- (c1 - map f) - c2 
  , Mitigate ty1 i1 j1 <- unComp c11
  , Mitigate ty2 i2 j2 <- unComp c2           -- (c11' -- mitigate(i2,j2) -- map f) - mitigate(i2,j2) - c2'
  , i1 >= j1 && i2 <= j2                      -- down mit and up mit
  , let d1 = i1 `div` j1
  , let d2 = j2 `div` i2 
  , d1 == d2                                  -- exactly the same rate 
  , _f == f
  = rewrite_mit_map dflags ty1 (i1,j1) ty2 (i2,j2) (_f, fun) 

  | MkComp c0 cloc _ <- c          
  , Par p c1 c2 <- c0
  , Par p c21 c22 <- unComp c2
  , LetHeader _f fun (MkComp (Map v f) _ _) <- unComp c21  -- c1 - (map f - c2)
  , Mitigate ty1 i1 j1 <- unComp c1      -- (c11' -- mitigate(i1,j1) -- map f) - c2 
  , Mitigate ty2 i2 j2 <- unComp c22     -- (c11' -- mitigate(i2,j2) -- map f) - mitigate(i2,j2) - c2'
  , i1 >= j1 && i2 <= j2                      -- down mit and up mit
  , let d1 = i1 `div` j1
  , let d2 = j2 `div` i2 
  , d1 == d2                                  -- exactly the same rate 
  , _f == f
  = rewrite_mit_map dflags ty1 (i1,j1) ty2 (i2,j2) (_f, fun) 

  | otherwise
  = return c 



rewrite_mit_map :: DynFlags -> Ty -> (Int,Int) -> Ty -> (Int,Int) -> (Name, Fun Ty) -> RwM (Comp CTy Ty)
-- Precondition:  i1 `div` j1 == j2 `div` i2 
rewrite_mit_map fgs ty1 (i1,j1) ty2 (i2,j2) (f_name, fun)
  = do { let rng1 = if j1 == 1 then LISingleton else LILength j1
       ; let rng2 = if i2 == 1 then LISingleton else LILength i2
       ; let d = i1 `div` j1 
       ; let floc = funLoc fun 

         -- input variable
       ; x <- genSym "x"
       ; let x_ty   = TArr (Literal i1) ty1   -- input type
       ; let x_name = toName x floc (Just x_ty)
       ; let x_exp  = eVar floc x_ty x_name

         -- output variable
       ; y <- genSym "y"
       ; let y_ty   = TArr (Literal j2) ty2      -- output type
       ; let y_name = toName y floc (Just y_ty)
       ; let y_exp  = eVar floc y_ty y_name

         -- name of new map function
       ; new_f <- genSym "auto_map_mit"
       ; let f_ty = TArrow [x_ty] y_ty
       ; let new_f_name = toName new_f floc (Just f_ty) 

         -- type of original map function 
       ; let f_orig_ty@(TArrow [fun_in_ty] fun_ret_ty) = funInfo fun


         -- new counter 
       ; i <- genSym "i"
       ; let i_name = toName i floc (Just tint)   
       ; let i_exp  = eVar floc tint i_name 

         -- zero and 'd'
       ; let ezero = eVal floc tint (vint 0)
       ; let e_d   = eVal floc tint (vint d)
   
       
       ; let new_body = eSeq floc y_ty 
                         (eFor floc TUnit AutoUnroll i_name ezero e_d ekernel) -- do the for loop
                         y_exp                                                 -- and return y

             write_idx = eBinOp floc tint Mult (eVal floc tint (vint i2)) i_exp
             read_idx  = eBinOp floc tint Mult (eVal floc tint (vint j1)) i_exp

             earrrd    = eArrRead floc fun_in_ty x_exp read_idx rng1
             ekernel   = eArrWrite floc TUnit y_exp write_idx rng2 $ 
                            eCall floc fun_ret_ty (eVar floc f_orig_ty f_name) [earrrd]

      ; let new_trans_ty = CTBase (TTrans x_ty y_ty)

      ; let new_mapper = cMap floc new_trans_ty Nothing new_f_name
      ; let new_fun = MkFun (MkFunDefined new_f_name [(x_name,x_ty)] [(y_name,y_ty,Nothing)] new_body) floc f_ty

      ; rewrite $ 
         (cLetHeader floc new_trans_ty f_name fun $   -- original function 
            cLetHeader floc new_trans_ty new_f_name new_fun new_mapper)
       }
        







arrinit_step :: DynFlags -> TypedExpPass
-- Statically initialize as many arrays as possible
arrinit_step _fgs e1
  = do { r <- rwMIO (evalArrInt e1)
       ; case r of
           Nothing -> return e1
           Just vals ->
            rewrite $ MkExp (EValArr (map VInt vals)) (expLoc e1) (info e1)
       }

exp_inlining_steps :: DynFlags -> TypedExpPass
exp_inlining_steps fgs e
   -- Forced Inline!
 | ELet nm ForceInline e1 e2 <- unExp e
 = substExp (nm,e1) e2 >>= rewrite

 | ELet nm NoInline e1 e2 <- unExp e
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
exp_let_push_step fgs e
 | ELet nm fi e1 e2 <- unExp e
 , EArrWrite e0 estart0 elen0 erhs <- unExp e2
 , EArrRead evals estart rlen <- unExp erhs
 , let fvs = foldr (S.union . exprFVs) S.empty [e0, estart0, evals]
 , not (nm `S.member` fvs)
 = let estart' = eLet (expLoc estart) (info estart) nm fi e1 estart
   in rewrite $
      eArrWrite (expLoc e2) (info e2) e0 estart0 elen0 $
      eArrRead (expLoc erhs) (info erhs) evals estart' rlen
 | otherwise
 = return e



mk_read_ty :: Ty -> LengthInfo -> Ty
mk_read_ty base_ty LISingleton  = base_ty
mk_read_ty base_ty (LILength n) = TArr (Literal n) base_ty

asgn_letref_step :: DynFlags -> TypedExpPass
asgn_letref_step fgs e

  | EArrWrite e0 estart elen erhs <- unExp e
  , TArr _ ty <- info e0
  , not (ty == TBit)
   -- It has to be LILength so we can just take a pointer
  , LILength _ <- elen
  , EVar x <- unExp e0
   -- Just a simple expression with no side-effects
  , not (mutates_state estart)
  , Just (y, residual_erhs) <- returns_letref_var erhs
  , let read_ty = mk_read_ty ty elen
  = substExp (y, eArrRead loc read_ty e0 estart elen) residual_erhs >>= rewrite
  | otherwise = return e
  where
    loc = expLoc e
    returns_letref_var :: Exp Ty -> Maybe (Name, Exp Ty)
    returns_letref_var e = go e []

    go :: Exp Ty -> [(Name, Ty)] -> Maybe (Name, Exp Ty)
    go e letrefs
     = let loc = expLoc e
       in
       case unExp e of
          EVar v ->
            case lookup v letrefs of
              Nothing  -> Nothing
              Just _ty -> Just (v, eVal loc TUnit VUnit)

          ELet y fi e1 e2 ->
            case go e2 letrefs of
              Nothing -> Nothing
              Just (w,e2') -> Just (w, eLet loc TUnit y fi e1 e2')

          ELetRef y ty Nothing e2 ->
            case go e2 ((y,ty):letrefs) of
              Nothing -> Nothing
              Just (w,e2') | w == y
                           -> Just (w, e2')
                           | otherwise
                           -> Just (w, eLetRef loc TUnit y ty Nothing e2')
          ESeq e1 e2 ->
            case go e2 letrefs of
              Nothing -> Nothing
              Just (w,e2') -> Just (w, eSeq loc TUnit e1 e2')

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
alength_elim fgs e
 | EUnOp ALength e0 <- unExp e
 , (TArr nexp _)    <- info e0
 , let loc = expLoc e
 = rewrite $ numexp_to_exp loc nexp
 | otherwise
 = return e
 where numexp_to_exp loc (Literal i) = eVal loc tint (vint i)
       numexp_to_exp loc (NVar nm i) = eVar loc tint nm


eval_arith :: DynFlags -> TypedExpPass
eval_arith fgs e
  | arith_ty e              -- of arithmetic type
  , not (isEVal e)          -- not already a value
  , Just v <- evalArith e   -- evaluate it!
  = rewrite $ eVal (expLoc e) (info e) v
  | otherwise
  = return e
  where arith_ty e = case info e of
                       TInt {}    -> True
                       TDouble {} -> True
                       _ -> False

subarr_inline_step :: DynFlags -> TypedExpPass
subarr_inline_step fgs e
  | EArrRead evals estart LISingleton <- unExp e
  , EValArr vals <- unExp evals
  , EVal (VInt n') <- unExp estart
  , let n = fromIntegral n'
  = rewrite $ eVal (expLoc e) (getArrTy $ info e) (vals!!n)

  | EArrRead evals estart (LILength n) <- unExp e
  , EValArr vals <- unExp evals
  , EVal (VInt s') <- unExp estart
  , let s = fromIntegral s'
  = rewrite $ eValArr (expLoc e) (info e) (take n $ drop s vals)

    -- x[0,length(x)] == x
  | EArrRead evals estart (LILength n) <- unExp e
  , EVal (VInt 0) <- unExp estart
  , TArr (Literal m) _ <- info evals
  , n == m
  = rewrite evals

  | EArrWrite eval_tgt estart (LILength n) erhs <- unExp e
  , EVal (VInt 0) <- unExp estart
  , TArr (Literal m) _ <- info eval_tgt
  , n == m
  = rewrite $ eAssign (expLoc e) TUnit eval_tgt erhs

  | otherwise
  = return e

proj_inline_step :: DynFlags -> TypedExpPass
proj_inline_step fgs e
 | EProj e fn <- unExp e
 , EStruct _s fs_es <- unExp e
 , all (is_simpl_expr . snd) fs_es -- no side effects
 , Just ep <- lookup fn fs_es
 = rewrite ep
 | otherwise
 = return e


for_unroll_step :: DynFlags -> TypedExpPass
for_unroll_step fgs e
  | EFor ui nm estart elen ebody  <- unExp e
  , EVal (VInt 0) <- unExp estart
  , EVal (VInt n') <- unExp elen
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

  where mk_eseq_many :: [Exp Ty] -> Exp Ty
        mk_eseq_many []  = error "for_unroll_step: can't happen!"
        mk_eseq_many [x] = x
        mk_eseq_many (x:xs)
          = let e2 = mk_eseq_many xs
            in eSeq (expLoc e) (info e2) x e2

for_unroll_step fgs e
  = return e




const_fold :: DynFlags -> TypedExpPass
const_fold _ e@(MkExp e0 loc inf)
  = case go e0 of
      Nothing -> return e
      Just e' -> rewrite $ MkExp e' loc inf

  where
    go :: Exp0 Ty -> Maybe (Exp0 Ty)
    go (EBinOp Add e1 e2) | EVal (VInt 0) <- unExp e1 =
        return $ unExp e2

    go (EBinOp Add e1 e2) | EVal (VInt 0) <- unExp e2 =
        return $ unExp e1

    go (EBinOp Add e1 e2) | EVal (VInt i1) <- unExp e1
                          , EVal (VInt i2) <- unExp e2 =
        return $ EVal (VInt (i1+i2))

    go (EBinOp Sub e1 e2) | EVal (VInt i1) <- unExp e1
                          , EVal (VInt i2) <- unExp e2 =
        return $ EVal (VInt (i1-i2))

    go (EBinOp Mult e1 e2) | EVal (VInt 1) <- unExp e1 =
        return $ unExp e2

    go (EBinOp Mult e1 e2) | EVal (VInt 1) <- unExp e2 =
        return $ unExp e1

    go (EBinOp Mult e1 e2) | EVal (VInt i1) <- unExp e1
                           , EVal (VInt i2) <- unExp e2 =
        return $ EVal (VInt (i1*i2))

    go (EBinOp Div e1 e2) | EVal (VInt i1) <- unExp e1
                          , EVal (VInt i2) <- unExp e2 =
        return $ EVal (VInt (i1 `quot` i2))

    go (EBinOp Rem e1 e2) | EVal (VInt i1) <- unExp e1
                          , EVal (VInt i2) <- unExp e2 =
        return $ EVal (VInt (i1 `rem` i2))

    go (EBinOp Eq e1 e2) | EVal (VInt i1) <- unExp e1
                         , EVal (VInt i2) <- unExp e2 =
        return $ if i1 == i2 then EVal (VBool True) else EVal (VBool False)

    go (EBinOp Neq e1 e2) | EVal (VInt i1) <- unExp e1
                          , EVal (VInt i2) <- unExp e2 =
        return $ if i1 /= i2 then EVal (VBool True) else EVal (VBool False)

    go (EBinOp Lt e1 e2) | EVal (VInt i1) <- unExp e1
                         , EVal (VInt i2) <- unExp e2 =
        return $ if i1 < i2 then EVal (VBool True) else EVal (VBool False)

    go (EBinOp Gt e1 e2) | EVal (VInt i1) <- unExp e1
                         , EVal (VInt i2) <- unExp e2 =
        return $ if i1 > i2 then EVal (VBool True) else EVal (VBool False)

    go (EIf e1 e2 e3) | EVal (VBool flag) <- unExp e1 =
        return $ if flag then unExp e2 else unExp e3

    go e0 = Nothing
        -- pure e0

type TypedCompPass = Comp CTy Ty -> RwM (Comp CTy Ty)
type TypedExpPass  = Exp Ty -> RwM (Exp Ty)


runPasses :: GS.Sym
          -> RwStats
          -> [(String,TypedCompPass)]
          -> Comp CTy Ty
          -> IO (Bool, Comp CTy Ty, RwStats)
-- The Boolean value returned indicates that some rewriting happened (if True)
runPasses sym mp_init passes comp = go False mp_init passes comp
  where
    go b mp [] comp
      = return (b, comp, mp)
    go b mp ((pn,p):ps) comp
        = do { st <- getCPUTime
--             ; printf "Pass: %10s" pn
             ; r <- runRwM (p comp) sym
             ; en <- getCPUTime
             ; let diff = (fromIntegral (en - st)) / (10^12)
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
    , ("if-return-step",   if_return_step flags)

   ,  ("elim-automapped-mitigs", elim_automapped_mitigs flags)

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

runFold :: DynFlags -> GS.Sym -> Comp CTy Ty -> IO (Comp CTy Ty)
runFold flags sym comp
   = do { (comp',mp') <- go (RwStats Map.empty) 0 comp

        ; when (isDynFlagSet flags Verbose) $
          do { putStrLn "Optimizer statistics:"
             ; printRwStats mp' }

        ; return comp'
        }
 where
  comp_passes
    = map (\(nm, step) -> (nm, mapCompM_ return step))
          (foldCompPasses flags)
  exp_passes
    = map (\(nm,step) -> (nm, mapCompM_ (mapExpM_ step) return))
          (foldExpPasses flags)
  passes = comp_passes ++ exp_passes
  go mp depth comp
    | depth >= 10  = return (comp,mp)
    | otherwise
    = do { (rw_happened,comp1,mp1) <- runPasses sym mp passes comp
         ; if rw_happened then go mp1 (depth+1) comp1
                          else return (comp1,mp1)
         }


{- Elimination of mitigators
   ~~~~~~~~~~~~~~~~~~~~~~~~~ -}

elimMitigsIO :: GS.Sym -> Comp () () -> IO (Comp () ())
elimMitigsIO sym comp = go comp
  where go comp = do { r <- runRwM (elimMitigs comp) sym
                     ; case r of NotRewritten {} -> return comp
                                 Rewritten comp' -> go comp'
                     }






frm_mit :: Comp () () -> Maybe ((Ty,Int,Int), Comp () ())
-- returns (mitigator,residual-comp)
frm_mit c

  | Par p0 c1 c2 <- unComp c
  , Mitigate ty i1 i2  <- unComp c2
  = Just ((ty,i1,i2), c1)

  | Par p0 c1 c2 <- unComp c
  = case frm_mit c2 of
      Nothing -> Nothing
      Just (mit,c2') -> Just (mit, cPar (compLoc c) () p0 c1 c2')

  | LetHeader fn fdef@(MkFun (MkFunDefined {}) _ _) cont <- unComp c -- Needed because of AutoMaps! Yikes!
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetHeader loc () fn fdef cont')

  -- Special case for code emitted by the vectorizer
  | LetFunC fn prms locs body cont <- unComp c
  , Call fn' args <- unComp cont
  , fn == fn'
  , let loc = compLoc c
  = case frm_mit body of
      Nothing -> Nothing
      Just (mit,body') -> Just (mit, cLetFunC loc () fn prms locs body' cont)

  -- fallthrough general case
  | LetFunC fn prms locs body cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetFunC loc () fn prms locs body cont')

  | Let n c1 cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLet loc () n c1 cont')

  | LetE n f e cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetE loc () n f e cont')

  | LetERef n t me cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetERef loc () n t me cont')


  | otherwise
  = Nothing

flm_mit :: Comp () () -> Maybe ((Ty,Int,Int), Comp () ())
-- returns (mitigator,residual-comp)
flm_mit c

  | Par p0 c1 c2 <- unComp c
  , Mitigate ty i1 i2  <- unComp c1
  = Just ((ty,i1,i2), c2)

  | Par p0 c1 c2 <- unComp c
  = case flm_mit c1 of
      Nothing -> Nothing
      Just (mit,c1') -> Just (mit, cPar (compLoc c) () p0 c1' c2)

  | LetHeader fn fdef@(MkFun (MkFunDefined {}) _ _) cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetHeader loc () fn fdef cont')

  -- Special case for code emitted by the vectorizer
  | LetFunC fn prms locs body cont <- unComp c
  , Call fn' args <- unComp cont
  , fn == fn'
  , let loc = compLoc c
  = case flm_mit body of
      Nothing -> Nothing
      Just (mit,body') -> Just (mit, cLetFunC loc () fn prms locs body' cont)

  -- fallthrough general case
  | LetFunC fn prms locs body cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetFunC loc () fn prms locs body cont')


  | Let n c1 cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLet loc () n c1 cont')

  | LetE n f e cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetE loc () n f e cont')

  | LetERef n t me cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetERef loc () n t me cont')

  | otherwise
  = Nothing


elimMitigs :: Comp () () -> RwM (Comp () ())
-- Vectorizer-specific
elimMitigs comp
  = mapCompM_ return mitig comp 
  where
   mitig c
      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just (m1@(ty1,i1,j1),c1') <- frm_mit c1
      , Just (m2@(ty2,i2,j2),c2') <- flm_mit c2
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if (i1 `mod` j2 == 0) || (j2 `mod` i1) == 0 then
             do { rewrite $
                  cPar cloc () p c1' $
                  cPar cloc () (mkParInfo NeverPipeline)
                               (cMitigate cloc () ty1 i1 j2) c2'
                }
             else
             if l /= j1 then
                 rewrite $
                 cPar cloc () p (cPar cloc () pnever c1' (cMitigate cloc () ty1 i1 l))
                                (cPar cloc () pnever (cMitigate cloc () ty2 l j2) c2')
             else return c
           }

      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , Mitigate ty2 i2 j2 <- unComp c2
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if i1 `mod` j2 == 0 || j2 `mod` i1 == 0 then
             do { rewrite $
                  cPar cloc () p c1' (cMitigate cloc () ty1 i1 j2)
                }
             else
             if l /= j1 then
                 rewrite $
                 cPar cloc () p (cPar cloc () pnever c1' (cMitigate cloc () ty1 i1 l))
                                (cMitigate cloc () ty2 l j2)
             else return c
           }

      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , Mitigate ty1 i1 j1 <- unComp c1
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if i1 `mod` j2 == 0 || j2 `mod` i1 == 0 then
             do { rewrite $
                  cPar cloc () p (cMitigate cloc () ty1 i1 j2) c2'
                }
             else if l /= j1 then
                 rewrite $
                 cPar cloc () p (cMitigate cloc () ty1 i1 l)
                                (cPar cloc () pnever (cMitigate cloc () ty2 l j2) c2')
             else return c
           }

        -- throw away useless mitigators!
      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , WriteSnk {} <- unComp c2
      = rewrite $ cPar cloc () p c1' c2

      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , WriteInternal {} <- unComp c2
      = rewrite $ cPar cloc () p c1' c2

      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , ReadSrc {} <- unComp c1
      = rewrite $ cPar cloc () p c1 c2'

      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , ReadInternal {} <- unComp c1
      = rewrite $ cPar cloc () p c1 c2'

      -- trivial mitigators
      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Mitigate ty i1 i2 <- unComp c1
      , i1 == i2
      = rewrite c2

      | MkComp c0 cloc _ <- c
      , Par p c1 c2 <- c0
      , Mitigate ty i1 i2 <- unComp c2
      , i1 == i2
      = rewrite c1

      | otherwise
      = return c

