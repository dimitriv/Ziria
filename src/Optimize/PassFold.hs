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
    do { sym' <- GS.genSym gs
       ; return (NotRewritten $ prefix ++ show sym') }


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

    BindView (MkComp (Return e) rloc rinfo) nm c12 ->
     do { -- rwMIO $ putStrLn ("Found BindView" ++ compShortName comp)
        ; fold_step fgs c12 >>= \c12' -> 
                rewrite $ MkComp (LetE nm e c12') cloc cinfo
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

    SeqView (MkComp (Return e) rloc rinfo) c12 -> 
      do { -- rwMIO $ putStrLn ("Found BindView" ++ compShortName comp)
         ; let nm = toName ("__fold_unused_" ++ getLnNumInStr cloc) 
                           Nothing Nothing
         ; fold_step fgs c12 >>= \c12' -> 
               rewrite $ MkComp (LetE nm e c12) cloc cinfo }
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
  , LetE x e1 c1' <- unComp c1
  = rewrite $ cLetE loc cty x e1 (cPar loc cty p c1' c2)
  | Par p c1 c2 <- unComp comp
  , LetE x e2 c2' <- unComp c2
  = rewrite $ cLetE loc cty x e2 (cPar loc cty p c1 c2')
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
  where mk_letrefs eloc ety lcls e = go lcls e 
          where go [] e = e 
                go ((lcl,ty,Nothing):lcls) e 
                  = eLetRef eloc ety lcl (Left ty) (go lcls e)
                go ((lcl,ty,Just einit):lcls) e
                  = eLetRef eloc ety lcl (Right einit) (go lcls e)


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

  | LetE nm e1 c2 <- unComp comp
  , is_simpl_expr e1                     -- or if it is a simple expression
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
is_simpl_expr0 (EArrRead e1 e2 _li) = is_simpl_expr e1 && is_simpl_expr e2
is_simpl_expr0 (EUnOp u e)          = is_simpl_expr e
is_simpl_expr0 (EStruct _ fses) = all is_simpl_expr (map snd fses)
is_simpl_expr0 _ = False 

no_lut_inside x 
  = isJust (mapExpM_ elut_nothing x)
  where elut_nothing :: Exp Ty -> Maybe (Exp Ty) 
        elut_nothing (MkExp (ELUT {}) _ _) = Nothing
        elut_nothing other                 = Just other



inline_exp_fun :: (Name,[(Name,Ty)],[(Name,Ty,Maybe (Exp Ty))],Exp Ty) 
               -> Exp Ty -> RwM (Exp Ty)
-- True means that it is safe to just get rid of the function
inline_exp_fun (nm,params,locals,body) e
  = mapExpM_ replace_call e
  where replace_call e@(MkExp (ECall (MkExp (EVar nm') _ _) args) _ _)
          | all (not . is_side_effecting) args 
          , nm == nm'
          = do { let xs        = zip params args
                     subst     = arg_subst xs
                     subst_len = len_subst xs
               ; e' <- mk_local_lets subst subst_len locals body
               ; rewrite e' 
               }
         where
            loc = expLoc e
            arg_subst [] = []
            arg_subst (((prm_nm,prm_ty),arg):rest) 
              -- An array of variable size. 
              -- Here we must substitute for the size too
              | TArr (NVar siz_nm _m) _ <- prm_ty
              = (prm_nm,arg):(siz_nm, arg_size (info arg)):(arg_subst rest)
              | otherwise
              = (prm_nm,arg):(arg_subst rest)

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
            arg_size (TArr (Literal siz) _)    = eVal loc tint (VInt siz)
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
                             eLetRef (expLoc e) (info e) x' (Left ty') e' 
                           }
                    go ((x,ty,Just xe):xs) e 
                      = do { e' <- go xs e
                           ; xe' <- substAllTyped subst len_subst xe
                           ; ty' <- substAllLengthTy len_subst ty
                           ; let x' = x { mbtype = Just ty' } 
                           ; return $ 
                             eLetRef (expLoc e) (info e) x' (Right xe') e' 
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
      Just (binds, Return e) -> 
          rewrite $ MkComp (Return (mkMultiLetExp (reverse binds) e)) 
                           (compLoc comp) (compInfo comp) 

      Just (binds, Emit e) -> 
          rewrite $ MkComp (Emit (mkMultiLetExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      Just (binds, Emits e) ->
          rewrite $ MkComp (Emits (mkMultiLetExp (reverse binds) e))
                           (compLoc comp) (compInfo comp)

      _otherwise -> return comp


ifpar_step_left :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- m >>> (if e then c1 else c2)
-- ~~>
-- (if e then m >>> c1 else m >>> c2)
-- 
-- NB: This seems only safe if 'e' does not somehow depend on shared
-- state with m (which it shouldn't)
-- 
ifpar_step_left fgs comp = 
  case unComp comp of 
    Par _p m (MkComp (Branch e c1 c2) bloc binfo)
      -> let mc1 = MkComp (Par _p m c1) (compLoc comp) (compInfo comp)
             mc2 = MkComp (Par _p m c2) (compLoc comp) (compInfo comp)
         in rewrite $ MkComp (Branch e mc1 mc2) bloc (compInfo comp)

    _otherwise -> return comp

ifpar_step_right :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- (if e then c1 else c2) >>> c3 
-- ~~>
-- (if e then c1 >>> c3 else c2 >>> c3)
ifpar_step_right fgs comp = 
  case unComp comp of 

    Par _p (MkComp (Branch e c1 c2) bloc binfo) c3
     -> rewrite $ 
        MkComp (Branch e (MkComp (Par _p c1 c3) (compLoc comp) (compInfo comp))
                         (MkComp (Par _p c2 c3) (compLoc comp) (compInfo comp)))
               bloc (compInfo comp)

    _otherwise -> return comp


bm_par_step_left :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- m >>> (c1 ; ...;  cn)
-- ~~> 
-- (m >>> c1 ; m >>> c2 ; ... m >>> cn)
--
bm_par_step_left fgs comp 
  = case unComp comp of
      Par _p m (MkComp (BindMany c1 xs_cs) bloc binfo)
         -> let mc1 = MkComp (Par _p m c1) bloc (upd_inty m (compInfo c1))
                m_xs_cs = map (\(x,c) -> 
                 (x, MkComp (Par _p m c) bloc (upd_inty m (compInfo c)))) xs_cs
            in rewrite $ MkComp (BindMany mc1 m_xs_cs) bloc (compInfo comp)

      Par _p m (MkComp (Seq c1 c2) bloc binfo)
         -> let mc1 = MkComp (Par _p m c1) bloc (upd_inty m (compInfo c1))
                mc2 = MkComp (Par _p m c2) bloc (upd_inty m (compInfo c2))
            in rewrite $ MkComp (Seq mc1 mc2) bloc (compInfo comp)

      _otherwise -> return comp
    
  where upd_inty m (CTBase (TComp v a b))
           = CTBase (TComp v (inTyOfCTyBase $ compInfo m) b)
        upd_inty m (CTBase (TTrans a b))   
           = CTBase (TTrans (inTyOfCTyBase $ compInfo m) b)
        upd_inty m _ 
           = error "upd_comp_ty: not a base type"


bm_par_step_right :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
-- (c1 ; ...;  cn) >>> m 
-- ~~> 
-- (c1 >>> m; c2 >>> m; ... cn >>> m)
--
-- 
-- (c1 >>> {m1;m2}) >>> m 
bm_par_step_right fgs comp 
  = case unComp comp of
 
      Par _p (MkComp (BindMany c1 xs_cs) bloc binfo) m 
         -> let mc1     = MkComp (Par _p c1 m) bloc (upd_yldty m (compInfo c1))
                m_xs_cs = map (\(x,c) -> 
                 (x, MkComp (Par _p c m) bloc (upd_yldty m (compInfo c)))) xs_cs
            in rewrite $ MkComp (BindMany mc1 m_xs_cs) bloc (compInfo comp)

      Par _p (MkComp (Seq c1 c2) bloc binfo) m 
         -> let mc1 = MkComp (Par _p c1 m) bloc (upd_yldty m (compInfo c1))
                mc2 = MkComp (Par _p c2 m) bloc (upd_yldty m (compInfo c2))
            in rewrite $ MkComp (Seq mc1 mc2) bloc (compInfo comp)

      _otherwise -> return comp
    
  where upd_yldty m (CTBase (TComp a b x))
           = CTBase (TComp a b (yldTyOfCTyBase $ compInfo m))
        upd_yldty m (CTBase (TTrans a x))   
           = CTBase (TTrans a (yldTyOfCTyBase $ compInfo m))
        upd_yldty m _ 
           = error "upd_comp_ty: not a base type"

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
    LetFunC nm params locals (MkComp (Return e) _ _) cont
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
            in rewrite $ MkComp (Return call) xloc xnfo
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
         | EVal (VInt n) <- unExp elen 
         , EVal (VInt 0) <- unExp e
         , (n < 3 && n > 0 && ui == AutoUnroll) || ui == Unroll 
-- BOZIDAR: this will currently fail perf test for TX/test_encoding_34
--         , ui == Unroll -- || (n < 3 && n > 0 && ui == AutoUnroll)

         -> let idxs = [0..n-1]
                comps = replicate n c
                unrolled = zipWithM (\curr xc -> 
                  substExpComp (i, MkExp (EVal (VInt curr)) (expLoc e) tint) xc) idxs comps
            in case unrolled of
                 Nothing -> return comp
                 Just [] -> return $ MkComp (Return (MkExp (EVal VUnit) (compLoc comp) TUnit)) 
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


mkMultiLetExp :: [(Name,Exp a)] -> Exp a -> Exp a
mkMultiLetExp [] e = e
mkMultiLetExp ((x,e1):bnds) e 
  = MkExp (ELet x e1 (mkMultiLetExp bnds e)) (expLoc e) (info e) 


isMultiLet_maybe comp
  = go comp []
  where go (LetE x e c) acc = go (unComp c) ((x,e):acc)
        go (Return e)  []   = Nothing 
        go (Emit e)    []   = Nothing
        go (Emits e)   []   = Nothing 

        go (Return e) acc   = Just (acc, Return e) 
              -- We must have some accumulated bindings!
        go (Emit e) acc     = Just (acc, Emit e)
        go (Emits e) acc    = Just (acc, Emits e)
        go _ _ = Nothing


elim_times_step :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
elim_times_step fgs comp =
  case unComp comp of
    Times ui estart ebound cnt (MkComp (Return ebody) cloc cty) ->
        do { let efor = EFor ui cnt estart ebound ebody
           ; rewrite $ MkComp (Return (MkExp efor cloc (info ebody))) 
                              (compLoc comp) 
                              (compInfo comp)
           }

    _ -> return comp



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
 | ELet nm e1 e2 <- unExp e
 , let fvs = exprFVs e2
 , let b = nm `S.member` fvs
 = if not b && not (is_side_effecting e) 
   then rewrite e2 
   else 
     if is_simpl_expr e1 && not (isDynFlagSet fgs NoExpFold) 
     then substExp (nm,e1) e2 >>= rewrite
     else return e 
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
  , not (is_side_effecting estart)
  , Just (y, residual_erhs) <- returns_letref_var erhs
  , let read_ty = mk_read_ty ty elen
  = substExp (y, eArrRead loc read_ty e0 estart elen) residual_erhs >>= rewrite
  | otherwise = return e
  where 
    loc = expLoc e
    returns_letref_var :: Exp Ty -> Maybe (Name, Exp Ty)
    returns_letref_var e = go e []
    go e letrefs 
     = let loc = expLoc e 
       in 
       case unExp e of
          EVar v -> 
            case lookup v letrefs of 
              Nothing  -> Nothing
              Just _ty -> Just (v, eVal loc TUnit VUnit)

          ELet y e1 e2 -> 
            case go e2 letrefs of 
              Nothing -> Nothing
              Just (w,e2') -> Just (w, eLet loc TUnit y e1 e2')

          ELetRef y (Left ty) e2 -> 
            case go e2 ((y,ty):letrefs) of
              Nothing -> Nothing
              Just (w,e2') | w == y 
                           -> Just (w, e2')
                           | otherwise
                           -> Just (w, eLetRef loc TUnit y (Left ty) e2')
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
       -- >>= proj_inline_step fgs


exp_inline_step :: DynFlags -> TypedExpPass
exp_inline_step fgs e
  | ELet nm e1 e2 <- unExp e
  , not (isDynFlagSet fgs NoExpFold)
  , is_simpl_expr e1
  = substExp (nm,e1) e2 >>= rewrite 
  | otherwise
  = return e

alength_elim :: DynFlags -> TypedExpPass
alength_elim fgs e 
 | EUnOp ALength e0 <- unExp e
 , (TArr nexp _)    <- info e0
 , let loc = expLoc e
 = rewrite $ numexp_to_exp loc nexp
 | otherwise 
 = return e
 where numexp_to_exp loc (Literal i) = eVal loc tint (VInt i)
       numexp_to_exp loc (NVar nm i) = eVar loc tint nm
       numexp_to_exp loc (NArr {})   = error "TC bug: unresolved NArr!" 


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
  , EVal (VInt n) <- unExp estart
  = rewrite $ eVal (expLoc e) (getArrTy $ info e) (vals!!n)

  | EArrRead evals estart (LILength n) <- unExp e
  , EValArr vals <- unExp evals 
  , EVal (VInt s) <- unExp estart 
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
  , EVal (VInt n) <- unExp elen
  , (n < 8 && n > 0 && ui == AutoUnroll) || ui == Unroll 
  = -- liftIO (putStrLn "for_unroll_step, trying ...") >> 
    let idxs = [0..n-1]
        exps = replicate n ebody
        unrolled = zipWith (\curr xe -> runIdentity $ 
                                        substExp (nm, eVal (expLoc e) tint (VInt curr)) xe) idxs exps
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
    [ ("fold"        , fold_step  flags       )
    , ("purify"      , purify_step  flags     )
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


-- Experiment: this gives perf improvement for large pipelines
-- probably because of less code duplication and LUT sharing.
--
--    , ("ifpar-left"         , ifpar_step_left flags         )        
--    , ("ifpar-right"        , ifpar_step_right flags        )        
    , ("ifdead"             , ifdead_step flags             )       

    -- Don't use: not wrong but does not play nicely with LUT
    --  , ("float-top-letref"   , float_top_letref_step flags   )
    --     
    -- The following cause the DDK compiler to explode for no obvious reason,
    -- so I keep them commented for now (although they are actually important)
    --  , ("bmpar-left"  , bm_par_step_left )
    --  , ("bmpar-right" , bm_par_step_right)
    ]

foldExpPasses :: DynFlags -> [(String,TypedExpPass)]
foldExpPasses flags
  | isDynFlagSet flags NoFold || isDynFlagSet flags NoExpFold
  = []
  | otherwise 
  = [ ("for-unroll", for_unroll_step flags)
    , ("exp-inlining-steps", exp_inlining_steps flags)
    , ("asgn-letref-step", asgn_letref_step flags)
    , ("rest-chain", rest_chain flags)
    ]
 
runFold :: DynFlags -> GS.Sym -> Comp CTy Ty -> IO (Comp CTy Ty)
runFold flags sym comp 
   = do { (comp',mp') <- go (RwStats Map.empty) 0 comp
{-
        ; putStrLn "Optimizer statistics:"
        ; printRwStats mp'
-}
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

