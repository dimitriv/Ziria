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
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, 
    GeneralizedNewtypeDeriving, MultiWayIf, QuasiQuotes, DeriveGeneric #-}
module PassExpr (
   passForUnroll
 , passElimUnused
 , passExpInlining
 , passAsgnLetRef
 , passExpLetPush 
 , passEval
) where

import Prelude hiding (exp)
import Control.Monad.Reader
import qualified Data.Set as S
import AstExpr
import AstUnlabelled
import CtExpr ( ctExp  )
import Interpreter
import Opts
import PassFoldDebug
import PpComp ()
import PpExpr ()

import PassFoldM

{-------------------------------------------------------------------------------
  Expression passes
-------------------------------------------------------------------------------}


-- | Loop unrolling
passForUnroll :: TypedExpPass
passForUnroll = TypedExpBottomUp $ \eloc e -> do
    let mk_eseq_many :: [Exp] -> Exp
        mk_eseq_many []     = eVal eloc TUnit VUnit
        mk_eseq_many [x]    = x
        mk_eseq_many (x:xs) = eSeq (expLoc x) x (mk_eseq_many xs)

    if | EFor ui nm estart elen ebody <- unExp e
         , EVal valTy (VInt 0) <- unExp estart
         , EVal _     (VInt n) <- unExp elen
         , (n < 8 && n > 0 && ui == AutoUnroll) || ui == Unroll
        -> do
          logStep "for-unroll" eloc [step| Unrolling loop |]
          let subst i' = substExp [] [(nm, eVal eloc valTy (vint i'))] ebody
              unrolled = map subst [0..n-1]
          rewrite $ mk_eseq_many unrolled

       | otherwise
        -> return e

-- | Eliminate unused bindings
passElimUnused :: TypedExpPass
passElimUnused = TypedExpBottomUp $ \eloc e -> do
  if | ELet nm _ e1 e2 <- unExp e
       , nm `S.notMember` exprFVs e2
       , ctExp e1 == TUnit
      -> rewrite (eSeq eloc e1 e2)
     | otherwise
      -> return e 

-- | Inline let bindings
passExpInlining :: TypedExpPass
passExpInlining = TypedExpBottomUp $ \eloc e -> do
  fgs <- getDynFlags

  if | ELet nm ForceInline e1 e2 <- unExp e
      -> do
        logStep "exp-inlining" eloc [step| Inlining nm |]
        rewrite $ substExp [] [(nm,e1)] e2 -- Forced Inline!

     | ELet _nm NoInline _e1 _e2 <- unExp e
      -> return e

     | ELet nm AutoInline e1 e2 <- unExp e
      ->
       if nm `S.notMember` exprFVs e2
         then if not (mutates_state e1)
                then do
                  logStep "exp-inlining/unused" eloc
                    [step| Eliminating binding for nm |]
                  rewrite e2
                else
                  return e
         else if is_simpl_expr e1 && not (isDynFlagSet fgs NoExpFold)
                then do
                  logStep "exp-inlining/subst" eloc
                    [step| Inlining binding for nm |]
                  rewrite $ substExp [] [(nm,e1)] e2
                else
                  return e

     | otherwise
      -> return e

-- | If we have an assignment to a fresh array variable @y@ to a slice of an
-- array @x@, we can instead do all operations on @x@ directly.
passAsgnLetRef :: TypedExpPass
passAsgnLetRef = TypedExpBottomUp $ \eloc exp -> if
  | EArrWrite e0 estart elen erhs <- unExp exp
    , TArray _ ty <- ctExp e0
    , not (ty == TBit)
     -- It has to be LILength so we can just take a pointer
    , LILength n <- elen
    , EVar x <- unExp e0
     -- Just a simple expression with no side-effects
    , not (mutates_state estart)
    , Just (y, residual_erhs) <- returns_letref_var erhs
   -> do
     let exp' = substExp [] 
                  [(y, eArrRead (expLoc exp) e0 estart elen)] residual_erhs

     logStep "asgn-letref" eloc
       [step| x[estart, n] := var y in { ... y ... ; return y }
          ~~> { ... x[estart, n] ... } |]

     rewrite exp'
  | otherwise
   ->
     return exp
  where
    returns_letref_var :: Exp -> Maybe (GName Ty, Exp)
    returns_letref_var = go []

    go :: [GName Ty] -> Exp -> Maybe (GName Ty, Exp)
    go letrefs e =
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
       where
         loc = expLoc e

-- | Push a let into an array-write with an array-read as RHS
passExpLetPush :: TypedExpPass
passExpLetPush = TypedExpBottomUp $ \eloc e -> if
    | ELet nm fi e1 e2 <- unExp e
      , EArrWrite e0 estart0 elen0 erhs <- unExp e2
      , EArrRead evals estart rlen <- unExp erhs
      , let fvs = foldr (S.union . exprFVs) S.empty [e0, estart0, evals]
      , not (nm `S.member` fvs)
     -> do
       logStep "exp-let-push" eloc
         [step| let nm = .. in e0[..] := evals[..]
            ~~> e0[..] := evals[let nm = .. in ..] |]
       let estart' = eLet (expLoc estart) nm fi e1 estart
       rewrite $ eArrWrite (expLoc e2) e0 estart0 elen0
               $ eArrRead (expLoc erhs) evals estart' rlen
    | otherwise
     -> return e

-- | Static evaluator pass
passEval :: TypedExpPass
passEval = TypedExpManual eval
  where
    eval :: Exp -> RwM Exp
    eval e = do
        ((e', (prints, stats)), time) <- measure $
          case {-# SCC interpreter #-} evalPartial e of
              (Right e', prints) ->
                return (e', prints)
              (Left err, _prints) ->
                -- Error during interpretation indicates program error
                fail $ "Program failure during evaluation: " ++ err
        unless (null prints) $ logStep "eval: debug prints" eloc (format prints)
        shouldLog <- debugFold
        when (shouldLog) $ do
          let label = "eval (" ++ show time ++ "; " ++ formatStats stats ++ ")"
          if e /= e' then logStep label eloc [step| e ~~> e' |]
                     else logStep label eloc [step| unchanged: e |]
        -- We use 'return' rather than 'rewrite' so that we don't attempt to
        -- write the binding again
        return e'
      where
        eloc = expLoc e

    format :: [(Bool, Value)] -> String
    format []              = ""
    format ((True,  v):vs) = show v ++ "\n" ++ format vs
    format ((False, v):vs) = show v         ++ format vs




