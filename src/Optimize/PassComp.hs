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
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, 
    GeneralizedNewtypeDeriving, MultiWayIf, QuasiQuotes, DeriveGeneric #-}
module PassComp (
    passFold
  , passPurify
  , passPurifyLetRef
  , passElimTimes
  , passLetFunc
  , passLetFunTimes
  , passTimesUnroll
  , passInline
  , passPushCompLocals
  , passTakeEmit
  , passFloatLetFunRepeat
  , passFloatLetPar
  , passIfDead
  , passIfReturn
  , elimSeq
  , passPushMit

) where

import Prelude hiding (exp)
import GHC.Generics
import qualified Data.Set as S

-- import Outputable

import Data.Loc
import Data.Monoid

import Data.Maybe ( fromJust )

import AstComp
import AstExpr
import AstUnlabelled
import CtExpr ( ctExp  )
import CtComp ( ctComp )
import Interpreter
import PassFoldDebug
import PpComp ()
import PpExpr ()
import PassFoldM
import Utils ( panicStr )

{-------------------------------------------------------------------------------
  Comp passes: standard optimizations
-------------------------------------------------------------------------------}

-- | Convert `return` to `let`, and remove function definitions from the RHS
-- of binds.
--
-- NOTE: We have to manually traverse the entire bind structure,
-- because although the rewriting will happen at every node in the
-- AST, there may only be a single bind node for a sequence of
-- binds. The same is not true for a sequence of 'seq' nodes
-- though. This is rather subtle, and fold_step is the _only_ pass
-- that actually does this. I'm not sure if there are other subtle
-- bugs due to this.



elimSeq :: TypedCompPass
elimSeq = TypedCompBottomUp $ \_ -> go 
  where
    go comp = case unComp comp of 
      Seq c1 c2 -> do 
        nm <- newPassFoldGName "_unused" (fromJust $ doneTyOfCTy $ ctComp c1) cloc Imm
        rewrite $ cBindMany cloc c1 [(nm,c2)]
      _ -> return comp
      where cloc = compLoc comp

passFold :: TypedCompPass
passFold = TypedCompBottomUp $ \_ -> go
  where
    go comp = do
      let cloc = compLoc comp
      case bindSeqView comp of
        BindView nm (MkComp (Return fi e) _ ()) c12 -> do
          logStep "fold/bind-return" cloc
            [step| nm <- return .. ~~> let nm =  .. |]
          c12' <- go c12
          rewrite $ cLetE cloc nm fi e c12'

        BindView nm (MkComp (LetHeader fdef c) _ ()) c12 -> do
          logStep "fold/bind-header" cloc
            [step| nm <- fun f(..) { .. } in ..
               ~~> fun f(..) { .. } in nm <- .. |]
          c12' <- go c12
          rewrite $ cLetHeader cloc fdef (cBindMany cloc c [(nm, c12')])

        BindView nm c c12 -> do
          c12' <- go c12
          return $ cBindMany cloc c [(nm, c12')]

{- 
        SeqView (MkComp (Return fi e) _ ()) c12 -> do
          nm <- newPassFoldGName "_fold_unused" (ctExp e) cloc Imm
          logStep "fold/seq" cloc
            [step| return .. ; .. ~~> let nm = .. in .. |]
          c12' <- go c12
          rewrite $ cLetE cloc nm fi e c12'
-}

        _otherwise -> do
          return comp

-- | Translate computation level `LetE` to expression level `Let`
passPurify :: TypedCompPass
passPurify = TypedCompBottomUp $ \cloc comp ->
    case extractCLetEs comp of
      Just (binds, comp') | Return fi e <- unComp comp' -> do
        logStep "purify/return" cloc
          [step| let binds in return .. ~~> return (let binds in ..) |]
        rewrite $ cReturn cloc fi (insertELetEs binds e)

      Just (binds, comp') | Emit e <- unComp comp' -> do
        logStep "purify/emit" cloc
          [step| let binds in emit .. ~~> emit (let binds in ..) |]
        rewrite $ cEmit cloc (insertELetEs binds e)

      Just (binds, comp') | Emits e <- unComp comp' -> do
        logStep "purify/emits" cloc
          [step| let binds in emits .. ~~> emits (let binds in ..) |]
        rewrite $ cEmits cloc (insertELetEs binds e)

      _otherwise ->
        return comp

-- | Translate computation level `LetERef` to expression level `LetRef`
passPurifyLetRef :: TypedCompPass
passPurifyLetRef = TypedCompBottomUp $ \cloc comp -> do
    case extractCMutVars' comp of
      Just (binds, comp') | Return fi e <- unComp comp' -> do
        logStep "purify-letref/return" cloc
          [step| var binds in return .. ~~> return (var binds in ..) |]
        rewrite $ cReturn cloc fi (insertEMutVars' binds e)

      Just (binds, comp') | Emit e <- unComp comp' -> do
        logStep "purify-letref/emit" cloc
          [step| var binds in emit .. ~~> emit (var binds in ..) |]
        rewrite $ cEmit cloc (insertEMutVars' binds e)

      Just (binds, comp') | Emits e <- unComp comp' -> do
        logStep "purify-letref/emits" cloc
          [step| var binds in emits .. ~~> emits (var binds in ..) |]
        rewrite $ cEmits cloc (insertEMutVars' binds e)

      _otherwise ->
        return comp

-- | Translate computation level `Times` to expression level `For`
passElimTimes :: TypedCompPass
passElimTimes = TypedCompBottomUp $ \cloc comp ->
    case unComp comp of
      Times ui estart ebound cnt (MkComp (Return _ ebody) _cloc ()) -> do
        logStep "elim-times" cloc
          [step| for cnt in [estart, ebound] return {..}
             ~~> return (for cnt in [estart, ebound] {..} |]
        rewrite $ cReturn cloc AutoInline
                    (eFor cloc ui cnt estart ebound ebody)

      _ -> return comp

-- | Translate computation functions to expression functions
--
-- This will allow more drastic inlining in the inlining step.
passLetFunc :: TypedCompPass
passLetFunc = TypedCompBottomUp $ \cloc comp' -> do
    case unComp comp' of
      LetFunC nm params (MkComp (Return _fi e) _ ()) cont
        | Just params' <- mapM fromSimplCallParam params
         -> do
           logStep "letfunc" cloc
             [step| fun comp nm(..) { return {..} } in .. nm(..)
                ~~> fun nm(..) {..} in .. return(nm(..)) .. |]

           let fun_ty :: Ty
               fun_ty = TArrow (map nameArgTy params') (ctExp e)

               nm' :: GName Ty
               nm' = nm { nameTyp = fun_ty }

               def' :: Fun
               def' = mkFunDefined cloc nm' params' e

               replace_call :: Comp -> RwM Comp
               replace_call (MkComp (Call nm'' es) xloc ())
                 | uniqId nm' == uniqId nm''
                 = let es'  = map unCAExp es
                       call = eCall xloc nm' es'
                   in rewrite $ cReturn xloc AutoInline call
               replace_call other = return other

               purify_calls :: Comp -> RwM Comp
               purify_calls = 
                 mapCompM return return return return return replace_call

           cont' <- purify_calls cont
           rewrite $ cLetHeader cloc def' cont'
      _ ->
        return comp'

-- | Lift expression function definitions out of computation loops
passLetFunTimes :: TypedCompPass
passLetFunTimes = TypedCompBottomUp $ \_cloc comp ->
    case unComp comp of
      Times ui e elen i (MkComp (LetHeader def cont) cloc ())
        | MkFun (MkFunDefined f params body) floc () <- def
         -> do
           iprm <- newPassFoldGName "i_prm" (nameTyp i) cloc Imm

           logStep "letfun-times" cloc
             [step| for i in [e, elen] { fun f(..) { .. } ; .. }
                ~~> fun f(i, ..) { .. } ; for i in [e, elen] { .. } |]

           -- Freshen the function definition 
           let fty' = TArrow (map nameArgTy (iprm:params)) 
                             (fun_ret_ty (nameTyp f))
               f'   = f { nameTyp = fty' }
               def' = mkFunDefined floc f' (iprm:params) $ 
                      substExp [] [(i, eVar floc iprm)] body
               iexp = eVar cloc i -- The counter variable
           cont' <- augment_calls f' iexp cont
           rewrite $ cLetHeader cloc def' (cTimes cloc ui e elen i cont')

      _otherwise -> return comp

  where
    fun_ret_ty :: Ty -> Ty
    fun_ret_ty (TArrow _ r) = r
    fun_ret_ty  _           = error "Function not having an arrow type!?"

    augment_calls :: GName Ty -> Exp -> Comp -> RwM Comp
    augment_calls f' iexp =
        mapCompM return return return return replace_ecall return
      where
        replace_ecall (MkExp (ECall f es) xloc ())
          | uniqId f' == uniqId f
          = rewrite $ eCall xloc f' (iexp:es)
        replace_ecall other = return other

-- | Loop unrolling
passTimesUnroll :: TypedCompPass
passTimesUnroll = TypedCompBottomUp $ \cloc comp -> do
    unused <- newPassFoldGName "_unroll_unused" TUnit cloc Imm
    let mk_bind_many :: [Comp] -> Comp
        mk_bind_many []     = error "times_unroll_step: can't happen!"
        mk_bind_many [x]    = x
        mk_bind_many (x:xs) = 
          cBindMany (compLoc x) x [(unused, mk_bind_many xs)]

    case unComp comp of
      Times ui e elen i c
       | EVal valTy (VInt 0 Signed) <- unExp e
       , EVal _     (VInt n Signed) <- unExp elen
       , n > 0 -- NOTE: We don't unroll even if explicitly requested
       , (n < 3 && ui == AutoUnroll) || (ui == Unroll)
-- BOZIDAR: this will currently fail perf test for TX/test_encoding_34
--         , ui == Unroll -- || (n < 3 && n > 0 && ui == AutoUnroll)
       -> do

         logStep "times-unroll" cloc
           [step| for i in [0, elen] { body } ~~> body ; .. ; body |]

         let subst i' = substComp [] [(i, eVal (expLoc e) valTy (vint i'))] [] c
             unrolled = map subst [0..n-1]
                               
         rewrite $ mk_bind_many unrolled
      _ ->
        return comp

-- | Inlining of the various let bindings
passInline :: TypedCompPass
passInline = TypedCompBottomUp $ \cloc comp' -> if
    | Let nm c1 c2 <- unComp comp'
     -> do
       logStep "inline/Let" cloc
         [step| let comp nm = c in c' ~~> c'[c/nm] |]
       rewrite $ substComp [] [] [(nm,c1)] c2

{- Too   much inlining!
    | LetE nm e1 c2 <- unComp comp
      , isDynFlagSet fgs AutoLUT
      , Just rgs <- varRanges e1
      , Right True <- shouldLUT [] rgs e1    -- and the expression is luttable
      , Just (_, [], _) <- inOutVars [] Map.empty e1
     -> substExpComp (nm,e1) c2 >>= rewrite
-}

    | LetE nm ForceInline e1 c2 <- unComp comp'
     -> do
       logStep "inline/LetE/ForceInline" cloc
         [step| let nm = e in c ~~> c[e/nm] |]
       rewrite $ substComp [] [(nm,e1)] [] c2

    | LetE _nm NoInline _e1 _c2 <- unComp comp'
     -> return comp'

    | LetE nm AutoInline e1 c2 <- unComp comp'
      , is_simpl_expr e1
     -> do
       -- liftIO $ print (ppr c2)
       let c2' = substComp [] [(nm,e1)] [] c2
       -- liftIO $ putStrLn "##########"
       -- liftIO $ print (ppr c2')
       logStep "inline/LetE/AutoInline" cloc
         [step| let nm = e in c ~~> c[e/nm] |]
       rewrite c2'

    | LetHeader f@(MkFun (MkFunDefined {}) _ _) c2 <- unComp comp'
      , MkFunDefined nm params body <- unFun f
      , no_lut_inside body
     -> do
       -- Inlining functions not always completely eliminates them (e.g. args
       -- to map, or non-simple arguments).

       (c2', didRewrite) <- recordLocalRewrite $ 
                            inlineLetFun nm params body c2

       if | not didRewrite ->
             return $ cLetHeader cloc f c2'
          | S.member nm (compEFVs c2') -> do
             logStep "inline/LetHeader" cloc
               [step| inlining nm (unable to eliminate nm completely) |]
             rewrite $ cLetHeader cloc f c2'
          | otherwise -> do
             logStep "inline/LetHeader" cloc
               [step| inlining nm (eliminating nm completely) |]
             rewrite c2'

    | LetFunC nm _params _c1 c2 <- unComp comp'
      , not (S.member nm (compCFVs c2)) -- Completely unused
     -> do
       logStep "inline/LetFunC" cloc
         [step| removing unused function nm |]
       rewrite c2

    | LetFunC nm params c1 c2 <- unComp comp'
     -> do
       -- Like expression functions, computation functions cannot always be
       -- completely eliminated. Mostly this happens when they are given
       -- non-simple arguments.

       (c2', didRewrite) <- recordLocalRewrite $
                            inlineLetFunC nm params c1 c2

       if | not didRewrite ->
              return $ cLetFunC cloc nm params c1 c2'
          | S.member nm (compCFVs c2') -> do
              logStep "inline/LetFunC" cloc
                [step| inlining nm (unable to eliminate nm completely) |]
              rewrite $ cLetFunC cloc nm params c1 c2'
          | otherwise -> do
              logStep "inline/LetFunC" cloc
                [step| inlining nm (eliminating nm completely) |]
              rewrite $ c2'

    | otherwise
     -> return comp'

{-------------------------------------------------------------------------------
  Comp passes: more aggressive optimizations
-------------------------------------------------------------------------------}

-- | Turn explicit `take`/`emit` loop into application of `map`
passTakeEmit :: TypedCompPass
passTakeEmit = TypedCompBottomUp $ \cloc comp -> if
    | Repeat nfo bm <- unComp comp
      , BindMany tk [(x,emt)] <- unComp bm
      , Take1 ain <- unComp tk
      , Emit e    <- unComp emt
     -> do
       let xty  = ain
           ety  = ctExp e
           eloc = expLoc e
           fty  = TArrow [(GArgTy xty Imm)] ety

       fname <- newPassFoldGName "auto_map_" fty cloc Imm

       logStep "take-emit" cloc
         [step| repeat { x <- take ; emit .. }
             ~~> let fname(x) = { .. } in map fname } |]

       let letfun  = cLetHeader cloc fun mapcomp
           mapcomp = cMap cloc nfo fname
                   -- NB: We pass the nfo thing,
                   -- to preserve vectorization hints!
           fun = mkFunDefined eloc fname [x] e
       rewrite letfun
 
    | Repeat nfo bm <- unComp comp
      , BindMany mit_tk [(x,emt)] <- unComp bm
      , Par p mit tk <- unComp mit_tk
      , Take1 {} <- unComp tk
      , Emit {}  <- unComp emt
      -> rewrite $ 
         cPar cloc p mit $ 
         cRepeat cloc nfo (cBindMany cloc tk [(x,emt)])

    | BindMany c [(x,ret)] <- unComp comp
      , Return _ e <- unComp ret
      , EVar x' <- unExp e
      , x == x'
      -> rewrite c

    | otherwise
     -> return comp

-- | Push let and letref bindings after "take" nodes. This is important
-- for enabling the passTakeEmit phase in situations like:
--  repeat $ let x = e in do
--    z <- take
--    emit foobar
--  ~~> 
--  repeat $ do z <- take
--    let x = e
--    emit foobar
--
passPushCompLocals :: TypedCompPass
passPushCompLocals = TypedCompBottomUp $ \cloc comp -> if
    | Just (cbody,builder,fvs) <- ebind $ unComp comp
      , BindMany tk ((x1,c1):crest)  <- unComp cbody
      , is_tk (unComp tk)
      , x1 `S.notMember` fvs -- Avoid capture (which can happen because of unrolling!)
     -> do
      logStep "push-comp-locals" cloc
        [step| fun comp nm(..) { var locals ; x <- take ; emit .. }
           ~~> fun comp nm(..) { x <- take ; var locals ; emit .. } |]
      rewrite $
        cBindMany cloc tk [(x1, builder (cBindMany cloc c1 crest))]
    | otherwise
     -> return comp

    where
     ebind (LetE nm fi e cbody)
       = Just (cbody, cLetE (compLoc cbody) nm fi e, exprFVs e)
     ebind (LetERef nm mbe cbody)
       = Just (cbody, cLetERef (compLoc cbody) nm mbe, maybe S.empty exprFVs mbe)
     ebind _ = Nothing

     is_tk (Take1 {}) = True
     is_tk (Take {})  = True
     is_tk _          = False
    
-- | Let computation functions out of repeat loops.
--
-- This will give the opportunity to take-emit to kick in and rewrite the whole
-- thing to map!
passFloatLetFunRepeat :: TypedCompPass
passFloatLetFunRepeat = TypedCompBottomUp $ \cloc comp' -> if
    | Repeat wdth rcomp <- unComp comp'
      , LetFunC nm params cbody ccont <- unComp rcomp
      , Call nm' _args <- unComp ccont
      , nm' == nm
     -> do
       logStep "float-letfun-repeat" cloc
         [step| repeat { fun comp nm(..) { .. } in nm(..) }
            ~~> fun comp nm(..) { repeat { .. } } in nm(..) } |]

       rewrite $ cLetFunC cloc nm params (cRepeat cloc wdth cbody) ccont
    | otherwise
     -> return comp'

-- | Float let out of parallel composition
passFloatLetPar :: TypedCompPass
passFloatLetPar = TypedCompBottomUp $ \cloc comp -> if
    | Par p c1 c2 <- unComp comp
      , LetE x fi e1 c1' <- unComp c1
      , x `S.notMember` (compEFVs c2) -- avoid capture
     -> do
       logStep "float-let-par/left" cloc
         [step| (let x = .. in ..) >>> .. ~~> let x = .. in (.. >>> ..) |]
       rewrite $ cLetE cloc x fi e1 (cPar cloc p c1' c2)

    | Par p c1 c2 <- unComp comp
      , LetE x fi e2 c2' <- unComp c2
      , x `S.notMember` (compEFVs c1) -- avoid capture 
     -> do
       logStep "float-let-par/right" cloc
         [step| .. >>> (let x = .. in ..) ~~> let x = .. in (.. >>> ..) |]
       rewrite $ cLetE cloc x fi e2 (cPar cloc p c1 c2')

    | otherwise
     -> return comp

-- | Eliminate unreachable conditional branches
passIfDead :: TypedCompPass
passIfDead = TypedCompBottomUp $ \cloc comp -> do
    case unComp comp of
      Branch e c1 c2
        | provable e -> do
           logStep "ifdead/provable/true" cloc
             [step| if true then c else .. ~~> c |]
           rewrite $ c1
        | provable (eNot e) -> do
           logStep "ifdead/provable/false" cloc
             [step| if false then .. else c ~~> c |]
           rewrite $ c2
      Branch e (MkComp (Branch e' c1 c2) _ ()) c3
        | e `implies` e'
        -> do
          logStep "ifdead/left/implies" cloc
            [step| if e then {if e' then c else c'} else c''
               ~~> if e c else c'' |]
          rewrite $ cBranch cloc e c1 c3

        | e `implies` eNot e'
        -> do
          logStep "ifdead/left/implies-neg" cloc
            [step| if e then {if e' then c else c'} else c''
               ~~> if e then c' else c'' |]
          rewrite $ cBranch cloc e c2 c3

      Branch e c1 (MkComp (Branch e' c2 c3) _ ())
        | eNot e `implies` e'
        -> do
          logStep "ifdead/right/implies" cloc
            [step| if e then c else {if e' then c' else c''}
               ~~> if e then c else c' |]
          rewrite $ cBranch cloc e c1 c2

        | eNot e `implies` eNot e'
        -> do
          logStep "ifdead/right/implies-neg" cloc
            [step| if e then c else {if e' then c' else c''}
               ~~> if e then c else c'' |]
          rewrite $ cBranch cloc e c1 c3

      _otherwise -> return comp

-- | Translate computation-level conditional to expression-level conditional
passIfReturn :: TypedCompPass
passIfReturn = TypedCompBottomUp $ \_cloc comp -> if
    | Branch eguard c1 c2 <- unComp comp
      , Return f1 e1  <- unComp c1
      , Return _f2 e2 <- unComp c2
   -- , f1 == f2 important?
      , let cloc = compLoc comp
     -> do
       logStep "if-return" cloc
         [step| if eguard then { return .. } else { return .. }
            ~~> return (if eguard then .. else ..) |]
       rewrite $ cReturn cloc f1 $ eIf cloc eguard e1 e2

    | otherwise
     -> return comp


-- | Push mitigators under seq 
passPushMit :: TypedCompPass
passPushMit = TypedCompBottomUp $ \cloc  comp -> if 
{- 
  | Par p mit1 c2 <- unComp comp
    , BindMany ch xs_cs <- unComp c2
    , Mitigate {} <- unComp mit1
   -> rewrite $ cBindMany cloc (cMitIn cloc p mit1 ch) 
                               (map (\(x,c) -> (x, cMitIn cloc p mit1 c)) xs_cs)
  | Par p c1 mit2 <- unComp comp
    , BindMany ch xs_cs <- unComp c1
    , Mitigate {} <- unComp mit2
   -> rewrite $ cBindMany cloc (cMitOut cloc p ch mit2) 
                               (map (\(x,c) -> (x, cMitOut cloc p c mit2)) xs_cs)

  | Par p mit1 c2 <- unComp comp
    , Seq ch ct   <- unComp c2
    , Mitigate {} <- unComp mit1
   -> rewrite $ cSeq cloc (cMitIn cloc p mit1 ch) (cMitIn cloc p mit1 ct)

  | Par p c1 mit2 <- unComp comp
    , Seq ch ct   <- unComp c1
    , Mitigate {} <- unComp mit2
   -> rewrite $ cSeq cloc (cMitOut cloc p ch mit2) (cMitOut cloc p ct mit2)

-}

    -- These two have to do with the vectorizer 

  | Repeat va cbody <- unComp comp
   , Par p mit1 c2 <- unComp cbody
   , Mitigate {} <- unComp mit1
   -> rewrite $ cPar cloc p mit1 (cRepeat cloc va c2)

  | Repeat va cbody <- unComp comp
   , Par p c1 mit2  <- unComp cbody
   , Mitigate {} <- unComp mit2
   -> rewrite $ cPar cloc p (cRepeat cloc va c1) mit2


  | Let x c cbody <- unComp comp
   , Par p mit1 c2 <- unComp cbody
   , Mitigate {} <- unComp mit1
   -> rewrite $ cPar cloc p mit1 (cLet cloc x c c2)

  | Let x c cbody <- unComp comp
   , Par p c1 mit2 <- unComp cbody
   , Mitigate {} <- unComp mit2
   -> rewrite $ cPar cloc p (cLet cloc x c c1) mit2

  | LetE x f e cbody <- unComp comp
   , Par p mit1 c2 <- unComp cbody
   , Mitigate {} <- unComp mit1
   -> rewrite $ cPar cloc p mit1 (cLetE cloc x f e c2)

  | LetE x f e cbody <- unComp comp
   , Par p c1 mit2 <- unComp cbody
   , Mitigate {} <- unComp mit2
   -> rewrite $ cPar cloc p (cLetE cloc x f e c1) mit2

  | LetERef x e cbody <- unComp comp
   , Par p mit1 c2 <- unComp cbody
   , Mitigate {} <- unComp mit1
   -> rewrite $ cPar cloc p mit1 (cLetERef cloc x e c2)

  | LetERef x e cbody <- unComp comp
   , Par p c1 mit2 <- unComp cbody
   , Mitigate {} <- unComp mit2
   -> rewrite $ cPar cloc p (cLetERef cloc x e c1) mit2

  | LetHeader f cbody <- unComp comp
   , Par p mit1 c2 <- unComp cbody
   , Mitigate {} <- unComp mit1
   -> rewrite $ cPar cloc p mit1 (cLetHeader cloc f c2)

  | LetHeader f cbody <- unComp comp
   , Par p c1 mit2 <- unComp cbody
   , Mitigate {} <- unComp mit2
   -> rewrite $ cPar cloc p (cLetHeader cloc f c1) mit2

  | otherwise
   -> return comp
  
{-
  where cMitIn l p m c 
          | TVoid <- inTyOfCTy (ctComp c) = c
          | otherwise = cPar l p m c
        cMitOut l p c m 
          | TVoid <- yldTyOfCTy (ctComp c) = c
          | otherwise = cPar l p c m
-}

  --   -- Input mitigation on an emit/emits
  -- | Par _p mit1 c2 <- unComp comp
  --   , Emit {} <- unComp c2
  --   , Mitigate {} <- unComp mit1
  --  -> rewrite c2

  -- | Par _p mit1 c2 <- unComp comp
  --   , Emits {} <- unComp c2
  --   , Mitigate {} <- unComp mit1
  --  -> rewrite c2

  --   -- Output mitigation on an take/takes
  -- | Par _p c1 mit2 <- unComp comp
  --   , Take1 {} <- unComp c1
  --   , Mitigate {} <- unComp mit2
  --  -> rewrite c1
  -- | Par _p c1 mit2 <- unComp comp
  --   , Take {} <- unComp c1
  --   , Mitigate {} <- unComp mit2
  --  -> rewrite c1

  -- | Par p c mit <- unComp comp
  --   , Return {} <- unComp c
  --   , Mitigate {} <- unComp mit
  --  -> rewrite c

  -- | Par p mit c <- unComp comp
  --   , Return {} <- unComp c
  --   , Mitigate {} <- unComp mit
  --  -> rewrite c




{-------------------------------------------------------------------------------
  Some view patterns
-------------------------------------------------------------------------------}

data BindView
  = BindView (GName Ty) Comp Comp
  | SeqView Comp Comp
  | NotSeqOrBind Comp

-- | Decompose the AST `BindMany` constructors into single `BindView` nodes
-- so that we can consider each in turn
bindSeqView :: Comp -> BindView
bindSeqView = mk_view
  where
    mk_view c@(MkComp (BindMany c1 c2s) cloc ()) =
      case c2s of
        (nm,c2):rest -> BindView nm c1 (MkComp (mkBindMany c2 rest) cloc ())
        []           -> NotSeqOrBind c
    mk_view (MkComp (Seq c1 c2) _cloc ()) = SeqView c1 c2
    mk_view c = NotSeqOrBind c

fromSimplCallParam :: GName (CallArg Ty CTy) -> Maybe (GName Ty)
fromSimplCallParam nm =
  case nameTyp nm of
    CAExp  t -> Just nm{nameTyp = t}
    CAComp _ -> Nothing


{-------------------------------------------------------------------------------
  Inlining auxiliary
-------------------------------------------------------------------------------}

{- How to inline function arguments
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We consider two cases for inlining function arguments:

A. Immutable arguments.
   We simply let-bind the argument (and the ordinary
   inliner for let-bound variables should take care of inlining that). E.g.
       fun foo(w : arr[256] int) { body }
   and application:
       foo(my_array)
   will be inlined to:
       let w = my_array
       in body
   and the let-inliner should deal with potentially inlining w
   wherever that is safe.
  
B. Mutable arguments.
   In this case we know by static type checking that we
   have a mutable dereference expression. Consider for example the function:
       fun foo(var w : complex16) { body }
   and an application
       foo(y[(i+34*45):5].im)
   In this case we lift the expression indices inside the deref expression
   with:
       let idx = i+34*45
       in
       body[y[idx:5].im / w]
  that is we /directly/ substitute for the derefernce expression.
  The ordinary let-inliner may choose to evaluate the index or inline it.
-----------------------------------------------------------------------------}

data InlineData
  = InlineData { inl_let_bound :: [(GName Ty, Exp)]
               , inl_subst     :: [(GName Ty, Exp)]
               , inl_len_subst :: [(LenVar, NumExpr)] }

instance Monoid InlineData where
  mempty = InlineData [] [] []
  mappend (InlineData b1 s1 l1) 
          (InlineData b2 s2 l2) = InlineData (b1++b2) (s1++s2) (l1++l2)

mappendM :: RwM InlineData -> RwM InlineData -> RwM InlineData
mappendM m1 m2 = do
  i1 <- m1
  i2 <- m2
  return $ i1 `mappend` i2

-- | Apply the inline data to an expression
appInlDataExpr :: SrcLoc -> InlineData -> Exp -> Exp
appInlDataExpr loc (InlineData { inl_let_bound = let_bound
                               , inl_subst     = subst
                               , inl_len_subst = subst_len })
  = substExp subst_len subst . go let_bound
  where 
    go [] e               = e
    go ((nm1,eb1):bnds) e = eLet loc nm1 AutoInline eb1 $ go bnds e

-- | Apply the inline data plus a computation substition to a computation
appInlDataComp :: SrcLoc 
               -> InlineData -> [(GName CTy,Comp)] -> Comp -> Comp
appInlDataComp loc (InlineData { inl_let_bound = let_bound
                               , inl_subst     = subst
                               , inl_len_subst = subst_len }) csubst
  = substComp subst_len subst csubst . go let_bound
  where
    go [] c = c
    go ((nm1,eb1):bnds) c = cLetE loc nm1 AutoInline eb1 $ go bnds c


inlAsSubst :: GName Ty -> Exp -> InlineData
inlAsSubst nm e 
  = InlineData { inl_let_bound = []
               , inl_subst = [(nm,e)], inl_len_subst = [] }

inlAsLetBound :: GName Ty -> Exp -> InlineData
inlAsLetBound nm e 
  = InlineData { inl_let_bound = [(nm,e)]
               , inl_subst = [], inl_len_subst = [] }

-- | Figure out if this argument induces a length substitution
inline_length :: GName Ty -> Ty -> InlineData
inline_length prm argty
  | TArray (NVar siz_nm) _ <- nameTyp prm
  , TArray nexpr _ <- argty
  , let siz_var  = toName siz_nm noLoc tint Imm
  , let siz_expr = nexpr_to_expr nexpr
  = InlineData { inl_let_bound = []
               , inl_subst     = [(siz_var,siz_expr)]
               , inl_len_subst = [(siz_nm,nexpr)] }
  | otherwise = mempty
  where
    nexpr_to_expr (NVar s)    = eVar noLoc (toName s noLoc tint Imm)
    nexpr_to_expr (Literal s) = eVal noLoc tint (vint s)  


-- | How to inline an immutable parameter: effectively by let-binding it
inline_immutable :: GName Ty -> Exp -> RwM InlineData
inline_immutable nm expr
  | is_simpl_expr expr = return $ inlAsSubst nm expr
  | otherwise          = return $ inlAsLetBound nm expr


-- | How to inline a mutable parameter: see notes above
inline_mutable :: SrcLoc -> 
                  GName Ty -> GDerefExp Ty () -> RwM InlineData
inline_mutable loc nm dexpr = do 
  (bnds,simpl_dexpr) <- lift_idx dexpr
  return (bnds `mappend` inlAsSubst nm simpl_dexpr)
  where 
    -- | Lift and let-bind non-simple indices
    lift_idx (GDVar x)       = return (mempty, eVar loc x)
    lift_idx (GDProj de fld) = do
      (bnds, simpl_de) <- lift_idx de
      return (bnds, eProj loc simpl_de fld)
    lift_idx (GDArr de estart li)
      | is_simpl_expr estart
      = do (bnds,simpl_de) <- lift_idx de
           return (bnds, eArrRead loc simpl_de estart li)
      | otherwise
      = do (bnds,simpl_de) <- lift_idx de
           idx <- newPassFoldGName "idx" (ctExp estart) loc Imm
           return (bnds `mappend` inlAsLetBound idx estart,
                     eArrRead loc simpl_de (eVar loc idx) li)


inlineParam :: GName Ty -> Exp -> RwM InlineData
inlineParam prm earg = do
  inl <- inline_prm
  return $ inl `mappend` inline_length prm (ctExp earg)
  where 
    inline_prm
      | isMutable prm
      , Just earg_deref <- isMutGDerefExp earg
      = inline_mutable (expLoc earg) prm earg_deref
      | otherwise
      = inline_immutable prm earg

inlineParams :: [GName Ty] -> [Exp] -> RwM InlineData
inlineParams = go 
  where
    go [] [] = return mempty
    go (prm:prms) (earg:eargs) 
      = inlineParam prm earg `mappendM` go prms eargs
    go _ _ = panicStr "inlineParams"


inlineLetFun :: GName Ty   -- function name
             -> [GName Ty] -- function params
             -> Exp        -- function body
             -> Comp       -- computation to substitute in
             -> RwM Comp
inlineLetFun f prms body
  = mapCompM return 
             return 
             return 
             return (inlineLetFunInExp f prms body) return

inlineLetFunInExp :: GName Ty -> [GName Ty] -> Exp -> Exp -> RwM Exp
inlineLetFunInExp f prms body
  = mapExpM return return replace_call
  where 
    replace_call (MkExp (ECall f' eargs) loc ())
      | f == f'
      = do inl_data <- inlineParams prms eargs
           rewrite (appInlDataExpr loc inl_data body)
    replace_call eother = return eother

inlineLetFunC :: GName CTy                -- function name
              -> [GName (CallArg Ty CTy)] -- function params
              -> Comp                     -- function body
              -> Comp                     -- computation to substitute in
              -> RwM Comp
inlineLetFunC f prms body
  = mapCompM return 
             return
             return 
             return 
             return replace_call
  where
    replace_call (MkComp (Call f' call_args) loc ())
      | f == f'
      , let (eargs,cargs) = partitionCallArgs call_args
      , let (eprms,cprms) = partitionParams prms
      , let comp_binds    = zip cprms cargs
      = do inl_data <- inlineParams eprms eargs
           rewrite (appInlDataComp loc inl_data comp_binds body)
    replace_call eother = return eother


