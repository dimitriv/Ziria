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

) where

import Prelude hiding (exp)
import Control.Monad.Reader
import GHC.Generics
import qualified Data.Set as S

import AstComp
import AstExpr
import AstUnlabelled
import CtExpr ( ctExp  )
import Interpreter
import Opts
import Outputable ()
import PassFoldDebug
import PpComp ()
import PpExpr ()
import qualified GenSym as GS

import PassFoldM

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

        SeqView (MkComp (Return fi e) _ ()) c12 -> do
          nm <- newPassFoldGName "_fold_unused" (ctExp e) cloc Imm
          logStep "fold/seq" cloc
            [step| return .. ; .. ~~> let nm = .. in .. |]
          c12' <- go c12
          rewrite $ cLetE cloc nm fi e c12'

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
       | EVal valTy (VInt 0) <- unExp e
       , EVal _     (VInt n) <- unExp elen
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
       logStep "inline/LetE/AutoInline" cloc
         [step| let nm = e in c ~~> c[e/nm] |]
       rewrite $ substComp [] [(nm,e1)] [] c2

    | LetHeader f@(MkFun (MkFunDefined {}) _ _) c2 <- unComp comp'
      , MkFunDefined nm params body' <- unFun f
      , (locals, body) <- extractEMutVars body'
      , no_lut_inside body
     -> do
       -- Inlining functions not always completely eliminates them (e.g. args
       -- to map, or non-simple arguments).

       (c2', didRewrite) <- recordLocalRewrite $
         inline_exp_fun_in_comp (nm,params,locals,body) c2

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
         inline_comp_fun (nm,params,c1) c2

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

-- | Lift mutable variable bindings over `take`
passPushCompLocals :: TypedCompPass
passPushCompLocals = TypedCompBottomUp $ \cloc comp' -> if
    | LetFunC nm params cbody_with_locals ccont <- unComp comp'
      , Just (locals, cbody) <- extractCMutVars' cbody_with_locals
      , BindMany tk [(x,emt)] <- unComp cbody
      , Take1 {} <- unComp tk
      , Emit e <- unComp emt
     -> do
       logStep "push-comp-locals" cloc
         [step| fun comp nm(..) { var locals ; x <- take ; emit .. }
            ~~> fun comp nm(..) { x <- take ; var locals ; emit .. } |]

       let cbody' = cBindMany cloc tk [(x,emt')]
           emt'   = cEmit cloc e'
           e'     = insertEMutVars' locals e
       rewrite $ cLetFunC cloc nm params cbody' ccont

    | otherwise
     -> return comp'

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

    | otherwise
     -> return comp

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
     -> do
       logStep "float-let-par/left" cloc
         [step| (let x = .. in ..) >>> .. ~~> let x = .. in (.. >>> ..) |]
       rewrite $ cLetE cloc x fi e1 (cPar cloc p c1' c2)

    | Par p c1 c2 <- unComp comp
      , LetE x fi e2 c2' <- unComp c2
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
   -- , f1 == f2
      , let cloc = compLoc comp
     -> do
       logStep "if-return" cloc
         [step| if eguard then { return .. } else { return .. }
            ~~> return (if eguard then .. else ..) |]
       rewrite $ cReturn cloc f1 $ eIf cloc eguard e1 e2

    | otherwise
     -> return comp


{-

Not wrong, by evil, we lose the letref-structure and LUT can't do a
very good job!

passFloatTopLetRef :: DynFlags -> Comp CTy Ty -> RwM (Comp CTy Ty)
passFloatTopLetRef fgs comp
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

inline_exp_fun_in_comp :: (GName Ty, [GName Ty], [MutVar], Exp)
                       -> Comp -> RwM Comp
inline_exp_fun_in_comp fun
  = mapCompM return return return return (inline_exp_fun fun) return

inline_exp_fun :: (GName Ty, [GName Ty], [MutVar], Exp)
               -> Exp -> RwM Exp
-- True means that it is safe to just get rid of the function
inline_exp_fun (nm,params,locals,body) e
  = mapExpM return return replace_call e
  where
    replace_call (MkExp (ECall nm' args) loc ())
--    | all is_simpl_expr args -- Like what we do for LetE/ELet
      | nm == nm'
       = do let xs                        = zip params args
                (subst, locals_from_args) = arg_subst xs
                subst_len                 = len_subst xs
            rewrite $ mk_local_lets subst subst_len (locals_from_args ++ locals) body
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
          , let siz_bnd = (toName siz_nm Nothing tint Imm, arg_size (ctExp arg))
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
        arg_size (TArray (NVar siz_nm) _) = eVar loc (toName siz_nm Nothing tint Imm)
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

inline_comp_fun :: (GName CTy, [GName (CallArg Ty CTy)], Comp) 
                -> Comp -> RwM Comp
inline_comp_fun (nm,params,cbody) c = do
    mapCompM return return return return return replace_call c
  where
    replace_call :: Comp -> RwM Comp
    replace_call (MkComp (Call nm' args) _ ())
      | all is_simpl_call_arg args
      , nm == nm'
      = rewrite $ substComp [] (mk_expr_subst params args) [] cbody
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


