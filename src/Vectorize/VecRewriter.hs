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
{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts #-}
module VecRewriter ( 
   rwTakeEmitIO
 , RwState ( .. )
 , RwQSt   ( .. )
 , doRw
 , doNotRw
 , vectAssign, vectEmit
) where

import Control.Applicative
import Control.Monad.State
import Data.Loc

import AstComp
import AstExpr
import AstFM
import AstUnlabelled

import CardAnalysis
import CtComp
import CtExpr
import Outputable ()
import Text.PrettyPrint.HughesPJ
import Control.Exception.Base ( assert )
import VecM
import Utils ( panicStr, panic )
import Outputable

import Data.Maybe ( isJust, isNothing )

{-------------------------------------------------------------------------
  Vectorizer rewriter of simple computers 
-------------------------------------------------------------------------}

type IVecM s a = StateT s VecM a

type TakeTransf s     = SrcLoc -> Ty        -> IVecM s Comp
type TakeManyTransf s = SrcLoc -> Ty -> Int -> IVecM s Comp
type EmitTransf s     = SrcLoc -> Exp       -> IVecM s Comp
type EmitManyTransf s = SrcLoc -> Exp       -> IVecM s Comp

lkpCVar :: CId -> IVecM s LComp
lkpCVar nm = StateT (\s -> lookupCVarBind nm >>= \c -> return (c,s))

lkpCFun :: CId -> IVecM s CFunBind
lkpCFun nm = StateT (\s -> lookupCFunBind nm >>= \c -> return (c,s))

extCVar :: CId -> LComp -> IVecM s a -> IVecM s a
extCVar nm lc action = StateT (extendCVarBind nm lc . runStateT action)

extCFun :: CId -> [GName (CallArg Ty CTy)] -> LComp -> IVecM s a -> IVecM s a
extCFun nm params cbody action
  = StateT (extendCFunBind nm params cbody . runStateT action)

liftVecM :: VecM a -> IVecM s a
liftVecM action = StateT (\s -> action >>= \r -> return (r,s))


tryWithCurrSt :: IVecM s a -> IVecM s (s,a)
tryWithCurrSt action = do
  s <- get        -- get current state
  a <- action     -- execute the action in current state
  sact <- get     -- get final state
  put s           -- restore original state
  return (sact,a) -- return the state in the end of the action

{- Note [The Rewriter State]
   ~~~~~~~~~~~~~~~~~~~~~~~~

RwState consists of a state we can work with for rewriting
Take(s) and a state for rewriting Emit(s). Each of those is represented
with the RwQSt (rewriter queus state) datatype. 

A rewriter queue state either says:

   a) do not rewrite Take(s)/Emit(s) at all (presumably because we are
      not vectorizing in that queue,

   ... OR
 
   b) yes please do rewrite and the rw_offset is the current offset
      we are rewriting from and we should be updating upon every
      Take(s)/Emit(s).
-------------------------------------------------------------------------------}

data RwState 
  = RwState { rws_in  :: RwQSt
            , rws_out :: RwQSt }

data RwQSt
  = DoNotRw
  | DoRw { rw_vector :: EId
         , rw_offset :: Exp }

joinRwState :: RwState -> RwState -> RwState
joinRwState rws1 rws2 
  = RwState { rws_in  = rws_in rws1  `aux` rws_in rws2
            , rws_out = rws_out rws1 `aux` rws_out rws2 }
  where aux = joinRwQSt

joinRwQSt :: RwQSt -> RwQSt -> RwQSt
joinRwQSt DoNotRw DoNotRw = DoNotRw
joinRwQSt DoNotRw _       = panicStr "Can't join RwQSt!" 
joinRwQSt _ DoNotRw       = panicStr "Can't join RwQSt!" 
joinRwQSt (DoRw va1 off1) (DoRw va2 _off2) 
  = assert (va1 == va2) $ DoRw va1 off1


-- | If we use arrays of size <= 1 then we do not really have to rewrite
doRw :: Int -> EId -> Exp -> RwQSt
doRw n vect offset = if n <= 1 then DoNotRw else DoRw vect offset

doNotRw :: RwQSt
doNotRw = DoNotRw


-- | Set the new input offset in the case where input RwQSt was DoRw
upd_in_offset :: Exp -> IVecM RwState ()
upd_in_offset new_off = modify upd 
  where upd (RwState rin rout) = RwState rin { rw_offset = new_off } rout

-- | Set the new output offset in the case where output RwQSt was DoRw
upd_out_offset :: Exp -> IVecM RwState ()
upd_out_offset new_off = modify upd 
  where upd (RwState rin rout) = RwState rin rout { rw_offset = new_off }

-- Shorthand for incrementing an offset
offset_plus :: Exp -> Int -> Exp
offset_plus off i = interpE (expLoc off) $ (off .+ I(i))

-- | What to do on Take1
on_take :: TakeTransf RwState
on_take loc ty = do
  rin <- gets rws_in
  case rin of
    DoNotRw -> return $ cTake1 loc ty
    DoRw vect off -> do
      upd_in_offset (off `offset_plus` 1)
      liftVecM $ liftZr "" loc $ freturn _fI (vect .! off)

-- | What to do on Take (many)
on_takes :: TakeManyTransf RwState
on_takes loc ty n = do  
  rin <- gets rws_in
  case rin of
    DoNotRw -> return $ cTake loc ty n
    DoRw vect off -> do
      upd_in_offset (off `offset_plus` n)
      liftVecM $ liftZr "" loc $ freturn _fI (vect .! ( off :+ n ))

-- | What to do on Emit
on_emit :: EmitTransf RwState 
on_emit loc e = do
  rout <- gets rws_out
  case rout of
    DoNotRw -> return $ cEmit loc e
    DoRw vect off -> do      
      upd_out_offset (off `offset_plus` 1) 
      liftVecM $ liftZr "" loc $ do { vect .! off .:= e }

-- | What to do on Emits (many)
on_emits :: EmitManyTransf RwState 
on_emits loc es = do
  rout <- gets rws_out
  case rout of
    DoNotRw -> return $ cEmits loc es
    DoRw vect off -> do      
      upd_out_offset (off `offset_plus` len) 
      liftVecM $ liftZr "" loc $ do { vect .! (off :+ len) .:= es }
  where TArray (Literal len) _ = ctExp es


-- | Precondition: we only attempt to rewrite SCard cain caout
-- Moreover, if cain (caout, respectively) is 
--     * CAStatic i  => we will rewrite every take (or emit)
--     * CAUnknown   => we will not attempt to rewrite at all
-- Correspondingly, the state of the IVecM monad should be
rwTakeEmit :: LComp -> IVecM RwState Comp
rwTakeEmit lx = assert (isJust $ isSimplCard_mb (compInfo lx)) (go lx)
  where
    go lcomp =
      let loc = compLoc lcomp in 
      case unComp lcomp of 
        -- Standard boilerplate 
        Var x -> lkpCVar x >>= go 

        BindMany c1 xs_cs -> do
          c1' <- go c1
          let go_one (x,c) = go c >>= \c' -> return (x,c')
          xs_cs' <- sequence (map go_one xs_cs)
          return $ cBindMany loc c1' xs_cs'
        Let x c1 c2            -> extCVar x c1 (go c2)
        LetE x fi e c1         -> cLetE loc x fi e    <$> go c1
        LetERef x me c1        -> cLetERef loc x me   <$> go c1
        LetHeader fun c1       -> cLetHeader loc fun  <$> go c1
        LetStruct sdef c1      -> cLetStruct loc sdef <$> go c1
        LetFunC f params c1 c2 -> extCFun f params c1 $ go c2

        Standalone c1 -> cStandalone loc <$> go c1
        Return fi e   -> return $ cReturn loc fi e 
        Seq c1 c2 -> do
          c1' <- go c1
          c2' <- go c2
          return $ cSeq loc c1' c2'
        Call f es -> do 
          (CFunBind { cfun_params = prms
                    , cfun_body   = body }) <- lkpCFun f
          vbody <- go body
          let vf_type = CTArrow (map nameCallArgTy prms) (ctComp vbody)
          vf <- liftVecM $ newVectGName (name f ++ "_spec") vf_type loc Imm
          return $ cLetFunC loc vf prms vbody $ 
                   cCall loc vf (map eraseCallArg es)

        -- Actual rewriting
        Take1 t  -> on_take loc t
        Take t n -> on_takes loc t n 
        Emit e   -> on_emit loc e
        Emits es -> on_emits loc es

        Branch e c1 c2 -> do
          (s1,c1') <- tryWithCurrSt (go c1)
          (s2,c2') <- tryWithCurrSt (go c2)
          put (joinRwState s1 s2)
          return $ cBranch loc e c1' c2'

        Times ui e elen n c -> do
          let body_card = compInfo c
          case body_card of
            OCard -> return (eraseComp lcomp)
            SCard ain aout -> do
              -- normalize index to start from 0 (and make sure we are int32)
              let eidx_int32 = eUnOp loc (Cast tint) $ eBinOp loc Sub (eVar loc n) e
                  elen_int32 = eUnOp loc (Cast tint) $ elen
              c' <- iterInvRwState ain aout eidx_int32 elen_int32 (go c)
              return $ cTimes loc ui e elen n c'

        Until {} ->
          assert (unknown_times_card (compInfo lcomp)) $ 
          -- Input or output cardinality of until node will be 0 or unknown
          --  o If it is unknown then we won't be rewriting (see VecSF.hs)
          --  o If it is zero then there is nothing to rewrite!
          -- .. Hence it suffices to simply return the exact same node!
          return (eraseComp lcomp)

        While {} ->
          -- Same reasoning as for Until node, above
          assert (unknown_times_card (compInfo lcomp)) $ 
          return (eraseComp lcomp)
          
        
        -- All other cases will have OCard cardinality (we assert)
        -- and hence they are impossible to happen.
        _other
          -> assert (isNothing $ isSimplCard_mb (compInfo lcomp)) $
             panic $ vcat [ text "VecRewriter: encountered OCard"
                          , nest 2 $ ppr lcomp ]


iterInvRwState :: CAlpha -- Input cardinality
               -> CAlpha -- Output cardinality 
               -> Exp    -- Index expression (starting from 0)
               -> Exp    -- Length
               -> IVecM RwState a -> IVecM RwState a
iterInvRwState body_ain body_aout eidx elen action = do
  (_,res) <- tryWithCurrSt $ withStateT (updSt eidx body_ain body_aout) action
  modify (updSt elen body_ain body_aout)
  return res

  where 
    updSt :: Exp -> CAlpha -> CAlpha -> RwState -> RwState
    updSt x cain caout rws
      = RwState { rws_in  = updStQ x cain  $ rws_in rws
                , rws_out = updStQ x caout $ rws_out rws }
 
    updStQ :: Exp -> CAlpha -> RwQSt -> RwQSt
    updStQ _x (CAUnknown ) (DoNotRw     ) = DoNotRw
    updStQ _x _            (DoNotRw     ) = DoNotRw
    updStQ  x (CAStatic i) (DoRw vec off) = DoRw vec off'
      where off' = interpE (expLoc eidx) (off .+ (i .* x))
    updStQ _x _            _              = panicStr "updInStQ: impossible!"



-- | Main entry point to the take/emit rewriter
rwTakeEmitIO :: VecEnv -> RwState -> LComp -> IO Comp
rwTakeEmitIO venv rws lc = runVecM venv $ evalStateT (rwTakeEmit lc) rws



-- | Assign to a vector variable xa 
vectAssign :: (FExpy xa, FExpy x) => xa -> EId -> Int -> x -> Zr ()
vectAssign xa cnt arin x
 = if arin == 1 then (xa .! cnt) .:= x
   else xa .! ((cnt .* arin) :+ arin) .:= x

-- | Emit a vector variable ya
vectEmit :: FExpy ya => ya -> EId -> Int -> Zr ()
vectEmit ya cnt arout 
 = if arout == 1 then femit (ya .! cnt)
   else femit (ya .!((cnt .* arout) :+ arout)) 
