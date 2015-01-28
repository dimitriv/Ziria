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
module VecRewriter where

import Control.Applicative

import Text.Parsec.Pos ( SourcePos )

import AstComp
import AstExpr
import AstFM
import AstUnlabelled

import CardAnalysis
import CtComp
import CtExpr

import Outputable
import Text.PrettyPrint.HughesPJ

import VecM

{-------------------------------------------------------------------------
  Vectorizer rewriter of simple computers 
-------------------------------------------------------------------------}



-- | Type-preserving transformations on the take and emit nodes of a computation
-- NB: Does not include ReadSrc/WritSnk/ReadInternal/WriteInternal nodes as these
-- can't currently be expressed as just a set of repeated computers. 
rewriteTakeEmit ::
     (Maybe SourcePos -> Ty -> VecM Comp)         -- on Take1
  -> (Maybe SourcePos -> Ty -> Int -> VecM Comp)  -- on Take
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emit
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emits
  -> LComp -> VecM Comp
rewriteTakeEmit on_take on_takes on_emit on_emits = go
  where 
    go lcomp = 
      let loc = compLoc lcomp in 
      case unComp lcomp of 
        -- Standard boilerplate 
        Var x -> lookupCVarBind x >>= go 

        BindMany c1 xs_cs -> do
          c1' <- go c1
          let go_one (x,c) = go c >>= \c' -> return (x,c')
          xs_cs' <- mapM go_one xs_cs
          return $ cBindMany loc c1' xs_cs'
        Let x c1 c2        -> extendCVarBind x c1 (go c2)
        LetE x fi e c1     -> cLetE loc x fi e    <$> go c1
        LetERef x me c1    -> cLetERef loc x me   <$> go c1
        LetHeader fun c1   -> cLetHeader loc fun  <$> go c1
        LetStruct sdef c1  -> cLetStruct loc sdef <$> go c1
        LetFunC f params c1 c2
          -> extendCFunBind f params c1 $ go c2
        Branch e c1 c2 -> do
          c1' <- go c1
          c2' <- go c2
          return $ cBranch loc e c1' c2'
        Standalone c1 -> cStandalone loc <$> go c1
        Return fi e   -> return $ cReturn loc fi e 
        Seq c1 c2 -> do
          c1' <- go c1
          c2' <- go c2
          return $ cSeq loc c1' c2'
        Call f es -> do 
          (CFunBind { cfun_params = prms
                    , cfun_body = body }) <- lookupCFunBind f
          vbody <- go body
          let vf_type = CTArrow (map nameCallArgTy prms) (ctComp vbody)
          vf <- newVectGName (name f ++ "_spec") vf_type loc Imm
          return $ cLetFunC loc vf prms vbody $ 
                   cCall loc vf (map eraseCallArg es)

        -- Interesting cases
        Take1 t  -> on_take loc t
        Take t n -> on_takes loc t n 
        Emit e   -> on_emit loc e
        Emits es -> on_emits loc es

        Par pinfo c1 c2 -> do 
          -- Note we don't rewrite the intermediates!
          c1' <- rewriteTakeEmit on_take on_takes mEmit mEmits c1
          c2' <- rewriteTakeEmit mTake1 mTake on_emit on_emits c2
          return $ cPar loc pinfo c1' c2'

        Map vann f -> do 
          let inty = inTyOfCTy $ ctComp lcomp
          c <- map2take_emit loc vann inty f
          go c

        Until e c           -> cUntil loc e <$> go c
        While e c           -> cWhile loc e <$> go c
        Times ui e elen n c -> cTimes loc ui e elen n <$> go c
        Repeat vann c       -> cRepeat loc vann <$> go c

        VectComp ann c -> cVectComp loc ann <$> go c

        -- Other non-simple computers
        _other -> 
          vecMFail loc $ text "Not a simple computer:" <+> ppr lcomp
        

mTake1 :: Monad m => Maybe SourcePos -> t -> m (GComp tc t () ())
mTake1 loc t = return $ cTake1 loc t

mTake :: Monad m => Maybe SourcePos -> t -> Int -> m (GComp tc t () ())
mTake loc t n = return $ cTake loc t n

mEmit :: Monad m => Maybe SourcePos -> GExp t () -> m (GComp tc t () ())
mEmit loc e = return $ cEmit loc e

mEmits :: Monad m => Maybe SourcePos -> GExp t () -> m (GComp tc t () ())
mEmits loc e = return $ cEmits loc e

rwTakeEmitIO :: VecEnv -> 
     (Maybe SourcePos -> Ty -> VecM Comp)         -- on Take1
  -> (Maybe SourcePos -> Ty -> Int -> VecM Comp)  -- on Take
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emit
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emits
  -> LComp -> IO Comp
rwTakeEmitIO venv t1 t2 e1 e2 lc 
  = runVecM venv $ rewriteTakeEmit t1 t2 e1 e2 lc

{-------------------------------------------------------------------------
  Helpers for rewriting take and emit
-------------------------------------------------------------------------}

rw_take :: Int -> EId -> EId -> EId -> Maybe SourcePos -> Ty -> VecM Comp
rw_take arin iidx tidx xa loc _ty
  | arin == 1 = liftZr loc $ freturn _fI xa
  | otherwise = liftZr loc $ do
      tidx  .:= iidx
      iidx  .:= iidx .+ I(1)
      freturn _aI $ xa .! tidx -- Auto inline, not force-inline

rw_takes :: EId -> EId -> EId -> Maybe SourcePos -> Ty -> Int -> VecM Comp
rw_takes iidx tidx xa loc _ty n
  = liftZr loc $ do
      tidx  .:= iidx
      iidx  .:= iidx .+ I(n)
      freturn _aI $ xa .! (tidx :+ n)

rw_emit :: Int -> EId -> EId -> EId -> Maybe SourcePos -> Exp -> VecM Comp
rw_emit arout oidx tidx ya loc e
   -- there can be only one emit so this must be it
  | arout == 1 = liftZr loc $ femit e
  | otherwise  = liftZr loc $ do
      tidx .:= oidx
      oidx .:= oidx .+ I(1)
      ya .! tidx .:= e

rw_emits :: EId -> EId -> EId -> Maybe SourcePos -> Exp -> VecM Comp
rw_emits oidx tidx ya loc es
  = liftZr loc $ do
      tidx .:= oidx
      oidx .:= oidx .+ I(len)
      ya .! (tidx :+ len) .:= es
  where TArray (Literal len) _ = ctExp es
