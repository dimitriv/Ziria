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
module PassFold (runFold, elimMitigsIO) where

import Prelude hiding (exp)
import Control.Arrow (second)
import Control.Monad.Reader
import qualified Data.Map as Map

import AstComp
import AstExpr
import Opts
import PpComp ()
import PpExpr ()
import qualified GenSym as GS

import PassFoldM
import PassComp
import PassExpr
import PassOptMit


runTypedCompPass :: TypedCompPass -> Comp -> RwM Comp
runTypedCompPass (TypedCompBottomUp f) =
    mapCompM return return return return return f'
  where
    f' comp = f (compLoc comp) comp

runTypedExpPass :: TypedExpPass -> Comp -> RwM Comp
runTypedExpPass (TypedExpBottomUp f) =
    mapCompM return return return return (mapExpM return return f') return
  where
    f' exp = f (expLoc exp) exp
runTypedExpPass (TypedExpManual f) =
    -- We apply the function to each expression, but we don't use mapExpM
    mapCompM return return return return f return

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
      ((comp', rewritten), time) <- measure $ runRwM (p comp) (sym, flags)
      let mp' = incInvokes mp time pn
      case rewritten of
        NotRewritten -> do
          go b mp' ps comp'
        Rewritten -> do
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
    [ ("elim-seq"      , elimSeq         )
    , ("fold"          , passFold        )
    , ("purify"        , passPurify      )
    , ("purify-letref" , passPurifyLetRef)
    , ("elim-times"    , passElimTimes   )
    , ("letfunc"       , passLetFunc     )
    , ("letfun-times"  , passLetFunTimes )
    , ("times-unroll"  , passTimesUnroll )
    , ("inline"        , passInline      )

    -- More aggressive optimizations
    , ("push-comp-locals"       , passPushCompLocals      )
    , ("take-emit"              , passTakeEmit            )
    , ("float-letfun-repeat"    , passFloatLetFunRepeat   )
    , ("float-let-par"          , passFloatLetPar         )
    , ("ifdead"                 , passIfDead              )
    , ("if-return"              , passIfReturn            )
    , ("elim-automapped-mitigs" , passElimAutomappedMitigs)
    

    -- Don't use: not wrong but does not play nicely with LUT
    --  , ("float-top-letref"   , passFloatTopLetRef )
    ]

foldExpPasses :: DynFlags -> [(String,TypedExpPass)]
foldExpPasses flags
  | isDynFlagSet flags NoFold || isDynFlagSet flags NoExpFold
  = []
  | otherwise
  = [ ("for-unroll"         , passForUnroll  )
    , ("elim-unused"        , passElimUnused )
    , ("exp-inlining"       , passExpInlining)
    , ("asgn-letref"        , passAsgnLetRef )
    , ("exp-let-push"       , passExpLetPush )
    , ("elim-trivial-assign", passElimTrivialAssign)

    ] ++ 
    [ ("eval"         , passEval       ) 
    | not (isDynFlagSet flags NoStaticEval)
    ]


