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

module AutoLUT (runAutoLUT) where

import Opts
import GenSym

import AstComp
import AstExpr
import PpExpr
import CgLUT (pprLUTStats, shouldLUT)
import Analysis.Range

import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Text.PrettyPrint.Mainland

runAutoLUT :: DynFlags -> Sym -> Comp -> IO Comp
runAutoLUT dflags _ c = autolutC c
  where
    autolutC :: Comp -> IO Comp
    autolutC (MkComp c loc inf) =
        MkComp <$> go c <*> pure loc <*> pure inf
      where
        go :: Comp0 -> IO Comp0
        go c@(Var {}) =
            pure c

        go (BindMany c args) =
            BindMany <$> autolutC c <*> mapM (\(n,c) -> (,) <$> pure n <*> autolutC c) args

        go (Seq c1 c2) =
            Seq <$> autolutC c1 <*> autolutC c2

        go (Par inf c1 c2) =
            Par inf <$> autolutC c1 <*> autolutC c2

        go (Let v c1 c2) =
            Let v <$> autolutC c1 <*> autolutC c2

        go (LetE v fi e1 c2) =
            LetE v fi <$> autolutE e1 <*> autolutC c2

        -- CL
        go (LetERef v (Just e1) c1) =
            autolutE e1 >>= \e1' ->
            LetERef v (Just e1') <$> autolutC c1

        go (LetERef v Nothing c1) =
            LetERef v Nothing <$> autolutC c1

        go (LetHeader f c) =
            LetHeader <$> autolutF f <*> autolutC c
        --

        go (LetStruct sdef c) =
            LetStruct sdef <$> autolutC c

        go (LetFunC v params locals c1 c2) =
            LetFunC v params locals <$> autolutC c1 <*> autolutC c2

        go (Call n es) =
            Call n <$> mapM autolutCallArg es

        go (Emit t e) =
            Emit t <$> autolutE e

        go (Emits t e) =
            Emits t <$> autolutE e

        go (Return t1 t2 fi e) =
            Return t1 t2 fi <$> autolutE e

        go (Interleave c1 c2) =
            Interleave <$> autolutC c1 <*> autolutC c2

        go (Branch e c1 c2) =
            Branch <$> autolutE e <*> autolutC c1 <*> autolutC c2

        go (Take1 t1 t2 ) =
            pure (Take1 t1 t2)

        go (Take t1 t2 n) =
            pure (Take t1 t2 n)

        go (Until e c) =
            Until <$> autolutE e <*> autolutC c

        go (While e c) =
            While <$> autolutE e <*> autolutC c

        go (Times ui e1 e2 v c) =
            Times ui <$> autolutE e1 <*> autolutE e2 <*> pure v <*> autolutC c

        go (Repeat n c) =
            Repeat n <$> autolutC c

        go (VectComp n c) =
            VectComp n <$> autolutC c

        go (Map p nm) =
            pure (Map p nm)

        go (Filter f) =
            pure (Filter f) 

        go (ReadSrc mty) =
            pure (ReadSrc mty)

        go (WriteSnk mty) =
            pure (WriteSnk mty)

        go (ReadInternal t buf tp) =
            pure (ReadInternal t buf tp)

        go (WriteInternal t buf) =
            pure (WriteInternal t buf)

        go (Standalone c) =
            Standalone <$> autolutC c

        go c0@(Mitigate {}) = pure c0

    autolutCallArg :: CallArg Exp Comp
                   -> IO (CallArg Exp Comp)
    autolutCallArg (CAExp e)  = autolutE e >>= \e' -> return (CAExp e')
    autolutCallArg (CAComp c) = autolutC c >>= \c' -> return (CAComp c')

    autolutE :: Exp -> IO Exp
    autolutE e_ = autoE e_

      where
        ranges :: Map EId Range
        ranges = maybe Map.empty id (varRanges e_)

        autoE e0@(MkExp _ loc inf) | Right True <- shouldLUT dflags [] ranges e0 = do
            verbose dflags $ text "Expression autolutted:" </> nest 4 (ppr e0 <> line) </>
                             case pprLUTStats dflags [] ranges e0 of
                               Nothing  -> mempty
                               Just doc -> doc
            pure $ MkExp (ELUT ranges e0) loc inf

        autoE e0@(MkExp e loc inf)
           = MkExp <$> go e <*> pure loc <*> pure inf
          where
            go :: Exp0 -> IO Exp0
            go e@(EVal {})    = pure e
            go e@(EValArr {}) = pure e
            go e@(EVar {})    = pure e

            go (EUnOp op e) =
                EUnOp op <$> autoE e

            go (EBinOp op e1 e2) =
                EBinOp op <$> autoE e1 <*> autoE e2

            -- go (EComplex e1 e2) =
            --     EComplex <$> autoE e1 <*> autoE e2

            go (EAssign e1 e2) =
                EAssign <$> autoE e1 <*> autoE e2

            go (EArrRead e1 e2 len) =
                EArrRead <$> autoE e1 <*> autoE e2 <*> pure len

            go (EArrWrite e1 e2 len e3) =
                EArrWrite <$> autoE e1 <*> autoE e2 <*> pure len <*> autoE e3

            go (EIter i j e1 e2) = do
                verbose dflags $ text "Cannot autolut loop:" </> nest 4 (ppr e0 <> line) </>
                                 nest 4 (text "Variable ranges:" </> pprRanges ranges) </>
                                 case pprLUTStats dflags [] ranges e0 of
                                   Nothing  -> mempty
                                   Just doc -> doc
                EIter i j <$> autoE e1 <*> autoE e2

            go (EFor ui i e1 e2 e3) = do
                verbose dflags $ text "Cannot autolut loop:" </> nest 4 (ppr e0 <> line) </>
                                 nest 4 (text "Variable ranges:" </> pprRanges ranges) </>
                                 case pprLUTStats dflags [] ranges e0 of
                                   Nothing  -> mempty
                                   Just doc -> doc
                EFor ui i <$> autoE e1 <*> autoE e2 <*> autoE e3

            go (EWhile e1 e2) = do
                verbose dflags $ text "Cannot autolut loop:" </> nest 4 (ppr e0 <> line) </>
                                 nest 4 (text "Variable ranges:" </> pprRanges ranges) </>
                                 case pprLUTStats dflags [] ranges e0 of
                                   Nothing  -> mempty
                                   Just doc -> doc
                EWhile <$> autoE e1 <*> autoE e2


            go (ELet v fi e1 e2) =
                ELet v fi <$> autoE e1 <*> autoE e2

            go (ELetRef v (Just e1) e2) =
                autoE e1 >>= \e1' ->
                ELetRef v (Just e1') <$> autoE e2

            go (ELetRef v Nothing e2) =
                ELetRef v Nothing <$> autoE e2


            go (ESeq e1 e2) =
                ESeq <$> autoE e1 <*> autoE e2

            go (ECall f es) =
                ECall f <$> mapM autoE es

            go (EIf e1 e2 e3) =
                EIf <$> autoE e1 <*> autoE e2 <*> autoE e3

            go (EPrint nl e) =
                EPrint nl <$> autoE e

            go (EError _t str) = pure e

            go (ELUT _ e) =
                pure $ ELUT ranges e

            go (EBPerm e1 e2) =
               autoE e1 >>= \e1' -> autoE e2 >>= \e2' -> return (EBPerm e1' e2')

            -- TODO: Revisit this, it seems defensive
            go (EStruct tn tfs) = pure e
            go (EProj _ fn)     = pure e

    autolutF :: Fun -> IO Fun
    autolutF f0@(MkFun f loc inf) =
        MkFun <$> go f <*> pure loc <*> pure inf
      where
        go :: Fun0 -> IO Fun0
        go (MkFunDefined v params locals body@(MkExp _ loc inf))
          | Right True <- shouldLUT dflags locals' ranges body = do
            verbose dflags $ text "Function autolutted:" </> nest 4 (ppr f0 <> line) <>
                             case pprLUTStats dflags locals' ranges body of
                               Nothing  -> mempty
                               Just doc -> line <> doc
            pure $ MkFunDefined v params locals (MkExp (ELUT ranges body) loc inf)

          where
            locals' = [v | (v,_) <- locals]

            ranges :: Map EId Range
            ranges = maybe Map.empty id (varRanges body)

        go (MkFunDefined v params locals body) =
            MkFunDefined v params locals <$> autolutE body

        go f@(MkFunExternal {}) =
            pure f
