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

runAutoLUT :: forall a . DynFlags -> Sym -> Comp a Ty -> IO (Comp a Ty)
runAutoLUT dflags _ c = autolutC c
  where
    autolutC :: Comp a Ty -> IO (Comp a Ty)
    autolutC (MkComp c loc inf) =
        MkComp <$> go c <*> pure loc <*> pure inf
      where
        go :: Comp0 a Ty -> IO (Comp0 a Ty)
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

        go (LetE v e1 c2) =
            LetE v <$> autolutE e1 <*> autolutC c2

        go (LetExternal v f c) =
            LetExternal v <$> autolutF f <*> autolutC c

        go (LetFun v f c) =
            LetFun v <$> autolutF f <*> autolutC c


        go (LetStruct sdef c) =
            LetStruct sdef <$> autolutC c


        go (LetFunC v params locals c1 c2) =
            LetFunC v params locals <$> autolutC c1 <*> autolutC c2

        go (Call n es) =
            Call n <$> mapM autolutCallArg es

        go (Emit e) =
            Emit <$> autolutE e

        go (Emits e) =
            Emits <$> autolutE e

        go (Return e) =
            Return <$> autolutE e

        go (Interleave c1 c2) =
            Interleave <$> autolutC c1 <*> autolutC c2

        go (Branch e c1 c2) =
            Branch <$> autolutE e <*> autolutC c1 <*> autolutC c2

        go Take1 =
            pure Take1

        go (Take e) =
            Take <$> autolutE e

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

        go (Map p e) =
            Map p <$> autolutE e

        go (Filter e) =
            Filter <$> autolutE e

        go (ReadSrc mty) =
            pure (ReadSrc mty)

        go (WriteSnk mty) =
            pure (WriteSnk mty)

        go (ReadInternal buf tp) =
            pure (ReadInternal buf tp)

        go (WriteInternal buf) =
            pure (WriteInternal buf)

        go (Standalone c) =
            Standalone <$> autolutC c

    autolutCallArg :: CallArg (Exp Ty) (Comp a Ty) -> IO (CallArg (Exp Ty) (Comp a Ty))
    autolutCallArg (CAExp e)  = autolutE e >>= \e' -> return (CAExp e')
    autolutCallArg (CAComp c) = autolutC c >>= \c' -> return (CAComp c')
   
    autolutE :: Exp Ty -> IO (Exp Ty)
    autolutE e_ = do { putStrLn $ "Trying to autolutE expression: " ++ show e_
                     ; autoE e_
                     }
      where
        ranges :: Map Name Range
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
            go :: Exp0 Ty -> IO (Exp0 Ty)
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


            go (ELet v e1 e2) =
                ELet v <$> autoE e1 <*> autoE e2

            go (ELetRef v (Right e1) e2) =
                autoE e1 >>= \e1' -> 
                ELetRef v (Right e1') <$> autoE e2

            go (ELetRef v (Left t) e2) =
                ELetRef v (Left t) <$> autoE e2


            go (ESeq e1 e2) =
                ESeq <$> autoE e1 <*> autoE e2

            go (ECall f es) =
                ECall <$> autoE f <*> mapM autoE es

            go (EIf e1 e2 e3) =
                EIf <$> autoE e1 <*> autoE e2 <*> autoE e3

            go (EPrint nl e) =
                EPrint nl <$> autoE e

            go (EError str) = pure e

            go (ELUT _ e) =
                pure $ ELUT ranges e

            go (EBPerm e1 e2) = 
               autoE e1 >>= \e1' -> autoE e2 >>= \e2' -> return (EBPerm e1' e2')

            -- TODO: Revisit this, it seems defensive
            go (EStruct tn tfs) = pure e
            go (EProj _ fn)     = pure e

    autolutF :: Fun Ty -> IO (Fun Ty)
    autolutF f0@(MkFun f loc inf) =
        MkFun <$> go f <*> pure loc <*> pure inf
      where
        go :: Fun0 Ty -> IO (Fun0 Ty)
        go (MkFunDefined v params locals body@(MkExp _ loc inf)) 
          | Right True <- shouldLUT dflags locals' ranges body = do
            verbose dflags $ text "Function autolutted:" </> nest 4 (ppr f0 <> line) <>
                             case pprLUTStats dflags locals' ranges body of
                               Nothing  -> mempty
                               Just doc -> line <> doc
            pure $ MkFunDefined v params locals (MkExp (ELUT ranges body) loc inf)

          where
            locals' = [(v,ty) | (v,ty,_) <- locals]
            
            ranges :: Map Name Range
            ranges = maybe Map.empty id (varRanges body)

        go (MkFunDefined v params locals body) =
            MkFunDefined v params locals <$> autolutE body

        go f@(MkFunExternal {}) =
            pure f
