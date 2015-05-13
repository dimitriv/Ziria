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
{-# LANGUAGE  QuasiQuotes, GADTs, ScopedTypeVariables, RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-unused-binds -Werror #-}

module CgExpr
  ( codeGenExp
  -- , codeGenArrRead
  -- , codeGenArrWrite
  -- , codeGenParams
  -- , codeGenParamsByRef
  -- , codeGenGlobalDeclsOnlyAlg
  -- , codeGenGlobalInitsOnly
  -- , ArrIdx ( .. )

  ) where

import Opts
import AstExpr
import AstUnlabelled
import AstComp
import PpExpr
import PpComp
import qualified GenSym as GS
import CgHeader
import CgRuntime
import CgMonad
import CgTypes
import CgLUT
import Utils
import CtExpr
import {-# SOURCE #-} CgCall

import Outputable

import Control.Applicative
import Control.Monad ( when, unless )
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.List (elemIndex, foldl', isPrefixOf )
import Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Language.C.Quote.Base (ToConst(..))
import qualified Language.C.Pretty as P
import Numeric (showHex)
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ
import Data.Maybe

import LUTAnalysis
import Analysis.DataFlow

import CgBoundsCheck
import CgValDom
import CgCmdDom
import CgPrint 

{------------------------------------------------------------------------
  Code Generation Proper
------------------------------------------------------------------------}

cgLetBind :: DynFlags -> SrcLoc -> EId -> Exp -> Cg a -> Cg a
cgLetBind dfs loc x e m = do
  ce <- cgEvalRVal dflags e1
  x_binding <- case e of 
    -- Reuse allocated space
    EValArr {} -> return ce
    _          -> do x_name <- genSym $ name x ++ getLnNumInStr loc
      let ty = ctExp e
      appendCodeGenDeclGroup x_name ty ZeroOut 
      let cx = [cexp| $id:x_name|]
      assignByVal ty cx ce
      return cx
  extendVarEnv [(x,x_binding)] m

cgMutBind :: DynFlags -> SrcLoc -> EId -> Maybe Exp -> Cg a -> Cg a
cgMutBind dfs loc x mbe m = do
  x_binding <- case mbe of 
    Just e@(EValArr {}) -> cgEvalRVal dflags e
    Just e -> do 
      x_name <- genSym $ name x ++ getLnNumInStr loc
      let ty = ctExp e
      appendCodeGenDeclGroup x_name ty ZeroOut 
      let cx = [cexp| $id:x_name|]
      cgEvalRVal dflags e >>= assignByVal ty cx
      return cx
    Nothing -> do
      x_name <- genSym $ name x ++ getLnNumInStr loc
      appendCodeGenDeclGroup x_name (nameTyp x) ZeroOut 
      return [cexp| $id:x_name|]
  extendVarEnv [(x,x_binding)] m

cgIf_ :: DynFlags -> C.Exp -> Cg () -> Cg () -> Cg ()
cgIf_ dflags ccond act1 act2 = if ccond then act1 else act2

cgIf :: DynFlags -> C.Exp -> Cg C.Exp -> Cg C.Exp -> Cg C.Exp
cgIf dflags ccond act1 act2 = do 
   condvar <- genSym "__c"
   appendDecl [cdecl| $ty:int $id:condvar;|]
   appendStmt [cstm|  $id:condvar = ccond;|]

   (e2_decls, e2_stmts, ce2) <- inNewBlock act1
   (e3_decls, e3_stmts, ce3) <- inNewBlock act2

   appendDecls e2_decls
   appendDecls e3_decls

   appendStmt [cstm|if ($id:condvar) {
                      $stms:e2_stmts
                    } else {
                      $stms:e3_stmts
                    } |]
   return [cexp| $id:condvar?$ce2:$ce3|]


codeGenExp :: DynFlags -> Exp -> Cg C.Exp
codeGenExp = cgEvalRVal  

cgEvalRVal :: DynFlags -> Exp -> Cg C.Exp
cgEvalRVal dfs e = cgEval e >>= do_ret
 where do_ret (Left lval) = cgDeref dfs lval
       do_ret (Right ce)  = return ce

cgEvalLVal :: DynFlags -> Exp -> Cg (LVal ArrIdx)
cgEvalLVal dfs e = cgEval e >>= do_ret
 where do_ret (Left lval) = return lval
       do_ret (Right ce)  = error "cgEvalLVal"

cgEval :: DynFlags -> Exp -> Cg (Either (LVal ArrIdx) C.Exp)
cgEval dfs e = go (unExp e) where
  go (EVal _ v)      = Right <$> codeGenVal v
  go (EValArr vs)    = Right <$> cgArrVal (ctExp e) vs
  go (EStruct t tes) = Right <$> cgStruct t tes 
  go (EVar x)     = Left  <$> varLVal x
  go (EUnOp u e1) = do 
     ce1 <- cgEvalRVal dfs e1
     Right <$> cgUnOp u ce1 (ctExp e1) (ctExp e)
  go (EBinOp b e1 e2) = do 
     ce1 <- cgEvalRVal dfs e1
     ce2 <- cgEvalRVal dfs e2
     Right <$> cgBinOp b ce1 (ctExp e1) ce2 (ctExp e2) (ctExp e)
  go (EAssign elhs erhs) = do
     clhs <- cgEvalLVal dfs elhs
     crhs <- cgEvalRVal dfs erhs
     cgAssign (ctExp elhs) clhs crhs 
  go (EArrWrite earr ei rng erhs) 
    = go (EAssign (EArrRead earr ei rng) erhs)
  go (EArrRead earr ei rng) 
    | EVal _t (VInt i) <- unExp ei
    , let ii :: Int = fromIntegral i
    , let aidx = ArrIdxStatic ii
    = mk_arr_read (ctExp e) (ctExp earr) aidx
    | otherwise 
    = do aidx <- ArrIdxCExp <$> cgEvalRVal dfs ei
         mk_arr_read (ctExp e) (ctExp earr) aidx
    where 
      mk_arr_read exp_ty arr_ty aidx = do 
        d <- cgEval dfs earr
        cgBoundsCheck dflags (expLoc e0) arr_ty (cexpOfArrIdx aidx) r
        case d of 
          Left lval -> Left  <$> GDArr lval aidx rng
          Right ce  -> Right <$> cgArrRead (ctExp e) ce (ctExp earr) aidx rng 
  go (EProj e0 f) = do 
    d <- cgEval dfs e0
    case d of 
      Left lval -> Left  <$> GDProj lval f
      Right ce  -> return $ Right (cgStrProj (ctExp e) ce (ctExp e0) f
  go (ELet x _fi e1 e2)
     = cgLetBind dfs x e1 (cgEvalRVal e2 >>= (return . Right))
  go (ELetRef x mb_e1 e2)
     = cgMutBind dfs x mb_e1 (cgEvalRVal e2 >>= (return . Right))
  go (ESeq e1 e2) = do
      _ce1 <- cgEval dfs e1
      Right <$> cgEvalRVal dflags e2
  go (EIf e1 e2 e3) = do
     ce1 <- cgEvalRVal dfs e1
     Right <$> cgIf ce1 (cgEvalRVal dfs e2) (cgEvalRVal dfs e3)
  go (EPrint nl e1s) = do 
      printExps nl dflags e1s
      Right <$> return [cexp|UNIT|]
  go (EError ty str) = do
      appendStmt [cstm|printf("RUNTIME ERROR: %s\n", $str);|]
      appendStmt [cstm|exit(1);|]
      -- We allocate a new variable of the appropriate type just
      -- because error is polymorphic and this is a cheap way to 
      -- return "the right type"
      x <- genSym "__err"
      appendCodeGenDeclGroup x ty ZeroOut
      Right <$> return [cexp|$id:x|]
  
  go (ELUT r e1) 
     | isDynFlagSet dflags NoLUT 
     = codeGenExpAlloc dflags e1
     | isDynFlagSet dflags MockLUT
     = cgPrintVars dflags loc (lutVarUsePkg r) $
       codeGenExpAlloc dflags e1
     | otherwise
     = cgPrintVars dflags loc (lutVarUsePkg r) $
       codeGenLUTExp dflags r e1 Nothing

  go (EWhile econd ebody) = do
      (init_decls,init_stms,cecond) <- inNewBlock (cgEvalRVal dflags econd)
      (body_decls,body_stms,cebody) <- inNewBlock (cgEvalRVal dflags ebody)

      appendDecls init_decls
      appendDecls body_decls
      appendStmts init_stms

      appendStmt [cstm|while ($cecond) {
                         $stms:body_stms
                         $stms:init_stms
                  }|]
      Right <$> return [cexp|UNIT|]

  go (EFor _ui k estart elen ebody) = do

      k_new <- freshName (name k) (ctExp estart) Imm

      (init_decls, init_stms, (ceStart, ceLen)) <- inNewBlock $ do
          ceStart <- cgEvalRVal dflags estart
          ceLen   <- cgEvalRVal dflags elen
          return (ceStart, ceLen)

      (body_decls, body_stms, cebody) <- inNewBlock $
          extendVarEnv [(k, [cexp|$id:(name k_new)|])] $
          cgEvalRVal dflags ebody

      appendDecls init_decls
      appendDecls body_decls
      appendStmts init_stms
      let idx_ty = codeGenTyOcc (ctExp estart)
      appendStmt [cstm|for ($ty:(idx_ty) $id:(name k_new) = $ceStart; 
                            $id:(name k_new) < ($ceStart + $ceLen); 
                            $id:(name k_new)++) {
                         $stms:init_stms
                         $stms:body_stms
                       }|]
      Right <$> return [cexp|UNIT|]

    go (ECall nef eargs) = Right <$> cgCall dfs (expLoc e0) (ctExp e) nef eargs


cgCall :: DynFlags -> SrcLoc -> Ty -> EId -> [Exp] -> Cg C.Exp
cgCall dfs loc res_ty fn eargs = do 
  let (TArrow funtys _funres) = nameTyp fn
  let tys_args = zip funtys eargs
  ceargs <- mapM cgEvalArg tys_args
  error "IMPLEMENT ME!" 


cgEvalArg :: (ArgTy, Exp) -> Cg (Either (LVal ArrIdx) C.Exp
cgEvalArg (GArgTy _ Mut, earg) = Left  <$> cgEvalLVal earg -- ^ Force mutable
cgEvalArg (_, earg)            = Right <$> cgEvalRVal earg -- ^ Fully evaluate
 

------------------------------------------------------------------------------
-- | Generation of parameter signatures and argument lists
------------------------------------------------------------------------------

codeGenParamByRef :: EId -> Cg [C.Param]
codeGenParamByRef nm = codegen_param_byref nm (nameTyp nm)

codegen_param_byref nm ty@(TArray (Literal l) _) = do
    unused <- freshVar ("__unused_")
    return [cparams|$ty:(codeGenTy ty) $id:(name nm), int $id:unused|]

codegen_param_byref nm ty@(TArray (NVar c) _) =
    return [cparams|$ty:(codeGenTy ty) $id:(name nm), int $id:c|]

codegen_param_byref nm ty = do
    return [cparams|$ty:(tyByRef ty) $id:(name nm)|]
  where
    tyByRef :: Ty -> C.Type
    tyByRef ty
        | isArrayTy ty = codeGenTy ty
        | otherwise    = [cty|$ty:(codeGenTy ty)*|]

codeGenParamsByRef :: [EId] -> Cg [C.Param]
codeGenParamsByRef params = concat <$> mapM codeGenParamByRef params

codeGenParam :: EId -> Cg [C.Param]
codeGenParam nm
  = codegen_param nm (nameTyp nm)

codegen_param nm (ty@(TArray (Literal l) _)) = do
     unused <- freshVar ("__unused_")
     let pname = getNameWithUniq nm
     return [cparams|$ty:(codeGenTy ty) $id:pname, int $id:unused|]

codegen_param nm (ty@(TArray (NVar c) _)) =
     let pname = getNameWithUniq nm
     in
     return [cparams|$ty:(codeGenTy ty) $id:pname, int $id:c|]

codegen_param nm ty
  = do { b <- isStructPtrType ty
       ; let pname = getNameWithUniq nm
       ; return $
         -- NB: b = False applies to ordinary arrays
         if b then [cparams|$ty:(codeGenTy ty) * $id:pname |]
              else [cparams|$ty:(codeGenTy ty) $id:pname  |]
       }

codeGenParams :: [EId] -> Cg [C.Param]
codeGenParams prms = go prms []
  where go [] acc = return []
        go (nm:rest) acc
          | ty@(TArray (NVar c) tybase) <- nameTyp nm
          , c `elem` acc
          =  do { c' <- freshVar c
                  -- NB: avoiding binding the same length var twice
                ; ps  <- codegen_param nm (TArray (NVar c') tybase)
                ; ps' <- go rest acc
                ; return (ps ++ ps')
                }
          | ty@(TArray (NVar c) tybase) <- nameTyp nm
          = do { ps <- codeGenParam nm
               ; ps' <- go rest (c:acc)
               ; return (ps ++ ps') }
        go (other:rest) acc
          = do { ps <- codeGenParam other
               ; ps' <- go rest acc
               ; return (ps ++ ps')
               }

------------------------------------------------------------------------------
-- | Declarations and Globals
------------------------------------------------------------------------------

-- ^ Only declare globals
codeGenGlobalDeclsOnlyAlg :: DynFlags
                          -> [(EId, Maybe Exp)]
                          -> Cg [C.Definition]
codeGenGlobalDeclsOnlyAlg dflags = mapM $ \(nm, me) ->
  codeGenDeclDef (name nm) (nameTyp nm)

-- ^ Only initialize globals
codeGenGlobalInitsOnly :: DynFlags -> [MutVar] -> Cg ()
codeGenGlobalInitsOnly dflags defs = mapM_ go defs
  where
    go :: MutVar -> Cg ()
    go MutVar{..} =
      case mutInit of
        Just e -> do
          ce <- codeGenExp dflags e
          assignByVal (ctExp e) (ctExp e) [cexp|$id:(name mutVar)|] ce
        Nothing ->
          return ()

