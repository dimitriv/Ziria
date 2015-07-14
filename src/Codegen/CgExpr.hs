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
{-# OPTIONS_GHC -fwarn-unused-binds #-}

module CgExpr ( codeGenExp, cgMutBind ) where

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
import CgCall

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
import qualified Data.Set as S
import Text.PrettyPrint.HughesPJ
import Data.Maybe

import LUTAnalysis
import Analysis.DataFlow

import CgBoundsCheck
import CgValDom
import CgCmdDom
import CgPrint 
import CgCall

import Control.Monad ( zipWithM )

{------------------------------------------------------------------------
  Code Generation Proper
------------------------------------------------------------------------}

cgLetBind :: DynFlags -> SrcLoc -> EId -> Exp -> Cg a -> Cg a
cgLetBind dfs loc x e m = do
  ce <- cgEvalRVal dfs e
  case unExp e of 
    -- Reuse allocated space if we will not be mutating this (let bound)
    EValArr {} -> extendVarEnv [(x,ce)] m
    _          -> do 
      x_name <- genSym $ name x ++ getLnNumInStr loc
      let ty = ctExp e
      appendCodeGenDeclGroup x_name ty ZeroOut   
      let cx = [cexp| $id:x_name|]
      assignByVal ty cx ce
      extendVarEnv [(x,cx)] m

cgMutBind :: DynFlags -> SrcLoc -> EId -> Maybe Exp -> Cg a -> Cg a
cgMutBind dfs loc x mbe m = do
  case mbe of 
    -- NB: the following code is plain wrong because of mutation!
    -- We can only do something like this if we have an immutable
    -- let-binding (see above)
    -- 
    -- Just e@(MkExp (EValArr {}) _ _) -> do 
    --     x_binding <- cgEvalRVal dfs e
    --     extendVarEnv [(x,x_binding)] m

    Just e -> do 
      x_name <- genSym $ name x ++ getLnNumInStr loc
      let ty = ctExp e
      ce <- cgEvalRVal dfs e
      appendCodeGenDeclGroup x_name ty ZeroOut 
      let cx = [cexp| $id:x_name|]
      assignByVal ty cx ce
      let x_binding = cx
      extendVarEnv [(x,x_binding)] m

    Nothing -> do
      x_name <- genSym $ name x ++ getLnNumInStr loc
      appendCodeGenDeclGroup x_name (nameTyp x) ZeroOut
      let x_binding = [cexp| $id:x_name|]
      extendVarEnv [(x,x_binding)] m


cgIf :: DynFlags -> C.Exp -> Cg C.Exp -> Cg C.Exp -> Cg C.Exp
cgIf _dfs ccond act1 act2 = do
   condvar <- genSym "__c"
   appendDecl [cdecl| int $id:condvar;|]
   appendStmt [cstm|  $id:condvar = $ccond;|]

   (e2_decls, e2_stmts, ce2) <- inNewBlock act1
   (e3_decls, e3_stmts, ce3) <- inNewBlock act2

   appendDecls e2_decls
   appendDecls e3_decls

   unless (null e2_stmts && null e3_stmts) $ 
      appendStmt [cstm|if ($id:condvar) {
                         $stms:e2_stmts
                       } else {
                         $stms:e3_stmts
                       } |]

   return [cexp| $id:condvar?$ce2:$ce3|]


codeGenExp :: DynFlags -> Exp -> Cg C.Exp
codeGenExp = cgEvalRVal  

cgEvalRVal :: DynFlags -> Exp -> Cg C.Exp
cgEvalRVal dfs e = cgEval dfs e >>= do_ret
 where do_ret (Left lval) = cgDeref dfs (expLoc e) lval
       do_ret (Right ce)  = return ce

cgEvalLVal :: DynFlags -> Exp -> Cg (LVal ArrIdx)
cgEvalLVal dfs e = cgEval dfs e >>= do_ret
 where do_ret (Left lval) = return lval
       do_ret (Right ce)  = error "cgEvalLVal"


cgIdx :: DynFlags -> Exp -> Cg ArrIdx
cgIdx dfs x = cg_idx (classify x)
  where
   classify :: Exp -> GArrIdx Exp
   classify ei
     | EVal _t (VInt i _) <- unExp ei
     , let ii :: Int = fromIntegral i
     = AIdxStatic ii
     | EBinOp bop e1 e2 <- unExp ei
     = case (classify e1, classify e2, bop) of
         (AIdxStatic i1, AIdxMult i2 c2, Mult) -> AIdxMult (i1*i2) c2
         (AIdxStatic i1, AIdxCExp c2, Mult)    -> AIdxMult i1 c2
         (AIdxMult i2 c2, AIdxStatic i1, Mult) -> AIdxMult (i1*i2) c2
         (AIdxCExp c2, AIdxStatic i1, Mult)    -> AIdxMult i1 c2
         _ -> AIdxCExp ei
     | otherwise = AIdxCExp ei

   cg_idx (AIdxCExp e)   = AIdxCExp <$> cgEvalRVal dfs e
   cg_idx (AIdxMult i e) = AIdxMult i <$> cgEvalRVal dfs e
   cg_idx (AIdxStatic i) = return $ AIdxStatic i


cgEval :: DynFlags -> Exp -> Cg (Either (LVal ArrIdx) C.Exp)
cgEval dfs e = go (unExp e) where
  loc = expLoc e


  -- Any variable is byte aligned 
  is_lval_aligned (GDVar {}) _ = True 
  -- Any non-bit array deref exp is not aligned (NB: we could improve that)
  is_lval_aligned _ TBit            = False
  is_lval_aligned _ (TArray _ TBit) = False

  -- Any other 
  is_lval_aligned _ _ = True


  go_assign, asgn_slow :: DynFlags -> SrcLoc -> LVal ArrIdx -> Exp -> Cg C.Exp
  go_assign dfs loc clhs erhs 
    | is_lval_aligned clhs (ctExp erhs)    -- if lval is aligned
    , ECall nef eargs <- unExp erhs        -- and rval is function call

    -- Fast, but unsafe in case of aliasing ... 
    -- = do clhs_c <- cgDeref dfs loc clhs
    --      _ <- cgCall_aux dfs loc (ctExp erhs) nef eargs (Just clhs_c)
    --      return [cexp|UNIT|]

    -- Somewhat unsatisfactory compromise: for external functions make it 
    -- the programmer's responsibility to control aliasing effects. 
    -- See note [Safe Return Aliasing] in AstExpr.hs

    = do (fn,clos,_) <- lookupExpFunEnv nef -- Get closure arguments
         let is_external = isPrefixOf "__ext" (name fn)
         if is_external then do clhs_c <- cgDeref dfs loc clhs
                                _ <- cgCall_aux dfs loc (ctExp erhs) nef eargs (Just clhs_c)
                                return [cexp|UNIT|]
         else asgn_slow dfs loc clhs erhs

    | otherwise = asgn_slow dfs loc clhs erhs 
  asgn_slow dfs loc clhs erhs = do 
         mrhs <- cgEval dfs erhs
         case mrhs of
           Left rlval -> do
             -- | lvalAlias_exact clhs rlval -> do 
             --     crhs <- cgDeref dfs loc rlval
             --     cgAssignAliasing dfs loc clhs crhs
             -- | otherwise -> do 
                 crhs <- cgDeref dfs loc rlval
                 cgAssign dfs loc clhs crhs
           Right crhs -> cgAssign dfs loc clhs crhs
         -- crhs <- cgEvalRVal dfs erhs
         -- cgAssign dfs loc clhs crhs
         return [cexp|UNIT|]
             

  go :: Exp0 -> Cg (Either (LVal ArrIdx) C.Exp) 

  go (EVar x)   = return (Left (GDVar x))
  go (EVal _ v) = return (Right (codeGenVal v)) 

  go (EValArr vs)    = Right <$> cgArrVal dfs loc (ctExp e) vs
  go (EStruct t tes) = do 
      ctes <- mapM (\(f,ef) -> 
        do cef <- cgEvalRVal dfs ef 
           return (f,cef)) tes
      Right <$> cgStruct dfs loc t ctes

  go (EUnOp u e1) = do 
     ce1 <- cgEvalRVal dfs e1
     Right <$> return (cgUnOp (ctExp e) u ce1 (ctExp e1))

  go (EBinOp b e1 e2) = do 
     ce1 <- cgEvalRVal dfs e1
     ce2 <- cgEvalRVal dfs e2
     Right <$> return (cgBinOp (ctExp e) b ce1 (ctExp e1) ce2 (ctExp e2))

  go (EAssign elhs erhs) = do
    clhs <- cgEvalLVal dfs elhs
    Right <$> go_assign dfs loc clhs erhs 

  go (EArrWrite earr ei rng erhs) 
    = go (EAssign (eArrRead loc earr ei rng) erhs)

  go (EArrRead earr ei rng) = do
    aidx <- cgIdx dfs ei 
    mk_arr_read (ctExp e) (ctExp earr) aidx

    -- | EVal _t (VInt i Signed) <- unExp ei
    -- , let ii :: Int = fromIntegral i
    -- , let aidx = AIdxStatic ii
    -- = mk_arr_read (ctExp e) (ctExp earr) aidx
    -- | otherwise 
    -- = do aidx <- AIdxCExp <$> cgEvalRVal dfs ei
    --      mk_arr_read (ctExp e) (ctExp earr) aidx
    where 
      mk_arr_read exp_ty arr_ty aidx = do 
        d <- cgEval dfs earr
        case d of 
          Left lval -> return (Left (GDArr lval aidx rng))
          Right ce 
            -> Right <$> cgArrRead dfs loc exp_ty ce arr_ty aidx rng 

  go (EProj e0 f) = do 
    d <- cgEval dfs e0
    case d of 
      Left lval -> Left  <$> return (GDProj lval f)
      Right ce  -> Right <$> return (cgStrProj (ctExp e) ce (ctExp e0) f)

  go elet@(ELet x _fi e1 e2)
     = cgLetBind dfs loc x e1 (cgEvalRVal dfs e2 >>= (return . Right))

  go (ELetRef x mb_e1 e2)
     = cgMutBind dfs loc x mb_e1 (cgEvalRVal dfs e2 >>= (return . Right))

  go (ESeq e1 e2) = do
      _ce1 <- cgEval dfs e1
      Right <$> cgEvalRVal dfs e2

  go (EIf e1 e2 e3) = do
     ce1 <- cgEvalRVal dfs e1
     Right <$> cgIf dfs ce1 (cgEvalRVal dfs e2) (cgEvalRVal dfs e3)

  go (EPrint nl e1s) = do 
      printExps codeGenExp nl dfs e1s
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
     | isDynFlagSet dfs NoLUT 
     = cgEval dfs e1
     | isDynFlagSet dfs MockLUT
     = cgPrintVars dfs loc (lutVarUsePkg r) $
       cgEval dfs e1
     | otherwise
     = Right <$> codeGenLUTExp codeGenExp dfs r e1 Nothing

  go (EWhile econd ebody) = do
      (init_decls,init_stms,cecond) <- inNewBlock (cgEvalRVal dfs econd)
      (body_decls,body_stms,cebody) <- inNewBlock (cgEvalRVal dfs ebody)

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
          ceStart <- cgEvalRVal dfs estart
          ceLen   <- cgEvalRVal dfs elen
          return (ceStart, ceLen)

      (body_decls, body_stms, cebody) <- inNewBlock $
          extendVarEnv [(k, [cexp|$id:(name k_new)|])] $ cgEvalRVal dfs ebody

      appendDecls init_decls
      appendDecls body_decls
      appendStmts init_stms
      let idx_ty = codeGenTyOcc (ctExp estart)

      appendDecl [cdecl| $ty:(idx_ty) $id:(name k_new);|]
      appendStmt [cstm| for ($id:(name k_new) = $ceStart; 
                            $id:(name k_new) < ($ceStart + $ceLen); 
                            $id:(name k_new)++) {
                         $stms:init_stms
                         $stms:body_stms
                       }|]
      Right <$> return [cexp|UNIT|]

  go (ECall nef eargs) = Right <$> cgCall_aux dfs loc (ctExp e) nef eargs Nothing


cgPrintVars :: DynFlags -> SrcLoc -> VarUsePkg -> Cg a -> Cg a
cgPrintVars dflags loc vpkg action = do
  cg_print_vars dflags "Invars" loc (vu_invars vpkg)
  r <- action
  cg_print_vars dflags "Outvars" loc (vu_outvars vpkg)
  return r

cg_print_vars :: DynFlags -> String -> SrcLoc -> [EId] -> Cg ()
cg_print_vars dflags dbg_ctx loc vs
  | isDynFlagSet dflags Verbose
  = do appendStmt $ [cstm| printf("%s> cg_print_vars: %s\n", 
                             $string:(dbg_ctx), 
                             $string:(displayLoc (locOf loc)));|]
       sequence_ $ map (codeGenExp dflags . print_var) vs
  | otherwise
  = return ()
  where
    preamb = dbg_ctx ++ ">   "
    print_var v = ePrint loc True $ 
                  [ eVal loc TString (VString (preamb ++ show v ++ ":"))
                  , eVar loc v ]

cgCall_aux :: DynFlags -> SrcLoc -> Ty -> EId -> [Exp] -> Maybe C.Exp -> Cg C.Exp
cgCall_aux dfs loc res_ty fn eargs mb_ret = do 
  let funtys = map ctExp eargs
  let (TArrow formal_argtys _) = nameTyp fn
  let argfuntys = zipWith (\(GArgTy _ m) t -> GArgTy t m) formal_argtys funtys
  let tys_args = zip argfuntys eargs
 
  ceargs <- mapM (cgEvalArg dfs) tys_args
  CgCall.cgCall dfs loc res_ty argfuntys fn ceargs mb_ret


  -- extendVarEnv [(retn,cretn)] $
  --    CgCall.cgCall dfs loc res_ty argfuntys fn ceargs cretn


cgEvalArg :: DynFlags -> (ArgTy, Exp) -> Cg (Either (LVal ArrIdx) C.Exp)
cgEvalArg dfs (GArgTy _ Mut, earg) = Left <$> cgEvalLVal dfs earg 
cgEvalArg dfs (_, earg) = Right <$> cgEvalRVal dfs earg
 

{- Code generation for array values (belonging in ValDom in AbsInt.hs but 
   implemented here, due to some performance-oriented specialization
 ---------------------------------------------------------------------------------}

-- | Here we deviate a bit from AbsInt.hs and the reason is the
--   special-case code for value arrays versus array expressions that
--   contain expressions.
cgArrVal :: DynFlags -> SrcLoc -> Ty -> [Exp] -> Cg C.Exp
cgArrVal dfs loc arr_ty es 
  | Just vs <- expValMbs es = cgArrVal_val dfs loc arr_ty vs 
  | otherwise               = cgArrVal_exp dfs loc arr_ty es

cgArrVal_val :: DynFlags -> SrcLoc -> Ty -> [Val] -> Cg C.Exp
cgArrVal_val _ _ t@(TArray _ TBit) vs = do
   snm <- freshName "__bit_arr" t Mut
   let csnm = [cexp| $id:(name snm)|]
   let inits = cgBitValues vs
   appendCodeGenDeclGroup (name snm) t (InitWith inits)
   return csnm

cgArrVal_val _dfs loc t@(TArray _ _) ws
  | length ws <= 8192 
  = do snm <- freshName "__val_arr" t Mut
       let csnm = [cexp| $id:(name snm)|]
       let inits = cgNonBitValues ws
       appendCodeGenDeclGroup (name snm) t (InitWith inits)
       return csnm
  | otherwise -- ^ very large initializer, VS can't deal with that
  = go t ws
  where 
    go (TArray n tval) vs 
      | length vs <= 8192
      = do snm <- freshName "__val_arr" t Mut
           let csnm = [cexp| $id:(name snm)|]
           let inits = cgNonBitValues vs
           DeclPkg d istms <- 
               codeGenDeclGroup (name snm) (TArray n tval) (InitWith inits)
           appendTopDecl d
           mapM_ addGlobalWplAllocated istms
           return csnm
      | otherwise
      = do let len = length vs
               ln1 = len `div` 2
               ln2 = len - ln1 
               (vs1,vs2) = splitAt ln1 vs
           cv1 <- go (TArray (Literal ln1) tval) vs1
           cv2 <- go (TArray (Literal ln2) tval) vs2
           snm  <- freshName "__val_arr" t Mut
           let valsiz = tySizeOf_C tval
               csiz1  = [cexp| $int:ln1 * $valsiz|]
               csiz2  = [cexp| $int:ln2 * $valsiz|]
           DeclPkg d istms <- codeGenDeclGroup (name snm) t ZeroOut
           appendTopDecl d 
           mapM_ addGlobalWplAllocated istms

           addGlobalWplAllocated $ 
              [cstm| blink_copy((void *) $id:(name snm), 
                                (void *) $cv1, $csiz1);|] 
           addGlobalWplAllocated $
              [cstm| blink_copy((void *) & $id:(name snm)[$int:ln1], 
                                (void *) $cv2, $csiz2);|]

           let csnm = [cexp| $id:(name snm)|]
           return csnm
    go ty _ = panicCgNonArray "cgArrVal_val" loc ty

cgArrVal_val _ loc t _es = panicCgNonArray "cgArrVal_val" loc t


cgArrVal_exp :: DynFlags -> SrcLoc -> Ty -> [Exp] -> Cg C.Exp
cgArrVal_exp dfs loc t@(TArray _ _tbase) es = do
   snm <- freshName "__exp_arr" t Mut
   let csnm = [cexp| $id:(name snm)|]
   appendCodeGenDeclGroup (name snm) t ZeroOut
   extendVarEnv [(snm,csnm)] $ do 
     _ <- zipWithM (\e i -> codeGenExp dfs (eAssign loc (lhs snm i) e)) es [0..]
     return csnm
   where 
     lhs x idx = eArrRead loc (eVar loc x)
                              (eVal loc tint (VInt idx Signed)) LISingleton
             
cgArrVal_exp _dfs loc t _es = panicCgNonArray "cgArrVal_exp" loc t

panicCgNonArray :: String -> SrcLoc -> Ty -> Cg a
panicCgNonArray msg loc t = 
  panic $ vcat [ text "cgArr" <+> text msg
               , text "location: " <+> ppr loc
               , text "non-array-type: " <+> ppr t ]


