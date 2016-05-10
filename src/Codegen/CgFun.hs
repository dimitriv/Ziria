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
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module CgFun ( cgFunDefined, cgFunExternal ) where

import Prelude

import Rebindables
import Opts
import AstExpr
import AstUnlabelled
import CgMonad
import CgTypes
import CgExpr

import Control.Monad.State
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif /* !MIN_VERSION_base(4,8,0) */

import Data.Loc
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Data.Set as S
import Data.Maybe
import Data.List ( nub )
import Utils ( panicStr )

import CtExpr

{-------------------------------------------------------------------------------
   Generation of parameter signatures and argument lists
-------------------------------------------------------------------------------}
-- | We may have multiple arguments, all polymorphic in the same length:
-- ^            (x : arr[n] int, y : arr[n] int)
-- ^ which should be compiled to:
-- ^            (int *x_22, int n_1, int* y_34, int n_2) 
-- ^ I.e. we must not bind 'n' twice. For this reason we introduce a thin state
-- ^ monad that keeps track of the bound NVar variables.

-- | Returned values from function calls
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ^
--  (i) internal functions that return arrays or by-pointer structs
--      accept first argument to be the return storage
-- ^ 
--  (ii) external functions that return arrays accept first argument
--       to be the return storage.  NB: we do not do the same thing as for
--       internal functions as it's hard to tell external library
--       implementors, exactly which structs will be passed as pointers and
--       which not. Moreover this can change all the time but we do not want
--       the external functions calling convention to change!


type CgPrm a =  StateT [LenVar] Cg a

runCgPrm :: [LenVar] -> CgPrm a -> Cg (a,[LenVar])
runCgPrm lvs m = runStateT m lvs

evalCgPrm :: [LenVar] -> CgPrm a -> Cg a
evalCgPrm lvs m = fst <$> (runStateT m lvs)

liftCg :: Cg a -> CgPrm a
liftCg m = StateT (\s -> m >>= \r -> return (r,s))

cgParamsByRef :: [EId] -> CgPrm [C.Param]
cgParamsByRef params = concat <$> mapM cgParamByRef params

cgParamByRef :: EId -> CgPrm [C.Param]
cgParamByRef nm = cg_param_byref (nameTyp nm)
  where cg_param_byref ty
          | isArrayTy ty = cg_array_param nm ty
          | otherwise = return [cparams|$ty:(codeGenTyOcc ty) * $id:(name nm)|]

cgParam :: EId -> CgPrm [C.Param]
cgParam nm = cg_param (nameTyp nm)
  where 
    cg_param ty
      | isArrayTy ty       = cg_array_param nm ty
      | isStructPtrType ty 
      = return [cparams|$ty:(codeGenTyOcc ty) * $id:(name nm)|]
      | otherwise          
      = return [cparams|$ty:(codeGenTyOcc ty) $id:(name nm)  |]

cg_array_param :: EId -> Ty -> CgPrm [C.Param]
cg_array_param nm ty@(TArray (Literal _l) _) = do
  len <- liftCg $ freshName ("__len_unused_") tint Imm 
  return $ [cparams| $ty:(codeGenTyOcc ty) $id:(name nm), int $id:(name len)|]
cg_array_param nm ty@(TArray (NVar c) _) = do
  bnd_lvs <- get
  new_c <- if c `elem` bnd_lvs then liftCg (genSym "__len_unused") else return c
  put (new_c : bnd_lvs)
  return [cparams| $ty:(codeGenTyOcc ty) $id:(name nm), int $id:new_c |]
cg_array_param _ _ = panicStr "cg_array_param"


{-------------------------------------------------------------------------------
   Transforming functions that return big arrays or by-pointer structs
-------------------------------------------------------------------------------}

data RetBody a 
  = -- | Name is just a local variable returned, new body is of type Unit
    DoReuseLcl EId a
    -- | Name is just a local variable returned, new body is of type Unit
  | NoReuseLcl (EId -> a)                          
    -- | Reuse the result of LUT unpacking, don't transform the body!
  | DoReuseLUT a

instance Functor RetBody where
  fmap h (DoReuseLcl x e) = DoReuseLcl x (h e)
  fmap h (NoReuseLcl gen) = NoReuseLcl (\x -> h (gen x))
  fmap h (DoReuseLUT e)   = DoReuseLUT (h e)

data RetType = RetByVal Exp | RetByRef (RetBody Exp)

retByRef :: Exp     -- Body
         -> Ty      -- body ty 
         -> RetType -- Transformed body
retByRef body body_ty
  | isArrayTy body_ty       = RetByRef (transBodyByRef body)
  | isStructPtrType body_ty = RetByRef (transBodyByRef body)
  | otherwise               = RetByVal body 


transBodyByRef :: Exp -> RetBody Exp
transBodyByRef = go [] where
  go env e = go0 (expLoc e) (unExp e) where
     go0 _loc (ELUT {})              = DoReuseLUT e 
     go0 loc (EVar x) | x `elem` env = DoReuseLcl x (eVal loc TUnit VUnit)
     go0 loc (ESeq e1 e2)            = fmap (eSeq loc e1) (go env e2)
     go0 loc (ELet x fi e1 e2) =
        let e2' = go (x:env) e2
        in case e2' of
             DoReuseLcl y _
               | y == x -> fmap (eSeq loc (eAssign loc (eVar loc x) e1)) e2'
             _otherwise -> fmap (eLet loc x fi e1) e2'
     go0 loc (ELetRef x Nothing e2) =
        let e2' = go (x:env) e2
        in case e2' of
             DoReuseLcl y _
               | y == x -> e2'
             _otherwise -> fmap (eLetRef loc x Nothing) e2'
     go0 loc (ELetRef x (Just e1) e2) =
        let e2' = go (x:env) e2
        in case e2' of
             DoReuseLcl y _
               | y == x -> fmap (eSeq loc (eAssign loc (eVar loc x) e1)) e2'
             _otherwise -> fmap (eLetRef loc x (Just e1)) e2'
     go0 loc _e0 = NoReuseLcl $ \retN -> eAssign loc (eVar loc retN) e

getClosureVars :: Fun -> Cg [EId]
getClosureVars fdef = do
  -- | Get the free variables of this definition
  let clos_vars_from_body = S.toList (funFVsClos fdef)
  -- | Get all the closure parameters (recursively) of callees 
  called_funs <- map fromJust <$> filter isJust <$> 
                    (mapM lookupExpFunEnv_maybe $ S.toList (funFVs fdef))
  let clos_vars_from_called = concat (map (\(_,fs,_) -> fs) called_funs)
  -- | Sum the two up and return
  return $ nub (clos_vars_from_body ++ clos_vars_from_called)


cg_actual_param :: (EId,ArgTy) -> CgPrm [C.Param]
cg_actual_param (p,GArgTy _ Mut) = cgParamByRef p
cg_actual_param (p,GArgTy _ Imm) = cgParam p

cgFunDefined :: DynFlags
             -> SrcLoc
             -> Fun     -- ^ Function definition 
             -> Cg a    -- ^ Action for the continuation of the function
             -> Cg a
cgFunDefined dflags csp
             fdef@(MkFun (MkFunDefined f params orig_body) _ _)
             action = do 
  -- Make up a new name for the function
  newNm <- freshName (name f ++ "_" ++ getLnNumInStr csp) (nameTyp f) Imm
  let retTy      = ctExp orig_body
  let (TArrow argtys _) = nameTyp f
  let ret_by_ref = retByRef orig_body retTy
  -- | get closure variables
  closureEnv <- getClosureVars fdef

  let fresh :: EId -> Cg EId
      fresh v = genSym (name v) >>= \x -> return (v { name = x })

  -- Create C bindings for mutable versus immutable arguments
  let mk_cbind :: EId -> MutKind -> C.Exp
      mk_cbind p Mut | isPtrType (nameTyp p) 
                     = [cexp| $id:(name p)  |]
                     | otherwise
                     = [cexp| * $id:(name p)|]
      mk_cbind p Imm | isPtrType (nameTyp p) 
                     = [cexp| $id:(name p)  |]
                     | otherwise
                     = [cexp| $id:(name p)  |]

  -- | Freshen closure environment
  closureEnv_fresh :: [EId] <- mapM fresh closureEnv

  -- Closure parameters
  (closureParams :: [C.Param], (lvs :: [LenVar])) <-
     runCgPrm [] (cgParamsByRef closureEnv_fresh)

  let closureParams_cbinds = map (\p -> mk_cbind p Mut) closureEnv_fresh

  -- Return parameters and new body.
  (retParamEnv :: [EId], body' :: Exp, lut_returning) <-
      case ret_by_ref of
        RetByVal e -> return ([],e, False)
        RetByRef (DoReuseLcl lcl e') -> return ([lcl],e',False)
        RetByRef (NoReuseLcl gen_e ) -> do 
          retn <- freshName "_ret" retTy Mut
          return ([retn], gen_e retn,False)
        RetByRef (DoReuseLUT e) -> return ([], e, True)

  -- Freshen actual parameters
  paramsEnv_fresh <- mapM fresh params

  -- Create actual parameters   
  let penv = zip paramsEnv_fresh argtys
  actualParams <- evalCgPrm lvs $
    do rets <- cgParamsByRef retParamEnv
       rest <- mapM cg_actual_param penv -- ^ ordinary prms
       return (rets ++ concat rest)      -- ^ all of them

  -- Create C bindings for the parameters 
  let actualParams_cbinds = rets ++ rest 
        where rets = map (\p -> mk_cbind p Mut) retParamEnv
              rest = map (\(p,GArgTy _ m) -> mk_cbind p m) penv

  -- Total parameter list (excluding potential extra return param)
  let allParams :: [C.Param]
      allParams = actualParams ++ closureParams

  -- TODO: do we need to do anything special with the lenvars?
  (cdecls, cstmts, cbody) <-
     inNewBlock $
     pushAllocFrame $ 
     extendVarEnv (zip closureEnv closureParams_cbinds) $ 
     extendVarEnv (zip (retParamEnv ++ params) actualParams_cbinds)$ 
     codeGenExp dflags body'

  -- Emit prototype and definition
  ----------------------------------------------------------------------

  appendTopDecl $ 
     [cdecl| $ty:(codeGenTyOcc (ctExp body'))
                $id:(name newNm)($params:allParams);|]

  appendTopDef $ 
     [cedecl| $ty:(codeGenTyOcc (ctExp body'))
              $id:(name newNm)($params:allParams) {
                      $decls:cdecls    // Define things needed for body
                      $stms:cstmts     // Emit rest of body
                      return $(cbody);
             }
     |]
  extendExpFunEnv f (newNm, closureEnv, lut_returning) action

cgFunDefined _ _ _ _ = panicStr "cgFunDefined"



cgFunExternal :: DynFlags
              -> SrcLoc
              -> Fun     -- ^ Function definition 
              -> Cg a    -- ^ Action for the continuation of the function
              -> Cg a
cgFunExternal _dflags loc
              (MkFun (MkFunExternal nm ps retTy) _ _) action = go retTy where
  ret_str  = "__retf_" ++ name nm
  ext_str  = mkExternalCFunName (name nm)
  fun_ty   = nameTyp nm
  ext_nm   = toName ext_str loc fun_ty Imm
  ret_name = toName ret_str loc retTy Mut
  TArrow argtys _ = fun_ty

  -- Externals only have an extra argument if return arrays.
  go (TArray {}) = do
    actuals <- evalCgPrm [] $
       do rets <- cgParamsByRef [ret_name]
          rest <- mapM cg_actual_param (zip ps argtys) -- ^ ordinary prms
          return (rets ++ concat rest)      -- ^ all of them

    appendTopDecl [cdecl|void $id:ext_str ($params:actuals);|]
    extendExpFunEnv nm (ext_nm,[],False) action

  go _ = do
    actuals <- evalCgPrm [] $ concat <$> mapM cg_actual_param (zip ps argtys)
    appendTopDecl $
      [cdecl|$ty:(codeGenTyOcc retTy) $id:ext_str ($params:actuals);|]
    
    extendExpFunEnv nm (ext_nm,[],False) action

cgFunExternal _ _ _ _ = panicStr "cgFunExternal"






