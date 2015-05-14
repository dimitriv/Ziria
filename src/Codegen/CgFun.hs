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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module CgFun ( cgFunDefined ) where

import Prelude

import Rebindables
import Opts
import AstExpr
import AstUnlabelled

import AstComp
import PpComp
import PpExpr
import qualified GenSym as GS
import CgHeader
import CgRuntime
import CgMonad
import CgTypes
import CgExpr
import CgLUT

import Control.Monad.State
import Control.Applicative

import Control.Monad.Writer
import qualified Data.DList as DL
import qualified Data.List
import Data.Loc
import Data.Monoid
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Set as S
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ 
import Data.Maybe

import Outputable 

import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO(..))

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


type CgPrm a =  StateT [LenVar] Cg a

runCgPrm :: [LenVar] -> CgPrm a -> Cg (a,[LenVar])
runCgPrm lvs m = runStateT m lvs

liftCg :: Cg a -> CgPrm a
liftCg m = StateT (\s -> m >>= \r -> return (r,s))

cgParamsByRef :: [EId] -> CgPrm [C.Param]
cgParamsByRef params = concat <$> mapM cgParamByRef params

cgParamByRef :: EId -> CgPrm [C.Param]
cgParamByRef nm = cg_param_byref (nameTyp nm)
  where cg_param_byref ty
          | isArrayTy ty = cg_array_param nm ty
          | otherwise    = return [cparams|$ty:(codeGenTyOcc ty) * $id:(name nm)|]

cgParams :: [EId] -> CgPrm [C.Param]
cgParams params = concat <$> mapM cgParam params

cgParam :: EId -> CgPrm [C.Param]
cgParam nm = cg_param (nameTyp nm)
  where 
    cg_param ty
      | isArrayTy ty       = cg_array_param nm ty
      | isStructPtrType ty = return [cparams|$ty:(codeGenTyOcc ty) * $id:(name nm)|]
      | otherwise          = return [cparams|$ty:(codeGenTyOcc ty) $id:(name nm)  |]

cg_array_param :: EId -> Ty -> CgPrm [C.Param]
cg_array_param nm ty@(TArray (Literal l) _) = do
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
    -- ^ Reuse the result of LUT unpacking, new body is of type Unit

instance Functor RetBody where
  fmap h (DoReuseLcl x e) = DoReuseLcl x (h e)
  fmap h (NoReuseLcl gen) = NoReuseLcl (\x -> h (gen x))


data RetType = RetByVal Exp | RetByRef (RetBody Exp)

retByRef :: EId     -- Function name
         -> Exp     -- Body
         -> RetType -- Transformed body
retByRef f body
  | isArrayTy body_ty = RetByRef (transBodyByRef body)
  | otherwise         = RetByVal body 
  where body_ty = ctExp body


transBodyByRef :: Exp -> RetBody Exp
transBodyByRef = go [] where
  go env e = go0 (expLoc e) (unExp e) where
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
  let clos_vars_from_called = concat (map snd called_funs)
  -- | Sum the two up and return
  return $ nub (clos_vars_from_body ++ clos_vars_from_called)


cgFunDefined :: DynFlags
             -> SrcLoc
             -> Fun      -- ^ The function definition
             -> Cg a     -- ^ Action for the continuation of the function
             -> Cg a
cgFunDefined dflags csp
             fdef@(MkFun (MkFunDefined f params orig_body) _ _)
             action = do 
  -- | make up a new name for the function
  newNm <- freshName (name f ++ "_" ++ getLnNumInStr csp) (nameTyp f) Imm
  let retTy      = ctExp orig_body
  let (TArrow argtys _) = nameTyp f
  let ret_by_ref = retByRef f orig_body 
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
  (retParamEnv :: [EId], body' :: Exp) <-
      case ret_by_ref of
        RetByVal e -> return ([],e)
        RetByRef (DoReuseLcl lcl e') -> return ([lcl],e')
        RetByRef (NoReuseLcl gen_e ) -> do 
          retn <- freshName "_ret" retTy Mut
          return ([retn], gen_e retn)

  -- Freshen actual parameters
  paramsEnv_fresh <- mapM fresh params

  let mk_actual_param :: (EId,ArgTy) -> CgPrm [C.Param]
      mk_actual_param (p,GArgTy _ Mut) = cgParamByRef p
      mk_actual_param (p,GArgTy _ Imm) = cgParam p

  -- Create actual parameters   
  let penv = zip paramsEnv_fresh argtys
  (actualParams :: [C.Param],_lvs') <- runCgPrm lvs $ 
       do rets <- cgParamsByRef retParamEnv
          rest <- mapM mk_actual_param penv -- ^ ordinary prms
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

  extendExpFunEnv f (newNm, closureEnv) action


cgFunDefined _ _ _ _ = error "cgFunDefined: not a FunDefined function!"










   
{-   


  -- | Freshen params and closure variables
  (fresh_params,fresh_cparams) <- unzip <$> 
        mapM (\p -> do x <- genSym (name p)
                       return (p { name = x }

************************* here ... 

       ; let (params', locals_defs', locals_inits', body')
                = case ret_by_ref of
                     RetByVal ebody
                       -> (params, locals, locals, ebody)
                     RetByRef (NoReuseLcl (ret_n, ret_body))
                       -> let params' = ret_n : params
                              body'   = ret_body
                          in (params', locals, locals , body')
                     RetByRef (DoReuseLcl (ret_lcl, ret_body))
                       -> let params' = ret_lcl : params
                              locals' = filter (\lcl -> mutVar lcl /= ret_lcl) locals
                              locals_inits = locals
                          in (params', locals', locals_inits, ret_body)
                     RetByRef (DoReuseLUT (ret_n, ret_body))
                       -> let params' = ret_n : params
                              body'   = ret_body
                          in (params', locals, locals , body')

       ; let name_uniq pn = (pn,[cexp|$id:(getNameWithUniq pn)|])
             paramsEnv = map name_uniq params'
             localsEnv = map (name_uniq . mutVar) locals_defs'


       ; vars <- getBoundVars 
       ; let ambient_bound = map ppNameUniq vars


         -- Create an init group of all locals (just declarations)
       ; let decl_local MutVar{..} =
               codeGenDeclGroup (getNameWithUniq mutVar) (nameTyp mutVar)
       ; clocals_decls <- mapM decl_local locals_defs'
         -- Create initialization code for locals (just inits)
       ; (c_decls', c_stmts')
           <- inNewBlock_ $
              extendVarEnv paramsEnv  $
              extendVarEnv closureEnv $
              extendVarEnv localsEnv  $
              codeGenGlobalInitsOnly dflags $
              map (\mv -> setMutVarName (getNameWithUniq (mutVar mv)) mv) locals_inits'

       ; vars' <- getBoundVars 
       ; let ambient_bound' = map ppNameUniq vars'

       ; (cdecls, cstmts, cbody) <-
            inNewBlock $
            extendVarEnv paramsEnv  $
            extendVarEnv closureEnv $
            extendVarEnv localsEnv  $
            case body' of
              MkExp (ELUT r body'') _ _
                  | not (isDynFlagSet dflags NoLUT)               -- if it's a body that is lutted
                  , RetByRef (DoReuseLUT (ret_n,_)) <- ret_by_ref -- and here's the return name
                  -> codeGenLUTExp dflags r body'' (Just ret_n)   -- { name = getNameWithUniq ret_n })
                  | not (isDynFlagSet dflags NoLUT)
                  -> codeGenLUTExp dflags r body'' Nothing -- the usual thing

              _ -> codeGenExp dflags body'

       ; closure_params <- codeGenParamsByRef closureParams
       ; cparams <- codeGenParams params'

       ; appendTopDecl [cdecl|$ty:(codeGenTy_qual "" (ctExp body'))
                                 $id:(name newNm)($params:(cparams++closure_params));|]

       ; appendTopDef [cedecl|$ty:(codeGenTy_qual "" (ctExp body'))
                                 $id:(name newNm)($params:(cparams++closure_params)) {
                                $decls:c_decls'         // Define things needed for locals
                                $decls:clocals_decls    // Define locals

                                $decls:cdecls           // Define things needed for body

                                $stms:c_stmts'          // Emit initialization of locals
                                $stms:cstmts            // Emit rest of body
                                return $(cbody);
                              }|]

       ; extendExpFunEnv f (newNm, closureParams) $ action
       }



-}
