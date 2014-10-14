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

import Control.Monad.Writer
import qualified Data.DList as DL
import qualified Data.List
import qualified Data.Loc
import Data.Monoid
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.Mainland
import Data.Maybe

import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO(..))

import Data.List ( nub )

import CtExpr

data RetBody
  = DoReuseLcl (EId, Exp) -- name is just a local
  | NoReuseLcl (EId, Exp) -- name is new
  | DoReuseLUT (EId, Exp) -- reuse the result of LUT unpacking

data RetType
  = RetByVal Exp
  | RetByRef { -- We
               --  o either rewrite body so that (ret := body) or
               --  o simply forget about last instruction (e.g. 'return xvar')
               --  o ... but record which variable it was.
               ret_body :: RetBody
             }


retByRef :: EId -> [EId] -> Exp -> Cg RetType
retByRef f locals body
 = do { is_ret_ptr <- isStructPtrType body_ty
      ; case is_ret_ptr || isArrayTy body_ty of
          False -> return $ RetByVal body
          True  -> return $ RetByRef { ret_body = trans_body [] body  }
      }
 where
   lift_ret f (DoReuseLcl (n,e)) = DoReuseLcl (n, f e)
   lift_ret f (NoReuseLcl (n,e)) = NoReuseLcl (n, f e)
   lift_ret f (DoReuseLUT (n,e)) = DoReuseLUT (n, f e)

   body_ty = ctExp body
   loc     = expLoc body
   retN    = toName ("__retf_" ++ (name f)) Nothing body_ty
   retE    = eVar (expLoc body) retN 

   -- precondition: 'e' returns an array type
   trans_body xs e
     = case unExp e of
         ESeq e1 e2
           -> lift_ret (eSeq loc e1) $
              trans_body xs e2
         ELet x fi e1 e2
           -> lift_ret (eLet loc x fi e1) $
              trans_body (x:xs) e2
         EVar x
           | not (x `elem` xs)
           , x `elem` locals
           -> DoReuseLcl (x, eVal loc TUnit VUnit)
         ELUT {}
           -> DoReuseLUT (retN, e) -- don't rewrite the body
         _otherwise
           -> NoReuseLcl (retN, eAssign loc retE e)

getClosureVars :: Fun -> Cg [(EId, C.Exp)]
getClosureVars fdef
  = do { -- DV: What about length variables? Probably we don't need them
         -- here in accordance with Edsko's plan to just pass the actual
         -- term variables. 
       ; let closure_vars_def = S.toList $ fst $ funFVsClos fdef
       ; let allVars = S.toList $ funFVs fdef
       ; call_fun_infos <- mapM lookupExpFunEnv_maybe allVars

         -- Bound function calls
       ; let call_funs = filter isJust call_fun_infos
         -- /their/ closure parameters
             call_funs_clos_params = (concat (map (snd . fromJust) call_funs))

         -- Finally we sum all those up
       ; let closureVars = nub $ closure_vars_def ++ call_funs_clos_params

       ; let mkClosureVar nm
               = do { let ty = nameTyp nm -- (ty,_ce) <- lookupVarEnv nm
                    ; b <- isStructPtrType ty
                    ; if isArrayTy ty || b then
                          return (nm, [cexp| $id:(name nm) |])
                      else
                          return (nm, [cexp| *$id:(name nm)|])
                    }

       ; mapM mkClosureVar closureVars
       }

cgFunDefined :: DynFlags
             -> Maybe SourcePos
             -> EId      -- Function name
             -> Fun      -- The function definition
             -> Cg a     -- action for the body
             -> Cg a
cgFunDefined dflags csp f
             fdef@(MkFun (MkFunDefined nm params fun_locals orig_body) _ fTy)
             action
  = do { -- make up a new name for the function
         newNm <- freshName (name f ++ "_" ++ getLnNumInStr csp) (nameTyp f)

       ; let retTy = ctExp orig_body
         -- transform the body if by-ref return

         -- get the 'letref' bound locals
       ; let (extra_locals, stripped_body) = stripLetRefs orig_body
       ; let locals = fun_locals ++ extra_locals

       ; ret_by_ref <- retByRef f (map fst locals) stripped_body

         -- get the closure variables
       ; closureEnv <- getClosureVars fdef
       ; let closureParams = map fst closureEnv -- (\(nm,(ty,_)) -> (nm,ty)) closureEnv

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
                              locals' = filter (\(n,_) -> not (n == ret_lcl)) locals
                              locals_inits = locals
                          in (params', locals', locals_inits, ret_body)
                     RetByRef (DoReuseLUT (ret_n, ret_body))
                       -> let params' = ret_n : params
                              body'   = ret_body
                          in (params', locals, locals , body')

       ; let name_uniq pn = (pn,[cexp|$id:(getNameWithUniq pn)|])
             paramsEnv = map name_uniq params'
             localsEnv = map (name_uniq . fst) locals_defs'

         -- Create an init group of all locals (just declarations)
       ; let decl_local (nm, _) = codeGenDeclGroup (getNameWithUniq nm) (nameTyp nm)
       ; clocals_decls <- mapM decl_local locals_defs'
         -- Create initialization code for locals (just inits)
       ; (c_decls', c_stmts')
           <- inNewBlock_ $
              extendVarEnv paramsEnv  $
              extendVarEnv closureEnv $
              extendVarEnv localsEnv  $
              codeGenGlobalInitsOnly dflags $
              map (\(nm,e) -> (nm { name = getNameWithUniq nm },e)) locals_inits'


       ; (cdecls, cstmts, cbody) <-
            inNewBlock $
            extendVarEnv paramsEnv  $
            extendVarEnv closureEnv $
            extendVarEnv localsEnv  $
            case body' of
              MkExp (ELUT r body'') _ _
                  | not (isDynFlagSet dflags NoLUT)               -- if it's a body that is lutted
                  , RetByRef (DoReuseLUT (ret_n,_)) <- ret_by_ref -- and here's the return name
                  -> codeGenLUTExp dflags locals r body'' (Just ret_n { name = getNameWithUniq ret_n })
                  | not (isDynFlagSet dflags NoLUT)
                  -> codeGenLUTExp dflags locals r body'' Nothing -- the usual thing

              _ -> codeGenExp dflags body'

       ; closure_params <- codeGenParamsByRef closureParams
       ; cparams <- codeGenParams params'

       ; appendTopDecl [cdecl|$ty:(codeGenTy (ctExp body'))
                                 $id:(name newNm)($params:(cparams++closure_params));|]

       ; appendTopDef [cedecl|$ty:(codeGenTy (ctExp body'))
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

cgFunDefined _ _ _ _ _ = error "cgFunDefined: not a FunDefined function!"



stripLetRefs :: Exp -> ([(EId, Maybe Exp)], Exp)
-- Strip top-level letrefs
stripLetRefs e = go [] e
  where go defs e
          = go0 defs (unExp e)
          where
             go0 defs (ELetRef nm Nothing e')
               = go ((nm,Nothing):defs) e'
             -- TODO: Should we do something with _ty here?
             go0 defs (ELetRef nm (Just einit) e')
               = go ((nm, Just einit):defs) e'
             go0 defs _other = (reverse defs, e)
