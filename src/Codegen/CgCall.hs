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
{-# LANGUAGE  QuasiQuotes, GADTs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-binds -Werror #-}

module CgCall
  ( cgCall 
  ) where

import Opts
import AstExpr
import AstComp
import PpExpr
import PpComp
import qualified GenSym as GS
import CgHeader
import CgRuntime
import CgMonad
import CgTypes

import Control.Monad (when, unless)

import Control.Applicative
import Control.Monad (when)
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

import CtExpr
import CgValDom 
import CgCmdDom
import Utils


cgCall :: DynFlags 
       -> SrcLoc
       -> Ty
       -> GName Ty -> [(Either (LVal ArrIdx) C.Exp)]
       -> C.Exp -- ^ Where to store result
       -> Cg C.Exp
cgCall dfs loc retTy fName eargs cer = do
  (real_f_name, closure_params) <- lookupExpFunEnv fName
  let is_external = isPrefixOf "__ext" (name real_f_name)
    
  let cef = [cexp|$id:(name real_f_name)|]
  let (TArrow argtys _) = nameTyp fName

  -- Standard function arguments, disable bounds checks when external calls
  (ceargs,fixup_bitarrs) <- withDisabledBCWhen is_external $ 
                            codeGenArgs dfs loc argtys eargs

  (clos_ceargs, clos_fixup_bitarrs)
      <- codeGenArgs dfs loc 
            (map (\nm -> GArgTy (nameTyp nm) Mut) closure_params)
            (map (Left . GDVar) closure_params)

  let cargs = ceargs ++ clos_ceargs


  case retTy of
    TArray li _ty
      -> when_ (not is_external) inAllocFrame $
         appendStmt [cstm|$(cef)($cer, $args:(lenArg li ++ cargs));|]

        -- See CgFun.hs
    _ | isStructPtrType retTy
      -> when_ (not is_external) inAllocFrame $
         if is_external 
           then appendStmt [cstm| *($cer) = $(cef)($args:cargs);|]
           else appendStmt [cstm|$(cef)($cer, $args:cargs);|]
    TUnit
      -> when_ (not is_external) inAllocFrame $ 
         appendStmt [cstm| $(cef)($args:cargs);|]

    _otherwise
     -> when_ (not is_external) inAllocFrame $ 
        appendStmt [cstm| $cer = $(cef)($args:cargs);|]

  -- Fixup unaligned bit arrays!
  fixup_bitarrs
  clos_fixup_bitarrs
  
  return (if retTy == TUnit then [cexp|UNIT|] else [cexp|$cer|])


  where when_ :: Bool -> (Cg a -> Cg a) -> Cg a -> Cg a
        when_ True f  = f 
        when_ False f = id


{-------------------------------------------------------------------------------
  Argument code generation
-------------------------------------------------------------------------------}

lenArg :: NumExpr -> [C.Exp]
lenArg (Literal l) = [[cexp|$int:l|]]
lenArg (NVar n)    = [[cexp|$id:n|]]

-- | codeGenArg: 
-- Returns arguments for the function, plus "fixup" assignments in the end.
-- Those fixups account for non byte aligned bitreades. E.g:
--     int32 f(x : arr[4] bit) { ... }
-- Call site:
--     f(z[3,4])
-- We will emit code for the following:
-- 
--     (i)   result <- bitarray read z[3,4]
--     (ii)  call f(result)
--     (iii) z[3,4] := result
-- 
-- NB: For large bit arrays this will probably be inefficient, but for
-- performance we need to inline anyway, so there is no point in
-- optimizing this code too much.

codeGenArgs :: DynFlags 
            -> SrcLoc
            -> [ArgTy] 
            -> [Either (LVal ArrIdx) C.Exp] -> Cg ([C.Exp], Cg ())
codeGenArgs dfs loc argtys lvals = go argtys lvals
  where go [] [] = return ([], return ())
        go (a:as) (lv:lvs) = do (es,action) <- codeGenArg dfs loc a lv
                                (es',action') <- go as lvs
                                return (es ++ es', action >> action')
        go _ _ = panicStr "codeGenArgs"


codeGenArg :: DynFlags 
           -> SrcLoc
           -> ArgTy 
           -> Either (LVal ArrIdx) C.Exp -> Cg ([C.Exp], Cg ())
codeGenArg dfs loc argty carg_mb

    -- Immutable array
  | GArgTy (TArray ne _) Imm <- argty
  , Right cearg <- carg_mb
  = return ((cearg : lenArg ne), return ())

    -- Immutable non-array
  | GArgTy ty Imm <- argty
  , Right cearg <- carg_mb
  = return ([cearg], return () ) 

    -- Mutable bit-array
  | GArgTy (TArray ne TBit) Mut <- argty 
  , Left lval_pre <- carg_mb
  , let lval = squashArrDeref lval_pre
  = case lval of
      GDArr d idx lnfo -> cg_bitarr_arg dfs loc d idx lnfo ne
      _ -> do e <- cgDeref dfs loc lval 
              return ((e : lenArg ne), return ())

    -- Mutable array
  | GArgTy (TArray ne _tnonbit) Mut <- argty 
  , Left lval <- carg_mb
  = do e <- cgDeref dfs loc lval
       return ((e : lenArg ne), return ())

    -- Pointer-represented type, mutable arg
  | GArgTy ty Mut <- argty, Left lval <- carg_mb
  , isPtrType ty 
  = do { e <- cgDeref dfs loc lval; return ([e], return ()) }

    -- Non-pointer represented type, mutable arg
  | GArgTy ty Mut <- argty, Left lval <- carg_mb
  = do { e <- cgDeref dfs loc lval; return ([[cexp| &($e)|]], return ()) }


  | otherwise
  = panicStr "codeGenArg"


cg_bitarr_arg :: DynFlags 
              -> SrcLoc 
              -> LVal ArrIdx 
              -> ArrIdx 
              -> LengthInfo 
              -> NumExpr -> Cg ([C.Exp],Cg ())
cg_bitarr_arg dfs loc lbase astart li ne 
  | Just {} <- isBitArrByteAligned astart 
  = do e <- cgDeref dfs loc (GDArr lbase astart li)
       return ((e : lenArg ne), return ())
  | otherwise -- non-aligned, will have to create new storage
  = do new_e <- cgDeref dfs loc (GDArr lbase astart li)
       cbase <- cgDeref dfs loc lbase
       let stmt = cgArrWrite_chk ret_ty cbase astart li new_e
       return (new_e : lenArg ne, stmt)
  where ret_ty = ctDerefExp (GDArr lbase astart li)
