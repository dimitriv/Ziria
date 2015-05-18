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
{-# OPTIONS_GHC -Wall -Werror #-}

module CgValDom
  ( 
   -- Operators
    cgUnOp
  , cgBinOp

  -- Projections
  , cgStrProj
  , ArrIdx ( .. )
  , arrIdxPlus
  , cexpOfArrIdx
  , cgArrRead, cgArrRead_chk
  , isBitArrByteAligned

  -- New arrays or structs 
  , cgArrVal
  , cgStruct 

  ) where

import Opts
import AstExpr
import AstUnlabelled
import CgMonad
import CgTypes
import Utils
import {-# SOURCE #-} CgExpr

import Control.Monad ( zipWithM )
import Data.Loc
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.HughesPJ
import Data.Maybe


import Outputable 

import CgBoundsCheck

{------------------------------------------------------------------------
  ValDom interface (see AbsInt.hs)
------------------------------------------------------------------------}

cgUnOp :: Ty    -- ^ Type of @Unop op ce@
       -> UnOp  -- ^ Unary operator
       -> C.Exp -- ^ Inner expression (already compiled)
       -> Ty    -- ^ Type of @ce@
       -> C.Exp
cgUnOp _ Neg     ce _  = [cexp|-$ce|]
cgUnOp _ Not     ce _  = [cexp|!$ce|]
cgUnOp _ BwNeg   ce te -- NB: BwNeg is polymorphic
  | te == TBit = [cexp|((~$ce))&1|]
  | otherwise  = [cexp|(~$ce)|]

cgUnOp _ ALength _ce (TArray (Literal l) _t) = [cexp|$int:l|]
cgUnOp _ ALength _ce (TArray (NVar c) _t)    = [cexp|$id:c |]
cgUnOp _ ALength _ce _ = panicStr "cgUnOp: ALength on non-array!"
cgUnOp _ NatExp _ _    = panicStr "cgUnOp: NatExp not supported!"

cgUnOp _target_ty (Cast target_ty) ce src_ty
  | target_ty /= _target_ty
  = panicStr "cgUnOp: invalid cast!"
  | otherwise
  = case (target_ty, src_ty) of
      (TBit, TInt _)     -> [cexp|$ce & 1|]
      (TInt _, TBit)     -> ce
      (TDouble, TInt _)  -> [cexp|(double) $ce|]
      (TInt bw, TDouble) -> [cexp|($ty:(intty bw)) $ce |]
      (TInt bw, TInt _)  -> [cexp|($ty:(intty bw)) $ce |]

      -- | For complex types we must emit a proper function, defined 
      --   in csrc/numerics.h
      (TStruct tn _flds, TStruct sn _flds')
         | isComplexTy target_ty && isComplexTy src_ty
         , let castfun = sn ++ "_to_" ++ tn
         -> [cexp|$id:castfun($ce)|]
      (_,_) -> panicStr "cgUnOp: invalid cast!"
  where intty bw = namedCType (cgTIntName bw) 

cgBinOp :: Ty            -- ^ type of (BinOp op ce1 ce2)
        -> BinOp
        -> C.Exp -> Ty   -- ^ ce1 and its type
        -> C.Exp -> Ty   -- ^ ce2 and its type
        -> C.Exp
cgBinOp _ op ce1 t ce2 _
  | isComplexTy t
  , TStruct cn _flds <- t
  , let fplus  = cn ++ "_plus"
        fminus = cn ++ "_minus"
        fmult  = cn ++ "_mult"
        fdiv   = cn ++ "_div"
  = case op of
      Add  -> [cexp|$id:fplus($ce1,$ce2) |]
      Sub  -> [cexp|$id:fminus($ce1,$ce2)|]
      Mult -> [cexp|$id:fmult($ce1,$ce2) |]
      Div  -> [cexp|$id:fdiv($ce1,$ce2)  |]
      _    -> panicStr ("cgBinOp: unsupported complex op: " ++ show op) 
  | otherwise
  = case op of
      Add   -> [cexp|$ce1 + $ce2    |]   
      Sub   -> [cexp|$ce1 - $ce2    |]
      Mult  -> [cexp|$ce1 * $ce2    |]
      Div   -> [cexp|$ce1 / $ce2    |]
      Rem   -> [cexp|$ce1 % $ce2    |]   
      Expon -> [cexp|pow($ce1, $ce2)|]

      ShL   -> [cexp|($ce1 << $ce2) |]
      ShR   -> [cexp|($ce1 >> $ce2) |]
      BwAnd -> [cexp|($ce1 & $ce2)  |]
      BwOr  -> [cexp|($ce1 | $ce2)  |]
      BwXor -> [cexp|($ce1 ^ $ce2)  |]

      Eq    -> [cexp|$ce1 == $ce2   |]
      Neq   -> [cexp|$ce1 != $ce2   |]
      Lt    -> [cexp|$ce1 < $ce2    |]
      Gt    -> [cexp|$ce1 > $ce2    |]
      Leq   -> [cexp|$ce1 <= $ce2   |]
      Geq   -> [cexp|$ce1 >= $ce2   |]
      And   -> [cexp|$ce1 && $ce2   |]
      Or    -> [cexp|$ce1 || $ce2   |]


cgStrProj :: Ty            -- ^ Projection type
          -> C.Exp   -> Ty -- ^ Struct and its type
          -> FldName       -- ^ Field to project
          -> C.Exp
cgStrProj proj_ty cd struct_ty f 
  | is_struct_ptr
  = if not is_proj_ptr || isArrayTy proj_ty
    then [cexp| $cd->$id:f |] else [cexp| &($cd->$id:f) |]
  | otherwise
  = if not is_proj_ptr || isArrayTy proj_ty
    then [cexp| $cd.$id:f |]  else [cexp| &($cd.$id:f) |]
  where is_struct_ptr = isStructPtrType struct_ty -- ^ struct type
        is_proj_ptr   = isStructPtrType proj_ty   -- ^ field type
 
data ArrIdx
  = AIdxCExp C.Exp      -- ^ A C expression index
  | AIdxStatic Int      -- ^ A statically known index
  | AIdxMult Int C.Exp  -- ^ A statically known multiple of unknown C.Exp
  deriving Show 

arrIdxPlus :: ArrIdx -> ArrIdx -> ArrIdx
arrIdxPlus (AIdxStatic i) (AIdxStatic j) = AIdxStatic (i+j)
arrIdxPlus a1 a2 = AIdxCExp [cexp| $(cexpOfArrIdx a1) + $(cexpOfArrIdx a2)|]


cexpOfArrIdx :: ArrIdx -> C.Exp
cexpOfArrIdx (AIdxStatic i)  = [cexp| $int:i|]
cexpOfArrIdx (AIdxCExp ce)   = ce
cexpOfArrIdx (AIdxMult i ce) = [cexp| $int:i*$ce|]

cgArrRead :: DynFlags 
          -> SrcLoc
          -> Ty          -- ^ Return type
          -> C.Exp -> Ty -- ^ Array and array type
          -> ArrIdx      -- ^ Start
          -> LengthInfo  -- ^ Range to read from
          -> Cg C.Exp
cgArrRead dfs loc ty carr arr_ty start_idx li = do 
  cgBoundsCheck dfs loc arr_ty (cexpOfArrIdx start_idx) li
  -- | Do bounds checking and call the 'checked' version (chk)
  cgArrRead_chk ty carr start_idx li


isBitArrByteAligned :: ArrIdx -> Maybe C.Exp
isBitArrByteAligned (AIdxStatic i)  
  | i `mod` 8 == 0 
  = Just [cexp| $int:(i `div` 8)|]
isBitArrByteAligned (AIdxMult i ce)
  | i `mod` 8 == 0 
  = Just [cexp| $int:(i `div` 8)*$ce|]
isBitArrByteAligned _ = Nothing


cgArrRead_chk :: Ty          -- ^ Return type
              -> C.Exp       -- ^ Array 
              -> ArrIdx      -- ^ Start
              -> LengthInfo  -- ^ Range to read from
              -> Cg C.Exp
cgArrRead_chk TBit carr start_idx LISingleton = do 
  res <- freshName "bitres" TBit Mut 
  appendCodeGenDeclGroup (name res) TBit ZeroOut
  appendStmt $ 
    [cstm| bitRead($carr,$(cexpOfArrIdx start_idx),& $id:(name res)); |]
  return [cexp| $id:(name res) |]

cgArrRead_chk TBit carr start_idx (LILength l)
  | Just cidx <- isBitArrByteAligned start_idx 
  = return [cexp| & $carr[$cidx]|]
  | otherwise 
  = do let res_ty = TArray (Literal l) TBit
       res <- freshName "bitarrres" res_ty Mut
       appendCodeGenDeclGroup (name res) res_ty ZeroOut
       appendStmt [cstm| bitArrRead($carr,$ce,$int:l,$id:(name res)); |]
       return [cexp| $id:(name res) |]
  where ce = cexpOfArrIdx start_idx

cgArrRead_chk tbase carr start_idx LISingleton
  | isStructPtrType tbase || isArrayTy tbase 
  = return [cexp| & $carr[$cestart] |]
  | otherwise
  = return [cexp| $carr[$cestart]   |]
  where cestart = cexpOfArrIdx start_idx

cgArrRead_chk _tbase carr start_idx (LILength _)
  = return [cexp|&($carr[$(cexpOfArrIdx start_idx)])|]

cgArrRead_chk _tbase carr start_idx li
  = panic $ vcat [ text "cgArrRead_chk!"
                 , text "carr     :" <+> text (show carr)
                 , text "start_idx:" <+> text (show start_idx)
                 , text "li       :" <+> text (show li)
                 ]


cgStruct :: DynFlags -> SrcLoc -> Ty -> [(FldName,C.Exp)] -> Cg C.Exp
cgStruct _dfs _loc t@(TStruct _sn fld_tys) fld_exps = do
   snm <- freshName "__struct" t Mut
   let csnm = [cexp| $id:(name snm)|]
   appendCodeGenDeclGroup (name snm) t ZeroOut
   let fld_ty f   = fromJust $ lookup f fld_tys 
       proj_exp f = cgStrProj (fld_ty f) csnm t f
   mapM_ (\(fn,fe) -> assignByVal (fld_ty fn) (proj_exp fn) fe) fld_exps
   return csnm

cgStruct _ loc t _fld_exps = 
  panic $ vcat [ text "cgStruct"
               , text "location: "       <+> ppr loc
               , text "non-array-type: " <+> ppr t ]



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

cgArrVal_val _ _ t@(TArray {}) vs = do
   snm <- freshName "__val_arr" t Mut
   let csnm = [cexp| $id:(name snm)|]
   let inits = cgNonBitValues vs
   appendCodeGenDeclGroup (name snm) t (InitWith inits)
   return csnm
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
                              (eVal loc tint (VInt idx)) LISingleton
             
cgArrVal_exp _dfs loc t _es = panicCgNonArray "cgArrVal_exp" loc t

panicCgNonArray :: String -> SrcLoc -> Ty -> Cg a
panicCgNonArray msg loc t = 
  panic $ vcat [ text "cgArr" <+> text msg
               , text "location: " <+> ppr loc
               , text "non-array-type: " <+> ppr t ]
