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

module CgExpr
  ( codeGenExp
  , codeGenArrRead
  , codeGenArrWrite

  , codeGenParams
  , codeGenParamsByRef

  , codeGenGlobalDeclsOnlyAlg
  , codeGenGlobalInitsOnly
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

import Text.Parsec.Pos ( SourcePos )


import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.List (elemIndex, foldl', isPrefixOf )
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Language.C.Quote.Base (ToConst(..))
import qualified Language.C.Pretty as P
import Numeric (showHex)
import qualified Data.Map as M
import Text.PrettyPrint.Mainland
import Data.Maybe

-- TODO: reimport this when permutations are fixed, or maybe we can
-- express the optimized permutation primitive in Blink.
-- import CgPerm

cgBoundsCheck :: DynFlags
              -> Maybe SourcePos -> Ty -> C.Exp -> LengthInfo -> Cg ()
cgBoundsCheck dflags loc arrty cbase linfo
   | isDynFlagSet dflags BoundsCheck
   , TArray numexpr _ <- arrty
   = do { is_disabled <- isDisabledBC
        ; if is_disabled then return ()
          else let leninfo = case linfo of
                     LISingleton -> 0
                     LILength n  -> (n-1)
                     LIMeta _    -> panicStr "cgBoundsCheck: meta-variable"
                   spos = getLnNumInStr loc
               in
               do { cnumexpr
                        <- case numexpr of
                             Literal m -> return [cexp| $int:m |]
                             NVar nm -> return [cexp| $id:nm|] -- DV: used to be lookupVarEnv nm
                  ; appendStmt $
                    [cstm|bounds_check($cnumexpr, $cbase + $int:(leninfo),$string:spos);|]
                  }
        }
   | otherwise = return ()


cgUnOp :: UnOp
       -> C.Exp -- Inner expression (already compiled)
       -> Ty    -- Type of ce
       -> Ty    -- Type of (Unop op ce)
       -> Cg C.Exp
cgUnOp Neg     ce _ _  = return [cexp|-$ce|]
cgUnOp Not     ce _ _  = return [cexp|!$ce|]
cgUnOp BwNeg   ce te _ -- NB: BwNeg is polymorphic
  | te == TBit = return [cexp|((~$ce))&1|]
  | otherwise  = return [cexp|(~$ce)|]

cgUnOp ALength ce (TArray (Literal l) _t) _ = return [cexp|$int:l|]
cgUnOp ALength ce (TArray (NVar c) _t)  _   = return [cexp|$id:c|]
cgUnOp ALength ce _ _ = fail "codeGenExp: Cannot apply length on non-array!"
cgUnOp NatExp _ _ _   = fail "codeGenExp: NatExp not supported yet."

cgUnOp (Cast target_ty) ce src_ty _target_ty
  | target_ty /= _target_ty
  = fail "codeGenExp: catastrophic bug, type of castee different than cast target type!"
  | otherwise
  = case (target_ty, src_ty) of
      (TBit, TInt _)     -> return [cexp|$ce & 1|]
      (TInt _, TBit)     -> return ce
      (TDouble, TInt _)  -> return [cexp|(double) $ce|]
      (TInt bw, TDouble) -> return [cexp|($ty:(namedCType (cgTIntName bw))) $ce |]
      (TInt bw, TInt _)  -> return [cexp|($ty:(namedCType (cgTIntName bw))) $ce |]

      -- For complex we must emit a proper function, see _csrc/numerics.h
      (TStruct tn _flds, TStruct sn _flds')
         | isComplexTy target_ty && isComplexTy src_ty
         , let castfun = sn ++ "_to_" ++ tn
         -> return [cexp|$id:castfun($ce)|]
      (_,_) -> fail "codeGenExp: catastrophic bug, invalid cast passed through type checker?"


cgBinOp :: BinOp
        -> C.Exp -> Ty   -- ce1 and its type
        -> C.Exp -> Ty   -- ce2 and its type
        -> Ty            -- type of (BinOp op ce1 ce2)
        -> Cg C.Exp
cgBinOp op ce1 t@(TStruct cn _flds) ce2 _ _
  | isComplexTy t
  , let fplus  = cn ++ "_plus"
        fminus = cn ++ "_minus"
        fmult  = cn ++ "_mult"
        fdiv   = cn ++ "_div"
  = case op of
      Add  -> return [cexp|$id:fplus($ce1,$ce2) |]
      Sub  -> return [cexp|$id:fminus($ce1,$ce2)|]
      Mult -> return [cexp|$id:fmult($ce1,$ce2) |]
      Div  -> return [cexp|$id:fdiv($ce1,$ce2)  |]
      _    -> fail "CodeGen error, Operation unsupported on complex numbers."

cgBinOp op ce1 _ ce2 _ _ =
    case op of
        Add   -> return [cexp|$ce1 + $ce2|]
        Sub   -> return [cexp|$ce1 - $ce2|]
        Mult  -> return [cexp|$ce1 * $ce2|]
        Div   -> return [cexp|$ce1 / $ce2|]
        Rem   -> return [cexp|$ce1 % $ce2|]
        Expon -> return [cexp|pow($ce1, $ce2)|]

        ShL   -> return [cexp|($ce1 << $ce2)|]
        ShR   -> return [cexp|($ce1 >> $ce2)|]
        BwAnd -> return [cexp|($ce1 & $ce2)|]
        BwOr  -> return [cexp|($ce1 | $ce2)|]
        BwXor -> return [cexp|($ce1 ^ $ce2)|]

        Eq    -> return [cexp|$ce1 == $ce2|]
        Neq   -> return [cexp|$ce1 != $ce2|]
        Lt    -> return [cexp|$ce1 < $ce2|]
        Gt    -> return [cexp|$ce1 > $ce2|]
        Leq   -> return [cexp|$ce1 <= $ce2|]
        Geq   -> return [cexp|$ce1 >= $ce2|]
        And   -> return [cexp|$ce1 && $ce2|]
        Or    -> return [cexp|$ce1 || $ce2|]

codeGenExp :: DynFlags
           -> Exp
           -> Cg C.Exp
codeGenExp dflags e0 = go (ctExp e0) (unExp e0)
  where
    go :: Ty -> Exp0 -> Cg C.Exp
    go t (EVal _ v) = codeGenVal v

    go t (EValArr _ v) = do
        newName <- genSym ("__local_arr_" ++ getLnNumInStr (expLoc e0))
        -- To avoid stacks being pushed and popped
        -- we make these guys global ...
        -- Used to be:
        -- appendDecl =<< codeGenArrVal newName (ctExp e0) v
        appendTopDecl =<< codeGenArrVal newName (ctExp e0) v
        return [cexp| $id:newName |]

    go t (EVar x)
      = lookupVarEnv x 

    go t (EUnOp op e) = do
        ce <- codeGenExp dflags e
        cgUnOp op ce (ctExp e) (ctExp e0)

    go t (EBinOp op e1 e2) = do
        ce1 <- codeGenExp dflags e1
        ce2 <- codeGenExp dflags e2
        cgBinOp op ce1 (ctExp e1) ce2 (ctExp e2) (ctExp e0)

    go t (EAssign e1 e2) = do
        ce1 <- codeGenExp dflags e1
        ce2 <- codeGenExp dflags e2
        assignByVal (ctExp e1) (ctExp e2) ce1 ce2
        return [cexp|UNIT|]

    go t (EArrRead e1 e2 r) = do
        ce1 <- codeGenExp dflags e1
        ce2 <- codeGenExp dflags e2
        cgBoundsCheck dflags (expLoc e0) (ctExp e1) ce2 r
        codeGenArrRead dflags (ctExp e1) ce1 ce2 r

    go t (EArrWrite e1 e2 l e3) = do
        ce1 <- codeGenExp dflags e1
        ce2 <- codeGenExp dflags e2
        ce3 <- codeGenExp dflags e3
        cgBoundsCheck dflags (expLoc e0) (ctExp e1) ce2 l
        codeGenArrWrite dflags (ctExp e1) ce1 ce2 l ce3
        return [cexp|UNIT|]

    go t (EWhile econd ebody) = do
        (init_decls,init_stms,cecond) <- inNewBlock (codeGenExp dflags econd)
        (body_decls,body_stms,cebody) <- inNewBlock (codeGenExp dflags ebody)

        -- cebody may be side-effecting; must eval. each iter.
        freshSym <- genSym "__while"
        -- Invariant: info ebody == ()
        appendDecl  [cdecl|$ty:(codeGenTyAlg $ ctExp ebody) $id:freshSym;|]
        appendDecls init_decls
        appendDecls body_decls
        appendStmts init_stms

        appendStmt [cstm|while ($cecond) {
                           $stms:body_stms
                           $stms:init_stms
                           $id:freshSym = $cebody;
                    }|]
        return [cexp|UNIT|]


    go t (EFor _ui k estart elen ebody) = do

        k_new <- freshName (name k) (ctExp estart)

        (init_decls, init_stms, (ceStart, ceLen)) <- inNewBlock $ do
            ceStart <- codeGenExp dflags estart
            ceLen   <- codeGenExp dflags elen
            return (ceStart, ceLen)

        (body_decls, body_stms, cebody) <- inNewBlock $
            extendVarEnv [(k, [cexp|$id:(name k_new)|])] $
            codeGenExp dflags ebody

        -- cebody may be side-effecting; must eval. each iter.
        freshSym <- genSym "__for"
        -- Invariant: info ebody == ()
        appendDecl  [cdecl|$ty:(codeGenTyAlg $ ctExp ebody) $id:freshSym;|]
        appendDecls init_decls
        appendDecls body_decls
        appendStmts init_stms
        appendStmt [cstm|for ($ty:(codeGenTy (ctExp estart)) $id:(name k_new) = $ceStart; $id:(name k_new) < ($ceStart + $ceLen); $id:(name k_new)++) {
                           $stms:init_stms
                           $stms:body_stms
                           $id:freshSym = $cebody;
                         }|]
        return [cexp|UNIT|]

    -- type(earr) must be "TArray ?n ?t" after typechecking
    go t (EIter k v earr ebody) = do
       
        let TArray _ ta = ctExp earr
        
        k_new <- freshName (name k) tint
        v_new <- freshName (name v) ta

        (init_decls, init_stms, cearr) <-
            inNewBlock $
            extendVarEnv [ (k, [cexp|$id:(name k_new)|])
                         , (v, [cexp|$id:(name v_new)|])
                         ] $
            codeGenExp dflags earr

        (body_decls, body_stms, cebody) <-
            inNewBlock $
            extendVarEnv [ (k, [cexp|$id:(name k_new)|])
                         , (v, [cexp|$id:(name v_new)|])
                         ] $
            codeGenExp dflags ebody

        -- cebody may be side-effecting; must eval. each iter.
        freshSym <- freshVar "__iter"
        -- Invariant: ctExp ebody == ()
        appendDecl [cdecl|$ty:(codeGenTyAlg $ ctExp ebody) $id:freshSym;|]
        appendDecls init_decls
        appendDecls body_decls

        appendStmts init_stms

        cupperBound <- case ctExp earr of
                         TArray (Literal arrSz) _ -> return [cexp|$int:arrSz|]
                         TArray (NVar nlen) _     -> return [cexp|$id:nlen|]
                         _                        -> fail "Iterate has to be over an array!"

        appendStmt [cstm|for (int $id:(name k_new) = 0; $id:(name k_new) < $cupperBound; $id:(name k_new)++) {
                           $ty:(codeGenTyAlg ta) $id:(name v_new);
                           $stms:body_stms
                           $id:freshSym = $cebody;
                         }|]

        return [cexp|UNIT|]

    go t (ELet x _fi e1 e2) = do
        x_name <- genSym $ name x ++ getLnNumInStr (expLoc e0)
        let ty1 = ctExp e1
        d <- case unExp e1 of
               EValArr {} ->
                 return [cdecl|$ty:(codeGenArrTyPtrAlg ty1) $id:(x_name);|]
               _ -> codeGenDeclGroup x_name (ctExp e1)
        appendDecl d

        ce1 <- codeGenExp dflags e1

        extendVarEnv [(x,[cexp|$id:x_name|])] $
          do { case unExp e1 of
                 EValArr {}
                     -> appendStmt [cstm|$id:x_name = $ce1;|]
                 _ -> do { cx <- go (ctExp e1) (EVar x)
                         ; assignByVal (ctExp e1) (ctExp e1) cx ce1 }
            ; codeGenExp dflags e2
            }

    -- TODO: Is it right that we ignore _ty1 here?
    go t (ELetRef x (Just e1) e2) = do
        x_name <- freshVar $ name x ++ getLnNumInStr (expLoc e0)

        let t1 = ctExp e1 

        codeGenDeclGroup x_name t1 >>= appendDecl

        ce1 <- codeGenExp dflags e1

        extendVarEnv [(x, [cexp|$id:x_name|])] $ do
          cx <- go t1 (EVar x)
          assignByVal t1 t1 cx ce1
          codeGenExp dflags e2

    go t (ELetRef x Nothing e2) = do
        x_name <- freshVar $ name x ++ getLnNumInStr (expLoc e0)

        codeGenDeclGroup x_name (nameTyp x) >>= appendDecl

        extendVarEnv [(x,[cexp|$id:x_name|])] $
          codeGenExp dflags e2

    go t (ESeq e1 e2) = do
        ce1 <- codeGenExp dflags e1
        appendStmt [cstm|$ce1;|]
        codeGenExp dflags e2

    go t ce@(ECall nef eargs) = do

        -- Here first look if there is a name replacement created due to
        -- possible multiple calls of the same local function
        -- Invariant: ef = EVar nm

        (real_f_name, closure_params) <- lookupExpFunEnv nef

        let is_external = isPrefixOf "__ext" (name real_f_name)

        withDisabledBCWhen is_external $ do

        let cef = [cexp|$id:(name real_f_name)|]

        -- Standard function arguments
        ceargs <- concat <$> mapM (codeGenArg dflags) eargs

        -- Extra closure arguments
        let closure_args = [MkExp (EVar nm) (expLoc e0) ()
                                | nm <- closure_params]
        cclosure_args <- concat <$> mapM (codeGenArgByRef dflags) closure_args

        let cargs = ceargs ++ cclosure_args

        -- [external-retf] GS: It seems like we're relying on some very tricky
        -- invariants here to ensure that cer =
        -- retf_<name-of-external-function> when the lookup comes back empty!
        -- This seems bad :-)

        let retTy = ctExp e0

        -- liftIO $ putStrLn $ "retTy = " ++ show retTy

        is_struct_ptr <- isStructPtrType retTy

        newNm <- genSym $ name nef ++ "_" ++ getLnNumInStr (expLoc e0)
        
        let retNewN = toName ("__retcall_" ++ newNm) Nothing retTy
            cer     = [cexp|$id:(name retNewN)|]

        appendDecls =<<
           codeGenDeclGlobalGroups dflags [(retNewN, Nothing)]

        case retTy of
          TArray li _ty
            | let clen = case li of Literal l -> [cexp| $int:l|]
                                    NVar c -> [cexp| $id:c|]
            -> inAllocFrame $
               appendStmt [cstm|$(cef)($cer, $clen, $args:cargs);|]

          _ | is_struct_ptr
            -> inAllocFrame $
               appendStmt [cstm|$(cef)($cer, $args:cargs);|]

            | otherwise
           -> inAllocFrame $ 
              appendStmt [cstm| $cer = $(cef)($args:cargs);|]
        
        return [cexp|$cer|]  
         



    go t (EIf e1 e2 e3) = do
        ce1 <- codeGenExp dflags e1
        (e2_decls, e2_stmts, ce2) <- inNewBlock $ codeGenExp dflags e2
        (e3_decls, e3_stmts, ce3) <- inNewBlock $ codeGenExp dflags e3

        appendDecls e2_decls
        appendDecls e3_decls

        freshSym <- genSym "__if"
        appendDecl [cdecl|$ty:(codeGenTyAlg $ ctExp e2) $id:freshSym;|]

        appendStmt [cstm|if ($(ce1)) {
                           $stms:e2_stmts
                           $id:freshSym = $ce2;
                         } else {
                           $stms:e3_stmts
                           $id:freshSym = $ce3;
                         }|]
        return [cexp|$id:freshSym|]

    go t (EPrint nl e1) = do
        printExp nl dflags e1
        return [cexp|UNIT|]

    -- BOZIDAR TODO: We need a more graceful exit here...
    go t (EError ty str) = do
        appendStmts [cstms|printf("RUNTIME ERROR: %s\n", $str);
                           exit(1);|]
        -- DV: This is not quite right, we need a default value of type ty ... 
        -- TODO TODO
        return [cexp|UNIT|]

    go t (ELUT _ e1) | isDynFlagSet dflags NoLUT =
      codeGenExp dflags e1

    go t (ELUT r e1) | isDynFlagSet dflags MockLUT =
      codeGenLUTExp_Mock dflags (expLoc e1) r e1

    go t (ELUT r e1) =
      codeGenLUTExp dflags [] r e1 Nothing

    -- TODO: Re-enable permuations, or treat as library function?
    go t (EBPerm e1 e2) =
      fail "Permutation code is currently under refactoring!"
      -- CgPerm.genPermute dflags e1 e2

    go t (EProj e f) =
      do { b <- isStructPtrType (ctExp e)
         ; bproj <- isStructPtrType t -- The projection type!
         ; cd <- codeGenExp dflags e
         ; return $
           if b then
               if (not bproj || isArrayTy t)
               then [cexp| $cd->$id:f    |] -- if on stack or is array return the thing
               else [cexp| &($cd->$id:f) |] -- else return the address of it (not on stack and not array)
           else
               if (not bproj || isArrayTy t)
               then [cexp| $cd.$id:f    |] -- if on stack or is array return the thing
               else [cexp| &($cd.$id:f) |] -- else return the address of it (not on stack and not array)
         }


    go t (EStruct tn tfs) =
      do { snm <- freshName "__struct" t
         ; let csnm = [cexp| $id:(name snm)|]
         ; appendDecl =<< codeGenDeclGroup (name snm) t -- ASSERT t = TStruct tn ...
         ; b <- isStructPtrType t

         ; extendVarEnv [(snm,csnm)] $
           mapM_ (\(fn,fe) -> do { cfe <- codeGenExp dflags fe
                                 ; fproj <- go (ctExp fe) (EProj (MkExp (EVar snm) (expLoc fe) ()) fn)
                                 ; assignByVal (ctExp fe) (ctExp fe) fproj cfe
                                 }) tfs
         ; return csnm }

printExp :: Bool -> DynFlags -> Exp -> Cg ()
printExp nl dflags e1 = do
    go e1 (ctExp e1)
    when nl $ appendStmt [cstm|printf("\n");|]
  where
    go :: Exp -> Ty -> Cg ()
    go e (TArray l t) = printArray dflags e cUpperBound t
      where cUpperBound
              | Literal len <- l = [cexp|$exp:len|]
              | NVar len    <- l = [cexp|$id:len|]
              | otherwise = error "printExp: unknown upper bound!"
    go e _ = printScalar dflags e

printArray dflags e cupper t
  | TBit <- t
  = do { ce <- codeGenExp dflags e
       ; appendStmt [cstm|printBitArrLn($ce, $cupper); |]
       }
  | otherwise
  = do { pcdeclN <- freshName ("__print_cnt_") tint
       ; pvdeclN <- freshName ("__print_val_") t

       ; let pcdeclN_c = name pcdeclN
             pvdeclN_c = name pvdeclN
 
       ; let pcDeclE  = eVar Nothing pcdeclN 
             pvDeclE  = eVar Nothing pvdeclN
             pvAssign = eAssign Nothing
                           pvDeclE (eArrRead Nothing e pcDeclE LISingleton)

       ; extendVarEnv [(pcdeclN, [cexp|$id:pcdeclN_c|])
                      ,(pvdeclN, [cexp|$id:pvdeclN_c|])] $ do

       ; (e1_decls, e1_stms, ce1) <- inNewBlock $ codeGenExp dflags pvAssign
       ; (e2_decls, e2_stms, ce2) <- inNewBlock $ printScalar dflags pvDeclE
       ; (e3_decls, e3_stms, ce3) <- inNewBlock $ printScalar dflags
                                     (eVal Nothing TString (VString ","))

       ; appendDecls e1_decls
       ; appendDecls e2_decls
       ; appendDecls e3_decls

       ; appendStmt [cstm|for(int $id:pcdeclN_c=0; $id:pcdeclN_c < $exp:cupper ; $id:pcdeclN_c++) {
                            $ty:(codeGenTyAlg t) $id:pvdeclN_c;
                            $stms:e1_stms
                            $stms:e2_stms
                            $stms:e3_stms
                          }|]
       }

printScalar dflags e = do
   ce1 <- codeGenExp dflags e
   appendStmt $
     case ctExp e of
       TUnit        -> [cstm|printf("UNIT");      |]
       TBit         -> [cstm| printf("%d", $ce1); |]
       TBool        -> [cstm| printf("%d", $ce1); |]
       TString      -> [cstm| printf("%s", $ce1); |]
       TInt {}      -> [cstm| printf("%ld", $ce1);|]
       TDouble      -> [cstm| printf("%f", $ce1); |]
       ty | isComplexTy ty
          -> [cstm| printf("(%ld,%ld)", $ce1.re, $ce1.im);|]
          | otherwise
          -> error $ "Don't know how to print value of type " ++ show ty

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
codeGenParamsByRef params 
  = concat <$> mapM codeGenParamByRef params

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


codeGenArg :: DynFlags -> Exp -> Cg [C.Exp]
codeGenArg dflags e = do
    ce   <- codeGenExp dflags e
    clen <- case ctExp e of
              TArray (Literal l) _ -> return [[cexp|$int:l|]]
              TArray (NVar n)    _ -> return [[cexp|$id:n|]]
              _                    -> return []
    return $ ce : clen

codeGenArgByRef :: DynFlags -> Exp -> Cg [C.Exp]
codeGenArgByRef dflags e
    | isArrayTy (ctExp e) = codeGenArg dflags e
    | otherwise
    = do { ce <- codeGenExp dflags e
         ; case ce of
             C.Var (C.Id {}) _
              -> do { alloc_as_ptr <- isStructPtrType (ctExp e)
                    ; if alloc_as_ptr || isArrayTy (ctExp e)
                      then return [ [cexp|$ce|] ]
                      else return [ [cexp|&$ce|] ]
                    }
             _otherwise -- A bit of a weird case. Create storage and pass addr of storage
              -> do { new_tmp <- freshVar "clos_ref_arg"
                    ; let ety = ctExp e
                    ; g <- codeGenDeclGroup new_tmp ety
                    ; appendDecl g
                    ; assignByVal ety ety [cexp|$id:new_tmp|] ce
                    ; alloc_as_ptr <- isStructPtrType ety -- Pointer?
                    ; if alloc_as_ptr || isArrayTy ety
                            -- Already a ptr
                       then return [ [cexp| $id:new_tmp   |] ]
                            -- Otherwise take address
                       else return [ [cexp| & $id:new_tmp |] ]
                    }
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
codeGenGlobalInitsOnly :: DynFlags -> [(EId, Maybe Exp)] -> Cg ()
codeGenGlobalInitsOnly dflags defs = mapM_ go defs
  where go :: (EId, Maybe Exp) -> Cg ()
        go (nm, Just e) = do
            ce <- codeGenExp dflags e
            assignByVal (ctExp e) (ctExp e) [cexp|$id:(name nm)|] ce
        go (_, Nothing) = return ()

------------------------------------------------------------------------------
-- | Reading/writing to/from arrays
------------------------------------------------------------------------------

codeGenArrRead :: DynFlags
               -> Ty
               -> C.Exp      -- ce1
               -> C.Exp      -- ce2
               -> LengthInfo -- rng
               -> Cg C.Exp   -- ce1[ce2...ce2+rng-1]
codeGenArrRead dflags (TArray _ TBit) ce1 ce2 LISingleton
  = do { res <- genSym "bitres"
       ; codeGenDeclGroup res TBit >>= appendDecl
       ; appendStmt $ [cstm| bitRead($ce1,$ce2,& $id:res); |]
       ; return [cexp| $id:res |] }
codeGenArrRead dflags (TArray _ TBit) ce1 ce2 (LILength l)
  = do { res <- genSym "bitarrres"
       ; codeGenDeclGroup res (TArray (Literal l) TBit) >>= appendDecl
       ; appendStmt [cstm| bitArrRead($ce1,$ce2,$int:l,$id:res);  |]
       ; return [cexp| $id:res |] }
codeGenArrRead dflags (TArray _ tbase) ce1 ce2 LISingleton
  = do { b <- isStructPtrType tbase
       ; return $
         if b then [cexp| &($ce1[$ce2])|]
         else [cexp| $ce1[$ce2]|]
       }
codeGenArrRead dflags (TArray _ tbase) ce1 ce2 (LILength _)
  = return [cexp|& ($ce1[$ce2])|]
codeGenArrRead _ ty _ _ _
  = fail ("codeGenArrRead: non-array type " ++ show ty)

codeGenArrWrite :: DynFlags
                -> Ty
                -> C.Exp       -- c1
                -> C.Exp       -- c2
                -> LengthInfo  -- rng
                -> C.Exp       -- c3
                -> Cg ()       -- c1[c2...c2+rng-1] := c3
codeGenArrWrite dflags (TArray _ TBit) ce1 ce2 LISingleton ce3
  = appendStmt [cstm| bitWrite($ce1,$ce2,$ce3);|]
codeGenArrWrite dflags (TArray _ TBit) ce1 ce2 (LILength l) ce3
  = appendStmt $ [cstm| bitArrWrite($ce3,$ce2,$int:l,$ce1); |]
codeGenArrWrite dflags t@(TArray l tbase) ce1 ce2 LISingleton ce3
  = do { cread <- codeGenArrRead dflags t ce1 ce2 LISingleton
       ; assignByVal tbase tbase cread ce3 }

codeGenArrWrite dflags (TArray _ ty) ce1 ce2 (LILength l) ce3
  = appendStmt [cstm| blink_copy((void*)(& $ce1[$ce2]),
                        (void*)($ce3), ($int:l)*sizeof($ty:(codeGenTy ty)));|]
codeGenArrWrite _ ty _ _ _ _
  = fail ("codeGenArrWrite: non-array type " ++ show ty)
