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

module CgTypes ( codeGenTy
               , codeGenTy_qual
               , codeGenTy_val
               , codeGenTyAlg
               , codeGenArrTyPtr
               , codeGenArrTyPtrAlg
               , assignByVal
               , tyBitWidth, tySizeOf_C, tySizeOf, tyBitWidth_ByteAlign
               , codeGenDeclGroup, codeGenDeclVolGroup, codeGenDeclVolGroup_init
               , codeGenDeclGlobalGroups
               , codeGenDeclDef
               , codeGenDeclGlobalDefs
               , codeGenFieldDeclGroup
               , initGroupDef
               , codeGenVal
               , codeGenArrVal
               , namedCType
               , isStructPtrType
               ) where

import Opts
import AstExpr
import AstComp
import PpExpr
import CgMonad

import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Map as M
import Text.PrettyPrint.Mainland
import Data.Maybe

unitTy :: String -> C.Type
unitTy quals = namedCType $ quals ++ " int"

unitVal = [cexp|0|]

tickTy :: C.Type
tickTy = [cty|char|]

procTy :: C.Type
procTy = [cty|char|]


namedCType nm =
  C.Type
    (C.DeclSpec [] [] (C.Tnamed (C.Id nm emptyLoc) [] emptyLoc) emptyLoc)
    (C.DeclRoot emptyLoc)
    emptyLoc
  where emptyLoc = Data.Loc.SrcLoc Data.Loc.NoLoc

codeGenTy    = codeGenTy_qual ""
codeGenTyAlg = codeGenTy_qual "calign"


codeGenTy_val :: Ty -> Cg C.Initializer
codeGenTy_val (TStruct {}) = do { v <- cg_values [VInt 0] >>= (return . fromJust)
                                ; return [cinit| { $v } |] }
codeGenTy_val (TArray {})    = return [cinit| NULL |]
codeGenTy_val _              = return [cinit| 0 |]


codeGenTy_qual :: String -- Type qualifiers
               -> Ty
               -> C.Type
codeGenTy_qual quals ty =
  case ty of
    TBit           -> namedCType $ quals ++ " Bit"
    TBool          -> namedCType $ quals ++ " unsigned char"
    TArray _n ty'  -> codeGenArrTyPtr_qual quals ty
    TDouble        -> namedCType $ quals ++ " double"
    TUnit          -> unitTy quals

    -- TODO: Fail here (don't want to generate code for type vars.)!
    -- TODO: Make codeGenTy monadic so we can give some context error location
    TVar _ -> error "CodeGen error: ambiguous intermediate type variable, missing a type annotation?"

    -- Must keep in sync with CgLut.typeBitWidth and CgLut.lutField.

    TInt bw -> namedCType $ quals ++ " " ++ cgTIntName bw

    -- TODO: Why do we ever call codeGenTy
    -- for these? And why is the ExtBuf giving void?
    -- Old comment said:
    -- ``We should never have to generate code for TBuff but this is
    --   an artifact of the fact that we generate dummy signatures for
    --   everything.''
    TBuff (IntBuf (TVar _))
          -> error "CodeGen error: uninferred intermediate buffer type!"
    TBuff (IntBuf ty')
          -> codeGenTy_qual quals ty'
    TBuff (ExtBuf _ty) -> [cty|void |]

    -- Structs
    TStruct namety _sflds -> namedCType $ quals ++ " " ++ namety

    _ -> error $ show (ppr ty) ++ " type not yet supported in codeGenTy"


-- The bit width that is required to represent a type NB: the actual
-- storage bitwidth might be larger.
-- Example: (arr [5] bit) is represented as an unsigned char (or maybe
-- as a 4 byte unsigned int). But tyBitWidth would return 5
tyBitWidth :: Monad m => Ty -> m Int
tyBitWidth TUnit                 = return 0
tyBitWidth TBit                  = return 1 -- NB not 8
tyBitWidth TBool                 = return 1 -- NB not 8
tyBitWidth (TInt bw)             = bwBitWidth bw
tyBitWidth TDouble               = return 64
tyBitWidth (TArray (Literal n) ty)
  = do { w <- tyBitWidth ty
       ; return (n*w)
       }
tyBitWidth t@(TStruct tn _flds)
  | tn == complexTyName
  = return 64
  | tn == complex8TyName
  = return 16
  | tn == complex16TyName
  = return 32
  | tn == complex32TyName
  = return 64

-- For the moment let's say that we can't calculate bit width of
-- arbitrary structs.  This means that expressions that manipulated
-- structs will not be lutted.
tyBitWidth ty
  -- = error $ "Cannot calculate bit width of type: " ++ show ty
  = fail $ "Cannot calculate bit width of type " ++ show ty

tyBitWidth_ByteAlign :: Monad m => Ty -> m Int
-- This is a similar version but overshoots so that align to byte boundaries
tyBitWidth_ByteAlign TUnit                 = return 0
tyBitWidth_ByteAlign TBit                  = return 8 -- NB not 1
tyBitWidth_ByteAlign TBool                 = return 8 -- NB not 1
tyBitWidth_ByteAlign (TInt bw)             = bwBitWidth bw
tyBitWidth_ByteAlign TDouble               = return 64
tyBitWidth_ByteAlign (TArray (Literal n) ty)
  = do { w <- tyBitWidth ty
       ; return $ (((n*w) + 7) `div` 8) * 8
       }

tyBitWidth_ByteAlign t@(TStruct tn _flds)
  | tn == complexTyName
  = return 64
  | tn == complex8TyName
  = return 16
  | tn == complex16TyName
  = return 32
  | tn == complex32TyName
  = return 64

tyBitWidth_ByteAlign ty
  = fail $ "Cannot calculate bit width of type " ++ show ty

-- Returns the representation of the type in bytes
tySizeOf :: Ty -> Cg (Maybe Int)
tySizeOf TUnit      = return $ Just 1
tySizeOf TBit       = return $ Just 1
tySizeOf TBool      = return $ Just 1
tySizeOf (TInt bw)  = bwBitWidth bw >>= \s -> return $ Just (s `div` 8)
tySizeOf TDouble    = return $ Just (64 `div` 8)
tySizeOf (TArray (Literal n) TBit) = return $ Just (getBitArrayByteLength n)
tySizeOf (TArray (Literal n) ty)
  = do { m <- tySizeOf ty
       ; case m of
           Nothing -> return Nothing
           Just s -> return (Just (n*s))
       }
tySizeOf (TStruct sn _flds)
  -- Note from DV (13/10/14): We could get it directly from _flds but
  -- I am keeping the functionality as it was
  = do { sdef <- lookupTyDefEnv sn
       ; szs  <- mapM (tySizeOf . snd) (struct_flds sdef)
       ; return (sequence szs >>= (return . sum)) }

tySizeOf TVoid  = return $ Just 0
tySizeOf _other = return Nothing

tySizeOfCg :: Ty -> Cg Int
tySizeOfCg ty = do { mi <- tySizeOf ty
                   ; case mi of
                      Nothing ->
                        fail $ "Can't determine tySizeOf of type: " ++ show ty
                      Just i -> return i
                   }

tySizeOf_C :: Ty -> C.Exp
tySizeOf_C (TArray (Literal n) TBit)
 = [cexp| $int:(getBitArrayByteLength n)|]
tySizeOf_C (TArray (Literal n) t)
 = [cexp| $int:n * $(tySizeOf_C t) |]
tySizeOf_C t
 = [cexp| sizeof($ty:(codeGenTy t)) |]


bwBitWidth BW8           = return 8
bwBitWidth BW16          = return 16
bwBitWidth BW32          = return 32
bwBitWidth BW64          = return 64
bwBitWidth (BWUnknown _) = return 32 -- Defaulting!


codeGenArrTyPtr :: Ty -> C.Type
codeGenArrTyPtr = codeGenArrTyPtr_qual ""

codeGenArrTyPtrAlg :: Ty -> C.Type
codeGenArrTyPtrAlg = codeGenArrTyPtr_qual "calign"


codeGenArrTyPtr_qual :: String -- Qualifier
                     -> Ty     -- An array type
                     -> C.Type
codeGenArrTyPtr_qual quals ty
 | TArray (Literal {}) t <- ty = aux t
 | TArray (NVar {})    t <- ty = aux t
 | otherwise
 = error "codeGenArrTyPtr_qual: unexpected non-array type"
 where aux t
        | TBit <- t
        = [cty| $ty:(namedCType $ quals ++ " BitArrPtr") |]
        | otherwise
        = [cty| $ty:(codeGenTy_qual quals t)* |]


codeGenVal :: Val -> Cg C.Exp
codeGenVal v =
  case v of
    VBit True   -> return [cexp|1|]
    VBit False  -> return [cexp|0|]
    VInt i      -> return [cexp|$lint:i|] -- NB: $lint instead of $int to accommodate 64-bit constants
    VDouble d   -> return [cexp|$double:(toRational d)|]
    VBool True  -> return [cexp|$uint:(1)|]
    VBool False -> return [cexp|$uint:(0)|]
    VString str -> return [cexp|$string:(str)|]
    VUnit       -> return unitVal

------------------------------------------------------------------------------
-- | Arrays related code
------------------------------------------------------------------------------
-- Note: for SIMD/SSE instructions we need to declare 16-bytes aligned arrays
-- To do that we have special versions of codeGenTy and variants
-- with suffic Alg which appends the alignment keyword in front of decls.


-- Get the length of a bitarray in bytes.
-- Coupled to the implementation in _csrc/bit.c
getBitArrayByteLength n = (n + 7) `div` 8


-- Arrays more than 128K wide allocated on the heap
mAX_STACK_ALLOC = 1024 * 32

-- Is this type a *struct* represented by pointer?
isStructPtrType :: Ty -> Cg Bool
isStructPtrType ty
  | isArrayTy ty
  = return False
  | otherwise
  = tySizeOfCg ty >>= isLargeTy

isLargeTy :: Int -> Cg Bool
isLargeTy ty_size
  = do { mAX_STACK_ALLOC <- getMaxStackAlloc
       ; return (ty_size > mAX_STACK_ALLOC) }

codeGenDeclGroup_qual :: String      -- Type qualifiers
                      -> String      -- Id
                      -> Ty          -- Type to be declared with
                      -> Maybe C.Exp -- Initialization
                      -> Cg (C.InitGroup, (C.InitGroup, Maybe (C.Stm)))
-- We return two different representations
-- (a) the initgroup
-- (b) just the declaration and in a separate statement the initialization
codeGenDeclGroup_qual quals v ty vinit
  = do { alloc_as_ptr <- isStructPtrType ty
       ; case ty of
            TArray (Literal n) t
               -> do { s <- tySizeOfCg ty
                     ; alloc_as_ptr <- isLargeTy s
                     ; let vlen       = [cexp|$int:(arr_len t n)|]
                           tbase      = arr_base t
                           tdecl_base = aligned_base t
                     ; arr_decl alloc_as_ptr v vlen tbase tdecl_base
                     }

            -- dynamic allocation for variable-size arrays
            TArray (NVar siz_nm) t
               -> do { let vlen = case t of
                                    TBit -> [cexp|($id:siz_nm + 7) >> 3|]
                                    _    -> [cexp|$id:siz_nm|]
                           tbase = arr_base t
                           tdecl_base = aligned_base t
                     ; arr_decl True v vlen tbase tdecl_base
                     }

            _other
               | Just val <- vinit
               -> let ig = [cdecl| $ty:(codeGenTy_qual quals ty) $id:v = $val; |]
                  in return (ig, (ig, Nothing))
               | alloc_as_ptr
               -> let cty = codeGenTy_qual quals ty
                      cty_plain = codeGenTy_qual "" ty
                  in do { newHeapAlloc
                        ; heap_context <- getHeapContext
                        ; let ig1  = [cdecl| $ty:cty * $id:v =
                                         ($ty:cty_plain *) wpl_alloca($id:heap_context, sizeof($ty:cty_plain));|]
                              ig2  = [cdecl| $ty:cty * $id:v;|]
                        ; let stmt = [cstm| $id:v = ($ty:cty_plain *) wpl_alloca($id:heap_context, sizeof($ty:cty_plain)); |]
                        ; return (ig1, (ig2, Just stmt))
                        }

               | otherwise
               -> let ig = [cdecl| $ty:(codeGenTy_qual quals ty) $id:v;|]
                  in return (ig, (ig, Nothing))
        }

  where arr_len TBit n = getBitArrayByteLength n
        arr_len _    n = n
        arr_base TBit = [cty| unsigned char |]
        arr_base t    = codeGenTy_qual "" t

        aligned_base TBit = namedCType $ "calign" ++ " unsigned char"
        aligned_base t    = codeGenTy_qual "calign" t

        arr_decl alloc_as_ptr v len t decl_t
          | Just val <- vinit -- NB: "braces" around val as it's a Seq!
          = let ig = [cdecl| $ty:decl_t $id:v[$len] = {$val}; |]
            in return (ig, (ig, Nothing))
          | alloc_as_ptr
          = do { newHeapAlloc
               ; heap_context <- getHeapContext
               ; let ig1  = [cdecl| $ty:decl_t *$id:v =
                                       ($ty:t *) wpl_alloca($id:heap_context, $len * sizeof($ty:t)); |]
                     ig2  = [cdecl| $ty:decl_t *$id:v; |]
                     stmt = [cstm| $id:v =
                                       ($ty:t *) wpl_alloca($id:heap_context, $len * sizeof($ty:t)); |]
               ; return (ig1, (ig2, Just stmt))
               }
          | otherwise
          = let ig = [cdecl| $ty:decl_t $id:v[$len]; |]
            in return (ig, (ig, Nothing))




codeGenFieldDeclGroup :: String      -- Id
                      -> Ty          -- Type to be declared with
                      -> C.FieldGroup
codeGenFieldDeclGroup v ty =
  case ty of
    TArray (Literal n) t -> arr_decl v (arr_siz t n) (arr_base t)
    TArray (NVar n) t    -> error "codeGenFieldDeclGroup: struct array field with unknown size!?"
    _other               -> [csdecl| $ty:(codeGenTy ty) $id:v; |]

  where arr_siz TBit n = getBitArrayByteLength n
        arr_siz _    n = n
        arr_base TBit  = [cty| unsigned char |]
        arr_base t     = codeGenTy t
        arr_decl v sz t = [csdecl| $ty:t $id:v[$int:sz]; |]



codeGenDeclGroup :: String -> Ty -> Cg C.InitGroup
codeGenDeclGroup v ty = return . fst =<< codeGenDeclGroup_qual "calign" v ty Nothing

codeGenDeclVolGroup :: String -> Ty -> Cg C.InitGroup
codeGenDeclVolGroup v ty
  = return . fst =<< codeGenDeclGroup_qual "volatile" v ty Nothing

codeGenDeclVolGroup_init :: String -> Ty -> C.Exp -> Cg C.InitGroup
codeGenDeclVolGroup_init v ty e
  = return . fst =<< codeGenDeclGroup_qual "volatile" v ty (Just e)



codeGenDeclDef :: String -> Ty -> Cg C.Definition
codeGenDeclDef v ty
  = do { ig <- codeGenDeclGroup v ty
       ; return $ initGroupDef ig
       }

initGroupDef :: C.InitGroup -> C.Definition
initGroupDef ig = C.DecDef ig Data.Loc.noLoc


codeGenDeclGlobalGroups :: DynFlags -> [GName Ty] -> Cg [C.InitGroup]
codeGenDeclGlobalGroups dflags = mapM $ \nm ->
  codeGenDeclGroup (name nm) (nameTyp nm)

codeGenDeclGlobalDefs :: DynFlags
                      -> [MutVar]
                      -> Cg ([C.Definition],[C.Stm])
codeGenDeclGlobalDefs dflags defs
 = do { defs_inits <- mapM (\MutVar{..} ->
                      do { let ty = nameTyp mutVar
                         ; (_,(ig,mstm)) <- codeGenDeclGroup_qual "calign" (name mutVar) ty Nothing
                         ; let id = C.DecDef ig Data.Loc.noLoc
                         ; case mstm of Nothing   -> return (id,[])
                                        Just init -> return (id,[init]) }) defs
      ; let (defs,inits) = unzip defs_inits
      ; return (defs, concat inits)
      }

assignByVal :: Ty -> Ty -> C.Exp -> C.Exp -> Cg ()
assignByVal ty@(TArray (Literal n) TBit) _ ce1 ce2
  | n `mod` 8 == 0
  = if n == 8
    then appendStmt [cstm| *($ce1) = *($ce2); |]
    else appendStmt [cstm|blink_copy($ce1,$ce2,$int:(n `div` 8));|]
  | otherwise
  = appendStmt [cstm|bitArrRead($ce2,0,$int:n,$ce1);|]

assignByVal ty@(TArray (Literal n) t) _ ce1 ce2 =
    appendStmt [cstm|blink_copy($ce1, $ce2, $int:n*sizeof($ty:(codeGenTy t)));|]

assignByVal ty@(TArray (NVar n) TBit) _ ce1 ce2
  =  appendStmt [cstm|bitArrRead($ce2,0,$id:n,$ce1);|]

assignByVal ty@(TArray (NVar n) t) _ ce1 ce2 =
    appendStmt [cstm|blink_copy($ce1, $ce2,
                      $id:n * sizeof($ty:(codeGenTy t)));|]

assignByVal TDouble TDouble ce1 ce2 =
    appendStmt [cstm|$ce1 = $ce2;|]

-- Works for structs
assignByVal t t' ce1 ce2 =
  do { b <- isStructPtrType t
     ; if b then
         -- If represented as pointers then do a memcopy
         appendStmt [cstm|blink_copy($ce1, $ce2, sizeof($ty:(codeGenTy t)));|]
         -- Else just do assignment
       else just_assign ce1 ce2 }
  where just_assign ce1 ce2
            -- just an optimization for expensive complex assignments
          | isComplexTy t && is_c_var ce2
          = do appendStmt [cstm|$ce1.re = $ce2.re;|]
               appendStmt [cstm|$ce1.im = $ce2.im;|]

          | otherwise
          = appendStmt [cstm|$ce1 = $ce2;|]

        is_c_var (C.Var (C.Id {}) _) = True
        is_c_var _                   = False


-- NB: Only assigns by ref for arrays
assignByRef :: Ty -> C.Exp -> C.Exp -> C.Stm
assignByRef ty ce1 ce2 = [cstm| $(ce1) = $(ce2);|]


codeGenArrVal :: String -> Ty -> [Val] -> Cg C.InitGroup
codeGenArrVal name ty@(TArray _ TBit) vals
  = do { let mvs = cg_bitvalues vals
       ; return . fst =<< codeGenDeclGroup_qual "" name ty mvs }
codeGenArrVal name ty@(TArray _ _ty) vals
  = do { mvs <- cg_values vals
       ; return . fst =<< codeGenDeclGroup_qual "" name ty mvs }
codeGenArrVal name _ty vals
  = fail "codeGenArrVal: non-array type!"

-- Generates a list of vales
cg_values [] = return Nothing
cg_values (h:hs)
  = do { c <- codeGenVal h -- Why is this monadic?
       ; m_cs <- cg_values hs
       ; case m_cs of
           Nothing -> return (Just c)
           Just cs -> return (Just [cexp|$c, $cs|]) }

cg_bitvalues [] = Nothing
cg_bitvalues xs
  = case cg_bitvalues (drop 8 xs) of
           Just bs -> Just $ [cexp|$(first_cexp),$(bs)|]
           Nothing -> Just $ [cexp|$(first_cexp)|]
  where to_num = foldr (\(VBit t) s -> if t then s*2 + 1 else s*2) 0
        first = take 8 xs
        first_num = to_num (first ++ replicate (8 - length first) (VBit False))
        first_cexp = [cexp|$int:(first_num) |]

