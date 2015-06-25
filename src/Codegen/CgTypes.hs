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

module CgTypes ( bwBitWidth
               , namedCType, namedCType_
               , codeGenTyOcc, codeGenTyOcc_
               , tySizeOfApprox, tySizeOf_C

               , isStructPtrType
               , isPtrType

               , HowToInit ( .. ), isInitWith 
               , codeGenDeclGroup
               , appendCodeGenDeclGroup
               , codeGenDeclVolGroup
               , codeGenFieldDeclGroup
               , codeGenDeclDef
               , DeclPkg ( .. )

               , assignByVal
               , cgBitArrRead, cgBreakDown

               , cgInitVal
               , initGroupDef
               , codeGenVal 

               , cgBitValues
               , cgNonBitValues
               
               , expValMbs 

               , appendDeclPkg
               , appendTopDeclPkg
               , shouldAllocAsPtr

               ) where

import Opts
import AstExpr
import AstComp
import AstUnlabelled 
import PpExpr
import CgMonad

import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import qualified Language.C.Pretty as P
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ 
import Data.Maybe

import Outputable
import Utils


{------------------------------------------------------------------------
  Sizes of various types
------------------------------------------------------------------------}

-- | Get the length of a bitarray in bytes. 
--   NB: this is coupled to the implementation in csrc/bit.c
bitArrByteLen :: Int -> Int 
bitArrByteLen n = (n + 7) `div` 8

bwBitWidth :: BitWidth -> Int
bwBitWidth BW8           = 8
bwBitWidth BW16          = 16
bwBitWidth BW32          = 32
bwBitWidth BW64          = 64
bwBitWidth (BWUnknown _) = 32 -- ^ Defaulting to 32

-- ^ apx = True  => approximate, due to unknown alignment constraints)
-- ^     = False => precise, in which case we may fail.
tySizeOf :: Bool -> Ty -> Int
tySizeOf apx = go 
  where
     go TUnit       = 1
     go TBit        = 1
     go TBool       = 1
     go (TInt bw _) = ((bwBitWidth bw) + 7) `div` 8
     go TDouble     = (64 `div` 8)
     go (TArray (Literal n) TBit) = bitArrByteLen n
     go (TArray (Literal n) ty)   = n * go ty
     go (TStruct sn flds)
       | sn == complexTyName   = 8   -- NB: platform-dependent
       | sn == complex8TyName  = 2   -- but ok with GCC and VS in
       | sn == complex16TyName = 4   -- Cygwin and in VS
       | sn == complex32TyName = 8
       | sn == complex64TyName = 16
       | apx -- ^ approximate on? 
       = sum $ map (go . snd) flds
       | otherwise
       = panicStr "Can't compute tySizeOf: compiler/architecture specific!"
     go TVoid 
       = panicStr "Can't compute go(TVoid)"
     go other_ty = 
       panic $ text "go: Cannot determine size of: " <+> ppr other_ty

tySizeOfApprox :: Ty -> Int
tySizeOfApprox ty = tySizeOf True ty

tySizeOf_C :: Ty -> C.Exp
tySizeOf_C (TArray (NVar n) TBit)  = [cexp| (($id:n + 7) >> 3) |]
tySizeOf_C (TArray (NVar n) ty)    = [cexp| $id:n * $exp:(tySizeOf_C ty)|]
tySizeOf_C (TArray (Literal n) ty) = [cexp| $int:n * $exp:(tySizeOf_C ty)|]
tySizeOf_C (TStruct sn _) = [cexp| sizeof($id:sn)|]
tySizeOf_C ty             = [cexp| $int:(tySizeOf False ty)|]


{------------------------------------------------------------------------
  Language-c-quote infrastructure and fixed types
------------------------------------------------------------------------}

namedCType :: String -> C.Type
namedCType nm =
  C.Type
    (C.DeclSpec [] [] (C.Tnamed (C.Id nm noCLoc) [] noCLoc) noCLoc)
    (C.DeclRoot noCLoc)
    noCLoc

initGroupDef :: C.InitGroup -> C.Definition
initGroupDef ig = C.DecDef ig Data.Loc.noLoc

noCLoc :: Data.Loc.SrcLoc
noCLoc = Data.Loc.SrcLoc Data.Loc.NoLoc

-- | Initializers that consist of just an expression
exprInit :: C.Exp -> C.Initializer
exprInit e = C.ExpInitializer e noCLoc

-- | Compound initializers
compInit :: [C.Initializer] -> C.Initializer
compInit cis
  = C.CompoundInitializer (map aux cis) noCLoc
  where aux i = (Nothing, i)


-- | Unit value 
unitVal :: C.Exp
unitVal = [cexp|0|]

tickTy :: C.Type
tickTy = [cty|char|]

procTy :: C.Type
procTy = [cty|char|]

{------------------------------------------------------------------------
  Types to C-types
------------------------------------------------------------------------}

type Quals = String 

-- | Unit type 
unitTy :: Maybe Quals -> C.Type
unitTy quals = namedCType_ quals "int"

namedCType_ :: Maybe Quals -> String -> C.Type
namedCType_ Nothing  s = namedCType s
namedCType_ (Just q) s = namedCType $ q ++ " " ++ s

codeGenTyOcc_ :: Maybe Quals -> Ty -> C.Type
-- ^ Occurrence type 
codeGenTyOcc_ q ty =
  case ty of
    TBit             -> namedCType_ q "Bit"
    TBool            -> namedCType_ q "unsigned char"
    TArray _n ty'    -> codeGenArrTyPtrOcc_ q ty
    TDouble          -> namedCType_ q "double"
    TUnit            -> unitTy q
    TInt bw sg       -> namedCType_ q (cgTIntName bw sg)
    TStruct sname _  -> namedCType_ q sname
 
    -- Buffer types. NB: we should never have to generate code for
    -- those but we do generate dummy signatures (e.g. yld variables)
    -- for every component so we could choose *any* type (here int)
    TBuff (ExtBuf _ty) -> [cty|int |]
    TBuff (IntBuf _ty) -> [cty|int |]

    _ -> panic $
         ppr ty <+> text " type not supported in code generation."

codeGenArrTyPtrOcc_ :: Maybe Quals -> Ty -> C.Type 
codeGenArrTyPtrOcc_ q ty
 | TArray (Literal {}) t <- ty = aux t
 | TArray (NVar {})    t <- ty = aux t
 | otherwise
 = panicStr $ "codeGenArrTyPtrOcc: non array type " ++ show ty
 where aux TBit = [cty| $ty:(namedCType_ q "BitArrPtr")  |]
       aux t    = [cty| $ty:(codeGenTyOcc_ q t) * |]

codeGenArrTyPtrOcc :: Ty -> C.Type
codeGenArrTyPtrOcc t = codeGenArrTyPtrOcc_ Nothing t

codeGenTyOcc :: Ty -> C.Type
codeGenTyOcc t = codeGenTyOcc_ Nothing t

{------------------------------------------------------------------------
  When should values of a type declared as pointers? (heap-allocated)
------------------------------------------------------------------------}

-- | Is this type a pointer type?
isPtrType :: Ty -> Bool
isPtrType ty = isArrayTy ty || isStructPtrType ty

-- | Is this type a /struct/ represented by a pointer
isStructPtrType :: Ty -> Bool
isStructPtrType ty
  | isArrayTy ty = False
  | otherwise    = isLargeTy (tySizeOfApprox ty)

isLargeTy :: Int -> Bool
-- ^ Determine if this type is large. 
-- ^ If it's a large type then values 
-- ^ of this type will be declared as pointers and are heap allocated
isLargeTy ty_size = ty_size > cMAX_STACK_ALLOC


shouldAllocAsPtr :: Ty -> Bool
-- | Should we allocate as pointer
shouldAllocAsPtr (TArray (NVar {}) _) = True
shouldAllocAsPtr other = isLargeTy (tySizeOfApprox other)


{------------------------------------------------------------------------
  Value code generation
------------------------------------------------------------------------}

mIN_INT32 :: Integer
mIN_INT32 = -2147483648
mAX_INT32 :: Integer
mAX_INT32 = 2147483647

mIN_UINT32 :: Integer
mIN_UINT32 = -2147483648
mAX_UINT32 :: Integer
mAX_UINT32 = 2147483647

codeGenVal :: Val -> C.Exp
-- ^ Generate code for a value
codeGenVal v =
  case v of
    VInt i Signed
      | i <= mAX_INT32 && mIN_INT32 <= i
      -> [cexp|$int:i|]
      | otherwise -> [cexp|$lint:i|]
    VInt i Unsigned
      | i <= mAX_UINT32 && mIN_UINT32 <= i
      -> [cexp|$uint:i|]
      | otherwise -> [cexp|$ulint:i|]
    VBit True   -> [cexp| 1                     |]
    VBit False  -> [cexp| 0                     |]
    VDouble d   -> [cexp| $double:(toRational d)|]
    VBool True  -> [cexp| $uint:(1)             |]
    VBool False -> [cexp| $uint:(0)             |]
    VString str -> [cexp| $string:(str)         |]
    VUnit       -> unitVal

cgInitVal :: Ty -> C.Initializer
-- ^ Generate an initial value (all zeros) for declarations
cgInitVal ty = go ty
  where go (TStruct _ [])      = compInit [exprInit [cexp|0|]]
        go (TStruct _ fld_tys) = compInit (map (go . snd) fld_tys)
        go (TArray _ ty_base)  = compInit [go ty_base]
        go _t                  = exprInit [cexp|0|]

{------------------------------------------------------------------------
  Variable declarations
------------------------------------------------------------------------}

-- | C-declared variables
type CVar = String 

-- | When declaring a new variable we can choose how to initialize it
-- ^ by passing a @HowToInit@ value. @ZeroOut@ zeroes the relevant
-- ^ memory, whereas @InitWith init@ allows you to pass in a C-initializer.
-- 
data HowToInit = ZeroOut 
               | InitWith C.Initializer

-- | An InitGroup or Definition plus any statements for initialization
data DeclPkg d = DeclPkg { dp_decl :: d, dp_init :: [C.Stm] }

isInitWith :: HowToInit -> Bool
isInitWith (InitWith {}) = True
isInitWith _ = False


arrBaseCType :: Maybe Quals -> Ty -> C.Type 
-- ^ Array base type for declaration
arrBaseCType q (TArray _ TBit) = namedCType_ q "unsigned char"
arrBaseCType q (TArray _ tb)   = arrBaseCType q tb
arrBaseCType q tother          = codeGenTyOcc_ q tother

arrIdxCStack :: Ty -> [Int]
-- ^ Index stack for declaration
arrIdxCStack = go
  where 
    go (TArray (Literal n) TBit) = [bitArrByteLen n]
    go (TArray (Literal n) ty)   = n : go ty
    go _ty                       = []

cgDeclAllocPtr :: Quals -> CVar -> Ty -> Cg (DeclPkg C.InitGroup)
-- ^ Declare and heap-allocate (for large types or dynamic arrays)
cgDeclAllocPtr quals v ty = do 
  -- Are we already inside an allocator's frame?
  inside_alloc_ctx <- isInsideAllocFrame
  newHeapAlloc
  heap_ctx <- getHeapContext
  -- If yes then proceed as usual -- guaranteed no memory leak
  if inside_alloc_ctx then do
     let ig   = [cdecl| $ty:qty *$id:v = NULL;|]
         stmt = [cstm| $id:v = ($ty:nonqty *) 
                  wpl_alloca($id:heap_ctx, $exp:(tySizeOf_C ty));|]
     return $ DeclPkg ig [stmt]
  else 
     -- | This is a naked "global" wpl allocation
     -- ^ hence we can't be allocating a dynamic array whose length is 
     -- ^ say the argument of a function. Here is the plan then: 
     -- ^   (i) create global storage + allocation stm in wpl_global_init()
     -- ^   (ii) declare variable and set it to point to storage.
     do glob_store <- freshVar ("global_storage_" ++ v)
        newHeapAlloc
        -- Declare global storage
        appendTopDecl [cdecl| $ty:qty *$id:glob_store = NULL;|]
        -- Emit allocation code in wpl_global_init
        addGlobalWplAllocated [cstm| $id:glob_store = ($ty:nonqty *) 
              wpl_alloca($id:heap_ctx, $exp:(tySizeOf_C ty));|]
        -- Emit declaration and just set pointer 
        let ig   = [cdecl| $ty:qty *$id:v = NULL;|]
            stmt = [cstm| $id:v = $id:glob_store;|]
        return $ DeclPkg ig [stmt]

  where qty    = arrBaseCType (Just quals) ty 
        nonqty = arrBaseCType Nothing ty

cgDeclStatic :: Quals     -- ^ qualifiers
             -> CVar      -- ^ C variable
             -> Ty
             -> HowToInit
             -> C.InitGroup
-- ^ Declare a variable of array type statically
cgDeclStatic quals v ty mb_init =
  case idx_stack of 
    []            -> [cdecl| $ty:base_ty $id:v = $init:dflt;|]
    [i]           -> [cdecl| $ty:base_ty $id:v[$int:i] = $init:dflt;|]
    [i1,i2]       -> [cdecl| $ty:base_ty $id:v[$int:i1][$int:i2] = $init:dflt;|]
    [i1,i2,i3]    -> [cdecl| $ty:base_ty $id:v[$int:i1][$int:i2][$int:i3] = $init:dflt;|]
    [i1,i2,i3,i4] -> [cdecl| $ty:base_ty $id:v[$int:i1][$int:i2][$int:i3][$int:i4] = $init:dflt;|]
    _t -> panicStr "Code generator supports nested arrays of nesting level <= 4"
  where idx_stack = arrIdxCStack ty
        base_ty   = arrBaseCType (Just quals) ty
        dflt      = case mb_init of ZeroOut     -> cgInitVal ty
                                    InitWith it -> it

codeGenFieldDeclGroup :: FldName -> Ty -> C.FieldGroup
codeGenFieldDeclGroup v ty = 
  case idx_stack of 
    []            -> [csdecl| $ty:base_ty $id:v; |]
    [i]           -> [csdecl| $ty:base_ty $id:v[$int:i]; |]
    [i1,i2]       -> [csdecl| $ty:base_ty $id:v[$int:i1][$int:i2]; |]
    [i1,i2,i3]    -> [csdecl| $ty:base_ty $id:v[$int:i1][$int:i2][$int:i3];|]
    [i1,i2,i3,i4] -> [csdecl| $ty:base_ty $id:v[$int:i1][$int:i2][$int:i3][$int:i4];|]
    _t -> panicStr "Code generator supports nested arrays of nesting level <= 4"
  where idx_stack = arrIdxCStack ty
        base_ty   = arrBaseCType Nothing ty

codeGenDeclGroup_qual :: Quals 
                      -> CVar -> Ty -> HowToInit -> Cg (DeclPkg C.InitGroup)
codeGenDeclGroup_qual quals v ty mb_init
  | shouldAllocAsPtr ty && not (isInitWith mb_init) 
    -- ^ If the type is big and we do not have a programmer-supplied 
    --   initializer then we can simply do a heap alllocation
  = cgDeclAllocPtr quals v ty
  | otherwise 
    -- ^ We have given a static initializer or the type is small
  = return $ DeclPkg (cgDeclStatic quals v ty mb_init) []

-- | codeGenDeclGroup wrappers
codeGenDeclGroup, codeGenDeclVolGroup :: CVar -> Ty -> HowToInit 
                                      -> Cg (DeclPkg C.InitGroup)
codeGenDeclGroup    = codeGenDeclGroup_qual "calign"
codeGenDeclVolGroup = codeGenDeclGroup_qual "volatile"

-- | Init group to definitions
codeGenDeclDef :: CVar -> Ty -> HowToInit -> Cg (DeclPkg C.Definition)
codeGenDeclDef v ty minit = do
  (DeclPkg ig stms) <- codeGenDeclGroup v ty minit
  return $ DeclPkg (initGroupDef ig) stms


appendCodeGenDeclGroup :: CVar -> Ty -> HowToInit -> Cg ()
-- | Append declaration and initialization in the current block
appendCodeGenDeclGroup x ty minit = do
  (DeclPkg ig stms) <- codeGenDeclGroup x ty minit
  appendDecl ig
  appendStmts stms

appendDeclPkg :: DeclPkg C.InitGroup -> Cg ()
appendDeclPkg (DeclPkg ig stms) = do 
  appendDecl ig
  appendStmts stms

appendTopDeclPkg :: DeclPkg C.InitGroup -> Cg ()
appendTopDeclPkg (DeclPkg ig stms) = do 
  appendTopDecl ig
  appendStmts stms


{------------------------------------------------------------------------
  Assign by value
------------------------------------------------------------------------}

cgBreakDown :: Int -> C.Exp -> [(C.Exp, Int)]
-- ^ Breakdown to 128,64,32,16,8 ... (later we should also add SSE 128/256)
-- ^ Also returns the width of each breakdown
cgBreakDown m ptr  = go m 0
  where 
    go :: Int -> Int -> [(C.Exp, Int)]
    go n off
      | n-128 >= 0 = ([cexp|$offptr |],128): go (n-128) (off+16)
      | n-64  >= 0 = ([cexp|(typename uint64 *) $offptr|],64):go (n-64) (off+8)
      | n-32  >= 0 = ([cexp|(typename uint32 *) $offptr|],32):go (n-32) (off+4)
      | n-16  >= 0 = ([cexp|(typename uint16 *) $offptr|],16):go (n-16) (off+2)
      | n-8   >= 0 = ([cexp|(typename uint8  *) $offptr|], 8):go (n-16) (off+1)
      | n > 0      = [([cexp|$offptr|],8)]
      | otherwise  = []
      where offptr = [cexp| (typename BitArrPtr)($ptr + $int:off)|]

cgBreakDown64 :: Int -> C.Exp -> [(C.Exp, Int)]
-- ^ Breakdown to 64,32,16,8 ... (later we should also add SSE 128/256)
-- ^ Also returns the width of each breakdown
cgBreakDown64 m ptr  = go m 0
  where 
    go :: Int -> Int -> [(C.Exp, Int)]
    go n off
      | n-64  >= 0 = ([cexp|(typename uint64 *) $offptr|],64):go (n-64) (off+8)
      | n-32  >= 0 = ([cexp|(typename uint32 *) $offptr|],32):go (n-32) (off+4)
      | n-16  >= 0 = ([cexp|(typename uint16 *) $offptr|],16):go (n-16) (off+2)
      | n-8   >= 0 = ([cexp|(typename uint8  *) $offptr|], 8):go (n-16) (off+1)
      | n > 0      = [([cexp|$offptr|],8)]
      | otherwise  = []
      where offptr = [cexp| (typename BitArrPtr)($ptr + $int:off)|]


cgBitArrRead :: C.Exp -> Int -> Int -> C.Exp -> Cg ()
-- ^ Pre: pos and len are multiples of 8; src, tgt are of type BitArrPtr
cgBitArrRead src_base pos len tgt
  | len > 288
  = appendStmt [cstm| blink_copy((void *) $tgt, (void *) $src, $int:byte_len);|]
  | otherwise
  = sequence_ $ map (appendStmt . mk_stmt) (zip src_ptrs tgt_ptrs)
  where
    src_ptrs              = cgBreakDown64 len src
    tgt_ptrs              = cgBreakDown64 len tgt
    mk_stmt ((s,w),(t,_)) = assert "cgBitArrRead" (w < 128) $ 
                            [cstm| * $t = * $s;|]
    sidx                  = pos `div` 8
    src                   = [cexp| & ($src_base[$int:sidx])|]
    byte_len              = (len+7) `div` 8


assignByVal :: Ty -> C.Exp -> C.Exp -> Cg ()
assignByVal (TArray numexp base) ce1 ce2
  = asgn_arr numexp base ce1 ce2
assignByVal t ce1 ce2
  | isStructPtrType t = asgn_memcpy t ce1 ce2
  | otherwise         = asgn_direct t ce1 ce2

asgn_arr :: NumExpr -> Ty -> C.Exp -> C.Exp -> Cg ()
asgn_arr (Literal n) TBit ce1 ce2
  = cgBitArrRead ce2 0 (((n+7) `div` 8)*8) ce1
asgn_arr (NVar n) TBit ce1 ce2
  = appendStmt [cstm|bitArrRead($ce2,0,$id:n,$ce1);|]
asgn_arr (Literal n) ty ce1 ce2
  | n > 0 && n < 4 -- Small ones just to direct assignment
  = mapM_ (\i -> assignByVal ty [cexp| $ce1[$int:i]|] [cexp| $ce2[$int:i]|]) [0..(n-1)]
  | otherwise
  = appendStmt [cstm|blink_copy($ce1, $ce2, $int:n * $(tySizeOf_C ty));|]
asgn_arr (NVar n) ty ce1 ce2
  = appendStmt [cstm|blink_copy($ce1, $ce2, $id:n * $(tySizeOf_C ty));|]

asgn_memcpy :: Ty -> C.Exp -> C.Exp -> Cg ()
asgn_memcpy t ce1 ce2 
  = appendStmt [cstm|blink_copy($ce1, $ce2, $(tySizeOf_C t));|]

asgn_direct :: Ty -> C.Exp -> C.Exp -> Cg ()
asgn_direct ty ce1 ce2
  | isComplexTy ty && is_c_var ce2 
    -- ^ Assignments in the fields of complex numbers seem to 
    -- ^ result to much better performance than assigning the 
    -- ^ full struct, hence we special-case here. 
  = do appendStmt [cstm|$ce1.re = $ce2.re;|]
       appendStmt [cstm|$ce1.im = $ce2.im;|]
  | otherwise
  = appendStmt [cstm|$ce1 = $ce2;|]
  where -- DV: Seems defensive. Why is this necessary?
        is_c_var (C.Var (C.Id {}) _) = True
        is_c_var _                   = False


{------------------------------------------------------------------------
  Values and initialization
------------------------------------------------------------------------}

cgBitValues :: [Val] -> C.Initializer
cgBitValues = compInit . fromJust . go
  where 
    go [] = Nothing
    go xs = case go (drop 8 xs) of
              Nothing -> Just [exprInit [cexp| $int:first_num|]]
              Just bs -> Just $ exprInit [cexp| $int:first_num|] : bs
      where
        to_num = foldr action (0::Int)
        first  = take 8 xs
        bit0   = VBit False
        first_num = to_num (first ++ replicate (8 - length first) bit0)
        action (VBit t) s = if t then s*2 + 1 else s*2
        action _ _        = error "cgBitValues: not a bit value!"

cgNonBitValues :: [Val] -> C.Initializer
cgNonBitValues = compInit . go
  where go []     = panicStr "cgValues: empty"
        go [x]    = [exprInit (codeGenVal x)]
        go (x:xs) = exprInit (codeGenVal x) : go xs

expValMbs :: [Exp] -> Maybe [Val]
expValMbs = mapM expValMb 

expValMb :: Exp -> Maybe Val
expValMb e = case unExp e of { EVal t v -> return v; _ -> Nothing }





{- 
appendDeclArrVal :: CVar -> Ty -> [Exp] -> Cg ()
-- Pre: all values
appendDeclArrVal x ty einits = cg_arr_val x ty einits


codeGenArrVal :: CVar -> Ty -> [Exp] -> Cg (C.InitGroup,[C.Stm])
codeGenArrVal x ty einits
  | all is_eval einits
  = cg_arr_val x ty einits
  | otherwise
  = cg_arr_exp x ty einits

cg_arr_val :: CVar -> Ty -> [Exp] -> Cg (C.InitGroup,[C.Stm])
-- ^ Precondition: all are values
cg_arr_val name ty@(TArray _ TBit) vals = do 
  let mvs = cg_bitvalues (map unExp vals)
  codeGenDeclGroup_qual "" name ty mvs
cg_arr_val name ty@(TArray _ _ty) vals = do
  mvs <- cg_values (map unExp vals)
  codeGenDeclGroup_qual "" name ty mvs
cg_arr_val name _ty vals = fail "codeGenArrVal: non-array type!"

-- | Return a packed list of bits
cg_bitvalues [] = Nothing
cg_bitvalues xs
  = case cg_bitvalues (drop 8 xs) of
           Just bs -> Just $ [cexp|$(first_cexp),$(bs)|]
           Nothing -> Just $ [cexp|$(first_cexp)|]
  where to_num = foldr action (0::Int)
        first = take 8 xs
        first_num = to_num (first ++ replicate (8 - length first) 
                                               (EVal TBit (VBit False)))
        first_cexp = [cexp|$int:(first_num) |]
        action (EVal _ (VBit t)) s = if t then s*2 + 1 else s*2
        action _ _ = error "cg_bitvalues: non-value bit expresion!" 

-- | Return a list of values (non-bit)
cg_values [] = Nothing
cg_values (h:hs) = do
  let (EVal t v) = unExp h
       c = codeGenVal t v
  case cg_values hs of
    Nothing -> Just c
    Just cs -> Just [cexp|$c, $cs|]


cg_arr_exp :: CVar -> Ty -> [Exp] -> Cg (C.InitGroup,[C.Stm])
cg_arr_exp x ty es
  = do 





-- Values are Vals and structs of Values
codeGenValue (EVal _ v)    = codeGenVal v
codeGenValue (EStruct _ fes) = do
  vs <- cg_values (map (unExp . snd) fes)
  return [cexp| $(fromJust vs)|]
codeGenValue (EValArr vs) = do 
  vs <- cg_values (map unExp vs) 
  return [cexp| $(fromJust vs)|]
codeGenValue _v = 
  cgIO $ fail ("codeGenValue, non-value: " ++ show _v)


codeGenArrVal :: String -> Ty -> [Exp] -> Cg C.InitGroup
codeGenArrVal name ty@(TArray _ TBit) vals
  = do { let mvs = cg_bitvalues (map unExp vals)
       ; return . fst =<< codeGenDeclGroup_qual "" name ty mvs }
codeGenArrVal name ty@(TArray _ _ty) vals
  = do { mvs <- cg_values (map unExp vals)
       ; return . fst =<< codeGenDeclGroup_qual "" name ty mvs }
codeGenArrVal name _ty vals
  = fail "codeGenArrVal: non-array type!"




-}
