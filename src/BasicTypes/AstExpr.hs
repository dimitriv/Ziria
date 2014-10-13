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
{-# LANGUAGE GADTs, DeriveGeneric, ScopedTypeVariables, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module AstExpr where

import {-# SOURCE #-} Analysis.Range

import Prelude hiding (exp, mapM)
import Control.Monad (liftM, foldM, when)
import Control.Monad.State (State, execState, modify)
import Data.Functor.Identity ( Identity (..) )
import Data.List (nub)
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Parsec.Pos
import Text.PrettyPrint.Mainland
import Text.Show.Pretty (PrettyVal)
import Data.Traversable (mapM)
import qualified Data.Set as S
import qualified Data.Map as Map

import Orphans ()

{-------------------------------------------------------------------------------
  Various kinds of variables
-------------------------------------------------------------------------------}

type TyName  = String
type FldName = String

-- | Type variables (ranging over Ty or CTy)
type TyVar = String

-- | Arrow length variables (ranging over type level naturals)
type LenVar = String

-- | Bitwidth variables (ranging over type level bitwidth annotations)
type BWVar = String

-- | Buffer IDs
type BufId = String

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

data GName t
  = MkName { name    :: String
           , uniqId  :: String
           , nameTyp :: t
           , nameLoc :: Maybe SourcePos
           }
  deriving (Generic)

instance Eq (GName t) where
  nm1 == nm2 = (name nm1 == name nm2) && (uniqId nm1 == uniqId nm2)

instance Ord (GName t) where
  nm1 <= nm2 = (uniqId nm1 <= uniqId nm2)

instance Show (GName t) where
  show (MkName x _id _ _loc)    = x

instance Pretty (GName t) where
    ppr = string . show

toName :: String -> Maybe SourcePos -> t -> GName t
-- This is our only function to create new names
toName s mpos typ =
    MkName { name    = s
           , uniqId  = s
           , nameLoc = mpos
           , nameTyp = typ
           }

updNameId :: String -> GName t -> GName t
updNameId uid nm = nm { uniqId = uid }

getNameWithUniq :: GName t -> String
getNameWithUniq nm = name nm ++ "_blk" ++ uniqId nm

{-------------------------------------------------------------------------------
  Types in the source language

  (No type variables, "length" expressions)
-------------------------------------------------------------------------------}

data SrcTy where
  SrcTUnit     :: SrcTy
  SrcTBit      :: SrcTy
  SrcTBool     :: SrcTy

  SrcTArray    :: SrcNumExpr -> SrcTy -> SrcTy
  SrcTInt      :: SrcBitWidth -> SrcTy
  SrcTDouble   :: SrcTy
  SrcTStruct   :: TyName -> SrcTy

  deriving (Generic, Eq)

-- | Bit widths in the source language are _always_ given (unknown bit widths
-- are only used in the type checker for the types of literals).
data SrcBitWidth
  = SrcBW8
  | SrcBW16
  | SrcBW32
  | SrcBW64
  deriving (Generic, Eq, Show)

data SrcNumExpr where
  -- | User explicitly specifies the length
  SrcLiteral :: Int -> SrcNumExpr

  -- | NArr: Length is the same as the length of the array of the given name
  SrcNArr :: GName (Maybe SrcTy) -> SrcNumExpr

  -- | User doesn't specify array length.
  -- We record the the location for the sake of error messages.
  SrcNVar :: SourcePos -> SrcNumExpr

  deriving (Generic, Eq)

{-------------------------------------------------------------------------------
  Types in the internal language

  (Type variables, no "length" expressions)
-------------------------------------------------------------------------------}

data Ty where
  -- TVars are just strings since they don't appear in user programs
  TVar      :: TyVar -> Ty
  TUnit     :: Ty
  TBit      :: Ty
  TBool     :: Ty
  TString   :: Ty                       -- Currently we have very limited supports for strings -
                                        -- they can only be printed
  TArray    :: NumExpr -> Ty -> Ty
  TInt      :: BitWidth -> Ty
  TDouble   :: Ty
  -- TODO: We could inline GStructDef here?
  TStruct   :: TyName -> [(FldName, Ty)] -> Ty
  TInterval :: Int -> Ty

  -- Arrow and buffer types
  TArrow :: [Ty] -> Ty -> Ty
  TBuff  :: BufTy -> Ty

  deriving (Generic, Eq)

data NumExpr where
  Literal :: Int -> NumExpr

  -- | NVar: Length to be inferred from the context (or polymorphic)
  NVar :: LenVar -> NumExpr

  deriving (Generic, Eq)

data BitWidth
  = BW8
  | BW16
  | BW32
  | BW64
  | BWUnknown BWVar -- TODO: Why is this not a GName t instead of a BWVar?
  deriving (Generic, Eq, Show)

data BufTy =
    -- | Internal buffer (for parallelization)
    IntBuf { bufty_ty :: Ty }

    -- | External buffer (for the `ReadSrc` or `WriteSnk`)
    --
    -- NOTE: We record the type that the program is reading/writing, _NOT_
    -- its base type (in previous versions we recorded the base type here).
  | ExtBuf { bufty_ty :: Ty }
  deriving (Generic, Eq)

{-------------------------------------------------------------------------------
  Expressions (parameterized by the (Haskell) type of (Ziria) types
-------------------------------------------------------------------------------}

data GUnOp t =
    NatExp
  | Neg
  | Not
  | BwNeg
  | Cast t   -- Cast to this target type
  | ALength
  deriving (Generic, Eq)

data BinOp =
  -- arithmetic operators
    Add
  | Sub
  | Mult
  | Div
  | Rem
  | Expon
  -- bitwise operators
  | ShL
  | ShR
  | BwAnd
  | BwOr
  | BwXor
  -- comparison operators
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or
  deriving (Generic, Eq, Show)

data Val where
  VBit    :: Bool    -> Val
  VInt    :: Integer -> Val
  VDouble :: Double  -> Val
  VBool   :: Bool    -> Val
  VString :: String  -> Val
  VUnit   :: Val
  deriving (Generic, Show, Eq)

data LengthInfo
     = LISingleton
     | LILength Int -- Invariant: > 0
  deriving (Generic, Eq)

data UnrollInfo
  = Unroll        -- force unroll
  | NoUnroll      -- force no-unroll
  | AutoUnroll    -- do whatever the compiler would do (no annotation)
  deriving (Generic, Eq)

-- If true, the binding should be forced to be inlined.
-- This is used by e.g. the vectorizer to bind inlinable
-- sub-arrays of the input array.

data ForceInline
  = ForceInline   -- Always inline
  | NoInline      -- Never inline
  | AutoInline    -- Let the compiler decide
  deriving (Generic)

data GExp0 t a where
  -- | A single value
  --
  -- We record the type of the value because literals are overloaded.
  EVal :: t -> Val -> GExp0 t a

  -- | An array value
  --
  -- We record the type of the array value (see also `EVal`).
  EValArr :: t -> [Val] -> GExp0 t a

  EVar :: GName t -> GExp0 t a
  EUnOp :: GUnOp t -> GExp t a -> GExp0 t a
  EBinOp :: BinOp -> GExp t a -> GExp t a -> GExp0 t a

  -- | EArrRead ex ei j.
  --
  -- Read a subarray of 'ex' starting at index ei of length j and going as long
  -- as LengthInfo says.
  --
  -- If LengthInfo is LISingleton, then we are supposed to only read at a
  -- single position and return a scalar. Otherwise we return an array.
  EArrRead :: GExp t a -> GExp t a -> LengthInfo -> GExp0 t a

  -- | Assignment
  --
  -- Although the syntax here allows for arbitrary expressions on the LHS,
  -- really we only allow "dereferencing expressions" of the form
  --
  -- > d := x | d.f | d[e] | d[e,j]
  --
  -- See semantics for details.
  EAssign :: GExp t a -> GExp t a -> GExp0 t a

  -- | Array write
  --
  -- See comments for `EArrRead` and `EAssign`.
  --
  -- TODO: Maybe merge with `EAssign`.
  EArrWrite :: GExp t a -> GExp t a -> LengthInfo -> GExp t a -> GExp0 t a

  -- | Iterate over an array
  --
  -- @EIter ix x earr ebody@ iterates over array @earr@, binding @ix@ to
  -- the index into the array and @x@ to the value of the array at that index
  -- at every step.
  --
  -- TODO: We don't seem to be creating instances of EIter anywhere in the
  -- compiler (not in the parser, not anywhere else). Is it obsolete?
  EIter :: GName t -> GName t -> GExp t a -> GExp t a -> GExp0 t a

  EFor :: UnrollInfo -> GName t -> GExp t a -> GExp t a -> GExp t a -> GExp0 t a


  EWhile :: GExp t a -> GExp t a -> GExp0 t a


  ELet :: GName t -> ForceInline -> GExp t a -> GExp t a -> GExp0 t a

  -- | Potentially initialized read/write variable
  ELetRef :: GName t -> Maybe (GExp t a) -> GExp t a -> GExp0 t a

  ESeq :: GExp t a -> GExp t a -> GExp0 t a
  ECall :: GName t -> [GExp t a] -> GExp0 t a
  EIf :: GExp t a -> GExp t a -> GExp t a -> GExp0 t a

  -- | Print any expression, for debugging
  EPrint :: Bool -> GExp t a -> GExp0 t a

  -- | Generate runtime failure, with error report
  EError :: t -> String -> GExp0 t a
  ELUT :: Map (GName t) Range -> GExp t a -> GExp0 t a

  -- | Permute a bit array: In the long run this should probably
  -- become a generalized array read but for now I am keeping it as
  -- is.
  --
  -- > e1 : arr[N] bit   e2 : arr[N] int
  -- > ---------------------------------
  -- >  EBPerm e1 e2  : arr[N] bit
  EBPerm :: GExp t a -> GExp t a -> GExp0 t a

  -- | Constructing structs
  EStruct :: TyName -> [(FldName,GExp t a)] -> GExp0 t a

  -- | Project field out of a struct
  EProj   :: GExp t a -> FldName -> GExp0 t a
  deriving Generic

data GExp t a
  = MkExp { unExp :: GExp0 t a
          , expLoc :: Maybe SourcePos
          , info :: a }
  deriving (Generic)

-- Structure definitions
data GStructDef t
  = StructDef { struct_name :: TyName
              , struct_flds :: [(FldName,t)] }
  deriving (Generic)

data GFun0 t a where
  MkFunDefined  :: GName t                       -- name
                -> [GName t]                     -- params
                -> [(GName t,Maybe (GExp t a))]  -- locals
                -> GExp t a                      -- body
                -> GFun0 t a
  MkFunExternal :: GName t                       -- name
                -> [GName t]                     -- params
                -> t                             -- return type
                -> GFun0 t a
  deriving (Generic)

{- TODO plug this in at some point
data FunDef a body
  = FunDef { funName   :: GName t
           , funParams :: [(GName t,Ty)]
           , funLocals :: [(GName t,Ty,Maybe (Exp a))]
           , funDef    :: body }
-}

data GFun t a
  = MkFun { unFun   :: GFun0 t a
          , funLoc  :: Maybe SourcePos
          , funInfo :: a }
  deriving (Generic)

funName :: GFun t a -> GName t
funName (MkFun (MkFunDefined  nm _ _ _) _ _) = nm
funName (MkFun (MkFunExternal nm _ _)   _ _) = nm

{-------------------------------------------------------------------------------
  Specialization of the AST to Ty (internal types)

  These types are used everywhere in the compiler except in the front-end.
-------------------------------------------------------------------------------}

type UnOp      = GUnOp      Ty
type Exp0      = GExp0      Ty ()
type Exp       = GExp       Ty ()
type StructDef = GStructDef Ty
type Fun0      = GFun0      Ty ()
type Fun       = GFun       Ty ()
type EId       = GName      Ty 

{-------------------------------------------------------------------------------
  Specializations of the AST to SrcTy (source level types)

  These types are only used in the parser and as input to the renamer.
-------------------------------------------------------------------------------}

type SrcExp = GExp (Maybe SrcTy) ()
type SrcFun = GFun (Maybe SrcTy) ()

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

vint :: Int -> Val
-- Auxiliary function for use in the vectorizer
vint n = VInt (fromIntegral n)

eVal :: Maybe SourcePos -> a -> t -> Val -> GExp t a
eVal loc a t v = MkExp (EVal t v) loc a
eValArr :: Maybe SourcePos -> a -> t -> [Val] -> GExp t a
eValArr loc a t v = MkExp (EValArr t v) loc a
eVar :: Maybe SourcePos -> a ->  GName t -> GExp t a
eVar loc a v = MkExp (EVar v) loc a
eUnOp :: Maybe SourcePos -> a -> GUnOp t -> GExp t a -> GExp t a
eUnOp loc a o v = MkExp (EUnOp o v) loc a
eBinOp :: Maybe SourcePos -> a -> BinOp -> GExp t a -> GExp t a -> GExp t a
eBinOp loc a b x y = MkExp (EBinOp b x y) loc a
eAssign :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a
eAssign loc a x y = MkExp (EAssign x y) loc a
eArrRead :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> LengthInfo -> GExp t a
eArrRead loc a x y l = MkExp (EArrRead x y l) loc a
eArrWrite :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> LengthInfo -> GExp t a -> GExp t a
eArrWrite loc a x y l e = MkExp (EArrWrite x y l e) loc a
eIter :: Maybe SourcePos -> a -> GName t -> GName t -> GExp t a -> GExp t a -> GExp t a
eIter loc a x y e1 e2 = MkExp (EIter x y e1 e2) loc a
eFor :: Maybe SourcePos -> a -> UnrollInfo -> GName t -> GExp t a -> GExp t a -> GExp t a -> GExp t a
eFor loc a ui n e1 e2 e3 = MkExp (EFor ui n e1 e2 e3) loc a
eLet :: Maybe SourcePos -> a ->  GName t -> ForceInline -> GExp t a -> GExp t a -> GExp t a
eLet loc a x fi e1 e2 = MkExp (ELet x fi e1 e2) loc a
eLetRef :: Maybe SourcePos -> a ->  GName t -> Maybe (GExp t a) -> GExp t a -> GExp t a
eLetRef loc a nm x e = MkExp (ELetRef nm x e) loc a
eSeq :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a
eSeq loc a e1 e2 = MkExp (ESeq e1 e2) loc a
eCall :: Maybe SourcePos -> a ->  GName t -> [GExp t a] -> GExp t a
eCall loc a f es = MkExp (ECall f es) loc a
eIf :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a -> GExp t a
eIf loc a e1 e2 e3 = MkExp (EIf e1 e2 e3) loc a
ePrint :: Maybe SourcePos -> a ->  Bool -> GExp t a -> GExp t a
ePrint loc a b e = MkExp (EPrint b e) loc a
eError :: Maybe SourcePos -> a -> t -> String -> GExp t a
eError loc a t s = MkExp (EError t s) loc a
eLUT :: Maybe SourcePos -> a ->  Map (GName t) Range -> GExp t a -> GExp t a
eLUT loc a m e = MkExp (ELUT m e) loc a
eBPerm :: Maybe SourcePos -> a ->  GExp t a -> GExp t a -> GExp t a
eBPerm loc a e1 e2 = MkExp (EBPerm e1 e2) loc a
eStruct :: Maybe SourcePos -> a ->  TyName -> [(String,GExp t a)] -> GExp t a
eStruct loc a tn es = MkExp (EStruct tn es) loc a
eProj :: Maybe SourcePos -> a ->  GExp t a -> String -> GExp t a
eProj loc a e s = MkExp (EProj e s) loc a
eWhile :: Maybe SourcePos -> a -> GExp t a -> GExp t a -> GExp t a
eWhile loc a econd ebody = MkExp (EWhile econd ebody) loc a

{-------------------------------------------------------------------------------
  Built-in types
-------------------------------------------------------------------------------}

tint, tint8, tint16, tint32, tint64 :: Ty
tint64  = TInt BW64
tint32  = TInt BW32
tint16  = TInt BW16
tint8   = TInt BW8
tint    = tint32

tdouble :: Ty
tdouble = TDouble

tcomplex, tcomplex8, tcomplex16, tcomplex32, tcomplex64 :: Ty
tcomplex8  = complexTy complex8TyName  BW8
tcomplex16 = complexTy complex16TyName BW16
tcomplex32 = complexTy complex32TyName BW32
tcomplex64 = complexTy complex64TyName BW64
tcomplex   = tcomplex32

complexTy :: TyName -> BitWidth -> Ty
complexTy nm bw = TStruct nm [("re", TInt bw), ("im", TInt bw)]

{-------------------------------------------------------------------------------
  Built-in types (source syntax)
-------------------------------------------------------------------------------}

tintSrc, tintSrc8, tintSrc16, tintSrc32, tintSrc64 :: SrcTy
tintSrc64  = SrcTInt SrcBW64
tintSrc32  = SrcTInt SrcBW32
tintSrc16  = SrcTInt SrcBW16
tintSrc8   = SrcTInt SrcBW8
tintSrc    = tintSrc32

tdoubleSrc :: SrcTy
tdoubleSrc = SrcTDouble

tcomplexSrc, tcomplexSrc8, tcomplexSrc16, tcomplexSrc32, tcomplexSrc64 :: SrcTy
tcomplexSrc8  = SrcTStruct complex8TyName
tcomplexSrc16 = SrcTStruct complex16TyName
tcomplexSrc32 = SrcTStruct complex32TyName
tcomplexSrc64 = SrcTStruct complex64TyName
tcomplexSrc   = tcomplexSrc32

{-------------------------------------------------------------------------------
  Various map functions

  Since these are used on both source terms and internal terms they have the
  general types where possible.
-------------------------------------------------------------------------------}

mapTyM :: Monad m => (Ty -> m Ty) -> Ty -> m Ty
mapTyM f = go
  where
    go (TVar s)            = f $ TVar s
    go TUnit               = f $ TUnit
    go TBit                = f $ TBit
    go TBool               = f $ TBool
    go TString             = f $ TString
    go (TInt bw)           = f $ TInt bw
    go (TInterval n)       = f $ TInterval n
    go TDouble             = f $ TDouble
    go (TStruct tn ts)     = do ts' <- mapM go (map snd ts)
                                f $ TStruct tn (zip (map fst ts) ts')
    go (TArray n t)        = do t' <- go t
                                f $ TArray n t'
    go (TArrow ts t)       = do ts' <- mapM go ts
                                t'  <- go t
                                f $ TArrow ts' t'
    go (TBuff (IntBuf bt)) = do bt' <- go bt
                                f $ TBuff (IntBuf bt')
    go (TBuff (ExtBuf bt)) = do bt' <- go bt
                                f $ TBuff (ExtBuf bt')

mapNameM :: Monad m => (t -> m t') -> GName t -> m (GName t')
mapNameM onTyp MkName{..} = do
    nameTyp' <- onTyp nameTyp
    return MkName{nameTyp = nameTyp', ..}

-- | Most general form of mapping over expressions
mapExpM :: forall t t' a a' m. Monad m
        => (t -> m t')                     -- ^ On types
        -> (a -> m a')                     -- ^ On annotations
        -> (GExp t' a' -> m (GExp t' a'))  -- ^ Combine results
        -> GExp t a
        -> m (GExp t' a')
mapExpM onTyp onAnn f = goExp
  where
    goExp :: GExp t a -> m (GExp t' a')
    goExp MkExp{..} = do
      info'  <- onAnn info
      unExp' <- goExp0 unExp
      f MkExp{unExp = unExp', info = info', ..}

    goExp0 :: GExp0 t a -> m (GExp0 t' a')
    goExp0 (EVal t v) = do
      t' <- onTyp t
      return $ EVal t' v
    goExp0 (EValArr t varr) = do
      t' <- onTyp t
      return $ EValArr t' varr
    goExp0 (EVar x) = do
      x' <- mapNameM onTyp x
      return $ EVar x'
    goExp0 (EUnOp op e1) = do
      op' <- goUnOp op
      e1' <- goExp e1
      return $ EUnOp op' e1'
    goExp0 (EBinOp op e1 e2) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ EBinOp op e1' e2'
    goExp0 (EAssign e1 e2) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ EAssign e1' e2'
    goExp0 (EArrRead e1 e2 r) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ EArrRead e1' e2' r
    goExp0 (EArrWrite e1 e2 r e3) = do
      e1' <- goExp e1
      e2' <- goExp e2
      e3' <- goExp e3
      return $ EArrWrite e1' e2' r e3'
    goExp0 (EIter nm1 nm2 e1 e2) = do
      nm1' <- mapNameM onTyp nm1
      nm2' <- mapNameM onTyp nm2
      e1'  <- goExp e1
      e2'  <- goExp e2
      return $ EIter nm1' nm2' e1' e2'
    goExp0 (EFor ui nm1 e1 e2 e3) = do
      nm1' <- mapNameM onTyp nm1
      e1'  <- goExp e1
      e2'  <- goExp e2
      e3'  <- goExp e3
      return $ EFor ui nm1' e1' e2' e3'
    goExp0 (EWhile e1 e2) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ EWhile e1' e2'
    goExp0 (ELet nm1 fi e1 e2) = do
      nm1' <- mapNameM onTyp nm1
      e1'  <- goExp e1
      e2'  <- goExp e2
      return $ ELet nm1' fi e1' e2'
    goExp0 (ELetRef nm1 Nothing e2) = do
      nm1' <- mapNameM onTyp nm1
      e2'  <- goExp e2
      return $ ELetRef nm1' Nothing e2'
    goExp0 (ELetRef nm1 (Just e1) e2) = do
      nm1' <- mapNameM onTyp nm1
      e1'  <- goExp e1
      e2'  <- goExp e2
      return $ ELetRef nm1' (Just e1') e2'
    goExp0 (ESeq e1 e2) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ ESeq e1' e2'
    goExp0 (ECall fun es) = do
      fun' <- mapNameM onTyp fun
      es'  <- mapM goExp es
      return $ ECall fun' es'
    goExp0 (EIf e1 e2 e3) = do
      e1' <- goExp e1
      e2' <- goExp e2
      e3' <- goExp e3
      return $ EIf e1' e2' e3'
    goExp0 (EPrint nl e1) = do
      e1' <- goExp  e1
      return $ EPrint nl e1'
    goExp0 (EError t err) = do
      t' <- onTyp t
      return $ EError t' err
    goExp0 (ELUT r e1) = do
      r'   <- goRanges r
      e1'  <- goExp e1
      return $ ELUT r' e1'
    goExp0 (EBPerm e1 e2) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ EBPerm e1' e2'
    goExp0 (EStruct tn tfs) = do
      let do_fld (t,e') = goExp e' >>= \e'' -> return (t,e'')
      tfs' <- mapM do_fld tfs
      return $ EStruct tn tfs'
    goExp0 (EProj e1 fn) = do
      e1' <- goExp e1
      return $ EProj e1' fn

    goUnOp :: GUnOp t -> m (GUnOp t')
    goUnOp NatExp   = return NatExp
    goUnOp Neg      = return Neg
    goUnOp Not      = return Not
    goUnOp BwNeg    = return BwNeg
    goUnOp (Cast t) = do t' <- onTyp t ; return (Cast t')
    goUnOp ALength  = return ALength

    goRanges :: Map (GName t) Range -> m (Map (GName t') Range)
    goRanges = liftM Map.fromList
             . mapM (\(n, r) -> do n' <- mapNameM onTyp n ; return (n', r))
             . Map.toList

-- | Most general mapping over function and program locals
mapLocalsM :: forall t t' a a' m. Monad m
           => (t -> m t')                    -- ^ On types
           -> (GExp t a -> m (GExp t' a'))   -- ^ On expressions
           -> [(GName t,  Maybe (GExp t a))] -- ^ Locals
           -> m [(GName t', Maybe (GExp t' a'))]
mapLocalsM onTyp onExp = mapM (uncurry aux)
  where
    aux :: GName t -> Maybe (GExp t a) -> m (GName t', Maybe (GExp t' a'))
    aux nm exp = do
      nm'  <- mapNameM onTyp nm
      exp' <- mapM onExp exp
      return (nm', exp')

mapFunM :: forall t t' a a' m. Monad m
        => (t -> m t')                    -- ^ On types
        -> (a -> m a')                    -- ^ On annotations
        -> (GExp t a -> m (GExp t' a'))   -- ^ On expressions
        -> GFun t a
        -> m (GFun t' a')
mapFunM onTyp onAnn onExp = goFun
  where
    goFun :: GFun t a -> m (GFun t' a')
    goFun MkFun{..} = do
      unFun'   <- goFun0 unFun
      funInfo' <- onAnn funInfo
      return MkFun{unFun = unFun', funInfo = funInfo', ..}

    goFun0 :: GFun0 t a -> m (GFun0 t' a')
    goFun0 (MkFunDefined nm params locals body) = do
      nm'     <- mapNameM onTyp nm
      params' <- mapM (mapNameM onTyp) params
      locals' <- mapLocalsM onTyp onExp locals
      body'   <- onExp body
      return $ MkFunDefined nm' params' locals' body'
    goFun0 (MkFunExternal nm params ret) = do
      nm'     <- mapNameM onTyp nm
      params' <- mapM (mapNameM onTyp) params
      ret'    <- onTyp ret
      return $ MkFunExternal nm' params' ret'

{-------------------------------------------------------------------------------
  Pure mapping functions
-------------------------------------------------------------------------------}

mapTy :: (Ty -> Ty) -> Ty -> Ty
mapTy f = runIdentity . mapTyM (Identity . f)

mapName :: (t -> t') -> GName t -> GName t'
mapName f = runIdentity . mapNameM (Identity . f)

mapExp :: (t -> t')                   -- ^ On types
       -> (a -> a')                   -- ^ On annotations
       -> (GExp t' a' -> GExp t' a')  -- ^ Combine results
       -> GExp t a
       -> GExp t' a'
mapExp onTyp onAnn f = runIdentity . mapExpM (Identity . onTyp)
                                             (Identity . onAnn)
                                             (Identity . f)

mapLocals :: (t -> t')                       -- ^ On types
          -> (GExp t a -> GExp t' a')        -- ^ On expressions
          -> [(GName t,  Maybe (GExp t a))]  -- ^ Locals
          -> [(GName t', Maybe (GExp t' a'))]
mapLocals onTyp f = runIdentity . mapLocalsM (Identity . onTyp)
                                             (Identity . f)

mapFun :: (t -> t')                    -- ^ On types
       -> (a -> a')                    -- ^ On annotations
       -> (GExp t a -> (GExp t' a'))   -- ^ On expressions
       -> GFun t a
       -> GFun t' a'
mapFun onTyp onAnn f = runIdentity . mapFunM (Identity . onTyp)
                                             (Identity . onAnn)
                                             (Identity . f)

{-------------------------------------------------------------------------------
  Erase annotations
-------------------------------------------------------------------------------}

eraseExp :: GExp t a -> GExp t ()
eraseExp = mapExp id (const ()) id

eraseFun :: GFun t a -> GFun t ()
eraseFun = mapFun id (const ()) eraseExp

eraseLocals :: [(GName t, Maybe (GExp t a))] -> [(GName t, Maybe (GExp t ()))]
eraseLocals = mapLocals id eraseExp

{-------------------------------------------------------------------------------
  Free variables
-------------------------------------------------------------------------------}

type ExprFVs t = S.Set (GName t)

-- | Collect free variables in an expression
--
-- The `takeFuns` argument indicates whether we want to include the names of
-- functions that are called in the expression. This is useful when we are
-- computing the free variables in a nested function definition (which become
-- additional arguments to the function when we generate C code).
exprFVs' :: forall t b. Bool -> GExp t b -> ExprFVs t
exprFVs' takeFuns = \e ->
    execState (mapExpM return return goExp e) S.empty
  where
    goExp :: GExp t b -> State (ExprFVs t) (GExp t b)
    goExp x = goExp0 (unExp x) >> return x

    goExp0 :: GExp0 t b -> State (ExprFVs t) ()
    goExp0 (EVar nm)        = record nm
    goExp0 (EFor _ x _ _ _) = unrecord x
    goExp0 (ELet x _ _ _)   = unrecord x
    goExp0 (ELetRef x _ _)  = unrecord x
    goExp0 (EIter x v _ _)  = unrecord x >> unrecord v
    goExp0 (ECall f _)      = when takeFuns $ record f
    goExp0 _                = return ()

    record, unrecord :: GName t -> State (ExprFVs t) ()
    record   nm = modify $ S.insert nm
    unrecord nm = modify $ S.delete nm

-- NB: Take function variables (hence True)
exprFVs :: GExp t b -> S.Set (GName t)
exprFVs = exprFVs' True

-- NB: Don't take function variables when computing fvs of closure
exprFVsClos :: GExp t b -> S.Set (GName t)
exprFVsClos = exprFVs' False

funFVs :: GFun t a -> S.Set (GName t)
funFVs f = case unFun f of
  MkFunDefined _nm params locals body ->
    -- NB: important that we use foldr here instead of foldl
    (foldr (\(nm,me) s ->
             let se = case me of
                   Just e  -> S.union (exprFVs e) s
                   Nothing -> s
             in se S.\\ (S.singleton nm)) (exprFVs body) locals) S.\\
    (S.fromList params)
  MkFunExternal _nm _params _ty -> S.empty

-- | Find free variables in a function definition
--
-- NOTE: This is not polymorphic in the type because we also collect length
-- variables in the types of the function parameters and result type.
funFVsClos :: GFun Ty a -> (S.Set (GName Ty), S.Set LenVar)
funFVsClos f = case unFun f of
    MkFunDefined _nm params locals body ->
      -- NB: important that we use foldr here instead of foldl
      ( (foldr (\(nm,me) s ->
                 let se = case me of
                       Just e  -> S.union (exprFVsClos e) s
                       Nothing -> s
                 in se S.\\ (S.singleton nm)) (exprFVsClos body) locals) S.\\
        (S.fromList params)
      , S.fromList $ gatherPolyVars (map nameTyp params)
      )
    -- TODO: Can't externals have length variables in their types?
    MkFunExternal _nm _params _ty -> (S.empty, S.empty)

-- | Find all length variables in a set of types
--
-- TODO: Shouldn't we check for length variables in nested array types?
gatherPolyVars :: [Ty] -> [LenVar]
gatherPolyVars = nub . gather []
  where
    gather acc []                       = acc
    gather acc (TArray (NVar nm1) _:ts) = gather (nm1:acc) ts
    gather acc (_                  :ts) = gather acc       ts

{-------------------------------------------------------------------------------
  Substitutions
-------------------------------------------------------------------------------}

substExp :: Monad m => (GName t, GExp t a) -> GExp t a -> m (GExp t a)
substExp (nm,e') = mapExpM return return go
  where
    go e | EVar nm' <- unExp e = if nm == nm' then return e' else return e
         | otherwise           = return e

substAll :: Monad m => [(GName t, GExp t a)] -> GExp t a -> m (GExp t a)
substAll substs e = foldM (\x p -> substExp p x) e substs

-- | Substitute lengths through
substLength :: Monad m => (LenVar, NumExpr) -> GExp Ty a -> m (GExp Ty a)
substLength (nm,numexpr) = mapExpM (substLengthTy (nm,numexpr)) return return

substLengthTy :: Monad m => (LenVar, NumExpr) -> Ty -> m Ty
substLengthTy (nm,numexpr) = mapTyM on_ty
  where
    on_ty (TArray (NVar nm') t) | nm == nm' = return (TArray numexpr t)
    on_ty ty_other                          = return ty_other

substAllLengthTy :: Monad m => [(LenVar, NumExpr)] -> Ty -> m Ty
substAllLengthTy substs t = foldM (\x p -> substLengthTy p x) t substs

substAllTyped :: Monad m
              => [(GName Ty, GExp Ty a)]
              -> [(LenVar, NumExpr)]
              -> GExp Ty a
              -> m (GExp Ty a)
substAllTyped substs len_substs e = do
    e' <- foldM (\x p -> substLength p x) e len_substs
    foldM (\x p -> substExp p x) e' substs

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

getLnNumInStr :: Maybe SourcePos -> String
getLnNumInStr csp
   = case csp of
       Just l  -> "ln" ++ (show $ sourceLine l) ++ "_"
       Nothing -> "ln_"

isEVal :: GExp t a -> Bool
isEVal e
  | EVal {} <- unExp e
  = True
isEVal _
  = False

isArrayTy :: Ty -> Bool
isArrayTy (TArray _ _) = True
isArrayTy _          = False

getArrayTy :: Ty -> Ty
getArrayTy (TArray _n t) = t
getArrayTy t             = t


expEq :: Eq t => GExp t a -> GExp t a -> Bool
-- Are these two expressions /definitely/ equal?
expEq e e' = expEq0 (unExp e) (unExp e')
  where
    expEq0 (EVal t v) (EVal t' v') = t == t' && v == v'
    expEq0 (EVar x) (EVar y)  = x == y
    expEq0 (EArrRead e1 e2 li) (EArrRead e1' e2' li')
      = expEq e1 e1' && expEq e2 e2' && (li == li')
    expEq0 (EUnOp u1 e1) (EUnOp u1' e1')
      = (u1 == u1') && expEq e1 e1'
    expEq0 (EBinOp b e1 e2) (EBinOp b' e1' e2')
      = (b == b') && expEq e1 e1' && expEq e2 e2'
    expEq0 _e _e' = False

toExp :: a -> GExp0 t a -> GExp t a
toExp a e = MkExp { unExp = e, expLoc = Nothing, info = a }

toExpPos :: a -> SourcePos -> GExp0 t a -> GExp t a
toExpPos a pos e = MkExp { unExp = e, expLoc = Just pos, info = a }

binopList :: BinOp -> a -> GExp t a -> [GExp t a] -> GExp t a
binopList _  _ e0 []       = e0
binopList op a e0 (e : es) = toExp a $ EBinOp op e (binopList op a e0 es)

dotDotName :: String
dotDotName = "..."

-- Just for debugging purposes (and used in error messages)
unknownTArr :: Ty
unknownTArr = TArray (NVar dotDotName) (TVar dotDotName)

unknownTArrOfBase :: Ty -> Ty
unknownTArrOfBase t = TArray (NVar dotDotName) t

unknownTFun :: Int -> Ty
unknownTFun n = TArrow (replicate n (TVar dotDotName)) (TVar dotDotName)

-- Observations/questions about the following code
-- (a) Excludes Bit. Not sure why. (TODO)
-- (b) Type variables are scalars but after subst they may not be?
-- (c) Structs are assumed to be scalars. Probably that's fine as they
--     can be treated in a CBV fashion.
isScalarTy :: Ty -> Bool
isScalarTy t =
  case t of
    TVar {}      -> True
    TUnit        -> True
    TBit         -> False
    TInt {}      -> True
    TDouble {}   -> True
    TStruct {}   -> True
    TBool        -> True
    TString      -> True
    TInterval {} -> False
    TArray {}    -> False
    TArrow {}    -> False
    TBuff {}     -> False



-- Does this type support arithmetic operations?
supportsArithTy :: Ty -> Bool
supportsArithTy t =
  case t of
    TVar {}      -> True
    TInt {}      -> True
    TDouble {}   -> True
    TStruct {}   -> isComplexTy t
    _other       -> False


-- Does this type support direct comparison (= in C)
supportsEqTy :: Ty -> Bool
supportsEqTy t =
  case t of
    TVar {}      -> True
    TUnit        -> True
    TBit         -> True
    TInt {}      -> True
    TDouble {}   -> True
    TStruct {}   -> True
    TBool        -> True
    TString      -> False
    _other       -> False

-- Does this type support <, <= etc?
supportsCmpTy :: Ty -> Bool
supportsCmpTy t =
  case t of
    TVar {}      -> True
    TInt {}      -> True
    TDouble {}   -> True
    TBool        -> True
    TString      -> False
    _other       -> False


isComplexTy :: Ty -> Bool
isComplexTy (TStruct tn _)
  = any (== tn) [ complexTyName
                , complex8TyName
                , complex16TyName
                , complex32TyName
                ]
isComplexTy _other = False

complexTyName :: TyName
complexTyName = "complex"
complex8TyName :: TyName
complex8TyName = "complex8"
complex16TyName :: TyName
complex16TyName = "complex16"
complex32TyName :: TyName
complex32TyName = "complex32"
complex64TyName :: TyName
complex64TyName = "complex64"


toFunPos :: a -> SourcePos -> GFun0 t a -> GFun t a
toFunPos a pos fn = MkFun fn (Just pos) a

isArithBinOp :: BinOp -> Bool
isArithBinOp Add   = True
isArithBinOp Sub   = True
isArithBinOp Mult  = True
isArithBinOp Div   = True
isArithBinOp Rem   = True
isArithBinOp Expon = True
isArithBinOp _     = False

isShiftBinOp :: BinOp -> Bool
isShiftBinOp ShL = True
isShiftBinOp ShR = True
isShiftBinOp _   = False

isLogicalBinOp :: BinOp -> Bool
isLogicalBinOp BwAnd = True
isLogicalBinOp BwOr  = True
isLogicalBinOp BwXor = True
isLogicalBinOp _     = False

isEqualityBinOp :: BinOp -> Bool
isEqualityBinOp Eq  = True
isEqualityBinOp Neq = True
isEqualityBinOp _   = False


isRelBinOp :: BinOp -> Bool
isRelBinOp Lt  = True
isRelBinOp Leq = True
isRelBinOp Gt  = True
isRelBinOp Geq = True
isRelBinOp _   = False

isBoolBinOp :: BinOp -> Bool
isBoolBinOp And = True
isBoolBinOp Or  = True
isBoolBinOp _   = False


-- Can this expression potentially change the state?
-- A super conservative side-effect analysis
mutates_state :: GExp t a -> Bool
mutates_state e = case unExp e of
  EVal _ _              -> False
  EValArr _ _           -> False
  EVar _                -> False
  EUnOp _ e'            -> mutates_state e'
  EBinOp _ e1 e2        -> any mutates_state [e1,e2]
  EAssign _e1 _e2       -> True

  EArrRead e1 e2 LISingleton   -> any mutates_state [e1,e2]

  EArrRead e1 e2 (LILength {}) -> any mutates_state [e1,e2]

  EArrWrite _e1 _e2 _r _e3 -> True

  EIter _ _ e1 e2       -> any mutates_state [e1,e2]
  EFor _ _ e1 e2 e3     -> any mutates_state [e1,e2,e3]
  EWhile e1 e2          -> any mutates_state [e1,e2]

  ELet _nm _fi e1 e2     -> any mutates_state [e1,e2]
  ELetRef _nm (Just e1) e2 -> any mutates_state [e1,e2]
  ELetRef _nm Nothing   e2 -> mutates_state e2

  ESeq e1 e2     -> any mutates_state [e1,e2]
  ECall _e' _es  -> True
  EIf e1 e2 e3   -> any mutates_state [e1,e2,e3]

  EPrint _nl _e1 -> True -- See Note [IOEffects]
  EError _ _     -> True

  ELUT _ e1      -> mutates_state e1
  EBPerm e1 e2   -> any mutates_state [e1,e2]

  EStruct _tn tfs -> any mutates_state (map snd tfs)
  EProj e0 _f     -> mutates_state e0

{-------------------------------------------------------------------------------
  Built-ins
-------------------------------------------------------------------------------}

-- | Primitive complex structures
--
-- This is necessary for the translation from SrcTy (where structs have names
-- only) to Ty (where structs are fully defined)
primComplexStructs :: [(TyName,StructDef)]
primComplexStructs
  = [ (complex8TyName,  structDefFrom tcomplex8)
    , (complex16TyName, structDefFrom tcomplex16)
    , (complex32TyName, structDefFrom tcomplex32)
    , (complex64TyName, structDefFrom tcomplex64)
    ]
  where
    -- TODO: This translation back and forth to StructDef is annoying.
    -- We should inline StructDef in LetStruct.
    structDefFrom :: Ty -> StructDef
    structDefFrom (TStruct nm flds) = StructDef nm flds
    structDefFrom _ = error "Not a struct"

{-------------------------------------------------------------------------------
  PrettyVal instances (used for dumping the AST)
-------------------------------------------------------------------------------}

instance PrettyVal BitWidth
instance PrettyVal SrcBitWidth
instance PrettyVal Ty
instance PrettyVal SrcTy
instance PrettyVal BufTy
instance PrettyVal NumExpr
instance PrettyVal SrcNumExpr
instance PrettyVal BinOp
instance PrettyVal ForceInline
instance PrettyVal LengthInfo
instance PrettyVal UnrollInfo
instance PrettyVal Val

instance PrettyVal t => PrettyVal (GName t)
instance PrettyVal t => PrettyVal (GUnOp t)
instance PrettyVal t => PrettyVal (GStructDef t)

instance (PrettyVal t, PrettyVal a) => PrettyVal (GExp0 t a)
instance (PrettyVal t, PrettyVal a) => PrettyVal (GExp t a)
instance (PrettyVal t, PrettyVal a) => PrettyVal (GFun t a)
instance (PrettyVal t, PrettyVal a) => PrettyVal (GFun0 t a)

{-
Note [IOEffects]
~~~~~~~~~~~~~~~~

If an expression does not mutate state then we would like to not
execute it at all.  For instance, if we have:

     let _ = x[0:256]
     in (y+3)


then we should be able to rewrite this code to just (y+3) (and not
have to copy a useless value of 256 integers!)


So function mutates_state gives a conservative analysis about when can
an expression mutate or affect in some way the state of the
program. E.g. assignments and array writes do mutate state.


Also, 'error' and 'print' statements do affect the state by means of
writing to the console or terminating the program hence mutate_state
gives True for those.

-}




{-
Note [Polymorphic Arrays]
~~~~~~~~~~~~~~~~~~~~~~~~~

Arrays that may be polymorphic in their length arise from user code of
the form:

      (var x : arr int) <- ...

Now, it may turn out that such arrays end up with lengths that are
resolved. E.g. by the following assignment:

      x = y;   // where y is declared to be (arr[42] int)

we learn that the type of 'x' is an array of static size 42. But not
necessarily so, i.e. the length of 'x' may remain unconstrained until
we typecheck a function fully.

Moreover, arguments to functions may be polymorhic:

    f(x : arr int) { ...  }

This has the same effect. If x is not used in a way that fixes its
length in the body of the function then this remains polymorphic,
otherwise the length is fixed.


Note [Polymorphic Array Code Generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When (after type checking) we are in the process of generating code
for a declaration of a variable of array type, there exist several
cases:

(a) The variable has a length that has been statically resolved. This
    is easy, we simply declare the variable.

(b) The variable has length that has not been statically resolved yet,
    e.g. it's still a length variable.  Then it will be ensured that that
    variable is in scope (by type checking, see Note [Array Length Unification])
    and we will do a stack allocation for that variable of the required length.

Finally you can also say something like:

     (x : arr [length(y)] int)

meaning that x gets an array type with the same length as y, which
must be in scope at the program point where we refer to y. For instance
you can have:

     f(x : arr int, y : arr [length(x)] int) { ... }

Meaning: I don't know what is the size of 'x', in fact 'f' may be even
polymorphic in it, but I know that 'y' has that same length.

Note [Array Length Unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(a) Generation of length unification variables: An occurence of type
    (arr int) gives rise to a new unification variable for the length
    e.g. becomes (arr[l_32] int).

    NB: This is currently hardcoded in the AST and done by the parser
    which is unsatisfactory but not the end of the world. It'd be nicer
    if unification variables were born only in the type checker.

    TODO: Probably will do this.


(b) We wish to type check functions and let bindings in isolation so we
    maintain a map from length variables lvar |-> Literal n | lvar

    Whenever we meet an equation:  (arr [lvar] t) ~ (arr [42] t) we simply
    update the map unifying lvar to 42. Similarly for equations between lvars.

    Whenever we meet an equation:
                (arr [lvar] t) ~ (arr [length(y)] t)
    we pick up the type of 'y' from the environment, it must be a
   (TArr numexpr _) and we simply recurse: arr [lvar] t ~ arr numexpr t

    This ensures that 'y' must be in the environment. Moreover,
    whenever we introduce a new type in the environment we check it is
    well formed, by checking if the 'length' variables are bound. This
    will happen when we check parameter binding, annotated let-bound
    definitions, and annotated monadic bind.

(c) At generalization boundaries (function definitions,
    let-bound expressions) we make sure that the only 'length' variables
    that we generalize over (i.e. locally generated - outside-generated) are
    bound in the function parameters. (For ordinary let-bindings I am thinking
    to not generalize over anything). Fail otherwise.


Q:

   How to generalize this to arbitrary numerical expressions instead
   of just 'length' of a previously scoped variable? How to specify the
   return type of a function, if it is polymorphic? Outs ...

         f(x : arr int) : arr (length(x)) int ???

   'x' is typically considered out of scope in typical programming
   languages.  But need not, i.e. it is in scope in hybrid type systems.

-}
