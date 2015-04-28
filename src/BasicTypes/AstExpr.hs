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
{-# LANGUAGE GADTs, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module AstExpr where

import {-# SOURCE #-} LUTAnalysis
import Prelude hiding (exp, mapM)
import Control.DeepSeq.Generics (NFData(..), genericRnf)
import Data.Loc
import Data.Monoid
import Data.Data (Data)
import Data.Functor.Identity (Identity(..))
import Data.Maybe ( isJust )
import Data.Traversable (mapM)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Data.Set as S

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

-- Unique identifiers
newtype Uniq = MkUniq { unUniq :: String }
  deriving (Generic, Typeable, Data, Eq, Ord)

instance Show Uniq where
  show (MkUniq s) = s

-- | Mutability kind (mutable or immutable)
data MutKind = Imm | Mut
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

data GName t
  = MkName { name    :: String
           , uniqId  :: Uniq
           , nameTyp :: t
           , nameLoc :: SrcLoc
           , nameMut :: MutKind
           }
  deriving (Generic, Typeable, Data)

instance Located (GName t) where
    locOf = locOf . nameLoc

isMutable :: GName t -> Bool
isMutable nm = case nameMut nm of { Imm -> False ; Mut -> True }

instance Eq (GName t) where
  nm1 == nm2 = (name nm1 == name nm2) && (uniqId nm1 == uniqId nm2)

-- NB: The Ord class is suspicious in the light of the above Eq class.
-- We should revisit uses of Maps from GNames.

instance Ord (GName t) where
  nm1 <= nm2 = (uniqId nm1 <= uniqId nm2)

instance Show (GName t) where
  show (MkName x _id _ _ _loc)    = x


toName :: String -> SrcLoc -> t -> MutKind -> GName t
toName s mpos typ mk =
    MkName { name    = s
           , uniqId  = MkUniq s
           , nameLoc = mpos
           , nameMut = mk
           , nameTyp = typ
           }

nameArgTy :: GName t -> GArgTy t
nameArgTy nm = GArgTy (nameTyp nm) (nameMut nm)

updNameId :: Uniq -> GName t -> GName t
updNameId uid nm = nm { uniqId = uid }

updNameTy :: GName t -> u -> GName u
updNameTy (MkName n i _ mk l) utyp = MkName n i utyp mk l

getNameWithUniq :: GName t -> String
getNameWithUniq nm = name nm ++ "_blk" ++ unUniq (uniqId nm)

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

  -- Just useful for the embedding
  SrcInject    :: Ty -> SrcTy

  -- We record the absense of a type annotation here
  SrcTyUnknown :: SrcTy

  deriving (Generic, Typeable, Data, Eq)

-- | Bit widths in the source language are _always_ given (unknown bit widths
-- are only used in the type checker for the types of literals).
data SrcBitWidth
  = SrcBW8
  | SrcBW16
  | SrcBW32
  | SrcBW64
  deriving (Generic, Typeable, Data, Eq, Show)

data SrcNumExpr where
  -- | User explicitly specifies the length
  SrcLiteral :: Int -> SrcNumExpr

  -- | NArr: Length is the same as the length of the array of the given name
  SrcNArr :: GName SrcTy -> SrcNumExpr

  -- | User doesn't specify array length.
  -- We record the the location for the sake of error messages.
  SrcNVar :: SrcLoc -> SrcNumExpr

  deriving (Generic, Typeable, Data, Eq)

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
  TArrow :: [ArgTy] -> Ty -> Ty
  TBuff  :: BufTy -> Ty

  TVoid  :: Ty

  deriving (Generic, Typeable, Data, Eq, Ord)

-- An argument type (we record the mutability)
data GArgTy t
  = GArgTy { argty_ty  :: t
           , argty_mut :: MutKind
           }
  deriving (Generic, Typeable, Data, Eq, Ord)

type ArgTy = GArgTy Ty


data NumExpr where
  Literal :: Int -> NumExpr

  -- | NVar: Length to be inferred from the context (or polymorphic)
  NVar :: LenVar -> NumExpr

  deriving (Generic, Typeable, Data, Eq, Ord)

data BitWidth
  = BW8
  | BW16
  | BW32
  | BW64
  | BWUnknown BWVar -- TODO: Why is this not a GName t instead of a BWVar?
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

data BufTy =
    -- | Internal buffer (for parallelization)
    IntBuf { bufty_ty :: Ty }

    -- | External buffer (for the `ReadSrc` or `WriteSnk`)
    --
    -- NOTE: We record the type that the program is reading/writing, _NOT_
    -- its base type (in previous versions we recorded the base type here).
  | ExtBuf { bufty_ty :: Ty }
  deriving (Generic, Typeable, Data, Eq, Ord)

{------------------------------------------------------------------------
  Expressions (parameterized by the (Haskell) type of (Ziria) types
------------------------------------------------------------------------}

data GUnOp t =
    NatExp
  | Neg
  | Not
  | BwNeg
  | Cast t   -- Cast to this target type
  | ALength
  deriving (Generic, Typeable, Data, Eq, Ord)

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
  deriving (Generic, Typeable, Data, Show, Eq, Ord)

data Val where
  VBit    :: Bool    -> Val
  VInt    :: Integer -> Val
  VDouble :: Double  -> Val
  VBool   :: Bool    -> Val
  VString :: String  -> Val
  VUnit   :: Val
  deriving (Generic, Typeable, Data, Show, Eq, Ord)

data LengthInfo
     = LISingleton
     | LILength Int  -- Invariant: > 0
     | LIMeta String -- For meta-variables in quasi-quotes only
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

data UnrollInfo
  = Unroll        -- force unroll
  | NoUnroll      -- force no-unroll
  | AutoUnroll    -- do whatever the compiler would do (no annotation)
  deriving (Generic, Typeable, Data, Eq, Ord)

-- If true, the binding should be forced to be inlined.
-- This is used by e.g. the vectorizer to bind inlinable
-- sub-arrays of the input array.
data ForceInline
  = ForceInline   -- Always inline
  | NoInline      -- Never inline
  | AutoInline    -- Let the compiler decide
  deriving (Generic, Typeable, Data, Eq, Ord)


-- | Dereference expressions, abstract over expressions inside
data AGDerefExp expr t
  = GDVar  (GName t)
  | GDProj (AGDerefExp expr t) FldName
  | GDArr  (AGDerefExp expr t) expr LengthInfo

  -- DELETEME
  -- | GDNewArray t [expr]
  --         -- NB: t is the array type, not the element type
  -- | GDNewStruct t [(FldName,expr)]
  --         -- NB: t is the struct type
  deriving Show

type GDerefExp t a = AGDerefExp (GExp t a) t

-- | Dereference expressions with abstract values as indices
type LVal idx = AGDerefExp idx Ty

derefToExp :: SrcLoc -> AGDerefExp (GExp t ()) t -> GExp t ()
derefToExp loc = go 
  where go (GDVar nm)           = MkExp (EVar nm) loc ()
        go (GDProj de fld)      = MkExp (EProj (go de) fld) loc ()
        go (GDArr de1 e2 li)    = MkExp (EArrRead (go de1) e2 li) loc ()
        -- DELETEME
        -- go (GDNewArray _t es)   = MkExp (EValArr es) loc ()
        -- go (GDNewStruct t flds) = MkExp (EStruct t flds) loc ()


isMutGDerefExp :: GExp t a -> Maybe (GDerefExp t a)
isMutGDerefExp e = case unExp e of
  EVar nm | not (isMutable nm) -> Nothing
          | otherwise          -> Just (GDVar nm)
  EProj estruct fld -> do
    gde <- isMutGDerefExp estruct
    return (GDProj gde fld)
  EArrRead earr estart elen -> do
    gdarr <- isMutGDerefExp earr
    return (GDArr gdarr estart elen)
  _ -> Nothing -- All other cases are immutable


checkArgMut :: [ArgTy]    -- Function argument types (expected)
            -> [GExp t a] -- Arguments
            -> Bool       -- Mutable arguments must be isMutGDerefExp
checkArgMut fun_tys args = all check_mut (zip fun_tys args)
  where check_mut (GArgTy _ Mut, earg) = isJust $ isMutGDerefExp earg
        check_mut _other               = True

data GExp0 t a where
  -- | A single value
  --
  -- We record the type of the value because literals are overloaded.
  EVal :: t -> Val -> GExp0 t a

  -- | An array value
  EValArr :: [GExp t a] -> GExp0 t a

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
  -- TODO -- TODO : let bindings too are important to lift to normal forms!
  -- 
  -- See semantics for details.
  EAssign :: GExp t a -> GExp t a -> GExp0 t a

  -- | Array write
  --
  -- See comments for `EArrRead` and `EAssign`.
  --
  -- TODO: Maybe merge with `EAssign`.
  EArrWrite :: GExp t a -> GExp t a -> LengthInfo -> GExp t a -> GExp0 t a

  EFor :: UnrollInfo -> GName t -> GExp t a -> GExp t a -> GExp t a -> GExp0 t a


  EWhile :: GExp t a -> GExp t a -> GExp0 t a


  ELet :: GName t -> ForceInline -> GExp t a -> GExp t a -> GExp0 t a

  -- | Potentially initialized read/write variable
  ELetRef :: GName t -> Maybe (GExp t a) -> GExp t a -> GExp0 t a

  ESeq :: GExp t a -> GExp t a -> GExp0 t a
  ECall :: GName t -> [GExp t a] -> GExp0 t a
  EIf :: GExp t a -> GExp t a -> GExp t a -> GExp0 t a

  -- | Print any expression, for debugging
  EPrint :: Bool -> [GExp t a] -> GExp0 t a

  -- | Generate runtime failure, with error report
  EError :: t -> String -> GExp0 t a
  ELUT :: LUTStats -> GExp t a -> GExp0 t a

  -- | Constructing structs
  --
  -- We annotate the EStruct with the "official" definition of the struct
  -- so that we can lint an EStruct node without the definition of the
  -- struct having to be in scope.
  EStruct :: t -> [(FldName,GExp t a)] -> GExp0 t a

  -- | Project field out of a struct
  EProj   :: GExp t a -> FldName -> GExp0 t a

  deriving (Eq, Ord) 

data GExp t a
  = MkExp { unExp  :: !(GExp0 t a)
          , expLoc :: !SrcLoc
          , info :: a }
  deriving (Eq, Ord)

instance Located (GExp t a) where
    locOf = locOf . expLoc

-- Structure definitions
data GStructDef t
  = StructDef { struct_name :: TyName
              , struct_flds :: [(FldName,t)] }
  deriving (Generic, Typeable, Data)

data GFun0 t a where
  MkFunDefined  :: GName t     -- ^ name
                -> [GName t]   -- ^ params
                -> GExp t a    -- ^ body
                -> GFun0 t a
  MkFunExternal :: GName t     -- ^ name
                -> [GName t]   -- ^ params
                -> t           -- ^ return type
                -> GFun0 t a
  deriving (Eq, Ord) 

{- TODO plug this in at some point
data FunDef a body
  = FunDef { funName   :: GName t
           , funParams :: [(GName t,Ty)]
           , funLocals :: [(GName t,Ty,Maybe (Exp a))]
           , funDef    :: body }
-}

data GFun t a
  = MkFun { unFun   :: GFun0 t a
          , funLoc  :: SrcLoc
          , funInfo :: a }
  deriving (Eq, Ord) 

funName :: GFun t a -> GName t
funName (MkFun (MkFunDefined  nm _ _) _ _) = nm
funName (MkFun (MkFunExternal nm _ _) _ _) = nm

{-------------------------------------------------------------------------------
  NFData instances

  (Mostly for debugging)
-------------------------------------------------------------------------------}

instance NFData BinOp       where rnf = genericRnf
instance NFData BitWidth    where rnf = genericRnf
instance NFData BufTy       where rnf = genericRnf
instance NFData ForceInline where rnf = genericRnf
instance NFData LengthInfo  where rnf = genericRnf
instance NFData NumExpr     where rnf = genericRnf
instance NFData Ty          where rnf = genericRnf
instance NFData UnrollInfo  where rnf = genericRnf
instance NFData Val         where rnf = genericRnf
instance NFData MutKind     where rnf = genericRnf
instance NFData t => NFData (GArgTy t) where rnf = genericRnf

instance NFData Uniq        where rnf = genericRnf
instance NFData t => NFData (GUnOp t) where rnf = genericRnf
instance NFData t => NFData (GName t) where rnf = genericRnf

-- instance (NFData t, NFData a) => NFData (GExp0 t a) where rnf = genericRnf
-- instance (NFData t, NFData a) => NFData (GExp  t a) where rnf = genericRnf

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

type SrcExp = GExp SrcTy ()
type SrcFun = GFun SrcTy ()

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
    go (TArrow ts t)       = do ts' <- mapM go_arg ts
                                t'  <- go t
                                f $ TArrow ts' t'
    go (TBuff (IntBuf bt)) = do bt' <- go bt
                                f $ TBuff (IntBuf bt')
    go (TBuff (ExtBuf bt)) = do bt' <- go bt
                                f $ TBuff (ExtBuf bt')

    go TVoid               = f $ TVoid

    go_arg (GArgTy t m) = do { t' <- go t ; return (GArgTy t' m) }

mapNameM :: Monad m => (t -> m t') -> GName t -> m (GName t')
mapNameM onTyp MkName{..} = do
    nameTyp' <- onTyp nameTyp
    return MkName{nameTyp = nameTyp', ..}


mapExpM :: forall t t' a a' m. Monad m
        => (t -> m t')                     -- ^ On types
        -> (a -> m a')                     -- ^ On annotations
        -> (GExp t' a' -> m (GExp t' a'))  -- ^ Combine results
        -> GExp t a
        -> m (GExp t' a')
mapExpM onTyp onAnn f = mapExpM_env onTyp onAnn f (const id)


-- | Most general form of mapping over expressions
mapExpM_env :: forall t t' a a' m. Monad m
        => (t -> m t')                     -- ^ On types
        -> (a -> m a')                     -- ^ On annotations
        -> (GExp t' a' -> m (GExp t' a'))  -- ^ Combine results
        -> (GName t -> m (GExp t' a') -> m (GExp t' a'))
        -> GExp t a
        -> m (GExp t' a')
mapExpM_env onTyp onAnn f extend = goExp
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
    goExp0 (EValArr elems) = do
      elems' <- mapM goExp elems
      return $ EValArr elems'
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
    goExp0 (EFor ui nm1 e1 e2 e3) = do
      nm1' <- mapNameM onTyp nm1
      e1'  <- goExp e1
      e2'  <- goExp e2
      e3'  <- extend nm1 (goExp e3)
      return $ EFor ui nm1' e1' e2' e3'
    goExp0 (EWhile e1 e2) = do
      e1' <- goExp e1
      e2' <- goExp e2
      return $ EWhile e1' e2'
    goExp0 (ELet nm1 fi e1 e2) = do
      nm1' <- mapNameM onTyp nm1
      e1'  <- goExp e1
      e2'  <- extend nm1 (goExp e2)
      return $ ELet nm1' fi e1' e2'
    goExp0 (ELetRef nm1 Nothing e2) = do
      nm1' <- mapNameM onTyp nm1
      e2'  <- extend nm1 (goExp e2)
      return $ ELetRef nm1' Nothing e2'
    goExp0 (ELetRef nm1 (Just e1) e2) = do
      nm1' <- mapNameM onTyp nm1
      e1'  <- goExp e1
      e2'  <- extend nm1 (goExp e2)
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
    goExp0 (EPrint nl e1s) = do
      e1s' <- mapM goExp  e1s
      return $ EPrint nl e1s'
    goExp0 (EError t err) = do
      t' <- onTyp t
      return $ EError t' err
    goExp0 (ELUT r e1) = do
      r'   <- goRanges r
      e1'  <- goExp e1
      return $ ELUT r' e1'
    goExp0 (EStruct t fields) = do
      t' <- onTyp t
      let do_fld (fld,e') = goExp e' >>= \e'' -> return (fld,e'')
      fields' <- mapM do_fld fields
      return $ EStruct t' fields'
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

    goRanges :: LUTStats -> m LUTStats
    goRanges = return 

mapFunM_env :: forall t t' a a' m. Monad m
            => (t -> m t')                               -- ^ On types
            -> (a -> m a')                               -- ^ On annotations
            -> ([GName t] -> GExp t a -> m (GExp t' a')) -- ^ On expressions
            -> GFun t a
            -> m (GFun t' a')
mapFunM_env onTyp onAnn onExp = goFun
  where
    goFun :: GFun t a -> m (GFun t' a')
    goFun MkFun{..} = do
      unFun'   <- goFun0 unFun
      funInfo' <- onAnn funInfo
      return MkFun{unFun = unFun', funInfo = funInfo', ..}

    goFun0 :: GFun0 t a -> m (GFun0 t' a')
    goFun0 (MkFunDefined nm params body) = do
      nm'     <- mapNameM onTyp nm
      params' <- mapM (mapNameM onTyp) params
      body'   <- onExp (nm : params) body
      return $ MkFunDefined nm' params' body'
    goFun0 (MkFunExternal nm params ret) = do
      nm'     <- mapNameM onTyp nm
      params' <- mapM (mapNameM onTyp) params
      ret'    <- onTyp ret
      return $ MkFunExternal nm' params' ret'

mapFunM :: forall t t' a a' m. Monad m
        => (t -> m t')                               -- ^ On types
        -> (a -> m a')                               -- ^ On annotations
        -> (GExp t a -> m (GExp t' a')) -- ^ On expressions
        -> GFun t a
        -> m (GFun t' a')
mapFunM onTyp onAnn onExp = mapFunM_env onTyp onAnn (const onExp)

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

mapExp_env :: (t -> t')                   -- ^ On types
           -> (a -> a')                   -- ^ On annotations
           -> (GExp t' a' -> GExp t' a')  -- ^ Combine results
           -> (GName t -> GExp t' a' -> GExp t' a')
           -> GExp t a
           -> GExp t' a'
mapExp_env onTyp onAnn f extend 
  = runIdentity . mapExpM_env (Identity . onTyp)
                              (Identity . onAnn)
                              (Identity . f)
                              (\x y -> Identity (extend x (runIdentity y)))
                                             

mapFun_env :: (t -> t')                             -- ^ On types
           -> (a -> a')                             -- ^ On annotations
           -> ([GName t] -> GExp t a -> GExp t' a') -- ^ On expressions
           -> GFun t a
           -> GFun t' a'
mapFun_env onTyp onAnn f 
  = runIdentity . mapFunM_env (Identity . onTyp)
                              (Identity . onAnn)
                              (\x y -> Identity (f x y))


mapFun :: (t -> t')                    -- ^ On types
       -> (a -> a')                    -- ^ On annotations
       -> (GExp t a -> (GExp t' a'))   -- ^ On expressions
       -> GFun t a
       -> GFun t' a'
mapFun onTyp onAnn f = mapFun_env onTyp onAnn (const f)




{-------------------------------------------------------------------------------
  Erase annotations
-------------------------------------------------------------------------------}

eraseExp :: GExp t a -> GExp t ()
eraseExp = mapExp id (const ()) id

eraseFun :: GFun t a -> GFun t ()
eraseFun = mapFun id (const ()) eraseExp

-- | Remove all location annotations
--
-- This is on Exp rather than GExp because we cannot completely remove all
-- location annotations from SrcTys
eraseLoc :: Exp -> Exp
eraseLoc = mapExp id -- types don't change
                  id -- annotations don't change
                  (\e -> e { expLoc = noLoc }) -- delete locations

{-------------------------------------------------------------------------------
  Free variables
-------------------------------------------------------------------------------}


type GNameSet t = S.Set (GName t)


deleting :: GNameSet t -> GName t -> GNameSet t
deleting st n = S.delete n st

-- | Collect free variables in an expression
--
-- The `takeFuns` argument indicates whether we want to include the names of
-- functions that are called in the expression. This is useful when we are
-- computing the free variables in a nested function definition (which become
-- additional arguments to the function when we generate C code).
exprFVs' :: forall t b. Bool -> GExp t b -> GNameSet t
exprFVs' takeFuns = goExp
  where
    goExp = goExp0 . unExp
    goExp0 (EVar nm)                  = S.singleton nm
    goExp0 (EVal {})                  = mempty
    goExp0 (EValArr es)               = goExps es
    goExp0 (EUnOp _op e1)             = goExp e1 
    goExp0 (EBinOp _op e1 e2)         = goExp e1 `mappend` goExp e2
    goExp0 (EAssign e1 e2)            = goExp e1 `mappend` goExp e2
    goExp0 (EArrRead e1 e2 _r)        = goExp e1 `mappend` goExp e2
    goExp0 (EArrWrite e1 e2 _r e3)    = goExp e1 `mappend` goExp e2 `mappend` goExp e3
    goExp0 (EFor _ui nm1 e1 e2 e3)    = goExp e1 `mappend` goExp e2 `mappend` (goExp e3 `deleting` nm1)
    goExp0 (EWhile e1 e2)             = goExp e1 `mappend` goExp e2
    goExp0 (ELet nm1 _fi e1 e2)       = goExp e1 `mappend` (goExp e2 `deleting` nm1)
    goExp0 (ELetRef nm1 Nothing e2)   = goExp e2 `deleting` nm1
    goExp0 (ELetRef nm1 (Just e1) e2) = goExp e1 `mappend` (goExp e2 `deleting` nm1)
    goExp0 (ESeq e1 e2)               = goExp e1 `mappend` goExp e2
    goExp0 (ECall fun es)             = goExps es `mappend` (if takeFuns then S.singleton fun else mempty)
    goExp0 (EIf e1 e2 e3)             = goExp e1 `mappend` goExp e2 `mappend` goExp e3
    goExp0 (EPrint _nl e1s)           = goExps e1s
    goExp0 (EError _t _err)           = mempty
    goExp0 (ELUT _r e1)               = goExp e1
    goExp0 (EStruct _t fields)        = goExps (map snd fields)
    goExp0 (EProj e1 _fn)             = goExp e1

    goExps = mconcat . map goExp 


-- NB: Take function variables (hence True)
exprFVs :: GExp t b -> S.Set (GName t)
exprFVs = exprFVs' True


-- NB: Don't take function variables when computing fvs of closure
exprFVsClos :: GExp t b -> GNameSet t
exprFVsClos = exprFVs' False

funFVs :: GFun t a -> GNameSet t
funFVs f = case unFun f of
  MkFunDefined _nm params body  -> exprFVs body S.\\ S.fromList params
  MkFunExternal _nm _params _ty -> S.empty

-- | Find free variables in a function definition
funFVsClos :: GFun t a -> GNameSet t
funFVsClos f = case unFun f of
    MkFunDefined _nm params body  -> exprFVsClos body S.\\ S.fromList params
    MkFunExternal _nm _params _ty -> S.empty

{-------------------------------------------------------------------------------
  Substitutions
-------------------------------------------------------------------------------}

substTy :: [(LenVar, NumExpr)] -> Ty -> Ty
substTy slen = mapTy aux
  where
    -- mapTy works bottom up: we already substituted in the subtypes (ty)
    aux (TArray (NVar x) ty) = case lookup x slen of
                                 Just ne -> TArray ne ty
                                 Nothing -> TArray (NVar x) ty
    aux ty                   = ty

-- | Apply substitution to an expression
--
-- NOTE: We assume that the substitution _as a whole_ is idempotent. In
-- particular, we assume that if the expressions in the codomain of the
-- expression substitution do not mention any of the length variables in
-- the domain of the type substitution.
substExp :: [(LenVar,NumExpr)] -> [(GName Ty,GExp Ty a)] -> GExp Ty a -> GExp Ty a
substExp slen sexp ge
  = mapExpM_env (\t _ -> substTy slen t)
                (\x _ -> x)
                aux
                ext ge sexp
  where
    aux e s
      | EVar x <- unExp e
      , Just e' <- lookup x s
      = e'
      | otherwise = e
    
    ext nm act s = act (filter (\(snm,_) -> snm /= nm) s)



{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

getLnNumInStr :: SrcLoc -> String
getLnNumInStr csp
   = case locOf csp of
       Loc p _  -> "ln" ++ (show . posLine) p ++ "_"
       NoLoc    -> "ln_"

isEVal :: GExp t a -> Bool
isEVal e
  | EVal {} <- unExp e
  = True
isEVal _
  = False

isStructTy :: Ty -> Bool
isStructTy (TStruct _ _) = True
isStructTy _             = False

-- User-defined structure (as oppose to Ziria native structs such as complex numbers)
isUserStructTy :: Ty -> Bool
isUserStructTy t = (isStructTy t) && not (isComplexTy t)

isArrayTy :: Ty -> Bool
isArrayTy (TArray _ _) = True
isArrayTy _            = False

isArrayTy_maybe :: Ty -> Maybe Ty
-- If an array type, gives you back the type of the elements
isArrayTy_maybe (TArray _n t) = Just t
isArrayTy_maybe _other        = Nothing

isBufTy :: Ty -> Bool
isBufTy (TBuff {}) = True
isBufTy _          = False

atomTyOf :: Ty -> Ty
-- Give you back the biggest non-array type under this type
atomTyOf (TArray _ t) = atomTyOf t
atomTyOf t            = t

-- Arity 
tyArity :: Ty -> Int
tyArity (TArray (Literal n) _) = n
tyArity _t = 1


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
toExp a e = MkExp { unExp = e, expLoc = noLoc, info = a }

toExpPos :: a -> SrcLoc -> GExp0 t a -> GExp t a
toExpPos a pos e = MkExp { unExp = e, expLoc = pos, info = a }

binopList :: BinOp -> a -> GExp t a -> [GExp t a] -> GExp t a
binopList _  _ e0 []       = e0
binopList op a e0 (e : es) = toExp a $ EBinOp op e (binopList op a e0 es)

dotDotName :: String
dotDotName = "..."


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
    TVoid {}     -> False


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


toFunPos :: a -> SrcLoc -> GFun0 t a -> GFun t a
toFunPos a pos fn = MkFun fn pos a

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
  EVal _ _                     -> False
  EValArr elems                -> any mutates_state elems
  EVar _                       -> False
  EUnOp _ e'                   -> mutates_state e'
  EBinOp _ e1 e2               -> any mutates_state [e1,e2]
  EAssign _e1 _e2              -> True
  EArrRead e1 e2 _li           -> any mutates_state [e1,e2]
  EArrWrite _e1 _e2 _r _e3     -> True
  EFor _ _ e1 e2 e3            -> any mutates_state [e1,e2,e3]
  EWhile e1 e2                 -> any mutates_state [e1,e2]
  ELet _nm _fi e1 e2           -> any mutates_state [e1,e2]
  ELetRef _nm (Just e1) e2     -> any mutates_state [e1,e2]
  ELetRef _nm Nothing   e2     -> mutates_state e2
  ESeq e1 e2                   -> any mutates_state [e1,e2]
  ECall _e' _es                -> True -- See Note [Local funs]
  EIf e1 e2 e3                 -> any mutates_state [e1,e2,e3]
  EPrint _nl _e1               -> True -- See Note [IOEffects]
  EError _ _                   -> True
  ELUT _ e1                    -> mutates_state e1
  EStruct _ tfs                -> any mutates_state (map snd tfs)
  EProj e0 _f                  -> mutates_state e0

{-------------------------------------------------------------------------------
  Built-ins
-------------------------------------------------------------------------------}

-- | Primitive complex structures
--
-- This is necessary for the translation from SrcTy (where structs have names
-- only) to Ty (where structs are fully defined)
primComplexStructs :: [(TyName,StructDef)]
primComplexStructs
  = [ structDefFrom tcomplex8
    , structDefFrom tcomplex16
    , structDefFrom tcomplex32
    , structDefFrom tcomplex64
    ]
  where
    -- TODO: This translation back and forth to StructDef is annoying.
    -- We should inline StructDef in LetStruct.
    structDefFrom :: Ty -> (TyName, StructDef)
    structDefFrom (TStruct nm flds) = (nm, StructDef nm flds)
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
instance PrettyVal Uniq
instance PrettyVal MutKind
instance PrettyVal t => PrettyVal (GArgTy t)


instance PrettyVal t => PrettyVal (GName t)
instance PrettyVal t => PrettyVal (GUnOp t)
instance PrettyVal t => PrettyVal (GStructDef t)

-- instance (PrettyVal t, PrettyVal a) => PrettyVal (GExp0 t a)
-- instance (PrettyVal t, PrettyVal a) => PrettyVal (GExp t a)
-- instance (PrettyVal t, PrettyVal a) => PrettyVal (GFun t a)
-- instance (PrettyVal t, PrettyVal a) => PrettyVal (GFun0 t a)

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

Note [Local funs]
~~~~~~~~~~~~~~~~~

For the mutates_state analysis we don't have to check the function body
because if we ever call it, we always return true (see case for ECall). If we
change this definition for ECall we might have to do something different for
local functions.
-}



