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
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wall #-}
module AstExprTypes where

import Prelude hiding (exp, mapM)
import Data.Loc
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Orphans ()
import AstName 


import Outputable
import Text.PrettyPrint.HughesPJ

import Text.Show.Pretty (PrettyVal)
import Control.DeepSeq (NFData(..))

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
  Types in the source language

  (No type variables, "length" expressions)
-------------------------------------------------------------------------------}

data SrcTy where
  SrcTUnit     :: SrcTy
  SrcTBit      :: SrcTy
  SrcTBool     :: SrcTy

  SrcTArray    :: SrcNumExpr -> SrcTy -> SrcTy
  SrcTInt      :: SrcBitWidth -> SrcSignedness -> SrcTy
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

-- | Source-language integer types are annotated with a signedness flag.
data SrcSignedness
  = SrcSigned
  | SrcUnsigned
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
  TInt      :: BitWidth -> Signedness -> Ty
  TDouble   :: Ty
  -- TODO: We could inline GStructDef here?
  TStruct   :: TyName -> [(FldName, Ty)] -> Ty
  TInterval :: Int -> Ty

  -- Arrow and buffer types
  TArrow :: [ArgTy] -> Ty -> Ty
  TBuff  :: BufTy -> Ty

  TVoid  :: Ty

  deriving (Generic, Typeable, Data, Eq, Ord, NFData)

-- An argument type (we record the mutability)
data GArgTy t
  = GArgTy { argty_ty  :: t
           , argty_mut :: MutKind
           }
  deriving (Generic, Typeable, Data, Eq, Ord, NFData)

type ArgTy = GArgTy Ty


data NumExpr where
  Literal :: Int -> NumExpr

  -- | NVar: Length to be inferred from the context (or polymorphic)
  NVar :: LenVar -> NumExpr

  deriving (Generic, Typeable, Data, Eq, Ord, NFData)

data BitWidth
  = BW8
  | BW16
  | BW32
  | BW64
  | BWUnknown BWVar -- TODO: Why is this not a GName t instead of a BWVar?
  deriving (Generic, Typeable, Data, Eq, Ord, Show, NFData)

data Signedness
  = Signed
  | Unsigned
  deriving (Generic, Typeable, Data, Eq, Ord, Show, NFData)
           
data BufTy =
    -- | Internal buffer (for parallelization)
    IntBuf { bufty_ty :: Ty }

    -- | External buffer (for the `ReadSrc` or `WriteSnk`)
    --
    -- NOTE: We record the type that the program is reading/writing, _NOT_
    -- its base type (in previous versions we recorded the base type here).
  | ExtBuf { bufty_ty :: Ty }
  deriving (Generic, Typeable, Data, Eq, Ord, NFData)



type EId = GName Ty


{---------------------- Printing types ------------------------------------}

instance Outputable BitWidth where
  ppr bw = case bw of
    BW8  -> text "8"
    BW16 -> text "16"
    BW32 -> text "32"
    BW64 -> text "64"
    BWUnknown _nm -> text ""
    -- Or maybe print the name?

instance Outputable Signedness where
  ppr signedness = case signedness of
    Signed   -> text ""    
    Unsigned -> text "u"

instance Outputable SrcBitWidth where
  ppr bw = case bw of
    SrcBW8  -> text "8"
    SrcBW16 -> text "16"
    SrcBW32 -> text "32"
    SrcBW64 -> text "64"

instance Outputable SrcSignedness where
  ppr signedness = case signedness of
    SrcSigned   -> text ""    
    SrcUnsigned -> text "u"

instance Outputable t => Outputable (GArgTy t) where
  ppr (GArgTy t m) = parens (ppr m <+> ppr t)

instance Outputable Ty where
  ppr ty = case ty of
    TVar x                 -> text "?" <> text x
    TUnit                  -> text "()"
    TBit                   -> text "bit"
    TInt bw sg             -> ppr sg <> text "int" <> ppr bw
    TDouble                -> text "double"
    TBool                  -> text "bool"
    TString                -> text "string"
    TArray (Literal n) ty' -> text "arr" <> brackets (int n) <+> ppr ty'
    TArray (NVar n)    ty' -> text "arr" <> brackets (text (show n)) <+> ppr ty'
    TArrow tys tyres       -> parens (hsep (punctuate comma (map ppr tys))) <+> text "->" <+> ppr tyres
    TInterval n            -> text "interval" <> brackets (int n)
    TBuff (IntBuf t)       -> parens $ text "INTBUF" <> brackets (ppr t)
    TBuff (ExtBuf bt)      -> parens $ text "EXTBUF" <> brackets (text "base=" <> ppr bt)
    TStruct tyname _       -> text tyname 
    -- NOTE: If we change this to be the full type the instance for EStruct breaks

    TVoid                  -> text "void"

instance Outputable SrcTy where
  ppr ty = case ty of
    SrcTUnit       -> text "()"
    SrcTBit        -> text "bit"
    SrcTInt bw sg  -> text "int" <> ppr bw <> ppr sg
    SrcTDouble     -> text "double"
    SrcTBool       -> text "bool"
    SrcTStruct nm  -> text nm
    SrcInject  sty -> ppr sty
    SrcTyUnknown   -> empty
    SrcTArray (SrcLiteral n) ty'
      -> text "arr" <> brackets (int n) <+> ppr ty'
    SrcTArray (SrcNVar _loc) ty'
      -> text "arr" <> brackets empty <+> ppr ty'
    SrcTArray (SrcNArr n) ty'
      -> text "arr" <> brackets (text "length" <> parens (ppName n)) <+> ppr ty'


instance Outputable NumExpr where
  ppr ne = case ne of
    Literal i -> int i
    NVar n    -> text n
    -- TODO: here and elsewhere, are the quotes around the name intentional?


instance PrettyVal BitWidth
instance PrettyVal Signedness
instance PrettyVal SrcBitWidth
instance PrettyVal SrcSignedness
instance PrettyVal Ty
instance PrettyVal SrcTy
instance PrettyVal BufTy
instance PrettyVal NumExpr
instance PrettyVal SrcNumExpr
instance PrettyVal t => PrettyVal (GArgTy t)
