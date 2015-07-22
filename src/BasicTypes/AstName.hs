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
module AstName where

import Prelude hiding (exp, mapM)
import Data.Loc
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.DeepSeq.Generics (NFData(..), genericRnf)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import Text.PrettyPrint.HughesPJ
import Outputable

import Data.Char ( isAlphaNum )

import Orphans ()

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
           , nameDoc :: String
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
  show (MkName _x _id _ _ _loc d)    = d


toName :: String -> SrcLoc -> t -> MutKind -> GName t
toName s mpos typ mk =
    MkName { name    = s
           , uniqId  = MkUniq s
           , nameLoc = mpos
           , nameMut = mk
           , nameTyp = typ
           , nameDoc = s
           }

updNameId :: Uniq -> GName t -> GName t
updNameId uid nm = nm { uniqId = uid }

updNameTy :: GName t -> u -> GName u
updNameTy (MkName n i _ mk l s) utyp = MkName n i utyp mk l s

getNameWithUniq :: GName t -> String
getNameWithUniq nm = name nm ++ "_blk" ++ unUniq (uniqId nm)

alphaNumStr :: String -> String
alphaNumStr s = map (\c -> if isAlphaNum c then c else '_') s


{---------------- Printing names ---------------------------------}


instance Outputable ty => Outputable (GName ty) where
  ppr ix = ppName ix

ppName :: GName ty -> Doc
ppName nm = ppNameUniq nm
            -- text (name nm)

ppNameDoc :: Outputable ty => GName ty -> Doc
ppNameDoc nm = ppr nm <> braces (text (nameDoc nm))

ppNameUniq :: GName ty -> Doc
ppNameUniq nm = text (name nm) <> braces (text $ show $ uniqId nm)

instance Outputable MutKind where
  ppr mk = text (show mk)

{-------------- Other instances ---------------------------------}

instance NFData t => NFData (GName t) where rnf = genericRnf
instance PrettyVal t => PrettyVal (GName t)

instance NFData MutKind     where rnf = genericRnf
instance NFData Uniq        where rnf = genericRnf

instance PrettyVal Uniq
instance PrettyVal MutKind
