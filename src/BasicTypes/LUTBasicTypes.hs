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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveDataTypeable #-}

{- This module defines datatypes that are used for lookup table generation,
   including range the range analysis. 
 -}

module LUTBasicTypes where 

import Outputable
import Text.PrettyPrint.HughesPJ
import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Data.Maybe ( isJust )
import Data.Typeable
import Data.Data 

import Control.Applicative
import Control.Monad 

import AstName
import AstExprTypes 

import NameEnv 

{------------------------------------------------------------------
  LUT statistics
-------------------------------------------------------------------}

-- | LUT statistics
data LUTStats = LUTStats { 
     lutInBitWidth      :: Integer -- ^ Index bitwidth
   , lutOutBitWidth     :: Integer -- ^ Total output bitwidth (contains result!)
   , lutResultBitWidth  :: Integer -- ^ Result bitwidth
   , lutTableSize       :: Integer -- ^ Size needed (in bytes)
   , lutVarUsePkg       :: VarUsePkg -- ^ Var use info

     -- | If @lutResultInOutVars = Just v@ then 
     --   (a) @v@ is the result variable
     --   (b) and is in @vu_outvars lutVarUsePkg@
   , lutResultInOutVars :: Maybe EId
   , lutShould          :: Either String Bool -- ^ Should we LUT?
   } 
   deriving (Eq, Ord)

instance Outputable LUTStats where
  ppr s = vcat $
      [ pprVarUsePkg (lutVarUsePkg s)
      , text "   result bitsize:" <+> resdoc
      , text "    input bitsize:" <+> ppr (lutInBitWidth s)
      , text "   output bitsize:" <+> ppr (lutOutBitWidth s)
      , text "lut size in bytes:" <+> ppr (lutTableSize s)
      , text " should be lutted:" <+> should (lutShould s) 
      ]
   where
    should :: Either String Bool -> Doc
    should (Left err)    = text "No, because" <+> text err
    should (Right False) = text "No"
    should (Right True)  = text "Yes"
    resdoc = if isJust $ lutResultInOutVars s
             then text "included in output variables"
             else ppr (lutResultBitWidth s)


{----------------------- Variable use information -----------------}

data VarUsePkg 
  = VarUsePkg { vu_invars   :: [EId]
              , vu_outvars  :: [EId]
              , vu_allvars  :: [EId]
              , vu_ranges   :: RngMap }
  deriving (Typeable, Data, Eq, Ord) 

pprVarUsePkg :: VarUsePkg -> Doc
pprVarUsePkg (VarUsePkg invars outvars allvars rmap)
  = vcat [ text "  input variables:" <+> ppr invars
         , text " output variables:" <+> ppr outvars
         , text "    all variables:" <+> ppr allvars
         , text "      ranges used:" <+> pprRanges rmap
         ]


{----------------------- Ranges -----------------------------------}

pprRanges :: RngMap -> Doc
pprRanges r = vcat $
  map (\(k,v) -> ppr k <> char ':' <+> text (show v)) (neToList r)


-- | Range
data Range 
  = RInt  (IVal Integer)    -- integers
  | RBool (IVal Bool)       -- booleans
  | RArr Interval Interval  -- arrays
  | ROther                  -- other but also equivalent to bottom
  deriving (Generic, Typeable, Data, Eq, Show, Ord)


-- | Intervals
data Iv = Iv Integer Integer  -- i1 <= i2
        | IvEmpty
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

instance NFData Iv where
  rnf = genericRnf

type Interval = IVal Iv

instance NFData Interval where
  rnf = genericRnf


data IVal v = IUnknown | IKnown v
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

instance Monad IVal where
  return a = IKnown a
  m >>= f  = case m of 
    IUnknown -> IUnknown
    IKnown x -> f x

-- Boilerplate
instance Functor IVal where
    fmap f x = x >>= return . f
instance Applicative IVal where
    pure   = return
    (<*>)  = ap

type RngMap = NameMap Ty Range

