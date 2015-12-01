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

{-# LANGUAGE FlexibleInstances #-}

module Outputable where

import Text.PrettyPrint.HughesPJ
import Data.Loc
import qualified Data.Map as M

class Outputable a where
  ppr :: a -> Doc

instance Outputable Int where
  ppr = integer . fromIntegral

instance Outputable Integer where
  ppr = integer . fromIntegral

instance {-# OVERLAPPABLE #-} Outputable a => Outputable [a] where
  ppr = sep . punctuate comma . map ppr

instance (Outputable a, Outputable b) => Outputable (a,b) where
  ppr (a,b) = parens (ppr a <> comma <+> ppr b)

instance {-# OVERLAPPING #-} Outputable String where
  ppr = text 

instance Outputable a => Outputable (Maybe a) where 
  ppr Nothing  = text "N/A"
  ppr (Just a) = ppr a

instance (Outputable b, Outputable a) => Outputable (Either a b) where 
  ppr (Left x1)  = ppr x1
  ppr (Right x2) = ppr x2

instance Outputable Doc where
  ppr = id

instance Outputable SrcLoc where
  ppr = text . displayLoc . locOf

pretty :: Outputable a => a -> String
pretty = show . ppr

instance (Outputable k, Outputable v) => Outputable (M.Map k v) where
  ppr = vcat . map ppr_kv . M.toList
   where ppr_kv (k,v) = ppr k <+> text "|->" <+> ppr v

emptydoc :: Doc 
emptydoc = empty
