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
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module NameEnv (
   NameEnv
 , NameMap
 , neLookup
 , neExtend
 , neEmpty
 , neFromList
 , neKeys
 , neExtendMany
 , neUpdate
 , neToList
 , neUnionWith
) where 

import AstExpr
import PpExpr
import Utils ( warn )
import Text.PrettyPrint.HughesPJ
import Outputable

import Control.DeepSeq (NFData) 
import Data.Data (Data)
import Data.Typeable (Typeable)
import Text.Show.Pretty (PrettyVal)

{-------------------------------------------------------------------------------
  Name environments (implemented as lists to deal correctly with shadowing) 
-------------------------------------------------------------------------------}

newtype NameEnv t a = NameEnv { unNameEnv :: [(GName t, a)] }
  deriving (Typeable, Data, NFData, PrettyVal, Eq, Ord)

type NameMap t a = NameEnv t a 

instance (Outputable t, Outputable a) => Outputable (NameEnv t a) where
  ppr (NameEnv lst) = 
    vcat (map (\(n,a) -> ppr n <+> text "|->" <+> ppr a) lst)

neLookup :: GName t -> NameEnv t a -> Maybe a
neLookup x (NameEnv env) = go x env
  where go _nm [] = Nothing
        go nm ((nm1,a):rest)
          | nm1 == nm = Just a
          | otherwise = go nm rest

neUnionWith :: NameEnv t a -> NameEnv t a 
            -> (GName t -> a -> a -> a)
            -> NameEnv t a
neUnionWith (NameEnv nea) neb f = go nea neb
  where 
    go [] ne2 = ne2
    go ((n1,a1):ne1') ne2 = neUpdate n1 aux $ (go ne1' ne2)
      where 
        aux Nothing    = Just a1
        aux (Just a1') = Just (f n1 a1 a1')



neExtend :: GName t -> a -> NameEnv t a -> NameEnv t a
neExtend nm a menv = aux (neLookup nm menv)
  where
    aux Nothing = NameEnv ((nm,a) : unNameEnv menv)
    aux Just {} = warn (text "Shadowing" <+> ppNameUniq nm) $ 
                  NameEnv ((nm,a) : unNameEnv menv)

neEmpty :: NameEnv t a
neEmpty = NameEnv []

neFromList :: [(GName t, a)] -> NameEnv t a
neFromList x = NameEnv x

neToList :: NameEnv t a -> [(GName t,a)]
neToList = unNameEnv

neKeys :: NameEnv t a -> [GName t]
neKeys = map fst . unNameEnv

neExtendMany :: [(GName t, a)] -> NameEnv t a -> NameEnv t a
neExtendMany bnds (NameEnv orig) = NameEnv (bnds ++ orig)

neUpdate :: GName t
         -> (Maybe a -> Maybe a)
         -> NameEnv t a -> NameEnv t a
neUpdate x h (NameEnv env) 
  = NameEnv (go x h env)
  where go nm f [] 
          = case f Nothing of 
              Nothing -> []
              Just a  -> [(nm,a)]
        go nm f ((nm',a):rest) 
          | nm == nm'
          = case f (Just a) of 
              Nothing -> rest
              Just a' -> (nm',a'):rest
          | otherwise
          = (nm',a) : go nm f rest
