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

{-------------------------------------------------------------------------------
  Name environments (implemented as lists to deal correctly with shadowing) 
-------------------------------------------------------------------------------}

type NameEnv t a = [(GName t, a)]
type NameMap t a = NameEnv t a 


neLookup :: GName t -> NameEnv t a -> Maybe a
neLookup _nm [] = Nothing
neLookup nm ((nm1,a):rest)
  | nm1 == nm = Just a
  | otherwise = neLookup nm rest

neUnionWith :: NameEnv t a -> NameEnv t a 
            -> (GName t -> a -> a -> a)
            -> NameEnv t a
neUnionWith nea neb f = go nea neb
  where 
    go [] ne2 = ne2
    go ((n1,a1):ne1') ne2 = neUpdate n1 aux (go ne1' ne2)
      where 
        aux Nothing    = Just a1
        aux (Just a1') = Just (f n1 a1 a1')



neExtend :: GName t -> a -> NameEnv t a -> NameEnv t a
neExtend nm a menv = aux (neLookup nm menv)
  where
    aux Nothing = (nm,a) : menv
    aux Just {} = warn (text "Shadowing" <+> ppNameUniq nm) $ (nm,a) : menv

neEmpty :: NameEnv t a
neEmpty = []

neFromList :: [(GName t, a)] -> NameEnv t a
neFromList x = x

neToList :: NameEnv t a -> [(GName t,a)]
neToList x = x

neKeys :: NameEnv t a -> [GName t]
neKeys = map fst

neExtendMany :: [(GName t, a)] -> NameEnv t a -> NameEnv t a
neExtendMany bnds orig = bnds ++ orig

neUpdate :: GName t
         -> (Maybe a -> Maybe a)
         -> NameEnv t a -> NameEnv t a
neUpdate nm f [] 
  = case f Nothing of 
      Nothing -> []
      Just a  -> [(nm,a)]
neUpdate nm f ((nm',a):rest) 
  | nm == nm'
  = case f (Just a) of 
      Nothing -> rest
      Just a' -> (nm',a'):rest
  | otherwise
  = (nm',a) : neUpdate nm f rest
