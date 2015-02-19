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
 , neLookup
 , neExtend
 , neEmpty
 , neFromList
 , neKeys
 , neExtendMany
) where 

import AstExpr
import PpExpr
import Utils ( warn )
import Text.PrettyPrint.HughesPJ

{-------------------------------------------------------------------------------
  Name environments (implemented as lists to deal correctly with shadowing) 
-------------------------------------------------------------------------------}

type NameEnv t a = [(GName t, a)]

neLookup :: GName t -> NameEnv t a -> Maybe a
neLookup _nm [] = Nothing
neLookup nm ((nm1,a):rest)
  | nm1 == nm = Just a
  | otherwise = neLookup nm rest

neExtend :: GName t -> a -> NameEnv t a -> NameEnv t a
neExtend nm a menv = aux (neLookup nm menv)
  where
    aux Nothing = (nm,a) : menv
    aux Just {} = warn (text "Shadowing" <+> ppNameUniq nm) $ (nm,a) : menv

neEmpty :: NameEnv t a
neEmpty = []

neFromList :: [(GName t, a)] -> NameEnv t a
neFromList x = x

neKeys :: NameEnv t a -> [GName t]
neKeys = map fst

neExtendMany :: [(GName t, a)] -> NameEnv t a -> NameEnv t a
neExtendMany bnds orig = bnds ++ orig
