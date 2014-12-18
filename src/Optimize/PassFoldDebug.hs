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
-- | Debugging support for PassFold
--
-- This is a separate module because of TH stage restrictions.
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module PassFoldDebug (
    step
  ) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Outputable

step :: QuasiQuoter
step = QuasiQuoter {
      quoteExp  = aux [] . words
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
  where
    aux :: [Q Exp] -> [String] -> Q Exp
    aux acc [] = [| unwords $(listE (reverse acc)) |]
    aux acc (x:xs) = do
      inScope <- simpleInScope x
      case inScope of
        Just nm -> aux ([| pretty $(varE nm) |] : acc) xs
        Nothing -> aux ([| x |] : acc) xs

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Check if value is in scope and is not a function
simpleInScope :: String -> Q (Maybe Name)
simpleInScope x = do
    mName <- lookupValueName x
    case mName of
      -- If `reify` cannot find the type, we assume it's a local definition
      -- and hope it's simple :)
      Just name -> do simple <- recover (return True) (isSimple `liftM` reify name)
                      if simple then return (Just name)
                                else return Nothing
      Nothing   -> return Nothing

isSimple :: Info -> Bool
isSimple (VarI _ tp _ _) = isSimpleType tp
isSimple _ = False

isSimpleType :: Type -> Bool
isSimpleType ArrowT           = False
isSimpleType (AppT f a)       = isSimpleType f && isSimpleType a
isSimpleType (ForallT _ _ tp) = isSimpleType tp
isSimpleType _                = True
