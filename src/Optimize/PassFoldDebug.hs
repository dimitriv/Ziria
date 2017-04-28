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
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module PassFoldDebug (
    step
  ) where

import Control.Monad (liftM)
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (groupBy)
import Language.Haskell.TH hiding (ppr)
import Language.Haskell.TH.Quote
import Text.PrettyPrint.HughesPJ (text, hcat)

import Outputable

step :: QuasiQuoter
step = QuasiQuoter {
      quoteExp  = aux [] . split
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
  where
    aux :: [Q Exp] -> [String] -> Q Exp
    aux acc [] = [| show (hcat $(listE (reverse acc))) |]
    aux acc (x:xs) = do
      inScope <- simpleInScope x
      case inScope of
        -- can use PrettyShow.dumpDoc instead of ppr to see the actual AST
        Just nm -> aux ([| ppr $(varE nm) |] : acc) xs
        Nothing -> aux ([| text x |] : acc) xs

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Kind of like `words`, but given @"(x)"@, it will return
--
-- > ["(", "x", ")"]
--
-- (so it doesn't just split at whitespace)
split :: String -> [String]
split = groupBy ((==) `on` isIdent)
  where
    isIdent c = isAlphaNum c || c == '\'' || c == '_'

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
#if MIN_VERSION_template_haskell(2,11,0)
isSimple (VarI _ tp _) = isSimpleType tp
#else /* !MIN_VERSION_template_haskell(2,11,0) */
isSimple (VarI _ tp _ _) = isSimpleType tp
#endif /* !MIN_VERSION_template_haskell(2,11,0) */
isSimple _ = False

isSimpleType :: Type -> Bool
isSimpleType ArrowT           = False
isSimpleType (AppT f a)       = isSimpleType f && isSimpleType a
isSimpleType (ForallT _ _ tp) = isSimpleType tp
isSimpleType _                = True
