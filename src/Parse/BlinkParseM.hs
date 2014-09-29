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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
-- | The blink parser monad
module BlinkParseM (
    -- * The Blink parser monad
    BlinkParser
  , BlinkParseState
  , BlinkParseM(runParseM)
  , ParseCompEnv
    -- * Derived operators
  , debugParse
  , extendParseEnv
  , getParseEnv
  , withPos
  , bindExtend
    -- * Generic auxiliary parsers
  , xPrefix
  , lInfix
  , sepsBy
    -- * Generic utils
  , uncurry4
  , (.:)
  , (..:)
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Text.Parsec
import Text.Parsec.Expr

import AstExpr (Name, Ty)
import AstComp (CallArg, CTy0)

{-------------------------------------------------------------------------------
  The Blink parser monad

  1. reads strings
  2. maintains a BlinkParseState (unit for now)
  3. runs in the IO monad (for debugging)
-------------------------------------------------------------------------------}

type BlinkParseState = ()

-- We need environment of defined functions to intelligently parse applications
type ParseCompEnv     = [(Name,[(Name, CallArg Ty CTy0)])]
newtype BlinkParseM a = BlinkParseM { runParseM :: ParseCompEnv -> IO a }
type BlinkParser a    = ParsecT String BlinkParseState BlinkParseM a

instance Functor BlinkParseM where
  fmap f (BlinkParseM x) = BlinkParseM $ \env -> fmap f (x env)

instance Applicative BlinkParseM where
  pure = BlinkParseM . const . return
  (BlinkParseM f) <*> (BlinkParseM x) = BlinkParseM $ \env -> f env <*> x env

instance Monad BlinkParseM where
  (>>=) (BlinkParseM f) g
     = BlinkParseM (\env ->
         do { r <- f env; runParseM (g r) env })
  return = pure

instance MonadIO BlinkParseM where
  liftIO comp = BlinkParseM (\_ -> comp)

instance MonadReader ParseCompEnv BlinkParseM where
  ask = BlinkParseM (\env -> return env)
  local upd (BlinkParseM f) = BlinkParseM (\env -> f (upd env))
  reader f = BlinkParseM (\env -> return (f env))

{-------------------------------------------------------------------------------
  Derived operators
-------------------------------------------------------------------------------}

debugParse :: SourcePos -> IO () -> BlinkParser ()
debugParse p action
 = liftIO $ do { putStrLn $ "Debug"
               ; putStrLn $ "Position:" ++ show p
               ; action }

extendParseEnv :: ParseCompEnv -> BlinkParser a -> BlinkParser a
extendParseEnv penv action
  = local (\env -> penv ++ env) action

getParseEnv :: BlinkParser ParseCompEnv
getParseEnv = ask

withPos :: (Maybe SourcePos -> () -> a) -> BlinkParser a
withPos constr = constr' <$> getPosition
  where
    constr' p = constr (Just p) ()

bindExtend :: BlinkParser (ParseCompEnv, a) -> (a -> BlinkParser b) -> BlinkParser b
bindExtend x f = x >>= \(env, a) -> extendParseEnv env $ f a

infixl 1 `bindExtend`  -- Same precedence as (>>=)

{-------------------------------------------------------------------------------
  Generic auxiliary parsers
-------------------------------------------------------------------------------}

-- | Like parsec's `Prefix` but allow repeated application of the operator.
--
-- See http://stackoverflow.com/a/10475767/742991.
xPrefix :: Stream s m t => ParsecT s u m (a -> a) -> Operator s u m a
xPrefix op = Prefix . chainl1 op $ return (.)

-- | Left-associative `Infix`
lInfix :: ParsecT s u m (a -> a -> a) -> Operator s u m a
lInfix op = Infix op AssocLeft

-- | Variant on `sepBy` that takes a list of parsers rather than repeating one
sepsBy :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m sep -> ParsecT s u m [a]
sepsBy []     _  = return []
sepsBy [p]    _  = (:[]) <$> p
sepsBy (p:ps) op = (:) <$> p <* op <*> sepsBy ps op

{-------------------------------------------------------------------------------
  Generic auxiliary

  TODO: This should probably move to a seperate Utils module.
-------------------------------------------------------------------------------}

uncurry4 :: (a -> b -> c -> d -> z) -> (a, b, c, d) -> z
uncurry4 fun (a, b, c, d) = fun a b c d

(.:) :: (y -> z) -> (a -> b -> y) -> a -> b -> z
(.:) fun' fun a b = fun' (fun a b)

(..:) :: (y -> z) -> (a -> b -> c -> y) -> a -> b -> c -> z
(..:) fun' fun a b c = fun' (fun a b c)
