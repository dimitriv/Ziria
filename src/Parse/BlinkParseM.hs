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
  , ZiriaStream
  , mkZiriaStream
  , unconsZiriaStream
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
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Trans
import Control.Monad.Reader
import Text.Parsec
import Text.Parsec.Expr

import AstExpr (GName, SrcTy)
import AstComp (CallArg, SrcCTy)

import ZiriaLexer
import ZiriaLexerMonad

{-------------------------------------------------------------------------------
  Wrapping the lexer state
-------------------------------------------------------------------------------}

-- | ZiriaStream wraps the lexer state so that we memoize the next token
data ZiriaStream = ZiriaStream (Lexeme, ZiriaStream)

mkZiriaStream :: String -> ZiriaStream
mkZiriaStream str = ZiriaStream (StartOfFile, go (mkAlexState str))
  where
    go :: AlexState -> ZiriaStream
    go st = ZiriaStream (second go $ scan st)

unconsZiriaStream :: ZiriaStream -> Maybe (Lexeme, ZiriaStream)
unconsZiriaStream (ZiriaStream (tok, st')) =
    case tok of
      L _ LEOF _ -> Nothing
      _          -> Just (tok, st')

instance Monad m => Stream ZiriaStream m Lexeme where
  uncons = return . unconsZiriaStream

{-------------------------------------------------------------------------------
  The Blink parser monad

  1. reads strings
  2. maintains a BlinkParseState (unit for now)
  3. runs in the IO monad (for debugging)
-------------------------------------------------------------------------------}

type BlinkParseState = ()

-- | The parser environment
--
-- We need environment of defined functions to intelligently parse
-- applications: we map (comp) function names to their list of arguments. The
-- parser uses this to select the expr parser or the comp parser, depending on
-- the type of the argument.
type ParseCompEnv = [( GName SrcCTy, [GName (CallArg SrcTy SrcCTy)])]

-- | The parser monad
newtype BlinkParseM a = BlinkParseM { runParseM :: ParseCompEnv -> IO a }

type BlinkParser a    = ParsecT ZiriaStream BlinkParseState BlinkParseM a

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

withPos :: (Maybe SourcePos -> a) -> BlinkParser a
withPos constr = constr . Just <$> getPosition

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
sepsBy ps sep = choice [sepsBy1 ps sep, return []]

-- | Variant on `sepBy1` that takes a list of parsers rather than repeating one
sepsBy1 :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m sep -> ParsecT s u m [a]
sepsBy1 []     _   = return []
sepsBy1 (p:ps) sep = (:) <$> p <*> manys (map (sep >>) ps)

-- | Variant on `many` that takes a list of parsers rather than repeating one
manys :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m [a]
manys []     = return []
manys (p:ps) = choice [(:) <$> p <*> manys ps, return []]
