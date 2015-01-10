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
-- | Interpreter
--
-- The most obvious function of an interpreter is to reduce a (well-typed)
-- expression to a value. This function is served by
--
-- > evalFull :: Exp -> (Either String Value, Prints)
--
-- (`Value` is a generalization of `Val` which supports scalars as well as
-- structs and arrays with values as elements.)  We also provide some
-- specialized variants on `evalFull`:
--
-- > evalInt     :: Exp    -> (Either String Integer, Prints)
-- > evalBool    :: Exp    -> (Either String Bool, Prints)
-- > evalSrcInt  :: SrcExp -> (Either String Integer, Prints)
-- > evalSrcBool :: SrcExp -> (Either String Bool, Prints)
--
-- (The latter two are implemented by renaming and type checking the source
-- expression first.)
--
-- The question arises however what the interpreter should do when it encouters
-- free variables; `evalFull` will simply throw an error. However, other
-- choices are also possible.  Instead of reducing the expression to a value,
-- we can try to reduce the expression as much as possible instead:
--
-- > evalPartial :: Exp -> (Either String Exp, Prints)
--
-- For instance, @0 + (a + 2 * 3)@ for some free variable a will be reduced to
-- @a + 6@. In the implementation we regard partial evaluation as a different
-- mode to full evaluation, mostly for efficiency's sake: we could instead just
-- reduce the expression as much as possible and then see if the final
-- expression is in fact a value; however, by providing full evaluation as a
-- separate mode we can give up on evaluation as soon as we see the first free
-- variable.
--
-- Finally, we can also do non-deterministic evaluation. For example, we could
-- evaluate
--
-- > if a == 0 then 1 else 2
--
-- to the _list_ @[1, 2]@. This is done by
--
-- > evalNonDet :: Exp -> [Exp]
--
-- and is used for satisfiability checking; this particular approach is
-- described in <http://www.well-typed.com/blog/2014/12/simple-smt-solver/>,
-- but we are a little smarter than that blog post describes in how we
-- make guesses for integer valued variables by recording, where we know,
-- the "integer domain" for each integer valued expression. This is just a
-- heuristic, but it will catch a lot of simple cases.
--
-- NOTE: PARTIAL EVALUATION
--
-- We do not do symbolic execution. Suppose that `x` is a locally bound
-- mutuable variable, and `a` is some free variable. When we see an assignment
--
-- > x := a
--
-- we do not record that `x` has value `a` (for some expression `a`); if we
-- _did_ do this we could evaluate something like
--
-- > x := 0;
-- > for i in [0,3] { x := x + a }
--
-- to
--
-- > a + a + a
--
-- which is cute (and occassionally useful), but this gets hard real quick: if
-- we subsequently see an assignment to `a` we can no longer the expression `a`
-- to refer to the _old_ value of `a`, and if we no longer record the
-- assignment to the local variable `x` we also have no other way to refer to
-- this old value.  We could solve this by using something like SSA form, but
-- for now we just don't do symbolic evaluation at all.
{-# OPTIONS_GHC -Wall -Wwarn -funbox-strict-fields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Interpreter (
    -- * Values
    Complex(..)
  , Value(..)
  , expValue
  , valueExp
    -- * Main entry points
  , Prints
  , evalFull
  , evalPartial
  , evalNonDet
    -- ** Specializations of evalFull
  , evalInt
  , evalBool
    -- ** Specializations for source expressions
  , evalSrcInt
  , evalSrcBool
    -- ** Satisfiability
  , provable
  , implies
    -- * Convenience
  , eNot
  , eOr
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits hiding (bit)
import Data.Functor.Identity
import Data.Int
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Outputable
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec.Pos (SourcePos)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import AstExpr
import AstUnlabelled
import CtExpr (ctExp)
import GenSym (initGenSym)
import SparseArray (SparseArray)
import TcMonad (runTcM')
import Typecheck (tyCheckExpr)
import qualified Lens        as L
import qualified SparseArray as SA

{-------------------------------------------------------------------------------
  Complex numbers

  We don't reuse Haskell's infrastructure because Ziria's complex numbers are a
  bit different. Although we give the usual type class instances so that this
  integrates nicely with our implementation for binary operators, we only
  implement a handful of operations for now; this closely follows the
  definition on cgBinOp in CgExpr.hs and the corresponding definitions in
  numerics.c.
-------------------------------------------------------------------------------}

data Complex a = Complex { re :: !a, im :: !a }

instance Functor Complex where
  fmap f Complex{..} = Complex { re = f re, im = f im }

instance Eq (Complex a) where
  (==) = error "(==) for Complex not implemented"

instance Ord (Complex a) where
  (<=) = error "(<=) for Complex not implemented"

instance Enum (Complex a) where
  toEnum   = error "toEnum for Complex not implemented"
  fromEnum = error "fromEnum for Complex not implemented"

instance Num a => Num (Complex a) where
  x + y = Complex { re = re x + re y
                  , im = im x + im y
                  }
  x - y = Complex { re = re x - re y
                  , im = im x - im y
                  }
  x * y = Complex { re = (re x) * (re y) - (im x) * (im y)
                  , im = (im x) * (re y) + (re x) * (im y)
                  }

  abs         = error "abs for Complex not implemented"
  signum      = error "signum for Complex not implemented"
  fromInteger = error "fromInteger for Complex not implemented"

instance Num a => Real (Complex a) where
  toRational = error "toRational for Complex not implemented"

instance Integral a => Integral (Complex a) where
  x `quot` y = Complex { re = (a*c + b*d) `quot` (c*c + d*d)
                       , im = (b*c - a*d) `quot` (c*c + d*d)
                       }
    where
      a = re x
      b = im x
      c = re y
      d = im y

  quotRem   = error "quotRem for Complex not implemented"
  toInteger = error "toInteger for Complex not implemented"

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Values
--
-- Generalization of values that allows for arrays and structs containing
-- values as well as scalars. Moreover, we represent integers by their actual
-- fixed bitwidth representation rather than the arbitrary precision integers
-- used by `Val`.
--
-- TODO: We should use a strict hashmap for structs; if we don't, we may leak
-- memory.
data Value0 =
    ValueBit    !Bool
  | ValueInt8   !Int8
  | ValueInt16  !Int16
  | ValueInt32  !Int32
  | ValueInt64  !Int64
  | ValueCpx8   !(Complex Int8)
  | ValueCpx16  !(Complex Int16)
  | ValueCpx32  !(Complex Int32)
  | ValueCpx64  !(Complex Int64)
  | ValueDouble !Double
  | ValueBool   !Bool
  | ValueString !String
  | ValueUnit
  | ValueArray  !(SparseArray Value)
  | ValueStruct Ty [(FldName, Value)]

data Value = MkValue {
     unValue  :: !Value0
   , valueLoc :: !(Maybe SourcePos)
   }

instance Show Value where
  show = show . valueExp

scalarValue :: Ty -> Val -> Value0
scalarValue TBit        (VBit b)    = ValueBit    b
scalarValue (TInt BW8)  (VInt i)    = ValueInt8   (fromInteger i)
scalarValue (TInt BW16) (VInt i)    = ValueInt16  (fromInteger i)
scalarValue (TInt BW32) (VInt i)    = ValueInt32  (fromInteger i)
scalarValue (TInt BW64) (VInt i)    = ValueInt64  (fromInteger i)
scalarValue TDouble     (VDouble d) = ValueDouble d
scalarValue TBool       (VBool b)   = ValueBool   b
scalarValue TString     (VString s) = ValueString s
scalarValue TUnit       VUnit       = ValueUnit
scalarValue _           _           = error "invalid scalar value"

structValue :: Ty -> [(FldName, Value)] -> Value0
structValue ty flds =
    case ty of
      TStruct "complex8" _ ->
        let [("re", ValueInt8 re), ("im", ValueInt8 im)] = flds'
        in ValueCpx8 Complex{..}
      TStruct "complex16" _ ->
        let [("re", ValueInt16 re), ("im", ValueInt16 im)] = flds'
        in ValueCpx16 Complex{..}
      TStruct "complex32" _ ->
        let [("re", ValueInt32 re), ("im", ValueInt32 im)] = flds'
        in ValueCpx32 Complex{..}
      TStruct "complex64" _ ->
        let [("re", ValueInt64 re), ("im", ValueInt64 im)] = flds'
        in ValueCpx64 Complex{..}
      _otherwise ->
        ValueStruct ty flds
  where
    flds' = map (second unValue) flds

expValue :: Exp -> Maybe Value
expValue e = (\v0 -> MkValue v0 (expLoc e)) <$> go (unExp e)
  where
    go :: Exp0 -> Maybe Value0
    go (EVal ty val)         = return $ scalarValue ty val
    go (EValArr elems@(x:_)) = do let def = initScalar (expLoc e) $ ctExp x
                                  mkArray def <$> mapM expValue elems
    go (EStruct ty flds)     = structValue ty <$> mapM (second' expValue) flds
    go _                     = Nothing

    mkArray def = ValueArray . SA.newListArray def

valueExp :: Value -> Exp
valueExp v = MkExp (go (unValue v)) vloc ()
  where
    go :: Value0 -> Exp0
    go (ValueBit    b)         = EVal TBit        (VBit b)
    go (ValueInt8   i)         = EVal (TInt BW8)  (VInt (toInteger i))
    go (ValueInt16  i)         = EVal (TInt BW16) (VInt (toInteger i))
    go (ValueInt32  i)         = EVal (TInt BW32) (VInt (toInteger i))
    go (ValueInt64  i)         = EVal (TInt BW64) (VInt (toInteger i))
    go (ValueDouble d)         = EVal TDouble     (VDouble d)
    go (ValueBool   b)         = EVal TBool       (VBool   b)
    go (ValueString s)         = EVal TString     (VString s)
    go ValueUnit               = EVal TUnit       VUnit
    go (ValueArray elems)      = EValArr (map valueExp (SA.getElems elems))
    go (ValueStruct ty flds)   = EStruct ty (map (second valueExp) flds)

    go (ValueCpx8 Complex{..}) =
      EStruct tcomplex8 [
          ("re", eVal vloc tint8 (VInt (toInteger re)))
        , ("im", eVal vloc tint8 (VInt (toInteger im)))
        ]
    go (ValueCpx16 Complex{..}) =
      EStruct tcomplex16 [
          ("re", eVal vloc tint16 (VInt (toInteger re)))
        , ("im", eVal vloc tint16 (VInt (toInteger im)))
        ]
    go (ValueCpx32 Complex{..}) =
      EStruct tcomplex32 [
          ("re", eVal vloc tint32 (VInt (toInteger re)))
        , ("im", eVal vloc tint32 (VInt (toInteger im)))
        ]
    go (ValueCpx64 Complex{..}) =
      EStruct tcomplex64 [
         ("re", eVal vloc tint64 (VInt (toInteger re)))
       , ("im", eVal vloc tint64 (VInt (toInteger im)))
       ]

    vloc = valueLoc v

-- | The runtime currently leaves the initial value for unassigned variables
-- unspecified (https://github.com/dimitriv/Ziria/issues/79). This means that
-- we are free to specify whatever we wish in the interpret -- here we pick
-- sensible defaults.
initVal :: Maybe SourcePos -> Ty -> Maybe Value
initVal p ty = (\v0 -> MkValue v0 p) <$> go ty
  where
    go TBit        = return $ ValueBit    False
    go (TInt BW8)  = return $ ValueInt8   0
    go (TInt BW16) = return $ ValueInt16  0
    go (TInt BW32) = return $ ValueInt32  0
    go (TInt BW64) = return $ ValueInt64  0
    go TDouble     = return $ ValueDouble 0
    go TBool       = return $ ValueBool   False
    go TString     = return $ ValueString ""
    go TUnit       = return $ ValueUnit
    go (TArray (Literal n) ty') = ValueArray . SA.newArray n <$> initVal p ty'
    go (TStruct _ flds)         = ValueStruct ty <$> mapM initFld flds
    go _                        = Nothing

    initFld :: (FldName, Ty) -> Maybe (FldName, Value)
    initFld = second' (initVal p)

-- | Specialization of `initVal` for scalars
--
-- If we are sure that the type must be a scalar type we are justified in
-- stripping of the `Maybe`
initScalar :: Maybe SourcePos -> Ty -> Value
initScalar p = fromJust . initVal p

vTrue :: Maybe SourcePos -> Value
vTrue = MkValue (ValueBool True)

vFalse :: Maybe SourcePos -> Value
vFalse = MkValue (ValueBool False)

{-------------------------------------------------------------------------------
  Main entry points
-------------------------------------------------------------------------------}

-- | Any prints of statically known values we execute during interpretation
type Prints = [(Bool, Value)]

-- | (Full) evaluation of an expression
evalFull :: Exp -> (Either String Value, Prints)
evalFull e = aux . runIdentity $ evalEval (interpret e) EvalFull initState
  where
    aux (Right (EvaldFull v), prints) = (Right v,  prints)
    aux (Right (EvaldPart _), _)      = error "the impossible happened"
    aux (Left  err,           prints) = (Left err, prints)

-- | (Partial) evaluation of an expression
evalPartial :: Exp -> (Either String Exp, Prints)
evalPartial e = aux . runIdentity $ evalEval (interpret e) EvalPartial initState
  where
    aux (Right e', prints) = (Right (unEvald e'), prints)
    aux (Left err, prints) = (Left err, prints)

-- | (Full) evaluation of expressions, guessing values for boolean expressions
evalNonDet :: Exp -> [Exp]
evalNonDet e =
    [ unEvald e'
    | (Right e', _prints) <- evalEval (interpret e) EvalNonDet initState
    ]

{-------------------------------------------------------------------------------
  Specializations of `evalFull`

  NOTE: Evaluation of source expressions

  The evaluator needs type information to work. When we are evaluating source
  expressions, it's easier to type check the expression and then evaluate it,
  rather than parametrize the type checker so that it can work with source
  expressions directly.

  The type checker runs in the I/O monad to generate symbols. However, if we
  are evaluating a standalone source expression to an integer, then the
  evaluation is independent of the symbols we pick. Hence, it is sound to use
  `unsafePerformIO` here.
-------------------------------------------------------------------------------}

-- | Evaluate an expression to an integer
evalInt :: Exp -> (Either String Integer, Prints)
evalInt e = case evalFull e of
    (Right (MkValue (ValueInt8  i) _) , prints) -> (Right (toInteger i)   , prints)
    (Right (MkValue (ValueInt16 i) _) , prints) -> (Right (toInteger i)   , prints)
    (Right (MkValue (ValueInt32 i) _) , prints) -> (Right (toInteger i)   , prints)
    (Right (MkValue (ValueInt64 i) _) , prints) -> (Right (toInteger i)   , prints)
    (Right _                          , prints) -> (Left "Not an integer" , prints)
    (Left  err                        , prints) -> (Left err              , prints)

-- | Evaluate an expression to a boolean
evalBool :: Exp -> (Either String Bool, Prints)
evalBool e = case evalFull e of
    (Right (MkValue (ValueBool b) _) , prints) -> (Right b               , prints)
    (Right _                         , prints) -> (Left "Not an boolean" , prints)
    (Left  err                       , prints) -> (Left err              , prints)

-- | Evaluate a source expression to an integer
evalSrcInt :: SrcExp -> (Either String Integer, Prints)
evalSrcInt e = unsafePerformIO $ do
    sym <- initGenSym "evalSrcInt"
    me' <- runTcM' (tyCheckExpr e) sym
    case me' of
      Left  err     -> return (Left ("Type error: " ++ show err), [])
      Right (e', _) -> return $ evalInt e'

-- | Evaluate a source expression to a boolean
evalSrcBool :: SrcExp -> (Either String Bool, Prints)
evalSrcBool e = unsafePerformIO $ do
    sym <- initGenSym "evalSrcBool"
    me' <- runTcM' (tyCheckExpr e) sym
    case me' of
      Left  err     -> return (Left ("Type error: " ++ show err), [])
      Right (e', _) -> return $ evalBool e'

{-------------------------------------------------------------------------------
  Satisfiability
-------------------------------------------------------------------------------}

-- | Satisfiability check for expressions of type Bool
--
-- NOTE: This only makes sense for expressions of type Bool and should not be
-- called on other expressions.
satisfiable :: Exp -> Bool
satisfiable = any isTrue . evalNonDet
  where
    isTrue :: Exp -> Bool
    isTrue e | EVal TBool (VBool True) <- unExp e = True
    isTrue _                                      = False

-- | Provability of boolean expressions
--
-- NOTE: This only makes sense for expressions of type Bool and should not be
-- called on other expressions.
provable :: Exp -> Bool
provable = not . satisfiable . eNot

-- | Does one expression imply another?
--
-- NOTE: This only makes sense for expressions of type Bool and should not be
-- called on other expressions.
implies :: Exp -> Exp -> Bool
implies a b = provable (eNot a `eOr` b)

{-------------------------------------------------------------------------------
  Interpreter monad and basic infrastructure
-------------------------------------------------------------------------------}

-- | Interpreter mode
data EvalMode m where
    -- | Partial evaluation
    --
    -- Free variables will be left unchanged, where possible.
    --
    -- > a + 2 * 3 ~~> a + 6
    EvalPartial :: EvalMode Identity

    -- | Full evaluation
    --
    -- Free variables in the expression will result in an error.
    EvalFull :: EvalMode Identity

    -- | Approximation
    --
    -- For use in satisfiability checking.
    --
    -- > if a then 1 else 2 ~~> [1, 2]
    EvalNonDet :: EvalMode []

-- | Evaluation state
data EvalState = EvalState {
    -- | Let-bound variables (immutable)
    _evalLets :: !(Map String Value)

    -- | Letref-bound variables (mutable)
  , _evalLetRefs :: !(Map String Value)

    -- | Guesses about boolean-valued expressions
    --
    -- Used in `EvalNonDet` mode only.
  , _evalGuessesBool :: !(Map Exp Bool)

    -- | Guesses about integer-valued expressions
    --
    -- Used in `EvalNonDet` mode only.
  , _evalGuessesInt :: !(Map Exp IntDomain)

    -- | Print statements executed by the program
  , _evalPrints :: Prints
  }
  deriving Show

evalLets :: L.Lens EvalState (Map String Value)
evalLets f st = (\x -> st { _evalLets = x }) <$> f (_evalLets st)

evalLetRefs :: L.Lens EvalState (Map String Value)
evalLetRefs f st = (\x -> st { _evalLetRefs = x }) <$> f (_evalLetRefs st)

evalGuessesBool :: L.Lens EvalState (Map Exp Bool)
evalGuessesBool f st = (\x -> st { _evalGuessesBool = x}) <$> f (_evalGuessesBool st)

evalGuessesInt :: L.Lens EvalState (Map Exp IntDomain)
evalGuessesInt f st = (\x -> st { _evalGuessesInt = x}) <$> f (_evalGuessesInt st)

evalPrints :: L.Lens EvalState Prints
evalPrints f st = (\x -> st { _evalPrints = x }) <$> f (_evalPrints st)

initState :: EvalState
initState = EvalState {
    _evalLets        = Map.empty
  , _evalLetRefs     = Map.empty
  , _evalGuessesBool = Map.empty
  , _evalGuessesInt  = Map.empty
  , _evalPrints      = []
  }

-- | The evaluator monad
--
-- The evaluator monad keeps track of a lot of things:
--
-- 1. ErrorT: Runtime errors such as out-of-bound array indices
-- 2. R:  Interpreter mode (full, partial, non-deterministic)
-- 3. W:  Locally bound vars and writes to locally bound vars, and guesses for
--        non-deterministic evaluation, as well as any debug prints executed
--        by the program
-- 4. []: Non-determinism arising from guessing values
--
-- NOTE: Morally we should use a WriterT monad for the print statements.
-- However, WriterT (and, by extension, RWS) has unacceptable memory behaviour:
-- even if no logging takes place, it is building up large chunks of
--
-- > [] `mappend` [] `mappend` ...
--
-- which do not get evaluated until the very end, hence causing a memory leak.
newtype Eval m a = Eval {
      unEval :: ErrorT String (ReaderT (EvalMode m) (StateT EvalState m)) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError String
           , MonadState EvalState
           )

-- | We want the `MonadPlus` arising from the underlying list monad, not
-- from the top-level `ErrorT` monad.
instance MonadPlus (Eval []) where
  mzero       = mkEval $ \_mode _st -> []
  f `mplus` g = mkEval $ \mode st -> runEvald f mode st <|> runEvald g mode st

instance Alternative (Eval []) where
  empty = mzero
  (<|>) = mplus

runEvald :: Eval m a -> EvalMode m -> EvalState -> m (Either String a, EvalState)
runEvald act mode st = runStateT (runReaderT (runErrorT (unEval act)) mode) st

evalEval :: Monad m => Eval m a -> EvalMode m -> EvalState -> m (Either String a, Prints)
evalEval act mode st = second _evalPrints
               `liftM` runStateT (runReaderT (runErrorT (unEval act)) mode) st

mkEval :: (EvalMode m -> EvalState -> m (Either String a, EvalState)) -> Eval m a
mkEval f = Eval $ ErrorT $ ReaderT $ \mode -> StateT $ \st -> f mode st

getMode :: Monad m => Eval m (EvalMode m)
getMode = Eval ask

logPrint :: Monad m => Bool -> Value -> Eval m ()
logPrint newline val = L.modifySt evalPrints ((newline, val) :)

-- | Run the second action only if the first one results zero results
--
-- (Does NOT run the second action if the first one results an error).
onZero :: Eval [] a -> Eval [] a -> Eval [] a
onZero act handler = mkEval $ \mode st ->
  case runEvald act mode st of
    []     -> runEvald handler mode st
    result -> result

{-------------------------------------------------------------------------------
  Primitive operations in the evaluator monad
-------------------------------------------------------------------------------}

-- | Read a variable
--
-- (Either let-bound or letref bound)
readVar :: Monad m => GName Ty -> Eval m (Maybe Value)
readVar x = do
  mLetBound <- L.getSt $ evalLets . L.mapAt (uniqId x)
  case mLetBound of
    Just letBound -> return $ Just letBound
    Nothing       -> L.getSt $ evalLetRefs . L.mapAt (uniqId x)

extendScope :: Monad m
            => L.Lens EvalState (Map String Value)  -- ^ Scope to extend
            -> GName Ty                             -- ^ Variable to introduce
            -> Value                                -- ^ Initial value
            -> Eval m a -> Eval m a
extendScope scope x v act = do
    -- Check if variable already in scope (if this happens it's a compiler bug)
    mCurrentValue <- readVar x
    case mCurrentValue of
      Nothing -> return ()
      Just _  -> throwError $ "Variable " ++ pretty x ++ " already in scope"

    L.modifySt scope $ Map.insert (uniqId x) v
    a <- act
    L.modifySt scope $ Map.delete (uniqId x)
    return a

-- | Write a variable
--
-- The caller should verify that the variable is in scope.
writeVar :: Monad m => GName Ty -> Value -> Eval m ()
writeVar x v = L.modifySt evalLetRefs $ Map.insert (uniqId x) v

-- | Indicate that we could only partially evaluate an AST constructor
--
-- We use this whenever we were not able to eliminate an AST constructor. In
-- partial evaluation, this is just `return`; during full evaluation this is
-- an error, and during non-deterministic evaluation this results in an
-- empty list of results.
evaldPart :: Monad m => Exp -> Eval m Evald
evaldPart x = do
  mode <- getMode
  case mode of
    EvalPartial -> return $ EvaldPart x
    EvalFull    -> throwError "Free variables"
    EvalNonDet  -> mzero

-- | Indicate that we managed to fully evaluate an expression
evaldFull :: Monad m => Value -> Eval m Evald
evaldFull = return . EvaldFull

-- | Run the guess algorithm if we could not fully evaluate the expression.
guessIfUnevaluated :: Monad m => (Exp -> Eval m Evald) -> (Exp -> Eval m Evald)
guessIfUnevaluated f e = do
  mode <- getMode
  case mode of EvalNonDet -> f e `onZero` guess e
               _          -> f e

-- | Invalidate all state
--
-- This is used when we skip an unknown statement; after that, we
-- conservatively assume that the value of all variables and all previously
-- guessed values (during non-deterministic evaluation) as now no longer
-- accurate.
--
-- Note that let-bound variables never need to be invalidated: once we know
-- that a let-bound variable evaluated to a specific value, future statements
-- can never invalidate that.
invalidateAssumptions :: Monad m => Eval m ()
invalidateAssumptions = do
    L.putSt evalLetRefs     Map.empty
    L.putSt evalGuessesBool Map.empty
    L.putSt evalGuessesInt  Map.empty

-- | Invalidate all assumptions about a particular variable
--
-- We conservatively remove all guesses. We could instead only remove those
-- guesses for expressions that mention the variable.
--
-- See also comments for `invalidateAssumptions`.
invalidateAssumptionsFor :: Monad m => GName Ty -> Eval m ()
invalidateAssumptionsFor x = do
    L.modifySt evalLetRefs $ Map.delete (uniqId x)
    L.putSt evalGuessesBool Map.empty
    L.putSt evalGuessesInt  Map.empty

{-------------------------------------------------------------------------------
  The result of interpretation is either a value or an expression. We try to
  avoid marshaling back and forth between these two types whenever possible.
-------------------------------------------------------------------------------}

data Evald = EvaldFull Value | EvaldPart Exp

unEvald :: Evald -> Exp
unEvald (EvaldFull v) = valueExp v
unEvald (EvaldPart e) = e

partitionEvalds :: [Evald] -> Either [Value] [Exp]
partitionEvalds = go []
  where
    -- Prefer to return all values, only returning all expressions if we have to
    go :: [Value] -> [Evald] -> Either [Value] [Exp]
    go acc []               = Left (reverse acc)
    go acc (EvaldFull v:es) = go (v:acc) es
    go acc (EvaldPart e:es) = Right $ reverse (map valueExp acc)
                                   ++ e : map unEvald es

evaldArray :: Evald -> Either (SparseArray Value) Exp
evaldArray (EvaldFull (MkValue (ValueArray arr) _)) = Left arr
evaldArray e = Right $ unEvald e

evaldStruct :: Evald -> Either (Either (Value, Value) [(FldName, Value)]) Exp
evaldStruct (EvaldFull (MkValue (ValueStruct _ flds) _)) = Left $ Right flds
evaldStruct (EvaldFull (MkValue (ValueCpx8 val) p))      = Left $ Left
    (MkValue (ValueInt8 (re val)) p, MkValue (ValueInt8 (im val)) p)
evaldStruct (EvaldFull (MkValue (ValueCpx16 val) p))     = Left $ Left
    (MkValue (ValueInt16 (re val)) p, MkValue (ValueInt16 (im val)) p)
evaldStruct (EvaldFull (MkValue (ValueCpx32 val) p))     = Left $ Left
    (MkValue (ValueInt32 (re val)) p, MkValue (ValueInt32 (im val)) p)
evaldStruct (EvaldFull (MkValue (ValueCpx64 val) p))     = Left $ Left
    (MkValue (ValueInt64 (re val)) p, MkValue (ValueInt64 (im val)) p)
evaldStruct e =
    Right $ unEvald e

evaldInt :: Num a => Evald -> Either a Exp
evaldInt (EvaldFull (MkValue (ValueInt8  i) _)) = Left (fromIntegral i)
evaldInt (EvaldFull (MkValue (ValueInt16 i) _)) = Left (fromIntegral i)
evaldInt (EvaldFull (MkValue (ValueInt32 i) _)) = Left (fromIntegral i)
evaldInt (EvaldFull (MkValue (ValueInt64 i) _)) = Left (fromIntegral i)
evaldInt e = Right $ unEvald e

evaldUnit :: Evald -> Bool
evaldUnit (EvaldFull (MkValue ValueUnit _)) = True
evaldUnit _ = False

evaldBool :: Evald -> Either Bool Exp
evaldBool (EvaldFull (MkValue (ValueBool b) _)) = Left b
evaldBool e = Right $ unEvald e

{-------------------------------------------------------------------------------
  The interpreter proper
-------------------------------------------------------------------------------}

-- | Interpreter for the expression language
interpret :: forall m. Monad m => Exp -> Eval m Evald
interpret e = guessIfUnevaluated (go . unExp) e
  where
    go :: Exp0 -> Eval m Evald

    -- Values

    go (EVal ty val) =
      evaldFull $ MkValue (scalarValue ty val) eloc

    -- Arrays

    go (EValArr elems) = do
      elemsEvald <- mapM interpret elems
      case partitionEvalds elemsEvald of
        Left elems' -> do
          let def = initScalar eloc $ ctExp (head elems)
          evaldFull $ MkValue (ValueArray (SA.newListArray def elems')) eloc
        Right elems' ->
          evaldPart $ eValArr eloc elems'
    go (EArrRead arr ix li) = do
      arrEvald <- interpret arr
      ixEvald  <- interpret ix
      case (evaldArray arrEvald, evaldInt ixEvald) of
        -- We only select elements from fully known arrays. See comment for
        -- EProj about optimizing element selection from known arrays with
        -- non-value elements
        (Left elems, Left i) ->
          case li of
            LISingleton ->
              if 0 <= i && i < SA.size elems
                then evaldFull $ SA.unsafeReadArray i elems
                else throwError "Out of bounds"
            LILength len ->
              if 0 <= i && i + len <= SA.size elems
                then do
                  let slice = ValueArray (SA.unsafeSlice i len elems)
                  evaldFull $ MkValue slice eloc
                else
                  throwError "Out of bounds"
            LIMeta _ ->
              error "Unexpected meta variable"
        -- "Full" slice (capturing the entire array)
        (Right arr', Left 0)
          | TArray (Literal m) _ <- ctExp arr'
          , LILength n           <- li
          , m == n -> evaldPart $ arr'
        _otherwise ->
          evaldPart $ eArrRead eloc (unEvald arrEvald) (unEvald ixEvald) li

    -- Array permutation

    go (EBPerm e1 e2) = do
      e1Evald <- interpret e1
      e2Evald <- interpret e2
      -- TODO: We should attempt to execute the permutation.
      evaldPart $ eBPerm eloc (unEvald e1Evald) (unEvald e2Evald)

    -- Structs

    go (EStruct ty fields) = do
      let (fieldNames, fieldDefs) = unzip fields
      fieldDefsEvald <- mapM interpret fieldDefs
      case partitionEvalds fieldDefsEvald of
        Left fieldDefs' -> do
          let struct = structValue ty (zip fieldNames fieldDefs')
          evaldFull $ MkValue struct eloc
        Right fieldDefs' ->
          evaldPart $ eStruct eloc ty (zip fieldNames fieldDefs')
    go (EProj struct fld) = do
      structEvald <- interpret struct
      -- We _could_ potentially also optimize projections out of known structs
      -- with non-value fields, but we have to be careful in that case that we
      -- do not lose side effects, and anyway that optimization is less useful.
      case (evaldStruct structEvald, fld) of
        (Left  (Left (re, _)), "re") -> evaldFull re
        (Left  (Left (_, im)), "im") -> evaldFull im
        (Left  (Left _      ), _   ) -> error "Invalid field of complex"
        (Left  (Right fields), _   ) -> evaldFull $ getField fld fields
        (Right struct'       , _   ) -> evaldPart $ eProj eloc struct' fld

    -- Simple operators

    go (EUnOp op a) = do
      aEvald <- interpret a
      applyUnOp eloc op aEvald
    go (EBinOp op a b) = do
      aEvald <- interpret a
      bEvald <- interpret b
      applyBinOp eloc op aEvald bEvald

    -- Special case for force-inlining
    --
    -- (This is necessary because if the expression has side effects we cannot
    -- evaluate it strictly when force-inlining is specified)

    go (ELet nm ForceInline e1 e2) =
      interpret $ substExp [] [(nm,e1)] e2

    -- Variables

    go (EVar x) = do
      mv <- readVar x
      case mv of
        Just v  -> evaldFull v
        Nothing -> evaldPart $ eVar eloc x
    go (ELet x fi e1 e2) = do
      e1Evald <- interpret e1
      case e1Evald of
        EvaldFull v1 ->
          extendScope evalLets x v1 $ interpret e2
        EvaldPart e1' -> do
          e2' <- interpret e2
          -- We could potentially remove the let binding here if e2' is fully
          -- evaluated (this can only happen if it does not actually use the
          -- let-bound variable) and e1' is side effect free.
          evaldPart $ eLet eloc x fi e1' (unEvald e2')
    go (ELetRef x (Just e1) e2) = do
      e1Evald <- interpret e1
      case e1Evald of
        EvaldFull v1' -> do
          interpretLetRef eloc x v1' e2
        EvaldPart e1' -> do
          e2Evald <- interpret e2
          -- See comments for `ELet`
          evaldPart $ eLetRef eloc x (Just e1') (unEvald e2Evald)
    go (ELetRef x Nothing e2) =
      case initVal eloc (nameTyp x) of
        Just v1' ->
          interpretLetRef eloc x v1' e2
        Nothing -> do
          -- We cannot construct a default value for this type
          -- (perhaps because it's a length-polymorphic array)
          e2' <- interpret e2
          -- See comments for `ELet`
          evaldPart $ eLetRef eloc x Nothing (unEvald e2')
    go (EAssign lhs rhs) = do
      -- TODO: Check that we want to evaluate the RHS before the LHS
      rhsEvald      <- interpret rhs
      (x, lhsEvald) <- interpretDerefExp lhs
      didAssign <- case (lhsEvald, rhsEvald) of
        (Left derefExp, EvaldFull rhs') -> assign derefExp rhs'
        _otherwise                      -> return False
      if didAssign
        then evaldFull $ MkValue ValueUnit eloc
        else do
          invalidateAssumptionsFor x
          evaldPart $ eAssign eloc (unDeref lhsEvald) (unEvald rhsEvald)
    go (EArrWrite arr ix len rhs) = do
      -- TODO: Ideally we should just call
      --
      -- > go (EAssign (eArrRead eloc arr ix len) rhs)
      --
      -- but this does not work because this would translate all EArrWrite
      -- nodes to EAssign nodes and this causes a change in semantics due to a
      -- bug in code generation (#88). Instead, we attempt to do the assignment,
      -- and if this fails we leave the node an an EArrWrite.
      rhsEvald      <- interpret rhs
      (x, lhsEvald) <- interpretDerefExp (eArrRead eloc arr ix len)
      didAssign <- case (lhsEvald, rhsEvald) of
        (Left derefExp, EvaldFull rhs') -> assign derefExp rhs'
        _otherwise                      -> return False
      if didAssign
        then evaldFull $ MkValue ValueUnit eloc
        else do
          invalidateAssumptionsFor x
          -- Strip off the top-level EArrRead again (not that we should not
          -- _re-evaluate_ the LHS because that may duplicate side effects)
          case unExp (unDeref lhsEvald) of
            EArrRead arr' ix' len' ->
              -- Special optimizations
              case (ctExp arr, unExp ix', len') of
                (TArray (Literal m) _, EVal _ (VInt 0), LILength n) | m == n ->
                  evaldPart $ eAssign eloc arr' (unEvald rhsEvald)
                _otherwise ->
                  evaldPart $ eArrWrite eloc arr' ix' len' (unEvald rhsEvald)
            _otherwise ->
              error "the impossible happened"

    -- Control flow

    go (ESeq e1 e2) = do
      e1Evald <- interpret e1
      e2Evald <- interpret e2
      if evaldUnit e1Evald
        then return e2Evald
        else -- We could omit e1 completely if we manage to reduce e2 to a value
             -- independent of e1, and e1 is side effect free.
             evaldPart $ eSeq eloc (unEvald e1Evald) (unEvald e2Evald)
    go (EIf cond iftrue iffalse) = do
      condEvald <- interpret cond
      case evaldBool condEvald of
        Left b ->
          interpret (if b then iftrue else iffalse)
        Right cond' -> do
          -- We don't know which of the two branches will be executed, so we
          -- cannot execute any of its effects. Therefore we invalidate
          -- assumptions, so that any variables occurring in the branches will
          -- be considered free and cannot be assigned to.
          invalidateAssumptions
          iftrue'  <- interpret iftrue
          iffalse' <- interpret iffalse
          evaldPart $ eIf eloc cond' (unEvald iftrue') (unEvald iffalse')
    go (EFor ui x start len body) = do
      startEvald <- interpret start
      lenEvald   <- interpret len
      fullEv <- case (evaldInt startEvald, evaldInt lenEvald) of
        (Left start', Left len') -> do
          let loop n | n == start' + len' =
                return True
              loop n = do
                let n' = fromJust $ expValue (eVal eloc (nameTyp x) (VInt n))
                bodyEvald <- extendScope evalLets x n' $ interpret body
                -- Only when we can fully evaluate the body of the loop do we
                -- continue. If not, we give up completely (alternatively, we
                -- could choose to do loop unrolling here).
                if evaldUnit bodyEvald then loop (n + 1)
                                       else return False

          loop start'
        _otherwise -> -- Start or length not fully evaluated
          return False
      if fullEv
        then evaldFull $ MkValue ValueUnit eloc
        else do -- See comments for conditionals
                invalidateAssumptions
                bodyEvald <- interpret body
                evaldPart $ eFor eloc ui x (unEvald startEvald)
                                           (unEvald lenEvald)
                                           (unEvald bodyEvald)
    go (EWhile cond body) = do
      let loop = do
            condEvald <- interpret cond
            case evaldBool condEvald of
              Left False ->
                return True
              Left True -> do
                bodyEvald <- interpret body
                -- See comments for `EFor`
                if evaldUnit bodyEvald then loop
                                       else return False
              _otherwise -> -- Condition not fully evaluated
                return False
      fullEv <- loop
      if fullEv then evaldFull $ MkValue ValueUnit eloc
                else do -- See comments for conditionals
                        invalidateAssumptions
                        condEvald <- interpret cond
                        bodyEvald <- interpret body
                        evaldPart $ eWhile eloc (unEvald condEvald)
                                                (unEvald bodyEvald)
    go (EIter _ _ _ _) =
      throwError "EIter unsupported"

    -- Functions

    go (ECall fn args) = do
      argsEvald <- mapM interpret args
      -- We don't know the state of mutable variables after a function call
      invalidateAssumptions
      evaldPart $ eCall eloc fn (map unEvald argsEvald)

    -- Misc

    go (EPrint newline e1) = do
      e1Evald <- interpret e1
      case e1Evald of
        EvaldFull v1' -> do logPrint newline v1'
                            evaldFull $ MkValue ValueUnit eloc
        EvaldPart e1' -> evaldPart $ ePrint eloc newline e1'
    go (EError ty str) =
      evaldPart $ eError eloc ty str
    go (ELUT _ _) =
      throwError "Unexpected LUT during interpretation"

    eloc :: Maybe SourcePos
    eloc = expLoc e

interpretLetRef :: Monad m
                => Maybe SourcePos -> GName Ty -> Value -> Exp -> Eval m Evald
interpretLetRef eloc x v1 e2 = do
    (e2Evald, xDeleted) <- extendScope evalLetRefs x v1 $ do
      e2Evald  <- interpret e2
      xDeleted <- isNothing `liftM` readVar x
      return (e2Evald, xDeleted)
    -- If at any point x was (or might have been) assigned a not
    -- statically known value, we cannot remove the binding site.
    if xDeleted
      then evaldPart $ eLetRef eloc x (Just (valueExp v1)) (unEvald e2Evald)
      else return e2Evald

-- | Smart constructor for binary operators
applyBinOp :: Monad m
           => Maybe SourcePos -> BinOp -> Evald -> Evald -> Eval m Evald
applyBinOp _ op (EvaldFull a) (EvaldFull b) = do
    case applyBinaryOp (zBinOp op) a b of
      Just result -> evaldFull result
      Nothing     -> error $ "applyBinOp: type error in ("
                          ++ show a ++ " " ++ show op ++ " " ++ show b
                          ++ ")"
applyBinOp p op a b = case (op, evaldInt a, evaldInt b) of
    (Add,  _, Left (0 :: Int)) -> return a
    (Mult, _, Left (1 :: Int)) -> return a
    (Add,  Left (0 :: Int), _) -> return b
    (Mult, Left (1 :: Int), _) -> return b
    _otherwise        -> evaldPart $ eBinOp p op (unEvald a) (unEvald b)

-- | Smart constructor for unary operators
applyUnOp :: Monad m => Maybe SourcePos -> UnOp -> Evald -> Eval m Evald
applyUnOp _ op (EvaldFull a) = do
    case applyUnaryOp (zUnOp op) a of
      Just result -> evaldFull result
      Nothing     -> error $ "applyUnOp: type error in ("
                          ++ show op ++ " " ++ show a
                          ++ ")"
applyUnOp p op (EvaldPart e) = case (op, ctExp e) of
    -- Specific optimizations
    (ALength, TArray (Literal i) _) ->
      evaldFull $ MkValue (ValueInt32 (fromIntegral i)) p
    (ALength, TArray (NVar _) _) ->
      evaldPart $ eUnOp p ALength e
    (ALength, _) ->
      error $ "applyUnOp: type error in length(" ++ show e ++ ")"
    _otherwise ->
      evaldPart $ eUnOp p op e

{-------------------------------------------------------------------------------
  Assignment
-------------------------------------------------------------------------------}

data FullyEvaldDerefExp =
    DerefVar          (Maybe SourcePos) (GName Ty)
  | DerefArrayElement (Maybe SourcePos) FullyEvaldDerefExp Int
  | DerefArraySlice   (Maybe SourcePos) FullyEvaldDerefExp Int Int
  | DerefStructField  (Maybe SourcePos) FullyEvaldDerefExp FldName

unDeref :: Either FullyEvaldDerefExp Exp -> Exp
unDeref (Right e)        = e
unDeref (Left  derefExp) = go derefExp
  where
    go :: FullyEvaldDerefExp -> Exp
    go (DerefVar p x) =
      eVar p x
    go (DerefArrayElement p arr i) =
      eArrRead p (go arr) (eVal p tint32 (vint i)) LISingleton
    go (DerefArraySlice p arr i len) =
      eArrRead p (go arr) (eVal p tint32 (vint i)) (LILength len)
    go (DerefStructField p struct fld) =
      eProj p (go struct) fld

-- | Interpret a dereferencing expression (i.e., the LHS of an assignment)
--
-- Returns the fully or partially evaluated dereferencing expression, as well
-- as the "main variable"; i.e., for @x[0].y[5]...@  we return @x@.
--
-- TODO: The order of evaluation here (in case of side effects) is important
-- and should be verified against the semantics.
interpretDerefExp :: forall m. Monad m
                  => Exp -> Eval m (GName Ty, Either FullyEvaldDerefExp Exp)
interpretDerefExp e = go0 (unExp e)
  where
    go0 :: Exp0 -> Eval m (GName Ty, Either FullyEvaldDerefExp Exp)
    go0 (EVar x) =
      return (x, Left $ DerefVar eloc x)
    go0 (EArrRead arr ix LISingleton) = do
      (x, arrEvald) <- interpretDerefExp arr
      ixEvald       <- interpret ix
      case (arrEvald, evaldInt ixEvald) of
        (Left derefExp, Left i) ->
          return (x, Left $ DerefArrayElement eloc derefExp i)
        _otherwise ->
          return (x, Right $ eArrRead eloc (unDeref arrEvald)
                                           (unEvald ixEvald)
                                           LISingleton)
    go0 (EArrRead arr ix (LILength len)) = do
      (x, arrEvald) <- interpretDerefExp arr
      ixEvald       <- interpret ix
      case (arrEvald, evaldInt ixEvald) of
        (Left derefExp, Left i) ->
          return (x, Left $ DerefArraySlice eloc derefExp i len)
        _otherwise ->
          return (x, Right $ eArrRead eloc (unDeref arrEvald)
                                           (unEvald ixEvald)
                                           (LILength len))
    go0 (EArrRead _ _ (LIMeta _)) =
      error "interpretDerefExp: Unexpected metavariable"
    go0 (EProj struct fld) = do
      (x, structEvald) <- interpretDerefExp struct
      case structEvald of
        Left derefExp ->
          return (x, Left $ DerefStructField eloc derefExp fld)
        _otherwise ->
          return (x, Right $ eProj eloc (unDeref structEvald) fld)
    go0 _ =
      error "interpretDerefExp: invalid deferencing expression"

    eloc = expLoc e

-- | Assignment
--
-- (Auxiliary to `interpret`)
--
-- Returns whether the assignment was successful
assign :: forall m. Monad m => FullyEvaldDerefExp -> Value -> Eval m Bool
assign = \lhs rhs -> deref lhs (\_ -> return rhs)
  where
    -- `deref` gives the semantics of a derefercing expression by describing
    -- how a function on values is interpreted as a state update
    deref :: FullyEvaldDerefExp -> (Value -> Eval m Value) -> Eval m Bool
    deref (DerefVar _ x) f = do
      mOld <- readVar x
      case mOld of
        Just old -> do
          writeVar x =<< f old
          return True
        Nothing  -> do
          -- Not a local variable, or removed by `invalidateAssumptions`
          return False
    deref (DerefArrayElement p arr ix) f = do
      deref arr $ updateArray p ix f
    deref (DerefArraySlice p arr ix len) f = do
      deref arr $ updateSlice p ix len f
    deref (DerefStructField _p struct fld) f =
      deref struct $ updateStruct fld f

    -- Given a function that updates an element, construct a function that
    -- updates the array at a particular index
    updateArray :: Maybe SourcePos
                -> Int
                -> (Value -> Eval m Value) -> (Value -> Eval m Value)
    updateArray p i f arr =
      case arr of
        MkValue (ValueArray vs) eloc -> do
          if 0 <= i && i < SA.size vs
            then do
              let y = SA.unsafeReadArray i vs
              y' <- f y
              return $ MkValue (ValueArray (SA.unsafeWriteArray i y' vs)) eloc
            else
              throwError $ "Array index out of bounds at position " ++ show p
        _otherwise ->
          error "updateArray: type error"

    -- Given a function that updates a slice, construct a function that updates
    -- the whole array
    updateSlice :: Maybe SourcePos
                -> Int -> Int
                -> (Value -> Eval m Value) -> (Value -> Eval m Value)
    updateSlice p i len f arr =
      case arr of
        MkValue (ValueArray vs) eloc -> do
          if 0 <= i && i + len <= (SA.size vs)
            then do
              let slice = SA.unsafeSlice i len vs
              updated <- f $ MkValue (ValueArray slice) eloc
              case unValue updated of
                ValueArray slice' | SA.size slice == SA.size slice' ->
                  return $ MkValue (ValueArray (SA.unsafeUpdate i slice' vs)) eloc
                _  ->
                  -- TODO: Is a length mismatch between the two arrays
                  -- really a type error?
                  error "updateSlice: type error"
            else
              throwError $ "Array index out of bounds at position " ++ show p
        _otherwise ->
          error "updateSlice: type error"

    -- Given a function that updates an element, construct a function that
    -- updates a particular field of the struct
    updateStruct :: FldName
                 -> (Value -> Eval m Value) -> (Value -> Eval m Value)
    updateStruct fld f struct =
      case (struct, fld) of
        -- Structs
        (MkValue (ValueStruct ty flds) eloc, _) -> do
            let y = getField fld flds
            y' <- f y
            return $ MkValue (ValueStruct ty (updateField fld y' flds)) eloc
        -- 8-bit complex numbers
        (MkValue (ValueCpx8 v) eloc, "re") -> do
            let y = MkValue (ValueInt8 (re v)) eloc
            MkValue (ValueInt8 y') _ <- f y
            return $ MkValue (ValueCpx8 v{re = y'}) eloc
        (MkValue (ValueCpx8 v) eloc, "im") -> do
            let y = MkValue (ValueInt8 (im v)) eloc
            MkValue (ValueInt8 y') _ <- f y
            return $ MkValue (ValueCpx8 v{im = y'}) eloc
        -- 16-bit complex numbers
        (MkValue (ValueCpx16 v) eloc, "re") -> do
            let y = MkValue (ValueInt16 (re v)) eloc
            MkValue (ValueInt16 y') _ <- f y
            return $ MkValue (ValueCpx16 v{re = y'}) eloc
        (MkValue (ValueCpx16 v) eloc, "im") -> do
            let y = MkValue (ValueInt16 (im v)) eloc
            MkValue (ValueInt16 y') _ <- f y
            return $ MkValue (ValueCpx16 v{im = y'}) eloc
        -- 32-bit complex numbers
        (MkValue (ValueCpx32 v) eloc, "re") -> do
            let y = MkValue (ValueInt32 (re v)) eloc
            MkValue (ValueInt32 y') _ <- f y
            return $ MkValue (ValueCpx32 v{re = y'}) eloc
        (MkValue (ValueCpx32 v) eloc, "im") -> do
            let y = MkValue (ValueInt32 (im v)) eloc
            MkValue (ValueInt32 y') _ <- f y
            return $ MkValue (ValueCpx32 v{im = y'}) eloc
        -- 64-bit complex numbers
        (MkValue (ValueCpx64 v) eloc, "re") -> do
            let y = MkValue (ValueInt64 (re v)) eloc
            MkValue (ValueInt64 y') _ <- f y
            return $ MkValue (ValueCpx64 v{re = y'}) eloc
        (MkValue (ValueCpx64 v) eloc, "im") -> do
            let y = MkValue (ValueInt64 (im v)) eloc
            MkValue (ValueInt64 y') _ <- f y
            return $ MkValue (ValueCpx64 v{im = y'}) eloc
        _otherwise ->
          error "updateStruct: type error"

{-------------------------------------------------------------------------------
  Semantics of binary operators
-------------------------------------------------------------------------------}

newtype BinaryOp = BinaryOp { applyBinaryOp :: Value -> Value -> Maybe Value }

instance Monoid BinaryOp where
  mempty        = BinaryOp $ \_ _ -> Nothing
  f `mappend` g = BinaryOp $ \a b -> applyBinaryOp f a b <|> applyBinaryOp g a b

mkBinaryOp :: (Value0 -> Value0 -> Maybe Value0) -> BinaryOp
mkBinaryOp f = BinaryOp $ \(MkValue va p) (MkValue vb _) ->
                 (\v0 -> MkValue v0 p) <$> f va vb

zNum2 :: (forall a. Num a => a -> a -> a) -> BinaryOp
zNum2 f = mkBinaryOp go
  where
    go (ValueInt8   a) (ValueInt8   b) = Just $ ValueInt8   (f a b)
    go (ValueInt16  a) (ValueInt16  b) = Just $ ValueInt16  (f a b)
    go (ValueInt32  a) (ValueInt32  b) = Just $ ValueInt32  (f a b)
    go (ValueInt64  a) (ValueInt64  b) = Just $ ValueInt64  (f a b)
    go (ValueCpx8   a) (ValueCpx8   b) = Just $ ValueCpx8   (f a b)
    go (ValueCpx16  a) (ValueCpx16  b) = Just $ ValueCpx16  (f a b)
    go (ValueCpx32  a) (ValueCpx32  b) = Just $ ValueCpx32  (f a b)
    go (ValueCpx64  a) (ValueCpx64  b) = Just $ ValueCpx64  (f a b)
    go (ValueDouble a) (ValueDouble b) = Just $ ValueDouble (f a b)
    go _               _               = Nothing

zIntegral :: (forall a. Integral a => a -> a -> a) -> BinaryOp
zIntegral f = mkBinaryOp go
  where
    go (ValueInt8   a) (ValueInt8   b) = Just $ ValueInt8   (f a b)
    go (ValueInt16  a) (ValueInt16  b) = Just $ ValueInt16  (f a b)
    go (ValueInt32  a) (ValueInt32  b) = Just $ ValueInt32  (f a b)
    go (ValueInt64  a) (ValueInt64  b) = Just $ ValueInt64  (f a b)
    go (ValueCpx8   a) (ValueCpx8   b) = Just $ ValueCpx8   (f a b)
    go (ValueCpx16  a) (ValueCpx16  b) = Just $ ValueCpx16  (f a b)
    go (ValueCpx32  a) (ValueCpx32  b) = Just $ ValueCpx32  (f a b)
    go (ValueCpx64  a) (ValueCpx64  b) = Just $ ValueCpx64  (f a b)
    go _               _               = Nothing

zFloating :: (forall a. Floating a => a -> a -> a) -> BinaryOp
zFloating f = mkBinaryOp go
  where
    go (ValueDouble a) (ValueDouble b) = Just $ ValueDouble (f a b)
    go _               _               = Nothing

zBits2 :: (forall a. Bits a => a -> a -> a) -> BinaryOp
zBits2 f = mkBinaryOp go
  where
    go (ValueBit    a) (ValueBit    b) = Just $ ValueBit    (f a b)
    go (ValueBool   a) (ValueBool   b) = Just $ ValueBool   (f a b)
    go (ValueInt8   a) (ValueInt8   b) = Just $ ValueInt8   (f a b)
    go (ValueInt16  a) (ValueInt16  b) = Just $ ValueInt16  (f a b)
    go (ValueInt32  a) (ValueInt32  b) = Just $ ValueInt32  (f a b)
    go (ValueInt64  a) (ValueInt64  b) = Just $ ValueInt64  (f a b)
    go _               _               = Nothing

zShift :: (forall a. Bits a => a -> Int -> a) -> BinaryOp
zShift f = mkBinaryOp go
  where
    go (ValueInt8  a) (ValueInt8  b) = Just $ ValueInt8  (f' a b)
    go (ValueInt8  a) (ValueInt16 b) = Just $ ValueInt8  (f' a b)
    go (ValueInt8  a) (ValueInt32 b) = Just $ ValueInt8  (f' a b)
    go (ValueInt8  a) (ValueInt64 b) = Just $ ValueInt8  (f' a b)
    go (ValueInt16 a) (ValueInt8  b) = Just $ ValueInt16 (f' a b)
    go (ValueInt16 a) (ValueInt16 b) = Just $ ValueInt16 (f' a b)
    go (ValueInt16 a) (ValueInt32 b) = Just $ ValueInt16 (f' a b)
    go (ValueInt16 a) (ValueInt64 b) = Just $ ValueInt16 (f' a b)
    go (ValueInt32 a) (ValueInt8  b) = Just $ ValueInt32 (f' a b)
    go (ValueInt32 a) (ValueInt16 b) = Just $ ValueInt32 (f' a b)
    go (ValueInt32 a) (ValueInt32 b) = Just $ ValueInt32 (f' a b)
    go (ValueInt32 a) (ValueInt64 b) = Just $ ValueInt32 (f' a b)
    go (ValueInt64 a) (ValueInt8  b) = Just $ ValueInt64 (f' a b)
    go (ValueInt64 a) (ValueInt16 b) = Just $ ValueInt64 (f' a b)
    go (ValueInt64 a) (ValueInt32 b) = Just $ ValueInt64 (f' a b)
    go (ValueInt64 a) (ValueInt64 b) = Just $ ValueInt64 (f' a b)
    go _              _              = Nothing

    f' :: (Bits a, Integral b) => a -> b -> a
    f' x i = f x (fromIntegral i)

zOrd :: (forall a. Ord a => a -> a -> Bool) -> BinaryOp
zOrd f = mkBinaryOp $ go
  where
    go ValueUnit       ValueUnit       = Just $ ValueBool (f () ())
    go (ValueBit    a) (ValueBit    b) = Just $ ValueBool (f a b)
    go (ValueBool   a) (ValueBool   b) = Just $ ValueBool (f a b)
    go (ValueInt8   a) (ValueInt8   b) = Just $ ValueBool (f a b)
    go (ValueInt16  a) (ValueInt16  b) = Just $ ValueBool (f a b)
    go (ValueInt32  a) (ValueInt32  b) = Just $ ValueBool (f a b)
    go (ValueInt64  a) (ValueInt64  b) = Just $ ValueBool (f a b)
    go (ValueDouble a) (ValueDouble b) = Just $ ValueBool (f a b)
    go (ValueString a) (ValueString b) = Just $ ValueBool (f a b)
    go _               _               = Nothing

zBool2 :: (Bool -> Bool -> Bool) -> BinaryOp
zBool2 f = mkBinaryOp $ go
  where
    go (ValueBool a) (ValueBool b) = Just $ ValueBool (f a b)
    go _             _             = Nothing

zBinOp :: BinOp -> BinaryOp
zBinOp Add   = zNum2 (+)
zBinOp Sub   = zNum2 (-)
zBinOp Mult  = zNum2 (*)
zBinOp Div   = zIntegral quot <> zFloating (/)
zBinOp Rem   = zIntegral rem
zBinOp Expon = zIntegral (^) <> zFloating (**)
zBinOp ShL   = zShift shift
zBinOp ShR   = zShift (\x -> shift x . negate)
zBinOp BwAnd = zBits2 (.&.)
zBinOp BwOr  = zBits2 (.|.)
zBinOp BwXor = zBits2 xor
zBinOp Eq    = zOrd (==)
zBinOp Neq   = zOrd (/=)
zBinOp Lt    = zOrd (<)
zBinOp Gt    = zOrd (>)
zBinOp Leq   = zOrd (<=)
zBinOp Geq   = zOrd (>=)
zBinOp And   = zBool2 (&&)
zBinOp Or    = zBool2 (||)

{-------------------------------------------------------------------------------
  Semantics of unary operators
-------------------------------------------------------------------------------}

newtype UnaryOp = UnaryOp { applyUnaryOp :: Value -> Maybe Value }

instance Monoid UnaryOp where
  mempty        = UnaryOp $ \_ -> Nothing
  f `mappend` g = UnaryOp $ \a -> applyUnaryOp f a <|> applyUnaryOp g a

mkUnaryOp :: (Value0 -> Maybe Value0) -> UnaryOp
mkUnaryOp f = UnaryOp $ \(MkValue va p) -> (\v0 -> MkValue v0 p) <$> f va

zNum1 :: (forall a. Num a => a -> a) -> UnaryOp
zNum1 f = mkUnaryOp go
  where
    go (ValueInt8   a) = Just $ ValueInt8   (f a)
    go (ValueInt16  a) = Just $ ValueInt16  (f a)
    go (ValueInt32  a) = Just $ ValueInt32  (f a)
    go (ValueInt64  a) = Just $ ValueInt64  (f a)
    go (ValueDouble a) = Just $ ValueDouble (f a)
    go _               = Nothing

zBits1 :: (forall a. Bits a => a -> a) -> UnaryOp
zBits1 f = mkUnaryOp go
  where
    go (ValueBit   a) = Just $ ValueBit   (f a)
    go (ValueBool  a) = Just $ ValueBool  (f a)
    go (ValueInt8  a) = Just $ ValueInt8  (f a)
    go (ValueInt16 a) = Just $ ValueInt16 (f a)
    go (ValueInt32 a) = Just $ ValueInt32 (f a)
    go (ValueInt64 a) = Just $ ValueInt64 (f a)
    go _              = Nothing

zBool1 :: (Bool -> Bool) -> UnaryOp
zBool1 f = mkUnaryOp $ go
  where
    go (ValueBool a) = Just $ ValueBool (f a)
    go _             = Nothing

zALength :: UnaryOp
zALength = mkUnaryOp go
  where
    go (ValueArray arr) = Just $ ValueInt32 (fromIntegral (SA.size arr))
    go _                = Nothing

zCast :: Ty -> UnaryOp
zCast ty = mkUnaryOp (go ty)
  where
    go TUnit _ = Just $ ValueUnit

    go TBit  (ValueBit   a) = Just $ ValueBit a
    go TBit  (ValueBool  a) = Just $ ValueBit a
    go TBit  (ValueInt8  a) = Just $ ValueBit (toBool a)
    go TBit  (ValueInt16 a) = Just $ ValueBit (toBool a)
    go TBit  (ValueInt32 a) = Just $ ValueBit (toBool a)
    go TBit  (ValueInt64 a) = Just $ ValueBit (toBool a)
    go TBit  _              = Nothing

    go TBool (ValueBit  a)  = Just $ ValueBool a
    go TBool (ValueBool a)  = Just $ ValueBool a
    go TBool (ValueInt8  a) = Just $ ValueBool (toBool a)
    go TBool (ValueInt16 a) = Just $ ValueBool (toBool a)
    go TBool (ValueInt32 a) = Just $ ValueBool (toBool a)
    go TBool (ValueInt64 a) = Just $ ValueBool (toBool a)
    go TBool _              = Nothing

    go (TInt BW8)  (ValueBit    a) = Just $ ValueInt8 (fromBool a)
    go (TInt BW8)  (ValueBool   a) = Just $ ValueInt8 (fromBool a)
    go (TInt BW8)  (ValueInt8   a) = Just $ ValueInt8 a
    go (TInt BW8)  (ValueInt16  a) = Just $ ValueInt8 (fromIntegral a)
    go (TInt BW8)  (ValueInt32  a) = Just $ ValueInt8 (fromIntegral a)
    go (TInt BW8)  (ValueInt64  a) = Just $ ValueInt8 (fromIntegral a)
    go (TInt BW8)  (ValueDouble a) = Just $ ValueInt8 (round a)
    go (TInt BW8)  _               = Nothing

    go (TInt BW16) (ValueBit    a) = Just $ ValueInt16 (fromBool a)
    go (TInt BW16) (ValueBool   a) = Just $ ValueInt16 (fromBool a)
    go (TInt BW16) (ValueInt8   a) = Just $ ValueInt16 (fromIntegral a)
    go (TInt BW16) (ValueInt16  a) = Just $ ValueInt16 a
    go (TInt BW16) (ValueInt32  a) = Just $ ValueInt16 (fromIntegral a)
    go (TInt BW16) (ValueInt64  a) = Just $ ValueInt16 (fromIntegral a)
    go (TInt BW16) (ValueDouble a) = Just $ ValueInt16 (round a)
    go (TInt BW16) _               = Nothing

    go (TInt BW32) (ValueBit    a) = Just $ ValueInt32 (fromBool a)
    go (TInt BW32) (ValueBool   a) = Just $ ValueInt32 (fromBool a)
    go (TInt BW32) (ValueInt8   a) = Just $ ValueInt32 (fromIntegral a)
    go (TInt BW32) (ValueInt16  a) = Just $ ValueInt32 (fromIntegral a)
    go (TInt BW32) (ValueInt32  a) = Just $ ValueInt32 a
    go (TInt BW32) (ValueInt64  a) = Just $ ValueInt32 (fromIntegral a)
    go (TInt BW32) (ValueDouble a) = Just $ ValueInt32 (round a)
    go (TInt BW32) _               = Nothing

    go (TInt BW64) (ValueBit    a) = Just $ ValueInt64 (fromBool a)
    go (TInt BW64) (ValueBool   a) = Just $ ValueInt64 (fromBool a)
    go (TInt BW64) (ValueInt8   a) = Just $ ValueInt64 (fromIntegral a)
    go (TInt BW64) (ValueInt16  a) = Just $ ValueInt64 (fromIntegral a)
    go (TInt BW64) (ValueInt32  a) = Just $ ValueInt64 (fromIntegral a)
    go (TInt BW64) (ValueInt64  a) = Just $ ValueInt64 a
    go (TInt BW64) (ValueDouble a) = Just $ ValueInt64 (round a)
    go (TInt BW64) _               = Nothing

    go (TStruct "complex8"  _) (ValueCpx8  a) = Just $ ValueCpx8 a
    go (TStruct "complex8"  _) (ValueCpx16 a) = Just $ ValueCpx8 (fmap fromIntegral a)
    go (TStruct "complex8"  _) (ValueCpx32 a) = Just $ ValueCpx8 (fmap fromIntegral a)
    go (TStruct "complex8"  _) (ValueCpx64 a) = Just $ ValueCpx8 (fmap fromIntegral a)

    go (TStruct "complex16" _) (ValueCpx8  a) = Just $ ValueCpx16 (fmap fromIntegral a)
    go (TStruct "complex16" _) (ValueCpx16 a) = Just $ ValueCpx16 a
    go (TStruct "complex16" _) (ValueCpx32 a) = Just $ ValueCpx16 (fmap fromIntegral a)
    go (TStruct "complex16" _) (ValueCpx64 a) = Just $ ValueCpx16 (fmap fromIntegral a)

    go (TStruct "complex32" _) (ValueCpx8  a) = Just $ ValueCpx32 (fmap fromIntegral a)
    go (TStruct "complex32" _) (ValueCpx16 a) = Just $ ValueCpx32 (fmap fromIntegral a)
    go (TStruct "complex32" _) (ValueCpx32 a) = Just $ ValueCpx32 a
    go (TStruct "complex32" _) (ValueCpx64 a) = Just $ ValueCpx32 (fmap fromIntegral a)

    go (TStruct "complex64" _) (ValueCpx8  a) = Just $ ValueCpx64 (fmap fromIntegral a)
    go (TStruct "complex64" _) (ValueCpx16 a) = Just $ ValueCpx64 (fmap fromIntegral a)
    go (TStruct "complex64" _) (ValueCpx32 a) = Just $ ValueCpx64 (fmap fromIntegral a)
    go (TStruct "complex64" _) (ValueCpx64 a) = Just $ ValueCpx64 a

    go TDouble (ValueInt8   a) = Just $ ValueDouble (fromIntegral a)
    go TDouble (ValueInt16  a) = Just $ ValueDouble (fromIntegral a)
    go TDouble (ValueInt32  a) = Just $ ValueDouble (fromIntegral a)
    go TDouble (ValueInt64  a) = Just $ ValueDouble (fromIntegral a)
    go TDouble (ValueDouble a) = Just $ ValueDouble a
    go TDouble _               = Nothing

    go TString ValueUnit       = Just $ ValueString "()"
    go TString (ValueBit    a) = Just $ ValueString (show a)
    go TString (ValueBool   a) = Just $ ValueString (show a)
    go TString (ValueInt8   a) = Just $ ValueString (show a)
    go TString (ValueInt16  a) = Just $ ValueString (show a)
    go TString (ValueInt32  a) = Just $ ValueString (show a)
    go TString (ValueInt64  a) = Just $ ValueString (show a)
    go TString (ValueDouble a) = Just $ ValueString (show a)
    go TString (ValueString a) = Just $ ValueString a
    go TString _               = Nothing

    -- Unsupported target type
    go _ _ = Nothing

    toBool :: (Num a, Eq a) => a -> Bool
    toBool n = if n == 0 then False else True

    fromBool :: Num a => Bool -> a
    fromBool b = fromInteger (if b then 0 else 1)

-- | Semantics for unary operators
zUnOp :: UnOp -> UnaryOp
zUnOp NatExp    = error "NatExp not implemented"
zUnOp Neg       = zNum1 negate
zUnOp Not       = zBool1 not
zUnOp BwNeg     = zBits1 complement
zUnOp ALength   = zALength
zUnOp (Cast ty) = zCast ty

{-------------------------------------------------------------------------------
  Guessing (for satisfiability checking)
-------------------------------------------------------------------------------}

-- | Guess the value for expressions
--
-- See http://www.well-typed.com/blog/2014/12/simple-smt-solver/ for a
-- discussion of this approach.
guess :: Exp -> Eval [] Evald
guess e | Just (lhs, op, rhs) <- isComparison e = do
    dom <- L.getSt $ evalGuessesInt . L.mapAtDef fullIntDomain (eraseLoc lhs)
    let assumeTrue = do
          let dom' = intersectIntDomains dom (mkIntDomain op rhs)
          guard $ not (emptyIntDomain dom')
          L.putSt (evalGuessesInt . L.mapAt (eraseLoc lhs)) (Just dom')
          evaldFull $ vTrue (expLoc lhs)
        assumeFalse = do
          let dom' = intersectIntDomains dom (mkIntDomain (negBinOp op) rhs)
          guard $ not (emptyIntDomain dom')
          L.putSt (evalGuessesInt . L.mapAt (eraseLoc lhs)) (Just dom')
          evaldFull $ vFalse (expLoc lhs)
    assumeTrue `mplus` assumeFalse
guess e | TBool <- ctExp e = do
    previous <- L.getSt $ evalGuessesBool . L.mapAt (eraseLoc e)
    case previous of
      Just True  ->
        evaldFull $ vTrue (expLoc e)
      Just False ->
        evaldFull $ vTrue (expLoc e)
      Nothing -> do
        let assumeTrue = do
              L.putSt (evalGuessesBool . L.mapAt (eraseLoc e)) (Just True)
              evaldFull $ vTrue (expLoc e)
            assumeFalse = do
              L.putSt (evalGuessesBool . L.mapAt (eraseLoc e)) (Just False)
              evaldFull $ vFalse (expLoc e)
        assumeTrue `mplus` assumeFalse
guess _e = mzero

isComparison :: Exp -> Maybe (Exp, BinOp, Integer)
isComparison e
    | EBinOp op lhs rhs <- unExp e
    , EVal _ (VInt i)   <- unExp rhs
    , op `elem` comparisonOps
    = Just (lhs, op, i)
  where
    comparisonOps = [Eq, Neq, Lt, Gt, Leq, Geq]
isComparison _ = Nothing

negBinOp :: BinOp -> BinOp
negBinOp Eq  = Neq
negBinOp Neq = Eq
negBinOp Lt  = Geq
negBinOp Gt  = Leq
negBinOp Leq = Gt
negBinOp Geq = Lt
negBinOp _   = error "negBinOp: invalid bin op"

{-------------------------------------------------------------------------------
  Integer domains
-------------------------------------------------------------------------------}

-- | Integer domains
--
-- An integer domain describes our current approximation for an integer.  Let
-- @d@ be an integer domain, and @x@ be an integer; then if @x `in` d@..
data IntDomain = IntDomain {
      intDomLower :: Maybe Integer   -- ^ .. @intDomLower d <= x@
    , intDomUpper :: Maybe Integer   -- ^ .. @x <= intDomUpper d@
    , intDomHoles :: Set Integer     -- ^ .. @x `notElem` intDomHoles d@
    }
    deriving (Eq, Show)

fullIntDomain :: IntDomain
fullIntDomain = IntDomain {
      intDomLower = Nothing
    , intDomUpper = Nothing
    , intDomHoles = Set.empty
    }

emptyIntDomain :: IntDomain -> Bool
emptyIntDomain d =
    fromMaybe False $ aux (intDomHoles d) <$> intDomLower d <*> intDomUpper d
  where
    aux holes lo hi = lo > hi || (lo == hi && lo `Set.member` holes)

intersectIntDomains :: IntDomain -> IntDomain -> IntDomain
intersectIntDomains d d' = IntDomain {
      intDomLower = case (intDomLower d, intDomLower d') of
                      (Nothing, Nothing) -> Nothing
                      (Just l,  Nothing) -> Just l
                      (Nothing, Just l') -> Just l'
                      (Just l,  Just l') -> Just (l `max` l')
    , intDomUpper = case (intDomUpper d, intDomUpper d') of
                      (Nothing, Nothing) -> Nothing
                      (Just l,  Nothing) -> Just l
                      (Nothing, Just l') -> Just l'
                      (Just l,  Just l') -> Just (l `min` l')
    , intDomHoles = intDomHoles d `Set.union` intDomHoles d'
    }

mkIntDomain :: BinOp -> Integer -> IntDomain
mkIntDomain Eq  n = IntDomain (Just n)       (Just n)       Set.empty
mkIntDomain Leq n = IntDomain Nothing        (Just n)       Set.empty
mkIntDomain Geq n = IntDomain (Just n)       Nothing        Set.empty
mkIntDomain Lt  n = IntDomain Nothing        (Just (n - 1)) Set.empty
mkIntDomain Gt  n = IntDomain (Just (n + 1)) Nothing        Set.empty
mkIntDomain Neq n = IntDomain Nothing        Nothing        (Set.singleton n)
mkIntDomain _   _ = error "mkIntDomain: Invalid operator"

{-------------------------------------------------------------------------------
  Constructing expressions (for convenience)
-------------------------------------------------------------------------------}

eNot :: Exp -> Exp
eNot e = eUnOp (expLoc e) Not e

eOr :: Exp -> Exp -> Exp
eOr e e' = eBinOp (expLoc e) Or e e'

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

second' :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
second' f (c, a) = do b <- f a ; return (c, b)

getField :: FldName -> [(FldName, a)] -> a
getField fld = go
  where
    go [] = error $ "Type error: Unknown field " ++ show fld
    go ((fld', a):flds) | fld == fld' = a
                        | otherwise   = getField fld flds

updateField :: FldName -> a -> [(FldName, a)] -> [(FldName, a)]
updateField fld a' = go
  where
    go [] = error $ "Type error: Unknown field " ++ show fld
    go ((fld', a):flds) | fld == fld' = (fld',a') : flds
                        | otherwise   = (fld',a)  : go flds
