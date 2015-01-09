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
{-# OPTIONS_GHC -Wall -Wwarn #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter (
    -- * Values
    Value(..)
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
import Control.Monad.RWS hiding (Any)
import Data.Bits hiding (bit)
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import GHC.Prim (Any)
import Outputable
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec.Pos (SourcePos)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AstExpr
import AstUnlabelled
import CtExpr (ctExp)
import GenSym (initGenSym)
import SparseArray (SparseArray)
import TcMonad (runTcM')
import Typecheck (tyCheckExpr)
import Utils
import qualified Lens        as L
import qualified SparseArray as SA

{-------------------------------------------------------------------------------
  Values
-------------------------------------------------------------------------------}

-- | Generalization of `Val` that also allows for arrays consisting of values
-- and for structs consisting of values
data Value =
    ValueScalar (Maybe SourcePos) Ty Val
  | ValueArray  (Maybe SourcePos) (SparseArray Value)
  | ValueStruct (Maybe SourcePos) Ty [(FldName, Value)]
  deriving Eq

instance Show Value where
  show = show . valueExp

expValue :: Exp -> Maybe Value
expValue e = go (unExp e)
  where
    go :: Exp0 -> Maybe Value
    go (EVal    ty val)  = return $ ValueScalar eloc ty val
    go (EValArr    elms) = do let def = arrayDefault eloc $ ctExp (head elms)
                              mkArray def <$> mapM expValue elms
    go (EStruct ty flds) = mkStruct ty <$> mapM (second' expValue) flds
    go _                 = Nothing

    mkArray def = ValueArray eloc . SA.newListArray def
    mkStruct ty = ValueStruct eloc ty

    eloc = expLoc e

    second' :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
    second' f (c, a) = do b <- f a ; return (c, b)

valueExp :: Value -> Exp
valueExp (ValueScalar p ty val)  = eVal    p ty val
valueExp (ValueArray  p    vals) = eValArr p    (map valueExp (SA.getElems vals))
valueExp (ValueStruct p ty vals) = eStruct p ty (map (second valueExp) vals)

vTrue :: Maybe SourcePos -> Value
vTrue p = ValueScalar p TBool (VBool True)

vFalse :: Maybe SourcePos -> Value
vFalse p = ValueScalar p TBool (VBool False)

{-------------------------------------------------------------------------------
  Main entry points
-------------------------------------------------------------------------------}
--
-- | Any prints of statically known values we execute during interpretation
type Prints = [(Bool, Value)]

-- | (Full) evaluation of an expression
evalFull :: Exp -> (Either String Value, Prints)
evalFull e = aux $ evalEval (interpret e) EvalFull initState
  where
    aux [(Right (EvaldFull v), prints)] = (Right v,  prints)
    aux [(Left err, prints)]            = (Left err, prints)
    aux _ = error "evalFull: the impossible happened"

-- | (Partial) evaluation of an expression
evalPartial :: Exp -> (Either String Exp, Prints)
evalPartial e = aux $ evalEval (interpret e) EvalPartial initState
  where
    aux [(Right e', prints)] = (Right (unEvald e'), prints)
    aux [(Left err, prints)] = (Left err, prints)
    aux _ = error "evalPartial: the impossible happened"

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
    (Right (ValueScalar _ _ (VInt i)) , prints) -> (Right i               , prints)
    (Right _                          , prints) -> (Left "Not an integer" , prints)
    (Left  err                        , prints) -> (Left err              , prints)

-- | Evaluate an expression to a boolean
evalBool :: Exp -> (Either String Bool, Prints)
evalBool e = case evalFull e of
    (Right (ValueScalar _ _ (VBool b)) , prints) -> (Right b               , prints)
    (Right _                           , prints) -> (Left "Not an boolean" , prints)
    (Left  err                         , prints) -> (Left err              , prints)

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
data EvalMode =
    -- | Partial evaluation
    --
    -- Free variables will be left unchanged, where possible.
    --
    -- > a + 2 * 3 ~~> a + 6
    EvalPartial

    -- | Full evaluation
    --
    -- Free variables in the expression will result in an error.
  | EvalFull

    -- | Approximation
    --
    -- For use in satisfiability checking.
    --
    -- > if a then 1 else 2 ~~> [1, 2]
  | EvalNonDet

-- | Evaluation state
data EvalState = EvalState {
    -- | Let-bound variables (immutable)
    _evalLets :: Map String Value


    -- | Letref-bound variables (mutable)
  , _evalLetRefs :: Map String Value

    -- | Guesses about boolean-valued expressions
    --
    -- Used in `EvalNonDet` mode only.
  , _evalGuessesBool :: Map Exp Bool

    -- | Guesses about integer-valued expressions
    --
    -- Used in `EvalNonDet` mode only.
  , _evalGuessesInt :: Map Exp IntDomain
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

initState :: EvalState
initState = EvalState {
    _evalLets        = Map.empty
  , _evalLetRefs     = Map.empty
  , _evalGuessesBool = Map.empty
  , _evalGuessesInt  = Map.empty
  }

-- | The evaluator monad
--
-- The evaluator monad keeps track of a lot of things:
--
-- 1. ErrorT: Runtime errors such as out-of-bound array indices
-- 2. R:  Interpreter mode (full, partial, non-deterministic)
-- 3. W:  Any debug prints executed by the program
-- 4. S:  Locally bound vars and writes to locally bound vars, and guesses for
--        non-deterministic evaluation
-- 5. []: Non-determinism arising from guessing values
newtype Eval a = Eval {
      unEval :: ErrorT String (RWST EvalMode Prints EvalState []) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError String
           , MonadState EvalState
           )

-- | We want the `MonadPlus` arising from the underlying list monad, not
-- from the top-level `ErrorT` monad.
instance MonadPlus Eval where
  mzero       = mkEval $ \_mode _st -> []
  f `mplus` g = mkEval $ \mode st -> runEvald f mode st <|> runEvald g mode st

instance Alternative Eval where
  empty = mzero
  (<|>) = mplus

runEvald :: Eval a -> EvalMode -> EvalState -> [(Either String a, EvalState, Prints)]
runEvald = runRWST . runErrorT . unEval

evalEval :: Eval a -> EvalMode -> EvalState -> [(Either String a, Prints)]
evalEval = evalRWST . runErrorT . unEval

mkEval :: (EvalMode -> EvalState -> [(Either String a, EvalState, Prints)]) -> Eval a
mkEval = Eval . ErrorT . RWST

getMode :: Eval EvalMode
getMode = Eval ask

logPrint :: Bool -> Value -> Eval ()
logPrint newline val = Eval $ tell [(newline, val)]

-- | Run the second action only if the first one results zero results
--
-- (Does NOT run the second action if the first one results an error).
onZero :: Eval a -> Eval a -> Eval a
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
readVar :: GName Ty -> Eval (Maybe Value)
readVar x = do
  mLetBound <- L.getSt $ evalLets . L.mapAt (uniqId x)
  case mLetBound of
    Just letBound -> return $ Just letBound
    Nothing       -> L.getSt $ evalLetRefs . L.mapAt (uniqId x)

extendScope :: L.Lens EvalState (Map String Value)  -- ^ Scope to extend
            -> GName Ty                             -- ^ Variable to introduce
            -> Value                                -- ^ Initial value
            -> Eval a -> Eval a
extendScope scope x initVal act = do
    -- Check if variable already in scope (if this happens it's a compiler bug)
    mCurrentValue <- readVar x
    case mCurrentValue of
      Nothing -> return ()
      Just _  -> throwError $ "Variable " ++ pretty x ++ " already in scope"

    L.modifySt scope $ Map.insert (uniqId x) initVal
    a <- act
    L.modifySt scope $ Map.delete (uniqId x)
    return a

-- | Write a variable
--
-- The caller should verify that the variable is in scope.
writeVar :: GName Ty -> Value -> Eval ()
writeVar x v = L.modifySt evalLetRefs $ Map.insert (uniqId x) v

-- | Indicate that we could only partially evaluate an AST constructor
--
-- We use this whenever we were not able to eliminate an AST constructor. In
-- partial evaluation, this is just `return`; during full evaluation this is
-- an error, and during non-deterministic evaluation this results in an
-- empty list of results.
evaldPart :: Exp -> Eval Evald
evaldPart x = do
  mode <- getMode
  case mode of
    EvalPartial -> return $ EvaldPart x
    EvalFull    -> throwError "Free variables"
    EvalNonDet  -> mzero

-- | Indicate that we managed to fully evaluate an expression
evaldFull :: Value -> Eval Evald
evaldFull = return . EvaldFull

-- | Run the guess algorithm if we could not fully evaluate the expression.
guessIfUnevaluated :: (Exp -> Eval Evald) -> (Exp -> Eval Evald)
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
invalidateAssumptions :: Eval ()
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
invalidateAssumptionsFor :: GName Ty -> Eval ()
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

{-------------------------------------------------------------------------------
  The interpreter proper
-------------------------------------------------------------------------------}

-- | Interpreter for the expression language
interpret :: Exp -> Eval Evald
interpret e = guessIfUnevaluated (go . unExp) e
  where
    go :: Exp0 -> Eval Evald

    -- Values

    go (EVal ty val) =
      evaldFull $ ValueScalar eloc ty val

    -- Arrays

    go (EValArr elems) = do
      elemsEvald <- mapM interpret elems
      case partitionEvalds elemsEvald of
        Left elems' -> do
          let def = arrayDefault eloc $ ctExp (head elems)
          evaldFull $ ValueArray eloc (SA.newListArray def elems')
        Right elems' ->
          evaldPart $ eValArr eloc elems'
    go (EArrRead arr ix li) = do
      arrEvald <- interpret arr
      ixEvald  <- interpret ix
      case (arrEvald, ixEvald) of
        -- We only select elements from fully known arrays. See comment for
        -- EProj about optimizing element selection from known arrays with
        -- non-value elements
        (EvaldFull (ValueArray _ elems), EvaldFull (ValueScalar _ _ (VInt i))) ->
          case li of
            LISingleton ->
              if 0 <= i && i < toInteger (SA.size elems)
                then evaldFull $ SA.unsafeReadArray (fromInteger i) elems
                else throwError "Out of bounds"
            LILength len ->
              if 0 <= i && i + toInteger len <= toInteger (SA.size elems)
                then evaldFull $ ValueArray eloc
                               $ SA.unsafeSlice (fromInteger i) len elems
                else throwError "Out of bounds"
            LIMeta _ ->
              error "Unexpected meta variable"
        -- "Full" slice (capturing the entire array)
        (EvaldPart arr', EvaldFull (ValueScalar _ _ (VInt 0)))
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
        Left fieldDefs' ->
          evaldFull $ ValueStruct eloc ty (zip fieldNames fieldDefs')
        Right fieldDefs' ->
          evaldPart $ eStruct eloc ty (zip fieldNames fieldDefs')
    go (EProj struct fld) = do
      structEvald <- interpret struct
      -- We _could_ potentially also optimize projections out of known structs
      -- with non-value fields, but we have to be careful in that case that we
      -- do not lose side effects, and anyway that optimization is less useful.
      case structEvald of
        EvaldFull (ValueStruct _ _ fields) ->
          case splitListOn ((== fld) . fst) fields of
            Just (_, (_, y), _) -> evaldFull y
            Nothing             -> throwError $ "Unknown field"
        EvaldFull _ ->
          error "Type error"
        EvaldPart struct' ->
          evaldPart $ eProj eloc struct' fld

    -- Simple operators

    go (EUnOp  op a) = do
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
      case initialVal eloc (nameTyp x) of
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
        then evaldFull $ ValueScalar eloc TUnit VUnit
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
        then evaldFull $ ValueScalar eloc TUnit VUnit
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
      case e1Evald of
        EvaldFull (ValueScalar _ _ VUnit) ->
          return e2Evald
        _otherwise ->
          -- We could omit e1 completely if we manage to reduce e2 to a value
          -- independent of e1, and e1 is side effect free.
          evaldPart $ eSeq eloc (unEvald e1Evald) (unEvald e2Evald)
    go (EIf cond iftrue iffalse) = do
      condEvald <- interpret cond
      case condEvald of
        EvaldFull (ValueScalar _ _ (VBool b)) ->
          interpret (if b then iftrue else iffalse)
        EvaldFull _ ->
          error "interpret: type error (condition not a boolean)"
        EvaldPart cond' -> do
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
      fullEv <- case (startEvald, lenEvald) of
        (EvaldFull (ValueScalar _ ty (VInt start')),
         EvaldFull (ValueScalar _ _  (VInt len'))) -> do
          let loop n | n == start' + len' =
                return True
              loop n = do
                bodyEvald <-
                  extendScope evalLets x (ValueScalar eloc ty (VInt n)) $
                    interpret body
                -- Only when we can fully evaluate the body of the loop do we
                -- continue. If not, we give up completely (alternatively, we
                -- could choose to do loop unrolling here).
                case bodyEvald of
                  EvaldFull (ValueScalar _ _ VUnit) ->
                    loop (n + 1)
                  _otherwise ->
                    return False

          loop start'
        _otherwise -> -- Start or length not fully evaluated
          return False
      if fullEv
        then evaldFull $ ValueScalar eloc TUnit VUnit
        else do -- See comments for conditionals
                invalidateAssumptions
                bodyEvald <- interpret body
                evaldPart $ eFor eloc ui x (unEvald startEvald)
                                           (unEvald lenEvald)
                                           (unEvald bodyEvald)
    go (EWhile cond body) = do
      let loop = do
            condEvald <- interpret cond
            case condEvald of
              EvaldFull (ValueScalar _ _ (VBool False)) ->
                return True
              EvaldFull (ValueScalar _ _ (VBool True)) -> do
                bodyEvald <- interpret body
                case bodyEvald of
                  EvaldFull (ValueScalar _ _ VUnit) ->
                    loop
                  _otherwise ->
                    return False -- See comments for `EFor`
              _otherwise -> -- Condition not fully evaluated
                return False
      fullEv <- loop
      if fullEv then evaldFull $ ValueScalar eloc TUnit VUnit
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
        EvaldFull v1' -> do
          logPrint newline v1'
          evaldFull $ ValueScalar eloc TUnit VUnit
        EvaldPart e1' ->
          evaldPart $ ePrint eloc newline e1'
    go (EError ty str) =
      evaldPart $ eError eloc ty str
    go (ELUT _ _) =
      throwError "Unexpected LUT during interpretation"

    eloc :: Maybe SourcePos
    eloc = expLoc e

interpretLetRef :: Maybe SourcePos -> GName Ty -> Value -> Exp -> Eval Evald
interpretLetRef eloc x v1 e2 = do
    (e2Evald, xDeleted) <- extendScope evalLetRefs x v1 $ do
      e2Evald  <- interpret e2
      xDeleted <- isNothing <$> readVar x
      return (e2Evald, xDeleted)
    -- If at any point x was (or might have been) assigned a not
    -- statically known value, we cannot remove the binding site.
    if xDeleted
      then evaldPart $ eLetRef eloc x (Just (valueExp v1)) (unEvald e2Evald)
      else return e2Evald

-- | The runtime currently leaves the initial value for unassigned variables
-- unspecified (https://github.com/dimitriv/Ziria/issues/79). This means that
-- we are free to specify whatever we wish in the interpret -- here we pick
-- sensible defaults.
initialVal :: Maybe SourcePos -> Ty -> Maybe Value
initialVal p = \ty ->
    case ty of
      TArray (Literal n) ty' -> ValueArray p . SA.newArray n <$> initialVal p ty'
      TStruct _ fields       -> ValueStruct p ty <$> mapM initialField fields
      TUnit                  -> return $ ValueScalar p ty $ VUnit
      TBit                   -> return $ ValueScalar p ty $ VBit    False
      TBool                  -> return $ ValueScalar p ty $ VBool   False
      TString                -> return $ ValueScalar p ty $ VString ""
      TDouble                -> return $ ValueScalar p ty $ VDouble 0
      (TInt _)               -> return $ ValueScalar p ty $ VInt    0
      _                      -> Nothing
  where
    initialField :: (FldName, Ty) -> Maybe (FldName, Value)
    initialField (fldName, ty) = do e <- initialVal p ty ; return (fldName, e)

-- | Specialization of `initialVal` specifically for the default values in
-- arrays; since we can create defaults for all types that we allow to store
-- inside arrays, this does not return a Maybe.
arrayDefault :: Maybe SourcePos -> Ty -> Value
arrayDefault p = fromJust . initialVal p

-- | Smart constructor for binary operators
applyBinOp :: Maybe SourcePos -> BinOp -> Evald -> Evald -> Eval Evald
applyBinOp p op (EvaldFull v) (EvaldFull v') = do
    let applied = do a <- valueToDyn v
                     b <- valueToDyn v'
                     dynToValue p $ zBinOp op `dynApply` a `dynApply` b
    case applied of
      Just result -> evaldFull result
      Nothing     -> error $ "applyBinOp: type error in ("
                          ++ show v ++ " " ++ show op ++ " " ++ show v'
                          ++ ")"
applyBinOp p op a b = case (op, a, b) of
    (Add,  _, EvaldFull (ValueScalar _ _ (VInt 0))) -> return a
    (Mult, _, EvaldFull (ValueScalar _ _ (VInt 1))) -> return a
    (Add,  EvaldFull (ValueScalar _ _ (VInt 0)), _) -> return b
    (Mult, EvaldFull (ValueScalar _ _ (VInt 1)), _) -> return b
    _otherwise ->
      evaldPart $ eBinOp p op (unEvald a) (unEvald b)

-- | Smart constructor for unary operators
applyUnOp :: Maybe SourcePos -> UnOp -> Evald -> Eval Evald
applyUnOp p op (EvaldFull v) = case (op, v) of
    -- Special case for ALength, because it is a polymorphic operation and
    -- we don't support it in our Dynamic framework
    (ALength, ValueArray _ vs) ->
      evaldFull $ ValueScalar p tint32 (VInt (toInteger (SA.size vs)))
    (ALength, _) ->
      error $ "applyUnOp: type error in length("
           ++ show v
           ++ ")"
    _otherwise -> do
      let applied = do a <- valueToDyn v
                       dynToValue p $ zUnOp op `dynApply` a
      case applied of
        Just result -> evaldFull result
        Nothing     -> error $ "applyUnOp: type error in ("
                            ++ show op ++ " " ++ show v
                            ++ ")"
applyUnOp p op (EvaldPart e) = case (op, ctExp e) of
    -- Specific optimizations
    (ALength, TArray (Literal i) _) ->
      evaldFull $ ValueScalar p tint32 (VInt (toInteger i))
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
interpretDerefExp :: Exp -> Eval (GName Ty, Either FullyEvaldDerefExp Exp)
interpretDerefExp e = go0 (unExp e)
  where
    go0 :: Exp0 -> Eval (GName Ty, Either FullyEvaldDerefExp Exp)
    go0 (EVar x) =
      return (x, Left $ DerefVar eloc x)
    go0 (EArrRead arr ix LISingleton) = do
      (x, arrEvald) <- interpretDerefExp arr
      ixEvald       <- interpret ix
      case (arrEvald, ixEvald) of
        (Left derefExp, EvaldFull (ValueScalar _ _ (VInt i))) ->
          return (x, Left $ DerefArrayElement eloc derefExp (fromInteger i))
        (Left _derefExp, EvaldFull _) ->
          error "interpretDerefExp: type error"
        _otherwise ->
          return (x, Right $ eArrRead eloc (unDeref arrEvald)
                                           (unEvald ixEvald)
                                           LISingleton)
    go0 (EArrRead arr ix (LILength len)) = do
      (x, arrEvald) <- interpretDerefExp arr
      ixEvald       <- interpret ix
      case (arrEvald, ixEvald) of
        (Left derefExp, EvaldFull (ValueScalar _ _ (VInt i))) ->
          return (x, Left $ DerefArraySlice eloc derefExp (fromInteger i) len)
        (Left _derefExp, EvaldFull _) ->
          error "interpretDerefExp: type error"
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
assign :: FullyEvaldDerefExp -> Value -> Eval Bool
assign = \lhs rhs -> deref lhs (\_ -> return rhs)
  where
    -- `deref` gives the semantics of a derefercing expression by describing
    -- how a function on values is interpreted as a state update
    deref :: FullyEvaldDerefExp -> (Value -> Eval Value) -> Eval Bool
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
                -> (Value -> Eval Value) -> (Value -> Eval Value)
    updateArray p i f arr =
      case arr of
        ValueArray eloc vs -> do
          if 0 <= i && i < SA.size vs
            then do
              let y = SA.unsafeReadArray i vs
              y' <- f y
              return $ ValueArray eloc (SA.unsafeWriteArray i y' vs)
            else
              throwError $ "Array index out of bounds at position " ++ show p
        _otherwise ->
          error "updateArray: type error"

    -- Given a function that updates a slice, construct a function that updates
    -- the whole array
    updateSlice :: Maybe SourcePos
                -> Int -> Int
                -> (Value -> Eval Value) -> (Value -> Eval Value)
    updateSlice p i len f arr =
      case arr of
        ValueArray eloc vs -> do
          if 0 <= i && i + len <= (SA.size vs)
            then do
              let slice = SA.unsafeSlice i len vs
              updated <- f $ ValueArray eloc slice
              case updated of
                ValueArray _ slice' | SA.size slice == SA.size slice' ->
                  return $ ValueArray eloc (SA.unsafeUpdate i slice' vs)
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
                 -> (Value -> Eval Value) -> (Value -> Eval Value)
    updateStruct fld f struct =
      case struct of
        ValueStruct eloc ty flds
          | Just (xs, (_fld, y), zs) <- splitListOn ((== fld) . fst) flds -> do
              y' <- f y
              return $ ValueStruct eloc ty (xs ++ [(fld, y')] ++ zs)
        _otherwise ->
          error "updateStruct: type error"

{-------------------------------------------------------------------------------
  Since the interpreter works with dynamically typed values, we provide some
  infrastructure here for working with such values. This mirrors the standard
  definition of (old) Typeable and Dynamic, but using Ziria's own types and
  with support for ad-hoc polymorphism.
-------------------------------------------------------------------------------}

class Typeable a where
  typeOf :: a -> Ty

-- | Avoids confusion between booleans and bits
newtype Bit = Bit { bit :: Bool }
  deriving (Eq, Ord, Enum, Show, Bits)

instance Typeable ()     where typeOf _ = TUnit
instance Typeable Bit    where typeOf _ = TBit
instance Typeable Bool   where typeOf _ = TBool
instance Typeable Int8   where typeOf _ = TInt BW8
instance Typeable Int16  where typeOf _ = TInt BW16
instance Typeable Int32  where typeOf _ = TInt BW32
instance Typeable Int64  where typeOf _ = TInt BW64
instance Typeable Double where typeOf _ = TDouble
instance Typeable String where typeOf _ = TString

instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf (_ :: a -> b) = TArrow [typeOf (undefined :: a)]
                                (typeOf (undefined :: b))

data Dynamic = Dynamic [(Ty, Any)]

instance Show Dynamic where
  show (Dynamic vs) =
      "Dynamic [" ++ intercalate "," (map (uncurry aux) vs) ++ "]"
    where
      aux :: Ty -> Any -> String
      aux ty val = case dynToScalar' ty val of
                     Just e  -> pretty e      ++ " :: " ++ pretty ty
                     Nothing -> "<<dynamic>>" ++ " :: " ++ pretty ty

toDyn :: Typeable a => a -> Dynamic
toDyn x = Dynamic [(typeOf x, unsafeCoerce x)]

dynApply :: Dynamic -> Dynamic -> Dynamic
dynApply (Dynamic fs) (Dynamic xs) = Dynamic
    [ (yty, unsafeCoerce f x)
    | (fty, f) <- fs
    , (xty, x) <- xs
    , Just yty <- [funResult fty xty]
    ]
  where
    funResult :: Ty -> Ty -> Maybe Ty
    funResult (TArrow [a] b) a' | a == a' = Just $ b
    funResult _ _ = Nothing

instance Monoid Dynamic where
  mempty = Dynamic []
  Dynamic xs `mappend` Dynamic ys = Dynamic (xs ++ ys)

{-------------------------------------------------------------------------------
  Define the semantics of Ziria operators using the dynamic typing
  infrastructure developed in the previous section.
-------------------------------------------------------------------------------}

zNum2 :: (forall a. Num a => a -> a -> a) -> Dynamic
zNum2 f = mconcat [
      toDyn (f :: Int8   -> Int8   -> Int8)
    , toDyn (f :: Int16  -> Int16  -> Int16)
    , toDyn (f :: Int32  -> Int32  -> Int32)
    , toDyn (f :: Int64  -> Int64  -> Int64)
    , toDyn (f :: Double -> Double -> Double)
    ]

zIntegral :: (forall a. Integral a => a -> a -> a) -> Dynamic
zIntegral f = mconcat [
      toDyn (f :: Int8  -> Int8  -> Int8)
    , toDyn (f :: Int16 -> Int16 -> Int16)
    , toDyn (f :: Int32 -> Int32 -> Int32)
    , toDyn (f :: Int64 -> Int64 -> Int64)
    ]

zFloating :: (forall a. Floating a => a -> a -> a) -> Dynamic
zFloating f = mconcat [
      toDyn (f :: Double -> Double -> Double)
    ]

zBits2 :: (forall a. Bits a => a -> a -> a) -> Dynamic
zBits2 f = mconcat [
      toDyn (f :: Bit   -> Bit   -> Bit)
    , toDyn (f :: Bool  -> Bool  -> Bool)
    , toDyn (f :: Int8  -> Int8  -> Int8)
    , toDyn (f :: Int16 -> Int16 -> Int16)
    , toDyn (f :: Int32 -> Int32 -> Int32)
    , toDyn (f :: Int64 -> Int64 -> Int64)
    ]

zShift :: (forall a. Bits a => a -> Int -> a) -> Dynamic
zShift f = mconcat [
      toDyn (f' :: Int8  -> Int8  -> Int8)
    , toDyn (f' :: Int8  -> Int16 -> Int8)
    , toDyn (f' :: Int8  -> Int32 -> Int8)
    , toDyn (f' :: Int8  -> Int64 -> Int8)
    , toDyn (f' :: Int16 -> Int8  -> Int16)
    , toDyn (f' :: Int16 -> Int16 -> Int16)
    , toDyn (f' :: Int16 -> Int32 -> Int16)
    , toDyn (f' :: Int16 -> Int64 -> Int16)
    , toDyn (f' :: Int32 -> Int8  -> Int32)
    , toDyn (f' :: Int32 -> Int16 -> Int32)
    , toDyn (f' :: Int32 -> Int32 -> Int32)
    , toDyn (f' :: Int32 -> Int64 -> Int32)
    , toDyn (f' :: Int64 -> Int8  -> Int64)
    , toDyn (f' :: Int64 -> Int16 -> Int64)
    , toDyn (f' :: Int64 -> Int32 -> Int64)
    , toDyn (f' :: Int64 -> Int64 -> Int64)
    ]
  where
    f' :: (Bits a, Integral b) => a -> b -> a
    f' x i = f x (fromIntegral i)

zOrd :: (forall a. Ord a => a -> a -> Bool) -> Dynamic
zOrd f = mconcat [
      toDyn (f :: ()     -> ()     -> Bool)
    , toDyn (f :: Bit    -> Bit    -> Bool)
    , toDyn (f :: Bool   -> Bool   -> Bool)
    , toDyn (f :: Int8   -> Int8   -> Bool)
    , toDyn (f :: Int16  -> Int16  -> Bool)
    , toDyn (f :: Int32  -> Int32  -> Bool)
    , toDyn (f :: Int64  -> Int64  -> Bool)
    , toDyn (f :: Double -> Double -> Bool)
    , toDyn (f :: String -> String -> Bool)
    ]

zBinOp :: BinOp -> Dynamic
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
zBinOp And   = toDyn (&&)
zBinOp Or    = toDyn (||)

{-------------------------------------------------------------------------------
  Similar for unary operators
-------------------------------------------------------------------------------}

zNum1 :: (forall a. Num a => a -> a) -> Dynamic
zNum1 f = mconcat [
      toDyn (f :: Int8   -> Int8)
    , toDyn (f :: Int16  -> Int16)
    , toDyn (f :: Int32  -> Int32)
    , toDyn (f :: Int64  -> Int64)
    , toDyn (f :: Double -> Double)
    ]

zBits1 :: (forall a. Bits a => a -> a) -> Dynamic
zBits1 f = mconcat [
      toDyn (f :: Bit   -> Bit)
    , toDyn (f :: Bool  -> Bool)
    , toDyn (f :: Int8  -> Int8)
    , toDyn (f :: Int16 -> Int16)
    , toDyn (f :: Int32 -> Int32)
    , toDyn (f :: Int64 -> Int64)
    ]

zCast :: Typeable a
      => Maybe (()     -> a)
      -> Maybe (Bit    -> a)
      -> Maybe (Bool   -> a)
      -> Maybe (Int8   -> a)
      -> Maybe (Int16  -> a)
      -> Maybe (Int32  -> a)
      -> Maybe (Int64  -> a)
      -> Maybe (Double -> a)
      -> Maybe (String -> a)
      -> Dynamic
zCast fUnit fBit fBool fInt8 fInt16 fInt32 fInt64 fDouble fString =
    mconcat . catMaybes $ [
        fmap toDyn fUnit
      , fmap toDyn fBit
      , fmap toDyn fBool
      , fmap toDyn fInt8
      , fmap toDyn fInt16
      , fmap toDyn fInt32
      , fmap toDyn fInt64
      , fmap toDyn fDouble
      , fmap toDyn fString
      ]

-- | Semantics for unary operators
--
-- TODO: The matrix for cast may be incomplete
zUnOp :: UnOp -> Dynamic
zUnOp NatExp    = error "NatExp not implemented"
zUnOp Neg       = zNum1 negate
zUnOp Not       = toDyn not
zUnOp BwNeg     = zBits1 complement
zUnOp ALength   = error "zUnOp: ALength is polymorphic"
zUnOp (Cast ty) = case ty of
            -- Source: ()          Bit         Bool        Int8        Int16       Int32       Int64       Double       String
    TUnit     -> zCast (Just id)   (Just cu)   (Just cu)   (Just cu)   (Just cu)   (Just cu)   (Just cu)   (Just cu)    (Just cu)
    TBit      -> zCast Nothing     (Just id)   (Just Bit)  Nothing     Nothing     Nothing     Nothing     Nothing      Nothing
    TBool     -> zCast Nothing     (Just bit)  (Just id)   Nothing     Nothing     Nothing     Nothing     Nothing      Nothing
    TInt BW8  -> zCast Nothing     (Just ei)   (Just ei)   (Just id)   (Just fi)   (Just fi)   (Just fi)   (Just round) Nothing
    TInt BW16 -> zCast Nothing     (Just ei)   (Just ei)   (Just fi)   (Just id)   (Just fi)   (Just fi)   (Just round) Nothing
    TInt BW32 -> zCast Nothing     (Just ei)   (Just ei)   (Just fi)   (Just fi)   (Just id)   (Just fi)   (Just round) Nothing
    TInt BW64 -> zCast Nothing     (Just ei)   (Just ei)   (Just fi)   (Just fi)   (Just fi)   (Just id)   (Just round) Nothing
    TDouble   -> zCast Nothing     Nothing     Nothing     (Just fi)   (Just fi)   (Just fi)   (Just fi)   (Just id)    Nothing
    TString   -> zCast (Just show) (Just show) (Just show) (Just show) (Just show) (Just show) (Just show) (Just show)  (Just id)
    _         -> error "zUnOp: Invalid target type"
  where
    cu :: a -> ()
    cu = const ()

    ei :: (Enum a, Num b) => a -> b
    ei = fromIntegral . fromEnum

    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

{-------------------------------------------------------------------------------
  Conversion between expressions and dynamic values
-------------------------------------------------------------------------------}

valueToDyn :: Value -> Maybe Dynamic
valueToDyn (ValueScalar _ ty val) = Just (scalarToDyn ty val)
valueToDyn _                      = Nothing

dynToValue :: Maybe SourcePos -> Dynamic -> Maybe Value
dynToValue p dyn = uncurry (ValueScalar p) <$> dynToScalar dyn

scalarToDyn :: Ty -> Val -> Dynamic
scalarToDyn TUnit       VUnit       = toDyn ()
scalarToDyn TBit        (VBit b)    = toDyn (Bit b)
scalarToDyn TBool       (VBool b)   = toDyn b
scalarToDyn TString     (VString s) = toDyn s
scalarToDyn (TInt BW8)  (VInt i)    = toDyn (fromInteger i :: Int8)
scalarToDyn (TInt BW16) (VInt i)    = toDyn (fromInteger i :: Int16)
scalarToDyn (TInt BW32) (VInt i)    = toDyn (fromInteger i :: Int32)
scalarToDyn (TInt BW64) (VInt i)    = toDyn (fromInteger i :: Int64)
scalarToDyn TDouble     (VDouble d) = toDyn d
scalarToDyn _           _           = error "scalarToDyn: type error"

dynToScalar :: Dynamic -> Maybe (Ty, Val)
dynToScalar (Dynamic [(ty, x)]) = do val <- dynToScalar' ty x
                                     return (ty, val)
dynToScalar _                   = Nothing

-- | Internal auxiliary
dynToScalar' :: Ty -> Any -> Maybe Val
dynToScalar' ty val = case ty of
    TUnit     -> Just $ VUnit
    TBit      -> Just $ VBit    (bit (unsafeCoerce val))
    TBool     -> Just $ VBool   (unsafeCoerce val)
    TString   -> Just $ VString (unsafeCoerce val)
    TInt BW8  -> Just $ VInt    (toInteger (unsafeCoerce val :: Int8))
    TInt BW16 -> Just $ VInt    (toInteger (unsafeCoerce val :: Int16))
    TInt BW32 -> Just $ VInt    (toInteger (unsafeCoerce val :: Int32))
    TInt BW64 -> Just $ VInt    (toInteger (unsafeCoerce val :: Int64))
    TDouble   -> Just $ VDouble (unsafeCoerce val)
    _         -> Nothing

{-------------------------------------------------------------------------------
  Guessing (for satisfiability checking)
-------------------------------------------------------------------------------}

-- | Guess the value for expressions
--
-- See http://www.well-typed.com/blog/2014/12/simple-smt-solver/ for a
-- discussion of this approach.
guess :: Exp -> Eval Evald
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
