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
{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts #-}
module VecMonad where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Text.Parsec.Pos (SourcePos)

import AstComp
import AstExpr
import AstLabelled
import CardinalityAnalysis
import CtExpr (ctExp)
import qualified GenSym as GS

-- | Both VecM and VecMBnd satisfy VecMonad, and most functions in this
-- module are polymorphic over any monad that satisfies VecMoand
type VecMonad m = ( Functor m
                  , MonadState VecState m
                  , MonadReader (GS.Sym, VecEnv) m
                  , MonadIO m
                  )

{-------------------------------------------------------------------------------
  Vectorizer monad
-------------------------------------------------------------------------------}

-- | Work-in-progress vectorization plan
data VecState = VecState {
    vs_take_count :: Int
  , vs_emit_count :: Int
  }

data CFunBind = CFunBind {
    cfun_params :: [(GName (CallArg Ty CTy))]
  , cfun_locals :: [(GName Ty, Maybe Exp)]
  , cfun_body   :: LComp
  }

data VecEnv = VecEnv {
    cvar_binds :: [(GName CTy, LComp)]
  , cfun_binds :: [(GName CTy, CFunBind)]
  }

newtype VecM a = VecM (ReaderT (GS.Sym, VecEnv) (StateT VecState IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState VecState
           , MonadReader (GS.Sym, VecEnv)
           )

-- TODO: Is this the API we want to offer?
runVecM :: VecM a -> GS.Sym -> VecEnv -> VecState -> IO (a, VecState)
runVecM (VecM act) sym env st = runStateT (runReaderT act (sym, env)) st

inCurrentEnv :: (GS.Sym, VecEnv) -> VecM a -> IO a
inCurrentEnv (sym, venv) act = fst <$> runVecM act sym venv (VecState 0 0)

-- TODO: This is a nasty definition
vecMFail :: String -> VecM a
vecMFail msg = do
    liftIO $ putStrLn (error msg)
    return undefined

{-------------------------------------------------------------------------------
  Generating names
-------------------------------------------------------------------------------}

newVectUniq :: VecMonad m => m String
newVectUniq = liftIO . GS.genSymStr =<< asks fst

newTypedName :: VecMonad m => String -> ty -> Maybe SourcePos -> m (GName ty)
newTypedName nm ty loc = do
    str <- newVectUniq
    return $ (toName (nm ++ "_" ++ str) loc ty) {uniqId = "_v" ++ str}

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

getVecEnv :: VecMonad m => m (GS.Sym, VecEnv)
getVecEnv = ask

extendCVarBind :: VecMonad m => GName CTy -> LComp -> m a -> m a
extendCVarBind nm comp = local $ \(sym, env) -> (sym, env {
      cvar_binds = (nm,comp) : cvar_binds env
    })

extendCFunBind :: VecMonad m
               => GName CTy
               -> [GName (CallArg Ty CTy)]
               -> [(GName Ty, Maybe Exp)]
               -> LComp
               -> m a -> m a
extendCFunBind nm params locals cbody = local $ \(sym, env) -> (sym, env {
      cfun_binds = (nm, CFunBind params locals cbody) : cfun_binds env
    })

getCVarBinds :: VecMonad m => m [(GName CTy, LComp)]
getCVarBinds = asks $ cvar_binds . snd

getCFunBinds :: VecMonad m => m [(GName CTy, CFunBind)]
getCFunBinds = asks $ cfun_binds . snd

lookupCVarBind :: VecMonad m => GName CTy -> m LComp
lookupCVarBind nm = do
    bnds <- getCVarBinds
    case lookup nm bnds of
       Just bnd -> return bnd
       Nothing  -> error "Unbound cvar bind!"

lookupCFunBind :: VecMonad m => GName CTy -> m CFunBind
lookupCFunBind nm = do
    bnds <- getCFunBinds
    case lookup nm bnds of
       Just bnd -> return bnd
       Nothing  -> error "Unbound cfun bind!"

{-------------------------------------------------------------------------------
  Interacting with the state
-------------------------------------------------------------------------------}

incTakeCount :: VecMonad m => m ()
incTakeCount = modify $ \st -> st { vs_take_count = vs_take_count st + 1 }

incEmitCount :: VecMonad m => m ()
incEmitCount = modify $ \st -> st { vs_emit_count = vs_emit_count st + 1 }

getTakeCount :: VecMonad m => m Int
getTakeCount = vs_take_count <$> get

getEmitCount :: VecMonad m => m Int
getEmitCount = vs_emit_count <$> get

setVecState :: VecMonad m => VecState -> m ()
setVecState = put

-- | Run the action in the current state, but in the end return the final
-- state. The VecM state remains as it was in the beginning.
withCurrentCounters :: VecMonad m => m a -> m (a, VecState)
withCurrentCounters act = do
    st  <- get
    a   <- act
    st' <- get
    put st -- Restore original state
    return (a, st')

-- | Reset counters, run the action, and finally restore the state.
withInitCounts :: VecMonad m => m a -> m a
withInitCounts act = do
    st <- get
    put st { vs_take_count = 0, vs_emit_count = 0 }
    a  <- act
    put st -- Restore original state
    return a

{-------------------------------------------------------------------------------
  Bindings
-------------------------------------------------------------------------------}

-- | A thin wrapper around VecM that provides facilities for binding generation
-- and binding collection
newtype VecMBnd a = VecMBnd (WriterT [(GName Ty, Maybe Exp)] VecM a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState VecState
           , MonadReader (GS.Sym, VecEnv)
           , MonadWriter [(GName Ty, Maybe Exp)]
           )

runVecMBnd :: VecMBnd a -> VecM (a, [(GName Ty, Maybe Exp)])
runVecMBnd (VecMBnd act) = runWriterT act

-- | Generate new name and declare it
--
-- (see also newTypedName, which does _not_ declare the new name)
newDeclTypedName :: String
                 -> Ty -> Maybe SourcePos -> Maybe Exp
                 -> VecMBnd (GName Ty)
newDeclTypedName s ty loc me = do
    nm <- newTypedName s ty loc
    tell [(nm, me)]
    return nm

{-------------------------------------------------------------------------------
  Vectorization results
-------------------------------------------------------------------------------}

-- | Vectorization result
--
-- Quick way to get the result arity of vectorization w.o. re type checking.
data VectRes =
     NoVect
   | DidVect { in_card   :: Int
             , out_card  :: Int
             , vect_util :: Double -- The utility of this vectorization
             }
 deriving (Show, Eq, Ord)

-- Accepts 'cin' and 'cout' and it is safe and sound to just give back NoVect.
-- After all we performed no vectorization.
--
-- However, if the input/queue arities are 0 (that is, we never even took or
-- emited) we are in better shape emitting a 'DidVect 0 x' or 'DidVect x 0' so
-- that other components can take advantage of this information.
--
-- In the case where we have cin = 0 and cout = n > 0 we say DidVect 0 *1*.
--
-- NB: *1* is important, it effectively means: we did not vectorize on the
-- output queue.
mkNoVect :: Int -> Int -> VectRes
mkNoVect 0 0 = DidVect 0 0 minUtil
mkNoVect 0 _ = DidVect 0 1 minUtil
mkNoVect _ 0 = DidVect 1 0 minUtil
mkNoVect _ _ = NoVect

vectResQueueEq :: VectRes -> VectRes -> Bool
vectResQueueEq NoVect          NoVect            = True
vectResQueueEq (DidVect i j _) (DidVect i' j' _) = i == i' && j == j'
vectResQueueEq _               _                 = False

vectResQueueComp :: VectRes -> VectRes -> Ordering
vectResQueueComp (DidVect i j _) (DidVect i' j' _) = compare (i,j) (i',j')
vectResQueueComp v               v'                = compare v v'

vectResUtil :: VectRes -> Double
vectResUtil (DidVect _ _ u) = u
vectResUtil NoVect          = minUtil

-- | Utility computation of a Bind
chooseBindUtility :: [Double] -> Double
chooseBindUtility [] = 0.0
chooseBindUtility ds = (sum ds :: Double) / (fromIntegral (length ds) :: Double)

-- Utility computation of a Par
chooseParUtility :: Double -> Double -> (Int,Int) -> Double
chooseParUtility d1 d2 (middle1,middle2) = d1 + d2 + util middle1 + util middle2
  where
    util :: Int -> Double
    util 0 = minUtil
    util d = log (fromIntegral d)

minUtil :: Double
minUtil = log 0.1

allVecResMatch :: [VectRes] -> [VectRes]
allVecResMatch vs =
    if all_equal filtered_in && all_equal filtered_out
      then [DidVect (maximum (0:filtered_in)) (maximum (0:filtered_out))
                    (chooseBindUtility (map vectResUtil vs))
           ]
      else []
  where
    filtered_in    = filter (\i -> i > 0) all_in_queues
    filtered_out   = filter (\i -> i > 0) all_out_queues
    all_in_queues  = map get_in_queue  vs
    all_out_queues = map get_out_queue vs
    get_in_queue   = fst . get_queue
    get_out_queue  = snd . get_queue

    get_queue NoVect = (1,1)
    get_queue (DidVect i j _) = (i,j)

    all_equal []          = True
    all_equal [_]         = True
    all_equal (v1:v2:vs') = v1 == v2 && all_equal (v2:vs')

{-------------------------------------------------------------------------------
  Vectorization and types
-------------------------------------------------------------------------------}

-- | Give back a vectorized version of a type
mkVectTy :: Ty -> Int -> Ty
mkVectTy ty wdth
    | not (isVectorizable ty)
    = error ("Type not vectorizable:" ++ show ty)

    -- Specialization below implemented in the code generator, not here
    -- | ty == TBit && wdth <= 8
    -- = TWord8
    -- | ty == TBit && wdth <= 32
    -- = TWord32

    | wdth == 1
    = ty

    | otherwise
    = TArray (Literal wdth) ty

-- | When is a type vectorizable
isVectorizable :: Ty -> Bool
isVectorizable ty
    | isScalarTy ty = True
    | TBit <- ty    = True -- Bits are vectorizable all right
    | otherwise     = False

isBufTy :: Ty -> Bool
isBufTy (TBuff {}) = True
isBufTy _          = False

{-------------------------------------------------------------------------------
  Auxiliary definitions for creating new expressions in the vectorizer
-------------------------------------------------------------------------------}

-- TODO: Obsolete (replace use of `eadd` with `(.+)`
eadd :: Exp -> Exp -> Exp
eadd e1 e2 = toExp () (EBinOp Add e1 e2)

-- TODO: Obsolete (replace use of `emul` with `(.*)`
emul :: Exp -> Exp -> Exp
emul e1 e2 = toExp () (EBinOp Mult e1 e2)

-- Creates a for loop from 0 up to the size specified
mkTimes :: Exp -> GName Ty -> LComp -> LComp
mkTimes (MkExp (EVal valTy (VInt 1)) eloc ()) nm comp
  = let ezero = eVal eloc () valTy (VInt 0)
    in head $ substExpComp (nm, ezero) comp
mkTimes elen nm comp
  = let ezero = eVal (expLoc elen) () (ctExp elen) (VInt 0)
    in cTimes (compLoc comp) (compInfo comp) AutoUnroll ezero elen nm comp
