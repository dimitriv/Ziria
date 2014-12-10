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
module VecM where

import Control.Applicative
import Control.Monad.Reader
import Text.Parsec.Pos (SourcePos)

import AstComp
import AstExpr

import Card 
import CtComp
import Utils 

import qualified GenSym as GS


{-------------------------------------------------------------------------------
  Vectorizer monad
-------------------------------------------------------------------------------}


data CFunBind = CFunBind {
    cfun_params :: [(GName (CallArg Ty CTy))]
  , cfun_body   :: LComp
  }

data VecEnv = VecEnv {
    cvar_binds :: [(CId, LComp)]
  , cfun_binds :: [(CId, CFunBind)]
  }

newtype VecM a = VecM (ReaderT (GS.Sym, VecEnv) IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (GS.Sym, VecEnv)
           )

runVecM :: VecM a -> GS.Sym -> VecEnv -> IO a
runVecM (VecM act) sym env
  = runReaderT act (sym, env)


{-------------------------------------------------------------------------------
  Generating names
-------------------------------------------------------------------------------}

newVectUniq :: VecM String
newVectUniq = liftIO . GS.genSymStr =<< asks fst

newVectGName :: String -> ty -> Maybe SourcePos -> VecM (GName ty)
newVectGName nm ty loc = do
    str <- newVectUniq
    return $ (toName (nm ++ "_" ++ str) loc ty) {uniqId = "_v" ++ str}

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

getVecEnv :: VecM VecEnv
getVecEnv = ask >>= \(_,env) -> return env

extendCVarBind :: CId 
               -> LComp -> VecM a -> VecM a
extendCVarBind nm comp 
  = local $ \(sym,env) -> (sym, add_bind env)
  where add_bind env = env { cvar_binds = (nm,comp) : cvar_binds env }
                  
extendCFunBind :: CId
               -> [GName (CallArg Ty CTy)]
               -> LComp
               -> VecM a -> VecM a
extendCFunBind nm params cbody 
  = local $ \(sym, env) -> (sym, add_bind env)
  where new_bnd = (nm, CFunBind params cbody)
        add_bind env = env { cfun_binds = new_bnd : cfun_binds env }

getCVarBinds :: VecM [(CId, LComp)]
getCVarBinds = asks $ cvar_binds . snd

getCFunBinds :: VecM [(CId, CFunBind)]
getCFunBinds = asks $ cfun_binds . snd

lookupCVarBind :: CId -> VecM LComp
lookupCVarBind nm = do
    bnds <- getCVarBinds
    case lookup nm bnds of
       Just bnd -> return bnd
       Nothing  -> panicStr $ "VecM: unbound cvar bind: " ++ show nm

lookupCFunBind :: CId -> VecM CFunBind
lookupCFunBind nm = do
    bnds <- getCFunBinds
    case lookup nm bnds of
       Just bnd -> return bnd
       Nothing  -> panicStr $ "VecM: unbound cfun bind: " ++ show nm


{-------------------------------------------------------------------------------
  Vectorization utilities
-------------------------------------------------------------------------------}

{- Note [Utility] 
   ~~~~~~~~~~~~~~
   We associate with each vectorization an utility, represented as a
   Double. We provide below ways to calculate utilities through Bind and
   Par. It should be easy to modify these functions and experiment with
   different utilities.
-} 

-- | Utility computation of a Bind
-- Precondition: all input types are joinable and all output types are
-- joinable
chooseBindUtility :: [VectRes] -> Double
chooseBindUtility [] = panicStr "chooseBindUtility: empty list"
chooseBindUtility ds = us / len
  where us :: Double 
        us = sum $ map vResUtil ds
        len :: Double 
        len = fromIntegral $ length ds

-- | Utility computation of a Par
-- Precondition: the two vectorization results match, that is, the
-- intermediate types are joinable (with ctJoin)
chooseParUtility :: VectRes -> VectRes -> Double
chooseParUtility vr1 vr2 = u1 + u2 + util tmid 
  where 
    u1   = vResUtil vr1 
    u2   = vResUtil vr2 
    tmid = ctJoin (vect_out_ty vr1) (vect_in_ty vr2)

    util :: Ty -> Double 
    util (TArray (Literal n) _) = log $ fromIntegral n
    util _ = minUtil

minUtil :: Double
minUtil = log 0.1

{-------------------------------------------------------------------------------
  Vectorization results
-------------------------------------------------------------------------------}

{- Note [VectorizationResult]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   A Vectorization Result (VectRes) records whether vectorization
   happened and what are the final input and output types of the
   vectorization result (vect_in_ty and vect_out_ty respectively). 
   It also records the utility of this particular vectorization.
-}


-- | Vectorization result
data VectRes =
     -- No vectorization
     NotVect { 
        vect_in_ty  :: Ty   -- must be equal to original in_ty
      , vect_out_ty :: Ty } -- must be equal to original out_ty 

     -- Vectorization did happen
   | DidVect { 
        vect_in_ty  :: Ty       -- not necessarily equal to original in_ty
      , vect_out_ty :: Ty       -- not necessarily equal to original out_ty
      , vect_util   :: Double } -- utility of this vectorization

 deriving Show

mkNotVect :: Ty -> Ty -> VectRes
mkNotVect in_ty yld_ty = NotVect in_ty yld_ty 

vResUtil :: VectRes -> Double
vResUtil (DidVect _ _ u) = u
vResUtil (NotVect {})    = minUtil

didVect :: VectRes -> Bool
didVect (DidVect {}) = True
didVect _            = False 

-- | Are these two results the same (modulo utility)
vResEqQ :: VectRes -> VectRes -> Bool
vResEqQ (NotVect {}) (NotVect {}) = True 
vResEqQ (DidVect ity  oty  _u1) 
        (DidVect ity' oty' _u2) = ity == ity' && oty == oty'
vResEqQ _ _ = False 


vResMatch :: [VectRes] -> Maybe VectRes
vResMatch vs = do 
  inty  <- ctJoinMany_mb $ map vect_in_ty vs
  yldty <- ctJoinMany_mb $ map vect_out_ty vs
  let u = chooseBindUtility vs 
      any_vect = any didVect vs
  if any_vect 
   then return $ DidVect inty yldty u
   else return $ NotVect inty yldty

{-------------------------------------------------------------------------------
  Vectorization and types
-------------------------------------------------------------------------------}

-- | Give back a vectorized version of a type
-- NB: If wdth == 1 we return exactly the same type, /not/ a 
-- 1-element array. 
-- Precondition: it must be (isVectorizable ty)
mkVectTy :: Ty -> Int -> Ty
mkVectTy ty wdth
    | not (isVectorizable ty)
    = panicStr $ "VecM: non-vectorizable type " ++ show ty
    | wdth == 1 = ty
    | otherwise
    = TArray (Literal wdth) ty

-- | When is a type vectorizable
-- Not vectorizing arrays for the moment (because we do not support
-- arrays of naked arrays and we have not thought enough about
-- coarsening or braking up arrays) but that's about all the
-- restrictions we impose really.
isVectorizable :: Ty -> Bool
isVectorizable ty
    | TArray {} <- ty 
    = False  
    | otherwise
    = True

