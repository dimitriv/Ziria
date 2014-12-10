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

import CardAnalysis
import CtComp
import Utils 

import Outputable
import Text.PrettyPrint.HughesPJ


import qualified GenSym as GS


{-------------------------------------------------------------------------------
  Vectorizer monad
-------------------------------------------------------------------------------}


data CFunBind = CFunBind {
    cfun_params :: [(GName (CallArg Ty CTy))]
  , cfun_body   :: LComp }

data VecEnv = VecEnv {
    venv_sym        :: GS.Sym 
  , venv_cvar_binds :: [(CId, LComp)]
  , venv_cfun_binds :: [(CId, CFunBind)] }


newtype VecM a = VecM (ReaderT VecEnv IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader VecEnv
           )

runVecM :: VecM a -> VecEnv -> IO a
runVecM (VecM act) venv = runReaderT act venv


{-------------------------------------------------------------------------------
  Generating names
-------------------------------------------------------------------------------}

newVectUniq :: VecM String
newVectUniq = liftIO . GS.genSymStr =<< asks venv_sym 

newVectGName :: String -> ty -> Maybe SourcePos -> VecM (GName ty)
newVectGName nm ty loc = do
    str <- newVectUniq
    return $ (toName (nm ++ "_" ++ str) loc ty) {uniqId = "_v" ++ str}

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

getVecEnv :: VecM VecEnv
getVecEnv = ask

extendCVarBind :: CId 
               -> LComp -> VecM a -> VecM a
extendCVarBind nm comp 
  = local $ add_bind
  where 
    add_bind env = env { venv_cvar_binds = (nm,comp) : venv_cvar_binds env }
                  
extendCFunBind :: CId
               -> [GName (CallArg Ty CTy)]
               -> LComp
               -> VecM a -> VecM a
extendCFunBind nm params cbody 
  = local $ add_bind
  where new_bnd = (nm, CFunBind params cbody)
        add_bind env = env { venv_cfun_binds = new_bnd : venv_cfun_binds env }

getCVarBinds :: VecM [(CId, LComp)]
getCVarBinds = asks venv_cvar_binds

getCFunBinds :: VecM [(CId, CFunBind)]
getCFunBinds = asks venv_cfun_binds

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
-- Some notes: 
--   * If wdth == 1 we return exactly the same type ty, not a 1-element array.
--   * If ty = TVoid we do not return an array, just TVoid.
-- Precondition: it must be (isVectorizable ty). TVoid isVectorizable.
mkVectTy :: Ty -> Int -> Ty
mkVectTy ty wdth
  | not (isVectorizable ty)
  = panicStr $ "VecM: non-vectorizable type " ++ show ty
  | wdth == 1 
  = ty
  | TVoid <- ty
  = TVoid
  | otherwise
  = TArray (Literal wdth) ty

-- | When is a type vectorizable
-- Not vectorizing arrays for the moment because we do not support
-- arrays of naked arrays we need to think through coarsening and
-- breaking up of arrays (TODO). 
isVectorizable :: Ty -> Bool
isVectorizable ty
  | TArray {} <- ty -- TODO: revisit this
  = False  
  | otherwise = True


{-------------------------------------------------------------------------------
  Vectorization utilities
-------------------------------------------------------------------------------}


-- | Computes least array or primitive type from the two, used for mitigators
-- NB: No arrays of length 1
gcd_ty :: Ty -> Ty -> Ty 
gcd_ty TVoid t = t 
gcd_ty t TVoid = t
gcd_ty (TArray (Literal l1) t1) 
       (TArray (Literal l2) t2)
  = assert "gcd_ty" (t1 == t2) $
    let n = gcd l1 l2 
    in if n > 1 then TArray (Literal n) t1 else t1

gcd_ty t (TArray _ t') = assert "gcd_ty" (t == t') t
gcd_ty (TArray _ t') t = assert "gcd_ty" (t == t') t

gcd_ty t t' 
  = panic $ text "gcd_ty: types are non-joinable!" <+> 
            ppr t <+> text "and" <+> ppr t'

gcdTys :: [Ty] -> Ty
gcdTys xs = go (head xs) (tail xs)
  where go t []       = t 
        go t (t1:t1s) = go (gcd_ty t t1) t1s
