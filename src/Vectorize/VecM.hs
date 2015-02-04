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

import Text.Parsec.Pos ( SourcePos )

import AstComp
import AstExpr
import AstFM
import AstUnlabelled
import qualified AstLabelled as AstL

import CardAnalysis
import CtComp
import Utils 

import Outputable
import Text.PrettyPrint.HughesPJ

-- import Data.List 
import Data.Maybe ( fromJust )

import qualified Data.Map as Map


import System.Exit

import qualified GenSym as GS


{-------------------------------------------------------------------------
  Vectorizer monad
-------------------------------------------------------------------------}


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

runVecM :: VecEnv -> VecM a -> IO a
runVecM venv (VecM act) = runReaderT act venv

vecMFail :: Maybe SourcePos -> Doc -> VecM a
vecMFail loc msg
  = liftIO $ 
    do print $ vcat [ 
           text "Vectorization failure!"
         , text "Reason  :" <+> msg 
         , text "Location:" <+> text (maybe (error "BUG") show loc) ]
       exitFailure


{-------------------------------------------------------------------------
  Generating names
-------------------------------------------------------------------------}

newVectUniq :: VecM String
newVectUniq = liftIO . GS.genSymStr =<< asks venv_sym 

newVectGName :: String -> ty -> Maybe SourcePos -> MutKind -> VecM (GName ty)
newVectGName nm ty loc mk = do
  str <- newVectUniq
  return $ (toName (nm++"_"++str) loc ty mk) {uniqId = MkUniq ("_v"++str)}

{-------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------}

getVecEnv :: VecM VecEnv
getVecEnv = ask

extendCVarBind :: CId 
               -> LComp -> VecM a -> VecM a
extendCVarBind nm comp = local add_bind
  where 
    new_bnd = (nm,comp)
    add_bind env = env { venv_cvar_binds = new_bnd : venv_cvar_binds env }
                  
extendCFunBind :: CId
               -> [GName (CallArg Ty CTy)]
               -> LComp
               -> VecM a -> VecM a
extendCFunBind nm params cbody = local add_bind
  where 
    new_bnd = (nm, CFunBind params cbody)
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


{-------------------------------------------------------------------------
  Vectorizer results and data structures
-------------------------------------------------------------------------}

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
        vect_in_ty  :: Ty   -- same asoriginal in_ty
      , vect_out_ty :: Ty } -- same as original out_ty 

     -- Vectorization did happen
   | DidVect { 
        vect_in_ty  :: Ty       -- final input type
      , vect_out_ty :: Ty       -- final output type
      , vect_util   :: Double } -- utility, see Note [Utility]

 deriving Show

mkNotVect :: Ty -> Ty -> VectRes
mkNotVect in_ty yld_ty = NotVect in_ty yld_ty 

mkDidVect :: Ty -> Ty -> Double -> VectRes
mkDidVect in_ty yld_ty u = DidVect in_ty yld_ty u

-- | Utility of any vectorization result
vResUtil :: VectRes -> Double
vResUtil (DidVect _ _ u) = u
vResUtil (NotVect {})    = minUtil

didVect :: VectRes -> Bool
didVect (DidVect {}) = True
didVect _            = False 

-- | Are these results (about the same term) the same (modulo utility)?
vResEqQ :: VectRes -> VectRes -> Bool
vResEqQ (NotVect {}) (NotVect {}) = True
vResEqQ (DidVect ity  oty  _u1)
        (DidVect ity' oty' _u2) = ity == ity' && oty == oty'
vResEqQ _ _ = False


-- | Delayed vectorization result. Hides a DVR but we don't yet
-- execute the action to rewrite a term but rather keep it as a
-- thunk. Hence we can manipulate thousands of results without
-- actually having done the rewriting.
data DelayedVectRes
   = DVR { dvr_comp :: IO Comp   -- Delayed result
         , dvr_vres :: VectRes } -- Information about the result

-- | Lift an operation on VectRes to be on a DelayedVectRes
lift_dvr :: (VectRes -> a) -> DelayedVectRes -> a
lift_dvr f = f . dvr_vres

-- | Utility of delayed vectorization
dvResUtil :: DelayedVectRes -> Double
dvResUtil = lift_dvr vResUtil


updDVRComp :: (Comp -> Comp) -> DelayedVectRes -> DelayedVectRes
updDVRComp f dvr = dvr { dvr_comp = f <$> dvr_comp dvr }

forceDVR :: DelayedVectRes -> VecM Comp
forceDVR dvr = liftIO $ dvr_comp dvr


{- Note [Vectorizer Candidates]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   We need to keep groups of candidates indexed by type, and for each
   group we want a heap that allows fast access to the one with the
   maximum utility.  For this reason we introduce DVRQueues that
   represent the key queue types in this map, holding Heaps ordered by
   the utility of each item. The DVRCands type is used to hold those
   candidates.
-}

-- | Input and output type of vectorization result
type DVRQueues = (Ty,Ty) 

dvrQueueDVR :: DelayedVectRes -> (Ty,Ty)
dvrQueueDVR dvr = (inty,yldty)
  where inty  = vect_in_ty  (dvr_vres dvr)
        yldty = vect_out_ty (dvr_vres dvr)

-- | A non-empty list keeping track of the largest element separately
data MList a = MList { ml_max :: a, ml_rest :: [a] }

class Leq a where
  leq :: a -> a -> Bool


mlSingle :: a -> MList a 
mlSingle x = MList x []

mlInsert :: Leq a => a -> MList a -> MList a
mlInsert x (MList m rest)
  | m `leq` x = MList x (m:rest)
  | otherwise = MList m (x:rest)

mlMax :: MList a -> a
mlMax (MList m _) = m

mlUnion :: Leq a => MList a -> MList a -> MList a
mlUnion (MList m1 r1) (MList m2 r2)
  | m1 `leq` m2 = MList m2 (m1 : r1 ++ r2)
  | otherwise   = MList m1 (m2 : r1 ++ r2)

--  | Precondition: f does not change ordering
mapMList :: (a -> b) -> MList a -> MList b
mapMList f (MList m rest) = MList (f m) (map f rest)


-- | Type-indexed set of utility-sorted heaps
type DVRCands = Map.Map DVRQueues (MList DelayedVectRes)

instance Leq DelayedVectRes where
  dvr1 `leq` dvr2 = dvResUtil dvr1 <= dvResUtil dvr2


addDVR :: DelayedVectRes -> DVRCands -> DVRCands
addDVR dvr cands = Map.insertWith mlUnion (dvrQueueDVR dvr) ml cands
 where ml = mlSingle dvr

addDVRMany :: [DelayedVectRes] -> DVRCands -> DVRCands
addDVRMany [] cs     = cs
addDVRMany (r:rs) cs = addDVRMany rs (addDVR r cs)

fromListDVRCands :: [DelayedVectRes] -> DVRCands
fromListDVRCands rs = addDVRMany rs emptyDVRCands

unionDVRCands :: DVRCands -> DVRCands -> DVRCands
unionDVRCands = Map.unionWith mlUnion 


mapDVRCands :: (DelayedVectRes -> DelayedVectRes) -> DVRCands -> DVRCands
mapDVRCands f = Map.map (mapMList f) 

foldDVRCands :: (s -> DelayedVectRes -> s) -> s -> DVRCands -> s
foldDVRCands f = Map.fold (\r s -> mlFold f s r)

mlFold :: (s -> a -> s) -> s -> MList a -> s
mlFold f s (MList m1 ms) = foldl f s (m1:ms)


emptyDVRCands :: DVRCands
emptyDVRCands = Map.empty

singleDVRCands :: DelayedVectRes -> DVRCands
singleDVRCands dvr = dvr `addDVR` emptyDVRCands

-- | Get the maximal candidate
getMaximal :: DVRCands -> DelayedVectRes
getMaximal = fromJust . Map.fold aux Nothing 
  where aux hp Nothing = return $ mlMax hp
        aux hp (Just dvr) 
          | let dvr' = mlMax hp 
          = if dvr `leq` dvr' then return dvr' else return dvr


{-------------------------------------------------------------------------
  Vectorization utility calculations
-------------------------------------------------------------------------}

{- Note [Utility] 
   ~~~~~~~~~~~~~~
   We associate with each vectorization an utility, represented as a
   Double. We provide below ways to calculate utilities through Bind and
   Par. It should be easy to modify these functions and experiment with
   different utilities.
-} 

-- | Utility computation of a Bind/Seq or a Branch.
--   Preconditions: 
--     a) all input types are joinable 
--     b) all output types are joinable
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
chooseParUtility vr1 vr2 = parUtility u1 u2 tmid
  where 
    u1   = vResUtil vr1 
    u2   = vResUtil vr2 
    tmid = ctJoin (vect_out_ty vr1) (vect_in_ty vr2)

util :: Ty -> Double 
util (TArray (Literal n) _) = log $ fromIntegral n
util _ = minUtil

parUtility :: Double -> Double -> Ty -> Double
parUtility u1 u2 tmid = u1 + u2 + util tmid

minUtil :: Double
minUtil = log 0.1

-- | Choose a VectRes in case all types are joinable (precondition)
vResMatch :: [VectRes] -> VectRes
vResMatch vs = fromJust $ do 
  inty  <- ctJoinMany_mb $ map vect_in_ty vs
  yldty <- ctJoinMany_mb $ map vect_out_ty vs
  -- We have ensured precondition of chooseBindUtility
  let u = chooseBindUtility vs 
      any_vect = any didVect vs
  if any_vect 
   then return $ DidVect inty yldty u
   else return $ NotVect inty yldty


{-------------------------------------------------------------------------
  Vectorization and types
-------------------------------------------------------------------------}

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
-- Not vectorizing arrays. Two reasons for this:
-- (a) It is quite likely that if a programmer is using arrays
--     already in his streams then he has a good understanding of the
--     cost model of his application and it may not make sense for the
--     compiler to try and change this.
-- (b) It is also slightly more tedious to implement, since we have to
--     flatten nested arrays, copy out sub-arrays etc. Doable but tedious.
-- To summarize, the benefits are quite questionable to justify the
-- implementation effort.
isVectorizable :: Ty -> Bool
isVectorizable ty
  | TArray {} <- ty
  = False 
  | otherwise = True


-- | Computes least array or primitive type from the two, used to 
-- determine if we could insert mitigator. NB: We don't generate 
-- arrays of length 1
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
  | t == t' = t
  | otherwise
  = panic $ text "gcd_ty: types are non-joinable!" <+> 
            ppr t <+> text "and" <+> ppr t'

-- | Precondition: non-empty list
gcdTys :: [Ty] -> Ty
gcdTys xs = go (head xs) (tail xs)
  where go t []       = t 
        go t (t1:t1s) = go (gcd_ty t t1) t1s


{-------------------------------------------------------------------------
  Matching on the data path
-------------------------------------------------------------------------}

-- | Match vectorization candidates composed on the data path
combineData :: ParInfo  -> Maybe SourcePos
            -> DVRCands -> DVRCands -> DVRCands
combineData p loc xs ys = Map.fold foreach_xs emptyDVRCands xs
  where
    foreach_xs heap res = Map.fold foreach_ys res ys
      where foreach_ys heap' res'
                = maybe res' (`addDVR` res') $
                do let maxi  = mlMax heap
                       maxi' = mlMax heap'
                   combine_par p loc maxi maxi'

combine_par :: ParInfo -> Maybe SourcePos
            -> DelayedVectRes -> DelayedVectRes -> Maybe DelayedVectRes
combine_par pnfo loc dp1 dp2 = do
  vres <- mk_vres (dvr_vres dp1) (dvr_vres dp2)
  return $ DVR { dvr_comp = iopar, dvr_vres = vres }
  where
    ioc1  = dvr_comp dp1
    ioc2  = dvr_comp dp2 
    iopar = do c1 <- ioc1 
               c2 <- ioc2 
               return $ cPar loc pnfo c1 c2
    -- Non-joinable => just fail
    mk_vres r1 r2
      | Nothing <- ctJoin_mb (vect_out_ty r1) (vect_in_ty r2)
      = Nothing  
    -- Joinable and both NotVect => NotVect
    mk_vres (NotVect t1 _t2) (NotVect _s1 s2) = return $ NotVect t1 s2
    -- Joinable and one DidVect => Didvect 
    mk_vres r1 r2 
      = let u = chooseParUtility r1 r2 
        in return $ DidVect (vect_in_ty r1) (vect_out_ty r2) u 


{-------------------------------------------------------------------------
  Matching on the control path
-------------------------------------------------------------------------}

-- | In analogy to combine_par above this accepts a candidate for the
-- first element of a bind, the bound variables and candidates for the
-- continuations and potentially stitches those together.
-- NB: Never fails since originally the program used to work and we can
-- always mitigate input and output.
combine_bind :: Maybe SourcePos
             -> DelayedVectRes -> [EId] -> [DelayedVectRes] -> DelayedVectRes
combine_bind loc pre_dvr1 xs pre_dvrs
  = DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            cs <- mapM dvr_comp dvrs
            return $ cBindMany loc c1 (zip xs cs)
        , dvr_vres = vResMatch $ map dvr_vres (dvr1:dvrs) }
  where (dvr1:dvrs) = mitigate_all (pre_dvr1:pre_dvrs)

combine_branch :: Maybe SourcePos
               -> Exp -> DelayedVectRes -> DelayedVectRes -> DelayedVectRes
combine_branch loc e pre_dvr1 pre_dvr2
  = DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            c2 <- dvr_comp dvr2 
            return $ cBranch loc e c1 c2
        , dvr_vres = vResMatch $ map dvr_vres [dvr1,dvr2] }
  where
     [dvr1,dvr2] = mitigate_all [pre_dvr1,pre_dvr2]

mitigate_all :: [DelayedVectRes] -> [DelayedVectRes]
mitigate_all vcs = map (mit_one (Just final_ty_in, Just final_ty_out)) vcs
 where 
   -- Compute "the" common input type and "the" common output type
   final_ty_in  = tyArity $ gcdTys $ map (vect_in_ty  . dvr_vres) vcs
   final_ty_out = tyArity $ gcdTys $ map (vect_out_ty . dvr_vres) vcs

--    -- Mitigate from the expected input type (exp_tin) and to the 
--    -- expected output type (exp_tout)
--    mitigate_one exp_tin exp_tout dvr
--      = dvr { dvr_comp = new_dvr_comp, dvr_vres = new_dvr_vres }
--      where
--        vres  = dvr_vres dvr
--        vtin  = vect_in_ty vres
--        vtout = vect_out_ty vres
--        new_dvr_comp = do
--           comp <- dvr_comp dvr
--           return $ (exp_tin,vtin) `mitIn` comp `mitOut` (vtout,exp_tout)
--        -- Do not touch utility (since we do not want to increase it!)
--        new_dvr_vres = vres { vect_in_ty  = exp_tin, vect_out_ty = exp_tout }


-- -- | Mitigate by creating a Mitigate node (maybe) node between 
-- --   tin1 ~> tin2
-- -- Pre-condition: tin1 is a ``type divisor'' of tin2 or vice-versa.
-- -- Result is a mitigating (ST T tin1 tin2) transformer.  
-- mkMit :: Maybe SourcePos -> Ty -> Ty -> Maybe Comp 
-- mkMit loc tin1 tin2
--     -- If the two types are equal or Void no need for mitigation
--   | tin1 == tin2  = Nothing
--   | TVoid <- tin1 = Nothing
--   | TVoid <- tin2 = Nothing
--     -- If one is array but other non-array then the latter must be 
--     -- the base type of the former.  
--     -- We must up-mitigate: 1 ~> len
--   | not (isArrayTy tin1)
--   , TArray (Literal len) tbase <- tin2
--   = assert "mkMit" (tbase == tin1) $ 
--     return $ cMitigate loc tbase 1 len
--     -- Symmetric to previous case 
--   | not (isArrayTy tin2)
--   , TArray (Literal len) tbase <- tin1
--   = assert "mkMit" (tbase == tin2) $
--     return $ cMitigate loc tbase len 1
--     -- If both types are arrays (of different lenghts) let's mitigate 
--   | TArray (Literal len1) tbase1 <- tin1
--   , TArray (Literal len2) tbase2 <- tin2
--   = assert "mkMit" (tbase1 == tbase2) $
--     return $ cMitigate loc tbase1 len1 len2
--   | otherwise
--   = panic $ text "mkMit: can't mitigate:" <+> 
--             ppr tin1 <+> text "~>" <+> ppr tin2

-- -- | (gty,ty) `mitIn` comp
-- -- Mitigates in the input type of comp
-- mitIn :: (Ty,Ty) -> Comp -> Comp
-- mitIn (gty,ty) comp
--   | let loc = compLoc comp
--   , Just m <- mkMit loc gty ty
--   = cPar loc pnever m comp
--   | otherwise = comp

-- -- | comp `mitOut` (ty,gty)
-- -- Mitigates on the output type of comp, symmetrically to `mitIn' above.
-- mitOut :: Comp -> (Ty,Ty) -> Comp 
-- mitOut comp (ty,gty)
--   | let loc = compLoc comp
--   , Just m <- mkMit loc ty gty
--   = cPar loc pnever comp m
--   | otherwise = comp

mit_one :: (Maybe Int, Maybe Int) -> DelayedVectRes -> DelayedVectRes
mit_one (Nothing, Nothing) dvr = fixup_util_out (fixup_util_in dvr)
mit_one (Just n,  Nothing) dvr = fixup_util_out (mit_in n dvr)
mit_one (Nothing, Just n)  dvr = fixup_util_in  (mit_out dvr n)
mit_one (Just n, Just m)   dvr = mit_in n (mit_out dvr m)

mit_in :: Int -> DelayedVectRes -> DelayedVectRes
mit_in n (DVR { dvr_comp = io_comp, dvr_vres = vres })
  | Just (final_in_ty, cmit) <- mitin
  = -- trace ("mit_in:" ++ show final_in_ty ++ " ~~> " ++ show vinty ++ " ~~> " ++ show voutty) $ 
    DVR { dvr_comp = cPar Nothing pnever cmit <$> io_comp
        , dvr_vres = DidVect final_in_ty voutty u }
  where vinty  = vect_in_ty vres  -- type in the middle!
        voutty = vect_out_ty vres
        mitin  = mk_in_mitigator n vinty
        u      = parUtility minUtil (vResUtil vres) vinty
mit_in _ dvr = fixup_util_in dvr

-- | Fixes-up utility /as if/ mitigation has happened
-- This is to ensure all candidates are compared fairly, whether mitigation happened or not.
fixup_util_in :: DelayedVectRes -> DelayedVectRes
fixup_util_in dvr = dvr { dvr_vres = DidVect vinty voutty u } 
  where vres   = dvr_vres dvr
        vinty  = vect_in_ty vres
        voutty = vect_out_ty vres
        u = parUtility minUtil (vResUtil vres) vinty

mit_out :: DelayedVectRes -> Int -> DelayedVectRes
mit_out (DVR { dvr_comp = io_comp, dvr_vres = vres }) m 
  | Just (final_out_ty, cmit) <- mitout
  = -- trace ("mit_out:" ++ show vinty ++ " ~~> " ++ show voutty ++ " ~~> " ++ show final_out_ty) $ 
    DVR { dvr_comp = (\c -> cPar Nothing pnever c cmit) <$> io_comp
        , dvr_vres = DidVect vinty final_out_ty u }
  where vinty  = vect_in_ty vres
        voutty = vect_out_ty vres -- type in the middle!
        mitout = mk_out_mitigator voutty m
        u      = parUtility minUtil (vResUtil vres) voutty
mit_out dvr _ = fixup_util_out dvr

fixup_util_out :: DelayedVectRes -> DelayedVectRes
fixup_util_out dvr = dvr { dvr_vres = DidVect vinty voutty u } 
  where vres   = dvr_vres dvr
        vinty  = vect_in_ty vres
        voutty = vect_out_ty vres
        u = parUtility minUtil (vResUtil vres) voutty

-- | Mitigate on the input, return final input type
mk_in_mitigator :: Int -> Ty -> Maybe (Ty, Comp)
mk_in_mitigator n (TArray (Literal m) tbase) 
  | n > 1 || m > 1
  , n `mod` m == 0 || m `mod` n == 0
  = Just (mkVectTy tbase n, cMitigate Nothing tbase n m)
  | otherwise = Nothing 
mk_in_mitigator _n TVoid = Nothing -- nothing to mitigate
mk_in_mitigator n t -- non-array
  | n > 1     = Just (mkVectTy t n, cMitigate Nothing t n 1)
  | otherwise = Nothing 

-- | Mitigate on the output, return final output type
mk_out_mitigator :: Ty -> Int -> Maybe (Ty, Comp)
mk_out_mitigator (TArray (Literal n) tbase) m
  | n > 1 || m > 1 
  , n `mod` m == 0 || m `mod` n == 0
  = Just (mkVectTy tbase m, cMitigate Nothing tbase n m)
  | otherwise = Nothing
mk_out_mitigator TVoid _m = Nothing -- nothing to mitigate
mk_out_mitigator t m -- non-array
  | m > 1     = Just (mkVectTy t m, cMitigate Nothing t 1 m)
  | otherwise = Nothing

        

-- | Match vectorization candidates composed on the control path
combineBind :: Maybe SourcePos -> DVRCands -> [EId] -> [DVRCands] -> DVRCands 
combineBind loc c1cands xs cscands 
  = aux (c1cands:cscands) (\(r:rs) -> combine_bind loc r xs rs)
  where 
     aux :: [DVRCands] -> ([DelayedVectRes] -> DelayedVectRes) -> DVRCands
     aux cs h = go cs h 
       where go [] f       = singleDVRCands (f [])
             go (c1:c1s) f = Map.fold (\c st ->
                   let m     = mlMax c
                       f' rs = f (m : rs)
                   in go c1s f' `unionDVRCands` st) emptyDVRCands c1

combineBranch :: Maybe SourcePos -> Exp -> DVRCands -> DVRCands -> DVRCands
combineBranch loc e xs ys = Map.fold foreach_xs emptyDVRCands xs
 where
   foreach_xs heap res = Map.fold foreach_ys res ys
      where 
        foreach_ys heap' res' 
          = maybe res' (`addDVR` res') $
            do let maxi  = mlMax heap
               let maxi' = mlMax heap'
               return $ combine_branch loc e maxi maxi'



-- -- | Build an array type from a base type (like VecM.mkVectTy)
-- array_ty :: Int -> Ty -> Ty
-- array_ty 1 ty = ty
-- array_ty n ty = TArray (Literal n) ty 

{- 

-- | Keep a result with maximal utility for each group by vResEqQ
keepGroupMaximals :: [DelayedVectRes] -> [DelayedVectRes]
keepGroupMaximals xs = map getMaximal (groups xs)
  where groups = groupBy' $ vResEqQ <| dvr_vres

-- | Return a result with maximal utility
getMaximal :: [DelayedVectRes] -> DelayedVectRes
getMaximal = maximumBy $ compare <| dvResUtil 

-}




{-------------------------------------------------------------------------
  Matching on the control path
-------------------------------------------------------------------------}

-- | Match vectorization candidates composed on the control path (bind).
--
-- For a set of components c1..cn this function accepts the
-- vectorization candidates for each ci and creates the set of
-- vectorization candidates for matching them in the control path. For
-- example:
-- 
-- [ [vc11,vc12], [vc21,vc22] ] 
--     ~~>
-- [ [vc11,vc21], [vc11,vc22], [vc12,vc21], [vc11,vc22] ]
-- 
-- assuming that for each group in the resulting list the input types
-- of all components are joinable and the output types are also all
-- joinable.
-- 
-- NB: 'cross_prod_mit' may introduce mitigators to make sure that
-- queues match.  These mitigate between the ``greatest common
-- divisor'' type of all input types and each individual input type,
-- and similarly between each individual output type and the
-- ``greatest common divisor'' type of all output types.  See VecM.hs
-- for the definition of gcdTys.
-- cross_prod_mit :: [ [DelayedVectRes] ] -> [ [DelayedVectRes] ]
-- cross_prod_mit bcands 
--   = map mitigate $ Utils.cross_prod bcands 
--   where
--     mitigate :: [DelayedVectRes] -> [DelayedVectRes]
--     mitigate vcs = map (mitigate_one ty_in ty_out) vcs
--       -- Compute "the" common input type and "the" common output type
--       where ty_in  = gcdTys $ map (vect_in_ty  . dvr_vres) vcs
--             ty_out = gcdTys $ map (vect_out_ty . dvr_vres) vcs
--     -- Mitigate from the expected input type (exp_tin) and to the 
--     -- expected output type (exp_tout)
--     mitigate_one exp_tin exp_tout dvr
--       = dvr { dvr_comp = new_dvr_comp, dvr_vres = vres_new } 
--       where 
--        vect_tin     = vect_in_ty  $ dvr_vres dvr
--        vect_tout    = vect_out_ty $ dvr_vres dvr
--        new_dvr_comp = do
--           comp <- dvr_comp dvr
--           return $ 
--             (exp_tin,vect_tin) `mitIn` comp `mitOut` (vect_tout,exp_tout)
--        vres_new = (dvr_vres dvr) { vect_in_ty  = exp_tin
--                                  , vect_out_ty = exp_tout }



-- combineCtrl :: Maybe SourcePos
--             -> DelayedVectRes
--             -> [EId] -> [DelayedVectRes]
--             -> DelayedVectRes
-- combineCtrl loc dvr1 xs dvrs
--   = DVR { dvr_comp = do
--             c1 <- dvr_comp dvr1
--             cs <- mapM dvr_comp dvrs
--             return $ cBindMany loc c1 (zip xs cs)
--         , dvr_vres = bind_vres (dvr1:dvrs) }
--   where
--      bind_vres ds
--        | [] <- filter (didVect . dvr_vres) ds
--        = NotVect vtin vtout
--        | otherwise
--        = DidVect vtin vtout u
--        where
--           vds   = map dvr_vres ds
--           u     = chooseBindUtility vds
--           vtin  = fromJust $ ctJoinMany_mb $ map vect_in_ty  vds
--           vtout = fromJust $ ctJoinMany_mb $ map vect_out_ty vds




{-------------------------------------------------------------------------
  Interpret Ziria computations and AST manipulation 
-------------------------------------------------------------------------}

mkDVRes :: Bindable v => (VecEnv -> Zr v) 
        -> String
        -> Maybe SourcePos -> Ty -> Ty -> VecM DelayedVectRes
mkDVRes gen_zir kind loc vin_ty vout_ty = do 
  venv <- getVecEnv
  zirc <- liftZr kind loc (gen_zir venv)
  zirc_wrapped <- wrapCFunCall ("_vect_" ++ kind) loc zirc
  return $ DVR { dvr_comp = return zirc_wrapped
               , dvr_vres = DidVect vin_ty vout_ty minUtil }

liftZr :: Bindable v => String -> Maybe SourcePos -> Zr v -> VecM Comp
liftZr pref_dbg loc zr = do 
  sym  <- asks venv_sym
  liftIO $ interpC pref_dbg sym loc zr


-- | Wrap the comp in a new function and call, for debugging purposes.
wrapCFunCall :: String -> Maybe SourcePos -> Comp -> VecM Comp 
wrapCFunCall nm loc comp = do
  let carrty = CTArrow [] (ctComp comp)
  vname <- newVectGName nm carrty loc Imm
  return $ cLetFunC loc vname [] comp (cCall loc vname [])


-- | The cardinality of any Map, obviously 1-1, statically known
map_card :: Card
map_card = SCard (CAStatic 1) (CAStatic 1)

take_card :: Card
take_card = SCard (CAStatic 1) (CAStatic 0)
emit_card :: Card
emit_card = SCard (CAStatic 0) (CAStatic 1)


-- | Expand a Map node to a Repeat node
expandMapToTakeEmit :: Maybe SourcePos 
                    -> Maybe VectAnn -> Ty -> EId -> VecM LComp
expandMapToTakeEmit loc vann inty f = do 
  x <- newVectGName "_map_bnd" inty loc Imm
  return $
    AstL.cRepeat loc OCard vann $ 
         AstL.cBindMany loc map_card tk [(x,em x)]
  where tk   = AstL.cTake1 loc take_card inty
        em x = AstL.cEmit loc emit_card (eCall loc f [eVar loc x])

zERO :: I
zERO = I(0)

