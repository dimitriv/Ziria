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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Ziria.Vectorize.VecM where

import Control.Applicative
import Control.Monad.Reader
import Data.Char ( isAlphaNum )
import Data.Loc
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust )
import System.Exit
import Text.PrettyPrint.HughesPJ

import Ziria.BasicTypes.AstComp
import Ziria.BasicTypes.AstExpr
import Ziria.BasicTypes.AstFM
import Ziria.BasicTypes.AstUnlabelled
import qualified Ziria.BasicTypes.AstLabelled as AstL
import Ziria.BasicTypes.Outputable
import Ziria.ComputeType.CtComp
import qualified Ziria.Utils.GenSym as GS
import Ziria.Utils.Utils 
import Ziria.Vectorize.CardAnalysis

import Opts

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

vecMFail :: SrcLoc -> Doc -> VecM a
vecMFail loc msg
  = liftIO $ 
    do print $ vcat [ 
           text "Vectorization failure!"
         , text "Reason:" 
         , msg 
         , text "Location:" <+> ppr loc ]
       exitFailure


{-------------------------------------------------------------------------
  Generating names
-------------------------------------------------------------------------}

newVectUniq :: VecM String
newVectUniq = liftIO . GS.genSymStr =<< asks venv_sym 

newVectGName :: String -> ty -> SrcLoc -> MutKind -> VecM (GName ty)
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
        vect_in_ty  :: !Ty   -- same asoriginal in_ty
      , vect_out_ty :: !Ty } -- same as original out_ty 

     -- Vectorization did happen
   | DidVect { 
        vect_in_ty  :: !Ty       -- final input type
      , vect_out_ty :: !Ty       -- final output type
      , vect_util   :: !Double } -- utility, see Note [Utility]

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
   = DVR { dvr_comp :: !(IO Comp) -- Delayed result
         , dvr_vres :: !VectRes }  -- Information about the result

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
   group we just keep the maximal candidate.For this reason we introduce
   DVRQueues that represent the key queue types in this map. The DVRCands
  type is used to hold those candidates.

-}

-- | Input and output type of vectorization result
type DVRQueues = (Ty,Ty) 

dvrQueueDVR :: DelayedVectRes -> (Ty,Ty)
dvrQueueDVR dvr = (inty,yldty)
  where inty  = vect_in_ty  (dvr_vres dvr)
        yldty = vect_out_ty (dvr_vres dvr)

-- | Type-indexed set of utility-sorted heaps
type DVRCands = Map.Map DVRQueues DelayedVectRes

max_dvr :: DelayedVectRes -> DelayedVectRes -> DelayedVectRes
max_dvr x y = if dvResUtil x <= dvResUtil y then y else x

addDVR :: DelayedVectRes -> DVRCands -> DVRCands
addDVR dvr = dvr `seq` Map.insertWith max_dvr (dvrQueueDVR dvr) dvr

fromListDVRCands :: [DelayedVectRes] -> DVRCands
fromListDVRCands rs = 
  Map.fromListWith max_dvr (map (\r -> (dvrQueueDVR r, r)) rs)


unionDVRCands :: DVRCands -> DVRCands -> DVRCands
unionDVRCands d1 d2 
  = d1 `seq` d2 `seq` (Map.unionWith max_dvr d1 d2)


mapDVRCands :: (DelayedVectRes -> DelayedVectRes) -> DVRCands -> DVRCands
mapDVRCands f = Map.map f


emptyDVRCands :: DVRCands
emptyDVRCands = Map.empty

dvResDVRCands :: DVRCands -> [VectRes]
dvResDVRCands rs = map dvr_vres  (Map.elems rs)


singleDVRCands :: DelayedVectRes -> DVRCands
singleDVRCands dvr = dvr `addDVR` emptyDVRCands

-- | Get the maximal candidate
getMaximal :: DVRCands -> DelayedVectRes
getMaximal = fromJust . Map.foldr aux Nothing 
  where aux hp Nothing = return hp
        aux hp (Just dvr) 
          | let dvr' = hp
          = return $ max_dvr dvr dvr'


-- | The very same component, non-vectorized
mkSelf :: LComp -> Ty -> Ty -> DelayedVectRes
mkSelf lcomp tin tout 
  = DVR { dvr_comp = return (eraseComp lcomp), dvr_vres = NotVect tin tout }

-- | Warn if DVRCands is empty
warnIfEmpty :: DynFlags -> LComp -> DVRCands -> String -> VecM ()
warnIfEmpty _dfs lc cands msg = warnIfEmptyDoc _dfs lc cands msg (text "")

warnIfEmptyDoc :: DynFlags -> LComp -> DVRCands -> String -> Doc -> VecM ()
warnIfEmptyDoc _dfs lc cands msg extra_info
  = when (Map.null cands) $ do
       vecMFail (compLoc lc) $ 
          vcat [ text "ERROR: empty vectorization" <+> braces (text msg)
               , text "For computation:" 
               , nest 2 $ ppr lc
               , text "Extra info:" 
               , nest 2 extra_info 
               ]
       
pprDVRess :: DVRCands -> Doc
pprDVRess cands = 
  let xs = Map.elems cands
  in vcat $ map (text . show . dvr_vres) xs


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
util (TArray (Literal n) _) = log $ (10.0 * fromIntegral n)
util _ = minUtil

parUtility :: Double -> Double -> Ty -> Double
parUtility u1 u2 tmid = u1 + u2 + util tmid

minUtil :: Double
minUtil = 0.1 -- log 0.1

-- | Choose a VectRes in case all types are joinable (precondition)
vResMatch :: [VectRes] -> Maybe VectRes
vResMatch vs = do 
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
combineData :: ParInfo  -> SrcLoc
            -> DVRCands -> DVRCands -> DVRCands
combineData p loc xs ys 
 = let xs_list = Map.elems xs
       ys_list = Map.elems ys
   in fromListDVRCands $ 
          [ r | x <- xs_list, y <- ys_list
              , r <- from_mb $ combine_par p loc x y
          ]
 where from_mb Nothing  = []
       from_mb (Just x) = [x]

combine_par :: ParInfo -> SrcLoc
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

{- 
combine_bind :: SrcLoc
             -> DelayedVectRes -> [EId] -> [DelayedVectRes] -> DelayedVectRes
combine_bind loc pre_dvr1 xs pre_dvrs
  = DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            cs <- mapM dvr_comp dvrs
            return $ cBindMany loc c1 (zip xs cs)
        , dvr_vres = vResMatch $ map dvr_vres (dvr1:dvrs) }
  where (dvr1:dvrs) = mitigate_all (pre_dvr1:pre_dvrs)
-}


combine_bind_mb :: SrcLoc
                -> Bool 
                -> DelayedVectRes -> [EId] -> [DelayedVectRes] 
                -> Maybe DelayedVectRes
combine_bind_mb loc is_computer pre_dvr1 xs pre_dvrs = do
   res <- vResMatch $ map dvr_vres (dvr1:dvrs)
   return $ 
     DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            cs <- mapM dvr_comp dvrs
            return $ cBindMany loc c1 (zip xs cs)
        , dvr_vres = res }
  where (dvr1:dvrs) = 
           if is_computer then 
             mitigate_all (pre_dvr1:pre_dvrs)
           else -- if he is a transformer then only the last guy
                -- can be a transformer so mitigate everyone except him
             let (r:rs) = reverse (pre_dvr1:pre_dvrs)
             in reverse (r : mitigate_all rs)


combine_branch_mb :: SrcLoc
                  -> Bool
                  -> Exp -> DelayedVectRes -> DelayedVectRes 
                  -> Maybe DelayedVectRes
combine_branch_mb loc is_computer e pre_dvr1 pre_dvr2 = do 
  res <- vResMatch $ map dvr_vres [dvr1,dvr2]
  return $ 
     DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            c2 <- dvr_comp dvr2 
            return $ cBranch loc e c1 c2
        , dvr_vres = res }
  where
     [dvr1,dvr2] = 
       if is_computer 
         then mitigate_all [pre_dvr1,pre_dvr2]
         else [pre_dvr1,pre_dvr2]
          -- mitigate_all [pre_dvr1,pre_dvr2]

{- 
combine_branch :: SrcLoc
               -> Exp -> DelayedVectRes -> DelayedVectRes -> DelayedVectRes
combine_branch loc e pre_dvr1 pre_dvr2
  = DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            c2 <- dvr_comp dvr2 
            return $ cBranch loc e c1 c2
        , dvr_vres = vResMatch $ map dvr_vres [dvr1,dvr2] }
  where
     [dvr1,dvr2] = mitigate_all [pre_dvr1,pre_dvr2]

-}

mitigate_all :: [DelayedVectRes] -> [DelayedVectRes]
mitigate_all vcs = map (mit_one "MA" (Just final_ty_in, Just final_ty_out)) vcs
 where 
   -- Compute "the" common input type and "the" common output type
   final_ty_in  = tyArity $ gcdTys $ map (vect_in_ty  . dvr_vres) vcs
   final_ty_out = tyArity $ gcdTys $ map (vect_out_ty . dvr_vres) vcs


mit_one :: String -> (Maybe Int, Maybe Int) -> DelayedVectRes -> DelayedVectRes
mit_one _orig (Nothing, Nothing) dvr = fixup_util_out (fixup_util_in dvr)
mit_one orig (Just n,  Nothing) dvr  = fixup_util_out (mit_in orig n dvr)
mit_one orig (Nothing, Just n)  dvr  = fixup_util_in  (mit_out orig dvr n)
mit_one orig (Just n, Just m)   dvr  = mit_in orig n  (mit_out orig dvr m)

mit_in :: String -> Int -> DelayedVectRes -> DelayedVectRes
mit_in orig n (DVR { dvr_comp = io_comp, dvr_vres = vres })
  | Just (final_in_ty, cmit) <- mitin
  = -- trace ("mit_in:" ++ show final_in_ty ++ " ~~> " ++ show vinty ++ " ~~> " ++ show voutty) $ 
    DVR { dvr_comp = cPar noLoc pnever cmit <$> io_comp
        , dvr_vres = DidVect final_in_ty voutty u }
  where vinty  = vect_in_ty vres  -- type in the middle!
        voutty = vect_out_ty vres
        mitin  = mk_in_mitigator orig n vinty
        u      = parUtility minUtil (vResUtil vres) vinty
mit_in _ _ dvr = fixup_util_in dvr

-- | Fixes-up utility /as if/ mitigation has happened
-- This is to ensure all candidates are compared fairly, whether mitigation happened or not.
fixup_util_in :: DelayedVectRes -> DelayedVectRes
fixup_util_in dvr = dvr { dvr_vres = DidVect vinty voutty u } 
  where vres   = dvr_vres dvr
        vinty  = vect_in_ty vres
        voutty = vect_out_ty vres
        u = parUtility minUtil (vResUtil vres) vinty

mit_out :: String -> DelayedVectRes -> Int -> DelayedVectRes
mit_out orig (DVR { dvr_comp = io_comp, dvr_vres = vres }) m 
  | Just (final_out_ty, cmit) <- mitout
  = -- trace ("mit_out:" ++ show vinty ++ " ~~> " ++ show voutty ++ " ~~> " ++ show final_out_ty) $ 
    DVR { dvr_comp = (\c -> cPar noLoc pnever c cmit) <$> io_comp
        , dvr_vres = DidVect vinty final_out_ty u }
  where vinty  = vect_in_ty vres
        voutty = vect_out_ty vres -- type in the middle!
        mitout = mk_out_mitigator orig voutty m
        u      = parUtility minUtil (vResUtil vres) voutty
mit_out _ dvr _ = fixup_util_out dvr

fixup_util_out :: DelayedVectRes -> DelayedVectRes
fixup_util_out dvr = dvr { dvr_vres = DidVect vinty voutty u } 
  where vres   = dvr_vres dvr
        vinty  = vect_in_ty vres
        voutty = vect_out_ty vres
        u = parUtility minUtil (vResUtil vres) voutty

-- | Mitigate on the input, return final input type
mk_in_mitigator :: String -> Int -> Ty -> Maybe (Ty, Comp)
mk_in_mitigator orig n (TArray (Literal m) tbase) 
  | n > 1 || m > 1
  , n <= m        
  , n `mod` m == 0 || m `mod` n == 0
  = Just (mkVectTy tbase n, cMitigate noLoc orig tbase n m)
  | otherwise = Nothing 
mk_in_mitigator _ _n TVoid = Nothing -- nothing to mitigate
mk_in_mitigator orig n t -- non-array
  | n > 1     = Just (mkVectTy t n, cMitigate noLoc orig t n 1)
  | otherwise = Nothing 

-- | Mitigate on the output, return final output type
mk_out_mitigator :: String -> Ty -> Int -> Maybe (Ty, Comp)
mk_out_mitigator orig (TArray (Literal n) tbase) m
  | n > 1 || m > 1 
  , m <= n
  , n `mod` m == 0 || m `mod` n == 0
  = Just (mkVectTy tbase m, cMitigate noLoc orig tbase n m)
  | otherwise = Nothing
mk_out_mitigator _ TVoid _m = Nothing -- nothing to mitigate
mk_out_mitigator orig t m -- non-array
  | m > 1     = Just (mkVectTy t m, cMitigate noLoc orig t 1 m)
  | otherwise = Nothing

        

-- | Match vectorization candidates composed on the control path
combineBind :: SrcLoc -> Bool -> 
               DVRCands -> [EId] -> [DVRCands] -> DVRCands 
combineBind loc is_comp = go 
  where 
    go c1cands [] [] = c1cands
    go c1cands (y:ys) (cs:css) =
       let cont_cands = go cs ys css
       in Map.foldr (foreach1 cont_cands) emptyDVRCands c1cands
      where 
        foreach1 k heap res = Map.foldr foreach2 res k
          where 
            foreach2 heap' res' 
              = maybe res' (`addDVR` res') $
                combine_bind_mb loc is_comp heap [y] [heap']

    go _ _ _ = error "combineBind!"


-- | Match vectorization candidates to compose them in a branch
combineBranch :: SrcLoc -> Bool 
              -> Exp -> DVRCands -> DVRCands -> DVRCands
combineBranch loc is_comp e xs ys 
 = Map.foldr foreach_xs emptyDVRCands xs
 where
   foreach_xs heap res = Map.foldr foreach_ys res ys
      where 
        foreach_ys heap' res' 
          = maybe res' (`addDVR` res') $
               combine_branch_mb loc is_comp e heap heap'


{-------------------------------------------------------------------------
  Interpret Ziria computations and AST manipulation 
-------------------------------------------------------------------------}

mkDVRes :: Bindable v => (VecEnv -> Zr v) 
        -> String
        -> SrcLoc -> Ty -> Ty -> VecM DelayedVectRes
mkDVRes gen_zir kind loc vin_ty vout_ty = do 
  venv <- getVecEnv
  zirc <- liftZr kind loc (gen_zir venv)
  let srcn = sourceName' loc
  zirc_wrapped <- wrapCFunCall (srcn ++ "_vect_" ++ kind) loc zirc
  return $ DVR { dvr_comp = return zirc_wrapped
               , dvr_vres = DidVect vin_ty vout_ty minUtil }

  where 
    sourceName' (SrcLoc (Loc p _)) = map (\c -> if isAlphaNum c then c else '_') (posFile p)
    sourceName' (SrcLoc NoLoc)     = ""


liftZr :: Bindable v => String -> SrcLoc -> Zr v -> VecM Comp
liftZr pref_dbg loc zr = do 
  sym  <- asks venv_sym
  liftIO $ interpC pref_dbg sym loc zr


-- | Wrap the comp in a new function and call, for debugging purposes.
wrapCFunCall :: String -> SrcLoc -> Comp -> VecM Comp 
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
expandMapToTakeEmit :: SrcLoc 
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

