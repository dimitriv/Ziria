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
import CtExpr
import Utils 

import Outputable
import Text.PrettyPrint.HughesPJ

import Data.List 
import Data.Maybe ( fromJust )

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

newVectGName :: String -> ty -> Maybe SourcePos -> VecM (GName ty)
newVectGName nm ty loc = do
    str <- newVectUniq
    return $ (toName (nm ++ "_" ++ str) loc ty) {uniqId = "_v" ++ str}

{-------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------}

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


{-------------------------------------------------------------------------
  Vectorization utilities
-------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------
  Vectorization results
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


-- | Build an array type from a base type (like VecM.mkVectTy)
array_ty :: Int -> Ty -> Ty
array_ty 1 ty = ty
array_ty n ty = TArray (Literal n) ty 


{-------------------------------------------------------------------------
  Delayed results of vectorization
-------------------------------------------------------------------------}

-- | Delayed vectorization result
data DelayedVectRes
   = DVR { dvr_comp       :: IO Comp   -- Delayed result
         , dvr_vres       :: VectRes } -- Information about the result


-- | Lift an operation on VectRes to be on a DelayedVectRes
lift_dvr :: (VectRes -> a) -> DelayedVectRes -> a
lift_dvr f = f . dvr_vres

-- | Utility of delayed vectorization
dvResUtil :: DelayedVectRes -> Double
dvResUtil = lift_dvr vResUtil

-- | Keep a result with maximal utility for each group by vResEqQ
keepGroupMaximals :: [DelayedVectRes] -> [DelayedVectRes]
keepGroupMaximals xs = map getMaximal (groups xs)
  where groups = groupBy' $ vResEqQ <| dvr_vres

-- | Return a result with maximal utility
getMaximal :: [DelayedVectRes] -> DelayedVectRes
getMaximal = maximumBy $ compare <| dvResUtil 

liftCompDVR :: Monad m => (Comp -> Comp) -> DelayedVectRes -> m DelayedVectRes
liftCompDVR f dvr = return $ dvr { dvr_comp = f <$> dvr_comp dvr }

forceDVR :: DelayedVectRes -> VecM Comp
forceDVR dvr = liftIO $ dvr_comp dvr



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
cross_prod_mit :: [ [DelayedVectRes] ] -> [ [DelayedVectRes] ]
cross_prod_mit bcands 
  = map mitigate $ Utils.cross_prod bcands 
  where
    mitigate :: [DelayedVectRes] -> [DelayedVectRes]
    mitigate vcs = map (mitigate_one ty_in ty_out) vcs
      -- Compute "the" common input type and "the" common output type
      where ty_in  = gcdTys $ map (vect_in_ty  . dvr_vres) vcs
            ty_out = gcdTys $ map (vect_out_ty . dvr_vres) vcs
    -- Mitigate from the expected input type (exp_tin) and to the 
    -- expected output type (exp_tout)
    mitigate_one exp_tin exp_tout dvr
      = dvr { dvr_comp = new_dvr_comp, dvr_vres = vres_new } 
      where 
       vect_tin     = vect_in_ty  $ dvr_vres dvr
       vect_tout    = vect_out_ty $ dvr_vres dvr
       new_dvr_comp = do
          comp <- dvr_comp dvr
          return $ 
            (exp_tin,vect_tin) `mitIn` comp `mitOut` (vect_tout,exp_tout)
       vres_new = (dvr_vres dvr) { vect_in_ty  = exp_tin
                                 , vect_out_ty = exp_tout }



combineCtrl :: Maybe SourcePos
            -> DelayedVectRes
            -> [EId] -> [DelayedVectRes]
            -> DelayedVectRes
combineCtrl loc dvr1 xs dvrs
  = DVR { dvr_comp = do
            c1 <- dvr_comp dvr1
            cs <- mapM dvr_comp dvrs
            return $ cBindMany loc c1 (zip xs cs)
        , dvr_vres = bind_vres (dvr1:dvrs) }
  where
     bind_vres ds
       | [] <- filter (didVect . dvr_vres) ds
       = NotVect vtin vtout
       | otherwise
       = DidVect vtin vtout u
       where
          vds   = map dvr_vres ds
          u     = chooseBindUtility vds
          vtin  = fromJust $ ctJoinMany_mb $ map vect_in_ty  vds
          vtout = fromJust $ ctJoinMany_mb $ map vect_out_ty vds



-- | Mitigate by creating a Mitigate node (maybe) node between 
--   tin1 ~> tin2
-- Pre-condition: tin1 is a ``type divisor'' of tin2 or vice-versa.
-- Result is a mitigating (ST T tin1 tin2) transformer.  
mkMit :: Maybe SourcePos -> Ty -> Ty -> Maybe Comp 
mkMit loc tin1 tin2
    -- If the two types are equal or Void no need for mitigation
  | tin1 == tin2  = Nothing
  | TVoid <- tin1 = Nothing
  | TVoid <- tin2 = Nothing

    -- If one is array but other non-array then the latter must be 
    -- the base type of the former.  
    -- We must up-mitigate: 1 ~> len
  | not (isArrayTy tin1)
  , TArray (Literal len) tbase <- tin2
  = assert "mkMit" (tbase == tin1) $ 
    return $ cMitigate loc tbase 1 len
    -- Symmetric to previous case 
  | not (isArrayTy tin2)
  , TArray (Literal len) tbase <- tin1
  = assert "mkMit" (tbase == tin2) $
    return $ cMitigate loc tbase len 1
    -- If both types are arrays (of different lenghts) let's mitigate 
  | TArray (Literal len1) tbase1 <- tin1
  , TArray (Literal len2) tbase2 <- tin2
  = assert "mkMit" (tbase1 == tbase2) $
    return $ cMitigate loc tbase1 len1 len2
  | otherwise
  = panic $ text "mkMit: can't mitigate:" <+> 
            ppr tin1 <+> text "~>" <+> ppr tin2

-- | (gty,ty) `mitIn` comp
-- Mitigates in the input type of comp
mitIn :: (Ty,Ty) -> Comp -> Comp
mitIn (gty,ty) comp
  | let loc = compLoc comp
  , Just m <- mkMit loc gty ty
  = cPar loc pnever m comp
  | otherwise = comp

-- | comp `mitOut` (ty,gty)
-- Mitigates on the output type of comp, symmetrically to `mitIn' above.
mitOut :: Comp -> (Ty,Ty) -> Comp 
mitOut comp (ty,gty)
  | let loc = compLoc comp
  , Just m <- mkMit loc ty gty
  = cPar loc pnever comp m
  | otherwise = comp

{-------------------------------------------------------------------------
  Matching on the data path
-------------------------------------------------------------------------}

-- | Match vectorization candidates composed on the data path
combineData :: ParInfo
            -> Maybe SourcePos
            -> [DelayedVectRes] -> [DelayedVectRes] -> [DelayedVectRes]
combineData p loc xs ys 
  = cross_comb (mitigatePar p loc) xs ys

mitigatePar :: ParInfo
            -> Maybe SourcePos
            -> DelayedVectRes
            -> DelayedVectRes
            -> Maybe DelayedVectRes
mitigatePar pnfo loc dp1 dp2 = do
  vres <- mk_vres (dvr_vres dp1) (dvr_vres dp2)
  return $ 
    DVR { dvr_comp = mk_par (dvr_comp dp1) (dvr_comp dp2)
        , dvr_vres = vres }
  where
    mk_par ioc1 ioc2 = do
      c1 <- ioc1
      c2 <- ioc2 
      return $ cPar loc pnfo c1 c2   
    -- non joinable => just fail
    mk_vres r1 r2
      | Nothing <- ctJoin_mb (vect_out_ty r1) (vect_in_ty r2)
      = Nothing  
    -- both non-vect => non-vect
    mk_vres (NotVect t1 _t2) (NotVect _s1 s2) = return $ NotVect t1 s2
    -- joinable and one didVect => Didvect 
    mk_vres r1 r2 
      = let u = chooseParUtility r1 r2 
        in return $ DidVect (vect_in_ty r1) (vect_out_ty r2) u 

{-------------------------------------------------------------------------
  Vectorization utilities
-------------------------------------------------------------------------}


-- | Computes least array or primitive type from the two, used for
-- mitigators 
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
  | t == t' = t
  | otherwise
  = panic $ text "gcd_ty: types are non-joinable!" <+> 
            ppr t <+> text "and" <+> ppr t'

gcdTys :: [Ty] -> Ty
gcdTys xs = go (head xs) (tail xs)
  where go t []       = t 
        go t (t1:t1s) = go (gcd_ty t t1) t1s


-- | Wrap the comp in a new function and call, for debugging purposes.
wrapCFunCall :: String -> Maybe SourcePos -> Comp -> VecM Comp 
wrapCFunCall nm loc comp = do
  let carrty = CTArrow [] (ctComp comp)
  vname <- newVectGName nm carrty loc
  return $ cLetFunC loc vname [] comp (cCall loc vname [])


{-------------------------------------------------------------------------
  Rewrite the external emits and takes of a computer
-------------------------------------------------------------------------}

-- | The cardinality of any Map, obviously 1-1, statically known
map_card :: Card
map_card = SCard (CAStatic 1) (CAStatic 1)

take_card :: Card
take_card = SCard (CAStatic 1) (CAStatic 0)
emit_card :: Card
emit_card = SCard (CAStatic 0) (CAStatic 1)


map2take_emit :: Maybe SourcePos 
              -> Maybe VectAnn -> Ty -> EId -> VecM LComp
map2take_emit loc vann inty f = do 
  x <- newVectGName "_map_bnd" inty loc
  return $
    AstL.cRepeat loc OCard vann $ 
         AstL.cBindMany loc map_card tk [(x,em x)]
  where tk   = AstL.cTake1 loc take_card inty
        em x = AstL.cEmit loc emit_card (eCall loc f [eVar loc x])


-- | Type-preserving transformations on the take and emit nodes of a computation
-- NB: Does not include ReadSrc/WritSnk/ReadInternal/WriteInternal nodes as these
-- can't currently be expressed as just a set of repeated computers. 
rewriteTakeEmit ::
     (Maybe SourcePos -> Ty -> VecM Comp)         -- on Take1
  -> (Maybe SourcePos -> Ty -> Int -> VecM Comp)  -- on Take
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emit
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emits
  -> LComp -> VecM Comp
rewriteTakeEmit on_take on_takes on_emit on_emits = go
  where 
    go lcomp = 
      let loc = compLoc lcomp in 
      case unComp lcomp of 
        -- Standard boilerplate 
        Var x -> lookupCVarBind x >>= go 

        BindMany c1 xs_cs -> do
          c1' <- go c1
          let go_one (x,c) = go c >>= \c' -> return (x,c')
          xs_cs' <- mapM go_one xs_cs
          return $ cBindMany loc c1' xs_cs'
        Let x c1 c2        -> extendCVarBind x c1 (go c2)
        LetE x fi e c1     -> cLetE loc x fi e    <$> go c1
        LetERef x me c1    -> cLetERef loc x me   <$> go c1
        LetHeader fun c1   -> cLetHeader loc fun  <$> go c1
        LetStruct sdef c1  -> cLetStruct loc sdef <$> go c1
        LetFunC f params c1 c2
          -> extendCFunBind f params c1 $ go c2
        Branch e c1 c2 -> do
          c1' <- go c1
          c2' <- go c2
          return $ cBranch loc e c1' c2'
        Standalone c1 -> cStandalone loc <$> go c1
        Return fi e   -> return $ cReturn loc fi e 
        Seq c1 c2 -> do
          c1' <- go c1
          c2' <- go c2
          return $ cSeq loc c1' c2'
        Call f es -> do 
          (CFunBind { cfun_params = prms
                    , cfun_body = body }) <- lookupCFunBind f
          vbody <- go body
          let vf_type = CTArrow (map ctCallArg es) (ctComp vbody)
          vf <- newVectGName (name f ++ "_spec") vf_type loc
          return $ cLetFunC loc vf prms vbody $ 
                   cCall loc vf (map eraseCallArg es)

        -- Interesting cases
        Take1 t  -> on_take loc t
        Take t n -> on_takes loc t n 
        Emit e   -> on_emit loc e
        Emits es -> on_emits loc es

        Par pinfo c1 c2 -> do 
          -- Note we don't rewrite the intermediates!
          c1' <- rewriteTakeEmit on_take on_takes mEmit mEmits c1
          c2' <- rewriteTakeEmit mTake1 mTake on_emit on_emits c2
          return $ cPar loc pinfo c1' c2'

        Map vann f -> do 
          let inty = inTyOfCTy $ ctComp lcomp
          c <- map2take_emit loc vann inty f
          go c

        Until e c           -> cUntil loc e <$> go c
        While e c           -> cWhile loc e <$> go c
        Times ui e elen n c -> cTimes loc ui e elen n <$> go c
        Repeat vann c       -> cRepeat loc vann <$> go c

        VectComp ann c -> cVectComp loc ann <$> go c

        -- Other non-simple computers
        _other -> 
          vecMFail loc $ text "Not a simple computer:" <+> ppr lcomp
        

mTake1 :: Monad m => Maybe SourcePos -> t -> m (GComp tc t () ())
mTake1 loc t = return $ cTake1 loc t

mTake :: Monad m => Maybe SourcePos -> t -> Int -> m (GComp tc t () ())
mTake loc t n = return $ cTake loc t n

mEmit :: Monad m => Maybe SourcePos -> GExp t () -> m (GComp tc t () ())
mEmit loc e = return $ cEmit loc e

mEmits :: Monad m => Maybe SourcePos -> GExp t () -> m (GComp tc t () ())
mEmits loc e = return $ cEmits loc e

rwTakeEmitIO :: VecEnv -> 
     (Maybe SourcePos -> Ty -> VecM Comp)         -- on Take1
  -> (Maybe SourcePos -> Ty -> Int -> VecM Comp)  -- on Take
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emit
  -> (Maybe SourcePos -> Exp -> VecM Comp)        -- on Emits
  -> LComp -> IO Comp
rwTakeEmitIO venv t1 t2 e1 e2 lc 
  = runVecM venv $ rewriteTakeEmit t1 t2 e1 e2 lc

{-------------------------------------------------------------------------
  Helpers for rewriting take and emit
-------------------------------------------------------------------------}

rw_take :: Int -> EId -> EId -> EId -> Maybe SourcePos -> Ty -> VecM Comp
rw_take arin iidx tidx xa loc _ty
  | arin == 1 = liftZr loc $ freturn _fI xa
  | otherwise = liftZr loc $ do
      tidx  .:= iidx
      iidx  .:= iidx .+ I(1)
      freturn _aI $ xa .! tidx -- Auto inline, not force-inline

rw_takes :: EId -> EId -> EId -> Maybe SourcePos -> Ty -> Int -> VecM Comp
rw_takes iidx tidx xa loc _ty n
  = liftZr loc $ do
      tidx  .:= iidx
      iidx  .:= iidx .+ I(n)
      freturn _aI $ xa .! (tidx :+ n)

rw_emit :: Int -> EId -> EId -> EId -> Maybe SourcePos -> Exp -> VecM Comp
rw_emit arout oidx tidx ya loc e
   -- there can be only one emit so this must be it
  | arout == 1 = liftZr loc $ femit e
  | otherwise  = liftZr loc $ do
      tidx .:= oidx
      oidx .:= oidx .+ I(1)
      ya .! tidx .:= e

rw_emits :: EId -> EId -> EId -> Maybe SourcePos -> Exp -> VecM Comp
rw_emits oidx tidx ya loc es
  = liftZr loc $ do
      tidx .:= oidx
      oidx .:= oidx .+ I(len)
      ya .! (tidx :+ len) .:= es
  where TArray (Literal len) _ = ctExp es

zERO :: I
zERO = I(0)

mkDVRes :: Bindable v => (VecEnv -> Zr v) 
        -> String
        -> Maybe SourcePos -> Ty -> Ty -> VecM DelayedVectRes
mkDVRes gen_zir kind loc vin_ty vout_ty = do 
  venv <- getVecEnv
  zirc <- liftZr loc (gen_zir venv)
  zirc_wrapped <- wrapCFunCall ("_vect_" ++ kind) loc zirc
  return $ DVR { dvr_comp = return zirc_wrapped
               , dvr_vres = DidVect vin_ty vout_ty minUtil }

{-------------------------------------------------------------------------
  Interpret Ziria computations
-------------------------------------------------------------------------}

liftZr :: Bindable v => Maybe SourcePos -> Zr v -> VecM Comp
liftZr loc zr = do 
  sym  <- asks venv_sym
  liftIO $ interpC sym loc zr

