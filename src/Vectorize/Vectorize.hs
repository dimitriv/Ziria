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

{-# LANGUAGE RecordWildCards #-}

module Vectorize where

import AstExpr
import AstComp
import AstFM
import AstUnlabelled
import qualified AstLabelled as AstL

import Outputable

import Text.PrettyPrint.HughesPJ

import PpComp
import Outputable
import qualified GenSym as GS
import Text.Parsec.Pos
import qualified Data.Set as S
import Control.Monad.State
import Data.List as M
import qualified Data.Map  as Map
import Data.Functor.Identity
import Data.Maybe ( fromJust )

import Control.Applicative  ( (<$>) )

import qualified Data.Traversable as T

import Opts
import Utils

import CardAnalysis -- Cardinality analysis
import VecM         -- Vectorizer monad and infrastructure
import VecSF        -- Vectorization scale factors
import VecScaleUp
import VecScaleDn

import CtComp

import PassFold ( elimMitigsIO )
import Debug.Trace


{-------------------------------------------------------------------------------
  Vectorizer proper
-------------------------------------------------------------------------------}

-- | Pack information we want to capture as we traverse AST
data VectPack 
  = VectPack { vp_comp     :: LComp
             , vp_card     :: Card
             , vp_self_dvr :: DelayedVectRes
             , vp_vctx     :: CtxForVect
             , vp_loc      :: Maybe SourcePos
             , vp_cty      :: CTy
             , vp_tyin     :: Ty
             , vp_tyout    :: Ty }

computeVectTop :: DynFlags -> LComp -> VecM DVRCands
computeVectTop dfs x =
   -- See Note [Initial Vectorization Context]
  comp_vect dfs CtxExistsCompLeft x

{- Note [Initial Vectorization Context]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The in initial CtxForVect value can well be CtxUnrestricted but this 
   very quickly leads to explosion, particularly for transformers that are 
   connected to each other like:
             read >>> t1 >>> t2 >>> ... >>> tn >>> write
   So we are instead starting from CtxExistsCompLeft.
-}

comp_vect :: DynFlags -> CtxForVect -> LComp -> VecM DVRCands
comp_vect dfs vctx c = do 
  verbose dfs (text "^^^" <+> text (compShortName c))
  comp_vect0 dfs vpack (unComp c)
  where 
    vpack = VectPack { vp_comp     = c
                     , vp_card     = compInfo c 
                     , vp_vctx     = vctx
                     , vp_cty      = cty
                     , vp_loc      = loc 
                     , vp_self_dvr = mkSelf c inty outty
                     , vp_tyin     = inty
                     , vp_tyout    = outty }
    cty   = ctComp c
    loc   = compLoc c
    inty  = inTyOfCTy cty
    outty = yldTyOfCTy cty

comp_vect0 :: DynFlags -> VectPack -> LComp0 -> VecM DVRCands

-- | Variable 
comp_vect0 dfs (VectPack {..}) (Var x)
  = lookupCVarBind x >>= comp_vect dfs vp_vctx

-- | BindMany 
comp_vect0 dfs (VectPack {..}) (BindMany c1 xs_cs) = do
  let sfs = compSFDD vp_card vp_tyin vp_tyout
      css = c1 : map snd xs_cs
      xs  = map fst xs_cs
  -- Compute direct down-vectorizations
  direct_vss <- vect_comp_dd dfs vp_comp sfs
  -- Compute recursive vectorizations
  vss <- mapM (comp_vect dfs vp_vctx) css
  let recursive_vss = combineBind vp_loc (head vss) xs (tail vss)
  warnIfEmpty dfs vp_comp recursive_vss "BindMany (recursive)"
  -- Return directs + recursives (will contain self)
  return (direct_vss `unionDVRCands` recursive_vss)

-- | Sequence
comp_vect0 dfs (VectPack {..}) (Seq c1 c2) = do
   let dty = fromJust $ doneTyOfCTy (ctComp c1)
   tmp <- newVectGName "tmp" dty vp_loc Imm
   comp_vect dfs vp_vctx (AstL.cBindMany vp_loc vp_card c1 [(tmp,c2)])

-- | Par
comp_vect0 dfs (VectPack {..}) (Par p c1 c2)
  | ReadSrc orig_ty <- unComp c1
  = mapDVRCands (prependReadSrc vp_loc p orig_ty) <$> comp_vect dfs vp_vctx c2
  | WriteSnk orig_ty <- unComp c2
  = mapDVRCands (appendWriteSnk vp_loc p orig_ty) <$> comp_vect dfs vp_vctx c1
  | otherwise 
  = let is_c1 = isComputer (ctComp c1)
        is_c2 = isComputer (ctComp c2)
        ctx1  = if is_c2 then CtxExistsCompRight else vp_vctx
        ctx2  = if is_c1 then CtxExistsCompLeft  else vp_vctx
    in do vcs1 <- comp_vect dfs ctx1 c1
          vcs2 <- comp_vect dfs ctx2 c2
          let res = combineData p vp_loc vcs1 vcs2
          warnIfEmpty dfs vp_comp res "Par"
          return res

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (LetE x fi e c1) = 
  mapDVRCands (updDVRComp $ cLetE vp_loc x fi e) <$> comp_vect dfs vp_vctx c1

comp_vect0 dfs (VectPack {..}) (LetERef x mbe c1) = 
  mapDVRCands (updDVRComp $ cLetERef vp_loc x mbe) <$> comp_vect dfs vp_vctx c1

comp_vect0 dfs (VectPack {..}) (LetHeader fdef c1) =
  mapDVRCands (updDVRComp $ cLetHeader vp_loc fdef) <$> comp_vect dfs vp_vctx c1

comp_vect0 dfs (VectPack {..}) (Let x c1 c2) =
  mapDVRCands (updDVRComp $ cLet vp_loc x (eraseComp c1)) <$>
  extendCVarBind x c1 (comp_vect dfs vp_vctx c2)

comp_vect0 dfs (VectPack {..}) (LetFunC f params c1 c2) =
  mapDVRCands (updDVRComp $ cLetFunC vp_loc f params (eraseComp c1)) <$>
  extendCFunBind f params c1 (comp_vect dfs vp_vctx c2)

comp_vect0 dfs (VectPack {..}) (LetStruct sdef c2) =
  mapDVRCands (updDVRComp (cLetStruct vp_loc sdef)) <$> comp_vect dfs vp_vctx c2

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Call f es) = do
  CFunBind { cfun_params = prms, cfun_body = bdy } <- lookupCFunBind f

  let (prms', cbinds, es') = mkHOCompSubst prms es

  let bdy' = foldl mk_let_bind bdy cbinds
      mk_let_bind c (x,bnd) = AstL.cLet vp_loc (compInfo bdy) x bnd c

  -- NB: It's not very efficient to create a zillion typed names.
  -- Hence we create one and set its type each time.
  vf <- newVectGName (name f ++ "_vect") undefined vp_loc Imm

  let mk_vect_call vbd
        = cLetFunC vp_loc vf_typed prms' vbd $
          cCall vp_loc vf_typed (map eraseCallArg es)
        where vf_typed = updNameTy vf vf_type
              vf_type  = CTArrow (map nameCallArgTy prms') (ctComp vbd)

  mapDVRCands (updDVRComp mk_vect_call) <$> comp_vect dfs vp_vctx bdy'


{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Interleave {}) = 
  return (singleDVRCands vp_self_dvr)

comp_vect0 dfs (VectPack {..}) (Filter {}) =
  return (singleDVRCands vp_self_dvr)

comp_vect0 dfs (VectPack {..}) (ReadSrc {}) =
  return (singleDVRCands vp_self_dvr)

comp_vect0 dfs (VectPack {..}) (WriteSnk {}) =
  return (singleDVRCands vp_self_dvr)

comp_vect0 dfs (VectPack {..}) (WriteInternal {}) =
  return (singleDVRCands vp_self_dvr)

comp_vect0 dfs (VectPack {..}) (ReadInternal {}) =
  return (singleDVRCands vp_self_dvr)

comp_vect0 dfs (VectPack {..}) (Return {}) =
  return (singleDVRCands vp_self_dvr)

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Branch e c1 c2) = do 
  let sfs = compSFDD vp_card vp_tyin vp_tyout

  direct_vss <- vect_comp_dd dfs vp_comp sfs

  vcs1 <- comp_vect dfs vp_vctx c1
  vcs2 <- comp_vect dfs vp_vctx c2

  let recursive_vss = combineBranch vp_loc e vcs1 vcs2
  warnIfEmpty dfs vp_comp recursive_vss "Branch"

  return $ direct_vss `unionDVRCands` recursive_vss

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Standalone c) = 
  mapDVRCands (updDVRComp $ cStandalone vp_loc) <$> comp_vect dfs vp_vctx c

comp_vect0 dfs (VectPack {..}) (Mitigate {}) =
  vecMFail vp_loc $ text "Unexpected mitigate node in vectorization."


{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Until e c) =
  addDVR vp_self_dvr <$>
  vectIterComp dfs (cUntil vp_loc e) vp_tyin vp_tyout c

comp_vect0 dfs (VectPack {..}) (While e c) =
  addDVR vp_self_dvr <$>
  vectIterComp dfs (cWhile vp_loc e) vp_tyin vp_tyout c

comp_vect0 dfs (VectPack {..}) (Times ui nm e elen c) =
  addDVR vp_self_dvr <$>
  vectIterComp dfs (cTimes vp_loc ui nm e elen) vp_tyin vp_tyout c

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Map vann nm) =
  addDVR vp_self_dvr <$>
    vectMap dfs vp_vctx nm vp_tyin vp_tyout vp_loc vann

comp_vect0 dfs (VectPack {..}) (Repeat vann c) =
  addDVR vp_self_dvr <$>
    vectRepeat dfs vp_vctx c vp_tyin vp_tyout vp_loc vann

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (VectComp (fin,fout) c) = do
  -- I am not sure that the VectComp annotations are in fact used
  liftIO $ print $
    vcat [ text "VectComp, ignoring annotation."
         , text "At location:" <+> text (show vp_loc)
         ]
  comp_vect dfs vp_vctx c

{-------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) _other =
  addDVR vp_self_dvr <$>
  vect_comp_dd dfs vp_comp (compSFDD vp_card vp_tyin vp_tyout)


{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

-- | The very same component, non-vectorized
mkSelf :: LComp -> Ty -> Ty -> DelayedVectRes
mkSelf lcomp tin tout 
  = DVR { dvr_comp = return (eraseComp lcomp), dvr_vres = NotVect tin tout }

-- | Warn if DVRCands is empty
warnIfEmpty :: DynFlags -> LComp -> DVRCands -> String -> VecM ()
warnIfEmpty dfs lc cands msg 
  = when (Map.null cands) $ liftIO $ do
       print $ text "WARNING: empty vectorization" <+> braces (text msg)
       verbose dfs $ vcat [ text "For computation:" 
                          , nest 2 $ ppr lc ]

-- | Vectorizing a (finitely) iterated computer (NB: not adding 'self')
vectIterComp :: DynFlags -> (Comp -> Comp) -> Ty -> Ty -> LComp -> VecM DVRCands
vectIterComp dfs builder tin tout cbody = do
  let body_card = compInfo cbody
  body_cands <- vect_comp_dd dfs cbody $ compSFDD body_card tin tout
  return (mapDVRCands (updDVRComp $ builder) body_cands)


-- | Take a vectorization candidate vc and prepend it with an appropriately
--   vectorized version of readSrc, to get (read >>> vc)
prependReadSrc :: Maybe SourcePos -> ParInfo
               -> Ty -> DelayedVectRes -> DelayedVectRes
prependReadSrc loc p orig_ty (DVR { dvr_comp = iocomp, dvr_vres = vres })
  = DVR { dvr_comp = iocomp', dvr_vres = vres' }
  where 
    iocomp'   = cPar loc p (cReadSrc loc new_rd_ty) <$> iocomp
    new_rd_ty = if dvr_in_ty == TVoid then orig_ty else dvr_in_ty
    dvr_in_ty = vect_in_ty vres
    new_in_ty = TBuff (ExtBuf new_rd_ty)
    vres' = case vres of 
     NotVect tin tout -> NotVect new_in_ty tout
     DidVect _ tout u -> DidVect new_in_ty tout (parUtility minUtil u dvr_in_ty)

-- | Take a vectorization candidate vc and append an appropriately vectorized
--   version of WriteSnk, to get (vc >>> write).
appendWriteSnk :: Maybe SourcePos -> ParInfo
               -> Ty -> DelayedVectRes -> DelayedVectRes
appendWriteSnk loc p orig_ty (DVR { dvr_comp = iocomp, dvr_vres = vres })
  = DVR { dvr_comp = iocomp', dvr_vres = vres' }
  where 
    iocomp' = do
      c <- iocomp
      return (cPar loc p c (cWriteSnk loc new_wr_ty))
    new_wr_ty = if dvr_out_ty == TVoid then orig_ty else dvr_out_ty
    dvr_out_ty = vect_out_ty vres        
    new_out_ty = TBuff (ExtBuf new_wr_ty)
    vres' = case vres of 
     NotVect tin _ -> NotVect tin new_out_ty
     DidVect tin _ u -> DidVect tin new_out_ty (parUtility minUtil u dvr_out_ty)

{-------------------------------------------------------------------------------
  Vectorizing Map 
-------------------------------------------------------------------------------}

-- | To avoid duplication we vectorize Map exactly as we do for
-- repeat. Hence, below we create a node: seq { x <- take; emit f(x) }
-- and call the vectorizer for Repeat.
vectMap :: DynFlags 
        -> CtxForVect
        -> EId -> Ty -> Ty -> Maybe SourcePos
        -> Maybe VectAnn
        -> VecM DVRCands
vectMap dfs vctx f tin tout loc vann = do
  MkComp (Repeat _ c0) _ _ <- expandMapToTakeEmit loc vann tin f 
  vectRepeat dfs vctx c0 tin tout loc vann
  
{-------------------------------------------------------------------------------
  Vectorizing Repeat 
-------------------------------------------------------------------------------}

type ScaleFactors = ([SFUD], [SFDU], [SFDD])


-- | The scalefactors for a repeat
repeat_scalefactors :: CtxForVect -> Card -> Ty -> Ty -> ScaleFactors
repeat_scalefactors vctx card tyin tyout
  = let sfdus  = compSFDU card tyin tyout
        sfuds  = compSFUD card tyin tyout
        sfdds  = compSFDD card tyin tyout
    in case vctx of 
         CtxUnrestricted    -> (sfuds, sfdus, sfdds)
         CtxExistsCompLeft  -> (sfuds, []   , sfdds) 
         CtxExistsCompRight -> ([]   , sfdus, sfdds)


logCandidates :: DynFlags -> String -> DVRCands -> VecM DVRCands
logCandidates dfs origin cands = do
  verbose dfs $ text origin <>
    parens (text (show (Map.size cands)) <+> text "candidates")
  return cands


-- | NB: Not including 'self'
vectRepeat :: DynFlags
           -> CtxForVect
           -> LComp -- The iterated /computer/ 
           -> Ty -> Ty -> Maybe SourcePos
           -> Maybe VectAnn
           -> VecM DVRCands
vectRepeat dfs vctx c tyin tyout loc vann = go vann c
  where
   -- | Vectorize without restrictions and up/dn mitigate
   go Nothing c0 = do 
     let (sfuds,sfdus,sfdds)
            = repeat_scalefactors vctx (compInfo c0) tyin tyout
     vecuds <- vect_comp_ud dfs c0 sfuds >>= logCandidates dfs "vect_comp_ud"
     vecdus <- vect_comp_du dfs c0 sfdus >>= logCandidates dfs "vect_comp_du"
     vecdds <- vect_comp_dd dfs c0 sfdds >>= logCandidates dfs "vect_comp_dd"
     let cands = vecuds `unionDVRCands` vecdus `unionDVRCands` vecdds
     let res   = mapDVRCands (updDVRComp $ cRepeat loc Nothing) cands
     logCandidates dfs "VectRepeat" res
  
   -- | Vectorize internally to /exactly/ (fin,fout) and externally up
   -- or down depending on the flag f
   go (Just (Rigid f (fin,fout))) c0 = do
      vcs <- go Nothing c0
      let pred (ty1,ty2) _ = ty_match ty1 fin && ty_match ty2 fout
          vcs_matching     = Map.filterWithKey pred vcs
          repeat_sfs       = repeat_scalefactors vctx (compInfo c0) tyin tyout
      logCandidates dfs "VectRepeat" $ 
        mitigateFlexi f repeat_sfs vcs_matching

   -- | Vectorize internally to anything <= (fin,fout) and externally up
   -- or down depending on the flag f
   go (Just (UpTo f (fin,fout))) c0 = do
      vcs <- go Nothing c0
      let pred (ty1,ty2) _ = ty_upto ty1 fin && ty_upto ty2 fout
          vcs_matching     = Map.filterWithKey pred vcs
          repeat_sfs       = repeat_scalefactors vctx (compInfo c0) tyin tyout
      logCandidates dfs "VectRepeat" $ 
        mitigateFlexi f repeat_sfs vcs_matching

-- | Somewhat delicate arity equality and up-to comparisons
ty_match :: Ty -> Int -> Bool
ty_match (TArray (Literal n) _) j = j == n
ty_match TVoid j                  = j >= 0
ty_match _t j                     = j == 1

ty_upto :: Ty -> Int -> Bool
ty_upto (TArray (Literal n) _) j  = n <= j
ty_upto TVoid j                   = j >= 0
ty_upto _t j                      = j > 0

{-------------------------------------------------------------------------------
  Flexible modes of vectorization (via mitigators)
-------------------------------------------------------------------------------}

-- | Try to create mitigated versions of candidates to all scalefactors
mitigateFlexi :: Bool -> ScaleFactors -> DVRCands -> DVRCands
mitigateFlexi False _sfs cands = cands
mitigateFlexi True (sfuds,sfdus,sfdds) cands =
  let arities = map sfud_arity sfuds ++ 
                map sfdu_arity sfdus ++ 
                map sfdd_arity sfdds
  -- For each arity try to convert the candidate to it
  in foldDVRCands (upd arities) emptyDVRCands cands
  where upd ars s r = foldl (\cs ar -> mit_one ar r `addDVR` cs) s ars



{-------------------------------------------------------------------------------
  DD/UD/DU Vectorization entry points (see VecScaleUp/VecScaleDn)
-------------------------------------------------------------------------------}

vect_comp_ud :: DynFlags -> LComp -> [SFUD] -> VecM DVRCands
vect_comp_ud dfs lcomp sfs 
 = foldM (\cs sf ->
     do r <- VecScaleUp.doVectCompUD dfs cty lcomp sf
        return (addDVR r cs)) emptyDVRCands sfs
 where cty = ctComp lcomp

vect_comp_du :: DynFlags -> LComp -> [SFDU] -> VecM DVRCands
vect_comp_du dfs lcomp sfs 
  = foldM (\cs sf -> 
     do r <- VecScaleUp.doVectCompDU dfs cty lcomp sf
        return (addDVR r cs)) emptyDVRCands sfs
  where cty = ctComp lcomp

vect_comp_dd :: DynFlags -> LComp -> [SFDD] -> VecM DVRCands
vect_comp_dd dfs lcomp sfs 
  = foldM (\cs sf ->
     do r <- VecScaleDn.doVectCompDD dfs cty lcomp sf
        return (addDVR r cs)) emptyDVRCands sfs
  where cty = ctComp lcomp


{-------------------------------------------------------------------------------
  Entry point to the vectorizer
-------------------------------------------------------------------------------}

-- | Entry point to vectorizer 
-- The first Comp that we return is the maximal candidate. In Debug mode we 
-- also can return all possible candidates in the second component of the 
-- returned pair (including the maximal)
runVectorizer :: DynFlags -> GS.Sym -> Comp -> IO (Comp,[Comp])
runVectorizer dflags sym comp = do

  verbose dflags $ text "Cardinality analysis starting."
  lcomp <- runCardAnal dflags comp
  verbose dflags $ text "Cardinality analysis finished."

  verbose dflags $ text "Vectorization starting."
  vss <- runVecM (VecEnv sym [] []) (computeVectTop dflags lcomp)
  verbose dflags $ text "Vectorization finished." <+> 
                   parens (ppr (Map.size vss) <+> text "candidates")

  when (Map.null vss) $ 
    panic $ 
    vcat [ text "Empty vectorization candidate set for computation:"
         , ppr comp 
         ]
{- Too verbose even in verbose mode ...
  verbose dflags $
      do let vss_list = map mlMax (Map.elems vss)
         vcat (map (text . show . dvr_vres) vss_list)
-}
  
  let do_one (DVR { dvr_comp = io_comp, dvr_vres = vres }) = do
        vc_mit <- io_comp
        -- Optimize mitigators
        vc_opt_mit <- if isDynFlagSet dflags NoElimMit then return vc_mit
                      else elimMitigsIO dflags sym vc_mit
        -- Compile away mitigators if flag set
        let vc = vc_opt_mit 

        verbose dflags $ vcat [ text "Type checking vectorization candidate."
                              , nest 2 $ ppr vc ]

        case ctComp vc of _ -> return vc

        return vc

  -- in Debug mode optimize compile and type check all candidates
  let maxi = getMaximal vss
 
  maxi_comp <- dvr_comp maxi  
  verbose dflags $ vcat [ text "Selected candidate is: "
                        , nest 2 $ text $ show $ dvr_vres maxi
                        , nest 2 $ ppr maxi_comp
                        ] 

  final_maxi_comp <- do_one maxi

  when (isDynFlagSet dflags Debug) $ 
    T.mapM (do_one . mlMax) vss >> return ()

  return (final_maxi_comp, []) -- Don't emit candidates that's ok


