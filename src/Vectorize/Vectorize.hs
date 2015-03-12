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

{-# LANGUAGE RecordWildCards, BangPatterns #-}

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
import Data.IORef

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

import Debug.Trace

import CardAnalysis

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

pprVectPack :: VectPack -> Doc
pprVectPack (VectPack {..}) 
  = vcat [ text "vp_card  = " <+> text (show vp_card)
         , text "vp_cty   = " <+> ppr vp_cty
         , text "vp_tyin  = " <+> ppr vp_tyin
         , text "vp_tyout = " <+> ppr vp_tyout
         , text "vp_vctx  = " <+> text (show vp_vctx)
         ]

computeVectTop :: DynFlags -> LComp -> VecM DVRCands
computeVectTop dfs x =
   -- See Note [Initial Vectorization Context]
  comp_vect dfs CtxUnrestricted x

{- Note [Initial Vectorization Context]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   The in initial CtxForVect value can well be CtxUnrestricted but this 
   very quickly leads to explosion, particularly for transformers that are 
   connected to each other like:
             read >>> t1 >>> t2 >>> ... >>> tn >>> write
   So we are instead starting from CtxExCompLeft.
-}

comp_vect :: DynFlags -> CtxForVect -> LComp -> VecM DVRCands
comp_vect dfs vctx c = do 
  verbose dfs (text "^^^" <+> 
               text (compShortName c) <+> 
               parens (text (show loc))
              )
  r <- comp_vect0 dfs vpack (unComp c)
  return $!r
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
comp_vect0 dfs pack@(VectPack {..}) (BindMany c1 xs_cs) = do
  let sfs = compSFDD vp_card vp_tyin vp_tyout
      css = c1 : map snd xs_cs
      xs  = map fst xs_cs
      is_computer = isComputer vp_cty
  -- Compute direct down-vectorizations

  verbose dfs $ vcat [ text "BindMany vectorization"
                     , nest 2 $ pprVectPack pack 
                     ]

  verbose dfs $ text "BindMany: before direct."

  verbose dfs $ text "sfs length = " <+> int (length sfs)


  direct_vss <- vect_comp_dd dfs vp_comp sfs

  verbose dfs $ text "BindMany: before recursive."
              
  -- Compute recursive vectorizations
  vss <- mapM (comp_vect dfs vp_vctx) css

  verbose dfs $ 
    vcat [ text "Bindmany: after recursive"
         , text "Temp vss lengths = " 
         , nest 2 $ vcat $ map (\(v,c) -> 
                     vcat [ text "Computation: " <+> ppr c 
                          , text "Candidate length: " <+> 
                               (int (Map.size v)) ]) (zip vss css)
         ]

  let recursive_vss = combineBind vp_loc is_computer (head vss) xs (tail vss)

  warnIfEmpty dfs vp_comp recursive_vss "BindMany (recursive)"

  -- Return directs + recursives (will contain self)
  let ret = direct_vss `unionDVRCands` recursive_vss
  ret `seq` return ret

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
        ctx1  = if is_c2 then CtxExCompRight `join_ctx` vp_vctx else vp_vctx
        ctx2  = if is_c1 then CtxExCompLeft  `join_ctx` vp_vctx else vp_vctx
    in
    do verbose dfs $ text (show vp_vctx ++ " => " ++ show ctx1)
       verbose dfs $ text (show vp_vctx ++ " => " ++ show ctx2) 
       vcs1 <- comp_vect dfs ctx1 c1 >>= logCands dfs False (show (compLoc c1))
       vcs2 <- comp_vect dfs ctx2 c2 >>= logCands dfs False (show (compLoc c2))
       let !res = combineData p vp_loc vcs1 vcs2
    
       let dbg_doc = vcat $ 
               [ text "Left candidates: " <+> text (show (compLoc c1))
               , nest 2 $ pprDVRess vcs1
               , nest 2 $ text (show ctx1)
               , nest 4 $ ppr c1
               , text "Right candidates: " <+> text (show (compLoc c2))
               , nest 2 $ pprDVRess vcs2
               , nest 2 $ text (show ctx2)
               , nest 4 $ ppr c2
               , text "Final candidates" 
               , nest 2 $ pprDVRess res
               ]

--     verbose dfs dbg_doc

       warnIfEmptyDoc dfs vp_comp res "Par" dbg_doc

       return res
  where 
     join_ctx x CtxUnrestricted = x
     join_ctx CtxUnrestricted x = x
     join_ctx CtxExCompLeftAndRight _ = CtxExCompLeftAndRight
     join_ctx _ CtxExCompLeftAndRight = CtxExCompLeftAndRight
     join_ctx CtxExCompRight CtxExCompRight = CtxExCompRight
     join_ctx CtxExCompRight CtxExCompLeft  = CtxExCompLeftAndRight
     join_ctx CtxExCompLeft  CtxExCompRight = CtxExCompLeftAndRight
     join_ctx CtxExCompLeft  CtxExCompLeft  = CtxExCompLeft 


{------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------}

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


{------------------------------------------------------------------------------}

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
      is_computer = isComputer vp_cty

  direct_vss <- vect_comp_dd dfs vp_comp sfs

  vcs1 <- comp_vect dfs vp_vctx c1
  vcs2 <- comp_vect dfs vp_vctx c2

  let recursive_vss = combineBranch vp_loc is_computer e vcs1 vcs2
  warnIfEmpty dfs vp_comp recursive_vss "Branch"


  let ret = direct_vss `unionDVRCands` recursive_vss
  ret `seq` return ret

{------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Standalone c) = 
  mapDVRCands (updDVRComp $ cStandalone vp_loc) <$> comp_vect dfs vp_vctx c

comp_vect0 dfs (VectPack {..}) (Mitigate {}) =
  vecMFail vp_loc $ text "Unexpected mitigate node in vectorization."


{------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (Until e c) =
  addDVR vp_self_dvr <$>
  vectIterComp dfs (cUntil vp_loc e) vp_tyin vp_tyout c

comp_vect0 dfs (VectPack {..}) (While e c) =
  addDVR vp_self_dvr <$>
  vectIterComp dfs (cWhile vp_loc e) vp_tyin vp_tyout c

comp_vect0 dfs (VectPack {..}) (Times ui nm e elen c) =
  addDVR vp_self_dvr <$>
  vectIterComp dfs (cTimes vp_loc ui nm e elen) vp_tyin vp_tyout c

{------------------------------------------------------------------------------}

comp_vect0 dfs vp (Map vann nm) =
  -- addDVR vp_self_dvr <$> do not add self bacause the repeat scale factors will
    vectMap dfs vp nm vann

comp_vect0 dfs vp (Repeat vann c) =
  -- addDVR vp_self_dvr <$> do not add self because the repeat scale factors will
    vectRepeat dfs vp c vann

{------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) (VectComp (fin,fout) c) = do
  -- I am not sure that the VectComp annotations are in fact used
  liftIO $ print $
    vcat [ text "VectComp, ignoring annotation."
         , text "At location:" <+> text (show vp_loc)
         ]
  comp_vect dfs vp_vctx c

{------------------------------------------------------------------------------}

comp_vect0 dfs (VectPack {..}) _other = do 
  verbose dfs $ text "comp_vect0 _other, location = " <+> text (show vp_loc)
  addDVR vp_self_dvr <$>
    vect_comp_dd dfs vp_comp (compSFDD vp_card vp_tyin vp_tyout)


{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

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
        -> VectPack
        -> EId
        -> Maybe VectAnn
        -> VecM DVRCands
vectMap dfs vp@(VectPack {..}) f vann = do
  MkComp (Repeat _ c0) _ _ <- expandMapToTakeEmit vp_loc vann vp_tyin f
  vectRepeat dfs vp c0 vann
  
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
    in select_scalefactors vctx (sfuds,sfdus,sfdds)

select_scalefactors :: CtxForVect -> ScaleFactors -> ScaleFactors
select_scalefactors vctx (sfuds,sfdus,sfdds)
  = case vctx of 
{- Treat CtxUnrestricted a bit more ... restricted. In principle we should not have to do this
   if we were to use a better utility function, and instead we'd say:
      CtxUnrestricted       -> (sfuds, sfdus, sfdds)
-}
      CtxUnrestricted       -> (sfuds, [], sfdds)
 
      CtxExCompLeft         -> (sfuds, []   , sfdds) 
      CtxExCompRight        -> ([]   , sfdus, sfdds)
      CtxExCompLeftAndRight 
       -> (filter isSteady_sfud sfuds, 
              filter isSteady_sfdu sfdus, sfdds)


logCands :: DynFlags -> Bool -> String -> DVRCands -> VecM DVRCands
logCands dfs full_blown origin cands = do
  verbose dfs $ text origin <>
    parens (text (show (Map.size cands)) <+> text "candidates")
  when full_blown $
    verbose dfs (vcat (map (text . show . dvr_vres) (Map.elems cands)))
  return cands


-- | NB: Not including 'self'
vectRepeat :: DynFlags
           -> VectPack
           -> LComp -- The iterated /computer/ 
           -> Maybe VectAnn
           -> VecM DVRCands
vectRepeat dfs (VectPack { vp_vctx  = vctx
                         , vp_tyin  = tyin
                         , vp_tyout = tyout
                         , vp_loc   = loc 
                         , vp_comp  = orig_repeat_comp }) c vann 
  = do cands <- go vann c
       when (Map.null cands) $
         vecMFail loc $
           vcat [ text "Empty vectorization, check vectorization annotations."
                , text "Computation:" 
                , nest 2 (ppr orig_repeat_comp)
                ]
       return cands
 
  where
   -- | Vectorize without restrictions and up/dn mitigate
   go Nothing c0 = do 
     let (sfuds,sfdus,sfdds)
            = repeat_scalefactors vctx (compInfo c0) tyin tyout
     vecuds <- vect_comp_ud dfs c0 sfuds >>= logCands dfs False "vect_comp_ud"
     vecdus <- vect_comp_du dfs c0 sfdus >>= logCands dfs False "vect_comp_du"
     vecdds <- vect_comp_dd dfs c0 sfdds >>= logCands dfs False "vect_comp_dd"
 
     vec_recursives <- comp_vect dfs vctx c0

     let cands = vecuds `unionDVRCands` 
                 vecdus `unionDVRCands` 
                 vecdds `unionDVRCands` vec_recursives

     -- Add self too here:
     let self = mkSelf orig_repeat_comp tyin tyout
     let res = self `addDVR` 
                 mapDVRCands (updDVRComp $ cRepeat loc Nothing) cands
     logCands dfs True "VectRepeat" res
  
   -- | Vectorize internally to /exactly/ (fin,fout) and externally up
   -- or down depending on the flag f
   go (Just ann@(Rigid f (fin,fout))) c0 = do
      vcs <- go Nothing c0
      let pred (ty1,ty2) _ = ty_match ty1 fin && ty_match ty2 fout
          vcs_matching     = Map.filterWithKey pred vcs

      -- verbose dfs $ nest 2 $ vcat (map (text . show) (dvResDVRCands vcs))

      res <- logCands dfs False "VectRepeat" $ 
               mitigateFlexi ann vctx vcs_matching
      -- Force mitigation result
      res `seq` do 
         -- verbose dfs $ text "After mitigateFlexi:"
         -- verbose dfs $ nest 2 $ vcat (map (text . show) (dvResDVRCands res))
         return res

   -- | Vectorize internally to anything <= (fin,fout) and externally up
   -- or down depending on the flag f
   go (Just ann@(UpTo f (fin,fout))) c0 = do
      vcs <- go Nothing c0
      -- verbose dfs $ text ("UpTo (fin,fout) = " ++ show (fin,fout))
      -- verbose dfs $ nest 2 $ vcat (map (text . show) (dvResDVRCands vcs))

      let pred (ty1,ty2) _ = ty_upto ty1 fin && ty_upto ty2 fout
          vcs_matching     = Map.filterWithKey pred vcs
      res <- logCands dfs False "VectRepeat" $ 
                mitigateFlexi ann vctx vcs_matching

      res `seq` do 

        -- verbose dfs $ text "After mitigateFlexi:"
        -- verbose dfs $ nest 2 $ vcat (map (text . show) (dvResDVRCands res))
        return res
 

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
mitigateFlexi :: VectAnn -> CtxForVect -> DVRCands -> DVRCands
mitigateFlexi (Rigid False _) vctx cands = cands
mitigateFlexi (UpTo False _) vctx cands  = cands
mitigateFlexi vann vctx cands = 
  let is_rigid = case vann of { Rigid {} -> True; _ -> False }
      mitigated r = 
       let ain  = tyArity (vect_in_ty  $ dvr_vres r)
           aout = tyArity (vect_out_ty $ dvr_vres r)
           dummy_card = (CAStatic ain, CAStatic aout)
           sfuds0 = compSFUD_aux dummy_card
           sfdus0 = compSFDU_aux dummy_card
           -- If Rigid, the programmer explicitly asked for this
           -- internal vectorization, so it's ok to arbitrarily
           -- down-vectorize, if not then conservatively we do not
           -- down-vectorize.
           sfdds0 = if is_rigid 
                    then compSFDD_aux dummy_card
                    else []
           (sfuds,sfdus,sfdds) = select_scalefactors vctx (sfuds0,sfdus0,sfdds0)
           arities = map sfud_arity sfuds ++
                     map sfdu_arity sfdus ++
                     map sfdd_arity sfdds
       in map (\ar -> mit_one "MF" ar r) arities
  in fromListDVRCands (concatMap mitigated (Map.elems cands))


{- The following /must not/ happen, but it used to happen due to a bug 
   in flexible mitigation, particularly removeDC was becoming 64-64 but
   externally appeared as 8-8. As a result it really had to read chunks
   of 64 but the upstream guy sent only some multiple of 8 (I believe
   just 48)

   1-{read[arr[8] complex16]}-8 >>>
    repeat seq { y{_r2223} <- 8-{___downSample_blk_vect_DD1_9635{_v9635} ()}-0
               ; 0-{___downSample_blk_vect_DD2_9643{_v9643} ()}-4 } >>> 
    seq { det{_r2644} <- 4-mitigate(MAoMF)[complex16]-64 .>>>.
      repeat 64-{___removeDC_blk_vect_DU1_9888{_v9888} ()}-64 .>>>.
      64-mitigate(MF)[complex16]-4 >>>
      seq { _unused_4622{_pf4622} <- until (...) (...)
          ; 0-{return ...}-0
-}


{-------------------------------------------------------------------------------
  DD/UD/DU Vectorization entry points (see VecScaleUp/VecScaleDn)
-------------------------------------------------------------------------------}

vect_comp_ud :: DynFlags -> LComp -> [SFUD] -> VecM DVRCands
vect_comp_ud dfs lcomp sfs 
 = fromListDVRCands <$> mapM (VecScaleUp.doVectCompUD dfs cty lcomp) sfs
 where cty = ctComp lcomp

vect_comp_du :: DynFlags -> LComp -> [SFDU] -> VecM DVRCands
vect_comp_du dfs lcomp sfs 
 = fromListDVRCands <$> mapM (VecScaleUp.doVectCompDU dfs cty lcomp) sfs
 where cty = ctComp lcomp

vect_comp_dd :: DynFlags -> LComp -> [SFDD] -> VecM DVRCands
vect_comp_dd dfs lcomp sfs 
 = do -- verbose dfs $ 
      --   vcat [ text "vect_comp_dd scalefactor list length = " <+> int (length sfs)
      --        , nest 2 $ vcat (map (text . show) sfs) ]
      rres <- mapM (VecScaleDn.doVectCompDD dfs cty lcomp) sfs
      -- verbose dfs $ 
      --   vcat [ text "vect_comp_dd, result size = " <+> int (length rres)
      --        , nest 2 $ vcat (map (text . show . dvr_vres) rres) 
      --        ]

      return (fromListDVRCands rres)

 where cty = ctComp lcomp


{-------------------------------------------------------------------------------
  Entry point to the vectorizer
-------------------------------------------------------------------------------}


initVectorizer :: IO ()
initVectorizer = writeIORef VecSF.divsOfMemo (Map.empty)


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
        let vc_opt_mit = vc_mit
        -- Maybe here we want to do a very light elimination of mitigators
        -- before a proper PassFold later on. Not clear. 
        let vc = vc_opt_mit 

        verbose dflags $ vcat [ text "Type checking vectorization candidate."
                              -- too verbose: , nest 2 $ ppr vc 
                              ]

        case ctComp vc of _ -> return vc

        return vc

  -- in Debug mode optimize compile and type check all candidates
  let maxi = getMaximal vss
 
  maxi_comp <- dvr_comp maxi  
  verbose dflags $ vcat [ text "Selected candidate is: "
                        , nest 2 $ text $ show $ dvr_vres maxi
                        -- too verbose: , nest 2 $ ppr maxi_comp
                        ] 

  final_maxi_comp <- do_one maxi

  when (isDynFlagSet dflags Debug) $ 
    T.mapM do_one vss >> return ()

  return (final_maxi_comp, []) -- Don't emit candidates that's ok


