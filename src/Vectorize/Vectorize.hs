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
import Data.Functor.Identity

import Control.Applicative  ( (<$>) )

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

computeVectTop :: DynFlags -> LComp -> VecM [DelayedVectRes]
computeVectTop dfs lcomp = do
  let compname = compShortName lcomp
  verbose dfs $
    text "--> Vectorizer, traversing:" <+> text compname 
  rs <- go CtxUnrestricted lcomp
  verbose dfs $ text "<--"
  return rs
  where
    go :: CtxForVect -> LComp -> VecM [DelayedVectRes]
    go vctx comp =
      let card  = compInfo comp
          loc   = compLoc comp
          cty   = ctComp comp
          tyin  = inTyOfCTy cty
          tyout = yldTyOfCTy cty
          self  = DVR { dvr_comp = return $ eraseComp comp
                      , dvr_vres = NotVect tyin tyout }
          warn_if_empty = warn_empty_vect dfs comp
      in
      case unComp comp of

        Var x -> lookupCVarBind x >>= go vctx

        BindMany c1 xs_cs
          -> do let sfs = compSFDD card tyin tyout
                    css = c1 : map snd xs_cs
                    xs  = map fst xs_cs

                -- Compute direct down-vectorizations
                direct_vss <- vect_comp_dd dfs comp sfs

                -- Compute recursive vectorizations
                vss <- mapM (go vctx) css
                let ress = cross_prod_mit $ map keepGroupMaximals vss
                let recursive_vss = keepGroupMaximals $
                      map (\(vc:vcs) -> combineCtrl loc vc xs vcs) ress
                warn_if_empty recursive_vss "BindMany"
      
                -- Return directs + recursives (will contain self)
                return (direct_vss ++ recursive_vss)
  

        -- Seq should have been eliminated during type checking
        Seq {} -> vecMFail loc $
                  text "Seq node encountered during vectorization."

        Par p c1 c2 -> do
          let is_c1 = isComputer (ctComp c1)
              is_c2 = isComputer (ctComp c2)
              ctx1  = if is_c2 then CtxExistsCompRight else vctx
              ctx2  = if is_c1 then CtxExistsCompLeft  else vctx
          vcs1 <- go ctx1 c1
          vcs2 <- go ctx2 c2
          let ress = keepGroupMaximals $ combineData p loc vcs1 vcs2
          warn_if_empty ress "Par"
          return ress

        Let x c1 c2 ->
        -- Safe to ignore 'c1' as the vectorizer effectively inlined
        -- computations, look at Var and Call nodes.
          extendCVarBind x c1 (go vctx c2)
        LetE x fi e c1 -> do
          vcs1 <- go vctx c1
          mapM (liftCompDVR $ cLetE loc x fi e) vcs1
        LetERef x mbe c1 -> do
          vcs1 <- go vctx c1
          mapM (liftCompDVR $ cLetERef loc x mbe) vcs1

        LetHeader fdef c1 -> do
          vcs1 <- go vctx c1
          mapM (liftCompDVR $ cLetHeader loc fdef) vcs1
        LetFunC f params c1 c2 ->
          -- Safe to ignore the function as it will be effectively inlined
          extendCFunBind f params c1 $ go vctx c2

        LetStruct sdef c2 -> do
          vcs2 <- go vctx c2
          mapM (liftCompDVR (cLetStruct loc sdef)) vcs2

        Call f es -> do
          CFunBind { cfun_params = prms, cfun_body = bdy } <- lookupCFunBind f
          vbdys <- go vctx bdy
          -- It's not very efficient to create a zillion typed names
          -- so let us create one and set its type each time.
          vf <- newVectGName "_VECT" undefined loc
          let mk_vect_call vbd
                = cLetFunC loc vf_typed prms vbd $ 
                  cCall loc vf_typed (map eraseCallArg es)
                where vf_typed = updNameTy vf (ctComp vbd)
          mapM (liftCompDVR mk_vect_call) vbdys

        Interleave c1 c2 -> return [self]

        Branch e c1 c2
          -> do let sfs = compSFDD card tyin tyout

                -- Compute the direct vectorizations
                direct_vss <- vect_comp_dd dfs comp sfs

                -- Compute the recursive vss 
                vcs1 <- go vctx c1
                vcs2 <- go vctx c2
                let ress = cross_prod_mit $ map keepGroupMaximals [vcs1,vcs2]
                recursive_vss <- mapM (\[dvr1,dvr2] ->
                   let vres1 = dvr_vres dvr1 
                       vres2 = dvr_vres dvr2 
                       vtin  = vect_in_ty  vres1
                       vtout = vect_out_ty vres1
                       vres  = if any didVect [vres1,vres2]
                               then DidVect vtin vtout u
                               else NotVect vtin vtout
                       u = vResUtil vres1 + vResUtil vres2
                   in return $
                      DVR { dvr_comp = do c1' <- dvr_comp dvr1
                                          c2' <- dvr_comp dvr2
                                          return $ cBranch loc e c1' c2'
                          , dvr_vres = vres }) ress
                warn_if_empty recursive_vss "Branch"
                
                -- Return directs + recursives (recursives will contain 'self')
                return $ direct_vss ++ recursive_vss

        Filter {} -> return [ self ] -- TODO: Implement later on

        -- Reading and writing internal or external buffers
        ReadSrc ty       -> return $ self : vect_rdwr (cReadSrc  loc) ty
        WriteSnk ty      -> return $ self : vect_rdwr (cWriteSnk loc) ty
        ReadInternal  ty b rt 
          -> return $ self : vect_rdwr (\t -> cReadInternal  loc t b rt) ty
        WriteInternal ty b 
          -> return $ self : vect_rdwr (\t -> cWriteInternal loc t b) ty
        Return _fi _e    -> return [ self ]

        -- Iterated computers
        Until e c -> do
          vss <- vect_itercomp dfs (cUntil loc e) card tyin tyout c
          return (self : vss)
        While e c -> do
          vss <- vect_itercomp dfs (cWhile loc e) card tyin tyout c
          return (self : vss)
        Times ui nm e elen c -> do
          vss <- vect_itercomp dfs (cTimes loc ui nm e elen) card tyin tyout c
          return (self : vss)

       
        -- Special cases
        Standalone c -> do 
           vcs <- go vctx c
           mapM (liftCompDVR $ cStandalone loc) vcs
        Mitigate {} ->
           vecMFail loc $ text "BUG: Asked to vectorize Mitigate node."

        -- Map
        Map vann nm -> do
          vcs <- vect_map dfs vctx nm tyin tyout loc vann
          return (self : vcs)

        -- Repeat 
        Repeat vann c -> do
          vcs <- vect_repeat dfs vctx c tyin tyout loc vann
          return (self : vcs)

        -- Annotated computer 
        VectComp (fin,fout) c -> do 
          let sfs = compSFDD card tyin tyout
          vcs <- vect_comp_dd dfs c sfs
          -- NB: False below because we want /exactly/ the candidates
          return $
            concatMap (vec_exact (error "VectComp") fin fout False loc) vcs

        -- Others: Take1/Take/Emit/Emits, just down-vectorize
        _other
          -> do vss <- vect_comp_dd dfs comp $ compSFDD card tyin tyout
                return (self : vss)


{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

-- | Emit a warning when we encounter an empty vectorization result
warn_empty_vect :: DynFlags -> LComp -> [DelayedVectRes] -> String -> VecM ()
warn_empty_vect dfs comp ress origin
  =  when (null ress) $ liftIO $ do
       print $ text "WARNING: empty vectorization" <+> braces (text origin)
       verbose dfs $ ppr comp

-- | Vectorizing read/write nodes (NB: not adding 'self')
vect_rdwr :: (Ty -> Comp) -> Ty -> [ DelayedVectRes ]
vect_rdwr builder ty
  | isVectorizable ty = map do_vect [2..vECT_IOARRAY_BOUND]
  | otherwise         = []
  where
   do_vect n
     = DVR {dvr_comp = return $ builder vty, dvr_vres = DidVect vty vty minUtil}
     where vty = mkVectTy ty n

-- | Vectorizing a (finitely) iterated computer (NB: not adding 'self')
vect_itercomp :: DynFlags -> (Comp -> Comp)
              -> Card -> Ty -> Ty -> LComp -> VecM [DelayedVectRes]
vect_itercomp dfs builder vcard tin tout c
  = do vss <- vect_comp_dd dfs c $ compSFDD vcard tin tout
       mapM (liftCompDVR builder) vss

{-------------------------------------------------------------------------------
  Vectorizing Map 
-------------------------------------------------------------------------------}

-- | To avoid duplication we vectorize Map exactly as we do for
-- repeat. Hence, below we create a node: seq { x <- take; emit f(x) }
-- and call the vectorizer for Repeat.
vect_map :: DynFlags 
         -> CtxForVect
         -> EId -> Ty -> Ty -> Maybe SourcePos
         -> Maybe VectAnn
         -> VecM [DelayedVectRes]
vect_map dfs vctx f tin tout loc vann = do
  MkComp (Repeat _ c0) _ _ <- map2take_emit loc vann tin f 
  vect_repeat dfs vctx c0 tin tout loc vann
  
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


-- | NB: Not including 'self'
vect_repeat :: DynFlags 
            -> CtxForVect
            -> LComp -- The iterated /computer/ 
            -> Ty -> Ty -> Maybe SourcePos
            -> Maybe VectAnn
            -> VecM [DelayedVectRes]
vect_repeat dynflags vctx c tyin tyout loc vann = go vann c
  where
     -- | Nested annotation on computer
   go Nothing (MkComp (VectComp hint c0) _ _) 
     = go (Just (Rigid True hint)) c0

     -- | Vectorize rigidly and up/dn mitigate
   go (Just (Rigid f (fin,fout))) c0 = do
     vcs <- go Nothing c0
     let repeat_sfs = repeat_scalefactors vctx (compInfo c0) tyin tyout
     return $ concatMap (vec_exact repeat_sfs fin fout f loc) vcs

   go (Just (UpTo f (maxin,maxout))) c0 = do
     vcs <- go Nothing c0
     let repeat_sfs = repeat_scalefactors vctx (compInfo c0) tyin tyout
     return $ concatMap (vec_upto repeat_sfs maxin maxout f loc) vcs
     -- | Vectorize without restrictions and up/dn mitigate
   go Nothing c0 = do 
     let (sfuds,sfdus,sfdds) 
            = repeat_scalefactors vctx (compInfo c0) tyin tyout
     vecuds <- vect_comp_ud dynflags c0 sfuds
     vecdus <- vect_comp_du dynflags c0 sfdus
     vecdds <- vect_comp_dd dynflags c0 sfdds
     return $ map mk_repeat (vecuds ++ vecdus ++ vecdds)

   mk_repeat :: DelayedVectRes -> DelayedVectRes
   mk_repeat (DVR { dvr_comp = io_comp, dvr_vres = vres })
     = DVR { dvr_comp = cRepeat loc Nothing <$> io_comp, dvr_vres = vres }


-- | Take a DelayedVectRes and if the vectorized array sizes are
-- within the bounds given then keep all possible up/dn mitigations.
vec_upto :: ScaleFactors -> Int -> Int
         -> Bool -> Maybe SourcePos
         -> DelayedVectRes -> [DelayedVectRes]
vec_upto sfs maxin maxout f loc dvr
  = let vtin  = vect_in_ty  $ dvr_vres dvr
        vtout = vect_out_ty $ dvr_vres dvr
    in if check_tysiz vtin  maxin &&
          check_tysiz vtout maxout
       then mit_updn_maybe sfs f loc dvr else []
  where dres = dvr_vres dvr
        check_tysiz (TArray (Literal l) _) bnd | l > bnd = False
        check_tysiz _tother _bnd = True

vec_exact :: ScaleFactors -> Int -> Int
          -> Bool -> Maybe SourcePos
          -> DelayedVectRes -> [DelayedVectRes]
vec_exact sfs maxin maxout f loc dvr
  = let vtin  = vect_in_ty  $ dvr_vres dvr
        vtout = vect_out_ty $ dvr_vres dvr
    in if check_tysiz vtin  maxin &&
          check_tysiz vtout maxout
       then mit_updn_maybe sfs f loc dvr
       -- TODO: think of what this does to the utility function?
       -- Particularly the double mitigation: the user has 
       -- specified some bounds which did not correspond to any
       -- internal mode of vectorization and he wanted to be 
       -- flexible about those. Probably that's suspicious and we
       -- should instead be throwing a warning (or an error even!)
       else do dvr'  <- mit_in loc maxin dvr
               dvr'' <- mit_out loc dvr' maxout
               mit_updn_maybe sfs f loc dvr''
  where dres = dvr_vres dvr
        check_tysiz (TArray (Literal l) _) bnd | l /= bnd = False
        check_tysiz _tother _bnd = True
 

{-------------------------------------------------------------------------------
  DD/UD/DU Vectorization entry points (see VecScaleUp/VecScaleDn)
-------------------------------------------------------------------------------}

vect_comp_ud :: DynFlags -> LComp -> [SFUD] -> VecM [DelayedVectRes]
vect_comp_ud dfs lcomp sfs = mapM (VecScaleUp.doVectCompUD dfs cty lcomp) sfs
  where cty = ctComp lcomp

vect_comp_du :: DynFlags -> LComp -> [SFDU] -> VecM [DelayedVectRes]
vect_comp_du dfs lcomp sfs = mapM (VecScaleUp.doVectCompDU dfs cty lcomp) sfs
  where cty = ctComp lcomp

vect_comp_dd :: DynFlags -> LComp -> [SFDD] -> VecM [DelayedVectRes]
vect_comp_dd dfs lcomp sfs = mapM (doVectCompDD dfs cty lcomp) sfs
  where cty = ctComp lcomp

{-------------------------------------------------------------------------------
  Flexible mitigation
-------------------------------------------------------------------------------}

-- | Add mitigators around an already-vectorized component for more flexibility.
mit_updn_maybe :: ScaleFactors -> Bool -> Maybe SourcePos 
               -> DelayedVectRes -> [DelayedVectRes]
mit_updn_maybe sfs flexi loc dvr = if flexi then mit_updn sfs loc dvr else [dvr]

mit_updn :: ScaleFactors -> Maybe SourcePos 
         -> DelayedVectRes -> [DelayedVectRes]
mit_updn (sfuds,sfdus,sfdds) loc dvr
  | DidVect {} <- dvr_vres dvr 
  = concatMap mit_aux $
    map sfud_arity sfuds ++ map sfdu_arity sfdus ++ map sfdd_arity sfdds
  where mit_aux (Nothing, Nothing) = return dvr
        mit_aux (Just n , Nothing) = mit_in  loc n dvr
        mit_aux (Just n , Just m)  = mit_out loc dvr m >>= mit_in loc n
        mit_aux (Nothing, Just m)  = mit_out loc dvr m
mit_updn _ _ dvr = [dvr] -- no point in mitigating in case of NotVect


mit_in :: Maybe SourcePos -> Int -> DelayedVectRes -> [DelayedVectRes]
mit_in loc n dvr@(DVR { dvr_comp = io_comp, dvr_vres = vres })
  | Just (final_in_ty, cmit) <- mitin
  = [ DVR { dvr_comp = cPar loc pnever cmit <$> io_comp
          , dvr_vres = DidVect final_in_ty voutty u } ]
  where vinty  = vect_in_ty vres  -- type in the middle!
        voutty = vect_out_ty vres
        mitin  = mk_in_mitigator loc n vinty
        u      = parUtility minUtil (vResUtil vres) vinty
mit_in _ _ dvr = [dvr]

mit_out :: Maybe SourcePos -> DelayedVectRes -> Int -> [DelayedVectRes] 
mit_out loc dvr@(DVR { dvr_comp = io_comp, dvr_vres = vres }) m 
  | Just (final_out_ty, cmit) <- mitout
  = [ DVR { dvr_comp = (\c -> cPar loc pnever c cmit) <$> io_comp
          , dvr_vres = DidVect vinty final_out_ty u } ]
  where vinty  = vect_in_ty vres
        voutty = vect_out_ty vres -- type in the middle!
        mitout = mk_out_mitigator loc voutty m 
        u      = parUtility minUtil (vResUtil vres) voutty
mit_out _ dvr _ = [dvr]


-- | Mitigate on the input, return final input type
mk_in_mitigator :: Maybe SourcePos -> Int -> Ty -> Maybe (Ty, Comp)
mk_in_mitigator loc n (TArray (Literal m) tbase) 
  | n > 1 || m > 1
  , n `mod` m == 0 || m `mod` n == 0
  = Just (array_ty n tbase, cMitigate loc tbase n m)
  | otherwise = Nothing 
mk_in_mitigator loc n t -- non-array
  | n > 1     = Just (array_ty n t, cMitigate loc t n 1)
  | otherwise = Nothing 

-- | Mitigate on the output, return final output type
mk_out_mitigator :: Maybe SourcePos -> Ty -> Int -> Maybe (Ty, Comp)
mk_out_mitigator loc (TArray (Literal n) tbase) m
  | n > 1 || m > 1 
  , n `mod` m == 0 || m `mod` n == 0
  = Just (array_ty m tbase, cMitigate loc tbase n m)
  | otherwise = Nothing
mk_out_mitigator loc t m -- non-array
  | m > 1     = Just (array_ty m t, cMitigate loc t 1 m)
  | otherwise = Nothing 



-- | Entry point to vectorizer 
-- The first Comp is the maximal candidate. In Debug mode we also emit
-- all possible candidates in the second component of the
-- returned pair (including the maximal)
runVectorizer :: DynFlags -> GS.Sym -> Comp -> IO (Comp,[Comp])
runVectorizer dflags sym comp = do
  verbose dflags $ text "Cardinality analysis starting."
  lcomp <- runCardAnal dflags comp
  verbose dflags $ text "Cardinality analysis finished."

  verbose dflags $ text "Vectorization starting."
  vss <- runVecM (VecEnv sym [] []) (computeVectTop dflags lcomp)
  verbose dflags $ text "Vectorization finished." <+> 
                      parens (ppr (length vss) <+> text "candidates")


  when (null vss) $ 
    panic $ vcat [ text "Empty vectorization candidate set for computation:"
                 , ppr comp ]
  
  let do_one (DVR { dvr_comp = io_comp, dvr_vres = vres }) = do
        vc_mit <- io_comp
        -- Optimize mitigators
        vc_opt_mit <- elimMitigsIO dflags sym vc_mit
        -- Compile away mitigators if flag set
        let vc = vc_opt_mit 
    
        verbose dflags $ text "Type checking vectorization candidate."
        seq (ctComp vc) $ verbose dflags $ text "Done."
        return vc

  -- in Debug mode optimize compile and type check all candidates
  let maxi = getMaximal vss
  let final_vss = if isDynFlagSet dflags Debug then (maxi : vss) else [maxi]
  comps <- mapM do_one final_vss

  return (head comps, tail comps) -- We know it's the maximal in Debug or not


