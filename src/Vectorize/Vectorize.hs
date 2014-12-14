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

import PpComp
import Outputable
import qualified GenSym as GS
import Text.Parsec.Pos
import qualified Data.Set as S
import Control.Monad.State
import Data.List as M
import Data.Functor.Identity

import Opts

import CardAnalysis -- Cardinality analysis 
import VecM         -- Vectorizer monad and infrastructure
import VecSF        -- Vectorization scale factors
import CtComp 

import PassFold ( elimMitigsIO )
import Debug.Trace


{-------------------------------------------------------------------------------
  Vectorizer proper
-------------------------------------------------------------------------------}

-- | For non-vectorizable input or output type we convert the
-- corresponding CAlpha to CAUnknown so that we can still vectorize in
-- the other queue (output or input, respectively).
vect_card :: Card -> Ty -> Ty -> Maybe (CAlpha,CAlpha)
vect_card OCard ty1 ty2 = Nothing
vect_card (SimplCard ain aout) tin tout
  = return (fix_alpha tin ain, fix_alpha tout aout)
  where fix_alpha t a | not (isVectorizable t) = CAUnknown
                      | otherwise              = aa

-- | Emit a warning when we encounter an empty vectorization result
warn_empty_vect :: DynFlags -> LComp -> [DelayedVectRes] -> String -> VecM ()
warn_empty_vect dfs comp ress origin
  =  when (null ress) $ liftIO $ do 
       print $ text "WARNING: empty vectorization" <+> braces (text origin)
       verbose dfs $ ppr comp

vectRdWr :: (Ty -> Comp) -> Ty -> [ DelayedVectRes ]
vectRdWr builder ty 
  | isVectorizable ty
  -> let vcs = map (\n ->
          let vty = mkVectTy ty n
          in DVR { dvr_comp = return (builder vty)
                 , dvr_vres = DidVect vty vty } ) [2..vECT_ARR_BOUND] 
     in return $ self : vcs
  | otherwise -> return [ self ]

-- | Vectorizing a (finitely) iterated /computer/
-- NB: not adding `self'
vectIterComp :: (Comp -> Comp) -> Card -> LComp -> VecM [DelayedVectRes]
vectIterComp builder vc c
  | Just sf <- vcard >>= compSFDD 
  -> do env <- getVecEnv 
        vss <- doVectCompDD env dfs c sf
        mapM (liftCompDVR builder) vss 
  | otherwise -> return [] 

computeVectTop :: DynFlags -> LComp -> VecM [DelayedVectRes]
computeVectTop dfs x = do 
  verbose dfs $
   text "Vectorizer, traversing" <+> text (compShortName lcomp) <+> text "{"
  go CtxUnrestricted x
  verbose dfs $ text "} Vectorizer, finished."
  where 
    go :: CtxForVect -> LComp -> VecM [DelayedVectRes]
    go vctx comp = 
      let card  = compInfo comp
          loc   = compLoc comp
          cty   = ctComp comp
          tyin  = inTyOfCTy cty
          tyout = yldTyOfCTy cty
          self  = DVR { dvr_comp = return $ eraseComp comp
                      , dvr_vres = NotVect tyin tyout
          vcard = vect_card card tyin tyout
          warn_if_empty = warn_empty_vect dfs comp
      in
      case unComp comp of

        Var x -> lookupCVarBind x >>= go vctx

        BindMany c1 xs_cs
          | Just sf <- vcard >>= compSFDD 
          -> do env <- getVecEnv 
                vss <- doVectCompDD env dfs comp sf
                return $ self : vss
          | otherwise -> do 
            let css = c1 : map snd xs_cs
                xs  = map fst xs_cs 
            vss <- mapM (go vctx) css
            let ress = cross_prod_mit $ map keepGroupMaximals vss
            warn_if_empty ress "BindMany"
            return $ keepGroupMaximals $
                     map (\(vc:vcs) -> combineCtrl loc vc xs vcs) ress

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

        LetStruct sdef c2 -> do 
          vcs2 <- go vctx c2
          mapM (liftCompDVR (cLetStruct loc sdef)) vcs2

        Let x c1 c2 -> 
          -- Safe to ignore 'c1' as the vectorizer effectively inlined
          -- computations, look at Var and Call nodes. 
          extendCVarBind x c1 (go vctx c2)

        LetHeader fdef c1 -> do 
          vcs1 <- go vctx c1
          map (liftCompDVR $ cLetHeader loc fdef) vcs1

        LetE x fi e c1 -> do 
          vcs1 <- go vctx c1
          map (liftCompDVR $ cLetE loc x fi e) vcs1

        LetFunC f params c1 c2 ->
          -- Safe to ignore the function as it will be effectively inlined
          extendCFunBind f params c1 $ go vctx c2

        Call f es -> do 
          CFunBind { cfun_params = prms, cfun_body = bdy } <- lookupCFunBind f
          vbdys <- go vctx bdy
          -- It's not very efficient to create a zillion typed names
          -- so let us create one and set its type each time.
          vf <- newVectGName "_VECT" undefined loc
          let mk_vect_call vbd 
                = cLetFunC loc () vf_typed prms vbd (cCall loc vf_typed es)
                where vf_typed = updNameTy vf (ctComp vbd)
          mapM (liftCompDVR mk_vect_call) vbdys

        Interleave c1 c2 -> return [self]

        Branch e c1 c2
          | Just sf <- vcard >>= compSFDD
          -> do env <- getVecEnv
                vss <- doVectCompDD env dfs comp sf
                return $ self : vss
          | otherwise
          -> do vcs1 <- computeVect ra c1
                vcs2 <- computeVect ra c2
                let ress = cross_prod_mit $ map keepGroupMaximals [vcs1,vcs2]
                branch_cands <- mapM (\[dvr1,dvr2] -> do
                   let vtin  = vect_in_ty  $ dvr_vres dvr1
                       vtout = vect_out_ty $ dvr_vres dvr1
                       vres  = if any (didVect . dvr_vres) [dvr1,dvr2]
                               then DidVect vtin vtout 
                               else NotVect vtin vtout
                   in
                   DVR { dvr_comp = do { c1' <- dvr_comp dvr1
                                       ; c2' <- dvr_comp dvr2 
                                       ; return $ cBranch loc e c1' c2' }
                       ; dvr_vres = vres } ) ress 
                warn_if_empty branch_cands "Branch"
                return branch_cands

        Filter {} -> return [ self ] -- TODO: Implement later on

        -- Reading and writing internal or external buffers
        ReadSrc ty       -> return $ vectRdWr (cReadSrc       loc) ty
        WriteSnk ty      -> return $ vectRdWr (cWriteSnk      loc) ty 
        ReadInternal ty  -> return $ vectRdWr (cReadInternal  loc) ty
        WriteInternal ty -> return $ vectRdWr (cWriteInternal loc) ty

        Return _fi _e    -> return [ self ]

        -- Iterated computers
        Until e c -> do 
          vss <- vectIterComp (cUntil loc e) vcard c
          return (self : vss) 
        While e c -> do 
          vss <- vectIterComp (cWhile loc e) vcard c
          return (self : vss) 
        Times ui e elen c -> do 
          vss <- vectIterComp (cTimes loc ui e elen) vcard c
          return (self : vss) 

 


**********

          VectComp (finalin,finalout) c1
            -> do { vc <- vectorizeWithHint (finalin,finalout) c1
                  ; let self = self_no_vect
                  ; let vect =
                         DVR { dvr_comp  = return vc
                             , dvr_vres  = DidVect finalin finalout minUtil
                             , dvr_orig_tyin  = tyin
                             , dvr_orig_tyout = tyout }
                  ; return $ [vect] -- No self, FORCE this!
                  }

          -- Treat nested annotations exactly the same as repeat
          Repeat Nothing (MkComp (VectComp hint c1) _ _)
            -> computeVect ra (cRepeat loc (cty,card) (Just (Rigid True hint)) c1)

          Repeat (Just (Rigid f (finalin, finalout))) c1
            -> do { vc <- vectorizeWithHint (finalin,finalout) c1
                  ; let self = self_no_vect

                  ; let vect =
                         DVR { dvr_comp  = return $
                                           cRepeat loc () Nothing vc
                             , dvr_vres  = DidVect finalin finalout minUtil
                             , dvr_orig_tyin  = tyin
                             , dvr_orig_tyout = tyout }
                  ; return $ mitUpDn_Maybe f ra loc vect
                  }

          Repeat (Just (UpTo f (maxin, maxout))) c1
             -> do { vss <- computeVect ra $ cRepeat loc (cty,card) Nothing c1
                   ; let filter_res (dvr@DVR{ dvr_vres = r })
                           = case r of NoVect -> True
                                       DidVect i j _ -> i <= maxin && j <= maxout
                   ; return $ concat $
                     map (mitUpDn_Maybe f ra loc) (filter filter_res vss)
                   }


          Repeat Nothing c -- NB: Vectorizing in anything we wish!
            | SimplCard (Just cin) (Just cout) <- snd $ compInfo c
            , isVectorizable tyin || cin == 0
            , isVectorizable tyout || cout == 0
            -> do { when verbose $ vecMIO (putStrLn "Repeat (nothing)")
                  ; let [vsf_up,vsf_dn] = compVectScaleFacts ra tyin cin cout
                  ; (sym,venv) <- getVecEnv
                  ; let vcs_ups = doVectComp sym venv c vsf_up
                  ; let vcs_dns = doVectComp sym venv c vsf_dn

                  ; let vcs = vcs_ups ++ vcs_dns

                  ; when verbose $ vecMIO (putStrLn (show $ length vcs))

                  ; let self = self_no_vect
                  ; return $ self : [ liftDVR (cRepeat loc () Nothing) vc
                                    | vc <- vcs ]
                  }
            | SimplCard mcin mcout <- snd $ compInfo c
            , isVectorizable tyin ||
                 (case mcin  of { Nothing -> True ; Just cin  -> cin == 0 })
            , isVectorizable tyout ||
                 (case mcout of { Nothing -> True ; Just cout -> cout == 0 })
            -> do {
{-
                    vecMIO $
                    putStrLn "Repeat (just: scaling down only input/output!)"
-}
                  ; let sf = compVectScaleFactDn_InOrOut mcin mcout

                  ; (sym, venv) <- getVecEnv
                  ; let vcs = doVectComp sym venv c sf

                     -- Tedious 'self' case ...
                  ; let cin_fin =
                           case mcin of
                             Nothing -> 1
                             Just cin -> cin
                  ; let cout_fin =
                           case mcout of
                             Nothing -> 1
                             Just cout -> cout
                  ; let self = self_no_vect
                                  { dvr_vres = mkNoVect cin_fin cout_fin }

                   ; return (self : [ liftDVR (cRepeat loc () Nothing) vc
                                    | vc <- vcs
                                    ])
                   }

            | otherwise
            -> do { vcs <- computeVect ra c
                  ; return [ liftDVR (cRepeat loc () Nothing) vc
                           | vc <- vcs
                           ]
                  }


          Map Nothing nm ->
            let mults = allVectMults ra tyin 1 1
                mk_vect_map env (min,mout)
                  = DVR { dvr_comp
                              = inCurrentEnv env $
                                vectMap min mout tyin tyout loc nm
                        , dvr_vres = DidVect min mout minUtil
                        , dvr_orig_tyin  = tyin
                        , dvr_orig_tyout = tyout
                        }

            in do { env <- getVecEnv
                  ; let vect_maps = map (mk_vect_map env) mults
                  ; return $ self_no_vect : vect_maps }

          Map (Just (UpTo f (min,mout))) nm ->
            let mults = filter (\(i,j) -> i <= min && j <= mout) $
                        allVectMults ra tyin 1 1
                mk_vect_map env (min,mout)
                  = DVR { dvr_comp
                              = inCurrentEnv env $
                                vectMap min mout tyin tyout loc nm
                        , dvr_vres = DidVect min mout minUtil
                        , dvr_orig_tyin  = tyin
                        , dvr_orig_tyout = tyout
                        }

            in do { env <- getVecEnv
                  ; let vect_maps = map (mk_vect_map env) mults
                  ; return $ concat $ map (mitUpDn_Maybe f ra loc) $
                             self_no_vect : vect_maps
                  }

          Map (Just (Rigid f (min,mout))) nm
            | min `mod` mout == 0          -- mout divides min
            -> let mults = [(min,mout)]
                   mk_vect_map env (min,mout)
                     = DVR { dvr_comp
                                = inCurrentEnv env $
                                  vectMap min mout tyin tyout loc nm
                           , dvr_vres = DidVect min mout minUtil
                           , dvr_orig_tyin  = tyin
                           , dvr_orig_tyout = tyout
                           }

               in do { env <- getVecEnv
                     ; let vect_maps = map (mk_vect_map env) mults
                     ; return $ concat $ map (mitUpDn_Maybe f ra loc) vect_maps
                     }
            | otherwise
            -> vecMFail "Vectorization failure, bogus map annotation!"

          _other_simpl_comp0
             | SimplCard (Just cin) (Just cout) <- card
             , isVectorizable tyin  || cin == 0
             , isVectorizable tyout || cout == 0
             -> do { let sf = compVectScaleFactDn ra cin cout
                   ; (sym,venv) <- getVecEnv
                   ; let vss = doVectComp sym venv comp sf
                   ; return $
                     self_no_vect { dvr_vres = mkNoVect cin cout } : vss
                   }
             | otherwise
             -> do { vecMIO $
                     do { putStrLn "WARNING: NOT vectorizing:"
                        ; putStrLn $ "In-type     = " ++ show tyin
                        ; putStrLn $ "Out-type    = " ++ show tyout
                        ; putStrLn $ "Cardinality = " ++ show card
                        ; print $ ppr (eraseComp comp)
                        ; putStrLn $ "Returning self."
                        }
                   ; return $ [self_no_vect]
                   }








mitUpDn :: RateAction -> Maybe SourcePos -> DelayedVectRes -> [ DelayedVectRes ]
mitUpDn ra loc dvr@(DVR { dvr_comp       = mk_c
                        , dvr_vres       = r
                        , dvr_orig_tyin  = tin
                        , dvr_orig_tyout = tout
                        })
  = case r of
      NoVect
        -> [ dvr ]
      DidVect ain aout u
        | let x1 = if ain == 0 then 1 else ain
        , let x2 = if aout == 0 then 1 else aout
        , let avms = allVectMults ra undefined ain aout
        -> dvr : [ dvr { dvr_comp = do { c   <- mk_c
                                       ; let c'  = mit_dn c (m1*ain) ain tin
                                       ; let c'' = mit_up c' aout (m2*aout) tout
                                       ; return c''
                                       }



                       , dvr_vres =
                            let u' = chooseParUtility
                                        (chooseParUtility minUtil u (x1,m1*x1))
                                        minUtil (x2,m2*x2)
                            in DidVect (m1*ain) (m2*aout) u' -- u?
                       }
                 | (m1,m2) <- avms
                 ]
        | otherwise
        -> [ dvr ]

  where mit_dn c hi lo_c tin
           | lo_c == 0 || hi == lo_c
           = c
           | otherwise
           = cPar loc () pnever (cMitigate loc () tin hi lo_c) c
        mit_up c lo_c hi tout
           | lo_c == 0 || hi == lo_c
           = c
           | otherwise
           = cPar loc () pnever c (cMitigate loc () tout lo_c hi)



mitUpDn_Maybe :: Bool
              -> RateAction
              -> Maybe SourcePos
              -> DelayedVectRes -> [ DelayedVectRes ]
mitUpDn_Maybe flexi ra loc vres
  = if flexi then mitUpDn ra loc vres else [vres]



doVectComp :: GS.Sym
           -> VecEnv
           -> Comp (CTy, Card) Ty -> VectScaleFact
           -> [DelayedVectRes]
doVectComp gs venv comp (VectScaleUp cin cout mults)
  = map do_vect_up mults
  where
    do_vect_up (min,mout)
      = DVR { dvr_comp = inCurrentEnv (gs,venv) $
                         doVectorizeCompUp comp cin cout (min,mout)
            , dvr_vres = DidVect (cin*min) (cout*mout) minUtil
            , dvr_orig_tyin  = inTyOfCTyBase  (fst $ compInfo comp)
            , dvr_orig_tyout = yldTyOfCTyBase (fst $ compInfo comp)
            }

doVectComp gs venv comp (VectScaleDn cin cout divs)
  = map do_vect_dn divs
  where
    do_vect_dn (divin,divout)
      = DVR { dvr_comp = inCurrentEnv (gs,venv) $
                         doVectorizeCompDn comp cin cout (divin,divout)
            , dvr_vres = DidVect divin divout minUtil
            , dvr_orig_tyin  = inTyOfCTyBase  (fst $ compInfo comp)
            , dvr_orig_tyout = yldTyOfCTyBase (fst $ compInfo comp)
            }

doVectComp gs venv comp (VectScaleDnInOrOut cinout divs)
  = map do_vect_inout_dn divs
  where
    do_vect_inout_dn d
      = let vcomp = inCurrentEnv (gs,venv) $
                    doVectorizeCompDnInOrOut comp cinout d
            (divin,divout)
               = case cinout of
                   Left {}  -> (d,1) -- only vectorized input
                   Right {} -> (1,d) -- only vectorized output
        in DVR { dvr_comp = vcomp
               , dvr_vres = DidVect divin divout minUtil
               , dvr_orig_tyin  = inTyOfCTyBase  (fst $ compInfo comp)
               , dvr_orig_tyout = yldTyOfCTyBase (fst $ compInfo comp)
               }

computeVectTop :: Bool -> Comp (CTy, Card) Ty -> VecM [DelayedVectRes]
computeVectTop verbose = computeVect FlexiRate
  where
    computeVect ra x
       = do { when verbose $ vecMIO $ putStrLn $
              "Vectorizer, traversing: " ++ compShortName x
            ; r <- go ra x
            ; when verbose $ vecMIO $ putStrLn "... finished."
            ; return r
            }
    go ra comp =
        let (cty,card) = compInfo comp
            loc        = compLoc comp
            tyin       = inTyOfCTyBase cty
            tyout      = yldTyOfCTyBase cty
            self_no_vect =
              DVR { dvr_comp       = return (eraseComp comp)
                  , dvr_vres       = NoVect
                  , dvr_orig_tyin  = tyin
                  , dvr_orig_tyout = tyout }

        in
        case unComp comp of
          Var x -> lookupCVarBind x >>= computeVect ra
          BindMany c1 xs_cs
            | SimplCard (Just cin) (Just cout) <- card
            , isVectorizable tyin  || cin  == 0
            , isVectorizable tyout || cout == 0
            -> do { let sf = compVectScaleFactDn ra cin cout
                  ; (gs,venv) <- getVecEnv
                  ; let vss = doVectComp gs venv comp sf
                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return $ self : vss
                  }

            | otherwise
            -> do { let css = c1 : map snd xs_cs
                  ; let xs  = map fst xs_cs

                    -- Step 1: vectorize css
                  ; vss <- mapM (computeVect ra) css

{-
                  ; vecMIO $
                    do { putStrLn "(Bind) vss, lengths of each cand. set."
                       ; mapM (\(c,vs) -> do { -- putStrLn $ "** Computation is = " ++ show c
                                               putStrLn $ "** Candidates     = " ++ show (length vs)
                                             }) (zip css vss)
                       }
-}

                    -- Step 2: form candidates (lazily)
                  ; env <- getVecEnv
                  ; let ress = matchControl env (map pruneMaximal vss)

                  ; let builder = \(c:cs) -> cBindMany loc () c (zip xs cs)

{-
                  ; vecMIO $
                    mapM (\dvr-> putStrLn $ "DVR vres=" ++
                                         show (dvr_vres dvr))
                         (map (mkBindDelayedVRes builder) ress)
                  ; vecMIO $
                    putStrLn $
                    "(Bind) Length of ress = " ++ show (length ress)
-}

                  ; when (null ress) $ vecMIO $
                    do { putStrLn "WARNING: BindMany empty vectorization:"
                       ; print $ ppr comp }


                    -- Step 3: build for each candidate in ress a BindMany
                  ; return $
                    pruneMaximal $ map (mkBindDelayedVRes builder) ress

                  }


          Par p c1 c2
            -> do { -- See Note [RateAction]
                    let ra1 = if hasDoneTyBase $ fst (compInfo c2)
                              then RigidRate
                              else ra

                  ; vcs1 <- computeVect ra1 c1 -- NB: /not/ ra
                  ; vcs2 <- computeVect ra c2

                  ; let dbgv x xs =
                         vecMIO $
                          do { putStrLn $ "(Par) comp = " ++ show x
                             ; putStrLn $ "(Par) len  = " ++ show (length xs)
                             ; when (length xs < 400) $
                                 mapM_ (\w -> putStrLn $ show (dvr_vres w)) xs
                             ; putStrLn "----------------------"
                             }

{-
                  ; dbgv c1 vcs1
                  ; dbgv c2 vcs2
-}
                  ; env <- getVecEnv

                  ; let ress_pre = matchData env p loc vcs1 vcs2

{-
                  ; when verbose $
                    vecMIO $
                    putStrLn $ "(Par) Length ress_pre = " ++
                                       show (length ress_pre)
-}

                  ; let ress = pruneMaximal ress_pre

{-
                  ; vecMIO $
                    putStrLn $ "(Par) Length ress = " ++ show (length ress)
-}

                  ; when (null ress) $ vecMIO $
                    do { putStrLn "WARNING: Par empty vectorization:"
                       ; print $ ppr comp

                       }

                  ; return ress
                  }
          LetStruct sdef c2
            -> do { vcs2 <- computeVect ra c2
                  ; return $
                    [ liftDVR (cLetStruct loc () sdef) dvr
                    | dvr <- vcs2 ]
                  }
          Let x c1 c2
            -> do { vcs2 <- extendCVarBind x c1 $ computeVect ra c2
                  ; return $
                    [ liftDVR (cLet loc () x (eraseComp c1)) dvr
                    | dvr <- vcs2 ]
                  }
          -- CL
          LetHeader f fdef@(MkFun (MkFunExternal {}) _ _) c1
            -> do { vcs1 <- computeVect ra c1
                  ; return $
                    [ liftDVR (cLetHeader loc () f (eraseFun fdef)) dvr
                    | dvr <- vcs1 ]
                  }

          LetE x fi e c1
            -> do { vcs1 <- computeVect ra c1
                  ; return $
                    [ liftDVR (cLetE loc () x fi (eraseExp e)) dvr
                    | dvr <- vcs1 ]
                 }
          -- CL
          LetHeader x fn@(MkFun (MkFunDefined {}) _ _) c1
            -> do { vcs1 <- computeVect ra c1
                  ; return $
                    [ liftDVR (cLetHeader loc () x (eraseFun fn)) dvr
                    | dvr <- vcs1 ]
                  }
          --
          LetFunC f params locals c1 c2
            -> do { vcs2 <- extendCFunBind f params locals c1 $
                            computeVect ra c2
                  ; return $
                    [ liftDVR (cLetFunC loc () f params (eraseLocals locals)
                                                        (eraseComp c1)) dvr
                    | dvr <- vcs2
                    ]
                  }
          Call f es
            -> do { CFunBind { cfun_params = prms
                             , cfun_locals = lcls
                             , cfun_body   = bdy } <- lookupCFunBind f
                  ; vbdys <- computeVect ra bdy
                                 -- TODO: add (computation) params in context
                  ; let new_f = f  { name = name f ++ "_VECTORIZED" }
                                 -- TODO: proper uniq generation
                  ; return [ liftDVR mk_call dvr
                           | dvr <- vbdys
                           , let mk_call bd
                                   = cLetFunC loc ()
                                         new_f prms (eraseLocals lcls) bd $
                                         cCall loc () new_f $
                                         map eraseCallArg es
                           ]
                  }
          Interleave c1 c2
            -> return $ [ self_no_vect ]

          Branch e c1 c2
            | SimplCard (Just cin) (Just cout) <- card
            , isVectorizable tyin  || cin  == 0
            , isVectorizable tyout || cout == 0
            -> do { let sf = compVectScaleFactDn ra cin cout
                  ; (sym,venv) <- getVecEnv
                  ; let vss = doVectComp sym venv comp sf
                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return (self : vss)
                  }

            | otherwise
            -> do { -- when verbose $ vecMIO (putStrLn "Branch/other")
                  ; vcs1 <- computeVect ra c1
                  ; vcs2 <- computeVect ra c2

                  ; env <- getVecEnv
                  ; let ress = matchControl env (map pruneMaximal [vcs1,vcs2])

                  ; let builder = \([x1,x2])-> cBranch loc () (eraseExp e) x1 x2
                        branch_cands = pruneMaximal $ map (mkBindDelayedVRes builder) ress

{-
                  ; vecMIO $
                    do { putStrLn "(Branch) vss, lengths of each cand. set."
                       -- ; putStrLn $ "Branch itself is: " ++ show comp
                       ; mapM (\(c,vs) -> do { -- putStrLn $ "** Computation is = " ++ show c
                                               putStrLn $ "** Candidates     = " ++ show (length vs)
                                             }) (zip [c1,c2] [vcs1,vcs2])
                       ; putStrLn $ "Branch candidate length = " ++ show (length ress)
                       ; putStrLn $ "Branch pruned cands     = " ++ show (length branch_cands)
                       }
-}

                  ; when (null ress) $
                    vecMIO $
                    do { putStrLn "WARNING: Branch empty vectorization:"
                       ; print $ ppr comp }



                  -- ; vecMIO $ do { putStrLn $ "Branch candidates:"
                  --               ; mapM (\v -> putStrLn $ "dvr_vres = " ++ show (dvr_vres v)) branch_cands
                  --               }


                  ; return $ branch_cands
                  }


          VectComp (finalin,finalout) c1
            -> do { vc <- vectorizeWithHint (finalin,finalout) c1
                  ; let self = self_no_vect
                  ; let vect =
                         DVR { dvr_comp  = return vc
                             , dvr_vres  = DidVect finalin finalout minUtil
                             , dvr_orig_tyin  = tyin
                             , dvr_orig_tyout = tyout }
                  ; return $ [vect] -- No self, FORCE this!
                  }

          -- Treat nested annotations exactly the same as repeat
          Repeat Nothing (MkComp (VectComp hint c1) _ _)
            -> computeVect ra (cRepeat loc (cty,card) (Just (Rigid True hint)) c1)

          Repeat (Just (Rigid f (finalin, finalout))) c1
            -> do { vc <- vectorizeWithHint (finalin,finalout) c1
                  ; let self = self_no_vect

                  ; let vect =
                         DVR { dvr_comp  = return $
                                           cRepeat loc () Nothing vc
                             , dvr_vres  = DidVect finalin finalout minUtil
                             , dvr_orig_tyin  = tyin
                             , dvr_orig_tyout = tyout }
                  ; return $ mitUpDn_Maybe f ra loc vect
                  }

          Repeat (Just (UpTo f (maxin, maxout))) c1
             -> do { vss <- computeVect ra $ cRepeat loc (cty,card) Nothing c1
                   ; let filter_res (dvr@DVR{ dvr_vres = r })
                           = case r of NoVect -> True
                                       DidVect i j _ -> i <= maxin && j <= maxout
                   ; return $ concat $
                     map (mitUpDn_Maybe f ra loc) (filter filter_res vss)
                   }


          Repeat Nothing c -- NB: Vectorizing in anything we wish!
            | SimplCard (Just cin) (Just cout) <- snd $ compInfo c
            , isVectorizable tyin || cin == 0
            , isVectorizable tyout || cout == 0
            -> do { when verbose $ vecMIO (putStrLn "Repeat (nothing)")
                  ; let [vsf_up,vsf_dn] = compVectScaleFacts ra tyin cin cout
                  ; (sym,venv) <- getVecEnv
                  ; let vcs_ups = doVectComp sym venv c vsf_up
                  ; let vcs_dns = doVectComp sym venv c vsf_dn

                  ; let vcs = vcs_ups ++ vcs_dns

                  ; when verbose $ vecMIO (putStrLn (show $ length vcs))

                  ; let self = self_no_vect
                  ; return $ self : [ liftDVR (cRepeat loc () Nothing) vc
                                    | vc <- vcs ]
                  }
            | SimplCard mcin mcout <- snd $ compInfo c
            , isVectorizable tyin ||
                 (case mcin  of { Nothing -> True ; Just cin  -> cin == 0 })
            , isVectorizable tyout ||
                 (case mcout of { Nothing -> True ; Just cout -> cout == 0 })
            -> do {
{-
                    vecMIO $
                    putStrLn "Repeat (just: scaling down only input/output!)"
-}
                  ; let sf = compVectScaleFactDn_InOrOut mcin mcout

                  ; (sym, venv) <- getVecEnv
                  ; let vcs = doVectComp sym venv c sf

                     -- Tedious 'self' case ...
                  ; let cin_fin =
                           case mcin of
                             Nothing -> 1
                             Just cin -> cin
                  ; let cout_fin =
                           case mcout of
                             Nothing -> 1
                             Just cout -> cout
                  ; let self = self_no_vect
                                  { dvr_vres = mkNoVect cin_fin cout_fin }

                   ; return (self : [ liftDVR (cRepeat loc () Nothing) vc
                                    | vc <- vcs
                                    ])
                   }

            | otherwise
            -> do { vcs <- computeVect ra c
                  ; return [ liftDVR (cRepeat loc () Nothing) vc
                           | vc <- vcs
                           ]
                  }

          Filter e
            -> return $ [ self_no_vect ]



          ReadSrc (RWRealTyAnn ty)
            | isVectorizable tyout
            -> let r = self_no_vect
                         { dvr_comp = return $
                                      cReadSrc loc () (RWBaseTyAnn ty)
                         , dvr_vres = DidVect 0 0 minUtil
                         }
               in return [r]

          ReadSrc other_ann
            | isVectorizable tyout
            -> return [ self_no_vect { dvr_vres = DidVect 0 0 minUtil } ]

          WriteSnk (RWRealTyAnn ty)
            | isVectorizable tyin
            -> let r = self_no_vect
                         { dvr_comp = return $
                                      cWriteSnk loc () (RWBaseTyAnn ty)
                         , dvr_vres = DidVect 0 0 minUtil
                         }
               in return [r]

          WriteSnk other_ann
            | isVectorizable tyin
            -> return [ self_no_vect { dvr_vres = DidVect 0 0 minUtil } ]


          ReadInternal bid tp
            | isVectorizable tyout
            -> return [ self_no_vect { dvr_vres = DidVect 0 0 minUtil } ]

          WriteInternal bid
            | isVectorizable tyin
            -> return [ self_no_vect { dvr_vres = DidVect 0 0 minUtil } ]

          Return _fi e
            | isVectorizable tyin  || isBufTy tyin
            , isVectorizable tyout || isBufTy tyout
            -> return [ self_no_vect { dvr_vres = DidVect 0 0 minUtil } ]


          Until e c1
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , isVectorizable tyin  || cin  == 0
            , isVectorizable tyout || cout == 0
            -> do { let sf = compVectScaleFactDn ra cin cout
                  ; (sym,venv) <- getVecEnv
                  ; let vss = doVectComp sym venv c1 sf
                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return $
                    self : [ liftDVR (cUntil loc () (eraseExp e)) vc
                           | vc <- vss
                           ] }
            | otherwise
            -> return [ self_no_vect ]

          While e c1
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , isVectorizable tyin  ||  cin == 0
            , isVectorizable tyout || cout == 0
            -> do { let sf  = compVectScaleFactDn ra cin cout
                  ; (sym,venv) <- getVecEnv
                  ; let vss = doVectComp sym venv c1 sf

                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return $
                    self : [ liftDVR (cWhile loc () (eraseExp e)) vc
                           | vc <- vss
                           ] }
            | otherwise
            -> return [ self_no_vect ]

          Times ui e elen x c1
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , isVectorizable tyin  || cin == 0
            , isVectorizable tyout || cout == 0
            -> do { -- when verbose $ vecMIO (putStrLn "Times")
                  ; let sf_down = compVectScaleFactDn ra cin cout
                  ; (sym,venv) <- getVecEnv
                  ; let vss = doVectComp sym venv c1 sf_down
                  ; let downvects
                          = [ liftDVR (cTimes loc () ui (eraseExp e)
                                                        (eraseExp elen) x) vc
                            | vc <- vss
                            ]

                    -- Moreover, if 'elen' is a constant expression
                    -- then we can also scale up!
                  ; let sf_ups
                          | MkExp (EVal (VInt n')) _ _ <- elen
                          , let n = fromIntegral n'
                          , MkExp (EVal (VInt 0)) _ _ <- e
                          , VectScaleUp cin cout mults
                               <- compVectScaleFactUp ra tyin cin cout
                          , cin > 0
                          = let one_mult (min,mout)
                                   = n `mod` (cin*min) == 0 && n >= cin*min
                                build_mult (min,mout)
                                   = (n `div` (cin*min),
                                        VectScaleUp cin cout [(min,mout)])
                                possible_mults = filter one_mult mults
                            in map build_mult possible_mults
                         | otherwise
                         = []

                  ; upvects <- mapM (\(n',sf_up) ->
                       do { (sym,venv) <- getVecEnv
                          ; let ups = doVectComp sym venv c1 sf_up
                          ; return [ liftDVR (cTimes loc ()
                                                 ui
                                                 (eraseExp e)
                                                 (eVal loc () (vint n')) x) vc
                                   | vc <- ups
                                   ]
                          }) sf_ups

                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return $ self : downvects ++ concat upvects

                  }

            | otherwise
            -> do { vecMIO $ putStrLn $
                    "WARNING: Weird Times in vectorization!"
                  ; return [ self_no_vect ]
                  }


          Map Nothing nm ->
            let mults = allVectMults ra tyin 1 1
                mk_vect_map env (min,mout)
                  = DVR { dvr_comp
                              = inCurrentEnv env $
                                vectMap min mout tyin tyout loc nm
                        , dvr_vres = DidVect min mout minUtil
                        , dvr_orig_tyin  = tyin
                        , dvr_orig_tyout = tyout
                        }

            in do { env <- getVecEnv
                  ; let vect_maps = map (mk_vect_map env) mults
                  ; return $ self_no_vect : vect_maps }

          Map (Just (UpTo f (min,mout))) nm ->
            let mults = filter (\(i,j) -> i <= min && j <= mout) $
                        allVectMults ra tyin 1 1
                mk_vect_map env (min,mout)
                  = DVR { dvr_comp
                              = inCurrentEnv env $
                                vectMap min mout tyin tyout loc nm
                        , dvr_vres = DidVect min mout minUtil
                        , dvr_orig_tyin  = tyin
                        , dvr_orig_tyout = tyout
                        }

            in do { env <- getVecEnv
                  ; let vect_maps = map (mk_vect_map env) mults
                  ; return $ concat $ map (mitUpDn_Maybe f ra loc) $
                             self_no_vect : vect_maps
                  }

          Map (Just (Rigid f (min,mout))) nm
            | min `mod` mout == 0          -- mout divides min
            -> let mults = [(min,mout)]
                   mk_vect_map env (min,mout)
                     = DVR { dvr_comp
                                = inCurrentEnv env $
                                  vectMap min mout tyin tyout loc nm
                           , dvr_vres = DidVect min mout minUtil
                           , dvr_orig_tyin  = tyin
                           , dvr_orig_tyout = tyout
                           }

               in do { env <- getVecEnv
                     ; let vect_maps = map (mk_vect_map env) mults
                     ; return $ concat $ map (mitUpDn_Maybe f ra loc) vect_maps
                     }
            | otherwise
            -> vecMFail "Vectorization failure, bogus map annotation!"

          _other_simpl_comp0
             | SimplCard (Just cin) (Just cout) <- card
             , isVectorizable tyin  || cin == 0
             , isVectorizable tyout || cout == 0
             -> do { let sf = compVectScaleFactDn ra cin cout
                   ; (sym,venv) <- getVecEnv
                   ; let vss = doVectComp sym venv comp sf
                   ; return $
                     self_no_vect { dvr_vres = mkNoVect cin cout } : vss
                   }
             | otherwise
             -> do { vecMIO $
                     do { putStrLn "WARNING: NOT vectorizing:"
                        ; putStrLn $ "In-type     = " ++ show tyin
                        ; putStrLn $ "Out-type    = " ++ show tyout
                        ; putStrLn $ "Cardinality = " ++ show card
                        ; print $ ppr (eraseComp comp)
                        ; putStrLn $ "Returning self."
                        }
                   ; return $ [self_no_vect]
                   }


vectorizeWithHint (finalin,finalout) c
  -- Scale up!
  | SimplCard (Just cin) (Just cout) <- card
  , finalin  `mod` cin == 0      -- cin divides finalin
  , finalout `mod` cout == 0     -- cout divides finalout
  , let min  = finalin `div` cin
  , let mout = finalout `div` cout
  , min `mod` mout == 0          -- mout divides min
  , isVectorizable tyin || cin == 0
  , isVectorizable tyout || cout == 0
  = do { -- vecMIO (putStrLn "Repeat (just: scaling up)")
       ; doVectorizeCompUp c cin cout (min,mout)
       }

  -- or Scale down!
  | SimplCard (Just cin) (Just cout) <- card
  , cin `mod` finalin == 0
  , cout `mod` finalout == 0
  , isVectorizable tyin || cin == 0
  , isVectorizable tyout || cout == 0
  = do { -- vecMIO (putStrLn "Repeat (just: scaling down)")
       ; doVectorizeCompDn c cin cout (finalin,finalout)
       }

  -- If it is not a simple cardinality then we will just trust the annotation.
  | not (isSimplCard card)
  , isVectorizable tyin
  , isVectorizable tyout
  = doVectorizeCompForce c (finalin, finalout)

  | otherwise
  = vecMFail $
    "Vectorization failure, (annotation/analysis mismatch) for:" ++ show c

  where (cty,card) = compInfo c
        tyin       = inTyOfCTyBase cty
        tyout      = yldTyOfCTyBase cty






mkBindDelayedVRes :: ([Comp () ()] -> Comp () ())
                  -> [DelayedVectRes] -> DelayedVectRes
mkBindDelayedVRes f vs@(mk_v1:mk_v1s)
  = mk_v1 { dvr_comp = mk_bind_many
          , dvr_vres = mk_vres_many
          }
  where
    u = chooseBindUtility $
        map (vectResUtil . dvr_vres) vs
    mk_bind_many
      = do { x <- sequence (map dvr_comp vs)
           ; return $ f x }
    mk_vres_many
      = case assert_all_equal_vres vs of
         () ->
             let dids = filter (did_vect . dvr_vres) vs
             in case dids of
                  []          -> NoVect
                  (one_did:_) -> (dvr_vres one_did) { vect_util = u }

mkBindDelayedVRes _f [] = error "mkBindDelayedVRes: empty list"

assert_all_equal_vres vs =
  let tmp = map dvr_vres (filter (did_vect . dvr_vres) vs)
      ins  = map (\(DidVect i o _) -> i) tmp
      outs = map (\(DidVect i o _) -> o) tmp
  in if all_eq ins && all_eq outs then () else error "assert_all_equal_vres FAILURE!"

did_vect (DidVect {}) = True
did_vect NoVect       = False

all_eq []     = True
all_eq (x:[]) = True
all_eq (x:y:xs) = (x==y) && all_eq (y:xs)



{- Entry point to the vectorizer
 - ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

runDebugVecM :: DynFlags
             -> Comp CTy Ty
             -> TyDefEnv
             -> Env
             -> CEnv
             -> GS.Sym -> TcMState -> IO [Comp CTy Ty]
runDebugVecM dflags comp tenv env cenv sym unifiers
  = let verbose = isDynFlagSet dflags Verbose
    in
    do { -- First run cardinality analysis
       ; when verbose $ putStrLn "Vectorization starting ..."
       ; ccomp <- runCardinalityAnalysis verbose comp
       ; when verbose $
         putStrLn "Cardinality analysis finished, starting vectorization ..."
       ; (vss,_vstate)
             <- let vec_action = computeVectTop verbose ccomp
                in runVecM vec_action sym (VecEnv [] []) (VecState 0 0)

       ; putStrLn "====>"
       ; putStrLn $ "Vectorizer: length of results is: " ++ show (length vss)
         -- Now do the final selection!
       ; vs_maxi <- filterMaximal vss

       ; let do_one (DVR { dvr_comp = io_comp, dvr_vres = _vres })
                = do { vc_mit <- io_comp
                       -- Fuse mitigators
                     ; vc_opt_mit <- elimMitigsIO sym vc_mit
                       -- Compile away remaining mitigators
                     ; vc <- if isDynFlagSet dflags NativeMitigators
                             then return vc_opt_mit
                             else -- compile them away
                                  compileMitigs sym vc_opt_mit

                     ; res <- runTcM (tyCheckTopComp vc)
                                  tenv env cenv sym GlobalDefs unifiers
                     ; case res of
                         Left err
                          -> do { putStrLn "Type error in vectorization result."
                                ; print err
                                ; print (ppr vc)
                                ; error "Vectorization bug!" }
                         Right (tcv,_st)
                          -> return tcv
                     }
        ; mapM do_one [vs_maxi]
          -- mapM do_one vss
       }

