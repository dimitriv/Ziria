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

import PpComp
import Outputable
import qualified GenSym as GS
import Text.Parsec.Pos
import qualified Data.Set as S
import Control.Monad.State
import Data.List as M
import Data.Functor.Identity

import Opts

-- Cardinality analysis and vectorization imports
import CardAnalysis
import VecM


import VecMonad
-- import VecScaleUp    -- Up-vectorization
-- import VecScaleDn    -- Down-vectorization
-- import VecScaleForce -- Force vectorization

import TcMonad
import TcComp
import TcErrors ( ErrCtx (..) )
import PassFold ( elimMitigsIO )
import Debug.Trace

{-------------------------------------------------------------------------------
  Delayed results of vectorization
-------------------------------------------------------------------------------}

-- | Delayed vectorization result
data DelayedVectRes
   = DVR { dvr_comp       :: IO Comp -- Delayed result
         , dvr_vres       :: VectRes -- Information about the result
         , dvr_orig_tyin  :: Ty      -- Original (pre-vect) in  type
         , dvr_orig_tyout :: Ty      -- Original (pre-vect) out type
         }

-- | Lift an operation on VectRes to be on a DelayedVectRes
lift_dvr :: (VectRes -> a) -> DelayedVectRes -> a
lift_dvr f = f . dvr_vres

-- | Utility of delayed vectorization
dvResUtil :: DelayedVectRes -> Double
dvResUtil = lift_dvr vResUtil

-- | Keep a result with maximal utility for each group by vResEqQ
keepGroupMaximals :: [DelayedVectRes] -> [DelayedVectRes]
keepGroupMaximals xs = map getMaximal (groups x)
  where groups = groupBy' $ vResEqC <| dvr_vres

-- | Return a result with maximal utility
getMaximal :: [DelayedVectRes] -> DelayedVectRes
getMaximal = maximumBy $ compare <| dvResUtil 


{-------------------------------------------------------------------------------
  Matching on the control path
-------------------------------------------------------------------------------}

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
-- NB: 'matchControl' may introduce mitigators to make sure that
-- queues match.  These mitigate between the ``greatest common
-- divisor'' type of all input types and each individual input type,
-- and similarly between each individual output type and the
-- ``greatest common divisor'' type of all output types.  See VecM.hs
-- for the definition of gcdTys.
matchControl :: [ [DelayedVectRes] ] -> [ [DelayedVectRes] ]
matchControl bcands 
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
       vect_tin     = vec_in_ty vres
       vect_tout    = vec_out_ty vres
       new_dvr_comp = do
          comp <- dvr_comp dvr
          return $ (exp_tin,vect_tin) `mitIn` comp `mitOut` (vect_tout,exp_tout)
       vres_new = (dvr_vres dvr) { vec_in_ty = exp_tin, vec_out_ty = exp_tout }

-- | Mitigate by creating a Mitigate node (maybe) node between tin1 ~> tin2
-- Pre-condition: tin1 is a ``type divisor'' of tin2 or vice-versa.
-- Result is a mitigating (ST T tin1 tin2) transformer.  
mkMit :: Ty -> Ty -> Maybe Comp 
mkMit tin1 tin2
    -- If the two types are equal no need for mitigation
  | tin1 == tin2 = Nothing
    -- If one is array but other non-array then the latter must be 
    -- the base type of the former.  
    -- We must up-mitigate: 1 ~> len
  | not (isArrayTy tin1)
  , TArray (Literal len) tbase <- tin2
  = assert (tbase == tin1) $ 
    return $ cMitigate loc () tbase 1 len
    -- Symmetric to previous case 
  | not (isArrayTy tin2)
  , TArray (Literal len) tbase <- tin1
  = assert (tbase == tin2) $
    return $ cMitigate loc () tbase len 1
    -- If both types are arrays (of different lenghts) let's mitigate 
  | TArray (Literal len1) tbase1 <- tin1
  , TArray (Literal len2) tbase2 <- tin2
  = assert (tbase1 == tbase2) $ 
    return $ cMitigate loc () tbase1 len1 len2
  | otherwise
  = panic $ text "mkMit: can't mitigate:" <+> 
            ppr tin1 <+> text "~>" ppr tin2

-- | (gty,ty) `mitIn` comp
-- Mitigates in the input type of comp
mitIn :: (Ty,Ty) -> Comp -> Comp
mitIn (gty,ty) comp
  | Just m <- mkMit gty ty
  = cPar (compLoc comp) (mkParInfo NeverPipeline) m comp
  | otherwise = comp

-- | comp `mitOut` (ty,gty)
-- Mitigates on the output type of comp, symmetrically to before. 
mitOut :: Comp -> (Ty,Ty) -> Comp 
mitOut comp ty gty
  | Just m <- mkMit ty gty
  = cPar (compLoc comp) (mkParInfo NeverPipeline) comp m
  | otherwise = comp

{-------------------------------------------------------------------------------
  Matching on the data path
-------------------------------------------------------------------------------}

-- | Match vectorization candidates composed on the data path
matchData :: ParInfo
          -> Maybe SourcePos
          -> [ DelayedVectRes ] -> [ DelayedVectRes ] -> [ DelayedVectRes ]
matchData p loc xs ys 
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
      , dvr_vres = vres
      , dvr_orig_tyin  = dvr_orig_tyin  dp1
      , dvr_orig_tyout = dvr_orig_tyout dp2 }
  where
    mk_par ioc1 ioc2 = do
      c1 <- ioc1
      c2 <- ioc2 
      return $ cPar loc () pnfo c1 c2   
    -- non joinable => just fail
    mk_vres r1 r2
      | Nothing <- ctJoin_maybe (vect_out_ty r1) (vect_in_ty r2)
      = Nothing  
    -- both non-vect => non-vect
    mk_vres (NotVect t1 _t2) (NotVect _s1 s2) = return $ NotVect t1 s2
    -- joinable and one didVect => Didvect 
    mk_vres r1 r2 
      = let u = chooseParUtility r1 r2 
        in return $ DidVect (vect_in_ty r1) (vect_out_ty r2) u 



*************************** 


{-------------------------------------------------------------------------------
  Scaling factors
-------------------------------------------------------------------------------}

-- | Scaling factors correspond to "modes of vectorization"
-- 
--
--
--
--
--
--



data VectScaleFact
  = -- Scale up factor
    VectScaleUp
     { vs_cin  :: Int
     , vs_cout :: Int
     , vs_cand_mults :: [(Int,Int)] }
     -- List of (min,mout) s.t. mout divides min (multiplicities)

    -- Scale down factor
  | VectScaleDn
     { vs_cin  :: Int
     , vs_cout :: Int
     , vs_cand_divisors :: [(Int,Int)] }

    -- A variation of VectScaleDn
  | VectScaleDnInOrOut
     { vs_cinout :: Either Int Int        -- Only input or output is known
     , vs_cinout_cand_divisors :: [Int] } -- Divisors of whichever we known

  deriving ( Show )


{- Note [RateAction]
   ~~~~~~~~~~~~~~~~~
Consider the vectorization of the following code:

   seq { t1 >>> c1
       ; t2
       }

Assume that c1 (a computer) can vectorize to 24-8. However, as it
currently stands t1 has the flexibility of vectorizing up or down (if
it is a repeat component). For instance one valid vectorization may be
48-24; in fact this vectorization matches exactly the vectorization of
the input queue of c1.

However this behavior is incorrect. The t1 component can take 48bits,
and emit the 24 down to c1 who will then terminate. The bind will
switch at tha point and t2 will take over the rest of the input stream
-- alas t1 has already consumed 24bits that were destined for t2!

Why did this happen? We allowed in flexible vectorization for 't1's
rate to change, from 1-1 to 48-24 (or, effectively 2-1). Therefore the
output matched c1's vectorization by having consumed more than
necessary to produce what the vectorized c1 needs.

The solution to this is to disallow 'flexible' rate vectorization on
the left of a (>>>) whose right hand side is a computer!

-}

data RateAction = RigidRate | FlexiRate

isRigidRate RigidRate = True
isRigidRate _         = False

isFlexiRate FlexiRate = True
isFlexiRate _         = False


compVectScaleFactDn :: RateAction -> Int -> Int -> VectScaleFact
compVectScaleFactDn ra cin cout
  = VectScaleDn { vs_cin  = cin
                , vs_cout = cout
                , vs_cand_divisors
                    = [ (x,y)
                      | x <- divs_of cin                             -- take a divisor of the input
                      , y <- divs_of cout
                      , let mul1 = if x > 0 then cin `div` x else 1  -- here is the multiplicity x ~~> cin
                      , let mul2 = if y > 0 then cout `div` y else 1 -- here is the multiplicity x ~~> cin
                      , if isRigidRate ra then mul1 == mul2 else mul1 <= mul2
                        -- In rigid mode preserve the rate of the original component.
                      ] }
  where divs_of n = if n > 0 then someDivisors n else [0]

someDivisors :: Int -> [ Int ]
someDivisors n
  = filter (\x -> n `mod` x == 0 && x <= n) [1..n]


compVectScaleFacts :: RateAction -> Ty -> Int -> Int -> [VectScaleFact]
compVectScaleFacts ra ty_in cin cout
  = [ compVectScaleFactUp ra ty_in cin cout
    , compVectScaleFactDn ra cin cout]

compVectScaleFactUp :: RateAction -> Ty -> Int -> Int -> VectScaleFact
compVectScaleFactUp ra ty_in cin cout
  = VectScaleUp { vs_cin = cin
                , vs_cout = cout
                , vs_cand_mults = reverse $ allVectMults ra ty_in cin cout
                }

compVectScaleFactDn_InOrOut :: Maybe Int -> Maybe Int -> VectScaleFact
-- Precondition: only one of them is a *Just*
compVectScaleFactDn_InOrOut mcin mcout
  = case (mcin,mcout) of
      (Nothing, Nothing)
          -> error "compVectScaleFactDn_InOrOut: Can't happen!"
      (Just i, Nothing)
          -> VectScaleDnInOrOut (Left i)  (someDivisors i)
      (Nothing, Just j)
          -> VectScaleDnInOrOut (Right j) (someDivisors j)
      (Just _, Just _)
          -> error "compVectScaleFactDn_InOrOut: Can't happen!"

allVectMults :: RateAction -> Ty -> Int -> Int -> [(Int,Int)]
-- TODO: make this infinite (or at least very large)
allVectMults ra ty_in xin xout = [ (x,y)
                                 | x <- [1..128]
                                 , y <- [1..128]
                                 , if isRigidRate ra then x == y
                                   else x `mod` y == 0 && x >= y
                                 , good_sizes x y
                              ]
  where in_vect_bound   = 256 + 32 -- Increase this slightly above 256 (needed for some rates)
        out_vect_bound  = 256 + 32 -- Increase this slightly above 256 (needed for some rates)
        good_sizes x y = xin*x  <= in_vect_bound ty_in &&
                         xout*y <= out_vect_bound




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




liftDVR :: (Comp () () -> Comp () ())
        -> DelayedVectRes
        -> DelayedVectRes
-- Simply transform the computation inside a delayed DVR without
-- touching anything else.
liftDVR f dvr
  = dvr { dvr_comp = dvr_comp dvr >>= (return . f) }


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

