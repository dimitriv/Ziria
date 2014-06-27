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
import AstCombinator

import PpComp
import qualified GenSym as GS

import Text.Parsec.Pos

import qualified Data.Set as S
import Control.Monad.State

import Data.List as M

import Data.Functor.Identity 


import CardinalityAnalysis

import VecMonad 
import VecScaleUp    -- Up-vectorization
import VecScaleDn    -- Down-vectorization
import VecScaleForce -- Force vectorization

import TcMonad
import TcComp 
import TcErrors ( ErrCtx (..) )


import Debug.Trace 

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

compVectScaleFactDn :: Int -> Int -> VectScaleFact
compVectScaleFactDn cin cout
  = VectScaleDn { vs_cin  = cin
                , vs_cout = cout
                , vs_cand_divisors 
                    = [ (x,y)
                      | x <- divs_of cin                             -- take a divisor of the input
                      , y <- divs_of cout
                      , let mul1 = if x > 0 then cin `div` x else 1  -- here is the multiplicity x ~~> cin
                      , let mul2 = if y > 0 then cout `div` y else 1 -- here is the multiplicity x ~~> cin
                      , mul1 <= mul2 -- IMPORTANT!!!!!!!! Preserve the 'rate' of the component! 

 --                      , y <- [ cout `div` m1
 --                             | m1 <- divs_of mul1
 --                             ]

 --                      , y <- divs_of cout
 --                      , let mul1 = cin `div` x
 --                      , let mul2 = cout `div` y
 --                      , mul1 == mul2 
 -- 
                      -- , y <- filter (\c -> (cin `div` x) `mod` (cout `div` c) == 0) (divs_of cout)
                      -- DV: Don't break the output queue further, as it 
                      -- will already be mitigated 
                      -- y <- divs_of cout 
                      ] }
  where divs_of n = if n > 0 then someDivisors n else [0]

someDivisors :: Int -> [ Int ]
someDivisors n 
  = filter (\x -> n `mod` x == 0 && x <= n) [1..n]


compVectScaleFacts :: Ty -> Int -> Int -> [VectScaleFact]
compVectScaleFacts ty_in cin cout
  = [ compVectScaleFactUp ty_in cin cout
    , compVectScaleFactDn cin cout]

compVectScaleFactUp :: Ty -> Int -> Int -> VectScaleFact
compVectScaleFactUp ty_in cin cout
  = VectScaleUp { vs_cin = cin
                , vs_cout = cout
                , vs_cand_mults = reverse $ allVectMults ty_in cin cout
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

allVectMults :: Ty -> Int -> Int -> [(Int,Int)]
-- TODO: make this infinite (or at least very large)
allVectMults ty_in xin xout = [ -- DV: used to be (x,y)
                                (x,x) -- **  
                              | x <- [1..128]
                              , y <- [1..128]
                              , x `mod` y == 0 && x >= y
                              , good_sizes x y 
                              ]
  where in_vect_bound _ = 256
        out_vect_bound  = 256 
        good_sizes x y = xin*x  <= in_vect_bound ty_in && 
                         xout*y <= out_vect_bound


-- Delayed result of vectorization
data DelayedVectRes 
   = DVR { dvr_comp :: IO (Comp () ())
         , dvr_vres :: VectRes 
         , dvr_orig_tyin  :: Ty -- original (pre-vect) in type
         , dvr_orig_tyout :: Ty -- original (pre-vect) out type
         }



mitigateUp :: Maybe SourcePos -> String
           -> Ty -> Int -> Int -> VecM (Comp () ())
mitigateUp loc orig ty lo hi 
  = do { (comp,binds) <- runVecMBnd $ 
            do { x  <- newTypedName "x_mt_up" (if lo == 1 then ty else TArr (Literal lo) ty) loc
               ; i  <- newTypedName "i" tint loc 
               ; let ya_ty = TArr (Literal hi) ty
               ; ya <- newDeclTypedName "mt_ya_up" ya_ty loc Nothing
               ; let bnd = hi `div` lo
               ; let comp = xRepeat $ 
                            xSeq $ 
                            [ CMD $ 
                              xTimes i (0::Int) bnd $ 
                              xSeq [ CMD $ x <:- xTake
                                   , if lo == 1 then 
                                        CMD $ ya .!i .:= x
                                     else 
                                        CMD $ ya .!(i .* lo, lo) .:= x
                                   ]
                            , CMD $ xEmit ya 
                            ]
               ; return (comp loc)
               }
       ; fname <- newVectName ("mitigate_up" ++ orig) loc 
       ; return $
            cLetFunC loc () fname [] binds comp $ 
            cCall loc () fname []
       }

mitigateDn :: Maybe SourcePos
           -> String
           -> Ty -> Int -> Int -> VecM (Comp () ())
mitigateDn loc orig ty hi lo 
  = do { (comp,binds) <- runVecMBnd $ 
             do { x <- newTypedName "x_mt_dn" (TArr (Literal hi) ty) loc
                ; i <- newTypedName "i" tint loc
                ; let bnd = hi `div` lo
                ; let comp = xRepeat $ xSeq $
                             [ CMD $ x <:- xTake
                             , if lo == 1 
                               then CMD $ xEmits x
                               else CMD $ 
                                    xTimes i (0::Int) bnd $ 
                                      xEmit (x .!(i .* lo, lo))
                             ]
                ; return (comp loc)
                }
       ; fname <- newVectName ("mitigate_dn" ++ orig) loc 
       ; return $
            cLetFunC loc () fname [] binds  comp $ 
            cCall loc () fname []
       }

matchControl :: (GS.Sym, VecEnv)
             -> [ [DelayedVectRes] ] -> [ [DelayedVectRes] ]
-- For a set of blocks c1...cn
-- This function accepts the vectorization candidates for each one
-- and tries to match them so that input and output vectorizations
-- agree -- or can be mitigated to match.
-- E.g.
--   [ [vc11,vc12], [vc21,vc22] ]
--   ~~~> 
--   [ [vc11,vc21], [vc11,vc22], [vc12,vc21], [vc11,vc22] ]
-- in the case where all queues match up
matchControl (sym,venv) bcands 
  = let cands = go [] bcands [] 
    in map mitigate cands 
  where 
    go acc ([]:_) k           
      = error "Can't happen! Empty vectorization result!" 
    go acc ([c1]:crest) k     
      = go (c1:acc) crest k
    go acc ((c1:c1s):crest) k 
      = go (c1:acc) crest (go acc (c1s:crest) k)
    go acc [] k                 
      = (reverse acc) : k

    mitigate :: [DelayedVectRes] -> [DelayedVectRes]
    mitigate vcs = 
      let ain  = gcd_in vcs
          aout = gcd_out vcs
      in map (mitigate_one ain aout) vcs 

    gcd_in vcs = 
      let ins = 
            concat $ map (\dvr -> 
              case dvr_vres dvr of 
                NoVect            -> [1]
                DidVect 0 _ util  -> []
                DidVect ic _ util -> [ic]
            ) vcs 
      in gcd_many ins

    gcd_out vcs = 
      let outs = 
            concat $ map (\dvr -> 
              case dvr_vres dvr of 
                NoVect            -> [1]
                DidVect _ 0 util  -> []
                DidVect _ oc util -> [oc]
            ) vcs 
      in gcd_many outs

    gcd_many [a1] = a1
    gcd_many (a1:a2:as) = gcd_many (gcd a1 a2 : as)
    gcd_many [] = 0 
    -- Could be empty only in the exceptional case where they are all 
    -- filtered out because they are zero

    mitigate_one ain aout 
                 dvr@(DVR { dvr_comp = mk_comp
                          , dvr_vres = vres
                          , dvr_orig_tyin  = tin
                          , dvr_orig_tyout = tout
                          })
      | NoVect <- vres
      = dvr 
      | DidVect cin cout u <- vres
      = let mk_comp' = 
              do { comp <- vecMIO $ mk_comp 

{-
                 ; vecMIO $ do { putStrLn "mitigate_one" 
                               ; putStrLn $ "ain  = " ++ show ain
                               ; putStrLn $ "aout = " ++ show aout
                               ; putStrLn $ "cin  = " ++ show cin
                               ; putStrLn $ "cout = " ++ show cout 
                               -- ; putStrLn $ "comp = " ++ show comp
                               }
-}
                 ; let loc = compLoc comp
                 ; c'  <- mk_in comp cin ain tin
                 ; c'' <- mk_out c' cout aout tout
                 ; return c'' 
                 }

        in dvr { dvr_comp = inCurrentEnv (sym,venv) mk_comp'
               , dvr_vres = DidVect ain aout u -- (u - mitig_util)
               }

      | otherwise
      = error "mitigate_one: Can't happen!"


mk_in c cin ain tin 
  | cin == 0 || ain == cin
    -- no need for mitigation
  = return c
  | otherwise
  , let loc = compLoc c
  = do { m <- mitigateUp loc "_bnd" tin ain cin
       ; return $ 
         cPar loc () (mkParInfo NeverPipeline) m c
       }

mk_out c cout aout tout
  | cout == 0 || aout == cout
  = return c
  | otherwise
  , let loc = compLoc c
  = do { m <- mitigateDn loc "_bnd" tout cout aout
       ; return $ 
         cPar loc () (mkParInfo NeverPipeline) c m
       }


matchData :: (GS.Sym, VecEnv)
          -> ParInfo 
          -> Maybe SourcePos 
          -> [ DelayedVectRes ] 
          -> [ DelayedVectRes ] 
          -> [ DelayedVectRes ]
matchData (sym,venv) p loc xs ys = go_left xs ys 
  where 
    go_left [vc1] vcs2       = lchoose vc1 vcs2 []
    go_left (vc1:vcs1) vcs2  = lchoose vc1 vcs2 (go_right vcs1 vcs2)
    go_left [] _             = error "go_left"
    go_right vcs1 [vc2]      = rchoose vcs1 vc2 []
    go_right vcs1 (vc2:vcs2) = rchoose vcs1 vc2 (go_left vcs1 vcs2)
    go_right _ []            = error "go_right"

    lchoose vc1 [vc2] k
      | vcs <- mitigatePar (sym,venv) p loc vc1 vc2
      = vcs ++ k 
      | otherwise
      = k
    lchoose vc1 (vc2:vc2s) k
      | vcs <- mitigatePar (sym,venv) p loc vc1 vc2
      = vcs ++ k ++ lchoose vc1 vc2s []
      | otherwise
      = k ++ lchoose vc1 vc2s []

    lchoose _ [] _ = error "lchoose"

    rchoose [vc1] vc2 k
      | vcs <- mitigatePar (sym,venv) p loc vc1 vc2 
      = vcs ++ k
      | otherwise
      = k
    rchoose (vc1:vc1s) vc2 k
      | vcs <- mitigatePar (sym,venv) p loc vc1 vc2
      = vcs ++ k ++ rchoose vc1s vc2 []
      | otherwise
      = k ++ rchoose vc1s vc2 []

    rchoose [] _ _ = error "rchoose"


pruneMaximal :: [ DelayedVectRes ] -> [ DelayedVectRes ] 
pruneMaximal xs 
  = -- first group by VectRes in-out
    let groups 
          = groupBy (\vr1 vr2 -> 
              vectResQueueEq (dvr_vres vr1) (dvr_vres vr2)) $ 
            sortBy (\vr1 vr2 -> 
              vectResQueueComp (dvr_vres vr1) (dvr_vres vr2)) $ 
            xs
    in
    -- and for each group pick the maximal
    map filter_maximal groups

  where filter_maximal xs = runIdentity (filterMaximal xs)
  

filterMaximal :: Monad m => [ DelayedVectRes ] -> m DelayedVectRes
filterMaximal cs 
  = case cs of 
      []     -> fail "Empty vectorization choices!" 
      (c:cs) -> return $ go c cs 
  where go c []      = c
        go c (c':cs) = if vectResUtil (dvr_vres c) < vectResUtil (dvr_vres c')
                       then go c' cs 
                       else go c cs


mitigatePar :: (GS.Sym, VecEnv)
            -> ParInfo 
            -> Maybe SourcePos
            -> DelayedVectRes 
            -> DelayedVectRes 
            -> [ DelayedVectRes ]
mitigatePar (sym,venv) pnfo loc dp1 dp2
 = case (dvr_vres dp1, dvr_vres dp2) of
     (NoVect,NoVect) -> 
        [ DVR { dvr_comp = mk_par dp1 dp2 
              , dvr_vres = NoVect
              , dvr_orig_tyin  = dvr_orig_tyin dp1 
              , dvr_orig_tyout = dvr_orig_tyout dp2
              }  
        ]

     -- Treat NoVect as DidVect 1 1 
     (v1@NoVect, v2@(DidVect cin cout u))
        | let t1 = dvr_orig_tyout dp1 
        , let t2 = if cin > 1 
                   then TArr (Literal cin) (dvr_orig_tyin dp2)
                   else dvr_orig_tyin dp2
        , t1 == t2
        , let u = chooseParUtility (vectResUtil v1) 
                                   (vectResUtil v2) (cin,cin)
        -> [ DVR { dvr_comp = mk_par dp1 dp2
                 , dvr_vres = DidVect 1 cout u
                 , dvr_orig_tyin  = dvr_orig_tyin dp1
                 , dvr_orig_tyout = dvr_orig_tyout dp2
                 }
           ]
        | otherwise
          -- TODO: make this more flexible in the future
        -> []

     (v1@(DidVect cin cout u), v2@NoVect) 

        | let t2 = dvr_orig_tyin dp2 
        , let t1 = if cout > 1
                   then TArr (Literal cin) (dvr_orig_tyout dp1)
                   else dvr_orig_tyout dp1
        , t1 == t2
        , let u = chooseParUtility (vectResUtil v1) 
                                   (vectResUtil v2) (cout,cout) 
        -> [ DVR { dvr_comp = mk_par dp1 dp2
                 , dvr_vres = DidVect cin 1 u
                 , dvr_orig_tyin  = dvr_orig_tyin dp1
                 , dvr_orig_tyout = dvr_orig_tyout dp2
                 } 
           ] 
        | otherwise
          -- TODO: make this more flexible in the future
        -> []

     
     (DidVect ci co u1, DidVect ci' co' u2)
       | co == ci' || co == 0 || ci' == 0
       -> let m = if co > 0 then co else ci'
              u = chooseParUtility u1 u2 (m,m)
          in [ DVR { dvr_comp = mk_par dp1 dp2
                   , dvr_vres = DidVect ci co' u
                   , dvr_orig_tyin  = dvr_orig_tyin dp1
                   , dvr_orig_tyout = dvr_orig_tyout dp2
                   }
             ]
       | co `mod` ci' == 0 -- divisible
       -> mitPars (sym,venv) pnfo loc ci co u1 dp1 ci' co' u2 dp2

       | otherwise
       -> [] 

  where 
    -- No mitigation
    mk_par dp1 dp2 
      = do { p1 <- dvr_comp dp1
           ; p2 <- dvr_comp dp2 
           ; return $ cPar loc () pnfo p1 p2
           }

mitPars :: (GS.Sym, VecEnv)
        -> ParInfo 
        -> Maybe SourcePos 
        -> Int -> Int -> Double -> DelayedVectRes
        -> Int -> Int -> Double -> DelayedVectRes
        -> [ DelayedVectRes ]
-- Preconditions 
--  - co1 `mod` ci2 == 0 
--  - co1 =/= ci2
--  - co1 =/= 0, ci2 =/= 0
mitPars (sym,venv) pnfo loc 
        ci1 co1 u1 dp1 
        ci2 co2 u2 dp2
  = case mb_k of
      -- No need to up-vect the RHS 
      [] -> let m = do { m <- mitigateDn loc "_par_middle" (dvr_orig_tyout dp1) co1 ci2
                       ; p1 <- vecMIO (dvr_comp dp1)
                       ; p2 <- vecMIO (dvr_comp dp2)

                       -- ; vecMIO $ do { putStrLn "mitigateDn (par)"
                       --               ; putStrLn $ "ty = " ++ show ty
                       --               ; putStrLn $ "hi = " ++ show hi
                       --               ; putStrLn $ "lo = " ++ show lo
                       --               -- ; putStrLn $ "p1 = " ++ show p1
                       --               -- ; putStrLn $ "mitigator = " ++ show m
                       --               }

                       ; let p1' = cPar loc () pnever p1 m 
                       ; let comp = cPar loc () pnfo p1' p2
                       ; return comp
                       -- ; fname <- newVectName "mk_par" loc 
                       -- ; return $
                       --      cLetFunC loc () fname [] binds comp $ 
                       --      cCall loc () fname []
                       }
            in [ DVR { dvr_comp = inCurrentEnv (sym,venv) m
                     , dvr_vres = DidVect ci1 co2 u
                     , dvr_orig_tyin  = dvr_orig_tyin dp1
                     , dvr_orig_tyout = dvr_orig_tyout dp2
                     }
               ]
      -- We do need to create one for every divisor of k
      _other -> map mk_out_mit mb_k
   where 
        k = co1 `div` ci2 -- divisor
        mb_k = if co2 == 0 || k == 1   || 
                  isArrTy (dvr_orig_tyout dp2)
               then []
               else filter (> 1) $ someDivisors k 

        u = chooseParUtility u1 u2 (ci2,ci2) 
                -- NB: IT DOES NOT MATTER HOW high you might have gone, 
                -- ci2 is the bottleneck here!
                -- Note: maybe we also want to penalize differences 
                -- between co1 and ci2
        pnever = mkParInfo NeverPipeline     

        mk_out_mit k = 
          let m = do { m <- mitigateDn loc "_par_middle" (dvr_orig_tyout dp1) co1 ci2
 
                     ; p1 <- vecMIO (dvr_comp dp1)
                     ; p2 <- vecMIO (dvr_comp dp2)

                     -- ; vecMIO $ do { putStrLn "mitigateDn (par)"
                     --               ; putStrLn $ "ty = " ++ show ty
                     --               ; putStrLn $ "hi = " ++ show hi
                     --               ; putStrLn $ "lo = " ++ show lo
                     --               -- ; putStrLn $ "p1 = " ++ show p1
                     --               -- ; putStrLn $ "mitigator = " ++ show m
                     --               }

                     ; let p1' = cPar loc () pnever p1 m 
                     ; let comp = cPar loc () pnfo p1' p2

                     ; m' <- mitigateUp loc "_par_out" (dvr_orig_tyout dp2) co2 (k*co2)
                     ; return $ cPar loc () pnfo p1' $  
                                cPar loc () pnever p2 m'

                     }
          in DVR { dvr_comp = inCurrentEnv (sym,venv) m
                 , dvr_vres = DidVect ci1 (k*co2) u
                 , dvr_orig_tyin  = dvr_orig_tyin dp1
                 , dvr_orig_tyout = dvr_orig_tyout dp2
                 }



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
computeVectTop verbose = computeVect
  where
    computeVect x 
       = do { -- vecMIO $ putStrLn $ 
              -- "Vectorizer, traversing: " ++ compShortName x
              go x
            }
    go comp =
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
          Var x -> lookupCVarBind x >>= computeVect 
          BindMany c1 xs_cs 
            | SimplCard (Just cin) (Just cout) <- card
            , isVectorizable tyin  || cin  == 0 
            , isVectorizable tyout || cout == 0 
            -> do { let sf = compVectScaleFactDn cin cout 
                  ; (gs,venv) <- getVecEnv 
                  ; let vss = doVectComp gs venv comp sf
                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return $ self : vss
                  }

            | otherwise
            -> do { let css = c1 : map snd xs_cs
                  ; let xs  = map fst xs_cs

                    -- Step 1: vectorize css
                  ; vss <- mapM computeVect css
                    
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
                       ; print $ ppComp comp }


                    -- Step 3: build for each candidate in ress a BindMany
                  ; return $ 
                    pruneMaximal $ map (mkBindDelayedVRes builder) ress

                  }
  
          Par p (MkComp (Par p' c11 c12) l1 i1) c2
              -> let c12cty = fst $ compInfo c12
                     c2cty  = fst $ compInfo c2
                     cty'    = parCompose c12cty c2cty
                     comp' = cPar loc (cty,card) p' c11 $
                             cPar l1 (cty',mkDynamicCard) p c12 c2
                 in computeVect comp'

          Par p c1 c2 
            -> do { vcs1 <- computeVect c1
                  ; vcs2 <- computeVect c2

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
                  ; let ress = pruneMaximal ress_pre

{-
                  ; vecMIO $ 
                    putStrLn $ "(Par) Length ress = " ++ show (length ress) 
-}
                  ; when (null ress) $ vecMIO $ 
                    do { putStrLn "WARNING: Par empty vectorization:"
                       ; print $ ppComp comp
              
                       }

                  ; return ress
                  }
          LetStruct sdef c2
            -> do { vcs2 <- computeVect c2
                  ; return $ 
                    [ liftDVR (cLetStruct loc () sdef) dvr
                    | dvr <- vcs2 ]
                  }
          Let x c1 c2
            -> do { vcs2 <- extendCVarBind x c1 $ computeVect c2
                  ; return $ 
                    [ liftDVR (cLet loc () x (eraseComp c1)) dvr
                    | dvr <- vcs2 ]
                  }
          LetExternal f fdef c1
            -> do { vcs1 <- computeVect c1
                  ; return $ 
                    [ liftDVR (cLetExternal loc () f (eraseFun fdef)) dvr
                    | dvr <- vcs1 ]
                  }

          LetE x e c1
            -> do { vcs1 <- computeVect c1
                  ; return $ 
                    [ liftDVR (cLetE loc () x (eraseExp e)) dvr
                    | dvr <- vcs1 ]
                  }
          LetFun x fn c1
            -> do { vcs1 <- computeVect c1
                  ; return $ 
                    [ liftDVR (cLetFun loc () x (eraseFun fn)) dvr
                    | dvr <- vcs1 ]
                  }
          LetFunC f params locals c1 c2
            -> do { vcs2 <- extendCFunBind f params locals c1 $
                            computeVect c2
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
                  ; vbdys <- computeVect bdy
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
            -> do { let sf = compVectScaleFactDn cin cout 
                  ; (sym,venv) <- getVecEnv 
                  ; let vss = doVectComp sym venv comp sf
                  ; let self = self_no_vect { dvr_vres = mkNoVect cin cout }
                  ; return (self : vss)
                  }

            | otherwise
            -> do { -- when verbose $ vecMIO (putStrLn "Branch/other")
                  ; vcs1 <- computeVect c1
                  ; vcs2 <- computeVect c2

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
                       ; print $ ppComp comp }


 
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
            -> computeVect (cRepeat loc (cty,card) (Just (Rigid hint)) c1)

          Repeat (Just (Rigid (finalin, finalout))) c1
            -> do { vc <- vectorizeWithHint (finalin,finalout) c1
                  ; let self = self_no_vect

                  ; let vect = 
                         DVR { dvr_comp  = return $ 
                                           cRepeat loc () Nothing vc
                             , dvr_vres  = DidVect finalin finalout minUtil
                             , dvr_orig_tyin  = tyin
                             , dvr_orig_tyout = tyout }
                  ; return $ [vect] -- No self, FORCE this! 
                  }

          Repeat (Just (UpTo (maxin, maxout))) c1
             -> do { vss <- computeVect $ cRepeat loc (cty,card) Nothing c1
                   ; let filter_res (dvr@DVR{ dvr_vres = r })
                           = case r of NoVect -> True
                                       DidVect i j _ -> i <= maxin && j <= maxout
                   ; return $ filter filter_res vss
                   }
          

          Repeat Nothing c -- NB: Vectorizing in anything we wish!
            | SimplCard (Just cin) (Just cout) <- snd $ compInfo c 
            , isVectorizable tyin || cin == 0 
            , isVectorizable tyout || cout == 0
            -> do { when verbose $ vecMIO (putStrLn "Repeat (nothing)")
                  ; let [vsf_up,vsf_dn] = compVectScaleFacts tyin cin cout
                  ; (sym,venv) <- getVecEnv 
                  ; let vcs_ups = doVectComp sym venv c vsf_up
                  ; let vcs_dns = doVectComp sym venv c vsf_dn
    
                  ; let vcs = vcs_ups ++ vcs_dns

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
            -> do { vcs <- computeVect c
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

          Return e
            | isVectorizable tyin  || isBufTy tyin
            , isVectorizable tyout || isBufTy tyout 
            -> return [ self_no_vect { dvr_vres = DidVect 0 0 minUtil } ]


          Until e c1 
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , isVectorizable tyin  || cin  == 0
            , isVectorizable tyout || cout == 0
            -> do { let sf = compVectScaleFactDn cin cout 
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
            -> do { let sf  = compVectScaleFactDn cin cout 
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
                  ; let sf_down = compVectScaleFactDn cin cout 
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
                          | MkExp (EVal (VInt n)) _ _ <- elen
                          , MkExp (EVal (VInt 0)) _ _ <- e
                          , VectScaleUp cin cout mults 
                               <- compVectScaleFactUp tyin cin cout
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
                                                 (eVal loc () (VInt n')) x) vc
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


          Map Nothing e -> 
            let mults = allVectMults tyin 1 1 
                mk_vect_map env (min,mout) 
                  = DVR { dvr_comp 
                              = inCurrentEnv env $
                                vectMap min mout tyin tyout loc e
                        , dvr_vres = DidVect min mout minUtil
                        , dvr_orig_tyin  = tyin
                        , dvr_orig_tyout = tyout
                        }
                  
            in do { env <- getVecEnv 
                  ; let vect_maps = map (mk_vect_map env) mults
                  ; return $ self_no_vect : vect_maps } 

          Map (Just (UpTo (min,mout))) e -> 
            let mults = filter (\(i,j) -> i <= min && j <= mout) $ 
                        allVectMults tyin 1 1 
                mk_vect_map env (min,mout) 
                  = DVR { dvr_comp 
                              = inCurrentEnv env $
                                vectMap min mout tyin tyout loc e
                        , dvr_vres = DidVect min mout minUtil
                        , dvr_orig_tyin  = tyin
                        , dvr_orig_tyout = tyout
                        }
                  
            in do { env <- getVecEnv 
                  ; let vect_maps = map (mk_vect_map env) mults
                  ; return $ self_no_vect : vect_maps } 


          Map (Just (Rigid (min,mout))) e
            | min `mod` mout == 0          -- mout divides min
            -> let mults = [(min,mout)]  
                   mk_vect_map env (min,mout) 
                     = DVR { dvr_comp 
                                = inCurrentEnv env $ 
                                  vectMap min mout tyin tyout loc e 
                           , dvr_vres = DidVect min mout minUtil
                           , dvr_orig_tyin  = tyin
                           , dvr_orig_tyout = tyout
                           }

               in do { env <- getVecEnv
                     ; let vect_maps = map (mk_vect_map env) mults
                     ; return $ vect_maps  -- NO self, FORCE this!
                     }
            | otherwise
            -> vecMFail "Vectorization failure, bogus map annotation!"

          _other_simpl_comp0 
             | SimplCard (Just cin) (Just cout) <- card
             , isVectorizable tyin  || cin == 0 
             , isVectorizable tyout || cout == 0
             -> do { let sf = compVectScaleFactDn cin cout
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
                        ; print $ ppComp (eraseComp comp)
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

runDebugVecM :: Bool 
             -> Comp CTy Ty 
             -> TyDefEnv 
             -> Env 
             -> CEnv 
             -> GS.Sym -> TcMState -> IO [Comp CTy Ty] 
runDebugVecM verbose comp tenv env cenv sym unifiers
  = do { -- First run cardinality analysis
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
                = do { vc <- io_comp
                     ; res <- runTcM (tyCheckTopComp vc) 
                                  tenv env cenv sym GlobalDefs unifiers
                     ; case res of
                         Left err 
                          -> do { putStrLn "Type error in vectorization result."
                                ; print err 
                                ; print (ppComp vc) 
                                ; error "Vectorization bug!" }
                         Right (tcv,_st) 
                          -> return tcv
                     }
        ; mapM do_one [vs_maxi] 
          -- mapM do_one vss
       }

