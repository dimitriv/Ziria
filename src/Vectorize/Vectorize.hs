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

import Opts

import CardinalityAnalysis

import VecMonad 
import VecScaleUp    -- Up-vectorization
import VecScaleDn    -- Down-vectorization
import VecScaleForce -- Force vectorization

import TcMonad
import TcComp 
import TcErrors ( ErrCtx (..) )

import PassFold ( elimMitigsIO )

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
  where in_vect_bound _ = 256 + 32 -- Increase this slightly above 256 (needed for some rates)
        out_vect_bound  = 256 + 32 -- Increase this slightly above 256 (needed for some rates)
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

compileMitigs :: GS.Sym -> Comp () () -> IO (Comp () ())
-- Compile away mitigators.
-- TODO: at the moment this is a bit inefficient, 
-- better to compile directly in terms of tick and process.
compileMitigs sym comp 
  = runVecM vec_action sym (VecEnv [] []) (VecState 0 0) >>= (return . fst)
  where 
    vec_action = mapCompM_ return compile comp
    compile c 
      | MkComp c0 loc _ <- c
      , Mitigate ty i1 i2 <- c0 
      = if i1 <= i2 
        then mitigateUp loc "compiled" ty i1 i2
        else mitigateDn loc "compiled" ty i1 i2
      | otherwise
      = return c

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
  = do { -- m <- mitigateUp loc "_bnd" tin ain cin
       ; let m = cMitigate loc () tin ain cin 
       ; return $ 
         cPar loc () (mkParInfo NeverPipeline) m c
       }

mk_out c cout aout tout
  | cout == 0 || aout == cout
  = return c
  | otherwise
  , let loc = compLoc c
  = do { -- m <- mitigateDn loc "_bnd" tout cout aout
       ; let m = cMitigate loc () tout cout aout
       ; return $ 
         cPar loc () (mkParInfo NeverPipeline) c m
       }


matchData :: (GS.Sym, VecEnv)
          -> ParInfo 
          -> Maybe SourcePos 
          -> [ DelayedVectRes ] 
          -> [ DelayedVectRes ] 
          -> [ DelayedVectRes ]
matchData (sym,venv) p loc xs ys = go_left_top xs ys 
  where 
    go_left_top xs ys        = -- trace "go_left"  $ 
                               go_left xs ys
    go_right_top xs ys       = -- trace "go_right" $ 
                               go_right xs ys

    go_left [vc1] vcs2       = -- trace ("A" ++ (show $ length vcs2)) $ 
                               lchoose vc1 vcs2 []

    go_left (vc1:vcs1) vcs2  = -- trace "B" $ 
                               lchoose vc1 vcs2 (go_right_top vcs1 vcs2)
    go_left [] _             = error "go_left"
    go_right vcs1 [vc2]      = rchoose vcs1 vc2 []
    go_right vcs1 (vc2:vcs2) = rchoose vcs1 vc2 (go_left_top vcs1 vcs2)
    go_right _ []            = error "go_right"

    lchoose vc1 [vc2] k
      | Just vcs <- mitigatePar (sym,venv) p loc vc1 vc2
      = vcs : k 
      | otherwise
      = k
    lchoose vc1 (vc2:vc2s) k
      | Just vcs <- -- trace ("foo" ++ show (length vc2s)) $ 
             mitigatePar (sym,venv) p loc vc1 vc2
      = -- trace "lchoose(1)" $ 
        vcs : lchoose vc1 vc2s k
      | otherwise
      = -- trace "lchoose(2)" $ 
        lchoose vc1 vc2s k

    lchoose _ [] _ = error "lchoose"

    rchoose [vc1] vc2 k
      | Just vcs <- mitigatePar (sym,venv) p loc vc1 vc2 
      = vcs : k
      | otherwise
      = k
    rchoose (vc1:vc1s) vc2 k
      | Just vcs <- mitigatePar (sym,venv) p loc vc1 vc2
      = vcs : rchoose vc1s vc2 k
      | otherwise
      = rchoose vc1s vc2 k

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
            -> Maybe DelayedVectRes
mitigatePar (sym,venv) pnfo loc dp1 dp2
 = -- trace ("mitigatePar:" ++ show loc) $ 
   case (dvr_vres dp1, dvr_vres dp2) of
     (NoVect,NoVect) -> 
        Just $ 
        DVR { dvr_comp = mk_par dp1 dp2 
            , dvr_vres = NoVect
            , dvr_orig_tyin  = dvr_orig_tyin dp1 
            , dvr_orig_tyout = dvr_orig_tyout dp2
            }  
        

     -- Treat NoVect as DidVect 1 1 
     (v1@NoVect, v2@(DidVect cin cout u))
        | let t1 = dvr_orig_tyout dp1 
        , let t2 = if cin > 1 
                   then TArr (Literal cin) (dvr_orig_tyin dp2)
                   else dvr_orig_tyin dp2
        , t1 == t2
        , let u = chooseParUtility (vectResUtil v1) 
                                   (vectResUtil v2) (cin,cin)
        -> Just $ 
             DVR { dvr_comp = mk_par dp1 dp2
                 , dvr_vres = DidVect 1 cout u
                 , dvr_orig_tyin  = dvr_orig_tyin dp1
                 , dvr_orig_tyout = dvr_orig_tyout dp2
                 }
           
        | otherwise
          -- TODO: make this more flexible in the future
        -> Nothing

     (v1@(DidVect cin cout u), v2@NoVect) 

        | let t2 = dvr_orig_tyin dp2 
        , let t1 = if cout > 1
                   then TArr (Literal cin) (dvr_orig_tyout dp1)
                   else dvr_orig_tyout dp1
        , t1 == t2
        , let u = chooseParUtility (vectResUtil v1) 
                                   (vectResUtil v2) (cout,cout) 
        -> Just $ 
             DVR { dvr_comp = mk_par dp1 dp2
                 , dvr_vres = DidVect cin 1 u
                 , dvr_orig_tyin  = dvr_orig_tyin dp1
                 , dvr_orig_tyout = dvr_orig_tyout dp2
                 } 
           
        | otherwise
          -- TODO: make this more flexible in the future
        -> Nothing

     (DidVect ci co u1, DidVect ci' co' u2)
       | co == ci' || co == 0 || ci' == 0
       -> let m = if co > 0 then co else ci'
              u = chooseParUtility u1 u2 (m,m)
          in Just $ 
               DVR { dvr_comp = mk_par dp1 dp2
                   , dvr_vres = DidVect ci co' u
                   , dvr_orig_tyin  = dvr_orig_tyin dp1
                   , dvr_orig_tyout = dvr_orig_tyout dp2
                   }
            
{- NO PAR MITIGATION!
       | co `mod` ci' == 0 -- divisible
       -> mitPars (sym,venv) pnfo loc ci co u1 dp1 ci' co' u2 dp2
-}
       | otherwise
       -> Nothing 

  where 
    -- No mitigation
    mk_par dp1 dp2 
      = do { p1 <- dvr_comp dp1
           ; p2 <- dvr_comp dp2 
           ; return $ cPar loc () pnfo p1 p2
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
                       ; print $ ppComp comp }


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
                       ; print $ ppComp comp
              
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
                                ; print (ppComp vc) 
                                ; error "Vectorization bug!" }
                         Right (tcv,_st) 
                          -> return tcv
                     }
        ; mapM do_one [vs_maxi] 
          -- mapM do_one vss
       }

