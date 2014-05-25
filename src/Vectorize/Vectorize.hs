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
import PpComp
import qualified GenSym as GS

import Text.Parsec.Pos

import qualified Data.Set as S
import Control.Monad.State

import Data.List as M


import CardinalityAnalysis

import VecMonad -- Import the vectorization monad
import VecScaleUp 
import VecScaleDn

import TcMonad
import TcComp 
import TcErrors ( ErrCtx (..) )

-- This module explores the space of vectorizations


pickMatchingVectsMany :: [ [(Comp () (), VectRes)] ] 
                      -> [ ([Comp () ()], VectRes) ]
pickMatchingVectsMany css
  = [ (a_choice,a_res) | pre_choices <- sequence css 
                       , a_res <- allVecResMatch (map snd pre_choices)
                       , let a_choice = map fst pre_choices 
    ]



middleResMatch :: VectRes -> VectRes -> [VectRes]
-- Check if the two vectorization results match in their intermediate queue
-- Return empty list if no match, otherwise singleton list with the result of vectorization
middleResMatch NoVect NoVect             = [NoVect]
middleResMatch NoVect (DidVect _ co u)   = [] -- [(1,DidVect 1 co)]
middleResMatch (DidVect ci _ _) NoVect   = [] -- [(1,DidVect ci 1)]
middleResMatch (DidVect ci co u1) (DidVect ci' co' u2)
  | (co == ci' || co == 0 || ci' == 0) 
  = let middle = if co > 0 then co else ci'
    in [ DidVect ci co' (chooseParUtility u1 u2 middle) ]

middleResMatch _ _ = []


-- Vectorization scaling of a computer, up or down.
data VectScaleFact 
  = VectScaleUp { vs_cin  :: Int
                , vs_cout :: Int
                , vs_cand_mults :: [(Int,Int)] } 
                  -- List of (min,mout) s.t. mout multiplicities must divide input multiplicities 
                  -- (min,mout) s.t. min `mod` mout = 0

  | VectScaleDn { vs_cin  :: Int 
                , vs_cout :: Int
                , vs_cand_divisors :: [(Int,Int)] }

    -- A variation of VectScaleDn
  | VectScaleDnInOrOut { vs_cinout :: Either Int Int        -- Only input or output is known statically  
                       , vs_cinout_cand_divisors :: [Int] } -- Divisors of whichever we know

  deriving ( Show ) 
-- Compute vectorization
-- ~~~~~~~~~~~~~~~~~~~~~
-- Heuristic basic plan:
-- The basic plan is that if we can scale up to vectors of 16 then we scale up
-- otherwise the input multiplicity is already too big so we scale down by all
-- power 2 divisors. This seems like a sensible plan.
compVectScaleFactUp :: Int -> Int -> VectScaleFact
compVectScaleFactUp cin cout
  = VectScaleUp { vs_cin = cin
                , vs_cout = cout
                , vs_cand_mults = allVectMults cin cout
                }

compVectScaleFactDn :: Int -> Int -> VectScaleFact
-- Cin = 0
compVectScaleFactDn cin 0
  = VectScaleDn { vs_cin  = cin
                , vs_cout = 0
                , vs_cand_divisors = [ (x,0) | x <- someDivisors cin ] }
-- Cout = 0
compVectScaleFactDn 0 cout
  = VectScaleDn { vs_cin  = 0
                , vs_cout = cout
                , vs_cand_divisors = [ (0,y) | y <- someDivisors cout ] }

-- None is 0
compVectScaleFactDn cin cout
  = VectScaleDn { vs_cin  = cin
                , vs_cout = cout
                , vs_cand_divisors = [ (x,y)
                                     | x <- someDivisors cin
                                     , y <- someDivisors cout ]
                }

{- 
compVectScaleFact :: Int -> Int -> VectScaleFact
compVectScaleFact cin cout
  | not (null $ allVectMults cin cout)
  = compVectScaleFactUp cin cout
  | otherwise
  = compVectScaleFactDn cin cout
-}

compVectScaleFacts :: Int -> Int -> [VectScaleFact]
compVectScaleFacts cin cout
  = [ compVectScaleFactUp cin cout
    , compVectScaleFactDn cin cout]


allVectMults :: Int -> Int -> [(Int,Int)]
allVectMults xin xout = [ (x,y) 
                        | x <- [1..64]         -- [1,2,3,4,6,8]
                        , y <- [1..64]         -- [1,2,3,4,6,8]
                        , x `mod` y == 0 && x >= y

-- NB: This is a potentially *very* large space that we should trim in the "matching" code

                        , xin*x  <= 128
                        , xout*y <= 128
                        ]


someDivisors :: Int -> [ Int ]
someDivisors n = filter (\x -> n `mod` x == 0 && x <= n) [1..n]


doVectorizeComp :: Comp (CTy,Card) Ty -> VectScaleFact -> VecM [(Comp () (), VectRes)]
doVectorizeComp comp (VectScaleUp cin cout mults)
  = mapM (\(min,mout) -> 
            do { vcomp <- doVectorizeCompUp comp cin cout (min,mout) 
               ; return (vcomp, DidVect (cin*min) (cout*mout) minUtil) } ) mults

doVectorizeComp comp (VectScaleDn cin cout divs)
  = mapM (\(divin,divout) -> 
            do { vcomp <- doVectorizeCompDn comp cin cout (divin,divout)
               ; return (vcomp, DidVect divin divout minUtil) } ) divs

doVectorizeComp comp (VectScaleDnInOrOut cinout divs)
  = mapM (\d -> 
            do { vcomp <- doVectorizeCompDnInOrOut comp cinout d
               ; let (divin,divout) = case cinout of 
                                        Left {}  -> (d,1) -- Only vectorized in input
                                        Right {} -> (1,d) -- Only vectorized in output
               ; return (vcomp, DidVect divin divout minUtil) } ) divs




computeVectTop :: Bool -> Comp (CTy,Card) Ty -> VecM [(Comp () (), VectRes)]
computeVectTop verbose c = 
  do { ress <- computeVect c
--     ; vecMIO $ putStrLn ("Total vectorizations = " ++ show (map snd ress))
--     ; vecMIO $ putStrLn ("Non NoVect vects     = " ++ show (length (filter (\(_,r) -> r /= NoVect) ress)))
     ; return ress } 
  where
    computeVect comp 
      = do { vss <- go comp
           ; when (length vss > 36) $  -- - 1,2,4,6,8,16 = 6 choices ^ 2 = 36
               do { vecMIO (putStrLn $ 
                             "WARNING: Too many vectorization choices for:" ++
                                compShortName c ++ "(" ++ show (length vss) ++ ")")
                  -- ; vecMIO (print (ppComp c))
--                  ; vecMIO (putStrLn $ "Options = " ++ show (map snd vss))  
--                  ; vecMFail "Bailing out ..." 
                  }
--           ; vecMIO (putStrLn $ "Options = " ++ show (map snd vss))
           ; return vss
           }

    go comp
      = let (cty,card) = compInfo comp
            loc        = compLoc comp 
            tyin       = inTyOfCTyBase cty
            tyout      = yldTyOfCTyBase cty

        in 
 {-
        vecMIO (putStrLn ("computeVect: " ++ compShortName comp)) >>
        vecMIO (putStrLn ("cardinality =" ++ show card))          >>
        vecMIO (putStrLn ("isvectorizable(tyin)  =" ++ show (isVectorizable tyin)))  >> 
        vecMIO (putStrLn ("isvectorizable(tyout) =" ++ show (isVectorizable tyout))) >> 
  -}
        case unComp comp of
          (Var x) -> do { -- when verbose $ vecMIO (putStrLn "Var")
                        ; xcomp <- lookupCVarBind x
                        ; computeVect xcomp }

          (BindMany c1 xs_cs) -- Downscale if it has a simple cardinality
             | (SimplCard (Just cin) (Just cout)) <- card
             , isVectorizable tyin  || cin  == 0 
             , isVectorizable tyout || cout == 0 
             -> do { -- when verbose $ vecMIO (putStrLn "BindMany")
                   ; let sf = compVectScaleFactDn cin cout 
--                   ; vecMIO $ putStrLn ("-> bindmany scaledown:" ++ show sf)
                   ; vss <- doVectorizeComp comp sf
--                   ; vecMIO $ putStrLn ("-> bindmany results=" ++ show (map snd vss))  
                   ; return $ (eraseComp comp, mkNoVect cin cout):vss }

             | otherwise
             -> do { -- when verbose $ vecMIO (putStrLn "BindMany/other")
                   ; let css = c1 : map snd xs_cs
                   ; vss <- mapM computeVect css
{-
                   ; vecMIO $ putStrLn "bindmany otherwise:" 
                   ; vecMIO $ print (ppComp comp) 
-}
                   -- ; vecMIO (putStrLn ("computeVect: " ++ compShortName comp))
                   -- ; vecMIO (putStrLn ("cardinality =" ++ show card))          
                   -- ; vecMIO (putStrLn ("isvectorizable(tyin)  =" ++ show (isVectorizable tyin)))  
                   -- ; vecMIO (putStrLn ("isvectorizable(tyout) =" ++ show (isVectorizable tyout))) 
                   -- ; vecMIO $ print (ppComp comp)
--                   ; vecMIO $ mapM (\vs -> putStrLn $ "Results: " ++ show (map snd vs)) vss 
                     
                   ; let ress = pickMatchingVectsMany vss 
          
                   ; when (null ress) $ vecMIO $ 
                                        do { putStrLn "WARNING: Bindmany empty vectorization results for computation:"
                                           ; print (ppComp comp) }

--                   ; vecMIO $ mapM (\vs -> putStrLn $ "=> " ++ show (snd vs)) ress 

                   ; return $ [ (MkComp (BindMany vc1 vxs_cs) loc (), vres)
                              | ((vc1:vrests),vres) <- ress
                              , let vxs_cs = zip (map fst xs_cs) vrests ]
                   }
          (Par p c1 c2)
             -> do { -- when verbose $ vecMIO (putStrLn "Par")

                   ; vcs1 <- computeVect c1
                   ; vcs2 <- computeVect c2
 
{- 
                   ; vecMIO (putStrLn $ "Par left (c1): ")
                   ; vecMIO (print (ppComp c1))
                   ; vecMIO (putStrLn $ "Computed vcs1 = " ++ show (length vcs1))
                   ; vecMIO (putStrLn $ "First results = " ++ show (map snd vcs1))


                   ; vecMIO (putStrLn $ "Par right (c2): " ++ compShortName c2)
                   ; vecMIO (putStrLn $ "Computed vcs2 = " ++ show (length vcs2))
                   ; vecMIO (putStrLn $ "First results = " ++ show (map snd vcs2))
-}

                   ; let res = [ (MkComp (Par p vc1 vc2) loc (), vres) 
                               | (vc2,res2) <- vcs2
                               , (vc1,res1) <- vcs1
                               , vres <- middleResMatch res1 res2 ]

{-
                   ; vecMIO (putStrLn $ "Par" ++ " " ++ (compShortName c1) ++ " " ++ (compShortName c2))
                   ; vecMIO (putStrLn $ "Par result size = " ++ show (length res))
                   ; vecMIO (putStrLn $ "Par result = " ++ show (map snd res))
-}

                   ; when (null res) $ vecMIO $ do { putStrLn "WARNING: Par empty vectorization results, for computation:" 
                                                   ; print (ppComp comp) 
                                                   ; putStrLn $ "Par left (c1): "
                                                   ; print (ppComp c1)
                                                   ; putStrLn $ "Computed vcs1 = " ++ show (length vcs1)
                                                   ; putStrLn $ "First results = " ++ show (map snd vcs1)

                                                   ; putStrLn $ "Par right (c2): " ++ compShortName c2
                                                   ; putStrLn $ "Computed vcs2 = " ++ show (length vcs2)
                                                   ; putStrLn $ "First results = " ++ show (map snd vcs2)
                                                   }


                     -- Watch out: Haskell's groupBy is not /unordered/ groupBy!
                   ; let grouped_res   = groupBy (\(v,r) (v',r') -> r `vectResQueueEq` r') $
                                         sortBy  (\(v,r) (v',r') -> vectResQueueComp r r') $ res 

{-
                   ; vecMIO (putStrLn $ "Par grouped_res = " ++ show (map (map snd) grouped_res))
-}
                   ; let pick_largest = maximumBy (\(v,r) (v',r') -> compare (vectResUtil r) (vectResUtil r'))
                 
                   ; let real_res = map pick_largest grouped_res

{- 
                   ; vecMIO (putStrLn $ "Par chosen_res = " ++ show (map snd chosen_res))
-}

--                   ; let real_res = map (\(v,(i,r)) -> (v,r)) chosen_res

{-
                   ; vecMIO (putStrLn $ "Par *real* result = " ++ show (map snd real_res))
-}                  

                   ; return real_res
                   }

          (LetStruct sdef c2)
              -> do { vcs2 <- computeVect c2
                    ; return $ [ (MkComp (LetStruct sdef vc2) loc (), vres2) 
                               | (vc2,vres2) <- vcs2 ] 
                    }

          (Let x c1 c2) 
              -> do { -- when verbose $ vecMIO (putStrLn "Let")
                    ; vcs2 <- extendCVarBind x c1 $ computeVect c2
                    ; return $ [ ( MkComp (Let x (eraseComp c1) vc2) loc ()
                                 , vres2) 
                               | (vc2,vres2) <- vcs2 ] 
                    }
          (LetExternal f fdef c1)
             -> do { -- when verbose $ vecMIO (putStrLn "LetExternal")
                   ; vcs1 <- computeVect c1
                   ; return $ [ (MkComp (LetExternal f (eraseFun fdef) vc1) 
                                            loc (), vres) 
                              | (vc1,vres) <- vcs1 ] 
                   }

          (LetE x e c1)
             -> do { -- when verbose $ vecMIO (putStrLn "LetE")
                   ; vcs1 <- computeVect c1
                   ; return $ [ (MkComp (LetE x (eraseExp e) vc1) loc (), vres) 
                              | (vc1,vres) <- vcs1 ] 
                   }
          (LetFun x fn c1)
             -> do { -- when verbose $ vecMIO (putStrLn "LetFun")
                   ; vcs1 <- computeVect c1
                   ; return $ [ ( MkComp (LetFun x (eraseFun fn) vc1) loc ()
                                , res) 
                              | (vc1,res) <- vcs1 ]
                   }

          (LetFunC f params locals c1 c2)
             -> do { -- when verbose $ vecMIO (putStrLn "LetFunC")
                   ; vcs2 <- extendCFunBind f params locals c1 $ computeVect c2
                   ; return $ [ (MkComp let0 loc (), vres) 
                              | (vc2,vres) <- vcs2 
                              , let let0 = LetFunC f params (eraseLocals locals)
                                                            (eraseComp c1) vc2
                              ] }

          (Call f es)
             -> do { -- when verbose $ vecMIO (putStrLn "Call")
                   ; CFunBind { cfun_params = prms
                              , cfun_locals = lcls
                              , cfun_body   = bdy } <- lookupCFunBind f
                   ; vbdys <- computeVect bdy
                   ; let new_f = f  { name = name f ++ "_VECTORIZED" } 
                                 -- TODO: proper uniq generation
                   ; return [(MkComp let0 loc (), vres)
                            | (vbdy,vres) <- vbdys 
                            , let call = MkComp (Call new_f (map eraseCallArg es)) 
                                                   loc ()
                            , let let0 = LetFunC new_f prms (eraseLocals lcls)
                                                   vbdy call
                            ] 
                   }

          (Interleave c1 c2)
            -> return $ [( MkComp (Interleave (eraseComp c1) 
                                              (eraseComp c2)) loc ()
                         , NoVect )]      
               -- Not sure about vectorizing Interleave.
     

          (Branch e c1 c2) -- Try to downscale, maybe
            | SimplCard (Just cin) (Just cout) <- card
            , isVectorizable tyin  || cin  == 0 
            , isVectorizable tyout || cout == 0 
            -> do { -- when verbose $ vecMIO (putStrLn "Branch")
                  ; let sf = compVectScaleFactDn cin cout 
                  ; vss <- doVectorizeComp comp sf
                  ; return $ (eraseComp comp, mkNoVect cin cout):vss }
            | otherwise
            -> do { -- when verbose $ vecMIO (putStrLn "Branch/other")
                  ; vcs1 <- computeVect c1
                  ; vcs2 <- computeVect c2
                  ; let vcss = pickMatchingVectsMany [vcs1,vcs2]


                  ; when (null vcss) $ 
                    vecMIO $
                    do { putStrLn "WARNING: Branch empty vectorization results, for computation:" 
                       ; print (ppComp comp) }

{-                    
                  ; vecMIO $ putStrLn "Branch-debugging!" 
                  ; vecMIO $ putStrLn "Left branch = " 
                  ; vecMIO (print (ppComp c1))

                  ; vecMIO $ mapM (putStrLn . show . snd) vcs1
                  ; vecMIO $ putStrLn "-$$$$-"
                  ; vecMIO $ putStrLn "Right branch = " 
                  ; vecMIO (print (ppComp c2))

                  ; vecMIO $ mapM (putStrLn . show . snd) vcs2
                  
                  ; vecMIO $ putStrLn ("Matching vcss = " ++ (show (length vcss)))
-}

                  ; return $ [ (MkComp (Branch (eraseExp e) vc1 vc2) loc (), vres)
                             | ([vc1,vc2],vres) <- vcss ] 
                  }

          (Repeat (Just (finalin, finalout)) c)

               -- Scale up!
             | SimplCard (Just cin) (Just cout) <- snd (compInfo c)
             , finalin  `mod` cin == 0      -- cin divides finalin
             , finalout `mod` cout == 0     -- cout divides finalout
             , let min  = finalin `div` cin
             , let mout = finalout `div` cout
             , min `mod` mout == 0          -- mout divides min
             , isVectorizable tyin || cin == 0
             , isVectorizable tyout || cout == 0
             -> do { vecMIO (putStrLn "Repeat (just: scaling up)")
                   ; vc <- doVectorizeCompUp c cin cout (min,mout)
                   ; let self = MkComp (Repeat Nothing (eraseComp c)) loc ()
                   ; return [ (self, mkNoVect cin cout)
                            , (MkComp (Repeat Nothing vc) loc (), 
                                     DidVect finalin finalout minUtil)] 
                   }

               -- or Scale down!
             | SimplCard (Just cin) (Just cout) <- snd (compInfo c)
             , cin `mod` finalin == 0
             , cout `mod` finalout == 0
             , isVectorizable tyin || cin == 0
             , isVectorizable tyout || cout == 0
             -> do { vecMIO (putStrLn "Repeat (just: scaling down)")
                   ; vc <- doVectorizeCompDn c cin cout (finalin,finalout)
                   ; let self = MkComp (Repeat Nothing (eraseComp c)) loc ()
                   ; return [ (self, mkNoVect cin cout)
                            , (MkComp (Repeat Nothing vc) loc (), 
                                          DidVect finalin finalout minUtil) ] 
                   }

             | otherwise
             -> vecMFail "Vectorization failure, bogus repeat annotation!"


          (Repeat Nothing c) -- NB: Vectorizing in anything we wish!
             | SimplCard (Just cin) (Just cout) <- snd $ compInfo c 
             , isVectorizable tyin || cin == 0 
             , isVectorizable tyout || cout == 0
             -> do { when verbose $ vecMIO (putStrLn "Repeat (nothing)")
                   ; let [vsf_up,vsf_dn] = compVectScaleFacts cin cout
                   ; vcs_ups <- doVectorizeComp c vsf_up
                   ; vcs_dns <- doVectorizeComp c vsf_dn
    
                   ; let vcs = vcs_ups ++ vcs_dns

{-
                   ; vecMIO $ putStrLn ("Repeat results: " ++ show (map snd vcs))
-}
                   ; let self = MkComp (Repeat Nothing (eraseComp c)) loc ()
                   ; return $ (self, mkNoVect cin cout) : 
                                 [ (MkComp (Repeat Nothing vc) loc (), vres) 
                                 | (vc,vres) <- vcs ]
                   }

             | SimplCard mcin mcout <- snd $ compInfo c
             , isVectorizable tyin ||  (case mcin  of { Nothing -> True ; Just cin  -> (cin == 0 ) } )
             , isVectorizable tyout || (case mcout of { Nothing -> True ; Just cout -> (cout == 0) } )

             -> do { vecMIO (putStrLn "Repeat (just: scaling down only input or only output!)")
                   ; scalefact <- 
                     case (mcin,mcout) of 
                        (Nothing, Nothing) -> vecMFail "Cardinality analysis bug: SimplCard Nothing Nothing!"
                        (Just i, Nothing ) -> return $ VectScaleDnInOrOut (Left i)  (someDivisors i)
                        (Nothing, Just j ) -> return $ VectScaleDnInOrOut (Right j) (someDivisors j)
                        (Just _, Just _)   -> error "Can't happen!" 

                   ; vcs <- doVectorizeComp c scalefact
                   ; let self = MkComp (Repeat Nothing (eraseComp c)) loc ()

                        -- Tedious 'self' case ... 
                   ; let cin_fin = case mcin of Nothing -> 1 
                                                Just cin -> cin
                   ; let cout_fin = case mcout of Nothing -> 1 
                                                  Just cout -> cout

                   ; return $ (self, mkNoVect cin_fin cout_fin) : 
                                    [ (MkComp (Repeat Nothing vc) loc (), vres) 
                                    | (vc,vres) <- vcs ]
                   }

             | otherwise
             -> return [(eraseComp comp,NoVect)]


          (Filter e) -> return [(MkComp (Filter (eraseExp e)) loc (), NoVect)]

          (Map Nothing e) -> 
            let mults = allVectMults 1 1 
                mk_vect_map (min,mout) 
                  = vectMap min mout (inTyOfCTyBase cty) (yldTyOfCTyBase cty) loc e >>= \vm -> 
                       return (vm, DidVect min mout minUtil)
            in do { -- when verbose $ vecMIO (putStrLn "Map")
                  ; vect_maps <- mapM mk_vect_map mults
                  ; return $ (eraseComp comp, NoVect):vect_maps 
                  }

          (Map (Just (min,mout)) e)
            | min `mod` mout == 0          -- mout divides min
            -> let mults = [(min,mout)]  
                   mk_vect_map (min,mout) 
                     = vectMap min mout (inTyOfCTyBase cty) (yldTyOfCTyBase cty) loc e >>= \vm -> 
                          return (vm, DidVect min mout minUtil)
               in do { -- when verbose $ vecMIO (putStrLn "Map")
                     ; vect_maps <- mapM mk_vect_map mults
                     ; return $ (eraseComp comp, NoVect):vect_maps 
                     }
            | otherwise
             -> vecMFail "Vectorization failure, bogus map annotation!"

          (Until e c1) 
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , (isVectorizable tyin) || cin == 0
            , (isVectorizable tyout) || cout == 0
            -> do { let sf = compVectScaleFactDn cin cout 
                  ; vss <- doVectorizeComp c1 sf
                  ; return $ (eraseComp comp, mkNoVect cin cout) :
                             [ (MkComp (Until (eraseExp e) vc) loc (),vres)
                             | (vc,vres) <- vss 
                             ] }
            | otherwise
            -> return [(eraseComp comp, NoVect)] 


          (While e c1) 
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , (isVectorizable tyin) || cin == 0
            , (isVectorizable tyout) || cout == 0
            -> do { let sf = compVectScaleFactDn cin cout 
                  ; vss <- doVectorizeComp c1 sf
                  ; return $ (eraseComp comp, mkNoVect cin cout) :
                             [ (MkComp (While (eraseExp e) vc) loc (),vres)
                             | (vc,vres) <- vss 
                             ] }
            | otherwise
            -> return [(eraseComp comp, NoVect)] 


          (Times e elen x c1)
            | SimplCard (Just cin) (Just cout) <- snd (compInfo c1)
            , isVectorizable tyin || cin == 0
            , isVectorizable tyout || cout == 0
            -> do { -- when verbose $ vecMIO (putStrLn "Times")
                  ; let sf_down = compVectScaleFactDn cin cout 
                  ; vss <- doVectorizeComp c1 sf_down
                  ; let downvects = [ (MkComp (Times (eraseExp e) (eraseExp elen) x vc) loc (), vres) 
                                    | (vc,vres) <- vss
                                    ]


                  --   -- Moreover, if 'elen' is a constant expression then we can also scale up!
                  ; let sf_ups
                         | MkExp (EVal (VInt n)) _ _ <- elen
                         , MkExp (EVal (VInt 0)) _ _ <- e
                         , VectScaleUp cin cout mults <- compVectScaleFactUp cin cout
                         , cin > 0
                         = let possible_mults = filter (\(min,mout) -> n `mod` (cin*min) == 0 && n >= cin*min) mults
                           in map (\(min,mout) -> (n `div` (cin*min), VectScaleUp cin cout [(min,mout)])) possible_mults
                         | otherwise
                         = [] 

--                  ; vecMIO $ putStrLn ("sf_ups = " ++ show sf_ups) 

                  ; upvects <- mapM (\(n',sf_up) -> 
                                 do { ups <- doVectorizeComp c1 sf_up
                                    ; return [ (mkTimes (MkExp (EVal (VInt n')) (expLoc e) ()) x vc, vres)
                                             | (vc,vres) <- ups ] } ) sf_ups


                  ; return $ (eraseComp comp, mkNoVect cin cout): (concat upvects ++ downvects)

                  }

            | otherwise
            -> vecMIO $ putStrLn ("WARNING: Weird Times in vectorization!") >> 
               return [(eraseComp comp, NoVect)]


          -- ReadSrc and WriteSnk can vectorize to any arities, 
          -- that's the whole point. The reason I am saying "Nothing" is because
          -- this is really "any" vectorization so we may not want to commit to 
          -- a specific type annotation the user has given. Or do we? It's a bit
          -- unsatisfactory. We could lift to arrays of unknown lengths I guess?

          ReadSrc (RWRealTyAnn ty)
            | isVectorizable tyout
            -> do { n <- newVectName "__vlen_gen" loc
                  ; return [(MkComp (ReadSrc (RWBaseTyAnn ty)) loc (), DidVect 0 0 minUtil)] }

          ReadSrc other_ann
            | isVectorizable tyout
            -> return [(MkComp (ReadSrc other_ann) loc (), DidVect 0 0 minUtil) ]

          WriteSnk (RWRealTyAnn ty)
            | isVectorizable tyin
            -> do { n <- newVectName "__vlen_gen" loc
                  ; return [(MkComp (WriteSnk (RWBaseTyAnn ty)) loc (), DidVect 0 0 minUtil)] }

          WriteSnk other_ann 
            | isVectorizable tyin
            -> return [(MkComp (WriteSnk other_ann) loc (), DidVect 0 0 minUtil)] 

          (ReadInternal bid tp)
            | isVectorizable tyout
            -> return $
               [(MkComp (ReadInternal bid tp) loc (), DidVect 0 0 minUtil) ]

          (WriteInternal bid)
            | isVectorizable tyin
            -> return $
               [(MkComp (WriteInternal bid) loc (), DidVect 0 0 minUtil)] 

          (Return e)
            | isVectorizable tyin  || isBufTy tyin
            , isVectorizable tyout || isBufTy tyout 
            -> return $
               [(MkComp (Return (eraseExp e)) loc (), DidVect 0 0 minUtil)]

          _other_simpl_comp0 
             | SimplCard (Just cin) (Just cout) <- card
             , isVectorizable tyin 
             , isVectorizable tyout
             -> do { -- when verbose $ vecMIO (putStrLn "_other_simpl_comp0")
                   ; let sf = compVectScaleFactDn cin cout
                   ; vss <- doVectorizeComp comp sf
                   ; return $ (eraseComp comp, mkNoVect cin cout):vss }
             | otherwise 
             -> do {  
                     vecMIO $ 
                     putStrLn "Non-simple cardinality or in/yld types not vectorizable. Not vectorizing:"
                   ; vecMIO $ putStrLn $ "Intype  = " ++ show tyin
                   ; vecMIO $ putStrLn $ "Outtype = " ++ show tyout
                   ; vecMIO $ putStrLn $ "Cardinality = " ++ show card 
                   ; vecMIO $ print (ppComp (eraseComp comp))
                    
                   ; return $ [(eraseComp comp, NoVect)] }


-- Vectorize a top-level computer and print all possible vectorizations
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
       ; when verbose $ putStrLn "Cardinality analysis finished, moving on to vectorization ..."
       ; (vss,_vstate) 
             <- runVecM (computeVectTop verbose ccomp) sym (VecEnv [] []) 
                                                           (VecState 0 0)
       -- ; when dumpvect $ putStrLn "Vectorization results:"
       ; mapM (\(vc,vres) -> 
               do { {- 
                    when (dumpvect || dumpvecttypes) $ 
                    do { putStrLn $ "Vect result: " ++ show vres
                       ; putStrLn "Program = "
                       ; print (ppComp vc)
                       ; putStrLn "---------------------------------"
                       ; putStrLn "Type checking it ..."
                       }
                    -}
                  ; res <- runTcM (tyCheckTopComp vc) tenv env cenv sym GlobalDefs unifiers

                  ; case res of 
                      Left err -> do { putStrLn "Type error in result of vectorization."
                                     ; print err 
                                     ; print (ppComp vc) 
                                     ; error "Vectorization bug!" }
                      Right (tcv,_st) -> 
                         do { -- when dumpvect $ putStrLn "Passed!"
                              -- ; when dumpvecttypes $ print (ppCompTyped tcv)
                              return tcv }
                  }) vss
       }





{-

   Vectorizing map to arities n and m
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   It seems that if a transformer is supposed to run 
   after a computer in a (c ++> t) situation then we 
   should be taking exact multiples of the arities of
   't', so that when 'c' says done we know exactly 
   what to emit.

   On the other hand, if we are genuinely supposed to be
   processing a stream then it does not matter and we can
   do as we wish. E.g. suppose that 'c' was 24-24 so we 
   convert him to e.g. 4-8 let's say. So now we need to 
   vectorize the 't' to accept array[8] elements. If 't'
   is a map then it is not ok to make it return array[16]
   elements. Because the very moment the first component 
   says 'Done' we might have not emitted our last intermediate
   data we are buffering to return the final array[16]. We either
   need finalizers, or we have to align the vectorization of a 
   'map' such that 

            final_in_cardinality `mod` final_out_cardinality = 0

   This will avoid having to pad.

   For a repeat c where card(c) = (cin,cout), once again I think
   we have to do something similar to avoid padding.

            final_in_card  = cin * min
            final_out_card = cout * mout
            and min / mout

    
final_in_cardinality `divides` final_out_cardinality

transformer to be one such that if
     card(t) = (cin,cout)
   The multiplicity can be:
               (n*cin,m*cout)
   

   map f ~~~> 
     letfun map_f_vect() {
              xa <- take // array of 'n' elements
              times (min/mout) i 
                  times mout 
                     (ya[i] := ....)
              
              x <- take // array of 'n' elements
              x <- take // .....................
              ...  LCM

input_mult  ::= 1/8 1/4 1/2 1 2 4 8
output_mult ::= if input_mult <= 1 then output_mult <= 1 
                else output_mult must divide input_mult.

So we need two functions:
      vectCompScaleDn   (deals with input_mult <= 1)
      vectCompScaleUp   (deals wint input_mult > 1)

I could choose which one to do. For instance if
  cin  = 1,2,4
  cout >= 16
Probably I want to do a 'scale down'
  cin = 24
  cout = 1,2,4,8
I want to do a 'scale down' (but keep large-ish multiples of cout)
If:
   cin =  24
   cout = 24
we can't go wrong by scaling down. If we have:
   cin  = 1-4
   cout = 1-4
probably we want to scale up. I should write a function that chooses
what is the best strategy, to trim the search space. 

whatToDo cin cout 
  = if cin <= 4 && cout <= 4 then ScaleUp multiplicities
    if ....
    if ....
    if ... 



-}

