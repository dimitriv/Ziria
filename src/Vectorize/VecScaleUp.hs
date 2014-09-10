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
module VecScaleUp ( doVectorizeCompUp, vectMap ) where
 
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

doVectorizeCompUp :: Comp (CTy,Card) Ty
                 -> Int       -- Input cardinality   (cin)
                 -> Int       -- Output cardinality  (cout
                 -> (Int,Int) -- Input multiplicity  (im)
                              -- Output multiplicity (om)
                 -> VecM (Comp () ())

-- Version for debugging: 
-- doVectorizeCompUp comp cin cout (min,mout)
--   = return $ MkComp (SimplCompVectUp cin cout (min,mout) $ eraseComp comp)
--                      (compLoc comp) 
--                      ()
-- Precondition
-- (a) im `mod` om == 0 && im >= om
-- (b) computer has simple cardinality
-- Postcondition:
--   Resulting computer has array inputs and output (cin*im) and (cout*om)

-- Real version
doVectorizeCompUp comp cin cout (min,mout)
  = do { let loc = compLoc comp
       ; wrap_fun_name <- newVectName "vect_up_wrap" loc
       ; let wrap_fun_prms = []

         -- Create names for the local arrays 
       ; xa_name <- newVectName "vect_xa" loc
       ; ya_name <- newVectName "vect_ya_up" loc 

       ; let mkcomp c = MkComp c loc ()
             mkexp e  = MkExp e loc ()


       ; let xa_name_typed = xa_name { mbtype = Just $ v_inty }
       ; let ya_name_typed = ya_name { mbtype = Just $ v_outty }
             xa_exp = mkexp $ EVar xa_name_typed
             ya_exp = mkexp $ EVar ya_name_typed


       ; let wrap_fun_lcls 
               | arityout >= 1
               = [ (ya_name, v_outty, Nothing) ]
               | otherwise
               = [] 

       ; icnt_name <- newVectName "vect_i" loc
       ; jcnt_name <- newVectName "vect_j" loc

 
       {- Here is the strategy, in comments. We will create the following:

          mk_wrap_vect() {
            var ya : arr[$arityout];

            (xa : arr[$arityin]) <- take ; 
 
            times i_cntr (min / mout)
              ( times j_cntr mout
                  $rewrite comp code as follows:
                      take ~~~> return (xa[i_cntr*(mout*cin) + j_cntr*cin + $take_count])
                      emit ~~~> return (ya[j_cntr*cout + $emit_count] := e)
              ; emit ya )

          -- NB: The above translation is slightly problematic for auto-lutting because it creates
          -- a global array to store the output, which is considered as an input for the autolutter
          -- and hence the expreesion (after transformations) can be harder to autolut (or we need huge
          -- tables, which is not an option). So we should fix this problem possibly by adding a local
          -- letref definition in the computation language too. The canonical example for this is:
          -- 
          -- transmitter/test_encoding_23_perf.wpl where if we vectorize in [8,12] the *output* buffer
          -- of 12 bits is considered as an additional input and prevents the autolutter from kicking in!
          -- 

        -}
         -- Do a big Take into xa and then use xa in the rest
       ; body <- withInitCounts $
                 do { inner_loop_body <- vectorize_body xa_exp ya_exp icnt_name jcnt_name comp
                    ; let inner_loop 
                             = mkTimes (mkexp $ EVal (VInt mout)) jcnt_name inner_loop_body
                    ; let final_emit 
                             = mkcomp $ Emit ya_exp 
                    ; let outer_loop_body 
                             = if arityout > 0 then 
                                    mkcomp (Seq inner_loop final_emit)
                               else inner_loop
                    ; let outer_loop 
                             =  let icount = mkexp $ EVal (VInt $ min `div` mout)
                                in mkTimes icount icnt_name outer_loop_body
                    ; let final_comp
                             | arityin <= 1 -- Could be zero if the original was not taking at all ...
                             = outer_loop   -- Don't do any big take thing
                             | otherwise  
                             = mkcomp $ mkBind (mkcomp Take1) (xa_name_typed, outer_loop)
                    ; return final_comp 
                    }
       
       ; let call = MkComp (Call wrap_fun_name []) loc ()
             let0 = LetFunC wrap_fun_name [] wrap_fun_lcls body call

       ; return (MkComp let0 loc ()) 
       }
  where 

    arityin  = cin*min
    arityout = cout*mout
    inty     = inTyOfCTyBase  (fst $ compInfo comp)
    outty    = yldTyOfCTyBase (fst $ compInfo comp)
    v_inty   = mkVectTy inty arityin
    v_outty  = mkVectTy outty arityout

    vectorize_body xa_exp ya_exp icnt_name jcnt_name comp = go comp 
      where go comp 
             | let loc = compLoc comp
             = case unComp comp of 
                (Var x) -> lookupCVarBind x >>= go 
                (BindMany c1 xs_cs) -> 
                    do { c1' <- go c1
                       ; let go_one (x,c) = go c >>= \c' -> return (x,c')
                       ; xs_cs' <- mapM go_one xs_cs
                       ; return (MkComp (BindMany c1' xs_cs') loc ()) 
                       }
                (Par {}) -> 
                   vecMFail "BUG: Par is not a simple computer!"
                (Let x c1 c2) -> 
                   extendCVarBind x c1 $ go c2

                (LetE x fi e c1) -> 
                   do { c1' <- go c1
                      ; return (MkComp (LetE x fi (eraseExp e) c1') loc ()) 
                      }
                -- CL
                (LetERef x (Right e) c1) -> 
                   do { c1' <- go c1
                      ; return (MkComp (LetERef x (Right (eraseExp e)) c1') loc ()) 
                      }
                (LetERef x (Left t) c1) -> 
                   do { c1' <- go c1
                      ; return (MkComp (LetERef x (Left t) c1') loc ()) 
                      }
                (LetHeader x fn@(MkFun (MkFunDefined {}) _ _) c1) -> 
                   do { c1' <- go c1
                      ; return $ MkComp (LetHeader x (eraseFun fn) c1') loc () 
                      }
                --
                (LetStruct sdef c1) -> 
                   do { c1' <- go c1
                      ; return $ MkComp (LetStruct sdef c1') loc () 
                      }

                (LetFunC f params locals c1 c2) ->
                   -- Aggressive specialization
                   extendCFunBind f params locals c1 $ go c2
                (Call f es) -> 
                   do { (CFunBind { cfun_params = prms
                                  , cfun_locals = lcls
                                  , cfun_body = body }) <- lookupCFunBind f
                      ; vbody <- go body
                      ; let new_f = f { name = name f ++ "_spec_" ++ (getLnNumInStr loc) }
                            es'   = map eraseCallArg es 
                            call = MkComp (Call new_f es') loc ()
                            lcls' = eraseLocals lcls 
                            let0 = LetFunC new_f prms lcls' vbody call
                      ; return (MkComp let0 loc ())
                      }
                (Branch e c1 c2) ->
                    do { (c1',st1) <- withCurrentCounters (go c1) 
                       ; (c2',st2) <- withCurrentCounters (go c2)
                       ; let g1 = vs_take_count st1 == vs_take_count st2
                             g2 = vs_emit_count st1 == vs_emit_count st2
                       ; unless (g1 && g2) $ 
                         vecMFail "BUG: Branch with non-simple card!?" 

                       ; setVecState st1

                       ; return $ MkComp (Branch (eraseExp e) c1' c2') loc () 
                       }

                (Standalone c1) -> 
                    do { c1' <- go c1
                       ; return $ MkComp (Standalone c1') loc ()
                       }

                -- Some non-simple computers
                (Interleave {}) ->
                    vecMFail "BUG: Interleave is not a simple computer!"
                (Repeat {}) -> 
                    vecMFail "BUG: Repeat is not a simple computer!" 

                (VectComp {}) -> 
                    vecMFail "BUG: VectComp is not a simple computer!" 

                (Filter {}) -> 
                    vecMFail "BUG: Filter is not a simple computer!" 
                (Map {}) -> 
                    vecMFail "BUG: Map is not a simple computer!" 
                (Until {}) -> 
                    vecMFail "BUG: Until is not a simple computer!" 
                (While {}) -> 
                    vecMFail "BUG: While is not a simple computer!" 

                (Times _ _ _ x c1) -> 
                    vecMFail "BUG: Times is not a simple computer!" 

                (Mitigate {}) -> 
                    vecMFail "BUG: Mitigate is not a simple computer!" 


                (ReadSrc mty)  
                   | RWRealTyAnn ty <- mty
                   , not (isArrTy ty)
                   , min > 1 -- For the vectorizer, a multiplicity of 1 is not really a vectorization
                   -> return $ MkComp (ReadSrc (RWRealTyAnn (TArr (Literal min) ty))) loc ()
                   | otherwise
                   -> return $ MkComp (ReadSrc mty) loc ()

                (WriteSnk mty)  
                   | RWRealTyAnn ty <- mty
                   , not (isArrTy ty)
                   , mout > 1 -- For the vectorizer, a multiplicity of 1 is not really a vectorization
                   -> return $ MkComp (WriteSnk (RWRealTyAnn (TArr (Literal mout) ty))) loc ()
                   | otherwise
                   -> return $ MkComp (WriteSnk mty) loc ()


                (ReadInternal bid tp)  
                      -> return $ MkComp (ReadInternal bid tp) loc ()
                (WriteInternal bid) -> return $ MkComp (WriteInternal bid) loc ()

                (Return fi e) -> return $ MkComp (Return fi $ eraseExp e) loc ()


                (LetHeader n fn@(MkFun (MkFunExternal {}) _ _) c) -> do {  c' <- go c 
                                           ; return $ MkComp (LetHeader n (eraseFun fn) c') loc () 
                                           -- Don't worry, even after erasure an external function has type info
                                           }
                (Seq c1 c2) -> do { c1' <- go c1 
                                  ; c2' <- go c2 
                                  ; return $ MkComp (Seq c1' c2') loc () }
 
                -- Now for the meat of the thing
                (Emit e) 
                   -> let mkexp e    = MkExp e loc ()
                          mkintexp e = MkExp e loc tint
                      in
                      do { ecnt <- getEmitCount
                         ; incEmitCount
                          -- emit ~~~> return (ya[j_cntr*cout + $emit_count] := e)
                         ; let idx = (mkintexp (EVar jcnt_name) `emul` mkintexp (EVal $ VInt cout)) `eadd` mkintexp (EVal (VInt ecnt))
                         ; let ya_write 
                                 | arityout == 1
                                 = EAssign (eraseExp ya_exp) (eraseExp e) -- Just assign! No indices involved
                                 | otherwise 
                                 = EArrWrite (eraseExp ya_exp) (eraseExp idx) LISingleton (eraseExp e)

                         ; return $ MkComp (Return False (mkexp ya_write)) loc () 
                         }
                Take1
                  | arityin == 1 
                  -> return $ MkComp Take1 loc ()
                  | otherwise -- Real vectorization on the input
                  -> let mkexp e    = MkExp e loc ()
                         mkintexp e = MkExp e loc tint 
                     in
                     do { tcnt <- getTakeCount
                        ; incTakeCount
                          -- take ~~~> return (xa[i_cntr*(mout*cin) + j_cntr*cin + $take_count])
                        ; let eidx = (mkintexp (EVar icnt_name) `emul` mkintexp (EVal $ VInt (mout*cin))) `eadd` 
                                     (mkintexp (EVar jcnt_name) `emul` mkintexp (EVal $ VInt cin))        `eadd`
                                     (mkintexp (EVal (VInt tcnt)))

                              rdexp = mkexp (EArrRead (eraseExp xa_exp) (eraseExp eidx) LISingleton)
                        ; return $ 
                          MkComp (Return True rdexp) loc () -- NB: Force Inline 
                        }
                Take ne
                  | arityin == 1
                  -> return $ MkComp (Take (eraseExp ne)) loc ()
                  | otherwise
                  -> let mkexp e = MkExp e loc ()
                         mkintexp e = MkExp e loc tint 
                     in   
                     do { let n | (EVal (VInt m)) <- unExp ne = m
                                | otherwise = error "getInt: can't happen!"
                        ; tcnt <- getTakeCount
                        ; mapM (\_ -> incTakeCount) [1..n]
                          -- take ~~~> return (xa[i_cntr*(mout*cin) + j_cntr*cin + $take_count])
                        ; let start_index = (mkintexp (EVar icnt_name) `emul` mkintexp (EVal $ VInt (mout*cin))) `eadd` 
                                            (mkintexp (EVar jcnt_name) `emul` mkintexp (EVal $ VInt cin))        `eadd`
                                            (mkintexp (EVal (VInt tcnt)))

                              rd_exp = mkexp $ EArrRead (eraseExp xa_exp) 
                                                        (eraseExp start_index) 
                                                        (LILength n)
                        ; return $ MkComp (Return True rd_exp) loc () -- NB: Force Inline!  
                        }

                (Emits e)
                  | TArr (Literal n) _ <- info e
                  -> -- Must create: 
                     -- return ( let x = e in 
                     --          for (k = 0; k <= n-1; k++) { ya[jcntr + ecnt + k] := x[k] })

                     -- Better one:
                     -- return (let x = e in ya {startpos = jcontr+ecount} {rangelen=k} = x }
                     let mkexp e = MkExp e loc ()
                         mkintexp e = MkExp e loc tint
                     in 
                     do { ecnt <- getEmitCount
                        ; let xvar = toName "_x" loc Nothing
                              xexp = mkexp (EVar xvar)
                              jexp = mkintexp (EVar jcnt_name)
                              ecntexp = mkintexp (EVal (VInt ecnt))

                         -- emit ~~~> return (ya[j_cntr*cout + $emit_count] := ... 
                        ; let write_index = (jexp `emul` mkintexp (EVal $ VInt cout)) `eadd` ecntexp
                                                
                        ; let asgn_expr 
                                | arityout == 1 
                                = mkexp $ EAssign ya_exp (mkexp (EArrRead (eraseExp e) (mkexp (EVal $ VInt 0)) LISingleton))
                                | otherwise
                                = -- DV: used to be ... mkexp $ EArrWrite ya_exp (eraseExp write_index) (LILength n) xexp
                                  mkexp $ EArrWrite ya_exp (eraseExp write_index) (LILength n) (eraseExp e)

                        -- DV: changing this! 
                        -- ; let let_exp   = mkexp $ ELet xvar (eraseExp e) asgn_expr
                        ; let let_exp = asgn_expr

                        ; mapM (\_ -> incEmitCount) [1..n]

                        ; return (MkComp (Return False let_exp) loc ())
                        }

                   | otherwise
                   -> vecMFail "BUG: emits with non known array size! Can't be simple computer!"


{- Scaling up a computer
   1) Create a function "f"
   2) Inside the function declare local variable
             ya : mout*cout array
      
      Perform a take into 
             (xa : min*cin array) <- take1
      Go through the code of min,mout maintaining a state counter i replacing:

       ([[take1]],incount)   ~~~> (return(xa[$incount]), incount+1) 
       ([[take N]],incount)  ~~~> (return(xa[$incount : $incount+N]), incount+N)

       ([[return e]],incount) ~~> (return e, incount)

       -- Monadic in incount
       [[let x = e in c]] ~~> let x = e in [[c]]
       [[if e then c else c]] ~~> if e then [[c]] else [[c]]

       /times, return, emit etc will not have simple cardinalities

       [[let f(x) = e in c]] ~~> let f(x) = e in [[c]] 
       [[letc x = c1 in c2]] ~~> extendBindEnv (x=c1) in [[c2]]
       [[letfunc f(x)=c1 in c2]] ~~> extendFunEnv (f(x)=c1) in [[c2]]
      
       ([[x]],incount) ~~> where (x=c1) in bind env
                           [[c1]]

       ([[f(e)]],incount) ~~> where f(x)=c1 in fund bind env
                              (let f_23(x) = [[c1]] in f_32(x))

       ([[emit e]],outcount) ~~> 
               (ya[outcount] = e)
               $if outcount == mout*cout-1 then ( ; outcount=0; emit ya) else (; outcount++)
       ([[emits e]], outcount) ~~>
               let x = e in 
               for i=0; i <= length e i++
                 ya[out_count+i] := x[i];
               $if outcount+N == mout*cout-1 then ( ; outcount=0; emit ya) else (; outcount+=N)
      
      
-}




vectMap :: Int -> Int -> Ty -> Ty -> Maybe SourcePos -> Name -> VecM (Comp () ())
-- Vectorizes a map (by upscaling)
-- The plan is to create a:
-- wrap_map_vect() {
--     var xb : arr[mout]
--     (xa : arr[min]) <- take
--       times i (min `div` mout)
--          times j mout
--             ya[j] := e(xa[i*mout + j]
--          emit ya
vectMap min mout tin tout loc nm
  | min == 1
  = return $ MkComp (Map Nothing nm) loc ()
  | otherwise
  = do { let mkexp x = MkExp x loc ()
             mkintexp x = MkExp x loc tint
             mkcomp x = MkComp x loc ()
       ; wrap_fun_name <- newVectName "vect_dn" loc
       ; let wrap_fun_prms = []

         -- Create names for the local arrays 
       ; xa_name <- newVectName "vect_xa" loc 
       ; ya_name <- newVectName "vect_ya_map" loc 
       ; let xa_name_typed = xa_name { mbtype = Just $ mkVectTy tin min }
       ; let ya_name_typed = ya_name { mbtype = Just $ mkVectTy tout mout }
             xa_exp = mkexp $ EVar xa_name_typed
             ya_exp = mkexp $ EVar ya_name_typed

         -- Declare them to be arrays of size cin and cout respectively
       ; let wrap_fun_lcls = 
                  [ (ya_name_typed, mkVectTy tout mout, Nothing) | mout > 1 ]

       ; icnt_name <- newVectName "vect_i" loc 
       ; jcnt_name <- newVectName "vect_j" loc 

       ; let icnt_exp = mkintexp $ EVar icnt_name
             jcnt_exp = mkintexp $ EVar jcnt_name

       ; let write_exp 
               | mout == 1
               = mkcomp (Emit (mkexp (ECall (mkexp $ EVar nm) [mkexp (EArrRead xa_exp (eraseExp icnt_exp) LISingleton)])))
               | otherwise 
               = let rd_idx = eraseExp $ (icnt_exp `emul` (mkintexp (EVal (VInt mout)))) `eadd` jcnt_exp
                 in
                 mkcomp $ 
                 Seq (mkTimes (mkexp $ EVal (VInt mout)) jcnt_name $
                        mkcomp $ Return False $ mkexp $ 
                        EArrWrite ya_exp (eraseExp jcnt_exp) LISingleton (mkexp $ ECall (mkexp $ EVar nm) [mkexp $ EArrRead xa_exp rd_idx LISingleton]))
                     (mkcomp $ Emit ya_exp)
       ; let outer_expr = 
               mkcomp $ BindMany (mkcomp Take1) [(xa_name_typed, outer_loop)]
             outer_loop = 
               mkTimes (mkexp $ EVal (VInt (min `div` mout))) icnt_name write_exp

       ; let call = MkComp (Call wrap_fun_name []) loc ()
             let0 = LetFunC wrap_fun_name [] wrap_fun_lcls outer_expr call

       ; return (MkComp (Repeat Nothing (MkComp let0 loc ())) loc ())
       }


