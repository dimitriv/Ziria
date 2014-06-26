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
module VecScaleDn ( doVectorizeCompDn, doVectorizeCompDnInOrOut ) where
 
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



doVectorizeCompDnInOrOut :: Comp (CTy,Card) Ty -> Either Int Int -> Int -> VecM (Comp () ()) 
doVectorizeCompDnInOrOut comp (Left i) di 
  = doVectorizeCompDn comp i 1 (di,1)
doVectorizeCompDnInOrOut comp (Right j) dj
  = doVectorizeCompDn comp 1 j (1,dj)


doVectorizeCompDn :: Comp (CTy,Card) Ty
                 -> Int       -- Input cardinality   (cin)
                 -> Int       -- Output cardinality  (cout
                 -> (Int,Int) -- Some divisor of input (divin)
                              -- Some divisor of output (divout)
                 -> VecM (Comp () ())
-- Postcondition:
--   Resulting computer has array inputs and output divin and divout
-- Plan for scaling down:
-- (1) Create a temporary array variable to hold cin values
-- (2) Create a temporary array variable to hold dout values (yes, dout, not cout)
-- (3) Create a loop cin / din times that initializes the bi input array. 
-- (4) Go through the code, replacing takes from the input with takes from that array
--     and every 'dout' emits really do emit a dout vector. 
-- 
-- Concretely:
--    wrap_vect_dn() {
--          var xa : arr[cin] 
--          var ya : arr[cout] 
--   
--          times i (cin/din) 
--             ( x <- take // array of size din
--               return (xa[i:i+din] := x))
--          v  <- rewritten body
--          times i (cout/dout)
--              emit ya[i:i+dout]
--          return v
--    }
--    in wrap_vect_dn()
--
doVectorizeCompDn comp cin cout (din,dout) 
  = do { 

{-
         vecMIO $ putStrLn "doVectorizeCompDn!"
       ; vecMIO $ putStrLn ("arityin  = " ++ show arityin)
       ; vecMIO $ putStrLn ("arityout = " ++ show arityout)
       ; vecMIO $ putStrLn ("cin  = " ++ show cin)
       ; vecMIO $ putStrLn ("cout = " ++ show cout)
-}

       ; let loc = compLoc comp
       ; wrap_fun_name <- newVectName "vect_dn" loc
       ; let wrap_fun_prms = []

         -- Create names for the local arrays 
       ; xa_name' <- newVectName "vect_xa" loc 
       ; ya_name' <- newVectName "vect_ya_dn" loc 

       ; let xa_name = xa_name' { mbtype = Just $ TArr (Literal cin) inty }
       ; let ya_name = ya_name' { mbtype = Just $ TArr (Literal cout) outty }

         -- avoid copying 
       ; let just_one_take = (cin == din)
       
         -- Declare them to be arrays of size cin and cout respectively
       ; let wrap_fun_lcls = 
                  [ (xa_name, TArr (Literal cin) inty,  Nothing) | arityin > 1  && (not just_one_take) ] ++ 
                  [ (ya_name, TArr (Literal cout) outty, Nothing) | arityout > 1 ]

       ; icnt_name <- newVectName "vect_i" loc 
       ; xtmp_name_untyped <- newVectName "xtemp" loc
       ; let xtmp_name = xtmp_name_untyped { mbtype = Just (TArr (Literal din) inty) }
 
       ; let mkexp e = MkExp e loc ()
       ; let mkcomp c = MkComp c loc ()

       ; let xa_exp   = mkexp $ EVar xa_name
             ya_exp   = mkexp $ EVar ya_name
             xtmp_exp = mkexp $ EVar xtmp_name
             iexp n   = mkexp $ EBinOp Mult (mkexp $ EVar icnt_name) (mkexp $ EVal (VInt n))
          
             init_arr_in crest
               | just_one_take
               = cBindMany loc () (cTake1 loc ()) [(xa_name, crest)]
               | otherwise
               = cSeq loc () ( mkTimes (mkexp $ (EVal (VInt $ cin `div` din))) icnt_name $ 
                               mkcomp $ 
                               BindMany (mkcomp Take1) $ 
                               [(xtmp_name, mkcomp (Return (mkexp $ EArrWrite xa_exp (iexp din) (LILength din) xtmp_exp)))]
                             ) crest

             emit_arr_out = mkTimes (mkexp $ (EVal (VInt $ cout `div` dout))) icnt_name $
                            mkcomp $ 
                            Emit (mkexp $ EArrRead ya_exp (iexp dout) (LILength dout))

       ; comp_rewritten <- withInitCounts $ vectorize_body xa_exp ya_exp comp

       ; res_var <- newVectName "vect_res" loc
       ; let res_exp = mkexp $ EVar res_var
             final_return = mkcomp $ Return res_exp

       ; let emit_and_return 
               | doneTyOfCTyBase (fst $ compInfo comp) == Just TUnit
               = unComp emit_arr_out
               | otherwise 
               = Seq emit_arr_out final_return 

         -- A bit of tedious case analysis depending on whether we generate
         -- code for arrays or not really

       ; let body = if (arityin > 1) then 
                       init_arr_in $ 
                       if (arityout > 1) then 
                          mkcomp $ 
                          BindMany comp_rewritten $ 
                          [(res_var, mkcomp emit_and_return)] 
                       else comp_rewritten
                    else
                        if (arityout > 1) then
                         mkcomp $
                         BindMany comp_rewritten $
                         [(res_var, mkcomp emit_and_return)]
                        else comp_rewritten

       ; let call = MkComp (Call wrap_fun_name []) loc ()
             let0 = LetFunC wrap_fun_name [] wrap_fun_lcls body call

             final_prog = MkComp let0 loc () 

--       ; vecMIO $ putStrLn "Vectorized (CompDn) program:" 
--       ; vecMIO $ print $ ppComp final_prog

       ; return final_prog
       }

 where    
   arityin  = din
   arityout = dout
   inty     = inTyOfCTyBase  (fst $ compInfo comp)
   outty    = yldTyOfCTyBase (fst $ compInfo comp)
   v_inty   = mkVectTy inty arityin
   v_outty  = mkVectTy outty arityout

   vectorize_body xa_exp ya_exp comp = go comp 
      where go comp 
             | let loc = compLoc comp
             , let mkexp e = MkExp e loc ()
             , let mkintexp e = MkExp e loc TInt
             , let mkcomp c = MkComp c loc ()
             = -- vecMIO (putStrLn ("go: "))      >> 
               -- vecMIO (print (ppCompAst comp)) >>
               case unComp comp of 
                (Var x) -> lookupCVarBind x >>= go 
                (BindMany c1 xs_cs) -> 
                    do { c1' <- go c1
                       ; let go_one (x,c) = go c >>= \c' -> return (x,c')
                       ; xs_cs' <- mapM go_one xs_cs
                       ; return (MkComp (BindMany c1' xs_cs') loc ()) 
                       }

                (Let x c1 c2) -> 
                   extendCVarBind x c1 $ go c2

                (LetE x e c1) -> 
                   do { c1' <- go c1
                      ; return (MkComp (LetE x (eraseExp e) c1') loc ()) 
                      }
                (LetFun x fn c1) -> 
                   do { c1' <- go c1
                      ; return $ MkComp (LetFun x (eraseFun fn) c1') loc () 
                      }
                (LetFunC f params locals c1 c2) ->
                   -- Aggressive specialization
                   extendCFunBind f params locals c1 $ go c2
                (Call f es) -> 
                   do { (CFunBind { cfun_params = prms
                                  , cfun_locals = lcls
                                  , cfun_body = body }) <- lookupCFunBind f
                      ; vbody <- go body
                      ; new_f <- newVectName (name f ++ "_spec") loc
                      ; let -- new_f = f { name = name f ++ "_spec_" ++ show loc }
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

                -- Some non-simple computers
                (Interleave {}) ->
                    vecMFail "BUG: Interleave is not a simple computer!"
                (Repeat {}) -> 
                    vecMFail "BUG: Repeat is not a simple computer!"
                (Filter {}) -> 
                    vecMFail "BUG: Repeat is not a simple computer!"
                (Map {}) -> 
                    vecMFail "BUG: Repeat is not a simple computer!"
                (Par p c1 c2) ->
                   vecMFail "BUG: Par is not a simple computer!"

                (Mitigate {}) -> 
                   vecMFail "BUG: Mitigate is not a simple computer!"

                -- However, these guys are ok, /provided/ they either take OR emit, in which case we will
                -- have vectorized only the takes or emits respectively. So this is not an error, instead we
                -- vectorize the contents. 
                (Until e c) -> 
                   do { c' <- go c
                      ; return $ MkComp (Until (eraseExp e) c') loc () } 
                (While e c) -> 
                   do { c' <- go c
                      ; return $ MkComp (While (eraseExp e) c') loc () } 

                (Times ui e elen x c1) -> 
                   do { c1' <- go c1
                      ; return $ MkComp (Times ui (eraseExp e) (eraseExp elen) x c1') loc () } 


                ReadSrc mty  -> return $ MkComp (ReadSrc mty)  loc ()
                WriteSnk mty -> return $ MkComp (WriteSnk mty) loc ()

                (ReadInternal bid tp)  
                     -> return $ MkComp (ReadInternal bid tp) loc ()
                (WriteInternal bid) -> return $ MkComp (WriteInternal bid) loc ()

                (Return e) -> return $ MkComp (Return $ eraseExp e) loc ()


                (LetExternal n fn c) -> do { c' <- go c 
                                           ; return $ MkComp (LetExternal n (eraseFun fn) c') loc () }
                                           -- NB: Don't worry, 
                                           -- even after erasure an external function has enough type info.

                (LetStruct sdef c) -> do { c' <- go c 
                                         ; return $ MkComp (LetStruct sdef c') loc ()
                                         }

                (Seq c1 c2) -> do { c1' <- go c1 
                                  ; c2' <- go c2 
                                  ; return $ MkComp (Seq c1' c2') loc () }
 
                -- Now for the meat of the thing
                (Emit e) 
                   | arityout == 1 -- NB: This now covers the case where we vectorize on the input only!
                   -> return $ MkComp (Emit (eraseExp e)) loc ()
                   | otherwise 
                   -> do { ecnt <- getEmitCount
                         ; incEmitCount
                         ; let idx = mkexp $ EVal (VInt ecnt)
                               ya_write = EArrWrite (eraseExp ya_exp) idx LISingleton (eraseExp e)
                         ; return $ mkcomp (Return $ mkexp ya_write) 
                         }
                Take1
                  | arityin == 1 -- NB: This now covers the case where we vectorize on the output only!
                  -> return $ MkComp Take1 loc () 
                  | otherwise 
                  -> do { tcnt <- getTakeCount
                        ; incTakeCount
                        ; let idx = mkexp $ EVal (VInt tcnt)
                              xa_read = EArrRead (eraseExp xa_exp) idx LISingleton 
                        ; return $ mkcomp (Return $ mkexp xa_read) }
                Take ne
                  | arityin == 1 -- NB: This now covers the case where we vectorize on the output only!
                  -> return $ mkcomp (Take (eraseExp ne))
                  | EVal (VInt n) <- unExp ne 
                  -> do { tcnt <- getTakeCount
                        ; mapM (\_ -> incTakeCount) [1..n]
                        ; let idx = mkexp $ EVal (VInt tcnt)
                              xa_read = mkexp $ EArrRead (eraseExp xa_exp) idx (LILength n) 
                        ; return $ mkcomp (Return xa_read) }
                  | otherwise
                  -> vecMFail "BUG: takes with unknown array size! Can't be simple computer."

                (Emits e)
                  | arityout == 1 -- NB: This now covers the case where we vectorize on the input only!
                  -> return $ MkComp (Emits (eraseExp e)) loc ()
                  | TArr (Literal n) _ <- info e
                  -> do { ecnt <- getEmitCount
                        ; mapM (\_ -> incEmitCount) [1..n]
                        ; let idx = mkexp $ EVal (VInt ecnt)
                              ya_write = EArrWrite (eraseExp ya_exp) idx (LILength n) (eraseExp e)
                        ; return $ mkcomp (Return $ mkexp ya_write) 
                        }
                  | otherwise
                  -> vecMFail "BUG: emits with unknown array size! Can't be simple computer."

                (Standalone c) -> do { c' <- go c 
                                     ; return $ cStandalone loc () c' }

                (VectComp hint c) -> vecMFail "BUG: VectComp is not a simple computer!" 

                -- _otherwise
                --    -> vecMFail "BUG: emits with non known array size! Can't be simple computer!"

