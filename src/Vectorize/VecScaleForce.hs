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

{-# LANGUAGE RebindableSyntax, FlexibleInstances, MultiParamTypeClasses #-}

module VecScaleForce ( doVectorizeCompForce ) where

import AstExpr
import AstComp

import Prelude ( ($), mod, div
               , fromInteger, (>)
               , Int, Maybe (..), String, Bool(..), snd, fst 
               , (<=), error, (==), (<), otherwise, Show (..)
               , Either (..) )

import AstCombinator

import PpComp
import qualified GenSym as GS

import Text.Parsec.Pos

import qualified Data.Set as S
import Control.Monad.State

import Data.List as M

import System.IO 

import Data.Maybe ( fromJust )
import CardinalityAnalysis

import Rebindables


import VecMonad 

-- Rebinding if/then/else
instance IfThenElse Bool a where
 ifThenElse True x y  = x
 ifThenElse False x y = y 
instance IfThenElse XExp XComp where
 ifThenElse x y z = xIf x y z


force_takes :: (Name,Name,Name) -- (in_buff,is_empty,in_buff_idx)
            -> Int              -- finalin : desired chunk size
            -> CTy              -- cty : (done)-type of current take or takes
            -> Maybe SourcePos  -- loc
            -> Maybe Int        -- Nothing = Take, Just n = Takes n
            -> VecMBnd (Comp () ())
force_takes (in_buff, is_empty, in_buff_idx) finalin cty loc ne
  = let done_ty = fromJust $ doneTyOfCTyBase cty 
    in
    case ne of
       Nothing 
         | 1 <= finalin 
         -> rewrite_takes in_buff is_empty in_buff_idx 
                          finalin Nothing done_ty loc
         | otherwise -> error "Bogus 'finalin' annotation (zero)"
       Just n ->
          if n <= finalin then 
             rewrite_takes in_buff is_empty in_buff_idx 
                           finalin (Just n) done_ty loc
          else 
           do { -- calculate some types and residual elements
              ; let rest = n `mod` finalin
              ; let (TArr _ tbase) = done_ty
              
                -- declare integer counter and buffer
              ; cnt <- newDeclTypedName "cnt" tint loc 
                              (Just $ eVal loc () (VInt 0))
              ; let arrty = TArr (Literal finalin) tbase 
              ; ya_buff <- newDeclTypedName "ya_buff" arrty loc Nothing

              -- ; liftIO $ putStrLn "force_takes, 1" 

                -- introduce some typed names for binds 
              ; x <- newTypedName "x" (TArr (Literal finalin) tbase) loc
              ; y <- newTypedName "y" (TArr (Literal rest) tbase) loc
              ; i <- newTypedName "i" tint loc


              -- ; liftIO $ putStrLn "force_takes, 2" 
  
              ; let zERO = 0::Int
                    oNE  = 1::Int

              ; takes_finalin 
                   <- rewrite_takes in_buff is_empty in_buff_idx 
                                    finalin (Just finalin) done_ty loc

              -- ; liftIO $ putStrLn ("takes_finalin = " ++ show takes_finalin)

              ; takes_rest 
                   <- rewrite_takes in_buff is_empty in_buff_idx 
                                    finalin (Just rest) done_ty loc

              -- ; liftIO $ putStrLn ("takes_rest = " ++ show takes_rest)

              ; let comp = xSeq $ 
                           [ CMD $ cnt .:= zERO
                           , CMD $ 
                             xTimes i zERO (n `div` finalin) $
                             xSeq [ CMD $ x <:- takes_finalin -- xTakes finalin
                                  , CMD $ ya_buff .!(i .* finalin, finalin).:= x
                                  , CMD $ cnt .:= (cnt .+ oNE)
                                  ]
                           , CMD $
                             if (0 < rest) then
                              xSeq [ CMD $ y <:- takes_rest  -- xTakes rest
                                   , CMD $ ya_buff.!(cnt, rest) .:= y
                                   , CMD $ xReturn ForceInline ya_buff
                                   ]
                             else xReturn ForceInline ya_buff
                           ]
               
              -- ; liftIO $ putStrLn $ "comp = " ++ show (comp loc)

              ; return (comp loc)
           }


force_emits :: (Name,Name)               -- (out_buff, out_buff_idx)
            -> Either (Exp Ty) (Exp Ty)  -- Left (Emit e), Right => Emits e
            -> Int                       -- finalout
            -> Maybe SourcePos 
            -> VecMBnd (Comp () ())
force_emits (out_buff, out_buff_idx) e_or_es finalout loc
  = case e_or_es of
      Left e 
        | 1 <= finalout 
        -> -- return $ cEmit loc () (eraseExp e)
            rewrite_emits out_buff out_buff_idx (Left $ eraseExp e) 
                   finalout (info e) loc
        | otherwise -> error "Bogus 'finalout' annotation (zero)!"
      Right es
        | (TArr (Literal n) tbase) <- info es 
        -> do { x <- newTypedName "x" (info es) loc
              ; i <- newTypedName "i" tint loc 

                -- Declare a counter 
              ; cnt <- newDeclTypedName "cnt" tint loc 
                          (Just $ eVal loc () (VInt 0))

              ; let rest = n `mod` finalout

              ; emits_i_finalout 
                   <- rewrite_emits out_buff out_buff_idx 
                            (Right ((x .!(i .* finalout, finalout)) loc))
                            finalout
                            (TArr (Literal finalout) tbase) loc
              ; emit_cnt
                   <- rewrite_emits out_buff out_buff_idx
                            (Left ((x .! cnt) loc)) finalout tbase loc 

              ; emits_cnt_rest
                   <- rewrite_emits out_buff out_buff_idx 
                            (Right ((x .!(cnt .* finalout, rest)) loc))
                            finalout
                            (TArr (Literal rest) tbase) loc 

              ; let c = xLetE x AutoInline es $
                        xSeq $ 
                        [ CMD $ cnt .:= (0::Int)
                        , CMD $ xTimes i (0::Int) (n `div` finalout) $
                                xSeq [CMD$ cnt .:= (cnt .+ (1::Int))
                                     ,CMD$ emits_i_finalout
                                      -- xEmits (x.!(i.* finalout,finalout))
                                     ]
                        , CMD $ if rest == 1 then
                                  -- xEmit (x .! cnt)
                                  (\_ -> emit_cnt) 
                                else if (rest > 1) then 
                                  -- xEmits (x .!(cnt .* finalout, rest))
                                  (\_ -> emits_cnt_rest)
                                else 
                                  xReturn AutoInline ()
                        ]

              ; return $ c loc 
              }
        | otherwise
        -> error "BUG: emits but non array emitted value!?"


rewrite_takes :: Name -> Name -> Name 
              -> Int -> Maybe Int -> Ty 
              -> Maybe SourcePos -> VecMBnd (Comp () ())
rewrite_takes in_buff     -- Input buffer name (of size finalin)
              is_empty    -- Is input buffer empty? 
              in_buff_idx -- Current index into the buffer 
              finalin     -- Desired chunk size
              n           -- Size of current take. Precondition (n <= finalin)
                          -- Nothing means 'take', Just n0 means 'takes'
              take_ty     -- Type of current take (or takes)
              loc         -- Location
  = do { -- declare a temp counter as a short lived 
         -- intermediate (C compiler can register it away hopefully)
      
       ; tmp_idx <- newDeclTypedName "idx" tint loc (Just $ eVal loc () (VInt 0))
       ; let (tbase,n0) = case n of 
                            Nothing -> (take_ty,1)
                            Just n0 -> case take_ty of
                                         TArr _ tb -> (tb,n0)
                                         _other -> error "BUG:rewrite_take"
{- 
       ; liftIO $ putStrLn "rewrite_takes,1"
       ; liftIO $ putStrLn $ "tbase = " ++ show tbase
       ; liftIO $ putStrLn $ "n0 = " ++ show n0
       ; liftIO $ putStrLn "rewrite_takes,2"
       ; liftIO $ putStrLn $ "finalin = " ++ show finalin
-}
       ; x <- newTypedName "x" (TArr (Literal finalin) tbase) loc
       ; u <- newTypedName "u" TUnit loc 
       ; let c = 
               xSeq [ CMD $
                      if is_empty .= True then 
                         xSeq [ CMD $ x <:- xTake
                              , CMD $ in_buff .:= x
                              , CMD $ is_empty .:= False
                              , CMD $ in_buff_idx .:= (0::Int)
                              ]
                       else xReturn AutoInline ()
                     , CMD $ 
                       if finalin .= (in_buff_idx .+ n0) then 
                         xSeq [ CMD $ is_empty .:= True
                              , CMD $ xReturn ForceInline (in_buff .! (in_buff_idx,n0))
                              ]
                       else if ((in_buff_idx .+ n0) .< finalin) then
                               xSeq [ CMD $ tmp_idx .:= in_buff_idx
                                    , CMD $ in_buff_idx .:= (in_buff_idx .+ n0)
                                    , CMD $ xReturn ForceInline (in_buff .! (tmp_idx,n0))
                                    ]
                                 -- The only reason this is an error is the
                                 -- lack of memcpy as a primitive but
                                 -- probably we will not encountere any such
                                 -- misaligned annotations.
                            else xSeq [ CMD $ u <:- xError "rewrite_take: unaligned!"
                                        -- Just a dummy return to make code generator happy ...
                                        -- Fix this at some point by providing a proper 
                                        -- computer-level error function 
                                      , CMD $ xReturn ForceInline (in_buff .!(0::Int,n0))
                                      ]
                     ] loc
--       ; liftIO $ putStrLn "rewrite_takes,3"
--       ; liftIO $ putStrLn $ "c = " ++ show c
       ; return c
       }

rewrite_emits :: Name -> Name -> Either (Exp ()) (Exp ()) 
              -> Int -> Ty 
              -> Maybe SourcePos -> VecMBnd (Comp () ())
rewrite_emits out_buf     -- buffer where we store the output
              out_buf_idx -- index 
              e_or_es     -- what to emit(s)
              finalout    -- Desired chunk size
              emit_ty     -- the type of e_or_es, whatever it was
              loc 
 = do { x <- newTypedName "x" emit_ty loc 
      ; let es = case e_or_es of { Left e -> e ; Right es -> es } 
      ; let n = case emit_ty of
                  TArr (Literal n0) _ -> n0
                  _other -> 1 
      ; let comp = 
              xLetE x AutoInline es $ 
              if finalout .= (out_buf_idx .+ n) then 
                   xSeq [ CMD $ 
                          case e_or_es of 
                            Left {}  -> (out_buf .! out_buf_idx) .:= x
                            Right {} -> out_buf .! (out_buf_idx, n) .:= x
                        , CMD $ out_buf_idx .:= (0::Int)
                        , CMD $ xEmit out_buf
                        ]
              else if (out_buf_idx .+ n .< finalout ) then
                   xSeq [ CMD $ 
                          case e_or_es of 
                            Left {}  -> out_buf .! out_buf_idx .:= x
                            Right {} -> out_buf .! (out_buf_idx, n) .:= x
                        , CMD $ out_buf_idx .:= (out_buf_idx .+ n)
                        ]
              else xError "rewrite_emit: unaligned!" 
      ; return (comp loc)
      }

doVectorizeCompForce :: Comp (CTy,Card) Ty
                     -> (Int, Int) -- input and output desired widths
                     -> VecM (Comp () ())
-- Precondition: computer is not of simple cardinality.  If it were it
-- would have been possible to check for the validity of the hint in
-- Vectorize.hs
doVectorizeCompForce comp (finalin,finalout)
  | let take_ty = inTyOfCTyBase  (fst $ compInfo comp)
  , let emit_ty = yldTyOfCTyBase (fst $ compInfo comp)
  , let loc     = compLoc comp
  = do { -- vecMIO $ putStrLn "A"
       ; (vect_body,binds) <- runVecMBnd $ create_vectorized take_ty emit_ty loc
         -- ; vecMIO $ putStrLn "B"
       ; fname <- newVectName "forced" loc 
       ; let comp' = cLetFunC loc () fname 
                        []    -- params 
                        binds -- locals
                        vect_body
                        (cCall loc () fname [])
       -- ; vecMIO $ putStrLn $ "F" ++ show comp'
       -- ; vecMIO $ putStrLn "G" 
       ; return comp'
       }

  where create_vectorized take_ty emit_ty loc
          = do { -- declare global names
                 let finalin_ty  = TArr (Literal finalin)  take_ty
               ; let finalout_ty = TArr (Literal finalout) emit_ty

--               ; liftIO $ putStrLn "C"
  
               ; let exp0     = eVal loc () (VInt 0)
               ; let exp_true = eVal loc () (VBool True)

               ; in_buff  <- newDeclTypedName "in_buff" finalin_ty loc Nothing
               ; is_empty <- newDeclTypedName "is_empty" TBool loc (Just exp_true)
               ; in_buff_idx <- newDeclTypedName "in_buff_idx" tint loc (Just exp0)
               ; out_buff <- newDeclTypedName "out_buff" finalout_ty loc Nothing 
               ; out_buff_idx <- newDeclTypedName "out_buff_idx" tint loc (Just exp0)
               
               ; vect_body (in_buff, is_empty, in_buff_idx)
                           (out_buff,out_buff_idx) comp 
               }
        vect_body (in_buff, is_empty, in_buff_idx) 
                  (out_buff,out_buff_idx) comp = go comp 
          where 
            go comp = 
             do { -- liftIO $ putStrLn "go"
                ; let loc = compLoc comp 
                ; case unComp comp of
                    (Var x) -> liftVecM (lookupCVarBind x) >>= go
                               -- Here there is a known problem with higher-order
                               -- functions, since a h.o. param will not be in the
                               -- environment bound to a computation. 
                    (BindMany c1 xs_cs) ->
                      do { c1' <- go c1
                         ; let go_one (x,c) = go c >>= \c' -> return (x,c')
                         ; xs_cs' <- mapM go_one xs_cs
                         ; return (MkComp (mkBindMany c1' xs_cs') loc ()) 
                         }
                    (Let x c1 c2) -> 
                      extendCVarBind' x c1 $ go c2
                    (LetE x fi e c1) -> 
                        do { c1' <- go c1
                           ; return $ cLetE loc () x fi (eraseExp e) c1' }
                    -- CL
                    (LetERef x (Right e) c1) -> 
                        do { c1' <- go c1
                           ; return (MkComp (LetERef x (Right (eraseExp e)) c1') loc ()) }
                    (LetERef x (Left t) c1) -> 
                        do { c1' <- go c1
                           ; return (MkComp (LetERef x (Left t) c1') loc ()) 
                        }
                    (LetHeader x fn@(MkFun (MkFunDefined {}) _ _) c1) ->
                        do { c1' <- go c1
                           ; return $ cLetHeader loc () x (eraseFun fn) c1' }
                    --
                    (LetFunC f params locals c1 c2) ->
                       -- Aggressive specialization
                       do { -- liftIO $ putStrLn "S"
                          ; r <- extendCFunBind' f params locals c1 $ go c2
                          ; -- liftIO $ putStrLn "R"
                          ; return r 
                          }

                    (Call f es) -> 
                          do { (CFunBind { cfun_params = prms
                                         , cfun_locals = lcls
                                         , cfun_body = body }) <- liftVecM $
                                                                  lookupCFunBind f
                             ; vbody <- go body
                             ; new_f <- liftVecM $ 
                                        newVectName (name f ++ "_spec") loc
                             ; let es'   = map eraseCallArg es 
                                   call = MkComp (Call new_f es') loc ()
                                   lcls' = eraseLocals lcls 
                                   let0 = LetFunC new_f prms lcls' vbody call
                             ; return (MkComp let0 loc ())
                             }
                    (Branch e c1 c2) -> 
                         do { c1' <- go c1
                            ; c2' <- go c2
                            ; return $ cBranch loc () (eraseExp e) c1' c2' }

                    (Interleave {}) ->
                        liftVecM $ vecMFail $ 
                        "Can't force vectorization of non-simple comp: interleave"
                    (Repeat {}) -> 
                        liftVecM $ vecMFail $ 
                        "Can't force vectorization of non-simple comp: repeat"
                    (Filter {}) -> 
                        liftVecM $ vecMFail $ 
                        "Can't force vectorization of non-simple comp: filter"
                    (Map {}) -> 
                        liftVecM $ vecMFail $ 
                        "Can't force vectorization of non-simple comp: map"
                    (Par p c1 c2) ->
                        liftVecM $ vecMFail $ 
                        "Can't force vectorization of non-simple comp: par"
                    (Mitigate {}) -> 
                        liftVecM $ vecMFail "Can't force vectorization of non-simple comp: mitigate"

                    (VectComp hint c) -> 
                        liftVecM $ 
                        vecMFail "Nested vectorization annotations disallowed" 

                    (Until e c) -> 
                       do { c' <- go c
                          ; return $ cUntil loc () (eraseExp e) c' } 
                    (While e c) -> 
                       do { c' <- go c
                          ; return $ cWhile loc () (eraseExp e) c' } 
                    (Times ui e elen x c1) -> 
                       do { c1' <- go c1
                          ; return $ 
                            cTimes loc () ui (eraseExp e) (eraseExp elen) x c1' } 

                    (ReadSrc mty)  -> return $ cReadSrc loc () mty
                    (WriteSnk mty) -> return $ cWriteSnk loc () mty
                    (ReadInternal bid tp) -> return $ cReadInternal loc () bid tp
                    (WriteInternal bid)   -> return $ cWriteInternal loc () bid

                    (Return fi e) -> return $ cReturn loc () fi (eraseExp e)

                    -- CL
                    (LetHeader n fn@(MkFun (MkFunExternal {}) _ _) c) -> 
                       do { c' <- go c 
                          ; return $ cLetHeader loc () n (eraseFun fn) c' }
                    --
                    (LetStruct sdef c) -> 
                       do { c' <- go c 
                          ; return $ cLetStruct loc () sdef c' }

                    (Seq c1 c2) -> 
                       do { c1' <- go c1 
                          ; c2' <- go c2 
                          ; return $ cSeq loc () c1' c2' }

                    (Standalone c) -> 
                       do { c' <- go c 
                          ; return $ cStandalone loc () c' }

                    (Take1) 
                      -> force_takes (in_buff,is_empty,in_buff_idx)
                              finalin (fst $ compInfo comp) loc Nothing 
                    (Take e) 
                      | EVal (VInt n) <- unExp e
                      -> do { r <- force_takes (in_buff,is_empty,in_buff_idx)
                                        finalin (fst $ compInfo comp) loc (Just n)
                            -- ; liftIO $ putStrLn $ "TAKES: " ++ show r
                            ; return r
                            }
                      | otherwise
                      -> liftVecM $ vecMFail "Take with non constant array size!"
                    (Emit e) 
                      -> do { r <- force_emits (out_buff, out_buff_idx) (Left e) 
                                   finalout loc  
                            -- ; liftIO $ putStrLn $ "EMITS: " ++ show r
                            ; return r
                            }
                    (Emits es) 
                      -> force_emits (out_buff, out_buff_idx) (Right es)
                              finalout loc

                } 


