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
{-# OPTIONS_GHC -Wall -Wwarn #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, 
    GeneralizedNewtypeDeriving, MultiWayIf, QuasiQuotes, DeriveGeneric #-}
module PassOptMit (
    passElimAutomappedMitigs
  , elimMitigsIO

) where

import Prelude hiding (exp)

import AstComp
import AstExpr
import Opts
import qualified GenSym as GS
import Control.Monad ( when ) 
import AstUnlabelled
import Outputable ()
import PpComp ()
import PpExpr ()

import PassFoldM

import CtComp ( ctComp )

{-------------------------------------------------------------------------------
  Rewriting and optimizing mitigators
-------------------------------------------------------------------------------}

rewrite_mit_map :: Ty -> (Int,Int) -> Ty -> (Int,Int) -> (EId, Fun) -> RwM Comp
-- Precondition:  i1 `div` j1 == j2 `div` i2
rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f_name, fun)
  = do { let rng1 = if j1 == 1 then LISingleton else LILength j1
       ; let rng2 = if i2 == 1 then LISingleton else LILength i2
       ; let d = i1 `div` j1
       ; let floc = funLoc fun

         -- input variable
       ; let x_ty   = TArray (Literal i1) ty1     -- input type
       ; x_name <- newPassFoldGName "x" x_ty floc Imm
       ; let x_exp  = eVar floc x_name

      
         -- output variable
       ; let y_ty   = TArray (Literal j2) ty2     -- output type
       ; y_name <- newPassFoldGName "y" y_ty floc Mut
       ; let y_exp  = eVar floc y_name

         -- name of new map function
       ; let f_ty = TArrow [(GArgTy x_ty Imm)] y_ty
       ; new_f_name <- newPassFoldGName "auto_map_mit" f_ty floc Imm


         -- new counter
       ; i_name <- newPassFoldGName "i" tint floc Imm
       ; let i_exp  = eVar floc i_name

         -- zero and 'd'
       ; let ezero = eVal floc tint (vint (0 :: Int))
       ; let e_d   = eVal floc tint (vint d)


       ; let new_body
               = eLetRef floc y_name Nothing $
                 eSeq floc
                  (eFor floc AutoUnroll i_name ezero e_d ekernel)
                  -- do the for loop
                  y_exp -- and return y

             write_idx = eBinOp floc Mult (eVal floc tint (vint i2)) i_exp
             read_idx  = eBinOp floc Mult (eVal floc tint (vint j1)) i_exp

             earrrd    = eArrRead floc x_exp read_idx rng1
             ekernel   = eArrWrite floc y_exp write_idx rng2 $
                         eCall floc f_name [earrrd]

      ; let new_mapper = cMap floc Nothing new_f_name
      ; let new_fun = MkFun (MkFunDefined new_f_name [x_name] new_body) floc ()

      ; rewrite $
        cLetHeader floc fun $   -- original function
        cLetHeader floc new_fun new_mapper
      }


-- | Elimination of mitigators
elimMitigsIO :: DynFlags -> GS.Sym -> Comp -> IO Comp
elimMitigsIO flags sym = go
  where
    go comp = do { (comp', rewritten) <- runRwM (elimMitigs comp) (sym, flags)
                 ; case rewritten of
                     NotRewritten -> return comp
                     Rewritten    -> go comp'
                 }


frm_mit :: Comp -> Maybe ((Ty,Int,Int), Comp)
-- returns (mitigator,residual-comp)
frm_mit c
  | Par _p0 c1 c2 <- unComp c
  , Mitigate _s ty i1 i2  <- unComp c2
  = Just ((ty,i1,i2), c1)

  | Par p0 c1 c2 <- unComp c
  = case frm_mit c2 of
      Nothing -> Nothing
      Just (mit,c2') -> Just (mit, cPar (compLoc c) p0 c1 c2')

  | LetHeader fdef@(MkFun (MkFunDefined {}) _ _) cont <- unComp c
    -- Needed because of AutoMaps! Yikes!
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetHeader loc fdef cont')

  -- Special case for code emitted by the vectorizer
  | LetFunC fn prms body cont <- unComp c
  , Call fn' _args <- unComp cont
  , fn == fn'
  , let loc = compLoc c
  = case frm_mit body of
      Nothing -> Nothing
      Just (mit,body') -> Just (mit, cLetFunC loc fn prms body' cont)

  -- fallthrough general case
  | LetFunC fn prms body cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetFunC loc fn prms body cont')

  | Let n c1 cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLet loc n c1 cont')

  | LetE n f e cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetE loc n f e cont')

  | LetERef n me cont <- unComp c
  , let loc = compLoc c
  = case frm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetERef loc n me cont')


  | otherwise
  = Nothing

flm_mit :: Comp -> Maybe ((Ty,Int,Int), Comp)
-- returns (mitigator,residual-comp)
flm_mit c
  | Par _p0 c1 c2 <- unComp c
  , Mitigate _s ty i1 i2  <- unComp c1
  = Just ((ty,i1,i2), c2)

  | Par p0 c1 c2 <- unComp c
  = case flm_mit c1 of
      Nothing -> Nothing
      Just (mit,c1') -> Just (mit, cPar (compLoc c) p0 c1' c2)

  | LetHeader fdef@(MkFun (MkFunDefined {}) _ _) cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit, cLetHeader loc fdef cont')

  -- Special case for code emitted by the vectorizer
  | LetFunC fn prms body cont <- unComp c
  , Call fn' _ <- unComp cont
  , fn == fn'
  , let loc = compLoc c
  = case flm_mit body of
      Nothing -> Nothing
      Just (mit,body') -> Just (mit, cLetFunC loc fn prms body' cont)

  -- fallthrough general case
  | LetFunC fn prms body cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetFunC loc fn prms body cont')

  | Let n c1 cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLet loc n c1 cont')

  | LetE n f e cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetE loc n f e cont')

  | LetERef n me cont <- unComp c
  , let loc = compLoc c
  = case flm_mit cont of
      Nothing -> Nothing
      Just (mit,cont') -> Just (mit,cLetERef loc n me cont')

  | otherwise
  = Nothing

elimMitigs :: Comp -> RwM Comp
-- Vectorizer-specific
elimMitigs comp
  = mapCompM return return return return return mitig comp
  where
   mitig c
      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if (i1 `mod` j2 == 0) || (j2 `mod` i1) == 0 then
             do { rewrite $
                  cPar cloc p c1' $
                  cPar cloc (mkParInfo NeverPipeline)
                            (cMitigate cloc "POM" ty1 i1 j2) c2'
                }
             else
             if l /= j1 then
              rewrite $
              cPar cloc p (cPar cloc pnever c1' (cMitigate cloc "POM" ty1 i1 l))
                          (cPar cloc pnever (cMitigate cloc "POM" ty2 l j2) c2')
             else return c
           }

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just ((ty1,i1,j1),c1') <- frm_mit c1
      , Mitigate s2 ty2 i2 j2 <- unComp c2
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if i1 `mod` j2 == 0 || j2 `mod` i1 == 0 then
             do { rewrite $
                  cPar cloc p c1' (cMitigate cloc "POM" ty1 i1 j2)
                }
             else
             if l /= j1 then
              rewrite $
              cPar cloc p (cPar cloc pnever c1' (cMitigate cloc "POM" ty1 i1 l))
                          (cMitigate cloc "POM" ty2 l j2)
             else return c
           }

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just ((ty2,i2,j2),c2') <- flm_mit c2
      , Mitigate s1 ty1 i1 j1 <- unComp c1
      , let l = lcm i1 j2
      = do { when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
           ; if i1 `mod` j2 == 0 || j2 `mod` i1 == 0 then
             do { rewrite $
                  cPar cloc p (cMitigate cloc "POM" ty1 i1 j2) c2'
                }
             else if l /= j1 then
              rewrite $
              cPar cloc p (cMitigate cloc "POM" ty1 i1 l)
                          (cPar cloc pnever (cMitigate cloc "POM" ty2 l j2) c2')
             else return c
           }

        -- throw away useless mitigators!
      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c1') <- frm_mit c1
      , WriteSnk _ty <- unComp c2
      , let c2' = cWriteSnk (compLoc c2) (yldTyOfCTy (ctComp c1'))
      = rewrite $ cPar cloc p c1' c2'

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c1') <- frm_mit c1
      , WriteInternal _ty bid <- unComp c2
      , let c2' = cWriteInternal (compLoc c2) (yldTyOfCTy (ctComp c1')) bid
      = rewrite $ cPar cloc p c1' c2'
      
      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c2') <- flm_mit c2
      , ReadSrc _ty <- unComp c1
      , let c1' = cReadSrc (compLoc c1) (inTyOfCTy (ctComp c2'))
      = rewrite $ cPar cloc p c1' c2'

      | MkComp c0 cloc () <- c
      , Par p c1 c2 <- c0
      , Just (_,c2') <- flm_mit c2
      , ReadInternal _ty bid rt <- unComp c1
      , let c1' = cReadInternal (compLoc c1) (inTyOfCTy (ctComp c2')) bid rt
      = rewrite $ cPar cloc p c1' c2'

      -- trivial mitigators
      | MkComp c0 _cloc () <- c
      , Par _p c1 c2 <- c0
      , Mitigate _ _ty i1 i2 <- unComp c1
      , i1 == i2
      = rewrite c2

      | MkComp c0 _cloc () <- c
      , Par _p c1 c2 <- c0
      , Mitigate _ _ty i1 i2 <- unComp c2
      , i1 == i2
      = rewrite c1

      | otherwise
      = return c



-- TODO: Add logStep and corresponding test case.
passElimAutomappedMitigs :: TypedCompPass
passElimAutomappedMitigs = TypedCompBottomUp $ \_cloc c -> if
  | MkComp c0 _cloc _ <- c
    , Par _p c1 c2 <- c0
    , Par _p c11 c12 <- unComp c1
    , LetHeader fun (MkComp (Map _v f) _ _) <- unComp c12  -- (c1 - map f) - c2
    , Mitigate _ ty1 i1 j1 <- unComp c11
    , Mitigate _ ty2 i2 j2 <- unComp c2           -- (c11' -- mitigate(i2,j2) -- map f) - mitigate(i2,j2) - c2'
    , i1 >= j1 && i2 <= j2                      -- down mit and up mit
    , let d1 = i1 `div` j1
    , let d2 = j2 `div` i2
    , d1 == d2                                  -- exactly the same rate
    , funName fun == f
   -> rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f, fun)

  | MkComp c0 _cloc _ <- c
    , Par _p c1 c2 <- c0
    , Par _p c21 c22 <- unComp c2
    , LetHeader fun (MkComp (Map _v f) _ _) <- unComp c21  -- c1 - (map f - c2)
    , Mitigate _ ty1 i1 j1 <- unComp c1      -- (c11' -- mitigate(i1,j1) -- map f) - c2
    , Mitigate _ ty2 i2 j2 <- unComp c22     -- (c11' -- mitigate(i2,j2) -- map f) - mitigate(i2,j2) - c2'
    , i1 >= j1 && i2 <= j2                      -- down mit and up mit
    , let d1 = i1 `div` j1
    , let d2 = j2 `div` i2
    , d1 == d2                                  -- exactly the same rate
    , funName fun == f
   -> rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f, fun)

  | otherwise
   -> return c
