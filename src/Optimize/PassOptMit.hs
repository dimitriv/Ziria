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
  , passElimMitigs

) where

import Prelude hiding (exp)

import AstComp
import AstExpr

import Control.Monad ( when ) 
import AstUnlabelled
import Outputable ()
import PpComp ()
import PpExpr ()

import Text.Parsec ( SourcePos )

import PassFoldM

import CtComp ( ctComp )

-- import Control.Monad.IO.Class ( liftIO )

{-------------------------------------------------------------------------------
  Fuse together consecutive mitigators connected with a Map
  Somewhat specialized optimization for code emitted by the vectorizer.
-------------------------------------------------------------------------------}

-- TODO: Add logStep and corresponding test case.
-- TODO: Needs revision: does not capture nesting well (see frmMit,flmMit below)
passElimAutomappedMitigs :: TypedCompPass
passElimAutomappedMitigs = TypedCompBottomUp $ \_cloc c -> if
  | MkComp c0 _cloc _ <- c
    , Par _p c1 c2 <- c0                 -- c = c1 >>> c2
    , Par _p c11 c12 <- unComp c1        --   = (c11 >>> c12) >>> c2
    , LetHeader fun k <- unComp c12      --   = ..
    , Map _v f <- unComp k               --   = ..
    , funName fun == f                   --   = (c11 >> map f) >>> c2
    , Mitigate _ ty1 i1 j1 <- unComp c11 --   = (mit(i1,j1) >>> map f) >>> c2
    , Mitigate _ ty2 i2 j2 <- unComp c2  --   = (mit(i1,j1) >>> map f) >>> mit(i2,j2)
    , i1 >= j1 && i2 <= j2              
    , let d1 = i1 `div` j1
    , let d2 = j2 `div` i2
    , d1 == d2                           -- same rate, i.e. can use an inner loop
   -> rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f, fun)

  | MkComp c0 _cloc _ <- c
    , Par _p c1 c2 <- c0                 -- c = c1 >>> c2
    , Par _p c21 c22 <- unComp c2        --   = c1 >>> (c21 >>> c22)
    , LetHeader fun k <- unComp c21      --   = ..
    , Map _v f <- unComp k               --   = ..
    , funName fun == f                   --   = c1 >>> (map f >>> c22)
    , Mitigate _ ty1 i1 j1 <- unComp c1  --   = mit(i1,j1) >>> (map f >>> c22)
    , Mitigate _ ty2 i2 j2 <- unComp c22 --   = mit(i1,j1) >>> (map f >>> mit(i2,j2))
    , i1 >= j1 && i2 <= j2
    , let d1 = i1 `div` j1
    , let d2 = j2 `div` i2
    , d1 == d2                           -- same rate
   -> rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f, fun)

  | otherwise -> return c


-- | Fuse two consecutive mitigators as a map with an inner loop.
rewrite_mit_map :: Ty -> (Int,Int) -> Ty -> (Int,Int) -> (EId, Fun) -> RwM Comp
-- Precondition:  i1 `div` j1 == j2 `div` i2
rewrite_mit_map ty1 (i1,j1) ty2 (i2,j2) (f_name, fun) = do 
  let rng1 = if j1 == 1 then LISingleton else LILength j1
  let rng2 = if i2 == 1 then LISingleton else LILength i2
  let d = i1 `div` j1
  let floc = funLoc fun

  -- input variable
  let x_ty   = TArray (Literal i1) ty1     -- input type
  x_name <- newPassFoldGName "x" x_ty floc Imm
  let x_exp  = eVar floc x_name

  
  -- output variable
  let y_ty   = TArray (Literal j2) ty2     -- output type
  y_name <- newPassFoldGName "y" y_ty floc Mut
  let y_exp  = eVar floc y_name

  -- name of new map function
  let f_ty = TArrow [(GArgTy x_ty Imm)] y_ty
  new_f_name <- newPassFoldGName "auto_map_mit" f_ty floc Imm


  -- new counter
  i_name <- newPassFoldGName "i" tint floc Imm
  let i_exp  = eVar floc i_name

  -- zero and 'd'
  let ezero = eVal floc tint (vint (0 :: Int))
  let e_d   = eVal floc tint (vint d)


  let new_body
         = eLetRef floc y_name Nothing $
           eSeq floc
            -- do the for loop
            (eFor floc AutoUnroll i_name ezero e_d ekernel)
            -- and return y
            y_exp 

      write_idx = eBinOp floc Mult (eVal floc tint (vint i2)) i_exp
      read_idx  = eBinOp floc Mult (eVal floc tint (vint j1)) i_exp

      earrrd    = eArrRead floc x_exp read_idx rng1
      ekernel   = eArrWrite floc y_exp write_idx rng2 $
                  eCall floc f_name [earrrd]

  let new_mapper = cMap floc Nothing new_f_name
  let new_fun = MkFun (MkFunDefined new_f_name [x_name] new_body) floc ()

  rewrite $
   cLetHeader floc fun $   -- original function
   cLetHeader floc new_fun new_mapper



{-------------------------------------------------------------------------------
  Eliminate or fuse consecutive mitigators
-------------------------------------------------------------------------------}

-- TODO: make MitPkg part of the AST
data MitPkg = MitPkg { mit_ty  :: Ty
                     , mit_in  :: Int
                     , mit_out :: Int
                     , mit_ctx :: String }

-- | Get the type of a mitigator package
mitPkgCTy :: MitPkg -> CTy 
mitPkgCTy (MitPkg { mit_ty = t, mit_in = i, mit_out = j, mit_ctx = s })
  = ctComp (cMitigate Nothing s t i j)

-- | If the computation /ends/ with a Mitigator then give it back
-- NB: If the computation consists of just a mitigator we will 
-- return Nothing.
frmMit :: Comp -> Maybe (MitPkg,Comp)
frmMit c = find_mit c on_par frmMit
  where on_par loc p c1 c2
          | Mitigate s ty i1 i2 <- unComp c2 
          = return (MitPkg ty i1 i2 s, c1)
          | otherwise
          = do (mit,c2') <- frmMit c2
               return (mit, cPar loc p c1 c2')

-- | Dual, get the leftmost mitigator
flmMit :: Comp -> Maybe (MitPkg, Comp)
flmMit c = find_mit c on_par flmMit
  where on_par loc p c1 c2
          | Mitigate s ty i1 i2 <- unComp c1 
          = return (MitPkg ty i1 i2 s, c2)
          | otherwise
          = do (mit,c1') <- flmMit c1
               return (mit, cPar loc p c1' c2)

find_mit :: Comp 
   -> (Maybe SourcePos -> ParInfo -> Comp -> Comp -> Maybe (MitPkg,Comp))
   -> (Comp -> Maybe (MitPkg,Comp))
   -> Maybe (MitPkg,Comp)              
find_mit c on_par on_rec
  | Par p c1 c2 <- unComp c = on_par loc p c1 c2
  | LetHeader fdef cont <- unComp c
  = do (mit,cont') <- on_rec cont
       return (mit,cLetHeader loc fdef cont')

    -- Special case for code emitted by the vectorizer
  | LetFunC fn prms body cont <- unComp c
  , Call fn' _args <- unComp cont
  , fn == fn'
  = do (mit,body') <- on_rec body
       return (mit,cLetFunC loc fn prms body' cont)
  | LetFunC fn prms body cont <- unComp c
  = do (mit,cont') <- on_rec cont
       return (mit,cLetFunC loc fn prms body cont')
  | Let n c1 cont <- unComp c
  = do (mit,cont') <- on_rec cont
       return (mit,cLet loc n c1 cont')
  | LetE n f e cont <- unComp c
  = do (mit,cont') <- on_rec cont
       return (mit, cLetE loc n f e cont')
  | LetERef n me cont <- unComp c
  = do (mit,cont') <- on_rec cont 
       return (mit,cLetERef loc n me cont')
  | otherwise = Nothing
  where loc = compLoc c


flmMitTop :: Comp -> Maybe (MitPkg, Maybe Comp)
flmMitTop comp 
  = case flmMit comp of 
      Just (mit,comp') -> return (mit,Just comp')
      Nothing 
        | Mitigate s ty i1 i2 <- unComp comp
        -> return (MitPkg ty i1 i2 s, Nothing)
      _ -> Nothing

frmMitTop :: Comp -> Maybe (MitPkg, Maybe Comp)
frmMitTop comp 
  = case frmMit comp of 
      Just (mit,comp') -> return (mit,Just comp')
      Nothing 
        | Mitigate s ty i1 i2 <- unComp comp
        -> return (MitPkg ty i1 i2 s, Nothing)
      _ -> Nothing

cParMbR :: Maybe SourcePos -> ParInfo -> Comp -> Maybe Comp -> Comp
cParMbR _loc _p comp Nothing     = comp
cParMbR loc p comp1 (Just comp2) = cPar loc p comp1 comp2

cParMbL :: Maybe SourcePos -> ParInfo -> Maybe Comp -> Comp -> Comp
cParMbL _loc _p Nothing comp     = comp
cParMbL loc p (Just comp1) comp2 = cPar loc p comp1 comp2


{- Elimination of mitigators proper -}
passElimMitigs :: TypedCompPass
passElimMitigs = TypedCompBottomUp elim_mit

elim_mit :: Maybe SourcePos -> GComp CTy Ty () () -> RwM Comp
elim_mit cloc c
  | Par p c1 c2 <- unComp c
  , Just (MitPkg ty1 i1 j1 s1, mc1') <- frmMitTop c1
  , Just (MitPkg ty2 i2 j2 s2, mc2') <- flmMitTop c2
  , let l = lcm i1 j2
  = do when (j1 /= i2) $ error "BUG: Mitigation mismatch!"
       if (i1 `mod` j2 == 0) || (j2 `mod` i1) == 0 
        then
          let mit = cMitigate cloc (s1++"o"++s2) ty1 i1 j2
          in rewrite $ cParMbL cloc p mc1' (cParMbR cloc pnever mit mc2')
        else if l /= j1 then
                let mit1 = cMitigate cloc (s1++"-") ty1 i1 l
                    mit2 = cMitigate cloc ("-"++s2) ty2 l j2
                in 
                rewrite $ 
                  cPar cloc p (cParMbL cloc pnever mc1' mit1)
                              (cParMbR cloc pnever  mit2 mc2')
             else return c

    -- throw away useless mitigators!
  | Par p c1 c2 <- unComp c
  , Just (mp,mc1') <- frmMitTop c1
  , WriteSnk _ty <- unComp c2
  , let c2' = cWriteSnk cloc (inTyOfCTy (mitPkgCTy mp))
  = rewrite $ cParMbL cloc p mc1' c2'

  | Par p c1 c2 <- unComp c
  , Just (mp,mc1') <- frmMitTop c1
  , WriteInternal _ty bid <- unComp c2
  , let c2' = cWriteInternal cloc (inTyOfCTy (mitPkgCTy mp)) bid
  = rewrite $ cParMbL cloc p mc1' c2'

  | Par p c1 c2 <- unComp c
  , Just (mp,mc2') <- flmMitTop c2
  , ReadSrc _ty <- unComp c1
  , let c1' = cReadSrc cloc (yldTyOfCTy (mitPkgCTy mp))
  = rewrite $ cParMbR cloc p c1' mc2'

  | Par p c1 c2 <- unComp c
  , Just (mp,mc2') <- flmMitTop c2
  , ReadInternal _ty bid rt <- unComp c1
  , let c1' = cReadInternal cloc (yldTyOfCTy (mitPkgCTy mp)) bid rt
  = rewrite $ cParMbR cloc p c1' mc2'


  | Par p c1 c2 <- unComp c
  , Just (mp,mc1') <- frmMitTop c1
  , mit_in mp == mit_out mp
  = rewrite $ cParMbL cloc p mc1' c2

  | Par p c1 c2 <- unComp c
  , Just (mp,mc2') <- flmMitTop c2
  , mit_in mp == mit_out mp
  = rewrite $ cParMbR cloc p c1 mc2'

  | Par p c1 c2 <- unComp c
  , Just (_mp,mc1') <- frmMitTop c1
  , TVoid <- inTyOfCTy (ctComp c2)
  = rewrite $ cParMbL cloc p mc1' c2

  | Par p c1 c2 <- unComp c
  , Just (_mp,mc2') <- flmMitTop c2
  , TVoid <- yldTyOfCTy (ctComp c1)
  = rewrite $ cParMbR cloc p c1 mc2'

  | otherwise
  = return c
