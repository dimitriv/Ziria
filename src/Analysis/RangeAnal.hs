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
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, MultiParamTypeClasses, 
             FunctionalDependencies, StandaloneDeriving, 
             GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

-- | Range analysis
module Analysis.RangeAnal 
     ( Range ( .. )
     , Interval ( .. )
     , RngMap
     , pprRanges
     , varRanges
     )
where

import Control.Applicative
import Control.DeepSeq.Generics (NFData(..), genericRnf)
import Control.Monad.Error
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJ

import AstExpr
import Orphans ()

import Outputable 
import PpExpr ()

import NameEnv
import AbsInt
import CtExpr

import Opts

import qualified Data.Set as S
import qualified Data.Monoid as M
import Text.Show.Pretty (PrettyVal)


{- The goal of the Range analysis
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Given an expression, calculate:
      - for every array variable
          1) an overapproximation of the input range
          2) an exact output range or don't know (over or under approx not good)
   Later we will deal with structs too. -}

{----------------------------------------------------------------------
  Intervals
----------------------------------------------------------------------}

-- | Intervals
data Iv = Iv Integer Integer
type Interval = IVal Iv
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

instance NFData Interval where
  rnf = genericRnf

ijoin_apx :: Interval -> Interval -> Interval 
ijoin_apx IUnknown _ = IUnknown
ijoin_apx _ IUnknown = IUnknown
ijoin_apx (IKnown (Iv i j)) (IKnown (Iv i' j')) 
  | let l = min i i'
  , let h = max j j'
  , let bound = 512
  = if l < - bound || h > bound then IUnknown else IKnown (Iv l h)

instance NFData Interval where
  rnf = genericRnf

ijoin_pcs :: Interval -> Interval -> Interval
ijoin_pcs i1 i2 
  | i1 == i2  = i1
  | otherwise = IUnknown

istretch :: Bool -- ^ precise or not?
         -> Interval -> IVal i -> LengthInfo -> Interval 
istretch pcs _ IUnknown _ = IUnknown
istretch pcs IUnknown _ _ = IUnknown
istretch pcs (IKnown (Iv i j)) (IKnown s) LISingleton  = stretch pcs (i,j) (s,s)
istretch pcs (IKnown (Iv i j)) (IKnown s) (LILength n) 
  = stretch pcs (i,j) (s,s+n)

stretch :: Bool -> (Integer,Integer) -> (Integer,Integer) -> Interval 
stretch precise (a,b) (c,d)
  | precise && ( (c-b > 1) || (a-d > 1)) = IUnknown 
  | otherwise              = IKnown (min a c, max b d)

{- Note: stretching an interval: 
   -----------a---------b----c---------d  <- bad
   -----------a-----c------b-----------d
   -----------c-----a----b-------------d
   -----------c-----a----d-------------b
   -----------c-----d-----a------------b  <- bad 
-------------------------------------------------}


{----------------------------------------------------------------------
  The Range
----------------------------------------------------------------------}
-- | Range
data Range 
  = RInt  (IVal Int)       -- integers
  | RBool (IVal Bool)      -- booleans
  | RArr Interval Interval -- arrays
  | ROther                 -- other 
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

data IVal v = IUnknown | IKnown v
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

instance Monad IVal where
  return a = IKnown a
  m >>= f  = case m of 
    IUnknown -> IUnknown
    IKnown x -> f x

instance PrettyVal Interval
instance PrettyVal Range
instance PrettyVal IVal
instance Outputable Range where 
  ppr r = text (show r)

iv_join_discrete :: Eq v => IVal v -> IVal v -> IVal v
iv_join_discrete IUnknown _ = IUnknown
iv_join_discrete _ IUnknown = IUnknown
iv_join_discrete (IKnown v1) (IKnown v2)
  | v1 == v2  = IKnown v1
  | otherwise = IUnknown


rjoin :: Range -> Range -> Range
rjoin (RInt iv1)  (RInt iv2)
  = RInt  (iv1 `iv_join_discrete` iv2)
rjoin (RBool iv1) (RBool iv2)
  = RBool (iv1 `iv_join_discrete` iv2)
rjoin (RArr r1 w1) (RArr r2 w2)
  = RArr (r1 `ijoin_apx` r2) (w1 `ijoin_pcs` w2)
rjoin _ _  
  = ROther

rangeTop :: Ty -> Range 
rangeTop (TInt {})   = RInt IUnknown
rangeTop (TArray {}) = RArr IUnknown IUnknown
rangeTop (TBool)     = RBool IUnknown
rangeTop _           = ROther


runop :: UnOp -> Range -> Range
-- | This is sensitive to the type checker
runop Neg (RInt i)       = RInt $ liftM negate
runop Neg _              = ROther
runop ALength _          = RInt IUnknown
runop (Cast (TInt {})) _ = RInt IUnknown
runop (Cast _) _         = ROther
runop Not (RBool b)      = RBool $ liftM not
runop Not _              = ROther
runop NatExp (RInt {})   = RInt IUnknown
runop NatExp _           = ROther
runop BwNeg (RInt {})    = RInt IUnknown
runop BwNeg _            = ROther 


rbinop :: BinOp -> Range -> Range -> Range
-- | This is sensitive to the type checker

-- Arithmetic binops
rbinop Add (RInt r1) (RInt r2) = RInt $ liftM2 (+)
rbinop Sub _ _                 = RInt IUnknown
rbinop Mult  _r1 _r2 = RInt IUnknown
rbinop Div   _r1 _r2 = RInt IUnknown
rbinop Rem   _r1 _r2 = RInt IUnknown
rbinop Expon _r1 _r2 = RInt IUnknown

-- Other binops that can return integers
rbinop ShL _r1 _r2   = RInt IUnknown
rbinop ShR _r1 _r2   = RInt IUnknown
rbinop BwAnd (RInt {}) _ = RInt IUnknown
rbinop BwOr (RInt {}) _  = RInt IUnknown
rbinop BwXor (RInt {}) _ = RInt IUnknown

-- Booleans 
rbinop Eq  (RInt i1) (RInt i2) = RBool $ liftM2 (==)
rbinop Neq (RInt i1) (RInt i2) = RBool $ liftM2 (!=) 
rbinop Lt  (RInt i1) (RInt i2) = RBool $ liftM2 (<)
rbinop Gt  (RInt i1) (RInt i2) = RBool $ liftM2 (>)
rbinop Leq (RInt i1) (RInt i2) = RBool $ liftM2 (<=)
rbinop Geq (RInt i1) (RIht i2) = RBool $ liftM2 (>=)
rbinop Or _ _     = RBool IUnknown
rbinop And _ _    = RBool IUnknown
rbinop Eq  _ _    = RBool IUnknown
rbinop Neq _ _    = RBool IUnknown
rbinop Lt  _ _    = RBool IUnknown
rbinop Gt  _ _    = RBool IUnknown
rbinop Leq _ _    = RBool IUnknown
rbinop Geq _ _    = RBool IUnknown

-- Default cases 
rbinop Add _ _    = ROther
rbinop BwAnd _ _  = ROther
rbinop BwOr _ _   = ROther
rbinop BwXor _ _  = ROther


{----------------------------------------------------------------------
  Abstract domain for values
----------------------------------------------------------------------}

instance ValDom Range where
  aVal (VInt i)      = RInt (return i)
  aVal (VBool b)     = RBool (return b)
  aArr _vs           = RArr IUnknown IUnknown
  aStruct _ _        = ROther
  aUnOp uop rs       = runop uop rs
  aBinOp bop rsa rsb = rbinop bop rsa rsb

  aArrRead ret_ty _arr _ _ = rangeTop ret_ty
  aStrProj ret_ty _str _   = rangeTop ret_ty

{--------------------------------------------------------------------
  Abstract interpreter for commands
---------------------------------------------------------------------}

type RngMap = NameMap Ty Range

instance POrd Interval where 
  IBot       `pleq` _r         = True
  IRng l1 h1 `pleq` IRng l2 h2 = l2 <= l1 && h1 <= h2
  _          `pleq` ITop       = True
  _          `pleq` _          = False

instance POrd Range where 
  RInt intv1   `pleq` RInt intv2   = intv1 `pleq` intv2
  ROther       `pleq` ROther       = True
  RArr ri1 wi1 `pleq` RArr ri2 wi2 = ri1 `pleq` ri2 && wi1 `pleq` wi2
  r1 `pleq` r2  = error ("POrd Range:" ++ show r1 ++ " <= " ++ show r2)


joinRngMap :: RngMap -> RngMap -> RngMap
joinRngMap rm1 rm2 
  = neUnionWith rm1 rm2 (\_ r1 r2 -> rjoin r1 r2)

newtype Rng a = Rng (StateT RngMap (ErrorT Doc IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState RngMap
           , MonadError Doc
           , MonadIO
           )

hasChanged :: RngMap -> RngMap -> EId -> Bool
hasChanged rm1 rm2 x =
  case (neLookup x rm1, neLookup x rm2) of
    (Nothing,Nothing)  -> False
    (Nothing,Just {})  -> True
    (Just {},Nothing)  -> True
    (Just r1, Just r2) -> not (r1 `pleq` r2 && r2 `pleq` r1)

blowRange :: Range -> Range
blowRange (RInt {}) = RInt ITop
blowRange (RArr {}) = RArr ITop ITop
blowRange ROther    = ROther

blowTrue :: (EId -> Bool) -> RngMap -> RngMap
-- Blow the range of all those variables that satisfy the condition
blowTrue f rm = neFromList (map blow rmelems)
  where rmelems = neToList rm
        blow (x,r) = if f x then (x, blowRange r) else (x, r)

instance AbsInt Rng RngVal where
  aSkip = return $ RngVal ROther Nothing

  aWiden (RngVal _r Nothing) m1 m2 = aJoin m1 m2
  aWiden (RngVal _r (Just se)) m1 m2 = do
    let fvs = S.elems $ symExpFVs se
    (v1, post1) <- inCurrSt m1
    (v2, post2) <- inCurrSt m2

    -- Heuristic: if any of the ctrl flow variables has changed then
    -- just join and keep going, else there's a good chance we have
    -- stabilized, so blow all those that did change.
    let post = if any (hasChanged post1 post2) fvs
               then joinRngMap post1 post2
               else blowTrue (hasChanged post1 post2)
                             (joinRngMap post1 post2)

    put post
    return (rngValJoin v1 v2)


  aJoin m1 m2 = do

    (v1, post1) <- inCurrSt m1
    (v2, post2) <- inCurrSt m2

    let post = joinRngMap post1 post2

{- 
    liftIO $ print $
             vcat [ text "aJoin"
                  , nest 2 $
                    vcat [ text "post1 = "
                         , ppr post1
                         , text "~~~~"
                         , text "post2 = "
                         , ppr post2
                         ]
                  , nest 2 $
                    vcat [ text "joined post = "
                         , ppr post 
                         ]
                  ]
-}

    put post
    return (rngValJoin v1 v2)

  aWithFact (RngVal _r Nothing) action 
    = action
  aWithFact (RngVal _r (Just s)) action
    = do pre <- get 
{- 
         liftIO $ print (text "aWithFact" <+> text (show s))
         liftIO $ print (nest 2 $ text "pre-fact:")
         liftIO $ print (nest 2 $ ppr pre)
-}
         let pre' = pushFact s pre
{-
         liftIO $ print (nest 2 $ text "post-fact:")
         liftIO $ print (nest 2 $ ppr pre')
-}
         put pre'
         action

pushFact :: SymExp -> RngMap -> RngMap
pushFact (SymBinOp bop  (SymVar x) (SymVal (VInt i)))
  = neUpdate x (varop bop i) 
pushFact (SymBinOp bop (SymVal (VInt i)) (SymVar x))
  = neUpdate x (varop (sym_bop bop) i)
  where sym_bop Lt  = Gt
        sym_bop Gt  = Lt
        sym_bop Leq = Geq
        sym_bop Geq = Leq
        sym_bop Eq  = Eq
        sym_bop Neq = Neq
        sym_bop b   = error ("sym_bop:" ++ show b)
pushFact _ = id


varop :: BinOp -> Integer -> Maybe Range -> Maybe Range
varop _bop _i Nothing  = Nothing
varop bop i (Just rng) = Just $ restrict_range bop i rng

restrict_range :: BinOp -> Integer -> Range -> Range
restrict_range bop i (RInt intv) = RInt $ restrict_ival bop i intv
restrict_range _bop _i _r        = error "restrict_range"

restrict_ival :: BinOp -> Integer -> Interval -> Interval
restrict_ival Leq i (IRng rl rh)
  | i < rl = IBot
  | i < rh = IRng rl i
  | otherwise = IRng rl rh
restrict_ival Geq i (IRng rl rh)
  | i > rh    = IBot
  | i > rl    = IRng i rh
  | otherwise = IRng rl rh
restrict_ival Lt i (IRng rl rh) 
  = restrict_ival Leq (i-1) (IRng rl rh)
restrict_ival Gt i (IRng rl rh) 
  = restrict_ival Geq (i+1) (IRng rl rh)
restrict_ival Eq i (IRng rl rh) 
  | i <= rh && i >= rl = IRng i i
  | otherwise = IBot
restrict_ival _ _ (IRng {}) = ITop
restrict_ival _ _ IBot      = IBot
restrict_ival Eq i ITop     = IRng i i
restrict_ival _ _ _         = ITop


derefArr :: EId -> Range -> LengthInfo 
         -> (Range -> Range -> Range)  -- how to update array rng 
         -> Rng RngVal
derefArr x idx_range LISingleton upd_arr_rng = do 
  s <- get
--  liftIO $ print (text "derefArr, s =" <+> ppr s)
  let rng' = case neLookup x s of 
                Nothing -> upd_arr_rng (RArr IBot IBot) idx_range
                Just arr_range -> upd_arr_rng arr_range idx_range
--  liftIO $ print (text "-> rng' =" <+> ppr rng')
  put $ neUpdate x (\_ -> Just rng') s
  let TArray _ base_ty = nameTyp x
  return $ RngVal (rangeTop base_ty) Nothing
derefArr x idx_range (LILength n) upd_arr_rng = do 
  s <- get
  let idx_range' = rstretch idx_range (n-1)
  let rng' = case neLookup x s of 
                Nothing -> upd_arr_rng (RArr IBot IBot) idx_range'
                Just arr_range -> upd_arr_rng arr_range idx_range'
  put $ neUpdate x (\_ -> Just rng') s
  return $ RngVal (RArr ITop ITop) Nothing
derefArr _x _idx_range (LIMeta {}) _upd_arr_rng 
  = fail "derefArr: LIMeta!"

updArrRdRng :: Range -> Range -> Range
updArrRdRng arr_range idx_range = 
  case (arr_range, idx_range) of 
    (RArr rdintv wrintv, RInt i) -> RArr (rdintv `ijoin` i) wrintv
    o -> error ("updArrRdRng: " ++ show o)

updArrWrRng :: Range -> Range -> Range
updArrWrRng arr_range idx_range = 
  case (arr_range, idx_range) of 
    (RArr rdintv wrintv, RInt i) -> RArr rdintv (wrintv `ijoin` i)
    o -> error ("updArrWrRng: " ++ show o)


data RdWr = Rd | Wr Range

arrRdWrRng :: RdWr -> Int -> Range
arrRdWrRng Rd     n = RArr (IRng 0 (fromIntegral n - 1)) IBot 
arrRdWrRng (Wr _) n = RArr IBot (IRng 0 (fromIntegral n - 1))


varSetRng :: EId -> Range -> Rng RngVal
varSetRng x range = do
  s <- get
  put $ neUpdate x upd s
  return $ RngVal range (Just $ SymVar x)
  where upd _ = Just range

varJoinRng :: EId -> Range -> Rng RngVal
varJoinRng x range = do
  s <- get
  put $ neUpdate x upd s
  return $ RngVal range (Just $ SymVar x)
  where upd Nothing  = Just range
        upd (Just r) = Just (r `rjoin` range)

varGetRng :: EId -> Rng (Maybe Range)
varGetRng x = get >>= (return . neLookup x)

derefVar :: EId -> RdWr -> Rng RngVal
derefVar x rdwr = do
  case nameTyp x of
    TArray (Literal n) _
       -> varJoinRng x $ arrRdWrRng rdwr n
    TArray _meta _
       -> varJoinRng x $ RArr ITop ITop -- Unknown!
    TInt {}
       -> case rdwr of
            Rd -> do m <- varGetRng x
                     -- liftIO $ print (text "derefVar(rd): " <+> ppr x)
                     -- liftIO $ print (text "current range:" <+> text (show m))
                     let r'    = case m of { Nothing -> RInt ITop; Just r -> r }
                     let rval' = RngVal r' symx 
                     s <- get
                     put $ neUpdate x (\_ -> Just r') s  -- update the range of variable
                     return rval'

            Wr r -> do varSetRng x r
              -- liftIO $ print (text "derefVar(wr):" <+> ppr x)
              -- s <- get
              -- liftIO $ print (text "new range   :" <+> ppr (neLookup x s))
                       return (RngVal r symx)
    _other
       -> varJoinRng x $ ROther
  where symx = Just (SymVar x)


dbgRngSt :: Doc -> Rng a -> Rng a
dbgRngSt _d action = do
--  liftIO $ print d
--  pre <- get
--  liftIO $ print $ vcat [ nest 2 $ text "Pre:",  nest 2 $ ppr pre  ]
  x <- action
--  post <- get 
--  liftIO $ print $ vcat [ nest 2 $ text "Post:", nest 2 $ ppr post ]
  return x

instance CmdDom Rng RngVal where

  aDerefRead lval = dbgRngSt (text $ "aDerefRead, lval= " ++ show lval) (go lval)
     where go d
             | GDArr (GDVar x) (RngVal ridx _) linfo <- d
             = derefArr x ridx linfo updArrRdRng
             | GDVar x <- d            = derefVar x Rd
             | GDArr d' _ _ <- d
             = do _ <- go d'
                  return $ rngValTop (ctDerefExp d)
             | GDProj d' _ <- d
             = do _ <- go d'
                  return $ rngValTop (ctDerefExp d)
             | otherwise = error "aDerefRead"

  aAssign lval r 
     = dbgRngSt (vcat [ text $ "aAssign, lval= " ++ show lval
                      , text $ "rhs range, r = " ++ show r ])
                (go lval r)
     where go d (RngVal rng _symexp)
              | GDArr (GDVar x) (RngVal ridx _) linfo <- d
              = void $ derefArr x ridx linfo updArrWrRng
              | GDVar x <- d 
              = void $ derefVar x (Wr rng)
              | GDArr d' _ _ <- d
              = void $ go d' (RngVal (error "aAssign(A)") (error "aAssign(B)"))
              | GDProj d' _ <- d
              = void $ go d' (RngVal (error "aAssign(C)") (error "aAssign(D)"))
              | otherwise = error "aAssign"

  withImmABind nm rng action 
    = do pre <- get
         put $ neExtend nm (av_range rng) pre
         res <- action
         post <- get
         -- Delete variable from state
         put $ neUpdate nm (\_ -> Nothing) post
         return res

  withMutABind nm action 
    = do res <- action
         post <- get 
         -- Delete nm from state
         put $ neUpdate nm (\_ -> Nothing) post
         return res

  aCall  _ _ = fail "Calls not supported in range analysis"
  aError     = fail "Error not supported in range analysis"
  aPrint _ _ = return ()



{------------------------------------------------------------------------
  Running the analysis
------------------------------------------------------------------------}
deriving instance MonadState RngMap (AbsT Rng) 
deriving instance Monad (AbsT Rng)

pprRanges :: RngMap -> Doc
pprRanges r = vcat $
  map (\(k,v) -> ppr k <> char ':' <+> text (show v)) (neToList r)


runRng :: Rng a -> ErrorT Doc IO (a, RngMap)
runRng (Rng action) = runStateT action neEmpty

varRanges :: DynFlags -> Exp -> ErrorT Doc IO (RngMap, Range)
varRanges _dfs e =
  case action of
    AbsT m -> do
      (rngval, rmap) <- runRng m
      return (rmap, av_range rngval)
  where 
    action :: AbsT Rng RngVal
    action = absEvalRVal e
