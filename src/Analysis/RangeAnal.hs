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

{----------------------------------------------------------------------
  Definition of the Range
----------------------------------------------------------------------}

-- | Range
data Range 
  = RInt Interval
  | RArr Interval Interval
  | ROther 
  deriving (Generic, Typeable, Data, Eq, Show, Ord)

instance PrettyVal Interval
instance PrettyVal Range

instance Outputable Range where
  ppr r = text (show r)


rjoin :: Range -> Range -> Range
rjoin (RInt i1) (RInt i2)       = RInt (i1 `ijoin` i2)
rjoin (RArr r1 w1) (RArr r2 w2) = RArr (r1 `ijoin` r2) (w1 `ijoin` w2)
rjoin _ _                       = ROther

rstretch :: Range -> Int -> Range
rstretch (RInt i1) len = RInt (istretch i1 len)
rstretch _ _ = error "rstretch!"

istretch :: Interval -> Int -> Interval
istretch ITop _ = ITop
istretch (IRng l h) len = IRng l (h + fromIntegral len)
istretch IBot _len = ITop
 
rangeTop :: Ty -> Range 
rangeTop (TInt {})   = RInt ITop
rangeTop (TArray {}) = RArr ITop ITop
rangeTop _           = ROther

-- | Symbolic Expressions
data SymExp
  = SymVar EId
  | SymBinOp BinOp SymExp SymExp
  | SymUnOp UnOp SymExp
  | SymVal Val
  deriving (Generic, Typeable, Data, Eq, Ord, Show)


symExpFVs :: SymExp -> GNameSet Ty
symExpFVs (SymVar x) = S.singleton x
symExpFVs (SymBinOp _ s1 s2) = symExpFVs s1 `M.mappend` symExpFVs s2
symExpFVs (SymUnOp _ s)      = symExpFVs s
symExpFVs (SymVal {})        = M.mempty


instance Outputable SymExp where
  ppr s = text (show s)

sjoin :: Maybe SymExp -> Maybe SymExp -> Maybe SymExp
sjoin ms1 ms2 = do 
  s1 <- ms1
  s2 <- ms2
  if s1 == s2 then return s1 else Nothing

-- | Intervals
data Interval 
  = IBot | IRng Integer Integer | ITop
  deriving (Generic, Typeable, Data, Eq, Ord, Show)

instance NFData Interval where
  rnf = genericRnf

data RngVal 
  = RngVal { av_range :: Range, av_symex :: Maybe SymExp}
  deriving (Generic, Typeable, Data, Eq, Show)

instance Outputable RngVal where
  ppr r = text (show r)

rngValJoin :: RngVal -> RngVal -> RngVal
rngValJoin (RngVal r1 s1) (RngVal r2 s2) 
  = RngVal (r1 `rjoin` r2) (s1 `sjoin` s2)

rngValTop :: Ty -> RngVal
rngValTop ty = RngVal (rangeTop ty) Nothing

ineg :: Interval -> Interval
ineg IBot       = ITop
ineg ITop       = ITop 
ineg (IRng i j) = IRng (-j) (-i)

iplus :: Interval -> Interval -> Interval
iplus IBot x = x 
iplus x IBot = x
iplus ITop _ = ITop
iplus _ ITop = ITop
iplus (IRng i j) (IRng i' j') = IRng (i+i') (j+j')

ijoin :: Interval -> Interval -> Interval
ijoin IBot x = x
ijoin x IBot = x
ijoin ITop _ = ITop
ijoin _ ITop = ITop
ijoin (IRng i j) (IRng i' j')
  = -- To ensure convergence
    if l < - bound || h > bound then ITop else IRng l h
  where l = min i i'
        h = max j j'
        -- A modest bound 
        bound = 512


runop :: UnOp -> Range -> Range
runop Neg (RInt i)       = RInt (ineg i)
runop _   (RInt {})      = RInt ITop
runop ALength _          = RInt ITop -- That's a bit sad but oh well
runop (Cast (TInt {})) _ = RInt ITop
runop _ _                = ROther

sunop :: UnOp -> Maybe SymExp -> Maybe SymExp
sunop Neg s = s >>= \x -> return (SymUnOp Neg x)
sunop Not s = s >>= \x -> return (SymUnOp Not x)
sunop _ _s  = Nothing

rbinop :: BinOp -> Range -> Range -> Range
-- Arithmetic binops
rbinop Add (RInt r1) (RInt r2) = RInt (r1 `iplus` r2)
rbinop Sub _ _                 = RInt ITop
rbinop Mult  _r1 _r2 = RInt ITop
rbinop Div   _r1 _r2 = RInt ITop
rbinop Rem   _r1 _r2 = RInt ITop
rbinop Expon _r1 _r2 = RInt ITop
-- Other binops
rbinop _bop _r1 _r2  = ROther

symBinOp :: BinOp -> SymExp -> SymExp -> SymExp 
symBinOp Add (SymVal (VInt i)) (SymVal (VInt j)) = SymVal (VInt (i+j))
symBinOp Sub (SymVal (VInt i)) (SymVal (VInt j)) = SymVal (VInt (i-j))
symBinOp op s1 s2 = SymBinOp op s1 s2

sbinop :: BinOp -> Maybe SymExp -> Maybe SymExp -> Maybe SymExp
sbinop bop msa msb 
  | known_bop bop
  = do x <- msa 
       y <- msb
       return $ symBinOp bop x y
  | otherwise
  = Nothing
  where 
    known_bop Add = True
    known_bop Sub = True
    known_bop Eq  = True
    known_bop Neq = True
    known_bop Lt  = True
    known_bop Gt  = True
    known_bop Geq = True
    known_bop Leq = True
    known_bop And = True
    known_bop Or  = True
    known_bop _   = False

{----------------------------------------------------------------------
  Abstract domain for values
----------------------------------------------------------------------}

instance ValDom RngVal where
  aVal (VInt i)
    = RngVal { av_range = RInt (IRng i i)
             , av_symex = Just $ SymVal (VInt i) }
  aVal val 
    = RngVal { av_range = ROther         
             , av_symex = Just $ SymVal val }
  aArr _vs     
    = RngVal { av_range = RArr IBot IBot 
             , av_symex = Nothing }
  aStruct _ _
    = RngVal { av_range = ROther         
             , av_symex = Nothing }
  aUnOp uop rs
    = RngVal { av_range = runop uop (av_range rs)
             , av_symex = sunop uop (av_symex rs) }
  aBinOp bop rsa rsb 
    = RngVal { av_range = rbinop bop (av_range rsa) (av_range rsb)
             , av_symex = sbinop bop (av_symex rsa) (av_symex rsb) }


{------------------------------------------------------------------------
  Abstract interpreter for commands
------------------------------------------------------------------------}

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
                     case m of
                       Nothing -> return $ RngVal (RInt ITop) symx
                       Just r  -> return $ RngVal r symx
            Wr r -> do varJoinRng x r
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
             | GDArr _ _ (GDVar _ _ x) (RngVal ridx _) linfo <- d
             = derefArr x ridx linfo updArrRdRng
             | GDVar _ _ x <- d            = derefVar x Rd
             | GDNewArray _ _ _ vs    <- d = return $ aArr vs
             | GDNewStruct _ _ t flds <- d = return $ aStruct t flds
             | GDArr _ _ d' _ _ <- d
             = do _ <- go d'
                  return $ rngValTop (ctDerefExp d)
             | GDProj _ _ d' _ <- d
             = do _ <- go d'
                  return $ rngValTop (ctDerefExp d)
             | otherwise = error "aDerefRead"

  aAssign lval r 
     = dbgRngSt (vcat [ text $ "aAssign, lval= " ++ show lval
                      , text $ "rhs range, r = " ++ show r ])
                (go lval r)
     where go d (RngVal rng _symexp)
              | GDArr _ _ (GDVar _ _ x) (RngVal ridx _) linfo <- d
              = void $ derefArr x ridx linfo updArrWrRng
              | GDVar _ _ x <- d 
              = void $ derefVar x (Wr rng)
              | GDArr _ _ d' _ _ <- d
              = void $ go d' (RngVal undefined undefined)
              | GDProj _ _ d' _ <- d
              = void $ go d' (RngVal undefined undefined)
              | otherwise = error "aAssign"
              | GDNewArray {} <- d
              = return () -- Writing to an unnamed array has no effects
              | GDNewStruct {} <- d
              = return () -- Writing to an unnamed struct has no effects

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
    action = absEval e
