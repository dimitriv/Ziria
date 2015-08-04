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
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

-- | Cardinality analysis to drive the vectorizer
module CardAnalysis (
     Card (..)
   , CAlpha (..)
   , LComp, LComp0
   , isSimplCard_mb
   , runCardAnal
   , scard
   , ocard

     -- For debugging/assertions
   , unknown_times_card
   ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception.Base ( assert )

import AstExpr
import AstComp
import AstLabelled
import CtExpr ( ctExp )
import Interpreter (evalInt)

import Opts

import Outputable
import Text.PrettyPrint.HughesPJ


{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Computation labelled with cardinality analysis results
type LComp  = GComp CTy Ty Card ()
type LComp0 = GComp0 CTy Ty Card ()

-- | Run the cardinality analysis
runCardAnal :: DynFlags -> Comp -> IO LComp
runCardAnal dflags
  = runCardM [] . computeCard dflags

{-------------------------------------------------------------------------------
  Cardinality data types
-------------------------------------------------------------------------------}

data Card
  = -- | Cardinality of a "simple computer".
    -- A simple computer is a computer that does not
    -- contain any uses of ">>>"
    SCard CAlpha -- input  alpha
          CAlpha -- output alpha
  | OCard
  deriving Show
data CAlpha
  = CAStatic Int -- A concrete N >= 0.
  | CAMultOf Int -- Some x, which is a multiple of a concrete N >= 1
  | CAUnknown    -- Could not deduce or data dependent/dynamic.
  deriving Show

instance Outputable Card where
  ppr card = text (show card)

-- | Construct a cardinality
scard :: Int -> Int -> Card
scard i j = SCard (CAStatic i) (CAStatic j)
ocard :: Card
ocard = OCard

-- | Sum two alphas
aplus :: CAlpha -> CAlpha -> CAlpha
aplus (CAStatic n1) (CAStatic n2) = CAStatic (n1+n2)
aplus (CAStatic n1) (CAMultOf n2)
  | n1 `mod` n2 == 0              = CAMultOf n2
  | otherwise                     = CAUnknown
aplus (CAMultOf n1) (CAMultOf n2)
  | let n = gcd n1 n2
  , n > 1
  = CAMultOf n
  | otherwise = CAUnknown
aplus _a CAUnknown = CAUnknown
aplus x y = aplus y x


-- | Sum two cardinalities
cplus :: Card -> Card -> Card
cplus (SCard ain aout)
      (SCard ain' aout') = SCard (ain `aplus` ain') (aout `aplus` aout')
cplus (SCard {}) OCard   = ocard
cplus OCard _            = ocard

-- | Sum together a list of cardinalities
cplus_many :: [Card] -> Card
-- Pre: non-empty list
cplus_many cs = foldl cplus (head cs) (tail cs)


-- | Multiply an alpha with a second argument that describes how many
-- times to iterate
--
-- Either a fixed number of times, or an unknown number of times or a
-- multiple of a fixed number
atimes :: CAlpha -> CAlpha -> CAlpha
atimes ca cb = go cb
  where
    go (CAStatic j) = atimes_static ca j
    go CAUnknown    = atimes_unknown ca
    go (CAMultOf j) = atimes_mult ca j

    atimes_static (CAStatic i) j = CAStatic (i*j)
    atimes_static (CAMultOf i) j = CAMultOf (i*j)
    atimes_static CAUnknown _    = CAUnknown

    atimes_unknown (CAStatic 0)  = CAStatic 0
    atimes_unknown (CAMultOf i)  = CAMultOf i
    atimes_unknown _other        = CAUnknown

    atimes_mult (CAStatic i) j = CAMultOf (i*j) -- i*x*j   = x*(i*j)
    atimes_mult (CAMultOf i) j = CAMultOf (i*j) -- y*i*x*j = x*y*(i*j)
    atimes_mult CAUnknown _    = CAUnknown

-- | This is an invariant on ctimes card CAUnknown
-- which is called for unknown number of iterations in while and Until nodes
unknown_times_card :: Card -> Bool
unknown_times_card OCard = True
unknown_times_card (SCard CAUnknown CAUnknown)       = True
unknown_times_card (SCard CAUnknown (CAStatic 0))    = True
unknown_times_card (SCard (CAStatic 0) CAUnknown)    = True
unknown_times_card (SCard (CAStatic 0) (CAStatic 0)) = True
unknown_times_card _other = False 

-- | Multiply a cardinality with an argument specifying how many times
--   to iterate.
ctimes :: Card -> CAlpha -> Card
ctimes (SCard ain aout) how_many
  = SCard (ain `atimes` how_many) (aout `atimes` how_many)
ctimes OCard _j = ocard

-- | Join two alphas
ajoin :: CAlpha -> CAlpha -> CAlpha
ajoin CAUnknown _  = CAUnknown
ajoin _ CAUnknown  = CAUnknown
ajoin (CAStatic i) (CAStatic j)
  | i == j         = CAStatic i
  | otherwise      = CAUnknown
ajoin (CAStatic i) (CAMultOf j)
  | i `mod` j == 0 = CAMultOf j
  | otherwise      = CAUnknown
ajoin (CAMultOf i) (CAMultOf j)
  | let n = gcd i j
  , n > 1          = CAMultOf n
  | otherwise      = CAUnknown
ajoin x y = ajoin y x


-- | Join two cardinalities
cjoin :: Card -> Card -> Card
cjoin OCard _ = ocard
cjoin _ OCard = ocard
cjoin (SCard ain aout) (SCard bin bout)
  = SCard (ain `ajoin` bin) (aout `ajoin` bout)


isSimplCard_mb :: Card -> Maybe (CAlpha, CAlpha)
isSimplCard_mb (SCard ain aout) = Just (ain,aout)
isSimplCard_mb _other           = Nothing


{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

data CEnvDom
  = CDomVar CId -- A (computation) variable
  | CDomFun CId -- A (computation) function
  deriving Eq

type CEnv = [(CEnvDom,Card)]

-- | Cardinality monad
--
-- NOTE: We don't actually rely on MonadIO anywhere
-- nor do we use the DynFlags. But I am keeping them
-- here for ease of debugging.

newtype CardM a = CardM (ReaderT CEnv IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader CEnv
           )

runCardM :: CEnv -> CardM a -> IO a
runCardM init_env (CardM f)
  = runReaderT f init_env

extendCVarEnv :: CId -> Card -> CardM a -> CardM a
extendCVarEnv nm c
  = local ((CDomVar nm, c) :)

extendCFunEnv :: CId -> Card -> CardM a -> CardM a
extendCFunEnv nm c
  = local ((CDomFun nm, c) :)

-- | Lookup variable cardinality
lookupVar :: CId -> CardM Card
lookupVar x = do
  cenv <- ask
  case lookup (CDomVar x) cenv of
    Just card -> return card
    Nothing   -> error $ "BUG: Unbound " ++ name x

-- | Lookup function cardinality
lookupFun :: CId -> CardM Card
lookupFun f = do
  cenv <- ask
  case lookup (CDomFun f) cenv of
    Just card -> return card
    Nothing   -> error $ "BUG: Unbound " ++ name f


{-------------------------------------------------------------------------------
  The analysis proper
-------------------------------------------------------------------------------}

computeCard :: DynFlags -> Comp -> CardM LComp
computeCard _dflags = go
  where
    go :: Comp -> CardM LComp
    go (MkComp c0 loc ()) = case c0 of

      Var x -> do
        card <- lookupVar x
        return $ cVar loc card x

      BindMany c1 xs_cs -> do
        c1' <- go c1
        let (xs, cs) = unzip xs_cs
        cs' <- mapM go cs
        let card = cplus_many $ map compInfo (c1':cs')
        return $ cBindMany loc card c1' (zip xs cs')

      Seq c1 c2 -> do
        c1' <- go c1
        c2' <- go c2
        let card = cplus_many [compInfo c1', compInfo c2']
        return $ cSeq loc card c1' c2'

      Par p c1 c2 -> do
        c1' <- go c1
        c2' <- go c2
        return $ cPar loc ocard p c1' c2'

      Let x c1 c2 -> do
        c1' <- go c1
        c2' <- extendCVarEnv x (compInfo c1') $ go c2
        return $ cLet loc (compInfo c2') x c1' c2'

      LetE x fi e c1 -> do
        c1' <- go c1
        return $ cLetE loc (compInfo c1') x fi e c1'

      LetERef x y c1 -> do
        c1' <- go c1
        return $ cLetERef loc (compInfo c1') x y c1'

      LetHeader fn c1 -> do
        c1' <- go c1
        return $ cLetHeader loc (compInfo c1') fn c1'

      LetStruct sdef c1 -> do
        c1' <- go c1
        return $ cLetStruct loc (compInfo c1') sdef c1'

      LetFunC f params c1 c2 -> do
        c1' <- cargs_extend params $ go c1
        c2' <- extendCFunEnv f (compInfo c1') $ go c2
        return $ cLetFunC loc (compInfo c2') f params c1' c2'

      Call f es -> do
        card <- lookupFun f
        es'  <- mapM go_callarg es
        return $ cCall loc card f es'

      Emit e      -> return $ cEmit   loc (scard 0 1) e
      Emits e     -> return $ cEmits  loc (emits_card $ ctExp e) e
      Return fi e -> return $ cReturn loc (scard 0 0) fi e
      Take1 a     -> return $ cTake1  loc (scard 1 0) a
      Take a n    -> return $ cTake   loc (scard n 0) a n

      Interleave c1 c2 -> do
        c1' <- go c1
        c2' <- go c2
        return $ cInterleave loc ocard c1' c2'

      Branch e c1 c2 -> do
        c1' <- go c1
        c2' <- go c2
        let card1 = compInfo c1'
            card2 = compInfo c2'
        let card  = card1 `cjoin` card2
        return $ cBranch loc card e c1' c2'

      Until e c1 -> do
        c1' <- go c1
        -- Don't know how many times to iterate
        let card = ctimes (compInfo c1') CAUnknown
        assert (unknown_times_card card) $ 
          return (cUntil loc card e c1')

      While e c1 -> do
        c1' <- go c1
        -- Don't know how many times to iterate
        let card = ctimes (compInfo c1') CAUnknown
        assert (unknown_times_card card) $
          return (cWhile loc card e c1')
  
      Times ui e elen x c1 -> do
        c1' <- go c1
        let alpha = getint_calpha elen
            card  = ctimes (compInfo c1') alpha
        return $ cTimes loc card  ui e elen x c1'

      Repeat hint c1 -> cRepeat loc ocard hint <$> go c1

      VectComp hint c1 -> do
        c1' <- go c1
        -- NB: We return ocard to prevent vectorizing expressions that
        -- have nested VectComp annotations.
        return $ cVectComp loc ocard  hint c1'

      Map w nm     -> return $ cMap loc ocard w nm
      Filter e     -> return $ cFilter loc ocard e
      ReadSrc mty  -> return $ cReadSrc loc ocard mty
      WriteSnk mty -> return $ cWriteSnk loc ocard mty
      ReadInternal a s tp -> return $ cReadInternal loc ocard a s tp
      WriteInternal a s -> return $ cWriteInternal loc ocard a s
      Standalone c1 -> do
        c1' <- go c1
        return $ cStandalone loc (compInfo c1') c1'
      Mitigate s t n1 n2  -> return $ cMitigate loc ocard s t n1 n2

    go_callarg :: CallArg Exp Comp -> CardM (CallArg Exp LComp)
    go_callarg (CAExp  e) = return $ CAExp e
    go_callarg (CAComp c) = CAComp <$> go c

    getint_calpha :: Exp -> CAlpha
    getint_calpha e
      | (Right j, []) <- evalInt e
      = CAStatic $ fromIntegral j
      | otherwise = CAUnknown

    -- extend environment for callargs: use ocard (unknown)
    -- as approximation of actual cardinality of argument
    cargs_extend [] m = m
    cargs_extend (pm:pms) m
      | MkName nm uid (CAComp cty) loc _mk d <- pm
      , let pm' = MkName nm uid cty loc _mk d
      = extendCVarEnv pm' ocard $ cargs_extend pms m
      | otherwise
      = cargs_extend pms m

    emits_card (TArray (Literal n) _) = scard 0 n
    emits_card _other                 = ocard
