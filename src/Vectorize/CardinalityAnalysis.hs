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
--
-- TODO: Currently we export all the Card constructors, but only one is used
-- outside of this  module (SimplCard), along with the isSimplCard check.
-- Maybe we can find a nicer way to do this.
module CardinalityAnalysis (
    runCardinalityAnalysis
  , Card(SimplCard)
  , isSimplCard
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import AstExpr
import AstComp
import AstLabelled
import CtExpr (ctExp)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Computation labelled with cardinality analysis results
type LComp = GComp CTy Ty Card ()

-- | Run the cardinality analysis
runCardinalityAnalysis :: Bool -> Comp -> IO LComp
runCardinalityAnalysis verbose = runCardM [] . computeCardTop verbose

{-------------------------------------------------------------------------------
  Cardinality
-------------------------------------------------------------------------------}

data Card
  =  -- | Simple cardinality
     SimplCard (Maybe Int) (Maybe Int)

    -- | Iterated cardinality, such as the one arising from until / repeat
    --
    -- We might actually statically know the iteration count, hence a Maybe Int
  | IterCard (Maybe Int) Card

    -- | Sum of all cardinalities
  | SumCard [Card]

    -- | Underlying cardinalities for (>>>) composition
  | ParCard Card Card

    -- | Dynamic cardinality, can't tell anything
  | DynamicCard
  deriving ( Show )

isSimplCard :: Card -> Bool
isSimplCard (SimplCard {}) = True
isSimplCard _ = False

cardEqual :: Card -> Card -> Bool
cardEqual (SimplCard (Just i) (Just j))
          (SimplCard (Just i') (Just j')) = (i==i') && (j == j')
cardEqual (SimplCard _ _) (SimplCard _ _) = False  -- Could contain some unknowns
cardEqual (IterCard (Just i) c1) (IterCard (Just j) c2) = (i==j) && (cardEqual c1 c2)
cardEqual (ParCard c1 c2) (ParCard c1' c2') = cardEqual c1 c1' && cardEqual c2 c2'
cardEqual (SumCard cs) (SumCard cs') = all (\(c,c') -> cardEqual c c') (zip cs cs')
cardEqual _ _ = False

mkSumCard :: [Card] -> Card
mkSumCard [c] = c
mkSumCard cs
 | all isSimplCard cs
 = let is = do { (xs :: [Int]) <- sequence ins
               ; return (sum xs) }
       os = do { (xs :: [Int]) <- sequence outs
               ; return (sum xs) }
   in case (is,os) of
        (Nothing, Nothing) -> mkDynamicCard
        _otherwise         -> SimplCard is os
 | otherwise
 = SumCard cs
 where ins  = map (\(SimplCard i _) -> i) cs
       outs = map (\(SimplCard _ j) -> j) cs

mkSimplCard :: Int -> Int -> Card
-- Definitely known input and output cardinalities
mkSimplCard i j = SimplCard (Just i) (Just j)

mkIterCard :: Maybe Int -> Card -> Card
mkIterCard (Just 1) c = c
-- No takes, just convert this to a simpl-card and unknown emits:
mkIterCard Nothing (SimplCard (Just 0) _other) = SimplCard (Just 0) Nothing
-- No emits, just convert this to a simpl-card and unknown takes
mkIterCard Nothing (SimplCard _other (Just 0)) = SimplCard Nothing (Just 0)
-- Fall back to just IterCard
mkIterCard m c = IterCard m c

mkParCard :: Card -> Card -> Card
mkParCard c1 c2 = ParCard c1 c2

mkDynamicCard :: Card
mkDynamicCard = DynamicCard

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

data CEnvDom = CDomVar String  -- A (computation) variable
             | CDomFun String  -- A (computation) function
  deriving Eq

type CEnv = [(CEnvDom,Card)]

-- | Cardinality monad
--
-- NOTE: We don't actually rely on MonadIO anywhere (though currently we also
-- ignore the `verbose` argument to `runCardinalityAnalysis`)
newtype CardM a = CardM (ReaderT CEnv IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader CEnv
           )

runCardM :: CEnv -> CardM a -> IO a
runCardM init_env (CardM f) = runReaderT f init_env

extendCardCVarEnv :: GName CTy -> Card -> CardM a -> CardM a
extendCardCVarEnv nm c = local ((CDomVar $ name nm, c) :)

extendCardCFunEnv :: GName CTy -> Card -> CardM a -> CardM a
extendCardCFunEnv nm c = local ((CDomFun $ name nm, c) :)

-- | Lookup variable cardinality
--
-- NOTE: We do not currently add bindings in the environment for computation
-- arguments to functions, so this lookup may fail.
--
-- TODO: It would be nicer if we _did_ introduce these arguments; see LetFunC.
lookupVar :: GName CTy -> CardM Card
lookupVar x = do
  cenv <- ask
  case lookup (CDomVar $ name x) cenv of
    Just card -> return card
    Nothing   -> return mkDynamicCard

-- | Lookup function cardinality
--
-- Functions _must_ always be bound (they cannot be passed as arguments
-- to other functions, so the fact that we do not extend the environment
-- with the function parameters in LetFunC does not matter here)
lookupFun :: GName CTy -> CardM Card
lookupFun f = do
  cenv <- ask
  case lookup (CDomFun $ name f) cenv of
    Just card -> return card
    Nothing   -> error $ "Unbound function " ++ name f

{-------------------------------------------------------------------------------
  The analysis proper
-------------------------------------------------------------------------------}

computeCardTop :: Bool -> Comp -> CardM LComp
computeCardTop _verbose = computeCard
  where
    computeCard :: Comp -> CardM LComp
    computeCard (MkComp c0 loc ()) = case c0 of
      Var x -> do
        card <- lookupVar x
        return $ cVar loc card x

      BindMany c1 xs_cs -> do
        c1' <- computeCard c1
        let (xs, cs) = unzip xs_cs
        cs' <- mapM computeCard cs
        let card = mkSumCard $ map compInfo (c1':cs')
        return $ cBindMany loc card c1' (zip xs cs')

      Seq c1 c2 -> do
        c1' <- computeCard c1
        c2' <- computeCard c2
        let card = mkSumCard [compInfo c1', compInfo c2']
        return $ cSeq loc card c1' c2'

      Par p c1 c2 -> do
        c1' <- computeCard c1
        c2' <- computeCard c2
        let card = mkParCard (compInfo c1') (compInfo c2')
        return $ cPar loc card p c1' c2'

      Let x c1 c2 -> do
        c1' <- computeCard c1
        c2' <- extendCardCVarEnv x (compInfo c1') $ computeCard c2
        return $ cLet loc (compInfo c2') x c1' c2'

      LetE x fi e c1 -> do
        c1' <- computeCard c1
        return $ cLetE loc (compInfo c1') x fi e c1'

      LetERef x y c1 -> do
        c1' <- computeCard c1
        return $ cLetERef loc (compInfo c1') x y c1'

      LetHeader fn c1 -> do
        c1' <- computeCard c1
        return $ cLetHeader loc (compInfo c1') fn c1'

      LetStruct sdef c1 -> do
        c1' <- computeCard c1
        return $ cLetStruct loc (compInfo c1') sdef c1'

      LetFunC f params locals c1 c2 -> do
        -- TODO: If we extend the environment here with the function arguments,
        -- change lookupVar and (comment of) lookupEnv.
        c1' <- computeCard c1
        c2' <- extendCardCFunEnv f (compInfo c1') $ computeCard c2
        return $ cLetFunC loc (compInfo c2') f params locals c1' c2'

      Call f es -> do
        card <- lookupFun f
        es'  <- mapM computeCallArgCard es
        return $ cCall loc card f es'

      Emit a e ->
        return $ cEmit loc (mkSimplCard 0 1) a e

      Emits a e ->
        case ctExp e of
          TArray (Literal n) _ -> return $ cEmits loc (mkSimplCard 0 n) a e
          _                    -> error "Type checker bug!"

      Return a b fi e ->
        return $ cReturn loc (mkSimplCard 0 0) a b fi e

      Interleave c1 c2 -> do
        c1' <- computeCard c1
        c2' <- computeCard c2
        -- Can do better? Not sure we care
        return $ cInterleave loc mkDynamicCard c1' c2'

      Branch e c1 c2 -> do
        c1' <- computeCard c1
        c2' <- computeCard c2
        let card1 = compInfo c1'
            card2 = compInfo c2'
        let card  = if card1 `cardEqual` card2 then card1 else mkDynamicCard
        return $ cBranch loc card e c1' c2'

      Take1 a b ->
        return $ cTake1 loc (mkSimplCard 1 0) a b

      Take a b n ->
        return $ cTake loc (mkSimplCard n 0) a b n

      Until e c1 -> do
        -- Don't know how many times to iterate
        c1' <- computeCard c1
        let card = mkIterCard Nothing (compInfo c1')
        return $ cUntil loc card e c1'

      While e c1 -> do
        -- Don't know how many times to iterate
        c1' <- computeCard c1
        let card = mkIterCard Nothing (compInfo c1')
        return $ cWhile loc card e c1'

      Times ui e elen x c1 -> do
        c1' <- computeCard c1
        let card = mkIterCard (getInt_maybe $ unExp e) (compInfo c1')
        return $ cTimes loc card  ui e elen x c1'

      Repeat hint c1 -> do
        c1' <- computeCard c1
        let card = mkIterCard Nothing (compInfo c1')
        return $ cRepeat loc card hint c1'

      VectComp hint c1 -> do
        c1' <- computeCard c1
        -- It's ok to make a dynamic cardinality as we will not use this information directly
        return $ cVectComp loc mkDynamicCard  hint c1'

      Map w nm ->
        return $ cMap loc (mkIterCard Nothing $ mkSimplCard 1 1) w nm

      Filter e ->
        return $ cFilter loc (mkIterCard Nothing $ mkDynamicCard) e

      ReadSrc mty ->
        return $ cReadSrc loc (IterCard Nothing (mkSimplCard 0 1)) mty

      WriteSnk mty ->
        return $ cWriteSnk loc (IterCard Nothing (mkSimplCard 1 0)) mty

      ReadInternal a s tp ->
        return $ cReadInternal loc (IterCard Nothing (mkSimplCard 0 1)) a s tp

      WriteInternal a s ->
        return $ cWriteInternal loc (IterCard Nothing (mkSimplCard 1 0)) a s

      Standalone c1 -> do
        c1' <- computeCard c1
        return $ cStandalone loc (compInfo c1') c1'

      Mitigate t n1 n2 ->
        return $ cMitigate loc (mkIterCard Nothing $ mkDynamicCard) t n1 n2

    computeCallArgCard :: CallArg Exp Comp -> CardM (CallArg Exp LComp)
    computeCallArgCard (CAExp  e) = return $ CAExp e
    computeCallArgCard (CAComp c) = CAComp <$> computeCard c

    getInt_maybe :: Exp0 -> Maybe Int
    getInt_maybe (EVal _ (VInt n)) = Just $ fromIntegral n
    getInt_maybe _                 = Nothing
