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
{-# LANGUAGE ScopedTypeVariables #-}
 
module CardinalityAnalysis ( 

    runCardinalityAnalysis
  , Card ( .. )
  , isSimplCard
  , mkSumCard
  , mkSimplCard
  , mkIterCard
  , mkParCard
  , mkDynamicCard 
) where


import AstExpr
import AstComp
import PpComp
import qualified GenSym as GS

import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State

import Data.List as M


data CEnvDom = CDomVar String  -- A (computation) variable
             | CDomFun String  -- A (computation) function
  deriving Eq

type CEnv = [(CEnvDom,Card)]
data CardM a = CardM (CEnv -> IO a)


cardMIO :: IO a -> CardM a 
cardMIO m = CardM (\_ -> m)


data Card 
  =  -- Simple cardinality
     SimplCard (Maybe Int) (Maybe Int)
 
  | -- Iterated cardinality, such as the one arising from until / repeat
    -- We might actually statically know the iteration count, 
    -- hence a Maybe Int   
    IterCard (Maybe Int) Card  
     
    -- Sum of all cardinalities                           
  | SumCard [Card] 

    -- Underlying cardinalities for (>>>) composition 
  | ParCard Card Card

    -- Dynamic cardinality, can't tell anything
  | DynamicCard  

  deriving ( Show )

-- Smart constructors: think later if we need more functionality 

                             
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

mkInSimplCard :: Int -> Card 
-- Definitely known input cardinality, unknown output cardinality
mkInSimplCard i = SimplCard (Just i) Nothing

mkOutSimplCard :: Int -> Card
-- Definitely known output cardinality, unknown input cardinality
mkOutSimplCard j = SimplCard Nothing (Just j)

mkIterCard :: Maybe Int -> Card -> Card
mkIterCard (Just 1) c = c
-- No takes, just convert this to a simpl-card and unknown emits:
mkIterCard Nothing (SimplCard (Just 0) other) = SimplCard (Just 0) Nothing 
-- No emits, just convert this to a simpl-card and unknown takes
mkIterCard Nothing (SimplCard other (Just 0)) = SimplCard Nothing (Just 0)
-- Fall back to just IterCard
mkIterCard m c = IterCard m c

mkParCard :: Card -> Card -> Card
mkParCard c1 c2 
  = ParCard c1 c2 

mkDynamicCard :: Card
mkDynamicCard = DynamicCard

instance Functor CardM where
  fmap f (CardM c) = CardM $ \env -> fmap f (c env)

instance Applicative CardM where
  pure = CardM . const . pure
  (CardM f) <*> (CardM x) = CardM $ \env -> f env <*> x env

instance Monad CardM where
  (>>=) m1 m2 =
    CardM $ \env ->
      do { res <- runCardM env m1
         ; runCardM env (m2 res) }
  return x = CardM $ \_env -> return x


-- | Auxiliary functions
extendCardCVarEnv :: Name -> Card -> CardM a -> CardM a
extendCardCVarEnv nm c (CardM action) =
  CardM (\env -> action ((CDomVar $ name nm,c):env))

extendCardCFunEnv :: Name -> Card -> CardM a -> CardM a
extendCardCFunEnv nm c (CardM action) =
  CardM (\env -> action ((CDomFun $ name nm,c):env))

getCardEnv :: CardM CEnv
getCardEnv = CardM (\env -> return env)


runCardM :: CEnv -> CardM a -> IO a
runCardM init_env (CardM f) = f init_env


-- Compute cardinality of a type checked computation
computeCardTop :: Bool -> Comp CTy Ty -> CardM (Comp (CTy,Card) Ty)
computeCardTop verbose ct = computeCard ct
  where 
    computeCallArgCard (CAExp e)  
      = return (CAExp e)
    computeCallArgCard (CAComp c) 
      = do { c' <- computeCard c
           ; return (CAComp c') 
           }
    computeCard c = computeCard0 (unComp c)
      where
        cty = compInfo c
        loc = compLoc c 
        computeCard0 :: Comp0 CTy Ty -> CardM (Comp (CTy,Card) Ty)
        computeCard0  (Var x) = 
          do { cenv <- getCardEnv
             ; case M.lookup (CDomVar $ name x) cenv of
                 Just card -> return $ MkComp (Var x) loc (cty,card)
                 Nothing   -> return $ MkComp (Var x) loc (cty,mkDynamicCard) } -- Or fail?

        computeCard0  (BindMany c1 xs_cs) 
          = do { c1' <- computeCard c1
               ; let xs = map fst xs_cs
               ; cs' <- mapM (\(_,c) -> computeCard c) xs_cs
               ; let cards = map (snd . compInfo) (c1':cs')
                     card = mkSumCard cards
               ; return $ MkComp (mkBindMany c1' (zip xs cs')) loc (cty,card)
               }
 
        computeCard0 (Seq c1 c2)
          = do { c1' <- computeCard c1
               ; c2' <- computeCard c2
               ; let card = mkSumCard [snd (compInfo c1'),snd (compInfo c2')]
               ; return $ MkComp (Seq c1' c2') loc (cty,card) }

        computeCard0  (Par p c1 c2)
          = do { c1' <- computeCard c1
               ; c2' <- computeCard c2
               ; let card = mkParCard (snd (compInfo c1')) (snd (compInfo c2'))
               ; return $ cPar loc (cty,card) p c1' c2' }

        computeCard0  (Let x c1 c2)
          = do { c1' <- computeCard c1
               ; c2' <- extendCardCVarEnv x (snd $ compInfo c1') $ 
                        computeCard c2 
               ; return $ 
                 MkComp (Let x c1' c2') loc (cty, snd $ compInfo c2')
               }

        computeCard0  (LetE x fi e c1) 
          = do { c1' <- computeCard c1
               ; return $ 
                 MkComp (LetE x fi e c1') loc (cty, snd $ compInfo c1') }

        -- CL
        computeCard0  (LetERef x y c1) 
          = do { c1' <- computeCard c1
               ; return $ 
                 MkComp (LetERef x y c1') loc (cty, snd $ compInfo c1') }
        
        computeCard0  (LetHeader x fn c1)
          = do { c1' <- computeCard c1
               ; return $ MkComp (LetHeader x fn c1') loc (cty, snd $ compInfo c1') }
        --

        computeCard0  (LetStruct sdef c1) 
          = do { c1' <- computeCard c1
               ; return $ MkComp (LetStruct sdef c1') loc (cty, snd $ compInfo c1') 
               }

        computeCard0  (LetFunC f params locals c1 c2)
          = do { c1' <- computeCard c1
               ; c2' <- extendCardCFunEnv f (snd $ compInfo c1') $ computeCard c2 
               ; return $ MkComp (LetFunC f params locals c1' c2') loc (cty, snd $ compInfo c2')
               }

        computeCard0  (Call f es) 
{-  
          = do { es' <- mapM computeCallArgCard es
               ; return $ MkComp (Call f es') loc (cty, mkDynamicCard)  -- Unknown, for now
               }
-}
          =  do { cenv <- getCardEnv
                ; es' <- mapM computeCallArgCard es
                ; case M.lookup (CDomFun $ name f) cenv of
                   Just card -> return $ MkComp (Call f es') loc (cty,card)
                   Nothing   -> return $ MkComp (Call f es') loc (cty,mkDynamicCard) -- Or fail? 
                }


        computeCard0  (Emit e)
          = return $ (MkComp (Emit e) loc (cty, mkSimplCard 0 1))


        computeCard0  (Emits e)
          = case (info e) of 
              TArr (Literal n) _ -> 
                return $ MkComp (Emits e) loc (cty, mkSimplCard 0 n)
              _ -> error "Type checker bug!" 

        computeCard0  (Return fi e)
          = return $ MkComp (Return fi e) loc (cty, mkSimplCard 0 0)

        computeCard0  (Interleave c1 c2)
          = do { c1' <- computeCard c1
               ; c2' <- computeCard c2
               ; return $ MkComp (Interleave c1' c2') loc (cty, mkDynamicCard) }
               -- Can do better? Not sure we care

        computeCard0  (Branch e c1 c2)   
          = do { c1' <- computeCard c1
               ; c2' <- computeCard c2
               ; let card1 = snd $ compInfo c1'
                     card2 = snd $ compInfo c2'
               ; let card = if card1 `cardEqual` card2 
                            then card1 else mkDynamicCard
               ; return $ MkComp (Branch e c1' c2') loc (cty, card)
               }

        computeCard0  Take1 
          = return $ MkComp Take1 loc (cty, mkSimplCard 1 0)

        computeCard0 (Take e)
          = let n = getInt (unExp e)
            in return $ MkComp (Take e) loc (cty, mkSimplCard n 0)
          where getInt (EVal (VInt n)) = fromIntegral n
                getInt _ = error "getInt: can't happen!"

          -- Take is supposed to work on constants only apparently. See TcComp.

        computeCard0 (Until e c1)
          = do { c1' <- computeCard c1
               ; let card = mkIterCard Nothing (snd $ compInfo c1')
               ; return $ MkComp (Until e c1') loc (cty,card) } -- Don't know how many times to iterate

        computeCard0 (While e c1)
          = do { c1' <- computeCard c1
               ; let card = mkIterCard Nothing (snd $ compInfo c1')
               ; return $ MkComp (While e c1') loc (cty,card) } -- Don't know how many times to iterate

        computeCard0 (Times ui e elen x c1) 
          = do { c1' <- computeCard c1
               ; let card1 = snd $ compInfo c1'
                     card  = mkIterCard (getInt_maybe $ unExp e) card1 
               ; return $ MkComp (Times ui e elen x c1') loc (cty,card) }
          where getInt_maybe (EVal (VInt n)) = Just $ fromIntegral n
                getInt_maybe _ = Nothing

        computeCard0 (Repeat hint c1) 
          = do { c1' <- computeCard c1
               ; let card1 = snd $ compInfo c1'
                     card  = mkIterCard Nothing card1
               ; return $ MkComp (Repeat hint c1') loc (cty,card) }

        computeCard0 (VectComp hint c1)
          = do { c1' <- computeCard c1
               ; return $ MkComp (VectComp hint c1') loc (cty, mkDynamicCard)
                 -- It's ok to make a dynamic cardinality as we will not use this information directly
               }

        computeCard0 (Map w nm)
          = return $ MkComp (Map w nm) loc (cty, mkIterCard Nothing $ mkSimplCard 1 1)

        computeCard0 (Filter e)
          = return $ MkComp (Filter e) loc (cty, mkIterCard Nothing $ mkDynamicCard)

        computeCard0 (ReadSrc mty)
          = return $ 
            MkComp (ReadSrc mty) loc (cty,IterCard Nothing (mkSimplCard 0 1))
        computeCard0 (WriteSnk mty)
          = return $ 
            MkComp (WriteSnk mty) loc (cty,IterCard Nothing (mkSimplCard 1 0))

        computeCard0 (ReadInternal s tp)
          = return $ 
            MkComp (ReadInternal s tp) loc (cty,IterCard Nothing (mkSimplCard 0 1))
        computeCard0 (WriteInternal s)
          = return $ 
            MkComp (WriteInternal s) loc (cty,IterCard Nothing (mkSimplCard 1 0))

        computeCard0 (ActivateTask t mn)
          = taskControlCardBug

        computeCard0 (DeactivateSelf)
          = taskControlCardBug

        computeCard0 (Standalone c1)
          = computeCard c1
 
        computeCard0 (Mitigate t n1 n2)
          = return $ MkComp (Mitigate t n1 n2) loc (cty, mkIterCard Nothing $ mkDynamicCard)

taskControlCardBug :: a
taskControlCardBug =
  error "BUG: cardinality analysis hit a task start/stop statement!"

runCardinalityAnalysis :: Bool -> Comp CTy Ty -> IO (Comp (CTy,Card) Ty)
runCardinalityAnalysis verbose comp = 
   runCardM [] (computeCardTop verbose comp)




