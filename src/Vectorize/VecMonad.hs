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
module VecMonad where
 
import AstExpr
import AstComp
import PpComp
import qualified GenSym as GS

import Text.Parsec.Pos

import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State

import Data.List as M


import CardinalityAnalysis




-- 
-- Work-in-progress vectorization plan
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
data VecState = VecState { vs_take_count :: Int
                         , vs_emit_count :: Int }

-- Computation (variables or functions) bindings
-- TODO: Push these to the Comp AST at some point, 
type CVarBind = Comp (CTy,Card) Ty
data CFunBind = CFunBind { cfun_params :: [(Name,CallArg Ty CTy0)]
                         , cfun_locals :: [(Name,Ty,Maybe (Exp Ty))] 
                         , cfun_body   :: Comp (CTy,Card) Ty }

data VecEnv 
  = VecEnv { cvar_binds :: [(Name,CVarBind)]
           , cfun_binds :: [(Name,CFunBind)] }

data VecM a = VecM { runVecM :: GS.Sym 
                             -> VecEnv 
                             -> VecState 
                             -> IO (a, VecState) }

instance Functor VecM where
  fmap f (VecM x) = VecM $ \sym env st -> do
    (x', st') <- x sym env st
    return (f x', st')

instance Applicative VecM where
  pure x = VecM $ \sym env st -> return (x,st)
  (VecM f) <*> (VecM x) = VecM $ \sym env st -> do
    (f', st') <- f sym env st
    (x', st'') <- x sym env st'
    return (f' x', st'')

instance Monad VecM where
  (>>=) m1 m2 =
    VecM $ \sym env st ->
      do (res,st') <- runVecM m1 sym env st
         runVecM (m2 res) sym env st'
  return x = pure x

newVectUniq :: VecM Int
newVectUniq = VecM $ \sym _env st -> do { i <- GS.genSym sym
                                        ; return (i,st) }

newVectName :: String -> Maybe SourcePos -> VecM Name
newVectName nm loc
  = do { u <- newVectUniq
       ; return $ 
         (toName (nm ++ "_" ++ show u) loc Nothing) {uniqId = "_v" ++ show u} 
       }


extendCVarBind :: Name -> Comp (CTy,Card) Ty -> VecM a -> VecM a
extendCVarBind nm comp (VecM action)
  = VecM (\sym env st -> 
      let env' = env { cvar_binds = (nm,comp):(cvar_binds env) }
      in action sym env' st)

extendCFunBind :: Name -> [(Name,CallArg Ty CTy0)] 
                       -> [(Name,Ty,Maybe (Exp Ty))] 
                       -> (Comp (CTy,Card) Ty) -> VecM a -> VecM a
extendCFunBind nm params locals cbody (VecM action)
  = VecM (\sym env st -> 
      let entry = (nm, CFunBind params locals cbody)
          env' = env { cfun_binds = entry:(cfun_binds env) }
      in action sym env' st)

getVecEnv :: VecM (GS.Sym, VecEnv)
getVecEnv = VecM (\sym env _st -> return ((sym,env),_st))

getCVarBinds :: VecM [(Name,CVarBind)]
getCVarBinds = VecM (\sym env st -> return (cvar_binds env, st)) 



getCFunBinds :: VecM [(Name,CFunBind)]
getCFunBinds = VecM (\sym env st -> return (cfun_binds env, st)) 

lookupCVarBind :: Name -> VecM CVarBind
lookupCVarBind nm = do { bnds <- getCVarBinds
                       ; case lookup nm bnds of 
                            Just bnd -> return bnd
                            Nothing -> error "Unbound cvar bind!" 
                       }
lookupCFunBind :: Name -> VecM CFunBind
lookupCFunBind nm = do { bnds <- getCFunBinds
                       ; case lookup nm bnds of 
                            Just bnd -> return bnd
                            Nothing -> error "Unbound cfun bind!" 
                       }


 
incTakeCount :: VecM ()
incTakeCount = VecM (\_ _env st -> 
   let st' = st { vs_take_count = (vs_take_count st) + 1 }
   in return ((),st'))

withCurrentCounters :: VecM a -> VecM (a,VecState)
-- Run the action in the current state, but in the end return 
-- the final state. The VecM state remains as it was in the beginning.
withCurrentCounters (VecM action)
  = VecM (\sym env st -> do { (a,st') <- action sym env st
                            ; return ((a,st'),st) })

setVecState :: VecState -> VecM ()
setVecState st = VecM (\sym env _st -> return ((),st))


getTakeCount :: VecM Int
getTakeCount = VecM (\_ _env st -> return (vs_take_count st, st))

incEmitCount :: VecM ()
incEmitCount = VecM (\_ _env st -> 
   let st' = st { vs_emit_count = vs_emit_count st + 1 }
   in return ((),st'))

getEmitCount :: VecM Int
getEmitCount = VecM (\_ _env st -> return (vs_emit_count st, st))

vecMIO :: IO a -> VecM a
vecMIO action =
  VecM $ \_sym _env st -> action >>= \res -> return (res, st)

vecMFail :: String -> VecM a
vecMFail msg = 
  VecM $ \_ _ st -> 
        do { putStrLn (error msg) 
           ; return (undefined,st) }

withInitCounts :: VecM a -> VecM a
withInitCounts (VecM action)
  = VecM (\sym env st -> 
               do { (res,st') <- action sym env (st { vs_take_count = 0, vs_emit_count = 0 })
                  ; return (res,st) -- Return original state! 
                  })


inCurrentEnv :: (GS.Sym, VecEnv) -> VecM a -> IO a
inCurrentEnv (sym,venv) (VecM action)
  = do { (r,_) <- action sym venv (VecState 0 0)
       ; return r
       }

-- Vectorization result, a quick way to get 
-- the result arity of vectorization w.o. re type checking.
data VectRes
 = NoVect
 | DidVect { in_card   :: Int
           , out_card  :: Int
           , vect_util :: Double -- The utility of this vectorization
           }
 deriving ( Eq, Show, Ord )


-- vecResMatch :: VectRes -> VectRes -> Bool
-- -- The '0' cases have to do with being able to vectorize computers that do not take or 
-- -- do not emit. In this case /any/ vectorization of the input or output queue is valid
-- -- actually.
-- vecResMatch NoVect NoVect = True
-- vecResMatch (DidVect 1 i) NoVect | i <= 1 = True
-- vecResMatch NoVect (DidVect i 1) | i <= 1 = True
-- vecResMatch (DidVect i j) (DidVect i' j') 
--     = arMatch i i' && arMatch j j'
-- vecResMatch _ _ = False

-- arMatch 0 i2 
--   = (i2 `elem` [0,1,2,4,6,8,16])
-- arMatch i1 0
--   = (i1 `elem` [0,1,2,4,6,8,16])
-- arMatch i1 i2 
--   = i1 == i2 && arMatch 0 i1 && arMatch 0 i2


mkNoVect :: Int -> Int -> VectRes 
-- Accepts 'cin' and 'cout' and it is safe and sound to just give back NoVect.
-- After all we performed no vectorization.
-- However, if the input/queue arities are 0 (that is, we never even took or emited)
-- we are in better shape emitting a 'DidVect 0 x' or 'DidVect x 0' so that other components
-- can take advantage of this information. 
-- In the case where we have cin = 0 and cout = n > 0 we say DidVect 0 *1*. 
-- NB: *1* is important, it effectively means: we did not vectorize on the output queue. 

mkNoVect 0 0 = DidVect 0 0 minUtil  
mkNoVect 0 n = DidVect 0 1 minUtil 
mkNoVect n 0 = DidVect 1 0 minUtil 

mkNoVect n m = NoVect


vectResQueueEq :: VectRes -> VectRes -> Bool
vectResQueueEq NoVect NoVect = True
vectResQueueEq (DidVect i j _) (DidVect i' j' _) = i == i' && j == j'
vectResQueueEq _ _ = False

vectResQueueComp (DidVect i j _) (DidVect i' j' _) = compare (i,j) (i',j')
vectResQueueComp v v' = compare v v'


-- Utility functions
vectResUtil (DidVect _ _ u) = u
vectResUtil NoVect          = minUtil

-- Utility computation of a Bind 
chooseBindUtility :: [Double] -> Double
chooseBindUtility [] = 0.0
chooseBindUtility ds = (sum ds :: Double) / (fromIntegral (length ds) :: Double)

-- Utility computation of a Par
chooseParUtility :: Double -> Double -> (Int,Int) -> Double
chooseParUtility d1 d2 (middle1,middle2) = d1 + d2 + util middle1 + util middle2

util :: Int -> Double
util d = if d == 0 then minUtil
         else log (fromIntegral d :: Double)

minUtil = log 0.1 

allVecResMatch :: [VectRes] -> [VectRes]
allVecResMatch vs
  = if all_equal filtered_in && all_equal filtered_out 
    then [DidVect (maximum (0:filtered_in)) (maximum (0:filtered_out)) 
                  (chooseBindUtility (map vectResUtil vs))
         ]
    else []

  where filtered_in = filter (\i -> i > 0) all_in_queues
        filtered_out = filter (\i -> i > 0) all_out_queues
        all_in_queues = map get_in_queue vs 
        all_out_queues = map get_out_queue vs 
        get_in_queue  = fst . get_queue 
        get_out_queue = snd . get_queue 
        get_queue NoVect = (1,1)
        get_queue (DidVect i j _) = (i,j)
        all_equal [] = True
        all_equal [v] = True
        all_equal (v1:v2:vs) = v1 == v2 && all_equal (v2:vs)    



{- Give back a vectorized version of a type -}
mkVectTy :: Ty -> Int -> Ty
mkVectTy ty wdth
  | not (isVectorizable ty)
  = error ("Type not vectorizable:" ++ show ty)
  -- Specialization below implemented in the code generator, not here
  -- | ty == TBit && wdth <= 8 
  -- = TWord8
  -- | ty == TBit && wdth <= 32
  -- = TWord32
  | wdth == 1
  = ty
  | otherwise
  = TArr (Literal wdth) ty


{- When is a type vectorizable -}
isVectorizable :: Ty -> Bool
isVectorizable ty 
 | isScalarTy ty  = True
 | TBit <- ty     = True -- Bits are vectorizable all right
 | otherwise = False 

isBufTy (TBuff {}) = True
isBufTy _          = False


-- Some auxiliary things for creating new expressions in the vectorizer

mkTimes :: Exp b -> Name -> Comp a b -> Comp a b
-- Creates a for loop from 0 up to the size specified
mkTimes elen@(MkExp (EVal (VInt 1)) eloc einfo) nm comp
  = let ezero = MkExp (EVal (VInt 0)) eloc einfo
    in head $ substExpComp (nm, ezero) comp
mkTimes elen nm comp 
  = let ezero = MkExp (EVal (VInt 0)) (compLoc comp) (info elen)
    in MkComp (Times AutoUnroll ezero elen nm comp) (compLoc comp) (compInfo comp)


exp0             = toExp tint (EVal (VInt 0))
exp1             = toExp tint (EVal (VInt 1))
expN n           = toExp tint (EVal (VInt n))
assign e1 e2     = toExp TUnit (EAssign e1 e2)
eif e1 e2 e3     = toExp (info e2) (EIf e1 e2 e3)
efor e1 e2 e3 e4 = toExp (info e4) (EFor AutoUnroll e1 e2 e3 e4)
eeq e1 e2        = toExp TBool (EBinOp Eq e1 e2)
eseq e1 e2       = toExp (info e2) (ESeq e1 e2)
einc e           = assign e (toExp tint (EBinOp Add e (expN 0)))
esub e1 e2       = toExp tint (EBinOp Sub e1 e2)
eadd e1 e2       = toExp tint (EBinOp Add e1 e2)
edec e           = assign e (esub e exp1)
emul e1 e2       = toExp tint (EBinOp Mult e1 e2)


arrRead wdth ty x eix 
  = toExp ty (EArrRead evar eix LISingleton)
  where arrTy = TArr wdth ty
        evar  = toExp arrTy (EVar x)

arrWrite wdth x eix eval 
  = toExp TUnit (EArrWrite evar eix LISingleton eval)
  where arrTy = TArr wdth (info eval) 
        evar  = toExp arrTy (EVar x)

eseqarr (e : [])  = e
eseqarr (e : es') = toExp (info e) (ESeq e (eseqarr es')) 
eseqarr _         = error "Empty seq array!" 


{- VecMBnd
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   A thin wrapper around VecM that provides facilities for binding
   generation and binding collection -}

-- Just the vectorization monad with extra binds
newtype VecMBnd a 
  = VecMBnd { runVecMBnd :: VecM (a,[(Name,Ty, Maybe (Exp ()))]) }

instance Functor VecMBnd where
  fmap f (VecMBnd x) = VecMBnd $ do
    (x', binds) <- x
    return (f x', binds)

instance Applicative VecMBnd where
  pure x = VecMBnd $ return (x,[])
  (VecMBnd f) <*> (VecMBnd x) = VecMBnd $ do
    (f', binds1) <- f
    (x', binds2) <- x
    return (f' x', binds1 ++ binds2)

instance Monad VecMBnd where
  (>>=) (VecMBnd m) f 
     = VecMBnd $ do { (a,binds1) <- m
                    ; case f a of 
                        VecMBnd m_inside -> 
                          do { (b,binds2) <- m_inside
                             ; return (b,binds1++binds2) } }
  return = pure

instance MonadIO VecMBnd where
  liftIO m = VecMBnd $ do { a <- vecMIO m
                          ; return (a,[])
                          }

liftVecM :: VecM a -> VecMBnd a
liftVecM m 
  = VecMBnd $ do { a <- m
                 ; return (a,[]) 
                 }

extendCVarBind' :: Name -> Comp (CTy,Card) Ty -> VecMBnd a -> VecMBnd a
extendCVarBind' nm comp m
  = do { let action = runVecMBnd m
       ; (res,bnds) <- liftVecM $ extendCVarBind nm comp action
       ; throwBnds bnds
       ; return res
       }

extendCFunBind' :: Name -> [(Name,CallArg Ty CTy0)] 
                        -> [(Name,Ty,Maybe (Exp Ty))] 
                        -> (Comp (CTy,Card) Ty) -> VecMBnd a -> VecMBnd a
extendCFunBind' nm params locals cbody m 
  = do { let action = runVecMBnd m
       ; (res,bnds) <- liftVecM $ 
                       extendCFunBind nm params locals cbody action
       ; throwBnds bnds
       ; return res
       }

throwBnds :: [(Name,Ty, Maybe (Exp ()))] -> VecMBnd ()
throwBnds xs = VecMBnd $ return ((),xs)

newTypedName :: String -> Ty -> Maybe SourcePos -> VecMBnd Name
-- Generate new name (but don't declare it)
newTypedName s ty loc 
  = do { x <- liftVecM $ newVectName s loc
       ; return x { mbtype = Just ty }
       }

newDeclTypedName :: String 
                 -> Ty -> Maybe SourcePos 
                 -> Maybe (Exp ()) -> VecMBnd Name 
-- Generate new name (but do declare it)
newDeclTypedName s ty loc me
  = do { nm <- newTypedName s ty loc
       ; throwBnds [(nm,ty, me)]
       ; return nm
       }


