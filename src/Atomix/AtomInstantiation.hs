{-# LANGUAGE TupleSections, ScopedTypeVariables, InstanceSigs #-}
module AtomInstantiation where

import AtomComp -- simplified Ziria model
import AutomataModel
import AstExpr
import Data.Loc
import Control.Exception

import Data.Maybe
import qualified Data.List as List

import Outputable 
import Text.PrettyPrint.HughesPJ


data SymAtom = SAExp (AExp ())
             | SAExpIgnoreRet (AExp ())
             | SACast (Int,Ty) (Int,Ty) 
             | SADiscard (Int,Ty)

instance Outputable SymAtom where
  ppr (SAExp e) = ppr e
  ppr (SAExpIgnoreRet e) = ppr e

  ppr (SACast (n1,t1) (n2,t2)) 
    | t1 == t2  = assert (n1 == n2) (text "ID")
    | otherwise = parens (pprTy n1 t1) <> text "-CAST-" <> parens (pprTy n2 t2)
    where pprTy 1 t = ppr t
          pprTy n t = ppr n <> text "*" <> ppr t

  ppr (SADiscard _) = text "DISCARD"

instance Show SymAtom where
  show sa = render (pprShort sa)

pprShort :: SymAtom -> Doc
pprShort (SAExp e) = ppr (aexp_lbl e)
pprShort (SAExpIgnoreRet e) = ppr (aexp_lbl e)
pprShort sa = ppr sa



{--------------- Instantiation of Atom class -------------}


instance Atom SymAtom where
  atomInTy (SAExp e) = map ((1,) . nameTyp) (aexp_ivs e)
  atomInTy (SAExpIgnoreRet e) = map ((1,) . nameTyp) (aexp_ivs e)
  atomInTy (SACast inty _)  = [inty]
  atomInTy (SADiscard inty) = [inty]

  atomOutTy (SAExp e) = (1, aexp_ret e) : map ((1,) . nameTyp) (aexp_ovs e)
  atomOutTy (SAExpIgnoreRet e) = map ((1,) . nameTyp) (aexp_ovs e)

  atomOutTy (SACast _ outty) = [outty]
  atomOutTy (SADiscard _)    = []

  castAtom    = SACast
  discardAtom = SADiscard

  expToWiredAtom :: AExp () -> Maybe EId -> WiredAtom SymAtom
  expToWiredAtom e mb_out = 
      WiredAtom { wires_in  = map (1,) (aexp_ivs e)
                , wires_out = map (1,) (maybeToList mb_out ++ aexp_ovs e)
                , the_atom  = (if isNothing mb_out then SAExpIgnoreRet else SAExp) e
                }


-- (FunFun f) = map (1,) $ inTysOfFunction f
--   atomInTy (Cast in_ty _) = [in_ty]
--   atomInTy (Discard t) = [(1,t)]

--   atomOutTy (FunFun f) = map (1,) $ outTysOfFunction f
--   atomOutTy (Cast _ out_ty) = [out_ty]
--   atomOutTy (Discard t) = []

--   castAtom = Cast
--   discardAtom = Discard

--   expToWiredAtom = funAppToWiredAtom


-- atomStuffOfFunction :: EId -> ([Ty],[Ty])
-- atomStuffOfFunction f =
--   let TArrow argtys resty = nameTyp f
--       intys  = map (\(GArgTy t _) -> t) argtys
--       outtys = concat $ map (\(GArgTy t k) -> if k == Mut then [t] else []) argtys
--   in (intys,resty:outtys)

-- argTysOfFunction :: EId -> [ArgTy]
-- argTysOfFunction f =
--   let TArrow argtys _ = nameTyp f
--   in argtys

-- inTysOfFunction  :: EId -> [Ty]
-- inTysOfFunction f = fst (atomStuffOfFunction f)

-- outTysOfFunction :: EId -> [Ty]
-- outTysOfFunction f = snd (atomStuffOfFunction f)


-- funAppToWiredAtom :: Exp b -> Maybe Var -> WiredAtom FunLikeAtom
-- funAppToWiredAtom e mb = WiredAtom (map (1,) ins) (map (1,) outs) (FunFun f)
--   where
--     (ExpApp f args) = unExp e
--     ins  = args
--     outs = maybeToList mb ++ (
--              concat $
--              zipWith (\(GArgTy _ m) arg -> if m == Mut then [arg] else [])
--               (argTysOfFunction f) args
--              )




-- -- Given an atom instantiation, we can now build toy Ziria programs and translate them to Automata
-- -- For simplicity, we will ignore types for now
-- ty = TBit

-- mkComp c = MkComp c noLoc ()
-- mkExp e = MkExp e noLoc ()

-- mkApp func args = mkExp $ ExpApp { expAppFun = func, expAppArgs = args}

-- mkFunc name nins nouts = freshFun name argtys ty
--   where
--     argtys = [ GArgTy { argty_ty = ty, argty_mut = Imm } | _ <- [1..nins] ] ++
--              [ GArgTy { argty_ty = ty, argty_mut = Mut} | _ <- [2..nouts] ]

-- mkPar c1 c2 = mkComp (Par undefined c1 c2)

-- mkBind mbvar c1 c2 = mkComp $ Bind mbvar c1 c2
-- mkRepeat c = mkComp (Repeat c)
-- mkReturn f args = mkComp (Return $ mkApp f args)
-- mkEmit x = mkComp (Emit1 x)

-- --freshMap name closure = do
-- --  x <- freshVar ("inp(" ++ name ++ ")") ty Imm
-- --  y <- freshVar ("out(" ++ name ++ ")") ty Imm
-- --  f <- mkFunc name (1 + length closure) 1
-- --  return $ mkRepeat $ mkBind (Just x) (mkComp $ Take1 ty) $
-- --                      mkBind (Just y) (mkReturn f (x:closure)) $
-- --                      mkEmit y

-- freshMap name closure = do
--   f <- mkFunc name (1 + length closure) 1
--   return $ mkRepeat $ mkComp $ MapOnce f closure

-- mkRepeatN n c = mkComp $ RepeatN n c
-- mkTake = mkComp (Take1 ty)
-- mkWhile x b = mkComp $ While x b




-- -- Mockup of Wifi Pipeline

-- mkWifi :: CompM () (Comp () ())
-- mkWifi = do

--   constf <- mkFunc "CONST" 0 1

--   -- channels for control data
--   det <- freshVar "det" ty Imm
--   params <- freshVar "params" ty Imm
--   h <- freshVar "h" ty Imm

--   -- DetectSTS
--   removeDC <- freshMap "removeDC" []
--   cca <- do
--     x <- freshVar "cca_cond" ty Mut
--     let body = mkBind (Just x) mkTake (mkEmit x)
--     return $ mkBind Nothing (mkWhile x body) (mkReturn constf [])
--   let detectSTS = mkPar removeDC cca

--   -- LTS
--   lts <- do
--     x <- freshVar "lts_cond" ty Mut
--     let body = mkRepeatN 2 $ mkBind (Just x) mkTake (mkEmit x)
--     return $ mkComp $ Until x body

--   -- middle pipeline
--   dataSymbol <- freshMap "dataSymbol" [det]
--   fft <- freshMap "fft" []
--   channelEqualization <- freshMap "channelEqualization" [params]
--   pilotTrack <- freshMap "pilotTrack" []
--   getData <- freshMap "getData" []
--   let middlePipeline = List.foldr mkPar getData [dataSymbol, fft, channelEqualization, pilotTrack]

--   -- DecodePLCP
--   demodBPSK <- freshMap "demodBPSK" []
--   deinterleave <- freshMap "deinterleaveA" []
--   decode <- freshMap "decodeA" []
--   parseHeader <- do
--     x <- freshVar "parseHeader_cond" ty Mut
--     y <- freshVar "parseHeader_aux" ty Mut
--     f <- mkFunc "parseH_func" 1 2
--     let body = mkBind (Just y) mkTake $ mkReturn f [y,x]
--     return $ mkComp $ Until x body
--   let decodePLCP = List.foldr mkPar parseHeader [middlePipeline, demodBPSK, deinterleave, decode]

--   -- Decode
--   demod <- freshMap "demod" [h]
--   deinterleave <- freshMap "deinterleaveB" []
--   decode <- freshMap "decodeB" [h]
--   descramble <- do
--     x <- freshVar "descramble_cond" ty Mut
--     y1 <- freshVar "descramble_aux1" ty Mut
--     y2 <- freshVar "descramble_aux2" ty Mut
--     f <- mkFunc "descramble_func" 2 2
--     let body = mkBind (Just y1) mkTake $ mkBind (Just y2) mkTake $ mkReturn f [y1, y2, x]
--     return $ mkComp $ Until x body
--   let decodePipeline = List.foldr mkPar descramble [middlePipeline, demod, deinterleave, decode]

--   -- overall receiver
--   let mkBind (mbvar, pipeline) acc = mkComp $ Bind mbvar pipeline acc

--   return $ List.foldr mkBind decodePipeline [ (Just det, detectSTS)
--                                             , (Just params, lts)
--                                             , (Just h, decodePLCP) ]




