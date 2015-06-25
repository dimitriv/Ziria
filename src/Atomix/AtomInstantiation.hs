module AtomInstantiation where

import AtomComp -- simplified Ziria model
import AutomataModel
import AstExpr (GArgTy (..), ArgTy, EId)
import Data.Loc

import Data.Maybe
import qualified Data.List as List




data FunLikeAtom = FunFun FunName | Id Ty | Discard Ty

instance Atom FunLikeAtom where
  atomInTy (FunFun f) = inTysOfFunction f
  atomInTy (Id t) = [t]
  atomInTy (Discard t) = [t]

  atomOutTy (FunFun f) = outTysOfFunction f
  atomOutTy (Id t) = [t]
  atomOutTy (Discard t) = []

  idAtom = Id
  discardAtom = Discard

  expToWiredAtom = funAppToWiredAtom


atomStuffOfFunction :: EId -> ([Ty],[Ty])
atomStuffOfFunction f =
  let TArrow argtys resty = nameTyp f
      intys  = map (\(GArgTy t _) -> t) argtys
      outtys = concat $ map (\(GArgTy t k) -> if k == Mut then [t] else []) argtys
  in (intys,resty:outtys)

argTysOfFunction :: EId -> [ArgTy]
argTysOfFunction f =
  let TArrow argtys _ = nameTyp f
  in argtys

inTysOfFunction  :: EId -> [Ty]
inTysOfFunction f = fst (atomStuffOfFunction f)

outTysOfFunction :: EId -> [Ty]
outTysOfFunction f = snd (atomStuffOfFunction f)


funAppToWiredAtom :: Exp b -> Maybe Var -> WiredAtom FunLikeAtom
funAppToWiredAtom e mb = WiredAtom ins outs (FunFun f)
  where
    (ExpApp f args) = unExp e
    ins  = args
    outs = maybeToList mb ++ (
             concat $
             zipWith (\(GArgTy _ m) arg -> if m == Mut then [arg] else [])
              (argTysOfFunction f) args
             )




-- Given an atom instantiation, we can now build toy Ziria programs and translate them to Automata
-- For simplicity, we will ignore types for now
ty = TBit

mkComp c = MkComp c noLoc ()
mkExp e = MkExp e noLoc ()

mkApp func args = mkExp $ ExpApp { expAppFun = func, expAppArgs = args}

mkFunc name nins nouts = freshFun name argtys ty
  where
    argtys = [ GArgTy { argty_ty = ty, argty_mut = Imm } | _ <- [1..nins] ] ++
             [ GArgTy { argty_ty = ty, argty_mut = Mut} | _ <- [2..nouts] ]

mkPar c1 c2 = mkComp (Par undefined c1 c2)

mkBind mbvar c1 c2 = mkComp $ Bind mbvar c1 c2
mkRepeat c = mkComp (Repeat c)
mkReturn e = mkComp (Return e)

freshMap name = do
  x <- freshVar ("arg(" ++ name ++ ")") ty Imm
  f <- mkFunc name 1 1
  let app = mkApp f [x]
  return $ mkRepeat $ mkBind (Just x) (mkComp $ Take1 ty) (mkReturn app)

mkRepeatN n c = mkComp $ RepeatN n c




-- Mockup of Wifi Pipeline

mkWifi :: CompM () (Comp () ())
mkWifi = do

  -- channels for control data
  det <- freshVar "det" ty Imm
  params <- freshVar "params" ty Imm
  h <- freshVar "h" ty Imm

  -- DetectSTS
  removeDC <- freshMap "removeDC"
  cca <- do
    x <- freshVar "cca_cond" ty Mut
    let body = mkComp (Emit1 x)
    return $ mkComp $ While x body
  let detectSTS = mkPar removeDC cca

  -- LTS
  lts <- do
    x <- freshVar "lts_cond" ty Mut
    let body = mkRepeatN 4 $ mkComp (Emit1 x)
    return $ mkComp $ Until x body

  -- middle pipeline
  dataSymbol <- freshMap "dataSymbol"
  fft <- freshMap "fft"
  channelEqualization <- freshMap "channelEqualization"
  pilotTrack <- freshMap "pilotTrack"
  getData <- freshMap "getData"
  let middlePipeline = List.foldr mkPar getData [dataSymbol, fft, channelEqualization, pilotTrack]

  -- DecodePLCP
  demodBPSK <- freshMap "demodBPSK"
  deinterleave <- freshMap "deinterleaveA"
  decode <- freshMap "decodeA"
  parseHeader <- do
    x <- freshVar "parseHeader_cond" ty Mut
    let body = mkRepeatN 3 $ mkComp (Emit1 x)
    return $ mkComp $ Until x body
  let decodePLCP = List.foldr mkPar parseHeader [middlePipeline, demodBPSK, deinterleave, decode]

  -- Decode
  demod <- freshMap "demod"
  deinterleave <- freshMap "deinterleaveB"
  decode <- freshMap "decodeB"
  descramble <- do
    x <- freshVar "descramble_cond" ty Mut
    let body = mkRepeatN 7 $ mkComp (Emit1 x)
    return $ mkComp $ Until x body
  let decodePipeline = List.foldr mkPar descramble [middlePipeline, demod, deinterleave, decode]

  -- overall receiver
  let mkBind (mbvar, pipeline) acc = mkComp $ Bind mbvar pipeline acc

  return $ List.foldr mkBind decodePipeline [ (Just det, detectSTS)
                                            , (Just params, lts)
                                            , (Just h, decodePLCP) ]




