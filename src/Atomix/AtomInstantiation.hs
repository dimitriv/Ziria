module AtomInstantiation where

import AtomComp -- simplified Ziria model
import AutomataModel
import AstExpr (Ty (..), EId, GName (..), GArgTy (..), ArgTy, Uniq (..), MutKind (..))

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

mkComp c = MkComp c undefined ()
mkExp e = MkExp e undefined ()

mkApp func args = mkExp $ ExpApp { expAppFun = func, expAppArgs = args}

mkFunc name nins nouts = MkName { name = name
                                , uniqId = MkUniq name
                                , nameTyp = funty
                                , nameLoc = undefined
                                , nameMut = undefined }
  where
    funty = TArrow argtys ty
    argtys = [ GArgTy { argty_ty = ty, argty_mut = Imm } | _ <- [1..nins] ] ++
             [ GArgTy { argty_ty = ty, argty_mut = Mut} | _ <- [2..nouts] ]

mkPar c1 c2 = mkComp (Par undefined c1 c2)

mkChan :: CompM () Var
mkChan = undefined

ziria_map func = do
  x <- mkChan
  return $ mkComp $ Bind (Just x) (mkComp $ Take1 ty) (mkComp $ Return $ mkApp func [x])






-- Mockup of Wifi Pipeline

wifi :: CompM () (Comp () ())
wifi = do

  -- channels for control data
  det <- mkChan
  params <- mkChan
  h <- mkChan

  -- DetectSTS
  removeDC <- ziria_map (mkFunc "removeDC" 1 1)
  let cca = undefined
  let detectSTS = mkPar removeDC cca

  -- LTS
  let lts = undefined

  -- middle pipeline
  let dataSymbol = undefined
  let fft = undefined
  let channelEqualization = undefined
  let pilotTrack = undefined
  let getData = undefined
  let middlePipeline = List.foldr mkPar getData [dataSymbol, fft, channelEqualization, pilotTrack]

  -- DecodePLCP
  let demodBPSK = undefined
  let deinterleave = undefined
  let decode = undefined
  let parseHeader = undefined
  let decodePLCP = List.foldr mkPar parseHeader [middlePipeline, demodBPSK, deinterleave, decode]

  -- Decode
  let demod = undefined
  let deinterleave = undefined
  let decode = undefined
  let descramble = undefined
  let decodePipeline = List.foldr mkPar descramble [middlePipeline, demod, deinterleave, decode]

  -- overall receiver
  let mkBind (mbvar, pipeline) acc = mkComp $ Bind mbvar pipeline acc

  return $ List.foldr mkBind decodePipeline [ (Just det, detectSTS)
                                            , (Just params, lts)
                                            , (Just h, decodePLCP) ]




