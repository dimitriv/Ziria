module AtomInstantiation where

import AtomComp -- simplified Ziria model
import AutomataModel
import AstExpr (Ty (..), EId, GName (..), GArgTy (..), ArgTy)

import Data.Maybe



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






