module AtomComp where

import AstExpr (GName)
import AstComp (CompLoc,ParInfo)


data Exp t b
  = MkExp { unExp   :: !(Exp0 t b)
          , expLoc  :: !(CompLoc)
          , expInfo :: b }

data Comp tc t a b 
  = MkComp { unComp   :: !(Comp0 tc t a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data Exp0 t b 
  = ExpFun { expFunNm :: GName t, expFunArgs :: [GName t] }
  | ExpVar (GName t)

data Comp0 tc t a b
  = Take1 t
  | TakeN t Int
  | Emit1 (GName t)
  | EmitN (GName t)
  | Return (GName t)

  | Bind (GName t) (Comp tc t a b) (Comp tc t a b)
  | Let  (GName t) (Exp t b) (Comp tc t a b)
  | NewRef (GName t) (Comp tc t a b)

  | Seq (Comp tc t a b) (Comp tc t a b)
  | Par ParInfo (Comp tc t a b) (Comp tc t a b)

  | Branch (GName t) (Comp tc t a b) (Comp tc t a b)

  | RepeatN Int (Comp tc t a b)
  | Repeat (Comp tc t a b)

  | While (GName t) (Comp tc t a b)
  | Until (GName t) (Comp tc t a b)

  ----------------------------------------------
  | Standalone (Comp tc t a b)
  | Mitigate String  -- just for debugging 
           t Int Int