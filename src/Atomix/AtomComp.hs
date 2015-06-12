module AtomComp 
  (module AtomComp
  ,Uniq
  ,MutKind
  ,Ty
  ,GName
  ,CompLoc
  ,ParInfo) where

import AstExpr (Ty,GName,MutKind,Uniq)
import AstComp (CompLoc,ParInfo)

type NameSpec = GName Ty

data Exp b
  = MkExp { unExp   :: !(Exp0 b)
          , expLoc  :: !(CompLoc)
          , expInfo :: b }

data Comp a b
  = MkComp { unComp   :: !(Comp0 a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data Exp0 b
  = ExpFun { expFunNm :: Uniq, expFunArgs :: [(Uniq,MutKind)] } -- pass by name or reference
  | ExpVar Uniq

data Comp0 a b
  = Take1 Ty
  | TakeN Ty Int
  | Emit1 Uniq
  | EmitN Uniq
  | Return (Exp b)

  | NewName NameSpec (Comp a b) -- if immutable, can be initialized exactly once
  | Bind Uniq (Comp a b)

  | Seq (Comp a b) (Comp a b)
  | Par ParInfo (Comp a b) (Comp a b)

  | Branch Uniq (Comp a b) (Comp a b)

  | RepeatN Int (Comp a b)
  | Repeat (Comp a b)

  | While Uniq (Comp a b)
  | Until Uniq (Comp a b)

  ----------------------------------------------
  -- | Standalone (Comp a b)
  -- | Mitigate String  -- just for debugging
  --         Ty Int Int