module AtomComp 
  (module AtomComp
  ,Uniq
  ,MutKind(..)
  ,Ty
  ,GName
  ,CompLoc
  ,ParInfo) where

import AstExpr (Ty,GName,MutKind,Uniq,GFun0,GFun)
import qualified AstExpr as AstE
import AstComp (CompLoc,ParInfo)
import qualified AstComp as AstC
import qualified GenSym as GS
import Control.Monad.State

type Fun = GFun Ty
type Fun0 = GFun0 Ty
type Var = GName Ty

type FunName = Uniq
type VarName = Uniq

data Exp b
  = MkExp { unExp   :: !(Exp0 b)
          , expLoc  :: !(CompLoc)
          , expInfo :: b }

data Comp a b
  = MkComp { unComp   :: !(Comp0 a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data Exp0 b
  = ExpApp { expAppFun :: FunName, expAppArgs :: [(VarName,MutKind)] } -- pass by name or reference
  | ExpVar VarName

data Comp0 a b
  = Take1 Ty
  | TakeN Ty Int
  | Emit1 VarName
  | EmitN VarName
  | Return (Exp b)

  | NewVar Var (Comp a b) -- if immutable, can be initialized exactly once
  | Bind VarName (Comp a b)

  | Seq (Comp a b) (Comp a b)
  | Par ParInfo (Comp a b) (Comp a b)

  | Branch VarName (Comp a b) (Comp a b)

  | RepeatN Int (Comp a b)
  | Repeat (Comp a b)

  | While VarName (Comp a b)
  | Until VarName (Comp a b)

  ----------------------------------------------
  -- | Standalone (Comp a b)
  -- | Mitigate String  -- just for debugging
  --         Ty Int Int



data CompEnv a = CompEnv { fun_binds  :: [(FunName,Fun a)]
                         , funGenSym :: GS.Sym
                         , var_binds  :: [(VarName,Var)]
                         , varGenSym :: GS.Sym
                         }

type CompM a = StateT (CompEnv a) IO

mkCompOfAst :: AstC.GComp tc t a b -> CompM a (Comp a b)
mkCompOfAst c = fail "not implemented"

runCompM :: CompM a b -> IO (b, CompEnv a)
runCompM m = do
  fungen <- GS.initGenSym "f"
  vargen <- GS.initGenSym "x"
  let env = CompEnv [] fungen [] vargen
  runStateT m env
  