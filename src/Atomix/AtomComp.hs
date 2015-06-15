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
type FunName = Var

data Exp b
  = MkExp { unExp   :: !(Exp0 b)
          , expLoc  :: !(CompLoc)
          , expInfo :: b }

data Comp a b
  = MkComp { unComp   :: !(Comp0 a b)
           , compLoc  :: !(CompLoc)
           , compInfo :: a }

data Exp0 b
  = ExpApp { expAppFun :: FunName, expAppArgs :: [Var] }





data Comp0 a b
  = Take1 Ty
  | TakeN Ty Int
  | Emit1 Var
  | EmitN Var

  | Return (Exp b)

  | NewVar Var (Comp a b)
  | Bind (Maybe Var) (Comp a b) (Comp a b) 

  | Par ParInfo (Comp a b) (Comp a b)

  | Branch Var (Comp a b) (Comp a b)

  | RepeatN Int (Comp a b)
  | Repeat (Comp a b)

  | While Var (Comp a b)
  | Until Var (Comp a b)

  ----------------------------------------------
  -- | Standalone (Comp a b)
  -- | Mitigate String  -- just for debugging
  --         Ty Int Int

data SymFun = SFFun FunName
            | SFId Ty
            | SFDiscard Ty 


-- Notes. Here we may want to add:
--   SFCompose (SymFun a) (SymFun a) ...
--   SFFunFun  ... some functional representation of actual functions ...

data CompEnv a = CompEnv { fun_binds  :: [(FunName, Fun a)]
                         , funGenSym :: GS.Sym
                         }

type CompM a = StateT (CompEnv a) IO

mkCompOfAst :: AstC.GComp tc t a b -> CompM a (Comp a b)
mkCompOfAst c = fail "not implemented"

runCompM :: CompM a b -> IO (b, CompEnv a)
runCompM m = do
  fungen <- GS.initGenSym "f"
  let env = CompEnv [] fungen
  runStateT m env
  
