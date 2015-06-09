module AutomataModel where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph  ( Node )
import qualified Data.Graph.Inductive.Graph  as G 

import Data.Graph.Inductive.PatriciaTree
import Control.Exception

import Control.Monad.Reader
import Control.Monad.State 

import qualified GenSym as GS


import AstExpr       -- BasicTypes/ 
import AstComp       -- BasicTypes/
import AstUnlabelled 


type Chan = EId
type ChanEnv = [(EId,Chan)]

data NodeLabel 
  = NodeLabel { node_id   :: Node
              , node_kind :: NodeKind
              }

data NodeKind
  = Action { action_in   :: Set Chan
           , action_out  :: Set Chan
           , action_code :: Exp
           , action_env  :: ChanEnv
           }

  | Loop { action_times :: Maybe Int }

  | Branch { branch_in   :: Set Chan
           , branch_code :: Exp
           , branch_env  :: ChanEnv
           , branch_next :: (Node,Node)
           }

data Automaton
  = Automaton { automaton_init  :: Node
              , automaton_final :: Set Node }

newtype ZirGraph = ZirGraph { graph :: Gr NodeLabel () }

data ZirEnv = ZirEnv { chan_binds  :: ChanEnv
                     , chan_gensym :: GS.Sym 
                     }

type GraphM a = StateT ZirGraph (ReaderT ZirEnv IO) a

extendChan :: (EId,Chan) -> GraphM a -> GraphM a
extendChan (x,c) = local add_bnd
  where add_bnd (ZirEnv binds sym) = ZirEnv ((x,c):binds) sym

lookupChan :: EId -> GraphM Chan
lookupChan x = do 
   env <- ask
   case lookup x (chan_binds env) of
      Nothing   -> fail ("Automata generation: unbound variable " ++ (show x))
      Just chan -> return chan


freshChan :: EId -> GraphM Chan
freshChan x = do 
  u <- liftIO . GS.genSymStr =<< asks chan_gensym
  return $ x { uniqId = MkUniq u }

runGraphM :: GraphM a -> IO (a,ZirGraph)
runGraphM m = do
   new_sym <- GS.initGenSym "automata"
   runReaderT (runStateT m (ZirGraph G.empty)) (ZirEnv [] new_sym)


{-------------------- Translation --------------------}

-- runStateT (ZirGraph G.empty) (runReaderT m [])
-- type GraphM expr var a = Env var Uniq -> Graph expr var -> (a, Graph expr var)

-- extend :: (var,Uniq) -> GraphM expr var a -> GraphM expr var a 
-- lookup :: var -> GraphM expr var Uniq

-- put :: ..
-- get :: ..


-- runGraphM :: GraphM Expr EId a -> (Graph expr var, a




-- translate :: Comp -> GraphM Expr EId (Auto


-- translate (Seq c1 (x,c2)) = do
--    a1 <- translate c1 
--    a2 <- extend x (translate c2)
--    return (merge_automata a1 a2)
-- }

-- translate (If e c1 c2) = do 
--    a1 <- translate c1
--    a2 <- translate c2
--    let vsins = varsOf e
--    c <- mkControl vsins e (start_node a1) (start_node a2)
   
--    return 

  




-- mkAction :: Set var -> Set var -> expr -> GraphM expr var Node


-- mkAction :: Graph expr var -> Set var -> Set var -> expr -> (Node, Graph expr var)

-- mkControl :: Graph expr var -> Set var -> expr -> Node -> Node -> (Node, Graph expr var)
