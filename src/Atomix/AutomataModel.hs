module AutomataModel where

import Data.Loc

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph  ( Node )
import qualified Data.Graph.Inductive.Graph  as G
import Data.Graph.Inductive.PatriciaTree as G

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

--import AtomComp -- simplified Ziria model

import qualified GenSym as GS
import AstExpr       -- BasicTypes/
import AstComp       -- BasicTypes/
import AstUnlabelled
import Opts

--type Node = Int --defined in FGL
type LNode = G.LNode NodeLabel
type Chan = EId
type ChanEnv = [(EId,Chan)]
type FuncEnv = [()]

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

  | Loop { loop_times :: Maybe Int }

  | Branch { branch_in   :: Set Chan
           , branch_code :: Exp
           , branch_env  :: ChanEnv
           , branch_next :: (Node,Node)
           }

data Automaton
  = Automaton { automaton_init  :: Node
              , automaton_final :: Set Node }

type ZirGraph = Gr NodeLabel ()

data ZirEnv = ZirEnv { chan_binds  :: ChanEnv
                     , chan_gensym :: GS.Sym
                     }

type GraphM a = StateT ZirGraph (ReaderT ZirEnv IO) a


mkNode :: NodeKind -> GraphM LNode
mkNode kind = do
  g <- get
  let [node] = G.newNodes 1 g
  let lnode = (node, NodeLabel node kind)
  put (G.insNode lnode g)
  return lnode

mkDummyNode :: GraphM LNode
mkDummyNode = mkNode $ Action Set.empty Set.empty (eint32 0) []

mkDummyAutomaton :: GraphM Automaton
mkDummyAutomaton = do
  (node,_) <- mkDummyNode
  return $ Automaton node (Set.singleton node)

singletonAutomaton :: Node -> Automaton
singletonAutomaton node = Automaton node (Set.singleton node)

extendChan :: (EId,Chan) -> GraphM a -> GraphM a
extendChan (x,c) = local add_bnd
  where add_bnd (ZirEnv binds sym) = ZirEnv ((x,c):binds) sym

--lookupChan :: EId -> GraphM Chan
--lookupChan x = do
--   env <- ask
--   case lookup x (chan_binds env) of
--      Nothing   -> fail ("Automata generation: unbound variable " ++ (show x))
--      Just chan -> return chan

-- TODO: use proper exception
lookupChan :: ZirEnv -> EId -> Chan
lookupChan env x =
   case lookup x (chan_binds env) of
      Nothing   -> assert False undefined
      Just chan -> chan

freshChan :: EId -> GraphM Chan
freshChan x = do
  u <- liftIO . GS.genSymStr =<< asks chan_gensym
  return $ x { uniqId = MkUniq u }

runGraphM :: GraphM a -> IO (a,ZirGraph)
runGraphM m = do
   new_sym <- GS.initGenSym "automata"
   runReaderT (runStateT m (G.empty)) (ZirEnv [] new_sym)

{-------------------- Translation --------------------}

concatAutomata :: Automaton -> Automaton -> GraphM Automaton
concatAutomata a1 a2 = do
    let edges = map (\x -> (x, automaton_init a2, ())) (Set.toList $ automaton_final a1)
    modify (G.insEdges edges)
    return $ Automaton (automaton_init a1) (automaton_final a2)

data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }

mkAutomaton :: DynFlags -> Channels -> Comp -> GraphM Automaton
mkAutomaton dfs chans comp = go chans (unComp comp)
  where
    loc = compLoc comp
    go :: Channels -> Comp0 -> GraphM Automaton

    go chans (BindMany c1 []) = mkAutomaton dfs chans c1

    go chans (BindMany c1 ((x1,c2):cs)) = do
      ctrl <- freshChan x1
      a1 <- mkAutomaton dfs (chans { ctrl_chan = Just ctrl }) c1
      a2 <- extendChan (x1,ctrl) $
            go chans (BindMany c2 cs)
      concatAutomata a1 a2

    go chans (Seq c1 c2) = do
      a1 <- mkAutomaton dfs (chans { ctrl_chan = Nothing }) c1
      a2 <- mkAutomaton dfs chans c2
      concatAutomata a1 a2

    go chans (Emit e) = do
      env <- ask
      let inp = Set.map (lookupChan env) (exprFVs' False e)
      let outp = Set.singleton (out_chan chans)
      (node,_) <- mkNode (Action inp outp e (chan_binds env))
      return $ singletonAutomaton node

    go chans (Return _ e) = do
      env <- ask
      let inp = Set.map (lookupChan env) (exprFVs' False e)
      let outp = case ctrl_chan chans of
                  Just chan -> Set.singleton chan
                  Nothing   -> Set.empty
      (node,_) <- mkNode (Action inp outp e (chan_binds env))
      return $ singletonAutomaton node

    go chans (AstComp.Branch e c1 c2) = do
      a1 <- mkAutomaton dfs chans c1
      a2 <- mkAutomaton dfs chans c2
      let branch = (automaton_init a1, automaton_init a2)
      env <- ask 
      let inp = Set.map (lookupChan env) (exprFVs' False e)
      (node,_) <- mkNode (AutomataModel.Branch inp e (chan_binds env) branch)
      return $ Automaton node (Set.union (automaton_final a1) (automaton_final a2))

    go chans (Take1 _) = do
      let in_c = in_chan chans
      ctrl_c <- case ctrl_chan chans of
                    Just c   -> return c
                    Nothing -> fail "mkAutomaton: input expression not well-typed"
      let inp = Set.singleton in_c
      let outp = Set.singleton ctrl_c
      let code = eAssign noLoc (eVar noLoc ctrl_c) (eVar noLoc in_c)
      let env = [(in_c, in_c), (ctrl_c, ctrl_c)]
      (node,_) <- mkNode (Action inp outp code env)
      return $ singletonAutomaton node

    go chans (Repeat _ c) = do
      a <- mkAutomaton dfs (chans { ctrl_chan = Nothing }) c
       --TODO: how many times should we loop?
      (loop,_) <- mkNode (Loop Nothing)
      let edges = map (\f -> (f,loop,())) $ Set.toList (automaton_final a)
      modify $ G.insEdges edges
      modify $ G.insEdge (loop,automaton_init a,())
      return $ a { automaton_final = Set.empty }

    go chans _ = assert False undefined -- TBD

    --go chans (Map _ f) = do
    --  let inc = in_chan chans
    --  let outc = out_chan chans
    --  let env = [(inc,inc), [outc,outc]]
    --  let inp = Set.singleton inc
    --  let outp = Set.singleton outc
    --  let code = 
    --  (node,_) <- mkNode (Action inp out code env)

    --go chans (Times _ e1 e2 )

    --go chans (While e c) = do
    --  a <- mkAutomaton dfs chans e
    --  done <- mkDummyAutomaton
    --  loop <- mkNode (Loop Nothing)

