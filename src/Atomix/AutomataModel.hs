module AutomataModel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)
import qualified Data.List as List

import Data.Graph.Inductive.Graph  ( Node )
import qualified Data.Graph.Inductive.Graph  as G
import Data.Graph.Inductive.PatriciaTree as G

import qualified System.IO as IO

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

import AtomComp -- simplified Ziria model
import AstExpr (nameTyp, name)
import Opts


type Chan = Var

type LNode atom = G.LNode (NodeLabel atom)

data NodeLabel atom
  = NodeLabel { node_id   :: Node
              , node_kind :: NodeKind atom
              }

data NodeKind atom
  = Action (WiredAtom atom) 
  | StaticLoop  { iterations :: Int, loop_body :: Automaton }
  | Branch { branch_in   :: Chan, branch_next :: (Node,Node) }

data Automaton
  = Automaton { autom_init  :: Node
              , autom_final :: Set Node }

type ZirGraph atom = Gr (NodeLabel atom) ()

type GraphM atom a = StateT (ZirGraph atom) (ReaderT (CompEnv ()) IO) a

data WiredAtom atom = WiredAtom { wires_in  :: [Var]
                                , wires_out :: [Var] 
                                , the_atom  :: atom }

class Atom a where
  
  atomInTy  :: a -> [Ty]
  atomOutTy :: a -> [Ty] 

  -- Constructors of atoms  
  idAtom      :: Ty -> a
  discardAtom :: Ty -> a
  noOpAtom    :: a
  
  -- Getting (wired) atoms from expressions 
  expToWiredAtom :: Exp b -> Maybe Var -> WiredAtom a


mkNode :: NodeKind code -> GraphM code (LNode code)
mkNode kind = do
  g <- get
  let [node] = G.newNodes 1 g
  let lnode = (node, NodeLabel node kind)
  put (G.insNode lnode g)
  return lnode

mkNoOpNode :: Atom code => GraphM code (LNode code)
mkNoOpNode = mkNode (Action $ WiredAtom [] [] noOpAtom)

singletonAutomaton :: Node -> Automaton
singletonAutomaton node = Automaton node (Set.singleton node)



{-------------------- Translation --------------------}

concatAutomata :: Automaton -> Automaton -> GraphM atom Automaton
concatAutomata a1 a2 = do
    let edges = map (\x -> (x, autom_init a2, ())) (Set.toList $ autom_final a1)
    modify (G.insEdges edges)
    return $ Automaton (autom_init a1) (autom_final a2)

data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }




mkAutomaton :: Atom e => DynFlags -> Channels -> Comp a b -> GraphM e Automaton
mkAutomaton dfs chans comp = go chans (unComp comp)
  where
    go chans (Take1 t) =
      let inp = [in_chan chans]
          outp = maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom t) (\_ -> idAtom t)
      in do
        (node,_) <- mkNode (Action $ WiredAtom inp outp atom)
        return $ singletonAutomaton node

    go chans (TakeN _ n) = fail "not implemented"

    go chans (Emit1 x) = do
      let inp = [x]
          outp = [out_chan chans]
          atom = idAtom (nameTyp x)
      (node,_) <- mkNode (Action $ WiredAtom inp outp atom)
      return $ singletonAutomaton node

    go chans (EmitN x) = fail "not implemented" 

    
    go chans (Return e) = do
      let watom = expToWiredAtom e (ctrl_chan chans)
      (node,_) <- mkNode (Action watom)
      return $ singletonAutomaton node


    go chans (NewVar x_spec c) = mkAutomaton dfs chans c -- NOP for now

    -- go chans (Bind x c) = mkAutomaton dfs (chans { ctrl_chan = Just x }) c

    go chans (Bind mbx c1 c2) = do
      a1 <- mkAutomaton dfs (chans { ctrl_chan = mbx }) c1
      a2 <- mkAutomaton dfs chans c2
      concatAutomata a1 a2

    -- go chans 

    -- go chans (Seq c1 c2) = do
    --   a1 <- mkAutomaton dfs chans c1
    --   a2 <- mkAutomaton dfs chans c2
    --   concatAutomata a1 a2

    go chans (Par _ c1 c2) = fail "not implemented" 

    go chans (AtomComp.Branch x c1 c2) = do
      a1 <- mkAutomaton dfs chans c1
      a2 <- mkAutomaton dfs chans c2
      (node,_) <- mkNode (AutomataModel.Branch x (autom_init a1, autom_init a2))
      let edges = [(node,autom_init a1,()), (node, autom_init a2,())] 
      modify (G.insEdges edges)
      return $ Automaton node (Set.union (autom_final a1) (autom_final a2))


    go chans (RepeatN n c) = do
      c_autom <- mkAutomaton dfs chans c
      (node,_) <- mkNode (StaticLoop n c_autom)
      modify $ G.insEdge (node, autom_init c_autom, ())
      modify $ G.insEdges $ map (\f -> (f, node, ())) $ Set.toList $ autom_final c_autom
      return $ singletonAutomaton node

    go chans (Repeat c) = do
      c_autom <- mkAutomaton dfs chans c
      modify $ G.insEdges $ map (\f -> (f,autom_init c_autom, ())) $ Set.toList $ autom_final c_autom
      return $ c_autom { autom_final = Set.empty }

    go chans (While x c) = do
      c_autom <- mkAutomaton dfs chans c
      (final,_) <- mkNoOpNode
      (while,_) <- mkNode (AutomataModel.Branch x (autom_init c_autom, final))
      modify $ G.insEdges $ map (\f -> (f,while, ())) $ Set.toList $ autom_final c_autom
      modify $ G.insEdge (while, autom_init c_autom, ())
      modify $ G.insEdge (while, final, ())
      return $ Automaton while (Set.singleton final)


    go chans (Until x c) = fail "not implemented"



instance Show (NodeLabel atom) where
  show (NodeLabel _ (Action watom)) = show' (wires_in watom) ++ "/" ++ show' (wires_out watom)
    where 
      show' = List.intercalate "," . map show
  show (NodeLabel _ (AutomataModel.Branch win _)) = show win ++ "/DQ"
  show (NodeLabel _ (StaticLoop n _)) = "/DQ="
    