module AutomataModel where

import Data.Set (Set)
import Data.Map 
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


data NodeKind atom nid 
  = Action { atoms :: [WiredAtom atom]
           , next  :: nid 
           }
  | Branch { branch_var   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
           , branch_true  :: nid
           , branch_false :: nid
           , is_while     :: Bool -- Is this a while loop? 
           }
  | Done

  -- TODO: think about this later 
  -- | StaticLoop  { iterations :: Int, loop_body :: Automaton }

data Node atom nid
  = Node { node_id   :: nid
         , node_kind :: NodeKind atom
         }

data Automaton atom nid 
  = Automaton { auto_graph   :: Map nid (Node atom nid)
              , auto_inchan  :: Chan
              , auto_outchan :: Chan
              , auto_start   :: nid
              }

data WiredAtom atom 
  = WiredAtom { wires_in  :: [Var]
              , wires_out :: [Var] 
              , the_atom  :: atom 
              }

type AutoM a = ReaderT (GS.Sym) (IO a)

type NodeId = Uniq

newNodeId :: AutoM Uniq
newNodeId = do 
  sym <- ask
  liftIO $ genSymStr sym


singletonAutomaton :: Node atom nid -> Automaton atom nid 



class Atom a where
  
  atomInTy  :: a -> [Ty]
  atomOutTy :: a -> [Ty] 

  -- Constructors of atoms  
  idAtom      :: Ty -> a
  discardAtom :: Ty -> a
  noOpAtom    :: a
  
  -- Getting (wired) atoms from expressions 
  expToWiredAtom :: Exp b -> Maybe Var -> WiredAtom a


**************** Yikes, use a state monad instead


fuseActions :: Automaton e NodeId -> Automaton e NodeId 
fuseActions auto = 
  let nodes_map = auto_graph auto
      inchan    = auto_inchan auto
      outchan   = auto_outchan auto

  go :: Node atom NodeId -> Automaton atom NodeId
  go (Node nid (Branch chan nidl nidr w)) = 
      let aleft  = go (fromJust (Map.lookup nidl nodes_map))
          aright = go (fromJust (Map.lookup nidr nodes_map))
      in insNode (Node nid (Branch chan nidl nidr w)) $ 
         Automaton { auto_graph  = Map.union (auto_graph aleft) (auto_graph aright)
                   , auto_start  = nid
                   , auto_inchan = inchan
                   , auto_outchan = outchan }
  go (Node nid (Action wireds next)) = 
      let anext = go (fromJust (Map.lookup next nodes_map))
      in case (node_kind $ fromJust (Map.lookup next (auto_graph anext))) of
           Action more_wireds next' -> 
             Automaton { auto_graph  = Map.union (auto_graph aleft) (auto_graph aright)
                   , auto_start  = nid
                   , auto_inchan = inchan
                   , auto_outchan = outchan }


  

  go (Node nid (Action wireds next)) head_nid prev_wireds
     | Just node_next <- Map.lookup next nodes_map
     = case node_kind node_next of 
         Action {} -> go node_next head_nid (prev_wireds ++ wireds)
         Branch {} -> 

 Node _ (Action {}) <- node_next
     = go node_next head_nid (prev_wireds ++ wireds)

     




mkAutomaton :: Atom e 
            => DynFlags 
            -> Channels  -- i/o/ctl channel
            -> Automaton e NodeId -- what to do next
            -> Comp a b 
            -> AutoM (Automaton e NodeId)
mkAutomaton dfs chans k comp = go (unComp comp)
  where
    go (Bind mbx c1 c2) = do
      a2 <- mkAutomaton dfs chans k c2
      mkAutomaton dfs (chans { ctrl_chan = mbx }) a2 c1

    go (Take1 t) =
      let inp = [in_chan chans]
          outp = maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom t) (\_ -> idAtom t) (ctrl_chan chans)
          nkind = Action [WiredAtom inp oup atom] (auto_start k)
      nid <- newNodeId 
      return $ insNode (Node nid nkind) k { auto_start = nid }



insNode :: Node atom NodeId 
        -> Automaton atom NodeId
        -> Automaton atom NodeId
insNode (Node nid nkind) a
  = a { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a) }

-- type LNode atom = G.LNode (NodeLabel atom)

-- data NodeLabel atom
--   = NodeLabel { node_id   :: Node
--               , node_kind :: NodeKind atom
--               }

-- data NodeKind atom
--   = Action (WiredAtom atom) 
--   | StaticLoop  { iterations :: Int, loop_body :: Automaton }
--   | Branch { branch_in   :: Chan, branch_next :: (Node,Node) }

-- data Automaton
--   = Automaton { autom_init  :: Node
--               , autom_final :: Set Node }
-- type ZirGraph atom = Gr (NodeLabel atom) ()
-- type GraphM atom a = StateT (ZirGraph atom) (ReaderT (CompEnv ()) IO) a




mkNode :: Automaton atom nid      
       -> NodeKind atom nid
       -> (nid, Automaton atom nid)
-- The continuation(s) of this node are already in the automaton
mkNode 


inAutomaton :: nid -> Automaton atom nid -> Bool

nodeIsMarked :: nid -> Auto Bool
markNode :: nid -> Auto ()




nidSeenAlready :: Automaton atom nid -> nid -> Auto Bool











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
          atom = maybe (discardAtom t) (\_ -> idAtom t) (ctrl_chan chans)
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

    go chans (Bind mbx c1 c2) = do
      a1 <- mkAutomaton dfs (chans { ctrl_chan = mbx }) c1
      a2 <- mkAutomaton dfs chans c2
      concatAutomata a1 a2


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

    go chans (Until x c) = do
      c_autom <- mkAutomaton dfs chans c
      (final,_) <- mkNoOpNode
      (while,_) <- mkNode (AutomataModel.Branch x (autom_init c_autom, final))
      modify $ G.insEdges $ map (\f -> (f,while, ())) $ Set.toList $ autom_final c_autom
      modify $ G.insEdge (while, autom_init c_autom, ())
      modify $ G.insEdge (while, final, ())
      return $ Automaton (autom_init c_autom) (Set.singleton final)



instance Show (NodeLabel atom) where
  show (NodeLabel _ (Action watom)) =
    let show_wires = List.intercalate "," . map show
    in show_wires (wires_in watom) ++ "/" ++ show_wires (wires_out watom)
  show (NodeLabel _ (AutomataModel.Branch win _)) = show win ++ "/DQ"
  show (NodeLabel _ (StaticLoop n _)) = "/DQ="
    
