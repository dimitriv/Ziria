module AutomataModel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List

--import qualified Data.Graph.Inductive.Graph  as G
--import Data.Graph.Inductive.PatriciaTree as G

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
  | Loop { loop_body :: nid } -- Infinite loop. Only transformers may (and must!) contain one of these.
  | Done

  -- TODO: think about this later
  -- | StaticLoop  { iterations :: Int, loop_body :: Automaton }

data Node atom nid
  = Node { node_id   :: nid
         , node_kind :: NodeKind atom nid
         }

type NodeMap atom nid = Map nid (Node atom nid)

data Automaton atom nid
  = Automaton { auto_graph   :: NodeMap atom nid
              , auto_inchan  :: Chan
              , auto_outchan :: Chan
              , auto_start   :: nid
              }

data WiredAtom atom
  = WiredAtom { wires_in  :: [Var]
              , wires_out :: [Var]
              , the_atom  :: atom
              }



class Atom a where

  atomInTy  :: a -> [Ty]
  atomOutTy :: a -> [Ty]

  -- Constructors of atoms
  idAtom      :: Ty -> a
  discardAtom :: Ty -> a
  noOpAtom    :: a

  -- Getting (wired) atoms from expressions
  expToWiredAtom :: Exp b -> Maybe Var -> WiredAtom a





nextNid :: Automaton atom Int -> Int
nextNid a = max+1
  where (max,_) = Map.findMax (auto_graph a)

insert_prepend :: NodeKind atom Int -> Automaton atom Int -> Automaton atom Int
insert_prepend nkind a =
  a { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)
            , auto_start = nid }
  where nid = nextNid a

nodeKindOfId :: Ord nid => nid -> Automaton atom nid -> NodeKind atom nid
nodeKindOfId nid a = node_kind $ fromJust $ Map.lookup nid (auto_graph a)


data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }



mkAutomaton :: Atom e
            => DynFlags
            -> Channels  -- i/o/ctl channel
            -> Comp a b
            -> Automaton e Int -- what to do next (continuation)
            -> CompM a (Automaton e Int)
mkAutomaton dfs chans comp k = go (unComp comp)
  where
    go (Take1 t) =
      let inp = [in_chan chans]
          outp = maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom t) (\_ -> idAtom t) (ctrl_chan chans)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in return $ insert_prepend nkind k

    go (TakeN _ n) = fail "not implemented"

    go (Emit1 x) =
      let inp = [x]
          outp = [out_chan chans]
          atom = idAtom (nameTyp x)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in return $ insert_prepend nkind k

    go (EmitN x) = fail "not implemented"

    go (Return e) =
      let watom = expToWiredAtom e (ctrl_chan chans)
          nkind = Action [watom] (auto_start k)
      in return $ insert_prepend nkind k

    go (NewVar x_spec c) = mkAutomaton dfs chans c k -- NOP for now

    go (Bind mbx c1 c2) = do
      a2 <- mkAutomaton dfs chans c2 k
      mkAutomaton dfs (chans { ctrl_chan = mbx }) c1 a2

    go (Par _ c1 c2) = do
      -- create new middle channel
      let middle = undefined
      let k' = k { auto_graph = Map.singleton 0 (Node 0 Done), auto_start = 0 }
      let k1 = k' { auto_outchan = middle }
      let k2 = k' { auto_inchan = middle }
      a1 <- mkAutomaton dfs chans c1 k1
      a2 <- mkAutomaton dfs chans c2 k2
      zipAutomata a1 a2 k

    go (AtomComp.Branch x c1 c2) = do
      a1 <- mkAutomaton dfs chans c1 k
      a2 <- mkAutomaton dfs chans c2 k
      let nkind = AutomataModel.Branch x (auto_start a1) (auto_start a2) False
      return $ insert_prepend nkind k

    go (RepeatN n c) = applyN n (mkAutomaton dfs chans c) k
      where applyN 0 f x = return x
            applyN n f x = do 
              y <- applyN (n-1) f x
              f y

    go (Repeat c) =
      case nodeKindOfId (auto_start k) k of
        Done -> do
          a <- mkAutomaton dfs chans c k
          let nid = auto_start k
          let nkind = Loop (auto_start a)
          return $ a { auto_start = nid, auto_graph = Map.insert nid (Node nid nkind) (auto_graph a) }
        _ -> fail "Repeat should not have a continuation!"

    go (While x c) = do
      let k' = insert_prepend Done k
      let nid = auto_start k'
      a <- mkAutomaton dfs chans c k'
      let nkind = AutomataModel.Branch x (auto_start a) (auto_start k) True
      return $ a { auto_start = nid, auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)}

    go (Until x c) = do
      let k' = insert_prepend Done k
      let nid = auto_start k'
      a <- mkAutomaton dfs chans c k'
      let nkind = AutomataModel.Branch x (auto_start a) (auto_start k) True
      return $ a { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)}



-- Mapping Automata Labels

map_ids :: Ord nid1 => Ord nid2 => (nid1 -> nid2) -> Automaton e nid1 -> Automaton e nid2
map_ids map_id a = a { auto_graph = new_graph, auto_start = new_start }
 where
    new_start = map_id (auto_start a)
    new_graph =  Map.mapKeys map_id $ Map.map map_node $ auto_graph a
    map_node (Node nid nkind) = Node (map_id nid) (map_nkind nkind)
    map_nkind Done =  Done
    map_nkind (Loop bodyId) = Loop (map_id bodyId)
    map_nkind (Action atoms nextId) = Action atoms (map_id nextId)
    map_nkind (AutomataModel.Branch x left right is_while) = 
      AutomataModel.Branch x (map_id left) (map_id right) is_while

tuple_ids :: Automaton e Int -> Automaton e (Int,Int)
tuple_ids = map_ids (\nid -> (nid,0))

untuple_ids :: Automaton e ((Int,Int),(Int,Int)) -> Automaton e Int
untuple_ids a = map_ids (\nid -> fromJust $ Map.lookup nid untuple_map) a
  where
    (_, untuple_map) = Map.foldWithKey f (0, Map.empty) (auto_graph a)
    f nid _ (counter, nid_map) = (counter+1, Map.insert nid counter nid_map)



-- marking automata nodes

type MarkingM nid = State (Set nid)
mark :: Ord nid => nid ->  MarkingM nid ()
mark nid = do modify (Set.insert nid)

isMarked :: Ord nid => nid -> MarkingM nid Bool
isMarked nid = do
  marks <- get
  return (Set.member nid marks)



-- fusing actions sequences in automata

fuseActions :: Ord nid => Automaton atom nid -> Automaton atom nid
fuseActions auto = auto { auto_graph = fused_graph }
  where
    fused_graph = fst $ runState (markAndFuse (auto_start auto) (auto_graph auto)) Set.empty

    markAndFuse :: Ord nid => nid -> NodeMap atom nid -> MarkingM nid (NodeMap atom nid)
    markAndFuse nid nmap = do
      marked <- isMarked nid
      if marked then return nmap else do
        mark nid
        fuse (fromJust $ Map.lookup nid nmap) nmap

    fuse :: Ord nid => Node atom nid -> NodeMap atom nid -> MarkingM nid (NodeMap atom nid)
    fuse (Node _ Done) nmap = return nmap
    fuse (Node _ (Loop b)) nmap = markAndFuse b nmap
    fuse (Node _ (AutomataModel.Branch _ b1 b2 _)) nmap = do
      nmap <- markAndFuse b1 nmap
      markAndFuse b2 nmap
    fuse (Node nid (Action atoms next)) nmap = do
      nmap <- markAndFuse next nmap
      case fromJust (Map.lookup next nmap) of
        Node _ Done -> return nmap
        Node _ (Loop _) -> return nmap
        Node _ (AutomataModel.Branch {}) -> return nmap
        Node nid' (Action atoms' next') ->
          let new_node = Node nid (Action (atoms++atoms') next')
              new_nmap = Map.insert nid new_node (Map.delete nid' nmap)
          in return new_nmap



-- Zipping Automata

zipAutomata :: Automaton e Int -> Automaton e Int -> Automaton e Int -> Automaton e Int
zipAutomata a1 a2 k = untuple_ids $ prod_a 
  where
    prod_a = Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) prod_start
    prod_start = (auto_start a1', auto_start a2')
    a1' = tuple_ids (fuseActions a1)
    a2' = tuple_ids (fuseActions a2)
    nmap1 = auto_graph a1'
    nmap2 = auto_graph a2'
    prod_nmap = go (auto_start a1') (auto_start a2') Map.empty

    go nid1 nid2 prod_nmap = 
      case Map.lookup (nid1,nid2) prod_nmap of
        Nothing -> go' (fromJust $ Map.lookup nid1 nmap1) (fromJust $ Map.lookup nid2 nmap2) prod_nmap
        Just _ -> prod_nmap -- TODO: what should we do here?

    go' (Node id1 Done) (Node id2 _) prod_nmap =
      let prod_nid = (id1,id2)
      in Map.insert prod_nid (Node prod_nid Done)

    go' (Node id1 _) (Node id2 Done) prod_nmap =
      let prod_nid = (id1,id2)
      in Map.insert prod_nid (Node prod_nid Done)

    go' (Node id1 (AutomataModel.Branch x l r w)) (Node id2 _) prod_nmap =
      let prod_nid = (id1,id2)
          prod_nkind = AutomataModel.Branch x (l,id2) (r,id2) w
          prod_node = Node prod_nid prod_nkind
      in go r id2 $ go l id2 $ Map.insert prod_nid prod_node prod_nmap

    go' (Node id1 _) (Node id2 (AutomataModel.Branch x l r w)) prod_nmap =
      let prod_nid = (id1,id2)
          prod_nkind = AutomataModel.Branch x (id1,l) (id1,r) w
          prod_node = Node prod_nid prod_nkind
      in go id1 r $ go id1 l $ Map.insert prod_nid prod_node prod_nmap

    go' (Node id1 (Action watoms1 next1)) (Node id2 (Action watoms2 next2)) =
      let out_card1 = count_writes watoms1 (auto_outchan a1)
          in_card2 = count_reades watoms2 (auto_inchan a2)
          card = min out_card1 in_card2
          split watoms1 card (auto_outchan a1)




type MarkingM nid = State (Set nid)
mark :: Ord nid => nid ->  MarkingM nid ()
mark nid = do modify (Set.insert nid)

isMarked :: Ord nid => nid -> MarkingM nid Bool
isMarked nid = do
  marks <- get
  return (Set.member nid marks)


data NodeKind atom nid
  = Action { atoms :: [WiredAtom atom]
           , next  :: nid
           }
  | Branch { branch_var   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
           , branch_true  :: nid
           , branch_false :: nid
           , is_while     :: Bool -- Is this a while loop?
           }
  | Loop { loop_body :: nid } -- Infinite loop. Only transformers may (and must!) contain one of these.
  | Done

instance Show (NodeKind atom nid) where
  show (NodeLabel _ (Action watom)) =
    let show_wires = List.intercalate "," . map show
    in show_wires (wires_in watom) ++ "/" ++ show_wires (wires_out watom)
  show (NodeLabel _ (AutomataModel.Branch win _)) = show win ++ "/DQ"
  show (NodeLabel _ (StaticLoop n _)) = "/DQ="

