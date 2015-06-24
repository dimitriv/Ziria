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
      return $ zipAutomata a1 a2 k

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

map_node_ids :: Ord nid1 => Ord nid2 => (nid1 -> nid2) -> Node e nid1 -> Node e nid2
map_node_ids map_id (Node nid nkind) = Node (map_id nid) (map_nkind nkind)
  where
    map_nkind Done =  Done
    map_nkind (Loop bodyId) = Loop (map_id bodyId)
    map_nkind (Action atoms nextId) = Action atoms (map_id nextId)
    map_nkind (AutomataModel.Branch x left right is_while) = 
      AutomataModel.Branch x (map_id left) (map_id right) is_while

map_auto_ids :: Ord nid1 => Ord nid2 => (nid1 -> nid2) -> Automaton e nid1 -> Automaton e nid2
map_auto_ids map_id a = a { auto_graph = new_graph, auto_start = new_start }
 where
    new_start = map_id (auto_start a)
    new_graph = Map.mapKeys map_id $ Map.map (map_node_ids map_id) $ auto_graph a
    
tuple_ids :: Automaton e Int -> Automaton e (Int,Int)
tuple_ids = map_auto_ids (\nid -> (nid,0))

untuple_ids :: Automaton e ((Int,Int),(Int,Int)) -> Automaton e Int
untuple_ids a = map_auto_ids (\nid -> fromJust $ Map.lookup nid untuple_map) a
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



-- fuses actions sequences in automata; inserts Loop nodes to make self-loops explicit

fuseActions :: Automaton atom Int -> Automaton atom Int
fuseActions auto = auto { auto_graph = fused_graph }
  where
    fused_graph = fst $ runState (markAndFuse (auto_start auto) (auto_graph auto)) Set.empty

    markAndFuse :: Int -> NodeMap atom Int -> MarkingM Int (NodeMap atom Int)
    markAndFuse nid nmap = do
      marked <- isMarked nid
      if marked then return nmap else do
        mark nid
        fuse (fromJust $ Map.lookup nid nmap) nmap

    fuse :: Node atom Int -> NodeMap atom Int -> MarkingM Int (NodeMap atom Int)
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
        Node nid' (Action atoms' next') 
          | nid == nid' -> -- self loop detected! Insert loop.
            let new_next_nid = nextNid auto
                new_next_node = Node new_next_nid (Loop nid)
                new_action_node = Node nid (Action atoms new_next_nid)
                new_nmap = Map.insert nid new_action_node $ Map.insert new_next_nid new_next_node nmap
            in return new_nmap
          | otherwise ->
            let new_node = Node nid (Action (atoms++atoms') next')
                new_nmap = Map.insert nid new_node $ Map.delete nid' nmap
            in return new_nmap



-- Zipping Automata

-- Precondition: a1 and a2 should satisfy (auto_outchan a1) == (auto_inchan a2)
zipAutomata :: Automaton e Int -> Automaton e Int -> Automaton e Int -> Automaton e Int
zipAutomata a1 a2 k = untuple_ids $ prod_a 
  where
    prod_a = Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) (s1,s2)
    s1 = (auto_start a1, 0)
    s2 = (auto_start a2, 0)
    prod_nmap = go s1 s2 Map.empty
    trans_ch = auto_outchan a1

    lookup :: (Int,Int) -> Automaton e Int -> Node e (Int,Int)
    lookup nid@(n_baseid,n_offset) a = 
      case fromJust $ Map.lookup n_baseid (auto_graph a) of
        Node _ (Action watoms next) -> 
          if n_offset >= length watoms
            then assert False undefined
            else Node nid $ Action (drop n_offset watoms) (next,0)
        node@(Node _ _) -> 
          if n_offset > 0 
            then assert False undefined
            else map_node_ids (\id -> (id,0)) node


    -- go :: (Int,Int) -> (Int,Int) -> NodeMap e ((Int,Int),(Int,Int)) -> NodeMap e ((Int,Int),(Int,Int))
    go nid1 nid2 prod_nmap = 
      case Map.lookup (nid1,nid2) prod_nmap of
        Nothing -> go' (lookup nid1 a1) (lookup nid2 a2) prod_nmap
        Just _ -> prod_nmap -- TODO: what should we do here?

    -- go' :: Node e (Int,Int) -> Node e (Int,Int) -> NodeMap e ((Int,Int),(Int,Int)) -> NodeMap e ((Int,Int),(Int,Int))
    go' (Node id1 Done) (Node id2 _) prod_nmap =
      let prod_nid = (id1,id2)
      in Map.insert prod_nid (Node prod_nid Done) prod_nmap

    go' (Node id1 _) (Node id2 Done) prod_nmap =
      let prod_nid = (id1,id2)
      in Map.insert prod_nid (Node prod_nid Done) prod_nmap

    go' (Node id1 (Loop next1)) (Node id2 _) prod_nmap =
      go next1 id2 prod_nmap

    go' (Node id1 _) (Node id2 (Loop next2)) prod_nmap =
      go id1 next2 prod_nmap

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

    go' (Node id1 (Action watoms1 next1)) (Node id2 (Action watoms2 next2)) prod_nmap =
      let out_card1 = count_writes watoms1 trans_ch
          in_card2 = count_reads watoms2 trans_ch
          card = min out_card1 in_card2
          (watoms1',next1') = split card (\wa -> if elem trans_ch (wires_out wa) then 1 else 0) id1 (watoms1, next1)
          (watoms2',next2') = split card (\wa -> if elem trans_ch (wires_in wa) then 1 else 0) id2 (watoms2, next2)
          prod_nid = (id1,id2)
          prod_nkind = Action (watoms1'++watoms2') (next1',next2')
          prod_node = Node prod_nid prod_nkind
      in go next1' next2' $ Map.insert prod_nid prod_node prod_nmap

    count :: [a] -> (a -> Bool) -> Int
    count xs f = List.sum $ List.map (\x -> if f x then 1 else 0) xs

    count_writes :: [WiredAtom atom] -> Chan -> Int
    count_writes watoms ch = count watoms (\wa -> List.elem ch $ wires_out wa)

    count_reads :: [WiredAtom atom] -> Chan -> Int
    count_reads watoms ch = count watoms (\wa -> List.elem ch $ wires_in wa)

    split :: Int -> (WiredAtom atom -> Int) -> (Int,Int) -> ([WiredAtom atom], (Int,Int)) -> ([WiredAtom atom], (Int,Int))
    split budget cost (id_base,id_offset) (watoms,next) = split' budget watoms 0 []
      where 
        split' budget [] idx acc = (List.reverse acc,next)
        split' budget (wa:watoms) idx acc =
          let budget' = budget - cost wa 
          in if budget' >= 0 then split' budget' watoms (idx+1) (wa:acc)
             else (List.reverse acc, (id_base, id_offset + idx))



--instance Show (NodeKind atom nid) where
--  show (NodeLabel _ (Action watom)) =
--    let show_wires = List.intercalate "," . map show
--    in show_wires (wires_in watom) ++ "/" ++ show_wires (wires_out watom)
--  show (NodeLabel _ (AutomataModel.Branch win _)) = show win ++ "/DQ"
--  show (NodeLabel _ (StaticLoop n _)) = "/DQ="

