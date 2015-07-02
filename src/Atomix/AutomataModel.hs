{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module AutomataModel where

import Data.Bool
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List
import Data.Loc

import qualified System.IO as IO

import Control.Exception
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.State

import AtomComp
import AstExpr
import Opts
import AtomixCompTransform ( freshName )

import qualified GenSym as GS

type Chan = EId

data NodeKind atom nid
  = Action { action_atoms :: [WiredAtom atom]
           , action_next  :: nid
           }
  | Branch { branch_var   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
           , branch_true  :: nid
           , branch_false :: nid
           , is_while     :: Bool -- Is this a while loop?
           }
  | Loop { loop_body :: nid } -- Infinite loop. Only transformers may (and must!) contain one of these.
  | Done

instance (Atom atom, Show nid) => Show (NodeKind atom nid) where
  show (Action was next) = "Action" ++ show was ++ "->" ++ (show next) ++ ""
  show (AutomataModel.Branch x n1 n2 True) = "While[" ++ show x ++ "]->(" ++ (show n1) ++ "," ++ (show n2) ++ ")"
  show (AutomataModel.Branch x n1 n2 False) = "If[" ++ show x ++ "]->(" ++ (show n1) ++ "," ++ (show n2) ++ ")"
  show (Loop next) = "Loop->" ++ (show next)
  show Done = "Done"

  -- TODO: think about this later
  -- | StaticLoop  { iterations :: Int, loop_body :: Automaton }

data Node atom nid
  = Node { node_id   :: nid
         , node_kind :: NodeKind atom nid
         }

instance (Atom atom, Show nid) => Show (Node atom nid) where
  show (Node nid nk) = "<" ++ (show nid) ++ ":" ++ (show nk) ++ ">"

type NodeMap atom nid = Map nid (Node atom nid)


data Automaton atom nid
  = Automaton { auto_graph   :: NodeMap atom nid
              , auto_inchan  :: Chan
              , auto_outchan :: Chan
              , auto_start   :: nid
              }
  deriving Show


data WiredAtom atom
  = WiredAtom { wires_in  :: [(Int,EId)]
              , wires_out :: [(Int,EId)]
              , the_atom  :: atom
              }

instance Atom a => Show (WiredAtom a) where
  show (WiredAtom inw outw atom) = showWires inw ++ show atom ++ showWires outw
    where
      showWires ws = "{" ++ (List.intercalate "," $ map showWire ws) ++ "}"
      showWire (n,var)
        | n==1      = show var
        | otherwise = show var ++ "^" ++ show n


class Show a => Atom a where

  atomInTy  :: a -> [(Int,Ty)]
  atomOutTy :: a -> [(Int,Ty)]

  -- Constructors of atoms
  discardAtom :: (Int,Ty) -> a
  castAtom    :: (Int,Ty) -> (Int,Ty) -> a

  -- Getting (wired) atoms from expressions
  expToWiredAtom :: AExp () -> Maybe EId -> WiredAtom a

  idAtom      :: Ty -> a
  idAtom t = castAtom (1,t) (1,t)
 

-- 
-- TakeN t N  ~~~>    
--       castAtom (t,N) (arr[N] t, 1)
-- 
-- Emits x    ~~~>    
--       castAtom (arr[N] t,1) (t,N)
--
--   n1 = k*n2
--  ~~~>  castAtom (arr[n1] t, 1) (arr[n2] t, k)
--   n2 = k*n1
--  ~~~> castAtom  (arr[n1] t, k) (arr[n2] t, 1)
--
--
-- 





-- auxilliary functions for automata construction & manipulation
sucs (Node _ Done) = []
sucs (Node _ (Loop nxt)) = [nxt]
sucs (Node _ (Action _ nxt)) = [nxt]
sucs (Node _ (AutomataModel.Branch _ nxt1 nxt2 _)) = [nxt1,nxt2]

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

-- precondition: a1 and a2 must agree on auto_inchan and auto_outchan
concat_auto :: Atom atom => Show nid1 => Ord nid1 => Automaton atom nid1 -> Automaton atom Int -> Automaton atom Int
concat_auto a1 a2 = a1' { auto_graph = concat_graph }
  where
    a1' = replace_done_with (auto_start a2) $ normalize_auto_ids (nextNid a2) a1
    graph1 = Map.delete (auto_start a2) (auto_graph a1')
    graph2 = auto_graph a2
    concat_graph = assert (auto_inchan a1 == auto_inchan a2) $
                   assert (auto_outchan a1 == auto_outchan a2) $
                   assert (Map.null $ Map.intersection graph1 graph2) $
                   Map.union graph1 graph2

mkDoneAutomaton :: Chan -> Chan -> Automaton e Int
mkDoneAutomaton ic oc
  = Automaton { auto_graph = Map.singleton 0 (Node 0 Done), auto_start = 0
              , auto_outchan = oc
              , auto_inchan  = ic
              }


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

-- replaces arbitrary automata node-ids with Ints >= first_id
normalize_auto_ids :: Atom e => Ord nid => Show nid => Int -> Automaton e nid -> Automaton e Int
normalize_auto_ids first_id a = map_auto_ids map_id a
  where
    map_id nid = fromJust $ assert (Map.member nid normalize_map) $ Map.lookup nid normalize_map
    (_, normalize_map) = Map.foldWithKey f (first_id, Map.empty) (auto_graph a)
    f nid _ (counter, nid_map) = (counter+1, Map.insert nid counter nid_map)

replace_done_with :: Ord nid => nid -> Automaton e nid -> Automaton e nid
replace_done_with nid a = map_auto_ids (\nid -> Map.findWithDefault nid nid replace_map) a
  where
    replace_map = Map.fold fold_f Map.empty (auto_graph a)
    fold_f (Node nid' Done) mp = Map.insert nid' nid mp
    fold_f _ mp = mp


-- debugging
auto_closed :: Ord nid => Automaton e nid -> Bool
auto_closed a = Map.foldWithKey node_closed (isDefined $ auto_start a) (auto_graph a)
  where
    isDefined nid = Map.member nid (auto_graph a)
    node_closed nid (Node nid' nkind) = (&&) (nid==nid' && isDefined nid && nkind_closed nkind)
    nkind_closed Done = True
    nkind_closed (Loop nid) = isDefined nid
    nkind_closed (Action _ nid) = isDefined nid
    nkind_closed (AutomataModel.Branch _ nid1 nid2 _) = isDefined nid1 && isDefined nid2

ensure :: (a -> Bool) -> a -> a
ensure f x = assert (f x) x


-- Constructing Automata from Ziria Comps

data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }


mkAutomaton :: Atom e
            => DynFlags
            -> GS.Sym
            -> Channels  -- i/o/ctl channel
            -> AComp a ()
            -> Automaton e Int -- what to do next (continuation)
            -> IO (Automaton e Int)
mkAutomaton dfs sym chans comp k = go (acomp_comp comp)
  where
    loc = acomp_loc comp
    go (ATake1 t) =
      let inp = [(1,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom (1,t)) (\_ -> idAtom t) (ctrl_chan chans)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in return $ insert_prepend nkind k

    go (ATakeN t n) = 
       let inp  = [(n,in_chan chans)]
           outp = map (1,) $ maybeToList (ctrl_chan chans)
           outty = TArray (Literal n) t 
           atom = maybe (discardAtom (n,t)) (\_ -> castAtom (n,t) (1,outty)) (ctrl_chan chans)
           nkind = Action [WiredAtom inp outp atom] (auto_start k)
       in return $ insert_prepend nkind k

    go (AEmit1 x) =
      let inp = [(1, x)]
          outp = [(1, out_chan chans)]
          atom = idAtom (nameTyp x)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in return $ insert_prepend nkind k

    go (AEmitN t n x) =
      let inp = [(1, x)]
          outp = [(n, out_chan chans)]
          atom = castAtom (1, nameTyp x) (n,t)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in return $ insert_prepend nkind k

    go (ACast s (n1,t1) (n2,t2)) =
      let inp  = [(n1, in_chan chans)]
          outp = [(n2, out_chan chans)]
          atom = castAtom (n1,t1) (n2,t2)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in return $ insert_prepend nkind k

 
{- 
    go (MapOnce f closure) =
      let args = in_chan chans : closure
          expr = MkExp (ExpApp f args) noLoc ()
          watom = expToWiredAtom expr (Just $ out_chan chans)
          nkind = Action [watom] (auto_start k)
      in return $ insert_prepend nkind k
-}

    go (AReturn e) =
      let watom = expToWiredAtom e (ctrl_chan chans)
          nkind = Action [watom] (auto_start k)
      in return $ insert_prepend nkind k


    go (ABind mbx c1 c2) = do
      a2 <- mkAutomaton dfs sym chans c2 k
      mkAutomaton dfs sym (chans { ctrl_chan = mbx }) c1 a2

    go (APar _ c1 t c2) = do
      transfer_ch <- freshName sym "transfer" loc t Mut
      let k1 = mkDoneAutomaton (in_chan chans) transfer_ch
      let k2 = mkDoneAutomaton transfer_ch (out_chan chans)
      a1 <- mkAutomaton dfs sym (chans {out_chan = transfer_ch}) c1 k1
      a2 <- mkAutomaton dfs sym (chans {in_chan = transfer_ch}) c2 k2
      return $ zipAutomata a1 a2 k

    go (ABranch x c1 c2) = do
      a1 <- mkAutomaton dfs sym chans c1 k
      a2 <- mkAutomaton dfs sym chans c2 k
      let nkind = AutomataModel.Branch x (auto_start a1) (auto_start a2) False
      return $ insert_prepend nkind k

    go (ARepeatN n c) = applyN n (mkAutomaton dfs sym chans c) k
      where applyN 0 f x = return x
            applyN n f x = do
              y <- applyN (n-1) f x
              f y

    go (ARepeat c) =
      case nodeKindOfId (auto_start k) k of
        Done -> do
          a <- mkAutomaton dfs sym  chans c k
          let nid = auto_start k
          let node = fromJust $ Map.lookup (auto_start a) (auto_graph a) -- Loop (auto_start a)
          let nmap = Map.insert nid node $ Map.delete (auto_start a) (auto_graph a)
          return $ map_auto_ids (\id -> if id == (auto_start a) then nid else id) $ a { auto_start = nid, auto_graph = nmap }
        _ -> fail "Repeat should not have a continuation!"

    go (AWhile x c) = do
      let k' = insert_prepend Done k
      let nid = auto_start k'
      a <- mkAutomaton dfs sym chans c k'
      let nkind = AutomataModel.Branch x (auto_start a) (auto_start k) True
      return $ a { auto_start = nid, auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)}

    go (AUntil x c) = do
      let k' = insert_prepend Done k
      let nid = auto_start k'
      a <- mkAutomaton dfs sym chans c k'
      let nkind = AutomataModel.Branch x (auto_start a) (auto_start k) True
      return $ a { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)}





---- Zipping Automata
-- remaining resources in pipe ("balance"), state of left automaton, state of right automaton
type ProdNid = (Int, (Int,Int), (Int,Int))

-- Precondition: a1 and a2 should satisfy (auto_outchan a1) == (auto_inchan a2)
-- a1 and a2 MUST NOT contain explicit loop nodes (but may contain loops)!!
zipAutomata :: forall e. Atom e => Automaton e Int -> Automaton e Int -> Automaton e Int -> Automaton e Int
zipAutomata a1 a2 k = concat_auto prod_a k
  where
    prod_a = ensure auto_closed      $
             assert (auto_closed a1) $
             assert (auto_closed a2) $
             Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) (0,s1,s2)
    s1 = (auto_start a1, 0)
    s2 = (auto_start a2, 0)
    prod_nmap = zipNodes 0 s1 s2 Map.empty
    trans_ch = assert (auto_outchan a1 == auto_inchan a2) $ auto_outchan a1


    -- this allows us to address nodes in the input automata using (base_id,offset) pairs
    lookup :: (Int,Int) -> Automaton e Int -> Node e (Int,Int)
    lookup nid@(baseId,offset) a =
      case fromJust $ Map.lookup baseId (auto_graph a) of
        Node _ (Action watoms next) ->
          assert (offset < length watoms) $
          Node nid $ Action (drop offset watoms) (next,0)
        node@(Node _ _) ->
          assert (offset == 0) $ map_node_ids (\id -> (id,0)) node


    zipNodes :: Int -> (Int,Int) -> (Int,Int) -> NodeMap e ProdNid -> NodeMap e ProdNid
    zipNodes balance nid1 nid2 prod_nmap =
      case Map.lookup (balance,nid1,nid2) prod_nmap of
        Nothing -> zipNodes' balance (lookup nid1 a1) (lookup nid2 a2) prod_nmap
        Just _ -> prod_nmap -- We have already seen this product location. We're done!

    zipNodes' :: Int -> Node e (Int,Int) -> Node e (Int,Int) -> NodeMap e ProdNid -> NodeMap e ProdNid
    zipNodes' balance (Node id1 Done) (Node id2 _) prod_nmap =
      let prod_nid = (balance,id1,id2)
      in Map.insert prod_nid (Node prod_nid Done) prod_nmap

    zipNodes' balance (Node id1 _) (Node id2 Done) prod_nmap =
      let prod_nid = (balance,id1,id2)
      in Map.insert prod_nid (Node prod_nid Done) prod_nmap

    -- We don't handle eplicit loops since they complicate the algorithm.
    -- The problem is that loop nodes in one of the source automata do NOT
    -- give rise to nodes in the product automaton. As a result,
    -- the algorithm will produce spurious product node ids.
    zipNodes' _ (Node _ (Loop _)) (Node {}) _ =
      assert False undefined
    zipNodes' _ (Node {}) (Node _ (Loop _)) _ =
      assert False undefined

    zipNodes' balance (Node id1 (AutomataModel.Branch x l r w)) (Node id2 _) prod_nmap =
      let prod_nid = (balance,id1,id2)
          prod_nkind = AutomataModel.Branch x (balance,l,id2) (balance,r,id2) w
          prod_node = Node prod_nid prod_nkind
      in zipNodes balance r id2 $ zipNodes balance l id2 $ Map.insert prod_nid prod_node prod_nmap

    zipNodes' balance (Node id1 _) (Node id2 (AutomataModel.Branch x l r w)) prod_nmap =
      let prod_nid = (balance,id1,id2)
          prod_nkind = AutomataModel.Branch x (balance,id1,l) (balance,id1,r) w
          prod_node = Node prod_nid prod_nkind
      in zipNodes balance id1 r $ zipNodes balance id1 l $ Map.insert prod_nid prod_node prod_nmap

    zipNodes' balance n1@(Node id1 (Action _ _)) n2@(Node id2 (Action _ _)) prod_nmap =
      let prod_nid = (balance,id1,id2)
          (watoms, balance', next1, next2) = zipActions balance n1 n2
          prod_nkind = Action watoms (balance',next1,next2)
          prod_node = Node prod_nid prod_nkind
      in zipNodes balance' next1 next2 $ Map.insert prod_nid prod_node prod_nmap

    zipActions :: Int -> Node atom (Int,Int) -> Node atom (Int,Int) -> ([WiredAtom atom], Int, (Int,Int), (Int,Int))
    zipActions balance (Node (base1,offset1) (Action watoms1 next1)) (Node (base2,offset2) (Action watoms2 next2))
      = tickRight [] balance watoms1 watoms2 offset1 offset2 
      where
        -- INVARIANT: the balance must always remain >= 0

        tickRight acc balance [] [] offset1 offset2 = (List.reverse acc, balance, next1, next2)
        tickRight acc balance _ [] offset1 offset2 = (List.reverse acc, balance, (base1,offset1), next2)
        tickRight acc balance watoms1 watoms2@(wa:watoms2') offset1 offset2
          | let cost = consumption wa,
            cost <= balance            = tickRight (wa:acc) (balance-cost) watoms1 watoms2' offset1 (offset2+1)
          | otherwise                  = tickLeft acc balance watoms1 watoms2 offset1 offset2

        tickLeft acc balance [] [] offset1 offset2 = (List.reverse acc, balance, next1, next2)
        tickLeft acc balance [] _ offset1 offset2 = (List.reverse acc, balance, next1, (base2,offset2))
        tickLeft acc balance (wa:watoms1') watoms2 offset1 offset2 =
          tickRight (wa:acc) (balance + production wa) watoms1' watoms2 (offset1+1) offset2

        consumption wired_atom = sum $ map fst $ filter ((== trans_ch) . snd) (wires_in wired_atom)
        production wired_atom = sum $ map fst $ filter ((== trans_ch) . snd) (wires_out wired_atom)
    zipActions _ _ _ = assert False undefined





-- Monad for marking automata nodes; useful for DFS/BFS

type MarkingM nid = State (Set nid)
mark :: Ord nid => nid ->  MarkingM nid ()
mark nid = modify (Set.insert nid)

isMarked :: Ord nid => nid -> MarkingM nid Bool
isMarked nid = do
  marks <- get
  return (Set.member nid marks)




-- Fuses actions sequences in automata; inserts Loop nodes to make self-loops explicit.
-- This brings automata into a "normalized form" that is convenient for translation to
-- Atomix. DO NOT CALL THIS FUNCTION PREMATURELY AS LOOP NODES WILL BRAKE ZIPAUTOMATA.

fuseActions :: Automaton atom Int -> Automaton atom Int
fuseActions auto = auto { auto_graph = fused_graph }
  where
    fused_graph = fst $ runState (markAndFuse (auto_start auto) (auto_graph auto)) Set.empty

    markAndFuse :: Int -> NodeMap atom Int -> MarkingM Int (NodeMap atom Int)
    markAndFuse nid nmap = do
      marked <- isMarked nid
      if marked then return nmap else do
        mark nid
        fuse (fromJust $ assert (Map.member nid nmap) $ Map.lookup nid nmap) nmap

    fuse :: Node atom Int -> NodeMap atom Int -> MarkingM Int (NodeMap atom Int)
    fuse (Node _ Done) nmap = return nmap
    fuse (Node _ (Loop b)) nmap = markAndFuse b nmap
    fuse (Node _ (AutomataModel.Branch _ b1 b2 _)) nmap = do
      nmap <- markAndFuse b1 nmap
      markAndFuse b2 nmap
    fuse (Node nid nk@(Action atoms next)) nmap
      | nid == next = do -- self loop detected! Insert loop.
        let new_nid = nextNid auto
        mark new_nid
        let new_action_node = Node new_nid nk
        let loop_node = Node nid (Loop new_nid)
        return $ Map.insert nid loop_node $ Map.insert new_nid new_action_node $ nmap
      | otherwise =
        case fromJust $ assert (Map.member next nmap) $ Map.lookup next nmap of
          Node _ (Action atoms' next') ->
            let node = Node nid (Action (atoms++atoms') next')
            in fuse node (Map.insert nid node nmap)
          Node _ _ -> markAndFuse next nmap

deleteDeadNodes :: Ord nid => Automaton e nid -> Automaton e nid
deleteDeadNodes auto = auto { auto_graph = insertRecursively Map.empty (auto_start auto)}
  where
    insertRecursively nmap nid
      | Map.member nid nmap = nmap
      | otherwise = 
          let node = fromJust $ Map.lookup nid (auto_graph auto)
          in List.foldl insertRecursively (Map.insert nid node nmap) (sucs node)


-- Automata to DOT files

dotOfAuto :: (Atom e, Show nid) => Automaton e nid -> String
dotOfAuto a = prefix ++ List.intercalate ";\n" (nodes ++ edges) ++ postfix
  where
    prefix = "digraph ziria_automaton {\n"
    postfix = ";\n}"
    nodes = ("node [shape = point]":start)++ ("node [shape = doublecircle]":final) ++ ("node [shape = box]":normal)
    start = ["start [label=\"\"]"]
    (finalN,normalN) = List.partition (\(Node _ nk) -> case nk of { Done -> True; _ -> False }) $ Map.elems (auto_graph a)
    final = List.map (\(Node nid _) -> show nid ++ "[label=\"\"]") finalN
    normal = List.map showNode normalN
    edges = ("start -> " ++ show (auto_start a)) : (List.concat $ List.map edges_of_node normalN)
    edges_of_node node = [edge (node_id node) suc | suc <- sucs node]
    edge nid1 nid2 = show nid1 ++ " -> " ++ show nid2

    showNode (Node nid nk) = "  " ++ show nid ++ "[label=\"" ++ showNk nk ++ "\"]"

    showNk (Action watoms _) = List.intercalate ";\\n" $ List.map show watoms
    showNk (AutomataModel.Branch x _ _ True) = "WHILE<" ++ show x ++ ">"
    showNk (AutomataModel.Branch x _ _ False) = "IF<" ++ show x ++ ">"
    showNk Done = "DONE"
    showNk (Loop _) = "LOOP"


{-------------------- Top-level pipeline ---------------------------}

automatonPipeline :: Atom e => DynFlags -> GS.Sym -> Ty -> Ty -> AComp () () -> IO (Automaton e Int)
automatonPipeline dfs sym inty outty acomp = do 
 inch  <- freshName sym "src"  (acomp_loc acomp) inty Imm
 outch <- freshName sym "snk" (acomp_loc acomp) outty Mut
 let channels = Channels { in_chan = inch, out_chan = outch, ctrl_chan = Nothing }
     k = mkDoneAutomaton inch outch
 a <- mkAutomaton dfs sym channels acomp k
 let a' = normalize_auto_ids 0 $ deleteDeadNodes $ fuseActions $ a
 return a'



