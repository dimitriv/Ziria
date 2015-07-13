{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module AutomataModel where

import Data.Bool
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

import Utils(panicStr)

import Outputable
import qualified Text.PrettyPrint.HughesPJ as PPR
import Text.PrettyPrint.Boxes

type Chan = EId
showVar withUnique var = name var ++ (if withUnique then "$" ++ show (uniqId var) else "")
showChan = showVar

data ZirNk atom nid
  = Action { action_atoms :: [WiredAtom atom]
           , action_next  :: nid
           , action_pipeline_balance :: Map Chan Int -- initial balance of pipeline queues
           }
  | Branch { branch_var   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
           , branch_true  :: nid
           , branch_false :: nid
           , is_while     :: Bool -- Is this a while loop?
           }
  | Loop { loop_body :: nid } -- Infinite loop. Only transformers may (and must!) contain one of these.
  | Done

instance (Atom atom, Show nid) => Show (ZirNk atom nid) where
  show (Action was next _) = "Action" ++ show was ++ "->" ++ (show next) ++ ""
  show (AutomataModel.Branch x n1 n2 True) = "While[" ++ show x ++ "]->(" ++ (show n1) ++ "," ++ (show n2) ++ ")"
  show (AutomataModel.Branch x n1 n2 False) = "If[" ++ show x ++ "]->(" ++ (show n1) ++ "," ++ (show n2) ++ ")"
  show (Loop next) = "Loop->" ++ (show next)
  show Done = "Done"

  -- TODO: think about this later
  -- | StaticLoop  { iterations :: Int, loop_body :: Automaton }

data Node atom nid nkind
  = Node { node_id   :: nid
         , node_kind :: nkind atom nid
         }

instance (Atom atom, Show nid, Show (nkind atom nid)) => Show (Node atom nid nkind) where
  show (Node nid nk) = "<" ++ (show nid) ++ ":" ++ (show nk) ++ ">"

type NodeMap atom nid nkind = Map nid (Node atom nid nkind)


data Automaton atom nid nkind
  = Automaton { auto_graph   :: NodeMap atom nid nkind
              , auto_inchan  :: Chan
              , auto_outchan :: Chan
              , auto_start   :: nid
              }
  deriving Show

type ZAuto atom nid = Automaton atom nid ZirNk
type ZNode atom nid = Node atom nid ZirNk
type ZNodeMap atom nid = NodeMap atom nid ZirNk


data WiredAtom atom
  = WiredAtom { wires_in  :: [(Int,EId)]
              , wires_out :: [(Int,EId)]
              , the_atom  :: atom
              }
  deriving Eq


instance Atom a => Show (WiredAtom a) where
  show (WiredAtom inw outw atom) = showWires inw ++ show atom ++ showWires outw
    where
      showWires ws = "{" ++ (List.intercalate "," $ map showWire ws) ++ "}"
      showWire (n,var)
        | n==1      = showVar True var
        | otherwise = showVar True var ++ "^" ++ show n


countWrites :: Chan -> WiredAtom a -> Int
countWrites ch wa = sum $ map fst $ filter ((== ch) . snd) $ wires_out wa
countReads :: Chan -> WiredAtom a -> Int
countReads  ch wa = sum $ map fst $ filter ((== ch) . snd) $ wires_in  wa



class (Show a, Eq a) => Atom a where

  atomInTy  :: a -> [(Int,Ty)]
  atomOutTy :: a -> [(Int,Ty)]

  -- Constructors of atoms
  discardAtom :: (Int,Ty) -> a
  castAtom    :: (Int,Ty) -> (Int,Ty) -> a
  assertAtom :: Bool -> a

  -- Getting (wired) atoms from expressions
  expToWiredAtom :: AExp () -> Maybe EId -> WiredAtom a

  idAtom      :: Ty -> a
  idAtom t = castAtom (1,t) (1,t)

  assertWAtom :: Bool -> Chan -> WiredAtom a
  assertWAtom b x = WiredAtom [(1,x)] [] (assertAtom b)


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

class NodeKind nkind where
  sucsOfNk :: nkind a nid -> [nid]
  mapNkIds :: (nid1 -> nid2) -> nkind a nid1 -> nkind a nid2

instance NodeKind ZirNk where
  sucsOfNk Done = []
  sucsOfNk (Loop nxt)  = [nxt]
  sucsOfNk (Action _ nxt _) = [nxt]
  sucsOfNk (AutomataModel.Branch _ nxt1 nxt2 _) = [nxt1,nxt2]

  mapNkIds _ Done = Done
  mapNkIds f (Loop nxt)  = Loop (f nxt)
  mapNkIds f (Action watoms nxt pipes) = Action watoms (f nxt) pipes
  mapNkIds f (AutomataModel.Branch x nxt1 nxt2 l) = AutomataModel.Branch x (f nxt1) (f nxt2) l



-- auxilliary functions for automata construction & manipulation
size :: Automaton atom nid nkind -> Int
size = Map.size . auto_graph

sucs :: NodeKind nkind => Node atom nid nkind -> [nid]
sucs (Node _ nk) = sucsOfNk nk

nextNid :: Automaton atom Int nkind -> Int
nextNid a = max+1
  where (max,_) = Map.findMax (auto_graph a)

insert_prepend :: nkind atom Int -> Automaton atom Int nkind -> Automaton atom Int nkind
insert_prepend nkind a = -- this may be too strict -- ensure auto_closed $
  a { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)
    , auto_start = nid }
  where nid = nextNid a

nodeKindOfId :: Ord nid => nid -> Automaton atom nid nkind -> nkind atom nid
nodeKindOfId nid a = node_kind $ fromJust $ assert (Map.member nid (auto_graph a)) $
                                            Map.lookup nid (auto_graph a)

-- precondition: a1 and a2 must agree on auto_inchan and auto_outchan
concat_auto :: Atom atom => Ord nid => ZAuto atom nid -> ZAuto atom Int -> ZAuto atom Int
concat_auto a1 a2 = a1' { auto_graph = concat_graph }
  where
    a1' = replace_done_with (auto_start a2) $ normalize_auto_ids (nextNid a2) a1
    graph1 = Map.delete (auto_start a2) (auto_graph a1')
    graph2 = auto_graph a2
    concat_graph = assert (auto_inchan a1 == auto_inchan a2) $
                   assert (auto_outchan a1 == auto_outchan a2) $
                   assert (Map.null $ Map.intersection graph1 graph2) $
                   Map.union graph1 graph2

mkDoneAutomaton :: Chan -> Chan -> ZAuto e Int
mkDoneAutomaton ic oc
  = Automaton { auto_graph = Map.singleton 0 (Node 0 Done), auto_start = 0
              , auto_outchan = oc
              , auto_inchan  = ic
              }


-- Mapping Automata Labels

map_node_ids :: NodeKind nk => (nid1 -> nid2) -> Node e nid1 nk -> Node e nid2 nk
map_node_ids map_id (Node nid nkind) = Node (map_id nid) (mapNkIds map_id nkind)

map_auto_ids :: (NodeKind nk, Ord nid1, Ord nid2) => (nid1 -> nid2) -> Automaton e nid1 nk -> Automaton e nid2 nk
map_auto_ids map_id a = a { auto_graph = new_graph, auto_start = new_start }
 where
    new_start = map_id (auto_start a)
    new_graph = Map.mapKeys map_id $ Map.map (map_node_ids map_id) $ auto_graph a

-- replaces arbitrary automata node-ids with Ints >= first_id
normalize_auto_ids :: (NodeKind nk, Atom e, Ord nid) => Int -> Automaton e nid nk -> Automaton e Int nk
normalize_auto_ids first_id a = map_auto_ids map_id a
  where
    map_id nid = fromJust $ assert (Map.member nid normalize_map) $ Map.lookup nid normalize_map
    (_, normalize_map) = Map.foldrWithKey f (first_id, Map.empty) (auto_graph a)
    f nid _ (counter, nid_map) = (counter+1, Map.insert nid counter nid_map)

replace_done_with :: Ord nid => nid -> ZAuto e nid -> ZAuto e nid
replace_done_with nid a = map_auto_ids (\nid -> Map.findWithDefault nid nid replace_map) a
  where
    replace_map = Map.foldr fold_f Map.empty (auto_graph a)
    fold_f (Node nid' Done) mp = Map.insert nid' nid mp
    fold_f _ mp = mp


-- debugging
auto_closed :: (NodeKind nk, Ord nid) => Automaton e nid nk -> Bool
auto_closed a = Map.foldrWithKey node_closed (isDefined $ auto_start a) (auto_graph a)
  where
    isDefined nid = Map.member nid (auto_graph a)
    node_closed nid (Node nid' nkind) closed = closed && nid==nid' &&
      foldl suc_closed (isDefined nid) (sucsOfNk nkind)
    suc_closed closed suc = closed && isDefined suc


-- Constructing Automata from Ziria Comps

data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }


mkAutomaton :: Atom e
            => DynFlags
            -> GS.Sym
            -> Channels  -- i/o/ctl channel
            -> AComp a ()
            -> ZAuto e Int -- what to do next (continuation)
            -> IO (ZAuto e Int)
mkAutomaton dfs sym chans comp k = go $ assert (auto_closed k) $ acomp_comp comp
  where
    loc = acomp_loc comp
    go (ATake1 t) =
      let inp = [(1,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom (1,t)) (\_ -> idAtom t) (ctrl_chan chans)
          nkind = Action [WiredAtom inp outp atom] (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ATakeN t n) =
      let inp  = [(n,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          outty = TArray (Literal n) t
          atom = maybe (discardAtom (n,t)) (\_ -> castAtom (n,t) (1,outty)) (ctrl_chan chans)
          nkind = Action [WiredAtom inp outp atom] (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (AEmit1 x) =
      let inp = [(1, x)]
          outp = [(1, out_chan chans)]
          atom = idAtom (nameTyp x)
          nkind = Action [WiredAtom inp outp atom] (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (AEmitN t n x) =
      let inp = [(1, x)]
          outp = [(n, out_chan chans)]
          atom = castAtom (1, nameTyp x) (n,t)
          nkind = Action [WiredAtom inp outp atom] (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ACast s (n1,t1) (n2,t2)) =
      let inp  = [(n1, in_chan chans)]
          outp = [(n2, out_chan chans)]
          atom = castAtom (n1,t1) (n2,t2)
          nkind = Action [WiredAtom inp outp atom] (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

--    go (MapOnce f closure) =
--      let args = in_chan chans : closure
--          expr = MkExp (ExpApp f args) noLoc ()
--          watom = expToWiredAtom expr (Just $ out_chan chans)
--          nkind = Action [watom] (auto_start k)
--      in return $ insert_prepend nkind k

    go (AReturn e) =
      let watom = expToWiredAtom e (ctrl_chan chans)
          nkind = Action [watom] (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a


    go (ABind mbx c1 c2) = do
      a2 <- mkAutomaton dfs sym chans c2 k
      a <- mkAutomaton dfs sym (chans { ctrl_chan = mbx }) c1 a2
      return $ assert (auto_closed a2) $ assert (auto_closed a) a

    go (APar _ c1 t c2) = do
      pipe_ch <- freshName sym (pipeName c1 c2) loc t Mut
      let k1 = mkDoneAutomaton (in_chan chans) pipe_ch
      let k2 = mkDoneAutomaton pipe_ch (out_chan chans)
      a1 <- mkAutomaton dfs sym (chans {out_chan = pipe_ch}) c1 k1
      a2 <- mkAutomaton dfs sym (chans {in_chan = pipe_ch}) c2 k2
      return $ zipAutomata a1 a2 k

    go (ABranch x c1 c2) = do
      a1 <- mkAutomaton dfs sym chans c1 k
      a2 <- mkAutomaton dfs sym chans c2 (a1 { auto_start = auto_start k})
      let nkind = AutomataModel.Branch x (auto_start a1) (auto_start a2) False
      let a = insert_prepend nkind a2
      return $ assert (auto_closed a1) $ assert (auto_closed a2) $ assert (auto_closed a) a

    go (ARepeatN n c) = do
      a <- applyN n (mkAutomaton dfs sym chans c) k
      return $ assert (auto_closed a) a
      where applyN 0 f x = return x
            applyN n f x = do
              y <- applyN (n-1) f x
              f y

    go (ARepeat c) =
      case nodeKindOfId (auto_start k) k of
        Done -> do
          a0 <- mkAutomaton dfs sym chans c k
          let nid = auto_start k
          let node = fromJust $ assert (Map.member (auto_start a0) (auto_graph a0)) $ 
                     Map.lookup (auto_start a0) (auto_graph a0) -- Loop (auto_start a)
          let nmap = Map.insert nid node $ Map.delete (auto_start a0) (auto_graph a0)
          let a = map_auto_ids (\id -> if id == (auto_start a0) then nid else id) $
                    a0 { auto_start = nid, auto_graph = nmap }
          return $ assert (auto_closed a0) $ assert (auto_closed a) a
        _ -> fail "Repeat should not have a continuation!"

    go (AWhile x c) = do
      let k' = insert_prepend Done k
      let nid = auto_start k'
      a0 <- mkAutomaton dfs sym chans c k'
      let nkind = AutomataModel.Branch x (auto_start a0) (auto_start k) True
      let a = a0 { auto_start = nid, auto_graph = Map.insert nid (Node nid nkind) (auto_graph a0)}
      return $ assert (auto_closed a0) $ assert (auto_closed a) a

    go (AUntil x c) = do
      let k' = insert_prepend Done k
      let nid = auto_start k'
      a0 <- mkAutomaton dfs sym chans c k'
      let nkind = AutomataModel.Branch x (auto_start a0) (auto_start k) True
      let a = a0 { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a0)}
      return $ assert (auto_closed a0) $ assert (auto_closed a) a


    -- pipe name
    pipeName c1 c2 = List.intercalate ">>>" $ 
                     map (getName . PPR.render . ppr) [parLoc (Left ()) c1, parLoc (Right ()) c2]
    getName = takeWhile (/= '.') . reverse . takeWhile (/= '\\') . takeWhile (/= '/') . reverse
    parLoc side c
      | APar _ cl _ cr <- acomp_comp c
      , Left c0 <- side = parLoc side cr
      | APar _ cl _ cr <- acomp_comp c
      , Right () <- side = parLoc side cl
      | otherwise = acomp_loc c




---- Zipping Automata
-- remaining resources in pipe ("balance"), state of left automaton, state of right automaton
type ProdNid = (Balance, (Int,Int), (Int,Int))
type Balance = Int

-- Precondition: a1 and a2 should satisfy (auto_outchan a1) == (auto_inchan a2)
-- a1 and a2 MUST NOT contain explicit loop nodes (but may contain loops)!!
zipAutomata :: forall e. Atom e => ZAuto e Int -> ZAuto e Int -> ZAuto e Int -> ZAuto e Int
zipAutomata a1 a2 k = concat_auto prod_a k
  where
    prod_a = (\a -> assert (auto_closed a) a) $
             assert (auto_closed a1) $
             assert (auto_closed a2) $
             Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) (0,s1,s2)
    s1 = (auto_start a1, 0)
    s2 = (auto_start a2, 0)
    prod_nmap = zipNodes 0 s1 s2 Map.empty
    pipe_ch = assert (auto_outchan a1 == auto_inchan a2) $ auto_outchan a1


    -- this allows us to address nodes in the input automata using (base_id,offset) pairs
    lookup :: (Int,Int) -> ZAuto e Int -> ZNode e (Int,Int)
    lookup nid@(baseId,offset) a =
      case fromJust $ assert (Map.member baseId (auto_graph a)) $ Map.lookup baseId (auto_graph a) of
        Node _ (Action watoms next pipes) ->
          assert (offset < length watoms) $
          Node nid $ Action (drop offset watoms) (next,0) pipes'
            where 
              pipes' = Map.mapWithKey updatePipe pipes
              updatePipe q n = n + (sum $ map (atomBalance q) $ take offset watoms)
              atomBalance q wa = countWrites q wa - countReads q wa
        node@(Node _ _) ->
          assert (offset == 0) $ map_node_ids (\id -> (id,0)) node


    zipNodes :: Balance -> (Int,Int) -> (Int,Int) -> ZNodeMap e ProdNid -> ZNodeMap e ProdNid
    zipNodes balance nid1 nid2 prod_nmap =
      case Map.lookup (balance,nid1,nid2) prod_nmap of
        Nothing -> zipNodes' balance (lookup nid1 a1) (lookup nid2 a2) prod_nmap
        Just _ -> prod_nmap -- We have already seen this product location. We're done!

    zipNodes' :: Balance -> ZNode e (Int,Int) -> ZNode e (Int,Int) -> ZNodeMap e ProdNid -> ZNodeMap e ProdNid
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

    zipNodes' balance n1@(Node id1 (Action _ _ pipes1)) n2@(Node id2 (Action _ _ pipes2)) prod_nmap =
      let prod_nid = (balance,id1,id2)
          noDups = const (assert False)
          pipes = Map.insertWith noDups pipe_ch balance $ Map.unionWith noDups pipes1 pipes2
          (watoms, balance', next1, next2) = zipActions balance n1 n2
          prod_nkind = Action watoms (balance',next1,next2) pipes
          prod_node = Node prod_nid prod_nkind
      in zipNodes balance' next1 next2 $ Map.insert prod_nid prod_node prod_nmap

    zipActions :: Balance -> ZNode atom (Int,Int) -> ZNode atom (Int,Int) -> ([WiredAtom atom], Int, (Int,Int), (Int,Int))
    zipActions balance (Node (base1,offset1) (Action watoms1 next1 _)) (Node (base2,offset2) (Action watoms2 next2 _))
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

        --consumption wired_atom = if any ((== pipe) . snd) (wires_in wired_atom) then 1 else 0
        --production wired_atom = if any ((== pipe) . snd) (wires_out wired_atom) then 1 else 0
        consumption = countReads pipe_ch
        production  = countWrites pipe_ch
    zipActions _ _ _ = assert False undefined





-- Monad for marking automata nodes; useful for DFS/BFS

-- We maintain two sets: active, and done
-- Invariant: every node starts as inactive and not done,
-- is eventially marked active, and finally marked done.
-- `active` and `done` are disjoint at all times
type MarkingM nid = State (Set nid,Set nid)

pushActive :: Ord nid => nid -> MarkingM nid a -> MarkingM nid a
pushActive nid m = do
  modify (\(active,done) -> (Set.insert nid active, done))
  m

inNewFrame :: Ord nid => MarkingM nid a -> MarkingM nid a
inNewFrame m = do
  modify (\(active,done) -> (Set.empty, Set.union active done))
  m

isActive :: Ord nid => nid -> MarkingM nid Bool
isActive nid = do
  (active,_) <- get
  return $ Set.member nid active

isDone :: Ord nid => nid -> MarkingM nid Bool
isDone nid = do
  (_,done) <- get
  return $ Set.member nid done


-- Fuses actions sequences in automata. This brings automata into a from that
-- is convenient for printing and further processing.
fuseActions :: forall atom nid. Ord nid => ZAuto atom nid -> ZAuto atom nid
fuseActions auto = auto { auto_graph = fused_graph }
  where
    fused_graph = fst $ runState (doAll [auto_start auto] (auto_graph auto)) (Set.empty, Set.empty)

    doAll :: [nid] -> ZNodeMap atom nid -> MarkingM nid (ZNodeMap atom nid)
    doAll work_list nmap =
      case work_list of
        [] -> return nmap
        nid:wl -> do
          (wl',nmap') <- markAndFuse nid nmap
          inNewFrame (doAll (wl'++wl) nmap')


   -- Invariant: if isDone nid then all
   -- its successors either satisfy isDone or are in the worklist, and the node is
   --  (a) a decision node (i.e. not an action node), or
    -- (b) an action node with its next node being a decision node

    markAndFuse :: nid -> ZNodeMap atom nid -> MarkingM nid ([nid], ZNodeMap atom nid)
    markAndFuse nid nmap = do
      done <- isDone nid
      if done
        then return ([],nmap)
        else pushActive nid $ fuse (fromJust $ assert (Map.member nid nmap) $ Map.lookup nid nmap) nmap


    -- precondition: input node is marked active
    fuse :: ZNode atom nid -> ZNodeMap atom nid -> MarkingM nid ([nid], ZNodeMap atom nid)
    fuse (Node _ Done) nmap = return ([],nmap)
    fuse (Node _ (Loop b)) nmap = return ([b],nmap)
    fuse (Node _ (AutomataModel.Branch _ b1 b2 _)) nmap = return ([b1,b2],nmap)

    fuse (Node nid nk@(Action atoms next pipes)) nmap = do
        active <- isActive next
        -- don't fuse back-edges (including self-loops)!
        if active then return ([],nmap) else do
          -- fuse sucessor node(s) first, ...
          (wl,nmap) <- markAndFuse next nmap
          -- ... then perform merger if possible
          return $ case fromJust $ assert (Map.member next nmap) $ Map.lookup next nmap of
            Node _ (Action atoms' next' _) ->
              let node = Node nid (Action (atoms++atoms') next' pipes)
              in (wl, Map.insert nid node nmap)
            Node _ _ -> (wl,nmap)


deleteDeadNodes :: (NodeKind nk, Ord nid) => Automaton e nid nk -> Automaton e nid nk
deleteDeadNodes auto = auto { auto_graph = insertRecursively Map.empty (auto_start auto)}
  where
    insertRecursively nmap nid
      | Map.member nid nmap = nmap
      | otherwise =
          case Map.lookup nid (auto_graph auto) of
            Nothing -> panicStr "deleteDeadNodes: input graph is not closed!"
            Just node -> List.foldl insertRecursively (Map.insert nid node nmap) (sucs node)


markSelfLoops :: ZAuto e Int -> ZAuto e Int
markSelfLoops a = a { auto_graph = go (auto_graph a)}
  where go nmap = Map.foldr markNode nmap nmap
        markNode (Node nid nk@(Action watoms next _)) nmap
          = if nid /= next then nmap else
              let nid' = nextNid a
              in Map.insert nid (Node nid (Loop nid')) $ Map.insert nid' (Node nid' nk) $ nmap
        markNode _ nmap = nmap


predecessors :: forall e nid nk. (NodeKind nk, Ord nid) => Automaton e nid nk -> Map nid (Set nid)
predecessors a = go (auto_start a) Map.empty
  where
    nmap = auto_graph a
    go nid pred_map =
      foldl (go' nid) pred_map (sucs $ fromJust $ assert (Map.member nid nmap) $ Map.lookup nid nmap)
    go' pred pred_map nid = 
      case Map.lookup nid pred_map of
        Just preds -> Map.insert nid (Set.insert pred preds) pred_map
        Nothing -> go nid $ Map.insert nid (Set.singleton pred) pred_map


-- prune action that are known to be unreachable
pruneUnreachable :: forall e nid. (Atom e, Ord nid) => nid -> ZAuto e nid -> ZAuto e nid
pruneUnreachable nid a = a { auto_graph = prune (auto_graph a) nid }
  where
    preds = let predMap = predecessors a in (\nid -> fromJust $ Map.lookup nid predMap)

    prune :: ZNodeMap e nid -> nid -> ZNodeMap e nid
    prune nmap nid = 
      case Map.lookup nid nmap of
        Nothing -> nmap -- already pruned
        Just _ -> Set.foldl (pruneBkw nid) (Map.delete nid nmap) (preds nid)

    pruneBkw :: nid -> ZNodeMap e nid -> nid -> ZNodeMap e nid
    pruneBkw suc nmap nid =
      case Map.lookup nid nmap of
        Nothing -> nmap
        Just (Node _ Done) -> assert False undefined
        Just (Node _ (Action _ next _)) -> if next==suc then prune nmap nid else nmap
        Just (Node _ (Loop next)) -> if next==suc then prune nmap nid else nmap
        Just (Node _ (Branch x suc1 suc2 _)) 
          | suc == suc1 -> -- suc2 becomes the unique sucessor (since suc1 is unreachable)
            let nk = Action [assertWAtom False x] suc2 Map.empty
            in Map.insert nid (Node nid nk) nmap
          | suc == suc2 -> -- suc1 becomes the unique sucessor (since suc2 is unreachable)
            let nk = Action [assertWAtom True x] suc1 Map.empty
            in Map.insert nid (Node nid nk) nmap
          | otherwise -> assert False undefined



-- Automata to DOT files

nextPipes watoms pipes = Map.mapWithKey updatePipe pipes
  where updatePipe pipe n = n + sum (map (countWrites pipe) watoms)
                              - sum (map (countReads pipe) watoms)

dotOfAuto :: (Atom e, Show nid) => DynFlags -> ZAuto e nid -> String
dotOfAuto dflags a = prefix ++ List.intercalate ";\n" (nodes ++ edges) ++ postfix
  where
    printActions = isDynFlagSet dflags Verbose
    printPipeNames = isDynFlagSet dflags PrintPipeNames
    prefix = "digraph ziria_automaton {\n"
    postfix = ";\n}"
    nodes = ("node [shape = point]":start) ++
            ("node [shape = doublecircle]":final) ++
            ("node [shape = box]":decision) ++
            ("node [shape = box, fontname=monospace, fontsize=11, style=filled, fillcolor=\"white\"]":action)
    start = ["start [label=\"\"]"]
    (finalN,normalN) = List.partition (\(Node _ nk) -> case nk of { Done -> True; _ -> False }) $ Map.elems (auto_graph a)
    (actionN,decisionN) = List.partition (\(Node _ nk) -> case nk of { Action {} -> True; _ -> False }) normalN
    final = List.map (\(Node nid _) -> show nid ++ "[label=\"\"]") finalN
    action = List.map showNode actionN
    decision = List.map showNode decisionN
    edges = ("start -> " ++ show (auto_start a)) : (List.map edges_of_node normalN)
    edges_of_node node = List.intercalate "; " [edge (node_id node) suc | suc <- sucs node]
    edge nid1 nid2 = show nid1 ++ " -> " ++ show nid2

    showNode (Node nid nk) = "  " ++ show nid ++ "[label=\"" ++ showNk nk ++ "\"" ++ maybeToolTip nk ++ "]"

    showNk (Action watoms _ pipes)
      | printActions = List.intercalate "\\n" (showPipes watoms pipes : showWatoms watoms)
      | otherwise = showPipes watoms pipes
    showNk (AutomataModel.Branch x _ _ True) = "WHILE<" ++ show x ++ ">"
    showNk (AutomataModel.Branch x _ _ False) = "IF<" ++ show x ++ ">"
    showNk Done = "DONE"
    showNk (Loop _) = "LOOP"

    showWatoms = map showWatomGroup . List.group
    showWatomGroup wa = case length wa of 1 -> show (head wa) 
                                          n -> show n ++ " TIMES DO " ++ show (head wa)

    showPipes watoms pipes 
      | printPipeNames = render $ punctuateH top (text " | ") $ boxedPipes
      | otherwise     = List.intercalate "\\n" $ map printPipes [pipes, nextPipes watoms pipes]
        where
          boxedPipes = map boxPipe $ Map.toAscList $
                       Map.intersectionWith (,) pipes (nextPipes watoms pipes)
          boxPipe (pipe, state) = vcat center1 $ map text [show pipe, show state]
          printPipes = List.intercalate "|" . map printPipe . Map.toAscList
          printPipe (_pipe_ch, val) = show val

    maybeToolTip (Action watoms _ pipes) = " tooltip=\"" ++ showPipeNames pipes ++ "\""
    maybeToolTip _ = ""
    showPipeNames = List.intercalate " | " . map show . Map.keys 

{-------------------- Top-level pipeline ---------------------------}

automatonPipeline :: Atom e => DynFlags -> GS.Sym -> Ty -> Ty -> AComp () () -> IO (ZAuto e Int)
automatonPipeline dfs sym inty outty acomp = do
  inch  <- freshName sym "src"  (acomp_loc acomp) inty Imm
  outch <- freshName sym "snk" (acomp_loc acomp) outty Mut
  let channels = Channels { in_chan = inch, out_chan = outch, ctrl_chan = Nothing }
  let k = mkDoneAutomaton inch outch

  putStrLn ">>>>>>>>>> mkAutomaton"
  a <- mkAutomaton dfs sym channels acomp k
  --putStrLn (dotOfAuto True a)
  putStrLn $ "<<<<<<<<<<< mkAutomaton (" ++ show (size a) ++ " states)"

  putStrLn ">>>>>>>>>>> fuseActions"
  let a_f = fuseActions a
  --putStrLn (dotOfAuto True a_f)
  putStrLn $ "<<<<<<<<<<< fuseActions (" ++ show (size a_f) ++ " states)"

  putStrLn ">>>>>>>>>>> deleteDeadNodes"
  let a_d = deleteDeadNodes a_f
  --putStrLn (dotOfAuto True a_d)
  putStrLn $ "<<<<<<<<<<< deleteDeadNodes (" ++ show (size a_d) ++ " states)"

  putStrLn ">>>>>>>>>>> markSelfLoops"
  let a_l = markSelfLoops a_d
  --putStrLn (dotOfAuto True a_l)
  putStrLn $ "<<<<<<<<<<< markSelfLoops (" ++ show (size a_l) ++ " states)"

  putStrLn ">>>>>>>>>>> normalize_auto_ids"
  let a_n = normalize_auto_ids 0 a_l
  --putStrLn (dotOfAuto True a_n)
  putStrLn $ "<<<<<<<<<<< normalize_auto_ids (" ++ show (size a_n) ++ " states)"
  putStrLn "<<<<<<<<<<< COMPLETED AUTOMATON CONSTRUCTION\n"

  return a_n



