{-# LANGUAGE ScopedTypeVariables, TupleSections, FlexibleContexts #-}
{-# OPTIONS #-}
module AutomataModel where

import Data.Either
import Data.Tuple
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Function

import Control.Exception
import Control.Monad.State

import AtomComp
import AstExpr
import AstComp ( ParInfo (..), plInfo, PlInfo (..) )
import Opts
import AtomixCompTransform ( freshName, freshNameDoc )
import qualified GenSym as GS

import Utils(panicStr)
import Control.Applicative ( (<$>) )

import Outputable
import qualified Text.PrettyPrint.HughesPJ as PPR
import Text.PrettyPrint.Boxes
import Debug.Trace


{------------------------------------------------------------------------
  Generic Atomix Automata Model
  (polymorp over atoms, node IDs, and node kinds)
------------------------------------------------------------------------}

type Chan = EId

data Automaton atom nid nkind
  = Automaton { auto_graph   :: NodeMap atom nid nkind
              , auto_inchan  :: Chan
              , auto_outchan :: Chan
              , auto_start   :: nid
              }
  deriving Show

type NodeMap atom nid nkind = Map nid (Node atom nid nkind)

data Node atom nid nkind
  = Node { node_id   :: nid
         , node_kind :: nkind atom nid
         }

class NodeKind nkind where
  sucsOfNk :: nkind atom nid -> [nid]
  mapNkIds :: (nid1 -> nid2) -> nkind atom nid1 -> nkind atom nid2

data WiredAtom atom
  = WiredAtom { wires_in  :: [(Int,EId)]      -- [(Int,Wire)]
              , wires_out :: [(Int,EId)]      -- [(Int,Wire)]
              , the_atom  :: atom
              }
  deriving Eq


type AId = String

{-- Generic Atom Interfae --------------------------------------------}
class (Show a, Eq a) => Atom a where

  atomInTy  :: a -> [(Int,Ty)]
  atomOutTy :: a -> [(Int,Ty)]

  -- Constructors of atoms
  discardAtom  :: AId -> (Int,Ty) -> a
  castAtom     :: AId -> (Int,Ty) -> (Int,Ty) -> a
  assertAtom   :: Bool -> a
  rollbackAtom :: Chan -> Int -> a
  isRollbackAtom :: a -> Maybe (Int, Chan)
  clearAtom :: Map Chan Int -> a

  -- Getting (wired) atoms from expressions
  expToWiredAtom :: AExp () -> Maybe Chan -> WiredAtom a

  wiredAtomId :: WiredAtom a -> String

  -- Default implementations based on this abstract interface
  idAtom     :: AId -> Ty -> a
  idAtom x t = castAtom x (1,t) (1,t)

  assertWAtom :: Bool -> Chan -> WiredAtom a
  assertWAtom b x = WiredAtom [(1,x)] [] (assertAtom b)

  rollbackWAtom :: Chan -> Int -> WiredAtom a
  rollbackWAtom ch n = WiredAtom [] [(n,ch)] (rollbackAtom ch n)

  isRollbackWAtom :: WiredAtom a -> Maybe (Int, Chan)
  isRollbackWAtom (WiredAtom in_tys out_tys a)
    | res@(Just x) <- isRollbackAtom a
    = assert (null out_tys) $
      assert (null (tail in_tys)) $
      assert (head in_tys == x) $ res
    | otherwise = Nothing

  clearWAtom :: Map Chan Int -> WiredAtom a
  clearWAtom pipes' = WiredAtom in_tys [] (clearAtom pipes)
    where in_tys = map swap (Map.toList pipes)
          pipes = Map.filter (>0) pipes'



{------------------------------------------------------------------------
  Concrete NodeKind Instances
------------------------------------------------------------------------}


data SimplNk atom nid
  = SAtom { wired_atom :: WiredAtom atom
          , atom_next  :: nid
          , pipe_balances :: Map Chan Int -- balance of pipeline queues
          }
  | SBranch { zbranch_ch   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
            , zbranch_true  :: nid
            , zbranch_false :: nid
            , zbranch_while :: Bool -- Is this a while loop?
            , pipe_balances :: Map Chan Int -- balance of pipeline queues
            }
  | SDone { pipe_balances :: Map Chan Int -- balance of pipeline queues
          , must_insert_rollback :: Maybe Int -- temporary info for zipping algorithm: record how far the
          }                                   -- input queue of the left automaton has to be rolled back.
  deriving Show

instance NodeKind SimplNk where
  sucsOfNk (SDone {}) = []
  sucsOfNk (SAtom _ nxt _) = [nxt]
  sucsOfNk (SBranch _ nxt1 nxt2 _ _) = [nxt1,nxt2]

  mapNkIds _ (SDone pipes rollback) = SDone pipes rollback
  mapNkIds f (SAtom watoms nxt pipes) = SAtom watoms (f nxt) pipes
  mapNkIds f (SBranch x nxt1 nxt2 l pipes) = SBranch x (f nxt1) (f nxt2) l pipes




data CfgNk atom nid
  = CfgAction { action_atoms :: [WiredAtom atom]
              , action_next  :: nid
              , action_pipeline_balance :: Map Chan Int -- initial balance of pipeline queues
              }
  | CfgBranch { branch_ch   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
              , branch_true  :: nid
              , branch_false :: nid
              , is_while     :: Bool -- Is this a while loop?
              }
  | CfgLoop { loop_body :: nid } -- Infinite loop. Only transformers may (and must!) contain one of these.
  | CfgDone

instance NodeKind CfgNk where
  sucsOfNk CfgDone = []
  sucsOfNk (CfgLoop nxt)  = [nxt]
  sucsOfNk (CfgAction _ nxt _) = [nxt]
  sucsOfNk (CfgBranch _ nxt1 nxt2 _) = [nxt1,nxt2]

  mapNkIds _ CfgDone = CfgDone
  mapNkIds f (CfgLoop nxt)  = CfgLoop (f nxt)
  mapNkIds f (CfgAction watoms nxt pipes) = CfgAction watoms (f nxt) pipes
  mapNkIds f (CfgBranch x nxt1 nxt2 l) = CfgBranch x (f nxt1) (f nxt2) l



data AtomixNk atom nid
  = AtomixState { state_atoms :: [WiredAtom atom]
                , constraints :: Map (Int,Int) [Dependency]
                , state_decision :: Decision nid
                }

data Dependency
  = RW
  | WR
  | WW
  | CC
  | PP
  | PC
  deriving (Show, Eq)

data Decision nid
  = AtomixDone
  | AtomixLoop { atomix_loop_body :: nid}
  | AtomixBranch { atomix_branch_ch :: Chan
                 , atomix_branch_true :: nid
                 , atomix_branch_false :: nid
                 }
  deriving Show


{-- Pretty Printing ------------------------------------------------------------}

instance (Atom atom, Show nid, Show (nkind atom nid)) => Show (Node atom nid nkind) where
  show (Node nid nk) = "<" ++ (show nid) ++ ":" ++ (show nk) ++ ">"


instance (Atom atom, Show nid) => Show (CfgNk atom nid) where

  show (CfgAction was next _) = "Action" ++ show was ++ "->" ++ (show next) ++ ""

  show (CfgBranch x n1 n2 True)
    = "While[" ++ show x ++ "]->(" ++ (show n1) ++ "," ++ (show n2) ++ ")"

  show (CfgBranch x n1 n2 False)
    = "If[" ++ show x ++ "]->(" ++ (show n1) ++ "," ++ (show n2) ++ ")"

  show (CfgLoop next) = "Loop->" ++ (show next)

  show CfgDone = "Done"


instance Atom a => Show (WiredAtom a) where
  show (WiredAtom inw outw atom) = showWires inw ++ show atom ++ showWires outw
    where
      showWires ws = "{" ++ (List.intercalate "," $ map showWire ws) ++ "}"
      showWire (n,ch)
        | n==1      = showChan True ch
        | otherwise = showChan True ch ++ "^" ++ show n


showChan :: Bool -> GName t -> String
showChan withUnique ch
  = nameDoc ch ++ (if withUnique then "$" ++ show (uniqId ch) else "")



{-- Type Abreviations ------------------------}

type SAuto atom nid = Automaton atom nid SimplNk
type SNode atom nid = Node atom nid SimplNk
type SNodeMap atom nid = NodeMap atom nid SimplNk

type CfgAuto atom nid = Automaton atom nid CfgNk
type CfgNode atom nid = Node atom nid CfgNk
type CfgNodeMap atom nid = NodeMap atom nid CfgNk

type AxAuto atom nid = Automaton atom nid AtomixNk
type AxNode atom nid = Node atom nid AtomixNk
type AxNodeMap atom nid = NodeMap atom nid AtomixNk









{------------------------------------------------------------------------
  Auxilliary Functions for Automata Construction & Manipulation
------------------------------------------------------------------------}

size :: Automaton atom nid nkind -> Int
size = Map.size . auto_graph

sucs :: NodeKind nkind => Node atom nid nkind -> [nid]
sucs (Node _ nk) = sucsOfNk nk

isDoneNk :: SimplNk e nid -> Bool
isDoneNk (SDone {}) = True
isDoneNk _ = False

isDonePred :: Ord nid => SAuto e nid -> SNode e nid -> Bool
isDonePred a = any (isDoneNk . nodeKindOfId a) . sucs

-- Create predecessor map. For efficieny reasons, should be cached locally:
-- let preds = mkPreds a in ...
mkPreds :: forall e nid nk. (NodeKind nk, Ord nid) => Automaton e nid nk -> nid -> Set nid
mkPreds a = (\nid -> assert (Map.member nid nmap) $ Map.findWithDefault Set.empty nid pred_map)
  where
    nmap = auto_graph a
    pred_map = go (auto_start a) Map.empty
    go nid pred_map = foldl (insertPred nid) pred_map (sucs node)
      where node = fromJust $ assert (Map.member nid nmap) $ Map.lookup nid nmap
    insertPred pred pred_map nid =
      case Map.lookup nid pred_map of
        Just preds -> Map.insert nid (Set.insert pred preds) pred_map
        Nothing -> go nid $ Map.insert nid (Set.singleton pred) pred_map

nodeKindOfId :: Ord nid => Automaton atom nid nkind -> nid -> nkind atom nid
nodeKindOfId a nid = node_kind $ fromJust $ assert (Map.member nid (auto_graph a)) $
                                            Map.lookup nid (auto_graph a)
countWrites :: Chan -> WiredAtom a -> Int
countWrites ch wa = sum $ map fst $ filter ((== ch) . snd) $ wires_out wa

countReads :: Chan -> WiredAtom a -> Int
countReads  ch wa = sum $ map fst $ filter ((== ch) . snd) $ wires_in  wa

-- in the presence of rollbacks, an atom may consume from and produce for the same queue
atomCost :: Chan -> WiredAtom a -> Int
atomCost ch wa = countReads ch wa - countWrites ch wa

nextPipes :: [WiredAtom a] -> Map Chan Int -> Map Chan Int
nextPipes watoms pipes = Map.mapWithKey updatePipe pipes
  where updatePipe pipe n = n - sum (map (atomCost pipe) watoms)

next_pipe_balances :: SimplNk atom nid -> Map Chan Int
next_pipe_balances (SAtom wa _ pipes) = nextPipes [wa] pipes
next_pipe_balances nk = pipe_balances nk

nextNid :: Automaton atom Int nkind -> Int
nextNid a = maxId+1
  where (maxId,_) = Map.findMax (auto_graph a)

insertNk :: Ord nid => nid -> nkind atom nid -> NodeMap atom nid nkind -> NodeMap atom nid nkind
insertNk nid nk nmap = Map.insert nid (Node nid nk) nmap

insert_prepend :: nkind atom Int -> Automaton atom Int nkind -> Automaton atom Int nkind
insert_prepend nkind a =
  a { auto_graph = insertNk nid nkind (auto_graph a)
    , auto_start = nid }
  where nid = nextNid a

-- precondition: a1 and a2 must agree on auto_inchan and auto_outchan
concat_auto :: Atom atom => Ord nid => SAuto atom nid -> SAuto atom Int -> SAuto atom Int
concat_auto a1 a2 = a1' { auto_graph = concat_graph }
  where
    a1' = replace_done_with (auto_start a2) $ normalize_auto_ids (nextNid a2) a1
    graph1 = Map.delete (auto_start a2) (auto_graph a1')
    graph2 = auto_graph a2
    concat_graph = assert (auto_inchan a1 == auto_inchan a2) $
                   assert (auto_outchan a1 == auto_outchan a2) $
                   assert (Map.null $ Map.intersection graph1 graph2) $
                   Map.union graph1 graph2


mkDoneAutomaton :: Chan -> Chan -> SAuto e Int
mkDoneAutomaton in_ch out_ch
  = Automaton { auto_graph = Map.singleton 0 (Node 0 $ SDone Map.empty Nothing), auto_start = 0
              , auto_outchan = out_ch
              , auto_inchan  = in_ch
              }

map_node_ids :: NodeKind nk => (nid1 -> nid2) -> Node e nid1 nk -> Node e nid2 nk
map_node_ids map_id (Node nid nkind) = Node (map_id nid) (mapNkIds map_id nkind)

map_auto_ids :: (NodeKind nk, Ord nid1, Ord nid2) => (nid1 -> nid2) -> Automaton e nid1 nk -> Automaton e nid2 nk
map_auto_ids map_id a = a { auto_graph = new_graph, auto_start = new_start }
 where
    new_start = map_id (auto_start a)
    new_graph = Map.mapKeys map_id $ Map.map (map_node_ids map_id) $ auto_graph a


replace_done_with :: Ord nid => nid -> SAuto e nid -> SAuto e nid
replace_done_with nid a = map_auto_ids (\nid -> Map.findWithDefault nid nid replace_map) a
  where
    replace_map = Map.foldr fold_f Map.empty (auto_graph a)
    fold_f (Node nid' (SDone {})) mp = Map.insert nid' nid mp
    fold_f _ mp = mp

-- debugging
auto_closed :: (NodeKind nk, Ord nid) => Automaton e nid nk -> Bool
auto_closed a = Map.foldrWithKey node_closed (isDefined $ auto_start a) (auto_graph a)
  where
    isDefined nid = Map.member nid (auto_graph a)
    node_closed nid (Node nid' nkind) closed = closed && nid==nid' &&
      foldl suc_closed (isDefined nid) (sucsOfNk nkind)
    suc_closed closed suc = closed && isDefined suc









{------------------------------------------------------------------------
  Automata Construction from Ziria Comps
------------------------------------------------------------------------}


data Channels = Channels { in_chan   :: Chan
                         , out_chan  :: Chan
                         , ctrl_chan :: Maybe Chan }


mkAutomaton :: forall a e. Atom e
            => DynFlags
            -> GS.Sym
            -> Channels  -- i/o/ctl channel
            -> AComp a ()
            -> SAuto e Int -- what to do next (continuation)
            -> IO (SAuto e Int)
mkAutomaton dfs sym chans comp k = go $ assert (auto_closed k) $ acomp_comp comp
  where
    loc = acomp_loc comp
    go :: AComp0 a () -> IO (SAuto e Int)
    go (ATake1 aid t) =
      let inp = [(1,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom aid (1,t)) (\_ -> idAtom aid t) (ctrl_chan chans)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (AEmit1 e) =
      let watom = expToWiredAtom e (Just (out_chan chans))
          nkind = SAtom watom (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (AReturn e) =
      let watom = expToWiredAtom e (ctrl_chan chans)
          nkind = SAtom watom (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ATakeEmit x e) =
      let watom = expToWiredAtom (substAExp x (in_chan chans) e) (Just (out_chan chans))
          nkind = SAtom watom (auto_start k) Map.empty
          a     = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ATakeReturn x e) =
      let watom = expToWiredAtom (substAExp x (in_chan chans) e) (ctrl_chan chans)
          nkind = SAtom watom (auto_start k) Map.empty
          a     = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ATakeN aid t n) =
      let inp  = [(n,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          outty = TArray (Literal n) t
          atom = maybe (discardAtom aid (n,t)) (\_ -> castAtom aid (n,t) (1,outty)) (ctrl_chan chans)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (AEmitN aid  t n x) =
      let inp = [(1, x)]
          outp = [(n, out_chan chans)]
          atom = castAtom aid (1, nameTyp x) (n,t)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ACast aid (n1,t1) (n2,t2)) =
      let inp  = [(n1, in_chan chans)]
          outp = [(n2, out_chan chans)]
          atom = castAtom aid (n1,t1) (n2,t2)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ABind mbx c1 c2) = do
      a2 <- mkAutomaton dfs sym chans c2 k
      a <- mkAutomaton dfs sym (chans { ctrl_chan = mbx }) c1 a2
      return $ assert (auto_closed a2) $ assert (auto_closed a) a

    go (APar par_info c1 t c2) = do
      pipe_ch <- freshNameDoc sym "par" loc t Mut (pipeName par_info c1 c2)
      let k1 = mkDoneAutomaton (in_chan chans) pipe_ch
      let k2 = mkDoneAutomaton pipe_ch (out_chan chans)
      a1 <- mkAutomaton dfs sym (chans {out_chan = pipe_ch}) c1 k1
      a2 <- mkAutomaton dfs sym (chans {in_chan = pipe_ch}) c2 k2
      return $ zipAutomata dfs par_info a1 a2 k

    go (ABranch x c1 c2) = do
      a1 <- mkAutomaton dfs sym chans c1 k
      a2 <- mkAutomaton dfs sym chans c2 (a1 { auto_start = auto_start k})
      let nkind = SBranch x (auto_start a1) (auto_start a2) False Map.empty
      let a = insert_prepend nkind a2
      return $ assert (auto_closed a1) $ assert (auto_closed a2) $ assert (auto_closed a) a

    go (ARepeatN n c) = do
      a <- trace (">>>>>>> WARNING >>>>>: Unfolding static looop " ++ show n ++ " times!") <$>
           applyN n (mkAutomaton dfs sym chans c) k
      return $ assert (auto_closed a) a
      where applyN 0 _ x = return x
            applyN n f x = do
              y <- applyN (n-1) f x
              f y

    go (ARepeat c) =
      case nodeKindOfId k (auto_start k) of
        SDone {} -> do
          a0 <- mkAutomaton dfs sym chans c k
          let nid = auto_start k
          let node = fromJust $ assert (Map.member (auto_start a0) (auto_graph a0)) $
                     Map.lookup (auto_start a0) (auto_graph a0)
          let nmap = Map.insert nid node $ Map.delete (auto_start a0) (auto_graph a0)
          let a = map_auto_ids (\id -> if id == (auto_start a0) then nid else id) $
                    a0 { auto_start = nid, auto_graph = nmap }
          return $ assert (auto_closed a0) $ assert (auto_closed a) a
        _ -> fail "Repeat should not have a continuation!"

    go (AWhile x c) = do
      let k' = insert_prepend (SDone Map.empty Nothing) k
      let nid = auto_start k'
      a0 <- mkAutomaton dfs sym chans c k'
      let nkind = SBranch x (auto_start a0) (auto_start k) True Map.empty
      let a = a0 { auto_start = nid, auto_graph = insertNk nid nkind (auto_graph a0)}
      return $ assert (auto_closed a0) $ assert (auto_closed a) a

    go (AUntil x c) = do
      let k' = insert_prepend (SDone Map.empty Nothing) k
      let nid = auto_start k'
      a0 <- mkAutomaton dfs sym chans c k'
      let nkind = SBranch x (auto_start k) (auto_start a0) True Map.empty
      let a = a0 { auto_graph = insertNk nid nkind (auto_graph a0)}
      return $ assert (auto_closed a0) $ assert (auto_closed a) a


    -- pipe name
    pipeName pinfo c1 c2 =
      List.intercalate (pipeSymbol pinfo) $
      map (extractName . PPR.render . ppr) [parLoc (Left ()) c1, parLoc (Right ()) c2]
    extractName = takeWhile (/= '.') . reverse . takeWhile (/= '\\') . takeWhile (/= '/') . reverse
    parLoc side c
      | APar _ _cl _ cr <- acomp_comp c
      , Left c0 <- side = parLoc side cr
      | APar _ cl _ _cr <- acomp_comp c
      , Right () <- side = parLoc side cl
      | otherwise = acomp_loc c
    pipeSymbol pinfo | NeverPipeline <- plInfo pinfo = ".>>>."
                     | otherwise = ">>>"







{------------------------------------------------------------------------
  Automata Zipping (aka static scheduling)
------------------------------------------------------------------------}

-- Represents the distances of a particular automaton state to the done node(s).
-- Since there may be several (possibly infinitely many) paths to the done node(s),
-- this is not a single distance, but rather a set of distances.
-- Since we cannot compute inifinite sets, we allow truncating these sets by
-- omitting all distances above a given threshold.
data DoneDist
  = DoneDist { dists           :: Set Int -- distances of node to done node(s).
             , dists_truncated :: Bool    -- True if the set is incomplete, False otherwise
             }
  deriving Eq


data DistUpdate = Truncation | NewDist Int

-- Compute "done distances" of automaton states (measured in # of reads from input-queue upto Done)
-- Distances above the threshold are not computed (to ensure termination);
-- however, if such uncomputed distances exists, we record this fact in the DoneDist data structure
-- (by setting dists_truncated = True).
--
-- For efficieny reasons, should be cached locally:
--   let doneDist = mkDoneDist treshold a in ...
mkDoneDist :: forall nid e. Ord nid => Int -> SAuto e nid -> nid -> DoneDist
mkDoneDist threshold a = (\nid -> fromJust $ assert (Map.member nid $ auto_graph a) $ Map.lookup nid dist_map) where

  dist_map = go dones init

  -- we compute the done-distances in a backwards-fashion, starting from the Done node(s).
  dones = map (,NewDist 0) $
          filter (\nid -> case nodeKindOfId a nid of { SDone {} -> True; _ -> False }) $
          Map.keys (auto_graph a)

  -- Inital distance-map.
  init = Map.map (const $ DoneDist Set.empty $ if null dones then True else False) (auto_graph a)

  -- Iteratively updates distance map through backwards-propagation of distance updates.
  -- Terminates since only true updates are propagated, and because distances above the
  -- given threshold are not propagated.
  go :: [(nid, DistUpdate)] -> Map nid DoneDist -> Map nid DoneDist
  go [] dist_map = dist_map
  go ((nid,update):work_list) dist_map
    -- true distance update
    | NewDist n <- update
    , Set.notMember n $ dists $ fromJust $ assert (Map.member nid dist_map) $ Map.lookup nid dist_map
    = let dist_map' = Map.adjust (\d -> d { dists = Set.insert n (dists d) }) nid dist_map in
      let new_work = map (mkUpdate n) (preds nid) in
      go (new_work ++ work_list) dist_map'

    -- true truncation update
    | Truncation <- update
    , not $ dists_truncated $ fromJust $ assert (Map.member nid dist_map) $  Map.lookup nid dist_map
    = let dist_map' = Map.adjust (\d -> d { dists_truncated = True }) nid dist_map in
      let new_work = map (,Truncation) (preds nid) in
      go (new_work ++ work_list) dist_map'

    -- all other updates have no effect and are therefore not back-propagated.
    | otherwise
    = go work_list dist_map

  preds = Set.toList . mkPreds a

  mkUpdate dist nid = (nid,) $
    let new_dist = dist + cost nid in
    if new_dist > threshold then Truncation else NewDist new_dist

  cost nid
    | SAtom watom _ _ <- nodeKindOfId a nid = atomCost (auto_inchan a) watom
    | otherwise                             = 0



-- Calculates the lowest upper bound on the cost (measured in reads) to execute n more non-trivial atoms,
-- starting from a gived node nid. An atom is considered non-trivial if it consumes resources.
-- The algorithm requires O(n * |a|^2) iterations.
--
-- For efficieny reasons, should be cached locally:
--   let executionCost = mkExecutionCost n a in ...
mkExecutionCost :: forall e nid. Show nid => Ord nid => Int -> SAuto e nid -> nid -> Int
mkExecutionCost n a = (\nid -> fromJust $ assert (Map.member nid $ auto_graph a) $ Map.lookup nid cost_map) where
  cost_map = iterate incr_cost_map (Map.map (const 0) (auto_graph a)) !! n

  incr_cost_map :: Map nid Int -> Map nid Int
  incr_cost_map cmap =
    foldl spreadUpdates (foldl (incr cmap) cmap non_trivial) trivial

  incr :: Map nid Int -> Map nid Int -> SNode e nid -> Map nid Int
  incr previous_cmap cmap (Node nid (SAtom wa next _))
    | let own_cost = atomCost (auto_inchan a) wa
    , own_cost > 0
    = let new_cost = own_cost + fromJust (Map.lookup next previous_cmap)
      in Map.insert nid new_cost cmap
  incr _ _ _ = panicStr "mkExecutionCost: unreachable code!"

  spreadUpdates :: Map nid Int -> SNode e nid -> Map nid Int
  spreadUpdates cmap (Node nid nk)
    | let new_cost = maximum $ 0 : [ Map.findWithDefault 0 suc cmap | suc <- sucsOfNk nk ]
    , new_cost > (fromJust $ Map.lookup nid cmap)
    = foldl spreadUpdates (Map.insert nid new_cost cmap)
        [ fromJust $ Map.lookup pred (auto_graph a) | pred <- Set.toList (preds nid) ]
    | otherwise = cmap

  preds = mkPreds a

  (non_trivial, trivial) = List.partition (is_consuming . node_kind) $ Map.elems (auto_graph a)
    where is_consuming (SAtom wa _ _) = atomCost (auto_inchan a) wa > 0
          is_consuming _ = False



-- simple queue interface for lists
type Queue e = [e]
type Stack e = [e]

push :: e -> Queue e -> Queue e
push x q = q ++ [x]

pushFront :: e -> Queue e -> Queue e
pushFront = (:)

pop :: Queue e -> Maybe (e, Queue e)
pop [] = Nothing
pop (e:es) = Just (e, es)

peek :: Queue e -> Maybe e
peek q = fst <$> pop q

popN :: Int -> Queue a -> Maybe ([a], Queue a)
popN = popN' [] where
  popN' acc 0 q  = Just (reverse acc, q)
  popN' acc n q | Just (a,q') <- pop q = popN' (a:acc) (n-1) q'
  popN' _ _ _ = Nothing




-- The zipping algorithm creates a product automaton. Each "product state" consists
-- of the state of the left automaton, the state of the right automaton, plus some
-- extra state that contains information about the queue (or pipe) between the
-- left and the right automaton.
type ProdNid = (Int, Int, ExtraState)

-- numer of elements in the queue and rollback-behavior
type ExtraState = (Int, RollbackBehavior)
type RollbackBehavior = Map Int Int


-- During construction of the product automaton, we keep track of the production/consumption
-- behavior of the left automaton ("PipeState"). This data structure allows us to calculate
-- both the current number of elements in the pipe, and the current number of "uncommited"
-- (i.e. optimistic) reads of the left automaton.
data PipeState
  = PipeState { pipe_state :: Queue (Either Int Int) -- left n means n reads, right m means m writes
              , pipe_history :: Stack (Either Int Int)
              }

empty_pipe_state :: PipeState
empty_pipe_state = PipeState [] []

getUncommitted = sum . lefts . pipe_state
getBalance = sum . rights . pipe_state


-- try consuming n elements from the transfer-pipe.
try_consume :: Int -> PipeState -> Maybe PipeState
try_consume n ps | n < 0 = trace "!! try_consume: Called with negative argument!" $
                           trace "!!              Performing rollback" $
                           Just (rollback (-n) ps)
try_consume 0 ps = Just ps
try_consume n (PipeState q h) =
  case pop q of
    Nothing              -> Nothing
    Just (x@(Left _),q)  -> try_consume n (PipeState q (x `pushFront` h))
    Just (x@(Right m),q)
      | m<=n             -> try_consume (n-m) (PipeState q (x `pushFront` h))
      | otherwise        -> Just $ PipeState (Right (m-n) `pushFront` q) (Right n `pushFront` h)

rollback n (PipeState q h) =
  case try_consume n (PipeState h q) of
    Just (PipeState h' q') -> PipeState q' h'
    Nothing -> panicStr "rollback: Bug in Automata Model!!"

rollbackInDistance n ps = case try_consume n ps of
  Nothing -> 0
  Just ps' -> getUncommitted ps'




-- Precondition: a1 and a2 should satisfy (auto_outchan a1) == (auto_inchan a2)
-- a1 and a2 MUST NOT contain explicit loop nodes (but may contain loops)!!
{- NOTE: In the presence of rollbacks, the consumer a2 may also produce, i.e. write
   to the pipe/middle queue connecting a1 and a2.
   (This breaks an earlier invariant).
-}
zipAutomata :: forall e. Atom e
            => DynFlags
            -> ParInfo
            -> SAuto e Int -- left automaton ("producer")
            -> SAuto e Int -- right automaton ("consumer")
            -> SAuto e Int -- continuation
            -> SAuto e Int
zipAutomata dfs pinfo a1' a2' k = concat_auto prod_a k
  where
    a1 = deleteDeadNodes a1'
    a2 = deleteDeadNodes a2'
    prod_a = (\a -> assert (auto_closed a) a) $
             assert (auto_closed a1) $
             assert (auto_closed a2) $
             (if prune then pruneUnfinished else clearUnfinished) $
             (if lazy then id else insertRollbacks lazy a1) $
             normalize_auto_ids 0 $
             Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) start_prod_nid
    start_prod_nid = mkProdNid empty_pipe_state (auto_start a1) (auto_start a2)
    prod_nmap = zipNodes empty_pipe_state start_prod_nid Map.empty

    -- paramters
    pipe_ch = assert (auto_outchan a1 == auto_inchan a2) $ auto_outchan a1
    optimism = case plInfo pinfo of { AlwaysPipeline {} -> getOptimism dfs; _ -> 0 }
    lazy = optimism == 0
    prune = isDynFlagSet dfs PruneIncompleteStates

    mkProdNid :: PipeState -> Int -> Int -> ProdNid
    mkProdNid ps nid1 nid2 = (nid1,nid2,) $
      let budget = getBalance ps
          rollback_behavior | lazy = Map.empty
                            | otherwise = Map.fromList $ map (\d -> (d, rollbackInDistance d ps)) $
                                          filter (<= budget) $ Set.toList $ dists $ done_dist nid2
      in (budget, rollback_behavior)



    zipNodes :: PipeState -> ProdNid -> SNodeMap e ProdNid -> SNodeMap e ProdNid
    zipNodes pipe_state prod_nid@(nid1,nid2,_) prod_nmap =
      case Map.lookup prod_nid prod_nmap of
        Just _ -> prod_nmap -- We have already seen this product location. We're done!
        Nothing ->
          let nk1 = assert (Map.member nid1 (auto_graph a1)) $ nodeKindOfId a1 nid1
              nk2 = assert (Map.member nid2 (auto_graph a2)) $ nodeKindOfId a2 nid2
              noDups = const (assert False)
              new_balances = Map.insertWith noDups pipe_ch (getBalance pipe_state) $
                             Map.unionWith noDups (pipe_balances nk1) (pipe_balances nk2)
          in zipNodes' pipe_state new_balances prod_nid nk1 nk2 prod_nmap


    -- Zips automata in accordance with the tick/proc-based standard Ziria semantics
    -- (i.e. lazy, right-biased, pull-style semantics).
    zipNodes' :: PipeState -> Map Chan Int -> ProdNid -> SimplNk e Int -> SimplNk e Int -> SNodeMap e ProdNid -> SNodeMap e ProdNid

    zipNodes' pipe_state pipe_balances prod_nid nk1 (SDone {})
      | let rollback = rollbackInDistance 0 pipe_state
      , rollback > 0
      = assert (not lazy) $ insertNk prod_nid (SDone pipe_balances (Just rollback))
      | otherwise = insertNk prod_nid (SDone pipe_balances Nothing)

    zipNodes' pipe_state pipe_balances prod_nid@(id1,_,_) nk1 (SBranch x l r w _) =
      let prod_nid_l = mkProdNid pipe_state id1 l
          prod_nid_r = mkProdNid pipe_state id1 r
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipe_balances
      in zipNodes pipe_state prod_nid_r . zipNodes pipe_state prod_nid_l . insertNk prod_nid prod_nkind

    {- Treating this case here is not strictly in accordance with Ziria's lazy (right-biased)
       semantics; however, since branching has no side effects, this should be a legal reordering.
       We prefer handling branches first, as it will potentially allow us to merge several
       decision nodes into a single decision.
    -}
    zipNodes' pipe_state pipe_balances prod_nid@(_,id2,_) (SBranch x l r w _) nk2 =
      let prod_nid_l = mkProdNid pipe_state l id2
          prod_nid_r = mkProdNid pipe_state r id2
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipe_balances
      in zipNodes pipe_state prod_nid_r . zipNodes pipe_state prod_nid_l . insertNk prod_nid prod_nkind

    zipNodes' pipe_state pipe_balances prod_nid@(id1,id2,_) nk1 (SAtom wa2 next2 _)
      | Just pipe_state' <- exec_right wa2 pipe_state
      , (lazy || isDoneNk nk1 || getBalance pipe_state >= threshold id2)
      = let next_prod_nid = mkProdNid pipe_state' id1 next2
            prod_nkind = SAtom wa2 next_prod_nid pipe_balances
        in zipNodes pipe_state' next_prod_nid . insertNk prod_nid prod_nkind

    -- Can't proceed in right automaton -- execute left automaton

    zipNodes' pipe_state pipe_balances prod_nid@(_,id2,_) (SAtom wa1 next1 _) nk2 =
      let pipe_state' = exec_left wa1 pipe_state
          next_prod_nid = mkProdNid pipe_state' next1 id2
          prod_nkind = SAtom wa1 next_prod_nid pipe_balances
      in zipNodes pipe_state' next_prod_nid . insertNk prod_nid prod_nkind

     -- if we reach this case, the pipeline is already drained as far as possible
    zipNodes' pipe_state pipe_balances prod_nid (SDone {}) _nk2 =
      insertNk prod_nid (SDone pipe_balances Nothing)



    {-- threshold for optimistic execution --}

    threshold nid_right
      -- if automaton is know to halt soon, don't be stupidly optimistic!
      | DoneDist dists False <- done_dist nid_right
      = minimum (executionCost nid_right : Set.toList dists)
      | otherwise
      = executionCost nid_right

    executionCost = mkExecutionCost (optimism+1) a2
    done_dist = mkDoneDist max_q_size a2
    max_q_size = largest_write + maximum (0:[ executionCost nid | nid <- Map.keys (auto_graph a2) ]) - 1
    largest_write = maximum $ 0 : [ writes (node_kind n) | n <- Map.elems (auto_graph a1) ]
      where writes (SAtom wa _ _) = countWrites pipe_ch wa
            writes _ = 0



    -- Try executing a wired atom from the right automaton.
    -- This will fail if there is not enough data in the pipe, or succeed and
    -- change the pipe state otherwise.
    exec_right :: WiredAtom e -> PipeState -> Maybe PipeState
    exec_right wa ps
      | Just (n,ch) <- isRollbackWAtom wa
      , ch == pipe_ch
      = Just (rollback n ps) -- rollback must always succeed!
      | otherwise = try_consume (countReads pipe_ch wa) ps

    -- Execute a wired atom from the left automaton, and update the pipe state
    -- to track the atom's consumption/production behavior.
    exec_left :: WiredAtom e -> PipeState -> PipeState
    exec_left wa (PipeState q h) = PipeState q' h
      where
        q' = (if production > 0 then push (Right production) else id) $
             (if consumption > 0 then push (Left consumption) else id) $ q
        production = countWrites pipe_ch wa
        consumption = atomCost (auto_inchan a1) wa








{------------------------------------------------------------------------
  Automaton Normalization, Transformation, Translation
------------------------------------------------------------------------}


-- replaces arbitrary automata node-ids with Ints >= first_id
normalize_auto_ids :: (NodeKind nk, Atom e, Ord nid) => Int -> Automaton e nid nk -> Automaton e Int nk
normalize_auto_ids first_id a = map_auto_ids map_id a
  where
    map_id nid = fromJust $ assert (Map.member nid normalize_map) $ Map.lookup nid normalize_map
    (_, normalize_map) = Map.foldrWithKey f (first_id, Map.empty) (auto_graph a)
    f nid _ (counter, nid_map) = (counter+1, Map.insert nid counter nid_map)

deleteDeadNodes :: (NodeKind nk, Ord nid) => Automaton e nid nk -> Automaton e nid nk
deleteDeadNodes auto = auto { auto_graph = insertRecursively Map.empty (auto_start auto)}
  where
    insertRecursively nmap nid
      | Map.member nid nmap = nmap
      | otherwise =
          case Map.lookup nid (auto_graph auto) of
            Nothing -> panicStr "deleteDeadNodes: input graph is not closed!"
            Just node -> List.foldl insertRecursively (Map.insert nid node nmap) (sucs node)


markSelfLoops :: CfgAuto e Int -> CfgAuto e Int
markSelfLoops a = a { auto_graph = go (auto_graph a)}
  where go nmap = Map.foldr markNode nmap nmap
        markNode (Node nid nk@(CfgAction _ next _)) nmap
          = if nid /= next then nmap else
              let nid' = nextNid a
              in Map.insert nid (Node nid (CfgLoop nid')) $ Map.insert nid' (Node nid' nk) $ nmap
        markNode _ nmap = nmap


-- prune action that are known to be unreachable
pruneUnreachable :: forall e nid. (Atom e, Ord nid) => SAuto e nid -> nid -> SAuto e nid
pruneUnreachable a nid = fixup $ a { auto_graph = prune (auto_graph a) nid }
  where
    preds = mkPreds a

    prune :: SNodeMap e nid -> nid -> SNodeMap e nid
    prune nmap nid =
      case Map.lookup nid nmap of
        Nothing -> nmap -- already pruned
        Just _ -> Set.foldl (pruneBkw nid) (Map.delete nid nmap) (preds nid)

    pruneBkw :: nid -> SNodeMap e nid -> nid -> SNodeMap e nid
    pruneBkw suc nmap nid =
      case Map.lookup nid nmap of
        Nothing -> nmap
        Just (Node _ (SDone {})) -> panicStr "pruneBkw: impossible predecessor!"
        Just (Node _ (SAtom _ next _)) -> if next==suc then prune nmap nid else nmap
        Just (Node _ (SBranch x suc1 suc2 _ pipes))
          | suc == suc1 -> -- suc2 becomes the unique sucessor (since suc1 is unreachable)
            let nk = SAtom (assertWAtom False x) suc2 pipes
            in Map.insert nid (Node nid nk) nmap
          | suc == suc2 -> -- suc1 becomes the unique sucessor (since suc2 is unreachable)
            let nk = SAtom (assertWAtom True x) suc1 pipes
            in Map.insert nid (Node nid nk) nmap
          | otherwise -> panicStr "pruneBkw: impossible predecessor!"

    fixup a =
      let s = auto_start a in
      let nmap = auto_graph a in
      if Map.member s nmap then a
      else a { auto_graph = Map.insert s (Node s $ SDone Map.empty Nothing) nmap }


-- Prune all nodes that terminate with data still in the pipe.
pruneUnfinished :: forall e nid. (Atom e, Ord nid) => SAuto e nid -> SAuto e nid
pruneUnfinished a = foldl pruneUnreachable a (map node_id unfinished) where
  unfinished = filter isUnfinished $ filter (isDonePred a) $ Map.elems $ auto_graph a
  isUnfinished = any (>0) . Map.elems . next_pipe_balances . node_kind

-- Insert clear-atoms to clear all queues upon automaton-termination.In the process,
-- replaces all done states with a single done state (in which all pipes are empty).
-- NOTE: There is no danger of clearing the rollback channel, since it is not in scope
-- (because it is the input channel).
clearUnfinished :: forall e. Atom e => SAuto e Int -> SAuto e Int
clearUnfinished a = fixup $  Map.foldl clear (nmap, []) nmap
  where
    nmap = auto_graph a
    done_nid = nextNid a
    done_nk = SDone Map.empty Nothing -- an empty map is equivalent to a map with range = {0}

    clear :: (SNodeMap e Int, [Int]) -> SNode e Int -> (SNodeMap e Int, [Int])
    clear (nmap, done_ids) (Node nid (SDone pipes rollback))
      | Just _ <- rollback = panicStr "clear: bug in implementation: did not insert rollback!"
      | all (==0) (Map.elems pipes) = (nmap, nid:done_ids)
      | otherwise = (insertNk nid clear_nk nmap, done_ids)
      where clear_nk = SAtom (clearWAtom pipes) done_nid pipes
    clear acc _ = acc

    fixup :: (SNodeMap e Int, [Int]) -> SAuto e Int
    fixup (nmap', done_ids) = map_auto_ids map_nid $ a { auto_graph = nmap }
      where nmap = insertNk done_nid done_nk nmap'
            map_nid nid | nid `elem` done_ids = done_nid
                        | otherwise           = nid

insertRollbacks :: forall e. Atom e => Bool -> SAuto e Int -> SAuto e Int -> SAuto e Int
insertRollbacks lazy rollback_a a = a { auto_graph = Map.foldl go nmap nmap } where
  nmap = auto_graph a

  go :: SNodeMap e Int -> SNode e Int -> SNodeMap e Int
  go nmap (Node nid (SDone pipes rollback))
    | Nothing <- rollback = nmap
    | Just 0 <- rollback = insertNk nid (SDone pipes Nothing) nmap
    | Just n <- rollback, n>0, not lazy =
      let rollback_ch = auto_inchan rollback_a
          rollback_nk = SAtom (rollbackWAtom rollback_ch n) clear_nid pipes
          clear_ch = auto_outchan rollback_a
          clear_nid = fst (Map.findMax nmap) + 1
          clear_nk = SAtom (clearWAtom $ Map.filterWithKey (\ch _ -> ch == clear_ch) pipes) final_nid $
                     Map.insert clear_ch 0 pipes
          final_nid = fst (Map.findMax nmap) + 2
          -- the pipe balances need not be adjusted, since the rollback channel
          -- is not in scope (because it is the input channel!)
          final_nk = SDone pipes Nothing
      in insertNk nid rollback_nk $ insertNk clear_nid clear_nk $ insertNk final_nid final_nk $ nmap
    | otherwise = panicStr "insertRollbacks: impossible case!"
  go nmap _ = nmap






-- We maintain two sets: active, and done
-- Inchiant: every node starts as inactive and not done,
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
fuseActions :: forall atom nid. Ord nid => DynFlags -> CfgAuto atom nid -> CfgAuto atom nid
fuseActions dfs auto = auto { auto_graph = fused_graph }
  where
    fused_graph = fst $ runState (doAll [auto_start auto] (auto_graph auto)) (Set.empty, Set.empty)

    doAll :: [nid] -> CfgNodeMap atom nid -> MarkingM nid (CfgNodeMap atom nid)
    doAll [] nmap = return nmap
    doAll (nid:work_list) nmap = do
      (wl',nmap') <- markAndFuse nid nmap
      inNewFrame (doAll (wl'++work_list) nmap')


   -- Invariant: if isDone nid then all
   -- its successors either satisfy isDone or are in the worklist, and the node is
   --  (a) a decision node (i.e. not an action node), or
    -- (b) an action node with its next node being a decision node
    markAndFuse :: nid -> CfgNodeMap atom nid -> MarkingM nid ([nid], CfgNodeMap atom nid)
    markAndFuse nid nmap = do
      done <- isDone nid
      if done
        then return ([],nmap)
        else pushActive nid $ fuse (fromJust $ assert (Map.member nid nmap) $ Map.lookup nid nmap) nmap


    -- precondition: input node is marked active
    fuse :: CfgNode atom nid -> CfgNodeMap atom nid -> MarkingM nid ([nid], CfgNodeMap atom nid)
    fuse (Node _ CfgDone) nmap = return ([],nmap)
    fuse (Node _ (CfgLoop b)) nmap = return ([b],nmap)
    fuse (Node _ (CfgBranch _ b1 b2 _)) nmap = return ([b1,b2],nmap)

    fuse (Node nid (CfgAction atoms next pipes)) nmap = do
      active <- isActive next
      -- don't fuse back-edges (including self-loops)!
      if active then return ([],nmap) else do
        -- fuse sucessor node(s) first, ...
        (wl,nmap) <- markAndFuse next nmap
        -- ... then perform merger if possible
        return $ case fromJust $ assert (Map.member next nmap) $ Map.lookup next nmap of
          -- Don't fuse shared nodes in order to avoid code duplication.
          -- TODO: we may want to allow more aggressive fusing with a compiler flag,
          -- as it can lead to better performance under parallel execution (since state
          -- transitions require synchronization and are hence expensive).
          Node _ (CfgAction atoms' next' pipes') | Set.size (preds next) <= 1 || isDynFlagSet dfs FuseAggressively ->
            let pipes'' = Map.unionWith (curry fst) pipes pipes' in
            let node = Node nid (CfgAction (atoms++atoms') next' pipes'') in
            (wl, Map.insert nid node nmap)
          Node _ _ -> (wl,nmap)

    preds = mkPreds auto







{------------------------------------------------------------------------
  Building constraint graph from Control flow graph
------------------------------------------------------------------------}

cfgToAtomix :: forall e.CfgAuto e Int -> AxAuto e Int
cfgToAtomix a = a { auto_graph = state_graph} where
  state_graph = fst $ runState (fromCfg (auto_start a) Map.empty) Set.empty

  fromCfg :: Int -> AxNodeMap e Int -> State (Set Int) (AxNodeMap e Int)
  fromCfg nid nmap = do
    done <- gets (Set.member nid)
    modify (Set.insert nid)
    if done
      then return nmap
      else fromCfg' nid Map.empty [] (nodeKindOfId a nid) nmap

  fromCfg' :: Int -> Map Chan Int -> [WiredAtom e] -> CfgNk e Int -> 
              AxNodeMap e Int -> State (Set Int) (AxNodeMap e Int)

  fromCfg' nid pipes watoms (CfgAction was nxt pipes') nmap =
    -- left-biased union!
    fromCfg' nid (Map.union pipes pipes') (watoms++was) (nodeKindOfId a nxt) nmap

  fromCfg' nid pipes watoms CfgDone nmap =
    let nk = AtomixState watoms (mk_constraints pipes watoms Nothing) AtomixDone in
    return $ insertNk nid nk nmap

  fromCfg' nid pipes watoms (CfgLoop next) nmap =
    let nk = AtomixState watoms (mk_constraints pipes watoms Nothing) (AtomixLoop next) in
    fromCfg next $ insertNk nid nk nmap

  fromCfg' nid pipes watoms (CfgBranch x left right is_while) nmap =
    let constrs = mk_constraints pipes watoms (Just x) in
    let nk = AtomixState watoms constrs (AtomixBranch x left right) in
    fromCfg left =<< fromCfg right (insertNk nid nk nmap)




-- queue dependency environment
type QDependencyEnv = Map Chan (Int,Queue Int, Int) -- last read/queue of active writes (of cardinality one) from/to channel plus last writer to this queue
-- variable dependency environment
type VDependencyEnv = Map Chan (Maybe Int, Maybe Int) -- last read/write
type PipeBalances = Map Chan Int

type Acc = (QDependencyEnv, VDependencyEnv, [(Int,Int,Dependency)])

mk_constraints :: forall e. Map Chan Int -> [WiredAtom e] -> Maybe Chan -> Map (Int,Int) [Dependency]
mk_constraints _ [] _ = Map.empty
mk_constraints pipes watoms mb_decision 
  = trans_reduction $ go_decision mb_decision $ foldl go_watom (qenv0, venv0, []) (zip watoms [0..]) where

    -- queue and variable dependency environments
    qenv0 :: QDependencyEnv
    qenv0 = Map.map (\n -> (-1, replicate n (-1), -1)) pipes
    venv0 :: VDependencyEnv
    venv0 = Map.empty

    go_watom :: Acc -> (WiredAtom e, Int) -> Acc
    go_watom acc (wa, idx) =
      let acc' = foldl (go_inwires idx) acc (wires_in wa) in
      foldl (go_outwires idx) acc' (wires_out wa)
    

    go_inwires :: Int -> Acc -> (Int, Chan) -> Acc

    -- reading from queue
    go_inwires idx (qenv, venv, constrs) (n,ch) | Just (last_r,last_wrs, last_w) <- Map.lookup ch qenv =
      let Just (producers, last_wrs') = popN n last_wrs in
      let qenv' = Map.insert ch (idx,last_wrs',last_w) qenv in
      let constrs' = (if last_r >= 0 then ((last_r,idx,CC):) else id) $
                     [(wr,idx,PC) | wr <- producers] ++ constrs in
      (qenv', venv, constrs')

    -- reading from variable
    go_inwires idx (qenv, venv, constrs) (1,ch) =
      let (mb_last_r,mb_last_wr) = Map.findWithDefault (Nothing,Nothing) ch venv in
      let venv' = Map.insert ch (Just idx, mb_last_wr) venv in
      let constrs' = maybe constrs (\last_wr -> (last_wr, idx, WR) : constrs) mb_last_wr in
      (qenv, venv', constrs')

    go_inwires _ _ _ = panicStr "mk_constraints: unexpected case"


    go_outwires :: Int -> Acc -> (Int,Chan) -> Acc

    -- writing to queue
    go_outwires idx (qenv, venv, constrs) (n,ch) | Just (last_r, last_wrs, last_w) <- Map.lookup ch qenv =
      let wr = maybe last_w id (peek last_wrs) in
             -- It is possible to have nothing in the queue (so peek last_wrs may be Nothing) but at some point
             -- in the past we already wrote to this queue (last_w) (and someone consumed the data before we reach here).

      let qenv' = Map.adjust (\(last_r,last_wrs,_last_w) -> (last_r, iterate (push idx) last_wrs !! n,idx)) ch qenv in
      let constrs' = (wr,idx,PP):constrs in
      (qenv', venv, constrs')

    -- writing to variable
    go_outwires idx (qenv, venv, constrs) (1,ch) =
      let (mb_last_r,mb_last_wr) = Map.findWithDefault (Nothing,Nothing) ch venv in
      let venv' = Map.insert ch (mb_last_r, Just idx) venv in
      let constrs' = [ (r,idx,RW) | Just r <- [mb_last_r]  ] ++
                     [ (w,idx,WW) | Just w <- [mb_last_wr] ] ++ constrs in
      (qenv, venv', constrs')

    go_outwires _ _ _ = panicStr "mk_constraints: unexpected case"


    go_decision :: Maybe Chan -> Acc -> Map (Int,Int) [Dependency]

    go_decision mb_decision (qenv, venv, constrs) =
      let dec_constr = maybeToList $ do
          dec <- mb_decision
          (_mb_last_r,mb_last_wr) <- Map.lookup dec venv
          last_wr <- mb_last_wr
          return (last_wr,length watoms,WR)
      in Map.fromList $
         map ( \group -> (fst $ head $ group, List.nub $ map snd $ group) ) $
         List.groupBy ((==) `on` fst) $
         map ( \(a,b,c) -> ((a,b),c) ) $
         dec_constr ++ constrs


-- Naive transitivity-reduction (i.e. removal of transitive edges)
trans_reduction :: Map (Int,Int) a -> Map (Int,Int) a
trans_reduction mp = foldl (flip Map.delete) mp redundant
  where
    redundant = [ (i,i) | i <- idxs ] ++
                [ (i,k) | i <- idxs
                        , j <- maybe [] Set.toList $ Map.lookup i closure
                        , k <- maybe [] Set.toList $ Map.lookup j closure ]

    idxs = List.nub $ map fst $ Map.keys mp

    fix f x = let x' = f x in if x==x' then x else fix f x'

    closure :: Map Int (Set Int)
    closure = rem_refl $ fix add_trans $
              Map.fromListWith Set.union $
              map ( \(i,j) -> (i, Set.singleton j) ) $
              Map.keys mp

    rem_refl :: Map Int (Set Int) -> Map Int (Set Int)
    rem_refl = Map.mapWithKey Set.delete

    add_trans :: Map Int (Set Int) -> Map Int (Set Int)
    add_trans g = Map.foldlWithKey (\g' i js -> Map.adjust (Set.union $ trans g js) i g') g g
    trans g js = Set.unions $ map (\j -> Map.findWithDefault Set.empty j g) $ Set.toList js






{------------------------------------------------------------------------
  Automaton to DOT file translation
------------------------------------------------------------------------}

dotOfAuto :: (Atom e, Show nid) => DynFlags -> Maybe (WiredAtom e -> String) -> CfgAuto e nid -> String
dotOfAuto dflags atomPrinter a = prefix ++ List.intercalate ";\n" (nodes ++ edges) ++ postfix
  where
    printAtoms = isDynFlagSet dflags PrintAtoms
    printPipeNames = isDynFlagSet dflags PrintPipeNames
    prefix = "digraph ziria_automaton {\n"
    postfix = ";\n}"
    nodes = ("node [shape = point]":start) ++
            ("node [shape = doublecircle]":final) ++
            ("node [shape = box]":decision) ++
            ("node [shape = box, fontname=monospace, fontsize=11, style=filled, fillcolor=\"white\"]":action)
    start = ["start [label=\"\"]"]
    (finalN,normalN) = List.partition (\(Node _ nk) -> case nk of { CfgDone -> True; _ -> False }) $ Map.elems (auto_graph a)
    (actionN,decisionN) = List.partition (\(Node _ nk) -> case nk of { CfgAction {} -> True; _ -> False }) normalN
    final = List.map (\(Node nid _) -> show nid ++ "[label=\"\"]") finalN
    action = List.map showNode actionN
    decision = List.map showNode decisionN
    edges = ("start -> " ++ show (auto_start a)) : (List.map edges_of_node normalN)
    edges_of_node node = List.intercalate "; " [edge (node_id node) suc | suc <- sucs node]
    edge nid1 nid2 = show nid1 ++ " -> " ++ show nid2

    showNode (Node nid nk) = "  " ++ show nid ++ "[label=\"" ++ showNk nk ++ "\"" ++ maybeToolTip nk ++ "]"

    showNk (CfgAction watoms _ pipes)
      | printAtoms = List.intercalate "\\n" (showPipes watoms pipes : showWatoms watoms)
      | otherwise = showPipes watoms pipes
    showNk (CfgBranch x _ _ True) = "WHILE<" ++ show x ++ ">"
    showNk (CfgBranch x _ _ False) = "IF<" ++ show x ++ ">"
    showNk CfgDone = "DONE"
    showNk (CfgLoop _) = "LOOP"

    showWatoms = map showWatomGroup . List.group
    showWatomGroup wa = case length wa of 1 -> showWatom (head wa)
                                          n -> show n ++ " TIMES DO " ++ showWatom (head wa)
    
    showWatom wa@(WiredAtom inw outw _) | Just showAtom <- atomPrinter 
      = showWires inw ++ showAtom wa ++ showWires outw where
          showWires ws = "{" ++ (List.intercalate "," $ map showWire ws) ++ "}"
          showWire (n,ch)
            | n==1      = showChan True ch
            | otherwise = showChan True ch ++ "^" ++ show n
    showWatom wa | otherwise = show wa
      

    showPipes watoms pipes
      | printPipeNames = render $ punctuateH top (text " | ") $ boxedPipes
      | otherwise     = List.intercalate "\\n" $ map printPipes [pipes, nextPipes watoms pipes]
        where
          boxedPipes = map boxPipe $ Map.toAscList $
                       Map.intersectionWith (,) pipes (nextPipes watoms pipes)
          boxPipe (pipe, state) = vcat center1 $ map text [show pipe, show state]
          printPipes = List.intercalate "|" . map printPipe . Map.toAscList
          printPipe (_pipe_ch, val) = show val

    maybeToolTip (CfgAction _ _ pipes) = " tooltip=\"" ++ showPipeNames pipes ++ "\""
    maybeToolTip _ = ""
    showPipeNames = List.intercalate " | " . map (showChan True) . Map.keys


dotOfAxAuto :: Atom e => DynFlags -> AxAuto e Int -> [(Int, String)]
dotOfAxAuto dflags a = map (\(nid,state) -> (nid, dotify state)) $ Map.toList $ auto_graph a where
  dotify (Node nid nk) = prefix ++ List.intercalate ";\n" (label nid : mk_state nk) ++ postfix 
  prefix = "digraph atomix_state {\n"
  label nid = "label=\"State " ++ show nid ++ "\""
  postfix = ";\n}"

  mk_state (AtomixState watoms constrs decision) 
    = ["node [shape = point]", "  -1 [label=\"\"]"] ++
      ("node [shape = box]" : map mk_watom (zip watoms [0..])) ++
      ("node [shape = cds]" : [mk_decision decision (length watoms)]) ++
      mk_edges constrs

  mk_watom (w@(WiredAtom inw outw atom),idx)
   | isDynFlagSet dflags CLikeNames
   = "  " ++ show idx ++ "[label=\"" ++ mk_atom (wiredAtomId w) inw outw atom ++ "\"]"
   | otherwise
   = "  " ++ show idx ++ "[label=\"" ++ mk_atom (show atom) inw outw atom ++ "\"]"
  mk_atom aid inw outw atom = List.intercalate "\\l" $ 
                               aid :
                               map ((" IN: " ++) . showChan True . snd) inw ++
                               map (("OUT: " ++) . showChan True . snd) outw ++ [""]
  
  mk_decision dec idx = "  " ++ show idx ++ "[label=\"" ++ show dec ++ "\"]"

  mk_edges constrs = map (uncurry mk_edge) (Map.toList constrs)
  mk_edge (id1,id2) lbls = show id1 ++ " -> " ++ show id2 ++ "[label=\"" ++ show lbls ++ "\"]"




{------------------------------------------------------------------------
  Top-level Pipeline
------------------------------------------------------------------------}

simplToCfg :: SAuto e nid -> CfgAuto e nid
simplToCfg a = a { auto_graph = Map.map nodeToCfg $ auto_graph a }
  where nodeToCfg (Node nid nkind) = Node nid (nkToCfg nkind)
        nkToCfg (SDone _ Nothing) = CfgDone
        nkToCfg (SDone _ (Just _)) = panicStr "simplToCfg: missing rollback encountered!"
        nkToCfg (SAtom wa nxt pipes) = CfgAction [wa] nxt pipes
        nkToCfg (SBranch x nxt1 nxt2 l _) = CfgBranch x nxt1 nxt2 l



automatonPipeline :: Atom e => DynFlags -> GS.Sym -> Ty -> Ty -> AComp () () -> IO (CfgAuto e Int)
automatonPipeline dfs sym inty outty acomp = do
  inch  <- freshName sym "src"  (acomp_loc acomp) inty Imm
  outch <- freshName sym "snk" (acomp_loc acomp) outty Mut
  let channels = Channels { in_chan = inch, out_chan = outch, ctrl_chan = Nothing }
  let k = mkDoneAutomaton inch outch

  putStrLn "\n>>>>>>>>>> mkAutomaton"
  a <- simplToCfg <$> mkAutomaton dfs sym channels acomp k
  --putStrLn (dotOfAuto True a)
  putStrLn $ "<<<<<<<<<<< mkAutomaton (" ++ show (size a) ++ " states)"

  putStrLn ">>>>>>>>>>> fuseActions"
  let a_f = fuseActions dfs a
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
