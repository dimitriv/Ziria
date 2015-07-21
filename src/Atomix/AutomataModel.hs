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

import Control.Exception
import Control.Monad.State

import AtomComp
import AstExpr
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


{-- Generic Atom Interfae --------------------------------------------}
class (Show a, Eq a) => Atom a where

  atomInTy  :: a -> [(Int,Ty)]
  atomOutTy :: a -> [(Int,Ty)]

  -- Constructors of atoms
  discardAtom :: (Int,Ty) -> a
  castAtom    :: (Int,Ty) -> (Int,Ty) -> a
  assertAtom :: Bool -> a
  rollbackAtom :: Chan -> Int -> a
  isRollbackAtom :: a -> Maybe (Int, Chan)
  clearAtom :: Map Chan Int -> a

  -- Getting (wired) atoms from expressions
  expToWiredAtom :: AExp () -> Maybe Chan -> WiredAtom a


  -- Default implementations based on this abstract interface
  idAtom      :: Ty -> a
  idAtom t = castAtom (1,t) (1,t)

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
          }
  deriving Show

instance NodeKind SimplNk where
  sucsOfNk (SDone _) = []
  sucsOfNk (SAtom _ nxt _) = [nxt]
  sucsOfNk (SBranch _ nxt1 nxt2 _ _) = [nxt1,nxt2]

  mapNkIds _ (SDone pipes) = SDone pipes
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

nextPipes :: [WiredAtom a] -> Map Chan Int -> Map Chan Int
nextPipes watoms pipes = Map.mapWithKey updatePipe pipes
  where updatePipe pipe n = n + sum (map (countWrites pipe) watoms)
                              - sum (map (countReads pipe) watoms)

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
  = Automaton { auto_graph = Map.singleton 0 (Node 0 $ SDone Map.empty), auto_start = 0
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
    fold_f (Node nid' (SDone _)) mp = Map.insert nid' nid mp
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
    go (ATake1 t) =
      let inp = [(1,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom (1,t)) (\_ -> idAtom t) (ctrl_chan chans)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ATakeN t n) =
      let inp  = [(n,in_chan chans)]
          outp = map (1,) $ maybeToList (ctrl_chan chans)
          outty = TArray (Literal n) t
          atom = maybe (discardAtom (n,t)) (\_ -> castAtom (n,t) (1,outty)) (ctrl_chan chans)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (AEmit1 e) =
      let watom = expToWiredAtom e (Just (out_chan chans))
          nkind = SAtom watom (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

{------ OLD:
      let inp = [(1, x)]
          outp = [(1, out_chan chans)]
          atom = idAtom (nameTyp x)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a
-----------}

    go (AEmitN t n x) =
      let inp = [(1, x)]
          outp = [(n, out_chan chans)]
          atom = castAtom (1, nameTyp x) (n,t)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

    go (ACast _ (n1,t1) (n2,t2)) =
      let inp  = [(n1, in_chan chans)]
          outp = [(n2, out_chan chans)]
          atom = castAtom (n1,t1) (n2,t2)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

--    go (MapOnce f closure) =
--      let args = in_chan chans : closure
--          expr = MkExp (ExpApp f args) noLoc ()
--          watom = expToWiredAtom expr (Just $ out_chan chans)
--          nkind = SAtom [watom] (auto_start k)
--      in return $ insert_prepend nkind k

    go (AReturn e) =
      let watom = expToWiredAtom e (ctrl_chan chans)
          nkind = SAtom watom (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a


    go (ABind mbx c1 c2) = do
      a2 <- mkAutomaton dfs sym chans c2 k
      a <- mkAutomaton dfs sym (chans { ctrl_chan = mbx }) c1 a2
      return $ assert (auto_closed a2) $ assert (auto_closed a) a

    go (APar _ c1 t c2) = do
      pipe_ch <- freshNameDoc sym "par" loc t Mut (pipeName c1 c2)
      let k1 = mkDoneAutomaton (in_chan chans) pipe_ch
      let k2 = mkDoneAutomaton pipe_ch (out_chan chans)
      a1 <- mkAutomaton dfs sym (chans {out_chan = pipe_ch}) c1 k1
      a2 <- mkAutomaton dfs sym (chans {in_chan = pipe_ch}) c2 k2
      return $ zipAutomata dfs a1 a2 k

    go (ABranch x c1 c2) = do
      a1 <- mkAutomaton dfs sym chans c1 k
      a2 <- mkAutomaton dfs sym chans c2 (a1 { auto_start = auto_start k})
      let nkind = SBranch x (auto_start a1) (auto_start a2) False Map.empty
      let a = insert_prepend nkind a2
      return $ assert (auto_closed a1) $ assert (auto_closed a2) $ assert (auto_closed a) a

    go (ARepeatN n c) = do
      a <- applyN n (mkAutomaton dfs sym chans c) k
      return $ assert (auto_closed a) a
      where applyN 0 _ x = return x
            applyN n f x = do
              y <- applyN (n-1) f x
              f y

    go (ARepeat c) =
      case nodeKindOfId k (auto_start k) of
        SDone _ -> do
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
      let k' = insert_prepend (SDone Map.empty) k
      let nid = auto_start k'
      a0 <- mkAutomaton dfs sym chans c k'
      let nkind = SBranch x (auto_start a0) (auto_start k) True Map.empty
      let a = a0 { auto_start = nid, auto_graph = insertNk nid nkind (auto_graph a0)}
      return $ assert (auto_closed a0) $ assert (auto_closed a) a

    go (AUntil x c) = do
      let k' = insert_prepend (SDone Map.empty) k
      let nid = auto_start k'
      a0 <- mkAutomaton dfs sym chans c k'
      let nkind = SBranch x (auto_start a0) (auto_start k) True Map.empty
      let a = a0 { auto_graph = insertNk nid nkind (auto_graph a0)}
      return $ assert (auto_closed a0) $ assert (auto_closed a) a


    -- pipe name
    pipeName c1 c2 = List.intercalate ">>>" $
                     map (extractName . PPR.render . ppr) [parLoc (Left ()) c1, parLoc (Right ()) c2]
    extractName = takeWhile (/= '.') . reverse . takeWhile (/= '\\') . takeWhile (/= '/') . reverse
    parLoc side c
      | APar _ _cl _ cr <- acomp_comp c
      , Left c0 <- side = parLoc side cr
      | APar _ cl _ _cr <- acomp_comp c
      , Right () <- side = parLoc side cl
      | otherwise = acomp_loc c







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
          filter (\nid -> case nodeKindOfId a nid of { SDone _ -> True; _ -> False }) $
          Map.keys (auto_graph a)

  -- Inital distance-map.
  init = Map.map (const $ DoneDist Set.empty False) (auto_graph a)

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
    | SAtom watom _ _ <- nodeKindOfId a nid = countReads (auto_inchan a) watom
                                            - countWrites (auto_inchan a) watom -- there may be rollbacks...
    | otherwise                             = 0


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
try_consume n ps | n < 0 = assert False undefined
try_consume 0 ps = Just ps
try_consume n (PipeState q h) =
  case pop q of
    Nothing              -> Nothing
    Just (x@(Left _),q)  -> try_consume n (PipeState q (x:h))
    Just (x@(Right m),q)
      | m<=n             -> try_consume (n-m) (PipeState q (x `pushFront` h))
      | otherwise        -> Just $ PipeState (Right (m-n) `pushFront` q) (Right n:h)

rollback n (PipeState q h) =
  case try_consume n (PipeState h q) of
    Just (PipeState h' q') -> PipeState q' h'
    Nothing -> panicStr "rollback: Bug in Automata Model!!"




-- Precondition: a1 and a2 should satisfy (auto_outchan a1) == (auto_inchan a2)
-- a1 and a2 MUST NOT contain explicit loop nodes (but may contain loops)!!
zipAutomata :: forall e. Atom e
            => DynFlags
            -> SAuto e Int -- left automaton ("producer")
            -- NOTE: In the presence of rollbacks, the consumer may also produce!
            -> SAuto e Int -- right automaton ("consumer")
            -> SAuto e Int -- continuation
            -> SAuto e Int
zipAutomata dfs a1' a2' k = concat_auto prod_a k
  where
    a1 = deleteDeadNodes a1'
    a2 = deleteDeadNodes a2'
    prod_a = (\a -> assert (auto_closed a) a) $
             assert (auto_closed a1) $
             assert (auto_closed a2) $
             (if prune then pruneUnfinished else clearUnfinished) $
             normalize_auto_ids 0 $
             Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) start_prod_nid
    start_prod_nid = mkProdNid empty_pipe_state (auto_start a1) (auto_start a2)
    prod_nmap = zipNodes empty_pipe_state start_prod_nid Map.empty
    pipe_ch = assert (auto_outchan a1 == auto_inchan a2) $ auto_outchan a1
    optimism :: Double
    optimism = 0.0
    lazy = optimism == 0.0
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
          in (if lazy then zipLazy else zipOptimistic) pipe_state new_balances prod_nid nk1 nk2 prod_nmap


    -- Zips automata in accordance with the tick/proc-based standard Ziria semantics
    -- (i.e. lazy, right-biased, pull-style semantics).
    zipLazy :: PipeState -> Map Chan Int -> ProdNid -> SimplNk e Int -> SimplNk e Int -> SNodeMap e ProdNid -> SNodeMap e ProdNid

    zipLazy pipe_state pipe_balances prod_nid nk1 (SDone _)
      = insertNk prod_nid (SDone pipe_balances)

    zipLazy pipe_state pipe_balances prod_nid@(id1,_,_) nk1 (SBranch x l r w _) =
      let prod_nid_l = mkProdNid pipe_state id1 l
          prod_nid_r = mkProdNid pipe_state id1 r
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipe_balances
      in zipNodes pipe_state prod_nid_r . zipNodes pipe_state prod_nid_l . insertNk prod_nid prod_nkind

    {- Treating this case here is not strictly in accordance with Ziria's lazy (right-biased)
       semantics; however, since branching has no side effects, this should be a legal reordering.
       We prefer handling branches first, as it will potentially allow us to merge several
       decision nodes into a single decision.
    -}
    zipLazy pipe_state pipe_balances prod_nid@(_,id2,_) (SBranch x l r w _) nk2 =
      let prod_nid_l = mkProdNid pipe_state l id2
          prod_nid_r = mkProdNid pipe_state r id2
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipe_balances
      in zipNodes pipe_state prod_nid_r . zipNodes pipe_state prod_nid_l . insertNk prod_nid prod_nkind

    zipLazy pipe_state pipe_balances prod_nid@(id1,id2,_) nk1 (SAtom wa2 next2 _)
      | Just pipe_state' <- exec_right wa2 pipe_state
      = let next_prod_nid = mkProdNid pipe_state' id1 next2
            prod_nkind = SAtom wa2 next_prod_nid pipe_balances
        in zipNodes pipe_state' next_prod_nid . insertNk prod_nid prod_nkind

    -- Can't proceed in right automaton -- execute left automaton

    zipLazy pipe_state pipe_balances prod_nid@(_,id2,_) (SAtom wa1 next1 _) nk2 =
      let pipe_state' = exec_left wa1 pipe_state
          next_prod_nid = mkProdNid pipe_state' next1 id2
          prod_nkind = SAtom wa1 next_prod_nid pipe_balances
      in zipNodes pipe_state' next_prod_nid . insertNk prod_nid prod_nkind

     -- if we reach this case, the pipeline is already drained as far as possible
    zipLazy pipe_state pipe_balances prod_nid nk1@(SDone {}) _nk2 =
      insertNk prod_nid (SDone pipe_balances)



    -- Zips automata optimistically, i.e. running the left automaton slightly ahead of the right automaton,
    -- in order to keep the pipeline filled up at all times so greater parallelism can be achieved.
    zipOptimistic :: PipeState -> Map Chan Int -> ProdNid -> SimplNk e Int -> SimplNk e Int -> SNodeMap e ProdNid -> SNodeMap e ProdNid



------------------------------------------------- OLD ------------------------------------------------------
    zipOptimistic pipe_state pipe_balances prod_nid nk1 (SDone _)
      | let rollback = rollbackInDistance 0 pipe_state
      , rollback > 0
      = -- Hack to create fresh product-node id
        let final_prod_nid = ( 1 + (fst $ Map.findMax $ auto_graph a1)
                             , 1 + (fst $ Map.findMax $ auto_graph a2)
                             , (0,Map.empty) )
            final_pipe_balances = Map.adjust (+ rollback) (auto_inchan a1) pipe_balances
            final_nk = SDone final_pipe_balances
            rollback_nk = SAtom (rollbackWAtom pipe_ch rollback) final_prod_nid pipe_balances
        in insertNk final_prod_nid final_nk . insertNk prod_nid rollback_nk

    zipOptimistic pipe_state pipe_balances prod_nid nk1 (SDone _) =
        insertNk prod_nid (SDone pipe_balances)

    zipOptimistic pipe_state pipe_balances prod_nid@(id1,_,_) nk1 (SBranch x l r w _) =
      let prod_nid_l = mkProdNid pipe_state id1 l
          prod_nid_r = mkProdNid pipe_state id1 r
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipe_balances
      in zipNodes pipe_state prod_nid_r . zipNodes pipe_state prod_nid_l . insertNk prod_nid prod_nkind

    {- Treating this case here is not strictly in accordance with Ziria's lazy (right-biased)
       semantics; however, since branching has no side effects, this should be a legal reordering.
       We prefer handling branches first, as it will potentially allow us to merge several
       decision nodes into a single decision.
    -}
    zipOptimistic pipe_state pipe_balances prod_nid@(_,id2,_) (SBranch x l r w _) nk2 =
      let prod_nid_l = mkProdNid pipe_state l id2
          prod_nid_r = mkProdNid pipe_state r id2
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipe_balances
      in zipNodes pipe_state prod_nid_r . zipNodes pipe_state prod_nid_l . insertNk prod_nid prod_nkind

    zipOptimistic pipe_state pipe_balances prod_nid@(id1,id2,_) nk1 (SAtom wa2 next2 _)
      | getBalance pipe_state >= threshold id2
      , Just pipe_state' <- exec_right wa2 pipe_state
      = let next_prod_nid = mkProdNid pipe_state' id1 next2
            prod_nkind = SAtom wa2 next_prod_nid pipe_balances
        in zipNodes pipe_state' next_prod_nid . insertNk prod_nid prod_nkind

    -- Can't proceed in right automaton -- execute left automaton

    zipOptimistic pipe_state pipe_balances prod_nid@(_,id2,_) (SAtom wa1 next1 _) nk2 =
      let pipe_state' = exec_left wa1 pipe_state
          next_prod_nid = mkProdNid pipe_state' next1 id2
          prod_nkind = SAtom wa1 next_prod_nid pipe_balances
      in zipNodes pipe_state' next_prod_nid . insertNk prod_nid prod_nkind

     -- if we reach this case, the pipeline is already drained as far as possible
    zipOptimistic pipe_state pipe_balances prod_nid (SDone _) nk2
      = insertNk prod_nid (SDone pipe_balances)


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
    -- to track the atoms consumption/production behavior.
    exec_left :: WiredAtom e -> PipeState -> PipeState
    exec_left wa (PipeState q h) = PipeState q' h
      where
        q' = (if writes > 0 then push (Right writes) else id) $
             (if reads > 0 then push (Left reads) else id) $ q
        writes = countWrites pipe_ch wa
        reads = countReads (auto_inchan a1) wa




    largest_write = maximum $ map (getSize . node_kind) $ Map.elems (auto_graph a1)
      where getSize (SAtom wa _ _) = countWrites pipe_ch wa
            getSize _ = 0

    largest_read = maximum $ map (getSize . node_kind) $ Map.elems (auto_graph a2)
      where getSize (SAtom wa _ _) = countReads pipe_ch wa
            getSize _ = 0

    -- we won't consume unless the pipe contains >= elements than indicated
    -- by this threshold
    consumption_threshold = ceiling ((1 + optimism) * (fromIntegral largest_read))

    threshold nid2
      | dists_truncated $ done_dist nid2 = consumption_threshold
      | otherwise =
        let ds = dists (done_dist nid2) in
        let d = if Set.null ds then 0 else Set.findMax ds in
        d - largest_write + 1

    max_q_size :: Int
    max_q_size = largest_write + consumption_threshold

    done_dist = mkDoneDist max_q_size a2

    rollbackInDistance n ps = case try_consume n ps of
      Nothing -> 0
      Just ps' -> getUncommitted ps'






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
        Just (Node _ (SDone {})) -> assert False undefined
        Just (Node _ (SAtom _ next _)) -> if next==suc then prune nmap nid else nmap
        Just (Node _ (SBranch x suc1 suc2 _ pipes))
          | suc == suc1 -> -- suc2 becomes the unique sucessor (since suc1 is unreachable)
            let nk = SAtom (assertWAtom False x) suc2 pipes
            in Map.insert nid (Node nid nk) nmap
          | suc == suc2 -> -- suc1 becomes the unique sucessor (since suc2 is unreachable)
            let nk = SAtom (assertWAtom True x) suc1 pipes
            in Map.insert nid (Node nid nk) nmap
          | otherwise -> assert False undefined

    fixup a =
      let s = auto_start a in
      let nmap = auto_graph a in
      if Map.member s nmap then a
      else a { auto_graph = Map.insert s (Node s $ SDone Map.empty) nmap }


-- Prune all nodes that terminate with data still in the pipe.
pruneUnfinished :: forall e nid. (Atom e, Ord nid) => SAuto e nid -> SAuto e nid
pruneUnfinished a = foldl pruneUnreachable a (map node_id unfinished) where
  unfinished = filter isUnfinished $ filter (isDonePred a) $ Map.elems $ auto_graph a
  isUnfinished = any (>0) . Map.elems . next_pipe_balances . node_kind

-- Insert clear-atoms to clear all queues upon automaton-termination. In the process,
-- replaces all done states with a single done state (in which all pipes are empty).
clearUnfinished :: forall e. Atom e => SAuto e Int -> SAuto e Int
clearUnfinished a = fixup $  Map.foldl clear (nmap, []) nmap
  where
    nmap = auto_graph a
    done_nid = nextNid a
    done_nk = SDone Map.empty -- an empty map is equivalent to a map with range = {0}

    clear :: (SNodeMap e Int, [Int]) -> SNode e Int -> (SNodeMap e Int, [Int])
    clear (nmap, done_ids) (Node nid (SDone pipes))
      | all (==0) (Map.elems pipes) = (nmap, nid:done_ids)
      | otherwise = (insertNk nid clear_nk nmap, done_ids)
      where clear_nk = SAtom (clearWAtom pipes) done_nid pipes
    clear acc _ = acc

    fixup :: (SNodeMap e Int, [Int]) -> SAuto e Int
    fixup (nmap', done_ids) = map_auto_ids map_nid $ a { auto_graph = nmap }
      where nmap = insertNk done_nid done_nk nmap'
            map_nid nid | nid `elem` done_ids = done_nid
                        | otherwise           = nid




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
fuseActions :: forall atom nid. Ord nid => CfgAuto atom nid -> CfgAuto atom nid
fuseActions auto = auto { auto_graph = fused_graph }
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
          Node _ (CfgAction atoms' next' pipes') ->
            let pipes'' = Map.unionWith (curry fst) pipes pipes' in
            let node = Node nid (CfgAction (atoms++atoms') next' pipes'') in
            (wl, Map.insert nid node nmap)
          Node _ _ -> (wl,nmap)





{------------------------------------------------------------------------
  Automaton to DOT file translation
------------------------------------------------------------------------}

dotOfAuto :: (Atom e, Show nid) => DynFlags -> CfgAuto e nid -> String
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
      | printActions = List.intercalate "\\n" (showPipes watoms pipes : showWatoms watoms)
      | otherwise = showPipes watoms pipes
    showNk (CfgBranch x _ _ True) = "WHILE<" ++ show x ++ ">"
    showNk (CfgBranch x _ _ False) = "IF<" ++ show x ++ ">"
    showNk CfgDone = "DONE"
    showNk (CfgLoop _) = "LOOP"

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

    maybeToolTip (CfgAction _ _ pipes) = " tooltip=\"" ++ showPipeNames pipes ++ "\""
    maybeToolTip _ = ""
    showPipeNames = List.intercalate " | " . map (showChan True) . Map.keys





{------------------------------------------------------------------------
  Top-level Pipeline
------------------------------------------------------------------------}

simplToCfg :: SAuto e nid -> CfgAuto e nid
simplToCfg a = a { auto_graph = Map.map nodeToCfg $ auto_graph a }
  where nodeToCfg (Node nid nkind) = Node nid (nkToCfg nkind)
        nkToCfg (SDone _) = CfgDone
        nkToCfg (SAtom wa nxt pipes) = CfgAction [wa] nxt pipes
        nkToCfg (SBranch x nxt1 nxt2 l _) = CfgBranch x nxt1 nxt2 l


automatonPipeline :: Atom e => DynFlags -> GS.Sym -> Ty -> Ty -> AComp () () -> IO (CfgAuto e Int)
automatonPipeline dfs sym inty outty acomp = do
  inch  <- freshName sym "src"  (acomp_loc acomp) inty Imm
  outch <- freshName sym "snk" (acomp_loc acomp) outty Mut
  let channels = Channels { in_chan = inch, out_chan = outch, ctrl_chan = Nothing }
  let k = mkDoneAutomaton inch outch

  putStrLn ">>>>>>>>>> mkAutomaton"
  a <- simplToCfg <$> mkAutomaton dfs sym channels acomp k
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
