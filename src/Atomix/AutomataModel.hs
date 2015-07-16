{-# LANGUAGE ScopedTypeVariables, TupleSections, FlexibleContexts #-}
{-# OPTIONS #-}
module AutomataModel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.List as List
import Data.Either

import Control.Exception
import Control.Monad.State

import AtomComp
import AstExpr
import Opts
import AtomixCompTransform ( freshName )
import qualified GenSym as GS

import Utils(panicStr)
import Control.Applicative ( (<$>) )

import Outputable
import qualified Text.PrettyPrint.HughesPJ as PPR
import Text.PrettyPrint.Boxes



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
  = WiredAtom { wires_in  :: [(Int,Chan)]
              , wires_out :: [(Int,Chan)]
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

  -- Getting (wired) atoms from expressions
  expToWiredAtom :: AExp () -> Maybe Chan -> WiredAtom a

  idAtom      :: Ty -> a
  idAtom t = castAtom (1,t) (1,t)

  assertWAtom :: Bool -> Chan -> WiredAtom a
  assertWAtom b x = WiredAtom [(1,x)] [] (assertAtom b)




{------------------------------------------------------------------------
  Concrete NodeKind Instances
------------------------------------------------------------------------}


data SimplNk atom nid
  = SAtom { wired_atom :: WiredAtom atom
          , atom_next  :: nid
          , pipe_balance :: Map Chan Int -- balance of pipeline queues
          }
  | SBranch { zbranch_ch   :: Chan -- If we read True we go to branch_true, otherwise to branch_false
            , zbranch_true  :: nid
            , zbranch_false :: nid
            , zbranch_while :: Bool -- Is this a while loop?
            , pipe_balance :: Map Chan Int -- balance of pipeline queues
            }
  | SDone { pipe_balance :: Map Chan Int -- balance of pipeline queues
          }

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
  = name ch ++ (if withUnique then "$" ++ show (uniqId ch) else "")




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

nextNid :: Automaton atom Int nkind -> Int
nextNid a = maxId+1
  where (maxId,_) = Map.findMax (auto_graph a)

insertNk :: Ord nid => nid -> nkind atom nid -> NodeMap atom nid nkind -> NodeMap atom nid nkind
insertNk nid nk nmap = Map.insert nid (Node nid nk) nmap

insert_prepend :: nkind atom Int -> Automaton atom Int nkind -> Automaton atom Int nkind
insert_prepend nkind a = -- this may be too strict -- ensure auto_closed $
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

    go (AEmit1 x) =
      let inp = [(1, x)]
          outp = [(1, out_chan chans)]
          atom = idAtom (nameTyp x)
          nkind = SAtom (WiredAtom inp outp atom) (auto_start k) Map.empty
          a = insert_prepend nkind k
      in return $ assert (auto_closed a) a

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
      pipe_ch <- freshName sym (pipeName c1 c2) loc t Mut
      let k1 = mkDoneAutomaton (in_chan chans) pipe_ch
      let k2 = mkDoneAutomaton pipe_ch (out_chan chans)
      a1 <- mkAutomaton dfs sym (chans {out_chan = pipe_ch}) c1 k1
      a2 <- mkAutomaton dfs sym (chans {in_chan = pipe_ch}) c2 k2
      return $ zipAutomata a1 a2 k

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
                     Map.lookup (auto_start a0) (auto_graph a0) -- CfgLoop (auto_start a)
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




---- Zipping Automata
-- remaining resources in pipe ("balance"), state of left automaton, state of right automaton
type Queue e = [e]

push :: e -> Queue e -> Queue e
push = (:)

pop :: Queue e -> Maybe (e, Queue e)
pop [] = Nothing
pop es = Just (List.last es, List.init es)


type ExtraState = (Int, Map Int Int) -- map done-distance to rollback-amount
type ProdNid = (ExtraState, Int, Int)
type PipeState = Queue (Either Int Int) -- left n means n reads, right m means m writes

getBudget = sum . rights

data DoneDist
  = DoneDist { dists        :: Set Int
             , dist_precise :: Bool
             }
  deriving Eq

data DistUpdate = Imprecise | NewDist Int

-- Compute "done distance" map of automaton (measured in reads from input-queue upto Done)
-- For efficieny reasons, should be cached locally:
--   let doneDist = mkDoneDist treshold a in ...
mkDoneDist :: forall nid e. Ord nid => Int -> SAuto e nid -> nid -> DoneDist
mkDoneDist threshold a = (\nid -> fromJust $ assert (Map.member nid $ auto_graph a) $ Map.lookup nid dist_map) where
  dist_map = go dones init
  dones = map (,NewDist 0) $
          filter (\nid -> case nodeKindOfId a nid of { SDone _ -> True; _ -> False }) $
          Map.keys (auto_graph a)
  init = Map.map (const $ DoneDist Set.empty True) (auto_graph a)

  go :: [(nid, DistUpdate)] -> Map nid DoneDist -> Map nid DoneDist
  go [] dist_map = dist_map
  go ((nid,update):work_list) dist_map
    | Imprecise <- update
    , dist_precise $ fromJust $ assert (Map.member nid dist_map) $  Map.lookup nid dist_map
    = let dist_map' = Map.adjust (\d -> d { dist_precise = False }) nid dist_map in
      let new_work = map (,Imprecise) (preds nid) in
      go (new_work ++ work_list) dist_map'

    | NewDist n <- update
    , Set.notMember n $ dists $ fromJust $ assert (Map.member nid dist_map) $ Map.lookup nid dist_map
    = let dist_map' = Map.adjust (\d -> d { dists = Set.insert n (dists d) }) nid dist_map in
      let new_work = map (mkUpdate n) (preds nid) in
      go (new_work ++ work_list) dist_map'

    | otherwise
    = go work_list dist_map

  preds = Set.toList . mkPreds a

  mkUpdate dist nid = (nid,) $
    let new_dist = dist + cost nid in
    if new_dist > threshold then Imprecise else NewDist new_dist

  cost nid = case nodeKindOfId a nid of
    SAtom watom _ _ -> countReads (auto_inchan a) watom
    _ -> 0



-- Precondition: a1 and a2 should satisfy (auto_outchan a1) == (auto_inchan a2)
-- a1 and a2 MUST NOT contain explicit loop nodes (but may contain loops)!!
zipAutomata :: forall e. Atom e => SAuto e Int -> SAuto e Int -> SAuto e Int -> SAuto e Int
zipAutomata a1' a2' k = concat_auto prod_a k
  where
    a1 = deleteDeadNodes a1'
    a2 = deleteDeadNodes a2'
    prod_a = (\a -> assert (auto_closed a) a) $
             assert (auto_closed a1) $
             assert (auto_closed a2) $
             Automaton prod_nmap (auto_inchan a1) (auto_outchan a2) start_prod_nid
    start_balance = []
    start_prod_nid = mkProdNid start_balance (auto_start a1) (auto_start a2)
    prod_nmap = zipNodes start_balance start_prod_nid Map.empty
    pipe_ch = assert (auto_outchan a1 == auto_inchan a2) $ auto_outchan a1

    threshold = 1000 -- FIXME: replace this with a sensible value instead of a random one
    done_dist = mkDoneDist threshold a2


    mkProdNid :: PipeState -> Int -> Int -> ProdNid
    mkProdNid ps nid1 nid2 = (,nid1,nid2) $
      let budget = getBudget ps
          rollback_behavior = Map.fromList $ map (\d -> (d, getRollback d ps)) $
                              filter (<= budget) $ Set.toList $ dists $ done_dist nid2
      in (budget, rollback_behavior)

    getRollback d ps = case try_consume' d ps of
      Nothing -> 0
      Just ps' -> sum $ lefts ps'

    zipNodes :: PipeState -> ProdNid -> SNodeMap e ProdNid -> SNodeMap e ProdNid
    zipNodes balance prod_nid@(_,nid1,nid2) prod_nmap =
      case Map.lookup prod_nid prod_nmap of
        Just _ -> prod_nmap -- We have already seen this product location. We're done!
        Nothing ->
          let nk1 = nodeKindOfId a1 nid1
              nk2 = nodeKindOfId a2 nid2
              noDups = const (assert False)
              pipes = Map.insertWith noDups pipe_ch (getBudget balance) $
                      Map.unionWith noDups (pipe_balance nk1) (pipe_balance nk2)
          in zipNodes' balance pipes prod_nid nk1 nk2 prod_nmap




    zipNodes' :: PipeState -> Map Chan Int -> ProdNid -> SimplNk e Int -> SimplNk e Int -> SNodeMap e ProdNid -> SNodeMap e ProdNid

    -- Try executing right automaton first! (in accordance with lazy, right-biased semantics)

    zipNodes' balance pipes prod_nid nk1 (SDone _) =
      insertNk prod_nid (SDone pipes)

    zipNodes' balance pipes prod_nid@(_,id1,_) nk1 (SBranch x l r w _) =
      let prod_nid_l = mkProdNid balance id1 l
          prod_nid_r = mkProdNid balance id1 r
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipes
      in zipNodes balance prod_nid_r . zipNodes balance prod_nid_l . insertNk prod_nid prod_nkind

    {- Treating this case here is not strictly in accordance with Ziria's lazy (right-biased)
       semantics; however, since branching has no side effects, this should be a legal reordering.
       We prefer handling branches first, as it will potentially allow us to merge several
       decision nodes into a single decision.
    -}
    zipNodes' balance pipes prod_nid@(_,_,id2) (SBranch x l r w _) nk2 =
      let prod_nid_l = mkProdNid balance l id2
          prod_nid_r = mkProdNid balance r id2
          prod_nkind = SBranch x prod_nid_l prod_nid_r w pipes
      in zipNodes balance prod_nid_r . zipNodes balance prod_nid_l . insertNk prod_nid prod_nkind

    zipNodes' balance pipes prod_nid@(_,id1,_) nk1 (SAtom wa2 next2 _)
      | Just balance' <- try_consume wa2 balance
      = let next_prod_nid = mkProdNid balance' id1 next2
            prod_nkind = SAtom wa2 next_prod_nid pipes
        in zipNodes balance' next_prod_nid . insertNk prod_nid prod_nkind

    -- Can't proceed in right automaton -- execute left automaton

    zipNodes' balance pipes prod_nid@(_,_,id2) (SAtom wa1 next1 _) nk2 =
      let balance' = produce wa1 balance
          next_prod_nid = mkProdNid balance' next1 id2
          prod_nkind = SAtom wa1 next_prod_nid pipes
      in zipNodes balance' next_prod_nid . insertNk prod_nid prod_nkind

     -- if we reach this case, the pipeline is already drained as far as possible
    zipNodes' balance pipes prod_nid (SDone _) nk2 =
      insertNk prod_nid (SDone pipes)


    try_consume :: WiredAtom atom -> PipeState -> Maybe PipeState
    try_consume wa q = try_consume' (countReads pipe_ch wa) q

    try_consume' :: Int -> PipeState -> Maybe PipeState
    try_consume' 0 q = Just q
    try_consume' n q = case pop q of
      Nothing                      -> Nothing
      Just (Left _,q)              -> try_consume' n q
      Just (Right m,q) | m<=n      -> try_consume' (n-m) q
                       | otherwise -> Just $ q ++ [Right (m-n)]

    produce :: WiredAtom atom -> PipeState -> PipeState
    produce wa q =
      (if writes > 0 then push (Right writes) else id) $
      (if reads > 0 then push (Left reads) else id) $ q
      where
        writes = countWrites pipe_ch wa
        reads = countReads (auto_inchan a1) wa










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
pruneUnreachable :: forall e nid. (Atom e, Ord nid) => nid -> CfgAuto e nid -> CfgAuto e nid
pruneUnreachable nid a = a { auto_graph = prune (auto_graph a) nid }
  where
    preds = mkPreds a

    prune :: CfgNodeMap e nid -> nid -> CfgNodeMap e nid
    prune nmap nid =
      case Map.lookup nid nmap of
        Nothing -> nmap -- already pruned
        Just _ -> Set.foldl (pruneBkw nid) (Map.delete nid nmap) (preds nid)

    pruneBkw :: nid -> CfgNodeMap e nid -> nid -> CfgNodeMap e nid
    pruneBkw suc nmap nid =
      case Map.lookup nid nmap of
        Nothing -> nmap
        Just (Node _ CfgDone) -> assert False undefined
        Just (Node _ (CfgAction _ next _)) -> if next==suc then prune nmap nid else nmap
        Just (Node _ (CfgLoop next)) -> if next==suc then prune nmap nid else nmap
        Just (Node _ (CfgBranch x suc1 suc2 _))
          | suc == suc1 -> -- suc2 becomes the unique sucessor (since suc1 is unreachable)
            let nk = CfgAction [assertWAtom False x] suc2 Map.empty
            in Map.insert nid (Node nid nk) nmap
          | suc == suc2 -> -- suc1 becomes the unique sucessor (since suc2 is unreachable)
            let nk = CfgAction [assertWAtom True x] suc1 Map.empty
            in Map.insert nid (Node nid nk) nmap
          | otherwise -> assert False undefined



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
    doAll work_list nmap =
      case work_list of
        [] -> return nmap
        nid:wl -> do
          (wl',nmap') <- markAndFuse nid nmap
          inNewFrame (doAll (wl'++wl) nmap')


   -- Inchiant: if isDone nid then all
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
            Node _ (CfgAction atoms' next' _) ->
              let node = Node nid (CfgAction (atoms++atoms') next' pipes)
              in (wl, Map.insert nid node nmap)
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
    showPipeNames = List.intercalate " | " . map show . Map.keys





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
