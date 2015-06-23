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



type MarkingM nid = State (Set nid)
mark :: Ord nid => nid ->  MarkingM nid ()
mark nid = do modify (Set.insert nid)

isMarked :: Ord nid => nid -> MarkingM nid Bool
isMarked nid = do
  marks <- get
  return (Set.member nid marks)


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
            -> Automaton e Int
mkAutomaton dfs chans comp k = go (unComp comp)
  where
    go (Take1 t) =
      let inp = [in_chan chans]
          outp = maybeToList (ctrl_chan chans)
          atom = maybe (discardAtom t) (\_ -> idAtom t) (ctrl_chan chans)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in insert_prepend nkind k

    --go (TakeN _ n) = fail "not implemented"

    go (Emit1 x) =
      let inp = [x]
          outp = [out_chan chans]
          atom = idAtom (nameTyp x)
          nkind = Action [WiredAtom inp outp atom] (auto_start k)
      in insert_prepend nkind k

    --go (EmitN x) = fail "not implemented"

    go (Return e) =
      let watom = expToWiredAtom e (ctrl_chan chans)
          nkind = Action [watom] (auto_start k)
      in insert_prepend nkind k

    go (NewVar x_spec c) = mkAutomaton dfs chans c k -- NOP for now

    go (Bind mbx c1 c2) =
      let a2 = mkAutomaton dfs chans c2 k
      in mkAutomaton dfs (chans { ctrl_chan = mbx }) c1 a2

    --go (Par _ c1 c2) = fail "not implemented"

    go (AtomComp.Branch x c1 c2) =
      let a1 = mkAutomaton dfs chans c1 k
          a2 = mkAutomaton dfs chans c2 k
          nkind = AutomataModel.Branch x (auto_start a1) (auto_start a2) False
      in insert_prepend nkind k

    go (RepeatN n c) = applyN n (mkAutomaton dfs chans c) k
      where applyN 0 f = id
            applyN n f = f . applyN (n-1) f

    go (Repeat c) =
      case nodeKindOfId (auto_start k) k of
        Done ->
          let a = mkAutomaton dfs chans c k
              nid = auto_start k
              nkind = Loop (auto_start a)
          in a { auto_start = nid, auto_graph = Map.insert nid (Node nid nkind) (auto_graph a) }
        --_ -> fail "Repeat should not have a continuation!"

    go (While x c) =
      let k' = insert_prepend Done k
          nid = auto_start k'
          a = mkAutomaton dfs chans c k'
          nkind = AutomataModel.Branch x (auto_start a) (auto_start k) True
      in a { auto_start = nid, auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)}

    go (Until x c) =
      let k' = insert_prepend Done k
          nid = auto_start k'
          a = mkAutomaton dfs chans c k'
          nkind = AutomataModel.Branch x (auto_start a) (auto_start k) True
      in a { auto_graph = Map.insert nid (Node nid nkind) (auto_graph a)}






--instance Show (NodeKind atom nid) where
--  show (NodeLabel _ (Action watom)) =
--    let show_wires = List.intercalate "," . map show
--    in show_wires (wires_in watom) ++ "/" ++ show_wires (wires_out watom)
--  show (NodeLabel _ (AutomataModel.Branch win _)) = show win ++ "/DQ"
--  show (NodeLabel _ (StaticLoop n _)) = "/DQ="

