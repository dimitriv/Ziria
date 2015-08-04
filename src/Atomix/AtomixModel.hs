module AtomixModel (
  node_of_atomid,
  node_of_queueid,
  Atom(..),
  Queue(..),
  WireType,
  NodeType,
  FlowGraph,
  atom_of_id,
  queue_of_id,
  mk_atom,
  mk_queue,
  State(..),
  Core(..),
  Deployment(..)) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Exception 


newtype AtomId = AtomId { node_of_atomid :: Int }
  deriving (Eq, Ord, Show)
newtype QueueId = QueueId { node_of_queueid :: Int }
  deriving (Eq, Ord, Show)


data Atom anfo ty e 
  = Atom { atom_id     :: AtomId
         , atom_code   :: e
         , atom_ty_in  :: [ty]
         , atom_ty_out :: [ty]
         , atom_info   :: anfo 
         }

data Queue qnfo ty
  = Queue { queue_id   :: QueueId
          , queue_ty   :: ty
          , queue_info :: qnfo 
          }

data WireType = SelfRead | AllRead | Write
type NodeType anfo qnfo ty e = Either (Atom anfo ty e) (Queue qnfo ty)

newtype FlowGraph anfo qnfo ty e 
  = FlowGraph { flow_graph :: Gr (NodeType anfo qnfo ty e) WireType }


atom_of_id :: FlowGraph anfo qnfo ty e -> AtomId -> Atom anfo ty e
atom_of_id (FlowGraph fg) (AtomId id) = case lab fg id of
  Just (Left a) -> a
  Just (Right _) -> assert False undefined
  Nothing -> assert False undefined

queue_of_id :: FlowGraph anfo qnfo ty e -> QueueId -> Queue qnfo ty
queue_of_id (FlowGraph fg) (QueueId id) = case lab fg id of
  Just (Right q) -> q
  Just (Left _) -> assert False undefined
  Nothing -> assert False undefined

mk_atom :: FlowGraph anfo qnfo ty e -> e -> [ty] -> [ty] -> anfo -> (AtomId, FlowGraph anfo qnfo ty e)
mk_atom (FlowGraph fg) code ty_in ty_out info =
  let [id]  = newNodes 1 fg
      aid   = AtomId id
      atom  = Atom aid code ty_in ty_out info
      node = (id, Left atom)
      fg = insNode node fg
  in (aid, FlowGraph fg)

mk_queue :: FlowGraph anfo qnfo ty e -> ty -> qnfo -> (QueueId, FlowGraph anfo qnfo ty e)
mk_queue (FlowGraph fg) ty info =
  let [id]  = newNodes 1 fg
      qid   = QueueId id
      queue = Queue qid ty info
      node  = (id, Right queue)
      fg = insNode node fg
  in (qid, FlowGraph fg)



data State
  = State { state_id :: Int 
          , state_fg :: [AtomId]
          , state_rg :: [AtomId]
          }


data Core
  = Core { core_id         :: Int
         , core_state      :: [State]
         , core_disp_queue :: QueueId
         , core_init_st    :: (State, Int)
         }


data Deployment anfo qnfo ty e 
  = Deployment {  dep_atom_graph :: FlowGraph anfo qnfo ty e
               ,  dep_cores      :: [Core]
               }
