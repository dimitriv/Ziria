module Atomix where

import Data.Map 

newtype AtomId  = AtomId  { atomId :: String  } 
  deriving (Eq, Ord, Show)
newtype QueueId = QueueId { queueId :: String } 
  deriving (Eq, Ord, Show)
newtype StateId  = StateId  { stateId :: String  } 
  deriving (Eq, Ord, Show)


instance Eq (Atom nfo ty e) where 
   x == y = (atom_id x) == (atom_id y)
instance Ord (Atom nfo ty e) where 
  compare x y = compare (atom_id x) (atom_id y)
instance Eq (Queue nfo ty) where
   x == y = (queue_id x) == (queue_id y)
instance Ord (Queue nfo ty) where
  compare x y = compare (queue_id x) (queue_id y)
 
instance Eq (State anfo ty e) where
   x == y = (state_id x) == (state_id y)
instance Ord (State anfo ty e) where
  compare x y = compare (state_id x) (state_id y)


data Atom nfo ty e 
  = Atom { atom_id     :: AtomId 
         , atom_code   :: e
         , atom_ty_in  :: [ty]
         , atom_ty_out :: [ty]
         , atom_info   :: nfo 
         }

data Queue nfo ty 
  = Queue { queue_id   :: QueueId
          , queue_ty   :: ty
          , queue_info :: nfo 
          }

data State anfo ty e 
  = State { state_id :: StateId 
          , state_fg :: [Atom anfo ty e]
          , state_rg :: [Atom anfo ty e]
          }

data WireType = SelfRead | AllRead

data Wire endpoint = Wire { wire_type :: WireType, wire_queue :: endpoint }  
{-   ~~ WireType ~~> endpoint -}


newtype AtomGraph anfo qnfo ty e 
  = AtomGraph { atomGraph :: Map (Atom anfo ty e) ([InWire QueueId],[QueueId]) 
              ; queues    :: Msp QueueId          ([Atom anfo ty e],[InWire AtomId]  )
              }

data Core anfo ty e 
  = Core { core_id         :: Int
         , core_state      :: [State anfo ty e]
         , core_disp_queue :: QueueId
         , core_init_st    :: e
         }


data Deployment anfo qnfo ty e 
  = Deployment {  dep_atom_graph :: AtomGraph anfo qnfo ty e
               ,  dep_core       :: [Core anfo ty e ]
               }







-- If (incoming,outgoing) = lookup atom atomGraph 
-- Then:
--   forall n. atom_ty_in !! n  == queue_ty (incoming !! n)
--   forall n. atom_ty_out !! n == queue_ty (outgoing !! n)
--   length (atom_ty_in)  == length incoming
--   length (atom_ty_out) == length outgoing
