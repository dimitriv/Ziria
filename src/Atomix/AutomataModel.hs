module AutomataModel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Exception


newtype NodeId = NodeId { node_of_id :: Int }
  deriving (Eq, Ord, Show)

data NodeLabel expr var
  = Action { action_in   :: Set var
           , action_out  :: Set var
           , action_code :: expr
           , action_id   :: NodeId }
  | Control { control_in  :: Set var
            , control_ite :: (expr, NodeId, NodeId)
            , control_id  :: NodeId }
            
data Automaton expr var
  = Automaton { automaton_init  :: NodeId
              , automaton_final :: Set NodeId }

