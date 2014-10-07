-- | Basic types for task generation.
module TaskGenTypes (
    Queue (..)
  , TaskPlacement (..)
  , TaskInfo (..)
  , TaskEnv
  , TaskID
  ) where
import qualified Data.Map as M

import AstComp (TaskID, Comp, CTy)
import AstExpr (Ty)
import PpComp ()

-- | A queue identifier with an indicator as to whether the queue is a
--   commit queue or not.
data Queue = Queue {
    qID        :: Int,
    qIsCommitQ :: Bool
  } deriving Show

instance Enum Queue where
  succ (Queue qid qcq) = Queue (succ qid) qcq
  toEnum qid           = Queue qid False
  fromEnum             = qID

-- | Take care to put task on a separate core, or just put it anywhere?
data TaskPlacement = Alone | Anywhere
  deriving Show

-- | Task environment specialized to 'TaskID' and 'TaskInfo'.
type TaskEnv = M.Map TaskID TaskInfo

-- | Data structure describing a single task.
data TaskInfo = TaskInfo {
    -- | Should we take care to place the task on its own core?
    taskPlacement   :: TaskPlacement,

    -- | Code for the computation.
    taskComp        :: Comp CTy Ty,

    -- | Queue from which the task reads its inputs.
    taskInputQueue  :: Queue,

    -- | Queue which task writes its output into.
    taskOutputQueue :: Queue
  } deriving Show
