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
--   NB: The queue is characterized uniquely by both qId and
--   qIsCommitQ e.g.  we can well have an ordinary queue with qId=0
--   and a commit queue with qId=0.
--
--   Hence we derive an Eq instance. 
data Queue = Queue {
    qID        :: Int,
    qIsCommitQ :: Bool
  } deriving (Eq, Show)

instance Enum Queue where
  succ (Queue qid qcq) = Queue (succ qid) qcq
  toEnum qid           = Queue qid False
  fromEnum             = qID

-- | Take care to put task on a separate core where only this task will run, or just put it anywhere?
data TaskPlacement = Alone | Anywhere
  deriving Show

-- | Task environment specialized to 'TaskID' and 'TaskInfo'.
--   Invariant: every TaskInfo in the environment contains a taskComp where the input type and 
--   the output types are already buffer types (i.e. all reads and writes have been inserted)
--   Moreover the id's of internal queues used in the taskComp will agree with the taskInputQueue and
--   taskOutputQueue of the TaskInfo. 

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
    --   NB: if the taskComp ends with writing to output (WriteSnk) then this queue
    --   will never be used.  
    -- 
    -- TODO: probably we should fix this (in the long run taskComp will not contain 
    -- readSrc/writeSnk and the queue type should be something like:
    -- data Queue = QueueOutput ... | QueueInput ... | QueueCommit ... | QueueOrdinary ... 

    taskOutputQueue :: Queue
  } deriving Show
