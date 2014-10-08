-- | Basic types for task generation.
module TaskGenTypes (
    Queue (..)
  , TaskPlacement (..)
  , TaskInfo (..)
  , TaskEnv
  , TaskID
  , nextQ
  ) where
import qualified Data.Map as M

import AstComp (TaskID, Comp, CTy)
import AstExpr (Ty)
import PpComp ()

-- | A queue identifier with an indicator as to whether the queue is a
--   commit queue or not.
--
--   NB: A queue is characterized uniquely by both its identifier and
--       its type. Thus it's completely conceivable to have both a sync
--       queue and a commit queue with qID 0.
data Queue
  = SyncQ   {qID :: Int} -- ^ A normal Sora synchronization queue.
  | CommitQ {qID :: Int} -- ^ A commit queue. See @ParNotes.md@ for details.
  | ExtReadQ             -- ^ The external read buffer.
  | ExtWriteQ            -- ^ The external write buffer.
    deriving (Eq, Show)

-- | Successor function for queues. We use this rather than making an
--   'Enum' instance since 'toEnum' and 'fromEnum' don't make sense for
--   all queues.
--   This function preserves the type of queue; there is only one external
--   read and write queue respectively, so calling @nextQ@ on either of
--   those is an error as a sanity check.
nextQ :: Queue -> Queue
nextQ (SyncQ qid)   = SyncQ (succ qid)
nextQ (CommitQ qid) = CommitQ (succ qid)
nextQ q             = error $ "nextQ (" ++ show q ++ ") does not make sense!"

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
