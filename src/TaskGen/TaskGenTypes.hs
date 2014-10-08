-- | Basic types for task generation.
module TaskGenTypes (
    Queue (..)
  , TaskPlacement (..)
  , TaskInfo (..)
  , SyncInfo (..)
  , TaskEnv
  , TaskID
  , nextQ
  , transSyncInfo
  , compSyncInfo
  , noSyncInfo
  ) where
import qualified Data.Map as M
import Data.Maybe (catMaybes)

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
    taskOutputQueue :: Queue,

    -- | Actions to take when task finishes.
    taskSyncInfo    :: SyncInfo
  } deriving Show

-- | Task synchronization information. Keeps track of what needs to be done
--   after a task finished.
--
--   Synchronization happens in the following order:
--
--       waitForAll (syncWait taskInfo)
--       maybe (pure ()) rollback (syncCommitQueue taskInfo)
--       startAll (syncStart taskInfo)
--
--   In practice, a transformer task will only perform the first step since
--   its other 'syncCommitQueue' and 'syncStart' fields will be empty.
data SyncInfo = SyncInfo {
    -- | Tasks which must finish before this task may finish.
    syncWait        :: [TaskID],

    -- | Commit queue to rollback when task finishes.
    syncCommitQueue :: Maybe Queue,
    
    -- | Other tasks that need to be started after this task finishes.
    syncStart       :: [TaskID]
  } deriving Show

-- | Sync info for a transformer. All transformers wait for their upstream
--   component to finish. The upstream-most (is that even a word?) transformer
--   can just go ahead and die whenever he feels like it though.
transSyncInfo :: Maybe TaskID -> SyncInfo
transSyncInfo waitTID = SyncInfo {
    syncWait = catMaybes [waitTID],
    syncCommitQueue = Nothing,
    syncStart = []
  }

-- | Sync info for a computer which does not have any concurrent tasks.
--   A computer which is not part of a stretch of @>>>@ only has to start
--   the next set of tasks when he finishes.
compSyncInfo :: [TaskID] -> SyncInfo
compSyncInfo startTIDs = SyncInfo {
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = startTIDs
  }

-- | Don't perform any synchronization at all.
--   Placeholder to get stuff to build until conversion is done.
noSyncInfo :: SyncInfo
noSyncInfo = SyncInfo {
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = []
  }
