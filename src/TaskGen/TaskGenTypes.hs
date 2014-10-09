-- | Basic types for task generation.
module TaskGenTypes (
    Queue (..)
  , TaskPlacement (..)
  , TaskInfo (..)
  , SyncInfo (..)
  , TaskEnv
  , TaskID
  , nextQ
  , justAwait
  , justStart
  , dontSync
  , withStartsFrom
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

-- | Update the first 'SyncInfo' with any task starts from the second.
--   The second @SyncInfo@ needs to be empty except for those starts,
--   to avoid throwing away any sync information. If this invariant is
--   broken, this function bombs out with an angry error.
withStartsFrom :: SyncInfo -> SyncInfo -> SyncInfo
a `withStartsFrom` (SyncInfo [] Nothing bstarts) =
  a {syncStart = syncStart a ++ bstarts}
_ `withStartsFrom` _ =
  error "BUG: withStartsFrom discarded some sync information!"

-- | Just await completion of a single task before finishing. This is what
--   all transformers should do, except for the upstream-most one, which
--   can just go a head and die whenever, without any syncing.
justAwait :: TaskID -> SyncInfo
justAwait waitTID = SyncInfo {
    syncWait = [waitTID],
    syncCommitQueue = Nothing,
    syncStart = []
  }

-- | Only start the given list of tasks. This is what computers that are not
--   part of a @>>>@ stretch should do.
justStart :: [TaskID] -> SyncInfo
justStart startTIDs = SyncInfo {
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = startTIDs
  }

-- | Just die, without any synchronization. This is what upstream-most (is
--   that even a word?) transformers should do.
dontSync :: SyncInfo
dontSync = SyncInfo {
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = []
  }
