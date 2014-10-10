{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
-- | Basic types for task generation.
module TaskGenTypes (
    Queue (..)
  , TaskPlacement (..)
  , TaskInfo (..)
  , SyncInfo (..)
  , TaskEnv
  , TaskID
  , nextQ
  , justClose
  , justStart
  , dontSync
  , withStartsFrom
  , propagateDeathUp
  , propagateDeathDown
  ) where
import qualified Data.Map as M
import GHC.Generics
import Text.Show.Pretty (PrettyVal)
import Orphans

-- | A queue identifier with an indicator as to whether the queue is a
--   commit queue or not.
--
--   NB: A queue is characterized uniquely by both its identifier and
--       its type. Thus it's completely conceivable to have both a sync
--       queue and a commit queue with qID 0.
data Queue =
    -- | A normal Sora synchronization queue.
    SyncQ   {qID :: Int}
    -- | A commit queue. See @ParNotes.md@ for details.
    --   Invariant: commit queues are always locally first in a pipe.
  | CommitQ {qID :: Int,
             qLast :: Bool}
    -- | The external read buffer.
  | ExtReadQ
    -- | The external write buffer.
  | ExtWriteQ
    deriving (Eq, Show, Generic)

instance PrettyVal Queue

-- | Successor function for queues. We use this rather than making an
--   'Enum' instance since 'toEnum' and 'fromEnum' don't make sense for
--   all queues.
--   This function preserves the type of queue; there is only one external
--   read and write queue respectively, so calling @nextQ@ on either of
--   those is an error as a sanity check.
nextQ :: Queue -> Queue
nextQ (SyncQ qid)         = SyncQ (succ qid)
nextQ (CommitQ qid qlast) = CommitQ (succ qid) qlast
nextQ q                   = error $ "nextQ (" ++ show q ++ ") does not make sense!"

-- | Should this queue propagate a "finished" signal upstream?
propagateDeathUp :: Queue -> Bool
propagateDeathUp (CommitQ {}) = True
propagateDeathUp _            = False

-- | Should this queue propagate a "finished" signal downstream?
propagateDeathDown :: Queue -> Bool
propagateDeathDown (CommitQ _ islast) = not islast
propagateDeathDown (SyncQ {})         = True
propagateDeathDown _                  = False


-- | Take care to put task on a separate core where only this task will run, or just put it anywhere?
data TaskPlacement = Alone | Anywhere
  deriving Show

-- | Identifier for tasks.
type TaskID = Int

-- | Task environment specialized to 'TaskID' and 'TaskInfo'.
--   Invariant: every TaskInfo in the environment contains a taskComp where the input type and 
--   the output types are already buffer types (i.e. all reads and writes have been inserted)
--   Moreover the id's of internal queues used in the taskComp will agree with the taskInputQueue and
--   taskOutputQueue of the TaskInfo. 
type TaskEnv comp = M.Map TaskID (TaskInfo comp)

-- | Data structure describing a single task.
--   'SyncInfo' is not explicitly part of this data structure, as what
--   synchronization needs to be done may depend on conditionals within a task.
data TaskInfo comp = TaskInfo {
    -- | Should we take care to place the task on its own core?
    taskPlacement   :: TaskPlacement,

    -- | Code for the computation.
    taskComp        :: comp,

    -- | Queue from which the task reads its inputs.
    taskInputQueue  :: Queue,

    -- | Queue which task writes its output into.  
    taskOutputQueue :: Queue
  } deriving Show

-- | Task synchronization information. Keeps track of what needs to be done
--   after a task finished.
--
--   Synchronization happens in the following order:
--
--       closeAll (syncCloseQueues taskInfo)
--       waitForAll (syncWait taskInfo)
--       maybe (pure ()) rollback (syncCommitQueue taskInfo)
--       markSelfAsFinished
--       startAll (syncStart taskInfo)
--
--   In practice, a transformer task will only perform the first step since
--   its other 'syncCommitQueue' and 'syncStart' fields will be empty.
data SyncInfo = SyncInfo {
    -- | Queues which are to be closed/finished on sync.
    --   Used to propagate kill signals upstream and downstream.
    syncCloseQueues :: [Queue],

    -- | Tasks which must finish before this task may finish.
    syncWait        :: [TaskID],

    -- | Commit queue to rollback when task finishes.
    syncCommitQueue :: Maybe Queue,
    
    -- | Other tasks that need to be started after this task finishes.
    syncStart       :: [TaskID]
  } deriving (Show, Generic)

instance PrettyVal SyncInfo

-- | Update the first 'SyncInfo' with any task starts from the second.
--   The second @SyncInfo@ needs to be empty except for those starts,
--   to avoid throwing away any sync information. If this invariant is
--   broken, this function bombs out with an angry error.
withStartsFrom :: SyncInfo -> SyncInfo -> SyncInfo
a `withStartsFrom` (SyncInfo [] [] Nothing bstarts) =
  a {syncStart = syncStart a ++ bstarts}
_ `withStartsFrom` _ =
  error "BUG: withStartsFrom discarded some sync information!"

-- | Only start the given list of tasks. This is what computers that are not
--   part of a @>>>@ stretch should do.
justStart :: [TaskID] -> SyncInfo
justStart startTIDs = SyncInfo {
    syncCloseQueues = [],
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = startTIDs
  }

-- | Just close a queue when finished. This is what all transformers but the
--   downstream-most should do.
justClose :: Queue -> SyncInfo
justClose q = SyncInfo {
    syncCloseQueues = [q],
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = []
  }

-- | Just die, without any synchronization. This is what the
--   downstream-most transformer in a pipeline should do.
dontSync :: SyncInfo
dontSync = SyncInfo {
    syncCloseQueues = [],
    syncWait = [],
    syncCommitQueue = Nothing,
    syncStart = []
  }
