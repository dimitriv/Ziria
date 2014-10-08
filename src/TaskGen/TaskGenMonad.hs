{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Monad for turning an AST into a task-oriented dito.
module TaskGenMonad (
  -- The TaskGen type
   TaskGen

  -- Running TaskGen computations
  , runTaskGen

  -- Generating queues
  , freshQueue, freshCommitQueue

  -- Inspecting and manipulating tasks
  , lookupTask, updateTask, updateTaskComp

  -- Dealing with barrier functions
  , addBarrierFun, getAllBarrierFuns

  -- Creating tasks
  , createTask, createTaskWithPlacement
  ) where
import Control.Applicative
import Control.Monad.State
import Data.Default
import Data.Tuple (swap)
import qualified Data.Map as M
import qualified Data.Set as S

import PpComp ()
import AstComp hiding (comp)
import AstExpr hiding (name)
import TaskGenTypes

-- | Environment for task generation computations.
data TaskGenState name task = TaskGenState {
    -- | The set of functions containing merge barriers.
    --   We need to keep track of these to be able to shove calls
    --   to such functions into their own tasks.
    tgstBarrierFuns :: S.Set Name,

    -- | Mapping from task IDs to their respective info structures.
    tgstTaskInfo    :: M.Map name task,

    -- | Name of next sync queue to generate.
    tgstNextQ       :: Queue,

    -- | Name of next commit queue to generate.
    tgstNextCQ      :: Queue,

    -- | # of tasks created so far. Useful for statistics as well
    --   as for a *very* generous estimate of how large an active
    --   queue the scheduler will need.
    tgstNumTasks    :: Int
  }

instance Default (TaskGenState name task) where
  def = TaskGenState {
      tgstBarrierFuns = S.empty,
      tgstTaskInfo    = M.empty,
      tgstNextQ       = SyncQ 0,
      tgstNextCQ      = CommitQ 0,
      tgstNumTasks    = 0
    }

newtype NameT name m a = NameT {runNameT :: name -> m (name, a)}
type TaskGen name task a = NameT name (State (TaskGenState name task)) a

instance MonadState (TaskGenState name task)
         (NameT name (State (TaskGenState name task))) where
  get = NameT $ \n -> get >>= \x -> return (n, x)
  put x = NameT $ \n -> put x >> return (n, ())

instance Functor m => Functor (NameT name m) where
  fmap f (NameT x) = NameT $ \n -> fmap (fmap f) (x n)

instance (Functor m, Monad m) => Applicative (NameT name m) where
  (NameT f) <*> (NameT x) = NameT $ \n -> do
    (n', f') <- f n
    (n'', x') <- x n'
    return (n'', f' x')
  pure x = NameT $ \n -> return (n, x)

instance Monad m => Monad (NameT name m) where
  return x = NameT $ \n -> return (n, x)
  (NameT m) >>= f = NameT $ \n -> do
    (n', x) <- m n
    runNameT (f x) n'

-- | Generate a fresh name.
freshName :: (Enum name, Monad m) => NameT name m name
freshName = NameT $ \n -> return (succ n, n)

-- | Get a fresh queue.
freshQueue :: TaskGen name task Queue
freshQueue = do
  st <- get
  put $ st {tgstNextQ = nextQ $ tgstNextQ st}
  return $ tgstNextQ st

-- | Get a fresh commit queue.
freshCommitQueue :: TaskGen name task Queue
freshCommitQueue = do
  st <- get
  put $ st {tgstNextCQ = nextQ $ tgstNextCQ st}
  return $ tgstNextCQ st

-- | Associate a task with a name.
associate :: Ord name => name -> task -> TaskGen name task ()
associate name task = modify $ \st ->
  st {tgstTaskInfo = M.insert name task (tgstTaskInfo st)}

-- | Look up a task in the environment.
lookupTask :: Ord name => name -> TaskGen name task task
lookupTask tid = (M.! tid) . tgstTaskInfo <$> get

-- | Update a task in the environment.
updateTask :: Ord name => name -> (task -> task) -> TaskGen name task ()
updateTask tid f = do
  st <- get
  put $ st {tgstTaskInfo = M.adjust f tid $ tgstTaskInfo st}

-- | Special case of 'updateTask' for the common case where only the
--   comp needs updating.
updateTaskComp :: Ord name => name -> (Comp CTy Ty -> Comp CTy Ty) -> TaskGen name TaskInfo ()
updateTaskComp tid f = do
  st <- get
  put $ st {
      tgstTaskInfo =
        M.adjust (\t -> t {taskComp = f (taskComp t)})
                 tid
                 (tgstTaskInfo st)
    }

-- | Mark a function as a merge barrier.
addBarrierFun :: Name -> TaskGen name task ()
addBarrierFun f = modify $ \st ->
  st {tgstBarrierFuns = S.insert f (tgstBarrierFuns st)}

-- | Return the set of all barrier functions encountered so far.
getAllBarrierFuns :: TaskGen name task (S.Set Name)
getAllBarrierFuns = fmap tgstBarrierFuns get

-- | Run a task generation computation.
runTaskGen :: (Default name, Enum name) => TaskGen name task a -> (M.Map name task, a)
runTaskGen =
  swap . fmap tgstTaskInfo . flip runState def . fmap snd . flip runNameT def

-- | Create a new task from a 'TaskInfo' structure,
--   complete with queue reads and writes.
createTaskEx :: (Ord name, Enum name) => TaskInfo -> TaskGen name TaskInfo name
createTaskEx task = do
  modify $ \st -> st {tgstNumTasks = tgstNumTasks st + 1}
  name <- freshName
  associate name $ task {
      taskComp = insertQs (taskComp task)
                          (taskInputQueue task)
                          (taskOutputQueue task)
    }
  return name

-- | Create a new task from a computation and a pair of queues.
--   The new task will be schedulable 'Anywhere'. To create a task
--   for executing 'Alone', use 'createTaskWithPlacement'.
createTask :: (Ord name, Enum name) => Comp CTy Ty -> (Queue, Queue) -> TaskGen name TaskInfo name
createTask = createTaskWithPlacement Anywhere

-- | Like 'createTask' but creates the task with the 'Alone'
--   placement annotation.
createTaskWithPlacement :: (Ord name, Enum name)
                        => TaskPlacement  -- ^ CPU allocation info for task.
                        -> Comp CTy Ty    -- ^ Task computation.
                        -> (Queue, Queue) -- ^ Input/output queues.
                        -> TaskGen name TaskInfo name
createTaskWithPlacement placement comp (qin, qout) = do
  createTaskEx $ TaskInfo {
    taskComp = comp,
    taskInputQueue = qin,
    taskOutputQueue = qout,
    taskPlacement = placement
  }

-- | Insert queue reads and writes for a task.
--   Does not insert a read/write is one is already present.
insertQs :: Comp CTy Ty -> Queue -> Queue -> Comp CTy Ty
insertQs c qin qout =
    case (isReadBufTy $ compInfo c, isWriteBufTy $ compInfo c) of
      (False, False) ->
        cPar loc (mkParTy rbufty partyout) pnfo reader (cPar loc partyout pnfo c writer)
      (True, True) ->
        c
      (True, False) ->
        cPar loc partyout pnfo c writer
      (False, True) ->
        cPar loc (mkParTy rbufty (compInfo c)) pnfo reader c
  where
    (inty, outty) = case compInfo c of
      CTBase (TTrans i o)  -> (i, o)
      CTBase (TComp _ i o) -> (i, o)
      _                    -> error $ "BUG: bad types to readFrom"
    loc = compLoc c
    rbufty = CTBase $ TTrans (TBuff (IntBuf inty)) inty
    wbufty = CTBase $ TTrans outty (TBuff (IntBuf outty))
    pnfo = mkParInfo MaybePipeline
    partyout = mkParTy (compInfo c) wbufty
    -- TODO: read type doesn't really matter probably, but this may not be right
    reader = cReadInternal loc rbufty (show $ qID qin) SpinOnEmpty
    writer = cWriteInternal loc wbufty (show $ qID qout)

-- | Is the transformer from a buffer read to something else?
isReadBufTy :: CTy -> Bool
isReadBufTy (CTBase (TTrans (TBuff _) _)) = True
isReadBufTy _                             = False

-- | Is the transformer from something to a buffer write?
isWriteBufTy :: CTy -> Bool
isWriteBufTy (CTBase (TTrans _ (TBuff _))) = True
isWriteBufTy _                           = False

-- | Produces the type of a >>> b given the types of a and b.
mkParTy :: CTy -> CTy -> CTy
mkParTy (CTBase (TTrans i _)) (CTBase (TTrans _ o))  = CTBase (TTrans i o)
mkParTy (CTBase (TComp r i _)) (CTBase (TTrans _ o)) = CTBase (TComp r i o)
mkParTy (CTBase (TTrans i _)) (CTBase (TComp r _ o)) = CTBase (TComp r i o)
mkParTy _ _ = error $ "Can't reconstruct incompatible arrows!"
