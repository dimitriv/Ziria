{-# LANGUAGE GADTs, EmptyDataDecls #-}
module ExecPlan (
    Plan (..), Map (..), TaskID, TaskPlacement (..), TaskInfo (..),
    Queue (..), Computer, Transformer,
    makePlan,
    comp, trans
  ) where
import Data.Default
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import AstComp (CompCtxt (Hole), Comp, CTy)
import AstExpr (Ty)

data Computer
data Transformer

type TaskID = String

data TaskInfo = TaskInfo {
    -- | Should we take care to place the task on its own core?
    taskPlacement   :: TaskPlacement,

    -- | Code for the computation.
    taskCode        :: Comp CTy Ty,

    -- | Queue from which the task reads its inputs.
    taskInputQueue  :: Queue,

    -- | Queue which task writes its output into.
    taskOutputQueue :: Queue
  }

-- | A queue identifier with an indicator as to whether the queue is a
--   commit queue or not.
data Queue = Queue {
    qID        :: Int,
    qIsCommitQ :: Bool
  }

-- | Take care to put task on a separate core, or just put it anywhere?
data TaskPlacement = Alone | Anywhere

newtype Task a = Task TaskID
  deriving (Eq, Ord)

instance Show (Task a) where
  show (Task s) = s

data AnyTask = CompTask (Task Computer) | TransTask (Task Transformer)
  deriving (Eq, Ord)

-- | Just some debug pretty-printing.
instance Show AnyTask where
  show (CompTask c) = "c(" ++ show c ++ ")"
  show (TransTask t) = "t(" ++ show t ++ ")"

-- | If the given task a computer?
isComp :: AnyTask -> Bool
isComp (CompTask _) = True
isComp _            = False

-- | Construct a computation atom. Convenient for building test @Map@s.
comp :: String -> Map
comp = Atom . CompTask . Task

-- | Construct a transformer atom. Convenient for building test @Map@s.
trans :: String -> Map
trans = Atom . TransTask . Task

-- | Calculate the maximum number of tasks that will ever be active at the same
--   time for the given @Map@.
maxTasks :: Map -> Int
maxTasks (Seq ps)   = maximum $ map maxTasks ps
maxTasks (Par ps)   = sum $ map maxTasks ps
maxTasks (Repeat p) = maxTasks p
maxTasks (Atom _)   = 1

-- | An execution map, describing the flow of the program from a bird's view.
--   Used as an intermediary between AST and @Plan@, as the AST contains a lot
--   of detail that's completely unnecessary to figure out an execution plan.
--
--   In the execution map, each atom is not an atomic computer or transformer,
--   but rather a task; an independently schedulable unit. These tasks are of
--   course either computers or transformers, but they may be consist of
--   numerous smaller parts inside.
data Map where
  -- | A sequence of maps. Corresponds to seq {c1 ; ... ; cn}.
  Seq :: [Map]   -> Map

  -- | Parallel composition. Corresponds to t1 >>> ... >>> tn.
  --   Invariant: argument can contain any number of transformers, but only a
  --   single computer.
  Par :: [Map]   -> Map

  -- | Repeat a computater, turning it into a transformer.
  --   Invariant: argument must be a computer.
  Repeat :: Map     -> Map

  -- | A task; an individually schedulable computer or transformer. May be
  --   composed of numerous smaller parts, but on this level tasks are atomic.
  Atom :: AnyTask -> Map

-- | An execution plan, describing when and how to switch schedules.
data Plan taskinfo = Plan {
    -- | The mapping of computers to their continuations. When some tasks
    --   finish executing, some other task(s) should take their place.
    --   This field indicates which tasks to start upon each other task's death.
    planConts    :: M.Map (Task Computer) (S.Set AnyTask),

    -- | The mapping of computers to their last dependent.
    --   Then a computer finishes executing, we need to wait until that
    --   computer's downstream is done executing as well before we can continue
    --   with the next computation in a sequence. Thus, each computer with a
    --   downstream needs to know which task to wait for.
    planLastTask :: M.Map (Task Computer) AnyTask,

    -- | The set of tasks to activate on program initialization.
    planInitial  :: S.Set AnyTask,

    -- | The maximum number of tasks that will ever be active in the system at
    --   the same time. This includes tasks running on all cores, so it will be
    --   slightly too high for parallel programs. That's probably OK though,
    --   since each task takes up very little memory.
    planMaxTasks :: Int,

    -- | Detailed information about each task.
    planTaskInfo :: M.Map TaskID taskinfo,

    -- | Shared context for threads.
    planContext  :: CompCtxt
  }

-- | The default execution plan consists of doing nothing.
instance Default (Plan a) where
  def = Plan {
      planConts = M.empty,
      planLastTask = M.empty,
      planInitial = S.empty,
      planMaxTasks = 0,
      planTaskInfo = M.empty,
      planContext = Hole
    }

-- | Returns the final computer of a map. When this computer is done,
--   the whole map is done.
lastComp :: Map -> Maybe (Task Computer)
lastComp (Atom ac)
  | CompTask c <- ac = Just c
  | otherwise        = Nothing
lastComp (Repeat _)  = Nothing
lastComp (Seq ms)    = lastComp $ last ms
lastComp (Par ms)    = findComp (map lastComp ms)

-- | Find the computation in a list of possible computations.
--   If there is none, return Nothing. If there is more than one,
--   fail loudly, because this is clearly a bug.
findComp :: [Maybe (Task Computer)] -> Maybe (Task Computer)
findComp ms =
  case filter isJust ms of
    [mc] -> mc
    []   -> Nothing
    _    -> error "BUG: more than one computer in Par"

-- | Find the set of tasks that will activate immediately when the
--   given @Map@ is activated.
firstTasks :: Map -> S.Set AnyTask
firstTasks (Atom t)   = S.singleton t
firstTasks (Repeat m) = firstTasks m
firstTasks (Seq ms)   = firstTasks $ head ms
firstTasks (Par ms)   = S.unions $ map firstTasks ms

-- | Create an execution @Plan@ from an execution @Map@.
--   Will crash loud and hard when fed a Map that breaks the invariants
--   of the type.
makePlan :: Map -> Plan a
makePlan m = fillLasts m $ fillConts m def {
    planInitial = firstTasks m,
    planMaxTasks = maxTasks m
  }

-- | Fill in the @planLastTask@ field of a @Plan@.
fillLasts :: Map -> Plan a -> Plan a
fillLasts (Atom _) plan =
  plan
fillLasts (Repeat m) plan =
  fillLasts m plan
fillLasts (Seq ms) plan =
  foldl' (flip fillLasts) plan ms
fillLasts exmap@(Par _) plan =
    case (lastComp exmap, lastTrans exmap) of
      (Just c, t) -> plan {planLastTask = M.insert c t (planLastTask plan)}
      _           -> plan
  where
    lastTrans (Par m)    = lastTrans (last m)
    lastTrans (Seq m)    = lastTrans (last m)
    lastTrans (Repeat m) = lastTrans m
    lastTrans (Atom t)   = t

-- | Fill in the @planConts@ field of a @Plan@.
fillConts :: Map -> Plan a -> Plan a
fillConts (Repeat m) plan =
  fillConts m plan
fillConts (Atom _) plan =
  plan
fillConts (Seq (m1:ms@(m2:_))) plan =
    fillConts (Seq ms) (fillConts m1 plan')
  where
    nextTasks = firstTasks m2
    conts = planConts plan
    plan' =
      case lastComp m1 of
        Just c -> plan {planConts = M.insert c nextTasks conts}
        _      -> plan
fillConts (Seq _) plan =
  plan
fillConts (Par ms) plan =
  foldl' (flip fillConts) plan ms
