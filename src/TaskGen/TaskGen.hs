{-# LANGUAGE GADTs, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}
-- | Rewrite an optimized, renamed and generally mangled AST
--   into an AST utilizing the new fancy scheduler.
module TaskGen (
    insertTasks
  ) where
import Control.Applicative
import Control.Monad.State
import Data.Default
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Tuple
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Parsec.Pos

import AstComp
import AstExpr
import PpComp
import Outputable
import TaskGenTypes
import TaskGenMonad

import Debug.Trace

data Computer
data Transformer


newtype Task a = Task TaskID
  deriving (Eq, Ord)

instance Show (Task a) where
  show (Task s) = show s

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

-- | An execution plan, describing when and how to switch schedules.
--   TODO: this has to go, probably. This is better described as AST
--         transformations.
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

{-

This is dead code, but kept around as design notes until equivalent
can be designed for AstComp.


-- | Returns the final computer of a map. When this computer is done,
--   the whole map is done.
lastComp :: Map -> Maybe (Task Computer)
lastComp (Atom ac)
  | CompTask c <- ac         = Just c
  | otherwise                = Nothing
lastComp (ExecPlan.Repeat _) = Nothing
lastComp (ExecPlan.Seq ms)   = lastComp $ last ms
lastComp (ExecPlan.Par ms)   = findComp (map lastComp ms)

-- | Find the computation in a list of possible computations.
--   If there is none, return Nothing. If there is more than one,
--   fail loudly, because this is clearly a bug.
findComp :: [Maybe (Task Computer)] -> Maybe (Task Computer)
findComp ms =
  case filter isJust ms of
    [mc] -> mc
    []   -> Nothing
    _    -> error "BUG: more than one computer in Par"


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
fillLasts (ExecPlan.Repeat m) plan =
  fillLasts m plan
fillLasts (ExecPlan.Seq ms) plan =
  foldl' (flip fillLasts) plan ms
fillLasts exmap@(ExecPlan.Par _) plan =
    case (lastComp exmap, lastTrans exmap) of
      (Just c, t) -> plan {planLastTask = M.insert c t (planLastTask plan)}
      _           -> plan
  where
    lastTrans (ExecPlan.Par m)    = lastTrans (last m)
    lastTrans (ExecPlan.Seq m)    = lastTrans (last m)
    lastTrans (ExecPlan.Repeat m) = lastTrans m
    lastTrans (Atom t)            = t

-- | Fill in the @planConts@ field of a @Plan@.
fillConts :: Map -> Plan a -> Plan a
fillConts (ExecPlan.Repeat m) plan =
  fillConts m plan
fillConts (Atom _) plan =
  plan
fillConts (ExecPlan.Seq (m1:ms@(m2:_))) plan =
    fillConts (ExecPlan.Seq ms) (fillConts m1 plan')
  where
    nextTasks = firstTasks m2
    conts = planConts plan
    plan' =
      case lastComp m1 of
        Just c -> plan {planConts = M.insert c nextTasks conts}
        _      -> plan
fillConts (ExecPlan.Seq _) plan =
  plan
fillConts (ExecPlan.Par ms) plan =
  foldl' (flip fillConts) plan ms

-}

-- | Comp representing the starting of a task.
startTask :: Comp CTy Ty -> TaskID -> Comp CTy Ty
startTask c t = MkComp (ActivateTask t Nothing) (compLoc c) (compUnitTy c)

-- | Comp representing the starting of a task from a BindMany, with an input var.
startTaskFromBind :: Maybe SourcePos -> a -> TaskID -> Name -> Comp a Ty
startTaskFromBind mloc info t v = MkComp (ActivateTask t (Just v)) mloc info

-- | Split program into entry points and tasks to be started along the road.
--   TODO: task map needs more info.
insertTasks :: Comp CTy Ty -> (TaskEnv, Comp CTy Ty)
insertTasks = runTaskGen . go
  where
    go comp = do
      inq <- freshQueue
      outq <- freshQueue
      tid <- taskify (inq, outq) Nothing comp
      taskComp <$> lookupTask tid

-- | Given an expression of type Comp a i o, produces a type Comp () i o.
compUnitTy :: Comp CTy Ty -> CTy
compUnitTy c =
  case compInfo c of
    (CTBase (TComp _ i o)) -> CTBase $ TComp TUnit i o
    (CTBase (TTrans i o))  -> CTBase $ TComp TUnit i o
    _                      -> error $ "Non base type in compUnitTy:\n" ++ show (ppr $ compInfo c)

-- | The type Comp (C ()) () ().
compTripleUnitTy :: CTy
compTripleUnitTy = CTBase $ TComp TUnit TUnit TUnit

-- | A placeholder queue for now. Outputs annoying warnings as a reminder to
--   remove it.
dummyQueue :: Queue
dummyQueue = trace ("DUMMY QUEUE IN USE - PLEASE FIX ASAP!") $ Queue 0 False

-- | Split the given AST along task barriers. The returned AST is the entry
--   point of the program, from which further tasks are spawned as needed.
--   In the absence of `standalone` annotations, this is the identity function.
--
--   Currently, only @Standalone@ creates a primitive barrier. |>>>| is treated
--   like >>>.
--
--   TODO: the compInfo stuff (the types) will need some reshuffling for
--         BindMany; having tasks return (), etc.
--
--   TODO: current algorithm, calling containsBarrier all over the place, is
--         O(n^2) - could be O(n) if we instead annotated the AST as we
--         traverse it.
--
--   TODO: currently doesn't do synchronization at start/end of tasks.
--
--   TODO: currently doesn't calculate cardinalities for commit queues.
--
--   TODO: don't generate unnecessary queues or their associated reads/writes.
--         they don't really do anything so it's not incorrect per se to generate
--         them, but it's ugly.
taskify :: (Queue, Queue)
        -> Maybe (Comp CTy Ty)
        -> Comp CTy Ty
        -> TaskGen TaskID TaskInfo TaskID
taskify qs@(inq, outq) mnxt c = do
    case (containsBarrier c, mnxt) of
      (True, _)         -> go (unComp c)
      (False, Just nxt)
        | isComputer c  -> createTask (MkComp (c `Seq` nxt) (compLoc c) (compInfo c)) qs
      _                 -> createTask c qs
  where
    -- Invariant: if go gets called, its argument *does* contain a barrier.
    -- It's important to note that sequential code is generated right to left,
    -- in order to ensure that each sequential computation always knows its
    -- continuation.
    -- TODO: sync code here!
    go :: Comp0 CTy Ty -> TaskGen TaskID TaskInfo TaskID
    go (BindMany first rest)
      | containsBarrier first = do
          let ((_, comp):xs) = rest
          ms <- taskify qs mnxt (MkComp (BindMany comp xs) (compLoc comp) (compInfo c))
          tid <- taskify qs (Just $ startTask c ms) first
          createTask (startTask first tid) qs
      | otherwise = do
          comp <- genBindManyBarriers first (splitBarriers rest)
          createTask comp qs
      where
        -- Invariant: each (first ++ b) contains a barrier.
        genBindManyBarriers :: Comp CTy Ty -> [[(Name, Comp CTy Ty)]] -> TaskGen TaskID TaskInfo (Comp CTy Ty)
        genBindManyBarriers first (b:bs) = do
          case bs of
            (((_, first'):rest):bs') -> do
              -- Next task is just the next in the list of barriers
              nxt <- Just <$> genBindManyBarriers first' (rest:bs')
              b' <- taskify qs nxt $ MkComp (BindMany first b) (compLoc first) (compInfo c)
              startTask c <$> createTask (startTask c b') qs
            _                        -> do
              -- End of bind; next task is whatever came from the outside, if any
              b' <- taskify qs mnxt $ MkComp (BindMany first b) (compLoc first) (compInfo c)
              startTask c <$> createTask (startTask c b') qs
        genBindManyBarriers first [] = do
          error "BUG: barrier is in first comp of BindMany, but genBindManyBarriers was called!"

    go (Seq left right) = do
      -- TODO: pass in continuation task start
      mcomp <- foldM seqStart mnxt (reverse $ findBarriers c)
      case mcomp of
        Just comp -> createTask comp qs
        _         -> error "The impossible happened!"
      where
        -- TODO: some more sync code should go here!
        seqStart :: Maybe (Comp CTy Ty) -> Comp CTy Ty -> TaskGen TaskID TaskInfo (Maybe (Comp CTy Ty))
        seqStart (Just next) comp = do
          startNext <- startTask next <$> createTask next qs
          return . Just $ MkComp (comp `Seq` startNext) (compLoc comp) (compInfo comp)
        seqStart Nothing comp = do
          return $ Just comp

        -- Turn a ; standalone b ; c ; d into [a, standalone b, c ; d]
        findBarriers :: Comp CTy Ty -> [Comp CTy Ty]
        findBarriers s@(MkComp (Seq l r) loc nfo) =
          case (containsBarrier l, containsBarrier r) of
            (True, True)  -> findBarriers l ++ findBarriers r
            (True, False) -> findBarriers l ++ [r]
            (False, True) -> l:findBarriers r
            _             -> [s]
        findBarriers s = [s]

    go (Par _ left right) = do
      -- TODO: find first and last components of pipeline, as well as the computer (if any) and add sync
      --       code! Also, commit queues; currently everything is simple queues.
      let bars = findBarriers c
      queues <- mapM (const freshQueue) (drop 1 bars)
      taskIds <- mapM createTask' $ zip3 bars (inq:queues) (queues ++ [outq])
      let starts = foldr1 (cSeq (compLoc c) compTripleUnitTy) $
                     zipWith startTask bars taskIds
      createTask starts (head queues, last queues)
      where
        createTask' (c', qin', qout') = taskify (qin', qout') mnxt c'

        -- Turn a >>> standalone b >>> c >>> d into [a, standalone b, c >>> d]
        findBarriers p@(MkComp (Par _ l r) loc nfo) =
          case (containsBarrier l, containsBarrier r) of
            (True, True)  -> findBarriers l ++ findBarriers r
            (True, False) -> findBarriers l ++ [r]
            (False, True) -> l:findBarriers r
            _             -> [p]
        findBarriers p = [p]

    -- Let bindings will be declared on the top level, so we keep them around
    -- in the AST just to have their values properly assigned.
    go (Let name rhs comp) = do
      tid <- taskify qs mnxt comp
      updateTaskComp tid $ \c' ->
        MkComp (Let name rhs c') (compLoc c) (compInfo c)
      return tid
    go (LetE name inl rhs comp) = do
      tid <- taskify qs mnxt comp
      updateTaskComp tid $ \c' ->
        MkComp (LetE name inl rhs c') (compLoc c) (compInfo c)
      return tid
    go (LetERef name ty rhs comp)  = do
      tid <- taskify qs mnxt comp
      updateTaskComp tid $ \c' ->
        MkComp (LetERef name ty rhs c') (compLoc c) (compInfo c)
      return tid

    go (LetFunC name args locs body comp) = do
      -- We don't go into the body of the function since we simply can't split
      -- non-inlined functions into tasks.
      when (containsBarrier body) $ do
        addBarrierFun name
      tid <- taskify qs mnxt comp
      updateTaskComp tid $ \c' ->
        MkComp (LetFunC name args locs body c') (compLoc c) (compInfo c)
      return tid

    go (Interleave left right) =
      error "BUG: Interleave in AST!"
    go (Branch cond th el) = do
      th' <- startTask th <$> taskify qs mnxt th
      el' <- startTask el <$> taskify qs mnxt el
      createTask (MkComp (Branch cond th' el') (compLoc c) (compInfo c)) qs
    go (Until cond comp) = do
      error "TODO: until requires some extra code"
    go (While cond comp) = do
      error "TODO: while requires some extra code"
    go (Times ui from to it comp) = do
      error "TODO: times requires some extra code"
    go (AstComp.Repeat mvect comp) = do
      error "TODO: repeat requires some extra code"
    go (VectComp vect comp) = do
      -- TODO: cardinality information needed to inform the counting
      --       not necessarily right here though?
      tid <- taskify qs mnxt comp
      updateTaskComp tid $ \comp' ->
        MkComp (VectComp vect comp') (compLoc c) (compInfo c)
      return tid
    go (Standalone comp)
      | containsBarrier comp = do
        -- If we run into nested barriers, only the innermost level is
        -- honored. Maybe warn about this?
        taskify qs mnxt comp
      | otherwise = do
        let c' = case (isComputer c, mnxt) of
                   (True, Just nxt) -> MkComp (c `Seq` nxt) (compLoc c) (compInfo c)
                   _                -> c
        createTaskWithPlacement Alone c' qs
    -- If we get to either Map or Call, then the name they're calling on
    -- is a barrier for sure; make this a primitive barrier since we can't
    -- split funs.
    go (Map _ name) = do
        let c' = case (isComputer c, mnxt) of
                   (True, Just nxt) -> MkComp (c `Seq` nxt) (compLoc c) (compInfo c)
                   _                -> c
        createTaskWithPlacement Alone c' qs
    go (Call name _) = do
        let c' = case (isComputer c, mnxt) of
                   (True, Just nxt) -> MkComp (c `Seq` nxt) (compLoc c) (compInfo c)
                   _                -> c
        createTaskWithPlacement Alone c' qs
    go _ = error "Atomic computations can't possibly contain barriers!"

-- | Split a list of computations, with an extra piece of information,
--   along task barrier lines.
splitBarriers :: [(a, Comp CTy Ty)] -> [[(a, Comp CTy Ty)]]
splitBarriers cs =
  case break (containsBarrier . snd) cs of
    (front, (barrier:back)) -> front : [barrier] : splitBarriers back
    (front, [])             -> [front]

-- | Does the given computation contain a task barrier?
--   Essentially, does it have a standalone subtree?
containsBarrier :: Comp CTy Ty -> Bool
containsBarrier = containsBarrier' S.empty
  where
    containsBarrier' barriers = go . unComp
      where
        go (BindMany first rest) =
          any (containsBarrier' barriers) (first : map snd rest)
        go (Seq left right) =
          containsBarrier' barriers left || containsBarrier' barriers right
        go (Par _ left right) =
          containsBarrier' barriers left || containsBarrier' barriers right
        go (Let _ rhs comp) =
          containsBarrier' barriers comp
        go (LetE _ _ _ comp) =
          containsBarrier' barriers comp
        go (LetERef _ _ _ comp) =
          containsBarrier' barriers comp
        go (LetFunC name _ _ body comp) =
          if containsBarrier' barriers body
            then containsBarrier' (S.insert name barriers) comp
            else containsBarrier' barriers comp
        go (Interleave left right) =
          containsBarrier' barriers left || containsBarrier' barriers right
        go (Branch cond th el) =
          containsBarrier' barriers th || containsBarrier' barriers el
        go (Until cond comp) =
          containsBarrier' barriers comp
        go (While cond comp) =
          containsBarrier' barriers comp
        go (Times _ _ _ _ comp) =
          containsBarrier' barriers comp
        go (AstComp.Repeat _ comp) =
          containsBarrier' barriers comp
        go (VectComp _ comp) =
          containsBarrier' barriers comp
        go (Call n _) =
          n `S.member` barriers
        go (Map _ n) =
          n `S.member` barriers
        go (Standalone _) =
          True
        go _ =
          False
