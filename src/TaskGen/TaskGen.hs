{-# LANGUAGE GADTs, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}
-- | Rewrite an optimized, renamed and generally mangled AST
--   into an AST utilizing the new fancy scheduler.
module TaskGen (
    insertTasks
  ) where
import Control.Applicative
import Control.Monad.State
import qualified Data.Set as S
import Text.Parsec.Pos (SourcePos)

import AstComp hiding (comp)
import AstExpr hiding (name, info)
import Outputable
import TaskGenTypes
import TaskGenMonad

-- | Split program into entry points and tasks to be started along the road.
insertTasks :: Comp CTy Ty -> (TaskEnv (Comp CTy Ty), Comp CTy Ty)
insertTasks = runTaskGen . go
  where
    go comp = do
      tid <- taskify (ExtReadQ, ExtWriteQ) dontSync comp
      taskComp <$> lookupTask tid

-- | Split the given AST along task barriers. The returned task is the entry
--   point of the program, from which further tasks are spawned as needed.
--   In the absence of `standalone` annotations, this generates a single
--   task containing the entirety of the program.
--
--   Currently, only 'Standalone' creates a primitive barrier. |>>>| is treated
--   like >>>.
--
--   TODO: the compInfo stuff (the types) are probably incorrect for parts.
--
--   TODO: current algorithm, calling containsBarrier all over the place, is
--         O(n^2) - could be O(n) if we instead annotated the AST as we
--         traverse it.
--
--   TODO: currently doesn't do synchronization at start/end of tasks.
--
--   TODO: currently doesn't calculate cardinalities for commit queues.
--
--   TODO: don't generate unnecessary queue reads/writes
taskify :: (Queue, Queue)
        -> SyncInfo
        -> Comp CTy Ty
        -> TaskGen TaskID (TaskInfo (Comp CTy Ty)) TaskID
taskify qs@(inq, outq) snfo c = do
    if containsBarrier c
      then go (unComp c)
      else createTask c qs (Just snfo)
  where
    -- Invariant: if go gets called, its argument *does* contain a barrier.
    go :: Comp0 CTy Ty -> TaskGen TaskID (TaskInfo (Comp CTy Ty)) TaskID
    go (BindMany first rest)
      | containsBarrier first = do
          let ((v, comp):xs) = rest
          nxt <- taskify qs snfo (MkComp (BindMany comp xs) (compLoc comp) (compInfo c))
          taskify qs (justStart [nxt]) first
      | otherwise = do
          genBindManyBarriers first (splitBarriers rest)
      where
        -- Invariant: each (first ++ b) contains a barrier.
        genBindManyBarriers frst (b:bs) = do
          case bs of
            (((_, first'):rest'):bs') -> do
              -- Next task is just the next in the list of barriers
              nxt <- genBindManyBarriers first' (rest':bs')
              taskify qs (justStart [nxt]) $ MkComp (BindMany frst b) (compLoc frst) (compInfo c)
            _                        -> do
              -- End of bind; sync info is whatever came from the outside
              taskify qs snfo $ MkComp (BindMany frst b) (compLoc frst) (compInfo c)
        genBindManyBarriers _ [] = do
          error "BUG: barrier is in first comp of BindMany, but genBindManyBarriers was called!"

    go (Seq _ _) =
      -- Seq should be in source AST only
      error "Seq appeared during task generation!"

    go (Par _ _ _) = do
      -- Generating tasks for Par:
      --   For each component:
      --     If component is a computer:
      --       Pass full sync info, merged with the one coming from outside.
      --       Info from outside will always be justStart ts.
      --     Else:
      --       If component is the first in the pipeline:
      --         Pass dontSync.
      --       Else:
      --         Pass justClose outq.
      let bars = findBarriers c
      queues <- mapM (const freshQueue) (drop 1 bars)
      let syncsnfo = undefined
          compsnfo = syncsnfo `withStartsFrom` snfo
          bars_qs = zip3 bars (inq:queues) (queues ++ [outq])
      taskIds <- foldM (taskify' compsnfo) [] $ reverse bars_qs
      createTask (cSync (compLoc c) (compInfo c) (justStart taskIds)) qs Nothing
      where
        taskify' compsnfo downstream (c', qin', qout')
          | isComputer c' = do
            (: downstream) <$> taskify (qin', qout') compsnfo c'
          | [] <- downstream = do
            (: []) <$> taskify (qin', qout') dontSync c'
          | otherwise = do
            (: downstream) <$> taskify (qin', qout') (justClose qout') c'

        -- Turn a >>> standalone b >>> c >>> d into [a, standalone b, c >>> d]
        findBarriers p@(MkComp (Par _ l r) _ _) =
          case (containsBarrier l, containsBarrier r) of
            (True, True)  -> findBarriers l ++ findBarriers r
            (True, False) -> findBarriers l ++ [r]
            (False, True) -> l:findBarriers r
            _             -> [p]
        findBarriers p = [p]

    -- Let bindings will be declared on the top level, so we keep them around
    -- in the AST just to have their values properly assigned.
    go (Let name rhs comp) = do
      tid <- taskify qs snfo comp
      updateTaskComp tid $ \c' ->
        MkComp (Let name rhs c') (compLoc c) (compInfo c)
      return tid
    go (LetE name inl rhs comp) = do
      tid <- taskify qs snfo comp
      updateTaskComp tid $ \c' ->
        MkComp (LetE name inl rhs c') (compLoc c) (compInfo c)
      return tid
    go (LetERef name ty rhs comp)  = do
      tid <- taskify qs snfo comp
      updateTaskComp tid $ \c' ->
        MkComp (LetERef name ty rhs c') (compLoc c) (compInfo c)
      return tid

    go (LetFunC name args locs body comp) = do
      -- We don't go into the body of the function since we simply can't split
      -- non-inlined functions into tasks.
      when (containsBarrier body) $ do
        addBarrierFun name
      tid <- taskify qs snfo comp
      updateTaskComp tid $ \c' ->
        MkComp (LetFunC name args locs body c') (compLoc c) (compInfo c)
      return tid

    go (Interleave _ _) =
      error "BUG: Interleave in AST!"
    go (Branch cond th el) = do
      thID <- taskify qs snfo th
      elID <- taskify qs snfo el
      let th' = cSync (compLoc th') (compInfo th') (justStart [thID])
          el' = cSync (compLoc el') (compInfo el') (justStart [elID])
      createTask (MkComp (Branch cond th' el') (compLoc c) (compInfo c)) qs Nothing
    go (Until _cond _comp) = do
      error "TODO: until requires some extra code"
    go (While _cond _comp) = do
      error "TODO: while requires some extra code"
    go (Times _ui _from _to _it _comp) = do
      error "TODO: times requires some extra code"
    go (AstComp.Repeat _mvect _comp) = do
      error "TODO: repeat requires some extra code"
    go (VectComp vect comp) = do
      -- TODO: cardinality information needed to inform the counting
      --       not necessarily right here though?
      tid <- taskify qs snfo comp
      updateTaskComp tid $ \comp' ->
        MkComp (VectComp vect comp') (compLoc c) (compInfo c)
      return tid
    go (Standalone comp)
      | containsBarrier comp = do
        -- If we run into nested barriers, only the innermost level is
        -- honored. Maybe warn about this?
        taskify qs snfo comp
      | otherwise = do
        createTaskWithPlacement Alone c qs (Just snfo)
    -- If we get to either Map or Call, then the name they're calling on
    -- is a barrier for sure; make this a primitive barrier since we can't
    -- split funs.
    go (Map _ _) = do
        createTaskWithPlacement Alone c qs (Just snfo)
    go (Call _ _) = do
        createTaskWithPlacement Alone c qs (Just snfo)
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
        go (Let _ _rhs comp) =
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
        go (Branch _ th el) =
          containsBarrier' barriers th || containsBarrier' barriers el
        go (Until _ comp) =
          containsBarrier' barriers comp
        go (While _ comp) =
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
