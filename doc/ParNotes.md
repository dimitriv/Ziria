Ziria parallelism design notes
==============================

1. Our current situation
------------------------

Ziria currently has no good story for parallelism. At present, pipelines can be
split into separate threads using the `|>>>|` operator, which is nice, but only
on the top level. Parallelism is thus not very compositional, and in particular
is completely impossible to use under a bind. Consider the following program:

    x <- t |>>>| c1 ; c2 x

In this program, `t` and `c1` run in parallel; `t` is thus completely
independent of `c1`, happily `take`ing and `emit`ing data in its own little
bubble, without a care in the world for anything outside it. However, being a
computer, `c` might decide to terminate at some point. When this happens, `t`
terminates as well. However, since `t` runs independently, it may at this point
have consumed some data which was not needed by `c1` and should instead have
been consumed by `c2` - now that data is lost forever, and the program will
most likely yield incorrect results.


2. High level overview
----------------------

To improve on this situation, something along the lines of the following high
level design is to be implemented.

First, we generate an *execution map* as described in section 5. From this
map, we figure out which parts of the program may be run in parallel and
attempt to construct a schedule, guided by annotations from the programmer,
which gives us as much parallelism as possible.

Once we've figured out which parts of the program are going to run in
parallel, we insert queues and synchronization code between the different
tasks as described in section 3.

After this, we decide which tasks run on which core and compile each task
as a separate program. Finally, we compile the scheduler as the entry point
for each separate core, with the appropriate task configuration.


3. Task synchronization
------------------------

To keep tasks synchronized, we use two different kinds of queues: normal
Sora synchronization queues and the *commit queues* described in section 4.

In general, we use a commit queue to synchronize between `t1` on one side and
`t2` and `c2` on the other side for expressions of the form
`t1 >>> seq {t2 |>>>| c1 ; c2}`, where the queue has at least two consumers,
and Sora synchronization queues whenever a queue has only a single consumer.

Using commit queues also require some additional bookkeeping. In the above
example, whenever `c2` issues a `take` operation, that operation is seen as
a confirmation of all upstream `take`s needed to produce the `take`n element.
Thus, `c1` calculates how many elements from the commit queue feeding `t2`
have been needed so far, and `commit` that many reads on the queue.

When `c1` finishes, we `rollback` any uncommitted reads on the input commit
queue, to ensure it is in the correct state when `c2` starts reading from it.

How do we calculate how many reads to commit on each `take` performed by
`c1`? Assuming that we know the cardinality information for `t2` and any
intervening transformers, we can calculate how many elements to commit using
the following method:

In case of 

    t1(n1/m1) >>> t2(n2/m2) >>> c

for each `k` takes by `c`, we can commit `j=ceil(k/m2)*n2` takes by `t2`.
Similarly, for each `j` takes by `t2` we can commit `ceil(j/m1)*n1` takes
by `t1`. Thus, for each `k` takes by `c` we can commit

   ceil(ceil(k/m2)*n2/m1)*n1

elements from the commit queue of `t1`.

The calculation can be expressed as the following program:

    takes :: [(Int, Int)] -> Int -> Int
    takes cards n = foldr go n cards
      where
        go (is, os) ts = ceiling (fromIntegral ts/fromIntegral os)*is

We also need to keep track of how many elements we have already committed,
as not all takes in `c` will result in the same number of commits on the
commit queue. Compensating for previous takes can be expressed as follows:

    takesWithHistory :: [(Int, Int)] -> Int -> Int -> Int
    takesWithHistory cards previous n =
      takes cards (previous+n) - takes cards previous

This constant committing may be expensive in practice, so we want to batch
as much of it as possible. This can be accomplished by allowing the
transformer reading from the commit queue to just keep running until the
queue is full. When that happens, notify the downstream computer that it
needs to commit all of its `take`s, rinse and repeat until the computer
terminates. Before terminating, the computer once again commits all of its
outstanding `take`s before performing a `rollback` on all uncommitted of
the commit queue.

Doing this, we need to ensure that the commit queue serving the task has
space for enough elements to serve at least one `take` by the computer,
otherwise the whole thing will deadlock and everyone will be sad. Fortunately,
calculating how many elements we need is easy:

    minQSize cards = takes cards 1

Of course, the actual queue size should be a bit larger than this.


4. Commit queues
----------------

Commit queues are a special kind of synchronization primitive, improving on
the normal Sora synchronization queues by adding two new operations, `commit`
and `rollback`, and the concept of *optimistic reads*.

A commit queue starts out with an *optimistic read pointer* in addition to the
traditional front and back pointers. When a read is performed on the queue,
the element pointed to by the optimistic read pointer is returned, and the
optimistic read pointer is advanced to the next position in the queue.

As the front of the queue does not change, the read element is not removed from
the queue. Thus, a series of read operations on such a queue can be rolled back
using the `rollback` operation, which sets the optimistic read pointer to the
front of the queue.

The final operation, `commit`, advances the front of the queue by `n` elements,
marking the last `n` optimistic reads as confirmed. They are now set in stone
and can no longer be rolled back.

Commit queues have the following invariants:

* The back pointer can never advance past the front pointer, as this would
  overwrite data which has been optimistically read but not yet committed.

* The front pointer never advances past the optimistic read pointer, as this
  is completely nonsensical; it's not possible to commit a read that never
  happened in the first place.

* The optimistic read pointer never advances past the back pointer, as this
  would amount to reading data that has not yet been written to the queue.


5. Execution maps
-----------------

The flowcharts mapping the control and data flows of Ziria programs can be
seen as two dimensional maps, with the horizontal dimension representing
data pipelines and the vertical dimension representing time. Looking at such
a map, it becomes relatively easy to figure out which parts of a program can
run in parallel.

We want to generate such maps from the Ziria AST and be able to generate
static execution schedules from them. Additionally, we would like to be able
to dump these maps to the console or a file in some format which lets the
user visually inspect them to get a better understanting of their program's
structure and bottlenecks.

Looking at the execution map, we ought to be able to figure out the maximum
number of tasks any core will ever need to deal with at the same time as
well as how many it will need to run in total.

WRT the execution map, I'm thinking it might be a good idea to use something
simple to represent it, along the lines of:

    data Map where
      Seq :: [Map] -> Map
      Par :: [Map] -> Map
      Repeat :: Map -> Map
      Atom :: Task -> Map

Where a task is basically just an ID of a task, we can keep the actual tasks
in a store somewhere. For the most general case, we can assume that each
component is its own task. In reality, this will of course not be the case.
You could say it's a bird's view of the AST, perhaps.

Now, this map is kind of useless as an execution *plan* - I can't really look
at it and decide what to execute next. It would have been nice if we could use
this map to generate a simple, static, steppable schedule like this, where each
row contains the tasks that are to be run concurrently, and we can just
advance to the next row of the schedule whenever the preceeding one finishes:

    [
        ["task 1", "task 2"],
        ["task 3"],
        ["task 4", "task 5", "task 6", "task 7"]
    ]

Unfortunately, this isn't really possible. Consider the program
`seq {a ; b} >>> seq {c ; d}`. The first line of the schedule would obviously
be `["a", "c"]`, but the next one? Is it ["b", "c"] or ["a", "d"]? I guess you
could solve that by merging sequenced tasks somehow, but that seem like it'd
be a bit nasty.

Instead, each computation task c should have an associated list of tasks that
should start after it returns done. The scheduler will have to wait for any
transformers downstream of c to finish whatever they're doing somehow, and then
start c's "followup tasks". This part of the scheduler should be inlined into
tasks, so we don't have to keep an explicit list of things to wait for but can
just inline the logic itself. Conceptually, an execution plan would thus look
as follows:

    data Plan = Plan {
        planContinuations   :: Map (Task Comp) (Set AnyTask),
        planLastTask        :: Map (Task Comp) AnyTask,
        planFirstTask       :: Map (Task Comp) AnyTask,
        planInitiallyActive :: Set AnyFiber
      }

Note that `planLastTask` takes an `AnyTask` rather than a `Task Trans`. This is
because even though we can't compose two computers in a pipeline, the final
transformer in a pipeline may well be of the form `repeat (a ; b)`.


6. Scheduler
------------

Evaluation of the program will be driven by a scheduler, which is responsible
for keeping track of what component needs to be ticked in order to ensure
that the program progresses. The scheduler is static and knows how many tasks
it's dealing with. On core initialization the scheduler is the first thing to
run, and starts scheduling threads according to the following algorithm:

* Pick the next task from the ready queue and check whether the thread was
  ticked. If it was not, tick it and mark it as ticked. If the tick function
  immediately yields a result, process that result as well. If it needs to
  consume data, proceed with the next task in the queue.
  If the tick function returns done, remove the task from the ready queue,
  as it will not come back to life again.

* If the task was already ticked, check whether its input queue is empty.
  If it is, continue with the next thread in the queue. If there is some data
  in the queue, dequeue it and pass it to the task's process function, then
  proceed with the next task.

If the scheduler has no runnable tasks, it just spins until someone - most
likely the scheduler on another core - puts a runnable task in its ready
queue.

Since we know statically how many tasks we're dealing with on each core, we
can allocate a ring buffer of just the right size to hold any task data.

Rather than using fibers, as previously discussed, the scheduler should be
a simple trampoline-like loop dispatching functions. The main advantage of
fibers is that it preserves context, allowing us to continue execution where
we left off. However, we don't need that - the scheduler only gets control
whenever a computation either blocks waiting for data or is done with its
tick or proc function. Since we always have a well defined point for
resuming a task, we simply don't care about being able to resume tasks from
wherever.

We might also want to be smarter about when to schedule a thread that is to
be proc'd. If one task's input queue is filling up fast, we may want to
process that task more often, for instance.

The scheduler will need to keep some data about each task. That data
structure will need to look something like this:

    data TaskData = TaskData {
        taskWasTicked :: Bool,
        taskCanRun    :: IO Bool,
        taskTickFun   :: Ziria ResultOrConsume,
        taskProcFun   :: SomeData -> Ziria Result
      }

`taskCanRun` has type `IO :: Bool` to signify that whether a task can run
or not will change outside the control of the scheduler, for instance when
someone writes an element to its input queue. `taskWasTicked`, on the other
hand, is only ever updated by the scheduler, and the tick and proc functions
obviously never change.

This data can then be kept in a ring buffer-like structure, where the
take-like operation doesn't remove any elements but merely advances the
take pointer around the ring. Thus, each take amounts to picking the next
active task. We'll need a `removeCurrentTask` operation as well.

It might be more efficient in some cases to keep two structures: one for
runnable tasks and one for activated but not runnable ones. May be worth
exploring down the line.

It might also be worth exploring ways of extracting parallelism automatically.
If we can statically figure out which parts of the program are extra heavy,
apart from the user's annotations, we could have a work stealing scheduler to
make use of any core not currently doing anything useful. Putting any idle
cores into a pool for use by data parallel computations would also be nice.


7. Annotations
--------------

The programmer will be able to specify which computations are to run on their
own cores using the `standalone` notation. A computation

    a >>> standalone b >>> c

will turn into something like

    a@task1 >>> b@alone:task2 >>> c@task3

in the AST, indicating that `a`, `b` and `c` are to run in their own tasks,
and that `b` will additionally be placed on its own core if at all possible.

We should probably also emit some kind of warning when we end up with more
`standalone` components than we have cores for.


8. Synchronization
------------------

When we have a chain of the form `t1 >>> t2 >>> t3 >>> t4 >>> t5`, we have
to do the following synchronization:

* When `c` is done, he must send a FINISHED signal upstream and downstream.
  This may be accomplished using the ts_finish operation on sync queues, or
  it may be accomplished using some other shared variable. Then, it
  terminates and launches a new `c'` component. This component waits for the
  first and last downstream components *within* a sequence; in this case
  `t1` and `t5`, to finish. When this happens, `c'` rolls back any
  uncommitted changes to the input queue, launches all tasks that need to
  run next (in this example there are none, since there is no sequence),
  and terminates.

* When a component detects that its downstream says FINISHED, it propagates
  the signal upstream and terminates, iff it is not the last upstream
  component within a sequence. If it is, it simply terminates.

* When a component detects that its upstream says FINISHED *and* that its
  input queue is empty, it propagates the signal downtream and terminates,
  iff it is not the last downstream component within a sequence.
  If it is, it simply terminates.

We need to differentiate between a process dying because the upstream is
FINISHED and its input stream is empty, and it terminating on its own:
in the case of `repeat c`, `c` should be restarted whenever it terminates
normally, but not if it terminates because it's got no input left.

The code for the `read` and `write` operations on queues, performing part
of this functionality might look as follows, in pseudo-python-javascript:

    read.tick = function() {
      if(input_queue.is_empty) {
        if(input_queue.is_finished) {
          if(!self.last_in_pipeline || self.last_in_sequence) {
            output_queue.set_finished(true);
  	  }
          self.deactivate();
          self.set_task_finished(true); // read by terminating computer
        }
        else {
          SKIP;
        }
      } else {
        IMMEDIATELY(YIELD(input_queue.read()));
      }
    }

    write.tick = function() {
      if(output_queue.is_finished) {
        if(!self.first_in_pipeline || self.last_in_sequence) {
          input_queue.set_finished(true);
        }
        self.deactivate();
        self.set_task_finished(true); // read by terminating computer
      } else {
        if(input_queue.is_full) {
          SKIP;
        } else {
          CONSUME;
        }
      }
    }

    write.process = function(x) {
      output_queue.write(x);
      SKIP;
    }

We should propagate the self.first_in_pipeline, etc. info by at compile
time to avoid unnecessary runtime checks. It's important to note that
in the case of e.g. `repeat {a ; b}`, `b` is *not* the last in its
sequence.


9. Compiling tasks
------------------

By default, all components - transformers and computers alike - run in
their own task conceptually. We then merge as many components together
as we can manage. In practice, the following constructs will act as
barriers to this merging:

* `standalone` computations. Obviously. We can merge its upstream into
  one task and its downstream into one (assuming the absence of any other
  barrier constructs), resulting in three tasks in total.

* `BindMany` where one of the bound components contain a barrier construct.
  In this case, we can merge all computations before and all
  that come after, again resulting in three tasks.

* `a >>> (b >>> c_containing_barrier >>> d) >>> e` - due to silly placement
  of parentheses, all five components must run standalone unless we do some
  preprocessing to normalise parentheses.

* Conditionals where either branch contains a barrier construct. These get
  compiled into `if(x) then activate left else activate right`, where
  `left` and `right` are the *first* sequential component of each branch.
  Note that if, for instance, `left` is `a >>> b >>> c`, we must activate
  all those tasks.

Some combinators, such as `repeat`, `until`, `times`, etc. must have a small
piece of dynamic scheduling logic, activating different tasks depending on the
circumstances. For instance, `until` should not terminate but just reset itself
unless its inner components die by starvation or its condition becomes true;
and `repeat` should not ever terminate but rather reset, unless its internal
components die by starvation.

The appropriate synchronization would get inlined into tasks like this:

    compTask plan task = mconcat [
        codeFor task,
	deactivateSelf,
        activatePhaseTwo
      ]

    -- Here we want to split the sync into two different tasks, since
    -- we can't busy wait for tasks without blocking the scheduler.

    compTask2 plan task = mconcat [
        unless (done (planLastTask plan ! task)) skip,
        unless (done (planFirstTask plan ! task)) skip,
	resetCommitQueue taskInputQueue,
        mconcat $ Set.map activateTask (planContinuations plan ! task)
      ]

Note the `deactivateSelf` rather than something along the lines of
`deactivateTask task`; the scheduler can find the currently running task in
O(1) time, but takes O(n) time to find an arbitrary task by its ID.
