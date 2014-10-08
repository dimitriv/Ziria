/* Ziria scheduler.

   The scheduler is essentially a ring buffer, and tasks are scheduled
   in a round-robin fashion. Having only a single entry point, scheduling
   a task consists of just calling it.

   Please note that the scheduler is currently *not* thread safe!

   TODO: will need some optimizaion and cache tuning.
   TODO: thread safety and finding idle cores to start tasks on.
 */
#ifndef SCHED_H

/* Pointer to a Ziria tick function. */
typedef void (*task_tick_t)(int);

/* Task info for the scheduler.
   Currently only tick function and a var to keep track of
   whether the task's been initialized or not.
*/
typedef struct task_struct {
  task_tick_t tick;
  int initialized;
} *task_t;

/* State of the scheduler. */
typedef struct sched_struct {
  /* Active queue. */
  task_t *active_q;

  /* Current task. */
  int cur_task;

  /* Current # of active tasks. */
  int active_tasks;

  /* Max # of concurrent tasks. */
  int max_tasks;
} *sched_t;

/* Create a new scheduler. */
sched_t sched_create(int);

/* Put a task onto the active queue. */
void sched_activate(sched_t, task_t);

/* Remove the currently running task from the active queue. */
void sched_deactivate_current(sched_t);

/* Activate the next task in the active queue.
   Returns 0 if a task could be scheduled, otherwise nonzero.
*/
int sched_next(sched_t);

/* Destroy a scheduler and free any associated memory. */
void sched_destroy(sched_t);

#endif /* SCHED_H */
