/* Ziria scheduler.

   The scheduler is essentially a ring buffer, and tasks are scheduled
   in a round-robin fashion. Having only a single entry point, scheduling
   a task consists of just calling it.

   TODO: will need some fine tuning for playing as nice with
         cache as the rest of the C code.
 */
#ifndef SCHED_H

/* Pointer to a Ziria tick function. */
typedef void (*task_tick_t)();

/* Task info for the scheduler. Currently only tick function. */
typedef task_tick_t task_t;

/* State of the scheduler. */
typedef struct sched_struct {
  /* Active queue. */
  task_t *active_q;
} *sched_t;

/* Create a new scheduler. */
sched_t sched_create(int);

/* Put a task onto the active queue. */
void sched_activate(sched_t, task_t);

/* Remove the currently running task from the active queue. */
void sched_deactivate_current(sched_t);

/* Activate the next task in the active queue. */
void sched_next(sched_t);

/* Destroy a scheduler and free any associated memory. */
void sched_destroy(sched_t);

#endif /* SCHED_H */
