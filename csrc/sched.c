#include <stdlib.h>
#include <assert.h>
#include "sched.h"

sched_t sched_create(int qsize) {
  sched_t sched = malloc(sizeof(struct sched_struct));
  assert(sched != NULL);
  sched->active_q = malloc(sizeof(task_t)*qsize);
  assert(sched->active_q != NULL);
  return sched;
}

void sched_destroy(sched_t sched) {
  /* Invariant: allocation always succeeds */
  assert(sched != NULL);
  assert(sched->active_q != NULL);
  free(sched->active_q);
  free(sched);
}

void sched_activate(sched_t sched, task_t task) {
  /* Invariant: queue must not be full */
  assert(sched->active_tasks < sched->max_tasks);
  sched->active_q[sched->active_tasks++] = task;
}

/* Remove a task by overwriting it with the task at the
   front of the buffer, then shrinks the buffer by 1.
   This obviously changes the order of execution of tasks,
   causing the last added task to possibly be delayed a bit,
   so this may be a good place to start when
   the time comes to tune the scheduler.
 */
void sched_deactivate_current(sched_t sched) {
  int front = sched->active_tasks - 1;
  /* Invariant: queue must not be empty */
  assert(front >= 0);
  sched->active_q[sched->cur_task] = sched->active_q[front];
  sched->active_q[front] = NULL;
  --sched->active_tasks;
  sched->cur_task %= sched->active_tasks;
}

int sched_next(sched_t sched) {
  if(sched->active_tasks == 0) {
    return -1;
  }
  sched->active_q[sched->cur_task]();
  sched->cur_task = (sched->cur_task + 1) % sched->active_tasks;
  return 0;
}
