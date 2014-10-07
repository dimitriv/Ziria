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
  assert(sched != NULL);
  assert(sched->active_q != NULL);
  free(sched->active_q);
  free(sched);
}

void sched_activate(sched_t sched, task_t task) {
  /* TODO: implement */
}

void sched_deactivate_current(sched_t sched) {
  /* TODO: implement */
}

void sched_next(sched_t sched) {
  /* TODO: implement */
}
