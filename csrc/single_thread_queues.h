/* Simple queue implementation for single-thread execution (not thread-safe!!),
   with checked push, pop, and rollback operations that exit upon error.
   Runtime checks can be disabled via macro below.
*/
#pragma once

// #define QUEUE_CHECKS_ENABLED // comment this out to disable run-time checks.

#include "types.h"
#include "numerics.h" // include for the FORCE_INLINE macro


typedef struct {
	// where to write next
	char* next_write;

	// where to read from next
	char* next_read;

	// capacity of queue
	size_t capacity;
	// size of queue elements in bytes
	size_t elem_size;

	// current number of elements in the queue
	size_t size;

	// queue buffer of size (capacity * elem_size) bytes
	char * buffer_start;

	// pointer to first memory location beyond buffer
	char* buffer_end;

} queue;



queue *stq_init(int no, size_t *sizes, int *queue_capacities);

char * stq_acquire(queue *q);
void stq_release(queue *q);

char* stq_reserve(queue *q);
void stq_push(queue *q);

void stq_clear(queue *q);
void stq_rollback(queue *q, size_t n);
