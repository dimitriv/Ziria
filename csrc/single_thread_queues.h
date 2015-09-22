/* Simple queue implementation for single-thread execution (not thread-safe!!),
   with checked push, pop, and rollback operations that exit upon error.
   Runtime checks can be disabled via macro below.
*/
#pragma once

#define QUEUE_CHECKS_ENABLED 1 // comment this out to disable run-time checks.

#include "types.h"
#include "numerics.h" // include for the FORCE_INLINE macro


typedef struct {
	// capacity of queue
	size_t capacity;

	// size of queue elements in bytes
	size_t elem_size;

	// current number of elements in the queue
	size_t size;
	size_t acquired;
	size_t reserved;

	// queue buffer of size (capacity * elem_size) bytes
	void* buffer_start;

	// pointer to first memory location beyond buffer
	void* buffer_end;

	// where to write next
	void* next_write;

	// where to read from next
	void* next_read;
} queue;


void stq_init(int no, size_t *sizes, int *queue_capacities);
void* stq_acquire(int no, queue *q, size_t slots);
void stq_release(int no);
void* stq_reserve(int no, size_t slots);
void stq_push(int no);
void stq_clear(int no);
void stq_rollback(int no, size_t n, queue* q);

