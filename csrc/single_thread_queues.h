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
	unsigned char* next_write;

	// where to read from next
	unsigned char* next_read;

	// capacity of queue
	size_t capacity;
	// size of queue elements in bytes
	size_t elem_size;

	// current number of elements in the queue
	size_t size;

	// queue buffer of size (capacity * elem_size) bytes
	unsigned char * buffer_start;

	// pointer to first memory location beyond buffer
	unsigned char* buffer_end;

} queue;


queue *stq_init(int no, size_t *sizes, int *queue_capacities);

unsigned char * stq_acquire(int no, size_t slots);
void stq_release(int no, size_t slots);

unsigned char* stq_reserve(int no, size_t slots);
void stq_push(int no, size_t slots);

void stq_clear(int no);
void stq_rollback(int no, size_t n);

unsigned char * _stq_acquire(queue *q, size_t slots);
void _stq_release(queue *q, size_t slots);

unsigned char* _stq_reserve(queue *q, size_t slots);
void _stq_push(queue *q, size_t slots);

void _stq_clear(queue *q);
void _stq_rollback(queue *q, size_t n);

