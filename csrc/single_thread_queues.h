/* Simple queue implementation for single-thread execution (not thread-safe!!),
   with checked push, pop, and rollback operations that exit upon error.
   Runtime checks can be disabled via macro below.
*/
#pragma once

#define QUEUE_CHECKS_ENABLED true // comment this out to disable run-time checks.

#include "numerics.h"


struct queue {
	// capacity of queue
	size_t capacity;

	// size of queue elements in bytes
	size_t elem_size;

	// current number of elements in the queue
	size_t size;

	// queue buffer of size (capacity * elem_size) bytes
	void* buffer_start;

	// pointer to first memory location beyond buffer
	void* buffer_end;

	// where to write next
	void* next_write;

	// where to read from next
	void* next_read;
};


/* api **********************************************************/

void queue_init(queue* q, size_t capacity, size_t elem_size);

bool is_empty(queue* q);
bool is_full(queue* q);
size_t free_slots(queue* q);

void clear(queue* q);
void rollback(size_t n, queue* q);

void push(void* elem, queue* q);
void pushN(void* elems, size_t n, queue* q);

// unpack n bits into one byte each, and put them into the queue.
void pushNBits(void* elems, size_t n, queue* q);

void pop(void* elem, queue* q);
void popN(void* elems, size_t n, queue* q);

// get n bits (stored in 1 byte each), and pack them into (n+7)/8 bytes.
void popNBits(void* elems, size_t n, queue* q);





/***************************************************************************
  legacy api in order to use as drop in replacement for sora_thread_queues
***************************************************************************/

void stq_init(int no, size_t *sizes, int *queue_sizes);

FORCE_INLINE void stq_put(int nc, char *input);
FORCE_INLINE void stq_putMany(int nc, int n, char *input);
FORCE_INLINE void stq_putManyBits(int nc, int n, char *input);
FORCE_INLINE void stq_get(int nc, char *output);
FORCE_INLINE void stq_getMany(int nc, int n, char *output);
FORCE_INLINE void stq_getManyBits(int nc, int n, char *output);
