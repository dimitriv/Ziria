#include <stdlib.h>
#include <memory.h>
#include "single_thread_queues.h"
#include "bit.h"

void _stq_init(queue* q, size_t capacity, size_t elem_size) {
	q->capacity = capacity;
	q->elem_size = elem_size;
	q->size = 0;
	q->buffer_start = malloc(capacity * elem_size);
	if (q->buffer_start == NULL) {
		exit(EXIT_FAILURE);
	}
	// buffer_end points at the first memory location beyond the buffer
	q->buffer_end = (char*)q->buffer_start + capacity * elem_size;
	q->next_write = q->buffer_start;
	q->next_read = NULL;
	q->acquired = 0;
	q->reserved = 0;
}

/* Queue reader API 
 ***************************************************************************/

// Peek n slots into the queue (if aligned, throw error otherwise, or perhaps emit slow path -- todo)
FORCE_INLINE void* _stq_acquire(queue *q, size_t slots)
{
	// acquire n slots
	q->acquired += slots;

#ifdef QUEUE_CHECKS_ENABLED
	if ((uint)q->next_read + q->acquired*q->elem_size > (uint)q->buffer_end)
	{
		printf("Unaligned queue read!");
		exit(EXIT_FAILURE);
	}
#endif
	return q->next_read;
}

// Release the acquired slots 
FORCE_INLINE void _stq_release(queue *q)
{
	void *ptr = q->next_read;
	ptr = (void*)((uint)ptr + q->acquired*q->elem_size);
	if (ptr == q->buffer_end) ptr = q->buffer_start;
	q->acquired = 0;
	q->next_read = ptr;
}

/* Queue write API
***************************************************************************/

FORCE_INLINE void* _stq_reserve(queue *q, size_t slots)
{
	// reserve n slots
	q->reserved += slots;

#ifdef QUEUE_CHECKS_ENABLED
	if ((uint)q->next_write + q->reserved*q->elem_size > (uint)q->buffer_end)
	{
		printf("Unaligned queue write!");
		exit(EXIT_FAILURE);
	}
#endif
	return q->next_write;
}

FORCE_INLINE void _stq_push(queue *q)
{
	void *ptr = q->next_write;
	ptr = (void*)((uint)ptr + q->reserved*q->elem_size);
	if (ptr == q->buffer_end) ptr = q->buffer_start;
	q->reserved = 0;
	q->next_write = ptr;
}


FORCE_INLINE void _stq_clear(queue *q)
{
	q->acquired = 0;
	q->reserved = 0;
	q->next_read = NULL;
	q->next_write = q->buffer_start;
	q->size = 0;
}

FORCE_INLINE void _stq_rollback(size_t n, queue* q) {
	void *ptr = (void *)((uint)q->next_read - (q->elem_size*n));
#ifdef QUEUE_CHECKS_ENABLED
	ASSERT(q->acquired == 0 && q->reserved == 0);
	if (ptr < q->buffer_start)
	{
		printf("Unaligned queue rollback!");
		exit(EXIT_FAILURE);
	}
#endif
	q->next_read = ptr;

}

/* Top-level API 
 *********************************************************************/

static queue *queues;

void stq_init(int no, size_t *sizes, int *queue_capacities) {
	queues = (queue *) malloc(no * sizeof(queue));

	if (queues == NULL) exit(EXIT_FAILURE);

	for (size_t i = 0; i < no; i++)
	{
		_stq_init(&queues[i], queue_capacities[i], sizes[i]);
	}
}

FORCE_INLINE void* stq_acquire(int no, queue *q, size_t slots)
{
	return _stq_acquire(&queues[no], slots);
}

// Release the acquired slots 
FORCE_INLINE void stq_release(int no)
{
	_stq_release(&queues[no]);
}

FORCE_INLINE void* stq_reserve(int no, size_t slots)
{
	return _stq_reserve(&queues[no], slots);
}

FORCE_INLINE void stq_push(int no)
{
	_stq_push(&queues[no]);
}


FORCE_INLINE void stq_clear(int no)
{
	_stq_clear(&queues[no]);
}

FORCE_INLINE void stq_rollback(int no, size_t n, queue* q) {
	_stq_rollback(n, &queues[no]);
}
