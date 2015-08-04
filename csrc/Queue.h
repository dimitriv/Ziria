/* Simple queue implementation for single-thread execution (not thread-safe!!),
   with checked push, pop, and rollback operations that exit upon error.
   Runtime checks can be disabled via macro below.
*/
#pragma once
#define QUEUE_CHECKS_ENABLED true // comment this out to disable run-time checks.

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
typedef queue ts_context;
ts_context *queues;

int ts_init_var(int no, size_t *sizes, int *queue_sizes) {
	queues = (ts_context *)(malloc(no * sizeof(ts_context)));
	if (queues == NULL) {
		return 0;
	}
	
	for (size_t i = 0; i < no; i++)
	{
		queue_init(&queues[i], queue_sizes[i], sizes[i]);
	}
}

__forceinline void ts_put(int nc, char *input) {
	push(input, &queues[nc]);
}

__forceinline void ts_putMany(int nc, int n, char *input) {
	pushN(input, n, &queues[nc]);
}

__forceinline void ts_putManyBits(int nc, int n, char *input) {
	pushNBits(input, n, &queues[nc]);
}

__forceinline bool ts_get(int nc, char *output) {
	pop(output, &queues[nc]);
	return true;
}

__forceinline int ts_getManyBlocking(int nc, int n, char *output) {
	popN(output, n, &queues[nc]);
	return n;
}

__forceinline int ts_getManyBitsBlocking(int nc, int n, char *output) {
	popNBits(output, n, &queues[nc]);
	return n;
}