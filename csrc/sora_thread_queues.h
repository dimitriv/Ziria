/* 
   Copyright (c) Microsoft Corporation
   All rights reserved. 

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
*/
#pragma once 



// The main goal of this queue is to keep evey item in a different cache line
// to minimize the amount of cache invaliations. 
// <buf> is a buffer that stores data. Each data item is of size <size>,
// We add padding 
// to <alg_size> to make it occupy an integer number of cache lines
//
// Here is an example of <size> = 4 (bytes), and ST_CACHE_LINE = 16
// | V1 PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP |        (true if the next item is valid to be read)
// | D1 D1 D1 D1 PP PP PP PP PP PP PP PP PP PP PP PP | 
// | V3 PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP |
// | D2 D2 D2 D2 PP PP PP PP PP PP PP PP PP PP PP PP |
//
// wptr is a pointer to the last reserved entry
// wdptr is a pointer to the next pushed entry
// rptr is a pointer to the next acquired entry
// rdptr is a pointer to the next released entry
// 
// Usage:
// For producing, first call 
//   buf = ts_reserve(queue, num);
// where <queue> is the pointer to the queue and <num> is the number of elements (of size <size>) to be written
// <buf> is the buffer allocated for the operation (NULL if no space). Write data to <buf> and then commit by
//   ts_push(queue, num);
// For consuming, first call 
//   buf = ts_acquire(queue, num);
// to obtain a pointer and then use 
//   ts_release(queue);
// to release it. This is dual to the calls above. 

#define ST_CACHE_LINE	64

typedef struct 
{
	// All pointers are char to facilitate pointer arithmetics
	MEM_ALIGN(ST_CACHE_LINE) char *buf;

	// Accessed by producer
	MEM_ALIGN(ST_CACHE_LINE) char *wptr;
	char *wdptr;
	// Accessed by consumer
	MEM_ALIGN(ST_CACHE_LINE) char *rptr;
	char *rdptr;

	MEM_ALIGN(ST_CACHE_LINE) int size;
	MEM_ALIGN(ST_CACHE_LINE) int alg_size;

	MEM_ALIGN(ST_CACHE_LINE) volatile bool evReset;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFinish;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFlush;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evProcessDone;

	MEM_ALIGN(ST_CACHE_LINE) int queue_size;
} ts_context;



// ************
// Queue API:


// Init the queue
ts_context *ts_init(int no, size_t *sizes, int *queue_sizes);


// Producer
void ts_put(ts_context *locCont, char *input);						// Blocking
void ts_putMany(ts_context *locCont, int n, char *input);			// Blocking
void ts_putManyBits(ts_context *locCont, int n, char *input);		// unpack bits into one byte each, and put them into the queue 



// Consumer
bool ts_isFinished(ts_context *locCont);
bool ts_get(ts_context *locCont, char *output);
int ts_getMany(ts_context *locCont, int n, char *output);
int ts_getManyBlocking(ts_context *locCont, int n, char *output);
int ts_getManyBits(ts_context *locCont, int n, char *output);		// get n bits (stored in 1 byte each), and pack them into (n+7)/8 bytes
int ts_getManyBitsBlocking(ts_context *locCont, int n, char *output);


// Issued from upstream to downstream
void ts_reset(ts_context *locCont);
// Issued from upstream to downstream
void ts_flush(ts_context *locCont);
// Issued from upstream to downstream
void ts_finish(ts_context *locCont);
bool ts_isEmpty(ts_context *locCont);
bool ts_isFull(ts_context *locCont);


// Clear the entire queue
// WARNING: Unlike the rest of the code, this is not thread-safe 
// and might require a lock, depending on the use
void ts_clear(ts_context *locCont);

// Roll back up to n elements from the queue, as many as available
// WARNING: Unlike the rest of the code, this is not thread-safe 
// and might require a lock, depending on the use
void ts_rollback(ts_context *locCont, int n);


// Free memory allocated for queues
void ts_free();





// The following functions are non-blocking and the caller should spin-wait if required
// Producer
char *ts_reserve(ts_context *locCont, int num);
bool ts_push(ts_context *locCont, int num);
// Consumer
char *ts_acquire(ts_context *locCont, int num);
bool ts_release(ts_context *locCont, int num);





#ifndef __linux__
// For barriers
#include <windows.h>
#include<synchapi.h>


extern LONG volatile * barr_hist1;
extern LONG volatile * barr_hist2;

// Simple barrier
inline void _barrier(LONG volatile *barrier, LONG volatile *to_reset, int no_threads, int thr)
{
	// Reset old barrier
	if (thr == 0 && to_reset != NULL)
	{
		*to_reset = 0;
	}

	// Check new one
	InterlockedIncrement(barrier);
	MemoryBarrier();
	while (*barrier < no_threads)
	{
		MemoryBarrier();
	}
}


// When we come to a goto, we wait at the <state> barrier related to the current state
inline void barrier(LONG volatile *state, int no_threads, int thr)
{
	_barrier(state, barr_hist1, no_threads, thr);
	_barrier(state + 1, barr_hist2, no_threads, thr);
	_barrier(state + 2, state, no_threads, thr);
	if (thr == 0)
	{
		barr_hist1 = state + 1;
		barr_hist2 = state + 2;
	}
}
#endif


