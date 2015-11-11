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



// valid(q,slot) iff there data available for reading in slot


// The main goal of this queue is to keep evey item in a different cache line
// to minimize the amount of cache invaliations. 
// <buf> is a buffer that stores data. Each data item is of size <size>,
// We add padding 
// to <alg_size> to make it occupy an integer number of cache lines
//
// Here is an example of <size> = 4 (bytes), and ST_CACHE_LINE = 16
// | V1 PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP |        (true if the next item is valid to be read)
// | D1 D1 D1 D1 PP PP PP PP PP PP PP PP PP PP PP PP | 
// | V2 PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP |
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

	MEM_ALIGN(ST_CACHE_LINE) int queue_size;
} ts_context;


// ************
// Queue API:


// Init the queue
ts_context *ts_init(int no, size_t *sizes, int *queue_sizes);

// ** The prefered, 2-step API that avoids extra memcpy
// The following functions are non-blocking and the caller should spin-wait if required
// Producer
char *ts_reserve(ts_context *locCont);
bool ts_push(ts_context *locCont);
// Consumer
char *ts_acquire(ts_context *locCont);
bool ts_release(ts_context *locCont);


// ** The old, 1-step API that introduces extra memcpy
// Producer
void ts_put(ts_context *locCont, char *input);						// Blocking
void ts_putMany(ts_context *locCont, int n, char *input);			// Blocking
void ts_putManyBits(ts_context *locCont, int n, char *input);		// unpack bits into one byte each, and put them into the queue 
// Consumer
bool ts_get(ts_context *locCont, char *output);
int ts_getMany(ts_context *locCont, int n, char *output);
int ts_getManyBlocking(ts_context *locCont, int n, char *output);
int ts_getManyBits(ts_context *locCont, int n, char *output);		// get n bits (stored in 1 byte each), and pack them into (n+7)/8 bytes
int ts_getManyBitsBlocking(ts_context *locCont, int n, char *output);


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









#if !defined(__GNUC__) && !defined(__linux__)
// For barriers
#include <windows.h>
#include<synchapi.h>


extern LONG volatile * barr_hist1;
extern LONG volatile * barr_hist2;

/* Old implementation of barrier()
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
*/

#ifdef _VISUALIZE
#include "cvmarkers.h"
#include "cvmarkersobj.h"
using namespace Concurrency::diagnostic;
marker_series series;

#define NEWSPAN(x,y) \
	new span(series,x,y)
#endif 


#define MAXPOSSIBLETHREADS 16
MEM_ALIGN(ST_CACHE_LINE) static volatile long thr_cnt = 0;

#ifdef _DEBUG 
static volatile SHORT ok[MAXPOSSIBLETHREADS] = { true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true };
#else
MEM_ALIGN(ST_CACHE_LINE) static volatile SHORT ok[MAXPOSSIBLETHREADS] = { 0 };
#endif


inline void barrier(LONG volatile *___state, int no_threads, int thr)
{   // we are not really using the ___state any more

#ifdef _VISUALIZE
	// span *fooSpan = new span(series, 1, "spinwait");
	// series.write_flag("Spin wait time:");
#endif

	if (thr == 0)
	{
		// If we are the master controller then we spin-wait for every thread 
		// to increment their counter

		while (thr_cnt < no_threads - 1);

		// At this point thr_cnt = no_threads-1, so all other threads must be 
		// spinning. Hence, safe to zero-out thr_cnt as no-one is looking at it.
		thr_cnt = 0;
		// NB: Here we are missing a MemoryBarrier() for ARM (but we're OK for x64)
		// One by one, unleash other threads
		for (int i = 1; i < no_threads; i++)
		{
#ifdef _DEBUG
			// Assertion: all threads must be spinning here.
			if (ok[i]) __debugbreak();
#endif
			ok[i] = true;
		}
	}
	else
	{
		// All other threads reach here, set their flag to false and busy-wait
#ifdef _DEBUG
		// Assert that if you ever reach here is because 
		// someone woke you up previously
		if (!ok[thr]) __debugbreak();
#endif
		ok[thr] = false;
		// Atomically increment thr_cnt (NB: generates a barrier)
		InterlockedIncrement(&thr_cnt);

		while (!ok[thr]);
	}
#ifdef _VISUALIZE
	// delete fooSpan;
#endif

}


MEM_ALIGN(ST_CACHE_LINE) static volatile SHORT condok[MAXPOSSIBLETHREADS] = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

inline int barriercond(volatile unsigned char *cond, LONG volatile *___state, int no_threads, int thr)
{   // we are not really using the ___state any more

#ifdef _VISUALIZE
	// span *fooSpan = new span(series, 5, "s");
	// series.write_flag("Spin wait time:");
#endif

	if (thr == 0)
	{
		// If we are the master controller then we spin-wait for every thread 
		// to increment their counter

		while (thr_cnt < no_threads - 1);

		// At this point thr_cnt = no_threads-1, so all other threads must be 
		// spinning. Hence, safe to zero-out thr_cnt as no-one is looking at it.
		thr_cnt = 0;
		SHORT mycond = *cond;
		// NB: Here we are missing a MemoryBarrier() for ARM (but we're OK for x64)
		// One by one, unleash other threads
		for (int i = 1; i < no_threads; i++)
		{
#ifdef _DEBUG
			// Assertion: all threads must be spinning here.
			if (condok[i] >= 0) __debugbreak();
#endif
			condok[i] = *cond;
		}
		return mycond;
	}
	else
	{
		// All other threads reach here, set their flag to false and busy-wait
#ifdef _DEBUG
		// Assert that if you ever reach here is because 
		// someone woke you up previously ... sigh or it's the first time 
//		if (condok[thr] == -1) __debugbreak();
#endif
		condok[thr] = -1;
		// Atomically increment thr_cnt (NB: generates a barrier)
		InterlockedIncrement(&thr_cnt);

		while (condok[thr] < 0);
		return condok[thr];
	}
#ifdef _VISUALIZE
	// delete fooSpan;
#endif
}

#endif
