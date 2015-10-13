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


//#include<sora.h>

// Init the queue
int ts_init(int nc, size_t *sizes);
int ts_init_var(int no, size_t *sizes, int *queue_sizes);

// Called by the uplink thread
// Blocking
void ts_put(int nc, char *input);

// Blocking
void ts_putMany(int nc, int n, char *input);
// unpack bits into one byte each, and put them into the queue 
void ts_putManyBits(int nc, int n, char *input);

// Called by the downlink thread
bool ts_isFinished(int nc);

 // Called by the downlink thread
bool ts_get(int nc, char *output);

int ts_getMany(int nc, int n, char *output);

int ts_getManyBlocking(int nc, int n, char *output);

// get n bits (stored in 1 byte each), and pack them into (n+7)/8 bytes
int ts_getManyBits(int nc, int n, char *output);
int ts_getManyBitsBlocking(int nc, int n, char *output);


// Issued from upstream to downstream
void ts_reset(int nc);

// Issued from upstream to downstream
void ts_flush(int nc);

// Issued from upstream to downstream
void ts_finish(int nc);

bool ts_isEmpty(int nc);

bool ts_isFull(int nc);


// Clear the entire queue
// WARNING: Unlike the rest of the code, this is not thread-safe 
// and might require a lock, depending on the use
void ts_clear(int nc);

// Roll back up to n elements from the queue, as many as available
// WARNING: Unlike the rest of the code, this is not thread-safe 
// and might require a lock, depending on the use
void ts_rollback(int nc, int n);


// Free memory allocated for queues
void ts_free();




// The main goal of this queue is to keep evey item in a different cache line
// to minimize the amount of cache invaliations. 
// <buf> is a buffer that stores data. Each data item is of size <size>
// We store <batch_size> data items contiguously. We then add padding 
// to <alg_size> to make it occupy an integer number of cache lines
//
// Here is an example of <size> = 4 (bytes), <batch_size> = 2 (items) and ST_CACHE_LINE = 16
// | V1 PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP |        (true if the next batch is valid to be read)
// | D1 D1 D1 D1 D2 D2 D2 D2 PP PP PP PP PP PP PP PP | 
// | V3 PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP |
// | D3 D3 D3 D3 D4 D4 D4 D4 PP PP PP PP PP PP PP PP |
//
// <wptr, wind> is a pointer to the last reserved entry, where wptr is a pointer to the batch
// and wind is the index of the element within the batch. For example, wptr = 2, wind = 1 points to (D4 D4 D4 D4)
// <wdptr, wdind> is a pointer to the next pushed entry
// <rptr, rind> is a pointer to the next acquired entry
// <rdptr, rdind> is a pointer to the next released entry
// 
// Useage:
// For producing, first call 
//   buf = s_ts_reserve(queues, nc, num);
// where <nc> is the queue index and <num> is the number of elements (of size <size>) to be written
// <buf> is the buffer allocated for the operation (NULL if no space). Write data to <buf> and then commit by
//   s_ts_push(queues, nc, num);
// For consuming, first call 
//   buf = s_ts_acquire(queues, nc, num);
// to obtain a poitner and then use 
//   s_ts_release(queues, nc);
// to release it. This is dual to the calls above. 
//
// NOTE: There is an implied invariant:
//   wind + num <= batch_size
// or in other word Ziria compiler should make sure that a call to s_ts_reserve or s_ts_acquite 
// will not return a buffer that does not have <num> elements available.
// We DON'T check this invariant in runtime so it may cause seg fault
//
// NOTE: A batch is pushed for reading only once all items in it are released. 
// It cannot be read until it is completely filled. This is to avoid cache misses. 
// 
//
// Legacy interface was does not support batching and sets <batch_size> = 1
// TBD: remove legacy interface (ts_pu, ts_get) and rely only on the new one (ts_acquire, ts_release, ts_reserve, ts_push)

#define ST_CACHE_LINE	64

typedef struct 
{
	// All pointers are char to facilitate pointer arithmetics
	MEM_ALIGN(ST_CACHE_LINE) char *buf;

	// Accessed by producer
	MEM_ALIGN(ST_CACHE_LINE) char *wptr;
	int wind;
	char *wdptr;
	int wdind;
	// Accessed by consumer
	MEM_ALIGN(ST_CACHE_LINE) char *rptr;
	int rind;
	char *rdptr;
	int rdind;

	MEM_ALIGN(ST_CACHE_LINE) int size;
	MEM_ALIGN(ST_CACHE_LINE) int alg_size;
	MEM_ALIGN(ST_CACHE_LINE) int batch_size;

	MEM_ALIGN(ST_CACHE_LINE) volatile bool evReset;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFinish;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFlush;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evProcessDone;

	MEM_ALIGN(ST_CACHE_LINE) int queue_size;
} ts_context;



// Equivallent of the above, but as a utility function, 
// that can be used on an arbitrary set of queues
// TODO: rewrite the compiler to use explicit queues, 
// and get rid of the functions above.

ts_context *s_ts_init_batch(int no, size_t *sizes, int *queue_sizes, int *batch_sizes);
ts_context *s_ts_init_var(int no, size_t *sizes, int *queue_sizes);
ts_context *s_ts_init(int no, size_t *sizes);
void s_ts_put(ts_context *locCont, int nc, char *input);
bool s_ts_get(ts_context *locCont, int nc, char *output);
void s_ts_putMany(ts_context *locCont, int nc, int n, char *input);
int s_ts_getMany(ts_context *locCont, int nc, int n, char *output);
int s_ts_getManyBlocking(ts_context *locCont, int nc, int n, char *output);
bool s_ts_isFinished(ts_context *locCont, int nc);
bool s_ts_isFull(ts_context *locCont, int nc);
bool s_ts_isEmpty(ts_context *locCont, int nc);
void s_ts_reset(ts_context *locCont, int nc);
void ts_reset(int nc);
void s_ts_flush(ts_context *locCont, int nc);
void s_ts_finish(ts_context *locCont, int nc);
void s_ts_free(ts_context *locCont, int no);
void s_ts_clear(ts_context *locCont, int nc);
void s_ts_rollback(ts_context *locCont, int nc, int n);


// The following functions are non-blocking and the caller should spin-wait if required
// Producer
int ts_init_batch(int no, size_t *sizes, int *queue_sizes, int *batch_sizes);
char *s_ts_reserve(ts_context *locCont, int num);
bool s_ts_push(ts_context *locCont, int num);
// Consumer
char *s_ts_acquire(ts_context *locCont, int num);
bool s_ts_release(ts_context *locCont, int num);

char *ts_reserve(int nc, int num);
bool ts_push(int nc, int num);
char *ts_acquire(int nc, int num);
bool ts_release(int nc, int num);



char *_s_ts_reserve(ts_context *locCont, int num);
bool _s_ts_push(ts_context *locCont, int num);
// Consumer
char *_s_ts_acquire(ts_context *locCont, int num);
bool _s_ts_release(ts_context *locCont, int num);


#ifndef __linux__
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
#endif 


#define MAXPOSSIBLETHREADS 16
static volatile long thr_cnt = 0;

#ifdef _DEBUG 
static volatile bool ok[MAXPOSSIBLETHREADS] = { true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true };
#else
static volatile bool ok[MAXPOSSIBLETHREADS] = { 0 };
#endif


inline void barrier(LONG volatile *___state, int no_threads, int thr)
{   // we are not really using the ___state any more

#ifdef _VISUALIZE
	span *fooSpan = new span(series, 1, "spinwait");
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
	delete fooSpan;
#endif

}

#endif


