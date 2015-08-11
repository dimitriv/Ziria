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

#include<sora.h>



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



#define ST_CACHE_LINE	64

struct ts_context
{
	// All pointers are char to facilitate pointer arithmetics
	MEM_ALIGN(ST_CACHE_LINE) char *buf;
	MEM_ALIGN(ST_CACHE_LINE) char *wptr;
	MEM_ALIGN(ST_CACHE_LINE) char *rptr;

	MEM_ALIGN(ST_CACHE_LINE) int size;
	MEM_ALIGN(ST_CACHE_LINE) int alg_size;

	MEM_ALIGN(ST_CACHE_LINE) volatile bool evReset;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFinish;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFlush;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evProcessDone;

	MEM_ALIGN(ST_CACHE_LINE) int queue_size;
};



// Equivallent of the above, but as a utility function, 
// that can be used on an arbitrary set of queues
// TODO: rewrite the compiler to use explicit queues, 
// and get rid of the functions above.

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



