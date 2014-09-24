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

#include "sora_thread_queues.h"



// Init the queue
int cq_init(int nc, size_t *sizes);

// Called by the uplink thread
void cq_put(int nc, char *input);

// Called by the downlink thread
bool cq_isFinished(int nc);

 // Called by the downlink thread
bool cq_get(int nc, char *output);

// Issued from upstream to downstream
void cq_reset(int nc);

// Issued from upstream to downstream
void cq_flush(int nc);

// Issued from upstream to downstream
void cq_finish(int nc);

bool cq_isEmpty(int nc);
bool cq_isFull(int nc);

// Commit n elements of reads from the queue.
// Invariant: nelems must be less than or equal to the number of elements
// optimistically read from the queue since the last commit.
void cq_commit(int nc, int nelems);

// Undo any uncommitted reads.
void cq_rollback(int nc);

// Free memory allocated for queues
void cq_free();

struct cq_context
{
	// All pointers are char to facilitate pointer arithmetics
	MEM_ALIGN(ST_CACHE_LINE) char *buf;
	MEM_ALIGN(ST_CACHE_LINE) char *wptr;
	MEM_ALIGN(ST_CACHE_LINE) char *rptr;
	MEM_ALIGN(ST_CACHE_LINE) char *optimistic_rptr;

	MEM_ALIGN(ST_CACHE_LINE) int size;
	MEM_ALIGN(ST_CACHE_LINE) int alg_size;

	MEM_ALIGN(ST_CACHE_LINE) volatile bool evReset;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFinish;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evFlush;
	MEM_ALIGN(ST_CACHE_LINE) volatile bool evProcessDone;
};
