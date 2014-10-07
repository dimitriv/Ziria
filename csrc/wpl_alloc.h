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

#include "types.h"

typedef struct _HeapContextBlock {
	void * wpl_heap;
	memsize_int wpl_free_idx;
	memsize_int wpl_heap_siz;
} HeapContextBlock;


void initHeapCtxBlock(HeapContextBlock *hblk, memsize_int max_heap_size);


/* The bump allocator */
void wpl_init_heap(HeapContextBlock *blk, memsize_int max_heap_size);

void * wpl_alloca(HeapContextBlock *blk, memsize_int bytes);

/* Just a generic allocation routine */
char * try_alloc_bytes(HeapContextBlock *hblk, memsize_int siz);

unsigned int wpl_get_free_idx(HeapContextBlock *blk);
// precondition: 16-aligned
void wpl_restore_free_idx(HeapContextBlock *blk, memsize_int idx);
