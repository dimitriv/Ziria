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

#ifndef UNIX_THREADS_H_
#define UNIX_THREADS_H_

#include <pthread.h>
#include <stdbool.h>

#define MAX_THREADS 8

typedef void* Arguments;

typedef struct {
	int thread_id;
	bool running;
	Arguments args;
} ThreadInfo;

typedef void*(*ThreadProc)(void*);

int InitBarriers(pthread_barrier_t* bar_array, int no_bar, int no_threads);

int BarrierWait(pthread_barrier_t* barrier);

int CleanBarriers(pthread_barrier_t* bar_array, int no_bar);

int StartThreads(int no_threads, ThreadProc* user_routines);

#endif /* UNIX_THREADS_H_ */
