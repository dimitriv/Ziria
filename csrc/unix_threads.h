/*
 * unix_threads.h
 *
 *  Created on: Sep 25, 2015
 *      Author: ecidon
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
