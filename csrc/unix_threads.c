/*
 * unix_threads.c
 *
 *  Created on: Sep 25, 2015
 *      Author: ecidon
 */

#include "unix_threads.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

pthread_t u_threads[MAX_THREADS];
ThreadInfo t_info[MAX_THREADS];

/*extern*/bool atomix;

int InitBarriers(pthread_barrier_t* bar_array, int no_bar, int no_threads) {
	bool init_error = false;
	for (int i = 0; i < no_bar; ++i) {
		if (pthread_barrier_init(&bar_array[i], NULL, no_threads) != 0)
			init_error = true;
	}
	if (init_error) {
		printf("Initialization error: couldn't initialize a barrier!\n");
		exit(1);
	}
	return no_bar;
}

int BarrierWait(pthread_barrier_t* barrier) {
	if (pthread_barrier_wait(barrier) != 0) {
		printf("Barrier error: barrier not initialized!\n");
		exit(1);
	}
	return 0;
}

int CleanBarriers(pthread_barrier_t* bar_array, int no_bar) {
	bool clean_error = false;
	for (int i = 0; i < no_bar; ++i) {
		if (pthread_barrier_destroy(&bar_array[i]) != 0)
			clean_error = true;
	}
	if (clean_error) {
		printf("Barrier error: Couldn't destroy barrier, check if in use!\n");
		exit(1);
	}
	return no_bar;
}

int StartThreads(int no_threads, ThreadProc* user_routines) {
	bool init_error = false;
	for (int i = 0; i < no_threads; i++) {
		if (user_routines[i] != NULL) {
			t_info[i].thread_id = i;
			t_info[i].running = true;
			if (pthread_create(&u_threads[i], NULL, user_routines[i],
					t_info[i].args) != 0)
				init_error = true;
		} else {
			t_info[i].running = false;
		}
	}
	if (!init_error) {
		if (atomix) {
			// In atomix, all threads are in sync.
			// Only one thread will finish (the one that reads EOF)
			// So if we see on thread finished, we stop
			bool finished = false;
			while (!finished) {
				// Threads will exit when stop_program = true
				// So no need to close them explicitly
				finished = false;
				for (int i = 0; i < no_threads; i++) {
					finished = finished || (!t_info[i].running);
				}
				sleep(1); // NB: not Sleep(100) (!!)
				// Flush stdout to get any printout that could be out there
				fflush(stdout);
			}
		} else {
			// In the conventional execution model, we need to wait for all threads
			bool not_finished = true;
			while (not_finished) {
				// Threads will exit when stop_program = true
				// So no need to close them explicitly

				not_finished = false;
				for (int i = 0; i < no_threads; i++) {
					not_finished = not_finished || t_info[i].running;
				}
				sleep(1); // NB: not Sleep(100) (!!)
				// Flush stdout to get any printout that could be out there
				fflush(stdout);
				/* Removed as it fails to compile with WinDDK
				 if (kbhit())
				 {
				 stop_program = 1;
				 }
				 */
			}
		}
	} else {
		printf("Initialization error: couldn't start a thread!\n");
		exit(1);
	}
	// Stop measuring time
	return no_threads;
}

int main() {
	return 0;
}

