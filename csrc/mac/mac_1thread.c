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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>

#ifdef SORA_PLATFORM
#include <winsock2.h> // ws2_32.lib required
#include <ws2tcpip.h>


#include <sora.h>
#include <brick.h>
#include <dspcomm.h>
#include <soratime.h>
#include <windows.h>

#include "sora_radio.h"
#include "sora_threads.h"
#include "sora_thread_queues.h"
#include "sora_ip.h"
#endif


#ifdef __GNUC__
#include "threads.h"  // replaces sora_threads.h
#endif

#include "../wpl_alloc.h"
#include "../buf.h"
#include "../utils.h"
#include "../params.h"


#ifdef SORA_PLATFORM
extern PSORA_UTHREAD_PROC User_Routines[MAX_THREADS];
// size_t sizes[MAX_THREADS];
// set_up_threads is defined in the compiler-generated code
// and returns the number of threads we set up 
extern int wpl_set_up_threads_tx(PSORA_UTHREAD_PROC *User_Routines);
extern int wpl_set_up_threads_rx(PSORA_UTHREAD_PROC *User_Routines);
#endif


// tracks bytes copied 
extern unsigned long long bytes_copied;


// Blink generated functions 
extern void wpl_input_initialize_tx();
extern void wpl_input_initialize_rx();
extern void wpl_output_finalize_tx();
extern void wpl_output_finalize_rx();
extern void wpl_global_init_tx(unsigned int heap_size);
extern void wpl_global_init_rx(unsigned int heap_size);
extern int wpl_go_tx();
extern int wpl_go_rx();

// Contex blocks
extern BufContextBlock buf_ctx_tx, buf_ctx_rx;
extern HeapContextBlock heap_ctx_tx, heap_ctx_rx;
extern BufContextBlock *pbuf_ctx_tx;
extern BufContextBlock *pbuf_ctx_rx;
extern HeapContextBlock *pheap_ctx_tx;
extern HeapContextBlock *pheap_ctx_rx;

// Parameters
extern BlinkParams params[2];
extern BlinkParams *params_tx, *params_rx;

#ifdef SORA_PLATFORM
BOOLEAN __stdcall go_thread_main(void * pParam);
#endif


void init_mac_1thread()
{

	// Start Sora HW
	if (params_rx->inType == TY_SDR || params_tx->outType == TY_SDR)
	{
		// Here we assume both TX and RX use the same radio
		// and we use parameters set in TX
#ifdef SORA_RF
		RadioStart(*params_tx);
		if (params_tx->inType == TY_SORA)
		{
			InitSoraRx(*params_tx);
		}
		if (params_tx->outType == TY_SORA)
		{
			InitSoraTx(*params_tx);
		}
#endif
	}



#ifdef SORA_PLATFORM
	// Start NDIS
	if (params_tx->inType == TY_IP)
	{
		HRESULT hResult = SoraUEnableGetTxPacket();
		assert(hResult == S_OK);
		Ndis_init(NULL);
	}

	if (params_rx->outType == TY_IP)
	{
		// To be implemented
		//	  HRESULT hResult = SoraUEnableGetRxPacket();
		//	  assert(hResult == S_OK);
	}

	// Start measuring time
	initMeasurementInfo(&(params_tx->measurementInfo), params_rx->latencyCDFSize);
#endif

	initBufCtxBlock(&buf_ctx_tx);
	initBufCtxBlock(&buf_ctx_rx);
	initHeapCtxBlock(&heap_ctx_tx, params_tx->heapSize);
	initHeapCtxBlock(&heap_ctx_rx, params_rx->heapSize);

	wpl_global_init_tx(params_tx->heapSize);
	wpl_global_init_rx(params_tx->heapSize);

}

#ifdef SORA_PLATFORM
/* Returns the numer of threads */
int SetUpThreads_1t(PSORA_UTHREAD_PROC * User_Routines)
{
	User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_main;
	return 1;
}

// This should run in a thread to make sure that it is running 
// on the appropriate core and that it has real-time priority
BOOLEAN __stdcall go_thread_main(void * pParam)
{

	ULONGLONG ttstart, ttend;
	thread_info *ti = (thread_info *)pParam;

	printf("Starting TX ...\n");
	buf_ctx_tx.mem_output_buf_size = params_tx->outMemorySize;
	buf_ctx_tx.mem_output_buf = malloc(buf_ctx_tx.mem_output_buf_size);

	wpl_input_initialize_tx();

	// Run Ziria TX code
	wpl_go_tx();

	printf("Total input items (including EOF): %d (%d B), output items: %d (%d B)\n",
		buf_ctx_tx.total_in, buf_ctx_tx.total_in*buf_ctx_tx.size_in,
		buf_ctx_tx.total_out, buf_ctx_tx.total_out*buf_ctx_tx.size_out);

	wpl_output_finalize_tx();

	printf("Starting RX ...\n");
	if (params_tx->outType == TY_MEM)
	{
		buf_ctx_rx.mem_input_buf_size = (buf_ctx_tx.total_out * buf_ctx_tx.size_out) / 8;
		buf_ctx_rx.mem_input_buf = buf_ctx_tx.mem_output_buf;
	}
	wpl_input_initialize_rx();

	// Run Ziria RX code
	wpl_go_rx();

	printf("Total input items (including EOF): %d (%d B), output items: %d (%d B)\n",
		buf_ctx_rx.total_in, buf_ctx_rx.total_in*buf_ctx_rx.size_in,
		buf_ctx_rx.total_out, buf_ctx_rx.total_out*buf_ctx_rx.size_out);

	printf("Bytes copied: %llu\n", bytes_copied);

	wpl_output_finalize_rx();

	ti->fRunning = false;

	return false;
}

#endif

#ifdef __GNUC__
void * go_thread_main(void * pParam)
{

	thread_info *ti = (thread_info *)pParam;

	printf("Starting TX on thread %d ...\n", (int)(ti->mThr));
	buf_ctx_tx.mem_output_buf_size = params_tx->outMemorySize;
	buf_ctx_tx.mem_output_buf = malloc(buf_ctx_tx.mem_output_buf_size);

	wpl_input_initialize_tx();

	// Run Ziria TX code
	wpl_go_tx();

	printf("Total input items (including EOF): %d (%d B), output items: %d (%d B)\n",
		buf_ctx_tx.total_in, buf_ctx_tx.total_in*buf_ctx_tx.size_in,
		buf_ctx_tx.total_out, buf_ctx_tx.total_out*buf_ctx_tx.size_out);

	wpl_output_finalize_tx();

	printf("Starting RX ...\n");
	if (params_tx->outType == TY_MEM)
	{
		buf_ctx_rx.mem_input_buf_size = (buf_ctx_tx.total_out * buf_ctx_tx.size_out) / 8;
		buf_ctx_rx.mem_input_buf = buf_ctx_tx.mem_output_buf;
	}
	wpl_input_initialize_rx();

	// Run Ziria RX code
	wpl_go_rx();

	printf("Total input items (including EOF): %d (%d B), output items: %d (%d B)\n",
		buf_ctx_rx.total_in, buf_ctx_rx.total_in*buf_ctx_rx.size_in,
		buf_ctx_rx.total_out, buf_ctx_rx.total_out*buf_ctx_rx.size_out);

	printf("Bytes copied: %llu\n", bytes_copied);

	wpl_output_finalize_rx();

	ti->fRunning = false;
	return (void *)0;
}
#endif

