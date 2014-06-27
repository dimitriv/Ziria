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

#include "wpl_alloc.h"
#include "buf.h"
#include "utils.h"

#ifdef SORA_PLATFORM
	#define DEBUG_PERF

	TimeMeasurements measurementInfo;

	PSORA_UTHREAD_PROC User_Routines[MAX_THREADS];
    // size_t sizes[MAX_THREADS];

    // set_up_threads is defined in the compiler-generated code
    // and returns the number of threads we set up 
	extern int wpl_set_up_threads_tx(PSORA_UTHREAD_PROC *User_Routines);
	extern int wpl_set_up_threads_rx(PSORA_UTHREAD_PROC *User_Routines);
#endif


// Contex blocks
BufContextBlock buf_ctx_tx, buf_ctx_rx;
HeapContextBlock heap_ctx_tx, heap_ctx_rx;
BufContextBlock *pbuf_ctx_tx = &buf_ctx_tx;
BufContextBlock *pbuf_ctx_rx = &buf_ctx_rx;
HeapContextBlock *pheap_ctx_tx = &heap_ctx_tx;
HeapContextBlock *pheap_ctx_rx = &heap_ctx_rx;


// Blink generated functions 
extern void wpl_input_initialize_tx();
extern void wpl_input_initialize_rx();
extern void wpl_output_finalize_tx();
extern void wpl_output_finalize_rx();
extern void wpl_global_init_tx(unsigned int heap_size);
extern void wpl_global_init_rx(unsigned int heap_size);
extern int wpl_go_tx();
extern int wpl_go_rx();


int SetUpThreads(PSORA_UTHREAD_PROC * User_Routines);


// Parameters and parsing
#include "params.h"

/* Global configuration parameters 
***************************************************************************/
struct BlinkParams Globals;

// tracks bytes copied 
extern unsigned long bytes_copied; 

int __cdecl main(int argc, char **argv) {
  
  // Initialize the global parameters
  try_parse_args(argc,argv);

#ifdef SORA_PLATFORM
  // Start Sora HW
  if (Globals.inType == TY_SORA || Globals.outType == TY_SORA)
  {
	RadioStart(Globals.radioParams);
  }
  // Start NDIS
  if (Globals.inType == TY_IP || Globals.outType == TY_IP)
  {
	HRESULT hResult = SoraUEnableGetTxPacket();
	assert(hResult == S_OK);
  }

  // Start measuring time
  initMeasurementInfo(Globals.latencyCDFSize);
#endif


  // Init
  initBufCtxBlock(&buf_ctx_tx);
  initBufCtxBlock(&buf_ctx_rx);
  initHeapCtxBlock(&heap_ctx_tx);
  initHeapCtxBlock(&heap_ctx_rx);

  wpl_global_init_tx(Globals.heapSize);
  wpl_global_init_rx(Globals.heapSize);
  wpl_input_initialize_tx();
  wpl_input_initialize_rx();

#ifdef SORA_PLATFORM
  /////////////////////////////////////////////////////////////////////////////  
  // DV: Pass the User_Routines here

  // Currently we only support one thread per module
  // If there are multiple threads, the code will not
  // be initialized correctly!
  // (see /csrc/driver.c for explanation)

  //SINGLE MODULE CODE: int no_threads = wpl_set_up_threads_tx(User_Routines);
  int no_threads = SetUpThreads(User_Routines);

  printf("Setting up threads...\n");

  ULONGLONG ttstart, ttend;

  printf("Starting %d threads...\n", no_threads);
  StartThreads(&ttstart, &ttend, &measurementInfo.tsinfo, no_threads, User_Routines);

  printf("Time Elapsed: %ld us \n", 
	  SoraTimeElapsed((ttend / 1000 - ttstart / 1000), &measurementInfo.tsinfo));

  if (Globals.latencySampling > 0)
  {
	  printf("Min write latency: %ld, max write latency: %ld\n", (ulong) measurementInfo.minDiff, (ulong) measurementInfo.maxDiff);
	  printf("CDF: \n   ");
	  unsigned int i = 0;
	  while (i < measurementInfo.aDiffPtr)
	  {
		  printf("%ld ", measurementInfo.aDiff[i]);
		  if (i % 10 == 9)
		  {
			  printf("\n   ");
		  }
		  i++;
	  }
	  printf("\n");
  }


  // Free thread separators
  // NB: these are typically allocated in blink_set_up_threads
  ts_free();
#else
  printf("Only Sora supported at the moment!\n");
  exit(1);
  //wpl_go();
#endif

  printf("Bytes copied: %ld\n", bytes_copied);

  wpl_output_finalize_tx();
  wpl_output_finalize_rx();

#ifdef SORA_PLATFORM
  // Stop Sora HW
  if (Globals.inType == TY_SORA || Globals.outType == TY_SORA)
  {
	RadioStop(Globals.radioParams);
  }

  // Stop NDIS
  if (Globals.inType == TY_IP || Globals.outType == TY_IP)
    {
      if (hUplinkThread != NULL)
      {
	// Sora cleanup.
	SoraUThreadStop(hUplinkThread);
	SoraUThreadFree(hUplinkThread);
      }
      SoraUDisableGetTxPacket();
      // Winsock cleanup.
      closesocket(ConnectSocket);
      WSACleanup();
    }

#endif

  return 0;
}



#ifdef SORA_PLATFORM

BOOLEAN __stdcall go_thread_tx(void * pParam)
{
	thread_info *ti = (thread_info *) pParam;

	wpl_go_tx();

	ti->fRunning = false;

	return false;
}

BOOLEAN __stdcall go_thread_rx(void * pParam)
{
	thread_info *ti = (thread_info *)pParam;

	wpl_go_rx();

	ti->fRunning = false;

	return false;
}

/* Returns the numer of threads */
int SetUpThreads(PSORA_UTHREAD_PROC * User_Routines)
{
	User_Routines[0] = (PSORA_UTHREAD_PROC) go_thread_tx;
	User_Routines[1] = (PSORA_UTHREAD_PROC) go_thread_rx;
	return 2;
}

#else

// Define an empty SetUpThreads() function.
// This is here as a shim for compiling single threaded code with GCC.
// See note in Codegen/CgSetupThreads.hs for more information.
int SetUpThreads()
{
  return 1;
}

#endif

