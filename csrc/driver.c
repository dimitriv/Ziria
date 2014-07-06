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

	PSORA_UTHREAD_PROC User_Routines[MAX_THREADS];
    // size_t sizes[MAX_THREADS];

    // set_up_threads is defined in the compiler-generated code
    // and returns the number of threads we set up 
	extern int wpl_set_up_threads(PSORA_UTHREAD_PROC *User_Routines);
#endif


// Contex blocks
BufContextBlock buf_ctx;
HeapContextBlock heap_ctx;
BufContextBlock *pbuf_ctx = &buf_ctx;
HeapContextBlock *pheap_ctx = &heap_ctx;


// Blink generated functions 
extern void wpl_input_initialize();
extern void wpl_output_finalize();
extern void wpl_global_init(unsigned int heap_size);
extern int wpl_go();

extern void initBufCtxBlock(BufContextBlock *blk);


// Parameters and parsing
#include "params.h"

/* Global configuration parameters 
***************************************************************************/
BlinkParams Globals;
BlinkParams *params;

// tracks bytes copied 
extern unsigned long bytes_copied; 

int __cdecl main(int argc, char **argv) {

  // Initialize the global parameters
  params = &Globals;
  try_parse_args(params, argc, argv);

#ifdef SORA_PLATFORM
  // Start Sora HW
  if (Globals.inType == TY_SORA || Globals.outType == TY_SORA)
  {
	RadioStart(Globals);
  }
  // Start NDIS
  if (Globals.inType == TY_IP || Globals.outType == TY_IP)
  {
	HRESULT hResult = SoraUEnableGetTxPacket();
	assert(hResult == S_OK);
  }

  // Start measuring time
  initMeasurementInfo(&(Globals.measurementInfo), Globals.latencyCDFSize);
#endif


  // Init
  initBufCtxBlock(&buf_ctx);
  initHeapCtxBlock(&heap_ctx);

  wpl_global_init(Globals.heapSize);
  wpl_input_initialize();

#ifdef SORA_PLATFORM
  /////////////////////////////////////////////////////////////////////////////  
  // DV: Pass the User_Routines here

  int no_threads = wpl_set_up_threads(User_Routines);

  printf("Setting up threads...\n");

  ULONGLONG ttstart, ttend;

  printf("Starting %d threads...\n", no_threads);
  StartThreads(&ttstart, &ttend, &Globals.measurementInfo.tsinfo, no_threads, User_Routines);

  printf("Total input items (including EOF): %d (%d B), output items: %d (%d B)\n",
	  buf_ctx.total_in, buf_ctx.total_in*buf_ctx.size_in,
	  buf_ctx.total_out, buf_ctx.total_out*buf_ctx.size_out);
  printf("Time Elapsed: %ld us \n",
	  SoraTimeElapsed((ttend / 1000 - ttstart / 1000), &Globals.measurementInfo.tsinfo));

  if (Globals.latencySampling > 0)
  {
	  printf("Min write latency: %ld, max write latency: %ld\n", (ulong)Globals.measurementInfo.minDiff, 
																 (ulong) Globals.measurementInfo.maxDiff);
	  printf("CDF: \n   ");
	  unsigned int i = 0;
	  while (i < Globals.measurementInfo.aDiffPtr)
	  {
		  printf("%ld ", Globals.measurementInfo.aDiff[i]);
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
  wpl_go();
#endif

  printf("Bytes copied: %ld\n", bytes_copied);

  wpl_output_finalize();

#ifdef SORA_PLATFORM
  // Stop Sora HW
  if (Globals.inType == TY_SORA || Globals.outType == TY_SORA)
  {
	RadioStop(Globals);
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

BOOLEAN __stdcall go_thread(void * pParam)
{
	thread_info *ti = (thread_info *) pParam;

	wpl_go();

	ti->fRunning = false;

	return false;
}

// Default method. This gets called from test.c when there is only one thread.
// Otherwise, test.c created its own thread functions 
// and store them in User_Routines array
// These get started from the main 
// Returns the numer of threads 
int SetUpThreads(PSORA_UTHREAD_PROC * User_Routines)
{
	User_Routines[0] = (PSORA_UTHREAD_PROC) go_thread;
	return 1;
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

