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



// Contex blocks
BufContextBlock buf_ctx_tx, buf_ctx_rx;
HeapContextBlock heap_ctx_tx, heap_ctx_rx;
BufContextBlock *pbuf_ctx_tx = &buf_ctx_tx;
BufContextBlock *pbuf_ctx_rx = &buf_ctx_rx;
HeapContextBlock *pheap_ctx_tx = &heap_ctx_tx;
HeapContextBlock *pheap_ctx_rx = &heap_ctx_rx;

// Parameters
BlinkParams params[2];
BlinkParams *params_tx, *params_rx;


TimeMeasurements measurementInfo;

PSORA_UTHREAD_PROC User_Routines[MAX_THREADS];


// 2-threads MAC
extern void init_mac_2threads();
extern int SetUpThreads_2t(PSORA_UTHREAD_PROC * User_Routines);


int mac_type;
char txPC[255];

// tracks bytes copied 
extern unsigned long long bytes_copied;


// Only needed to support the call from Ziria code
// We currently don't support multiple threads in MAC
int SetUpThreads(PSORA_UTHREAD_PROC* User_Routines)
{
	return 0;
}


int __cdecl main(int argc, char **argv) 
{
	ULONGLONG ttstart, ttend;

	params_tx = &(params[0]);
	params_rx = &(params[1]);

	// Initialize the global parameters
	try_parse_args(params, argc, argv);


	printf("Setting up threads...\n");

	// **** TX/RX(2)-threaded MAC

	// Initialize various parameters
	init_mac_2threads();

	int no_threads = SetUpThreads_2t(User_Routines);
	StartThreads(&ttstart, &ttend, &(params_tx->measurementInfo.tsinfo), no_threads, User_Routines);

	printf("Time Elapsed: %ld us \n",
		SoraTimeElapsed((ttend / 1000 - ttstart / 1000), &(params_tx->measurementInfo.tsinfo)));


	// Free thread separators
	// NB: these are typically allocated in blink_set_up_threads
	ts_free();


	// Start Sora HW
	if (params_rx->inType == TY_SORA)
	{
		RadioStop(params_rx);
	}
	if (params_tx->outType == TY_SORA)
	{
		RadioStop(params_tx);
	}
	// Start NDIS
	if (params_tx->inType == TY_IP)
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

	if (params_rx->outType == TY_IP)
	{
		// To be implemented
		/*
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
		*/
	}


	return 0;
}




