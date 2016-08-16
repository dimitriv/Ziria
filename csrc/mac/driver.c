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
#include "threads.h"
#endif

#include "../wpl_alloc.h"
#include "../buf.h"
#include "../utils.h"
#include "mac.h"

#ifdef ADI_RF
#include "../fmcomms_radio.h"
#endif

#ifdef LIME_RF
#include "../lime_radio.h"
#endif



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

#ifdef SORA_PLATFORM
TimeMeasurements measurementInfo;

PSORA_UTHREAD_PROC User_Routines[MAX_THREADS];


// 2-threads MAC
extern void init_mac_2threads();
extern int SetUpThreads_2t(PSORA_UTHREAD_PROC * User_Routines);
#endif

#ifdef __GNUC__
#define MAX_THREADS 2

extern void init_mac_2threads();
extern void * go_thread_rx();
extern void * go_thread_tx();
#endif

MACType mac_type;
PHYRate phy_rate;

// PC name for wired uplink
char *txPC = NULL;
char txPCBuf[255];


// tracks bytes copied 
extern unsigned long long bytes_copied;


// Only needed to support the call from Ziria code
// We currently don't support multiple threads in MAC
int SetUpThreads(PSORA_UTHREAD_PROC* User_Routines)
{
	return 0;
}


#ifdef __GNUC__
#ifndef __cdecl
	#define __cdecl
#endif
#endif

int __cdecl main(int argc, char **argv) 
{
#ifdef SORA_PLATFORM
	ULONGLONG ttstart, ttend;
#endif

#ifdef __GNUC__
	thread_info t_info[MAX_THREADS];
#endif

	params_tx = &(params[0]);
	params_rx = &(params[1]);

	// Initialize the global parameters
	try_parse_args(params, argc, argv);


	printf("Setting up threads...\n");

	// **** TX/RX(2)-threaded MAC

	// Initialize various parameters
	init_mac_2threads();
#ifdef SORA_PLATFORM
	int no_threads = SetUpThreads_2t(User_Routines);
	StartThreads(&ttstart, &ttend, &(params_tx->measurementInfo.tsinfo), no_threads, User_Routines);

	printf("Time Elapsed: %ld us \n",
		SoraTimeElapsed((ttend / 1000 - ttstart / 1000), &(params_tx->measurementInfo.tsinfo)));


	// Free thread separators
	// NB: these are typically allocated in blink_set_up_threads
	ts_free();
#endif

#ifdef __GNUC__

	int numThr;

	switch (mac_type)
	{
	case MAC_TX_TEST:
	case MAC_TX_ONLY:
		numThr = 1;
		t_info[0].fRunning = true;
		t_info[0].mThr = StartPosixThread(go_thread_tx, (void *)t_info, 0, 0);
		break;
	case MAC_RX_TEST:
	case MAC_RX_ONLY:
		numThr = 1;
		t_info[0].fRunning = true;
		t_info[0].mThr = StartPosixThread(go_thread_rx, (void *)t_info, 0, 0);
		break;
	case MAC_TX_RX:
		numThr = 2;
		pthread_mutex_t lock;
		//t_info[0].fRunning = true;
		//t_info[1].fRunning = true;
	    if (pthread_mutex_init(&t_info[0].lock, NULL) != 0)
	    {
	        printf("\n mutex init failed\n");
	        return 1;
	    }

	    pthread_mutex_lock(&t_info[0].lock);
		t_info[0].mThr = StartPosixThread(go_thread_rx, (void *)&t_info[0], 1, 0); // core 0

		if (pthread_mutex_init(&t_info[1].lock, NULL) != 0)
	    {
	        printf("\n mutex init failed\n");
	        return 1;
	    }

		pthread_mutex_lock(&t_info[1].lock);
		t_info[1].mThr = StartPosixThread(go_thread_tx, (void *)&t_info[1], 1, 0); // core 1
		break;
	}

	int i;
	bool isRunning = true;
	while (isRunning) {
		for (i = 0; i < numThr; i++)
			isRunning = isRunning || t_info[i].fRunning;
		//Sleep (1);
	}
	pthread_mutex_destroy(&t_info[0].lock);
	pthread_mutex_destroy(&t_info[1].lock);

	for (i = 0; i < numThr; i++)
		pthread_join(t_info[i].mThr, NULL);
#endif

	if (params_rx->inType == TY_SDR || params_tx->outType == TY_SDR)
	{
#ifdef ADI_RF
		Fmcomms_RadioStop(params_tx);
#else
#ifdef LIME_RF
		LimeRF_RadioStop(params_tx);
#endif
#endif
	}

	// Start Sora HW
	if (params_rx->inType == TY_SDR)
	{
#ifdef SORA_RF
		RadioStop(params_rx);
#endif
	}
	if (params_tx->outType == TY_SDR)
	{
#ifdef SORA_RF
		RadioStop(params_tx);
#endif
	}

#ifdef SORA_PLATFORM
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
#endif

	return 0;
}




