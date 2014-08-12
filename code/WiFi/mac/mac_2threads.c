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


// TX or RX MAC type
extern int mac_type;


extern PSORA_UTHREAD_PROC User_Routines[MAX_THREADS];
// size_t sizes[MAX_THREADS];

// set_up_threads is defined in the compiler-generated code
// and returns the number of threads we set up 
extern int wpl_set_up_threads_tx(PSORA_UTHREAD_PROC *User_Routines);
extern int wpl_set_up_threads_rx(PSORA_UTHREAD_PROC *User_Routines);


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


BOOLEAN __stdcall go_thread_tx(void * pParam);
BOOLEAN __stdcall go_thread_rx(void * pParam);



void init_mac_2threads()
{
	// Start Sora HW
	if (params_rx->inType == TY_SORA)
	{
		RadioStart(*params_rx);
		InitSoraRx(*params_rx);
	}
	if (params_tx->outType == TY_SORA)
	{
		RadioStart(*params_tx);
		InitSoraTx(*params_tx);
	}

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
	initMeasurementInfo(&(params_tx->measurementInfo), params_tx->latencyCDFSize);

	// Init
	initBufCtxBlock(&buf_ctx_tx);
	initBufCtxBlock(&buf_ctx_rx);
	initHeapCtxBlock(&heap_ctx_tx);
	initHeapCtxBlock(&heap_ctx_rx);

	wpl_global_init_tx(params_tx->heapSize);
	wpl_global_init_rx(params_rx->heapSize);

}



/* Returns the numer of threads */
int SetUpThreads_2t(PSORA_UTHREAD_PROC * User_Routines)
{
	int noThr = 0;
	switch (mac_type) {
	case 0: 
		User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_tx;
		noThr = 1;
		break;
	case 1:
		User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_rx;
		noThr = 1;
		break;
	case 2:
		User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_tx;
		User_Routines[1] = (PSORA_UTHREAD_PROC)go_thread_rx;
		noThr = 2;
		break;
	}
	return noThr;
}



#define CR_12	0
#define CR_23	1
#define CR_34	2

#define M_BPSK	0
#define M_QPSK	1
#define M_16QAM	2
#define M_64QAM	3


void createHeader(char *header, int modulation, int encoding, int length)
{
	header[0] &= 0xF0;
	switch (modulation) {
	case M_BPSK:
		header[0] |= 3;
		if (encoding == CR_12) header[0] |= 8;
		else header[0] |= 12;
		break;
	case M_QPSK:
		header[0] |= 2;
		if (encoding == CR_12) header[0] |= 8;
		else header[0] |= 12;
		break;
	case M_16QAM:
		header[0] |= 1;
		if (encoding == CR_12) header[0] |= 8;
		else header[0] |= 12;
		break;
	case M_64QAM:
		header[0] |= 0;
		if (encoding == CR_23) header[0] |= 8;
		else header[0] |= 12;
		break;
	}

	header[0] &= 0x1F;
	header[1] &= 0xE0;
	header[0] |= (length & 7) << 5;
	header[1] |= (length & 0xF8) >> 3;
}





BOOLEAN __stdcall go_thread_tx(void * pParam)
{
	ULONGLONG ttstart, ttend;
	thread_info *ti = (thread_info *)pParam;
	BlinkFileType inType = params_tx->inType;
	BlinkFileType outType = params_tx->outType;
	const long maxInSize = 2048;
	const long maxOutSize = 200000;
	const int headerSizeInBytes = 3;
	char * headerBuf;
	char * payloadBuf;
	unsigned int payloadSize;


	// TX always first prepares the buffers in memory
	params_tx->inType = TY_MEM;
	buf_ctx_tx.mem_input_buf = (void *)try_alloc_bytes(pheap_ctx_tx, maxInSize);
	headerBuf = (char*)buf_ctx_tx.mem_input_buf;
	payloadBuf = (char*)buf_ctx_tx.mem_input_buf + headerSizeInBytes;

	if (params_tx->outType == TY_MEM)
	{
		buf_ctx_tx.mem_output_buf_size = maxOutSize;
		buf_ctx_tx.mem_output_buf = (void *)try_alloc_bytes(pheap_ctx_tx, maxOutSize * sizeof(complex16));
	}


	if (inType != TY_FILE && inType != TY_IP)
	{
		printf("Only TY_FILE or TY_IP supported for input!\n");
		exit(1);
	}

	if (outType != TY_FILE && outType != TY_SORA)
	{
		printf("Only TY_FILE or TY_SORA supported for output!\n");
		exit(1);
	}


	if (inType == TY_FILE)
	{
		char *filebuffer;
		try_read_filebuffer(pheap_ctx_tx, params_tx->inFileName, &filebuffer, &payloadSize);

		if (params_tx->inFileMode == MODE_BIN)
		{
			buf_ctx_tx.mem_input_buf_size = payloadSize + headerSizeInBytes;
			memcpy(payloadBuf, (void *)filebuffer, payloadSize);
		}
		else
		{
			buf_ctx_tx.mem_input_buf_size = headerSizeInBytes + parse_dbg_bit(filebuffer, (BitArrPtr)payloadBuf) / 8;
		}
	}	


	printf("Starting TX ...\n");

	memset(headerBuf, 0, 3);
	createHeader(headerBuf, M_BPSK, CR_12, buf_ctx_tx.mem_input_buf_size - headerSizeInBytes);

	wpl_input_initialize_tx();

	// Run Ziria TX code
	wpl_go_tx();


	wpl_output_finalize_tx();

	if (outType == TY_SORA)
	{
		// SORA
	}


	ti->fRunning = false;

	return false;
}




BOOLEAN __stdcall go_thread_rx(void * pParam)
{
	ULONGLONG ttstart, ttend;
	thread_info *ti = (thread_info *)pParam;

	BlinkFileType inType = params_rx->inType;
	BlinkFileType outType = params_rx->outType;
	const long maxInSize = 200000;
	const long maxOutSize = 2048;
	unsigned int sampleSize;

	// TX always first prepares the buffers in memory
	params_rx->inType = TY_MEM;
	buf_ctx_rx.mem_input_buf = (void *)try_alloc_bytes(pheap_ctx_rx, maxInSize * sizeof(complex16));

	if (inType != TY_FILE && inType != TY_SORA)
	{
		printf("Only TY_FILE or TY_SORA supported for input!\n");
		exit(1);
	}

	if (outType != TY_FILE && outType != TY_IP)
	{
		printf("Only TY_FILE or TY_IP supported for output!\n");
		exit(1);
	}


	if (inType == TY_FILE)
	{
		char *filebuffer;
		try_read_filebuffer(pheap_ctx_rx, params_rx->inFileName, &filebuffer, &sampleSize);

		if (params_rx->inFileMode == MODE_BIN)
		{
			buf_ctx_rx.mem_input_buf_size = sampleSize;
			memcpy(buf_ctx_rx.mem_input_buf, (void *)filebuffer, sampleSize);
		}
		else
		{
			buf_ctx_rx.mem_input_buf_size = parse_dbg_int16(filebuffer, (int16 *)buf_ctx_rx.mem_input_buf) * sizeof(complex16);
		}
	}


	if (outType == TY_IP)
	{
		params_rx->outType = TY_MEM;
		buf_ctx_rx.mem_output_buf_size = maxOutSize;
		buf_ctx_rx.mem_output_buf = (void *)try_alloc_bytes(pheap_ctx_rx, maxOutSize);
	}


	printf("Starting RX ...\n");

	wpl_input_initialize_rx();

	// Run Ziria TX code
	wpl_go_rx();

	wpl_output_finalize_rx();

	if (outType == TY_IP)
	{
		// IP
	}


	ti->fRunning = false;

	return false;
}


