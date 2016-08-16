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
#include <stdio.h>

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
#include "threads.h"      // replaces sora_threads.h
#include "ip_bridge.h"    // replaces sora_ip.h
#endif

#include "../wpl_alloc.h"
#include "../buf.h"
#include "../utils.h"

#include "mac.h"
#ifdef SORA_PLATFORM
// New Sora specific - DEBUG
#include "sora_RegisterRW.h"



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
BOOLEAN __stdcall go_thread_tx(void * pParam);
BOOLEAN __stdcall go_thread_rx(void * pParam);
#endif



int32 PHYenergy = 0;
int32 PHYnoise = 0;
double PHYEWMAnoise = 0;

int __ext_MAC_cca(int32 energy, int32 noise){
	PHYenergy = energy;
	PHYnoise = noise;
	PHYEWMAnoise = 0.95 * PHYEWMAnoise + 0.05 * (double)noise;
	return 0;
}



void init_mac_2threads()
{
#ifdef ADI_RF
	  int ret = Fmcomms_Init(params_tx);
	  if (ret < 0) exit(1);
#endif

#ifdef LIME_RF
	  int ret = LimeRF_RadioStart(params_tx);
	  params_rx->radioParams.iris = params_tx->radioParams.iris;
	  if (ret < 0) exit(1);
#endif

	// Start Sora HW
	if (params_rx->inType == TY_SDR)
	{
#ifdef ADI_RF
	  ret = Fmcomms_RadioStartRx(params_tx);
	  params_rx->radioParams.Rxbuf = params_tx->radioParams.Rxbuf;
	  params_rx->radioParams.rxch0 = params_tx->radioParams.rxch0;
	  if (ret < 0) exit(1);
#endif

#ifdef LIME_RF
	  ret = LimeRF_ConfigureRX(params_rx);
	  if (ret < 0) exit(1);
#endif

#ifdef SORA_RF
		RadioStart(params_rx);
		InitSoraRx(params_rx);
		// New Sora specific - DEBUG
		SetFirmwareParameters();
#endif
	}
	if (params_tx->outType == TY_SDR)
	{
#ifdef ADI_RF
	  ret = Fmcomms_RadioStartTx(params_tx);
	  if (ret < 0) exit(1);
#endif

#ifdef LIME_RF
	  ret = LimeRF_ConfigureTX(params_tx);
	  if (ret < 0) exit(1);
#endif

#ifdef SORA_RF
		RadioStart(params_tx);
		InitSoraTx(params_tx);
		// New Sora specific - DEBUG
		SetFirmwareParameters();
#endif
	}

#ifdef SORA_PLATFORM
	// Start NDIS
	// This has to be after Sora as Sora initializes UMX
	if (params_tx->inType == TY_IP)
	{

		if (mac_type == MAC_TX_ONLY && txPC != NULL)
		{
			// str != NULL implies RX
			printf("In TX-test mode with NDIS the TX wait for the connection and hence TX-PC should be empty.\n");
			exit(1);
		}

		HRESULT hResult = SoraUEnableGetTxPacket();
		assert(hResult == S_OK);
		Ndis_init(NULL);
	}

	if (params_rx->outType == TY_IP)
	{

		if (mac_type == MAC_RX_ONLY && txPC == NULL)
		{
			// str == NULL implies TX
			printf("In RX-test mode with NDIS the RX should connect to the TX by supplying TX's IP address or host name through TX-PC.\n");
			exit(1);
		}

		HRESULT hResult = SoraUEnableGetTxPacket();
		assert(hResult == S_OK);
		Ndis_init(txPC);

	}
#endif


#ifdef __GNUC__
	if 	(params_rx->outType == TY_IP || params_tx->inType == TY_IP)
	{
		Ndis_init(txPC); // txPC in this case in the name of TUN interface
	}
#endif

#ifdef SORA_PLATFORM
	// Start measuring time
	initMeasurementInfo(&(params_tx->measurementInfo), params_tx->latencyCDFSize);
#endif

	// Init
	initBufCtxBlock(&buf_ctx_tx);
	initBufCtxBlock(&buf_ctx_rx);
	initHeapCtxBlock(&heap_ctx_tx, params_tx->heapSize);
	initHeapCtxBlock(&heap_ctx_rx, params_rx->heapSize);

	wpl_global_init_tx(params_tx->heapSize);
	wpl_global_init_rx(params_rx->heapSize);

}


#ifdef SORA_PLATFORM
/* Returns the numer of threads */
int SetUpThreads_2t(PSORA_UTHREAD_PROC * User_Routines)
{
	int noThr = 0;
	switch (mac_type) {
	case MAC_TX_TEST:
	case MAC_TX_ONLY:
		User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_tx;
		noThr = 1;
		break;
	case MAC_RX_TEST:
	case MAC_RX_ONLY:
		User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_rx;
		noThr = 1;
		break;
	case MAC_TX_RX:
		User_Routines[0] = (PSORA_UTHREAD_PROC)go_thread_tx;
		User_Routines[1] = (PSORA_UTHREAD_PROC)go_thread_rx;
		noThr = 2;
		break;
	}
	return noThr;
}
#endif




void createHeader(unsigned char *header, PHYMod modulation, PHYEnc encoding, int length)
{
	header[0] &= 0xF0;
	switch (modulation) {
	case PHY_MOD_BPSK:
		header[0] |= 3;
		if (encoding == PHY_ENC_CR_12) header[0] |= 8;
		else header[0] |= 12;
		break;
	case PHY_MOD_QPSK:
		header[0] |= 2;
		if (encoding == PHY_ENC_CR_12) header[0] |= 8;
		else header[0] |= 12;
		break;
	case PHY_MOD_16QAM:
		header[0] |= 1;
		if (encoding == PHY_ENC_CR_12) header[0] |= 8;
		else header[0] |= 12;
		break;
	case PHY_MOD_64QAM:
		header[0] |= 0;
		if (encoding == PHY_ENC_CR_23) header[0] |= 8;
		else header[0] |= 12;
		break;
	}

	header[0] &= 0x1F;
	header[1] &= 0xE0;
	header[0] |= (length & 7) << 5;
	header[1] |= (length & 0xFFF8) >> 3;
}




#ifdef SORA_PLATFORM
BOOLEAN __stdcall go_thread_tx(void * pParam)
{
	ULONGLONG ttstart, ttend;
	thread_info *ti = (thread_info *)pParam;
	BlinkFileType inType = params_tx->inType;
	BlinkFileType outType = params_tx->outType;
	const long maxInSize = 2048;
	const long maxOutSize = 200000;
	const int headerSizeInBytes = 3;
	unsigned char * headerBuf;
	unsigned char * payloadBuf;
	uint16 * payloadBuf16;
	memsize_int payloadSize;


	// TX always first prepares the buffers in memory
	params_tx->inType = TY_MEM;
	buf_ctx_tx.mem_input_buf = (void *)malloc(maxInSize);
	headerBuf = (unsigned char*)buf_ctx_tx.mem_input_buf;
	payloadBuf = (unsigned char*)buf_ctx_tx.mem_input_buf + headerSizeInBytes;
	payloadBuf16 = (uint16 *)payloadBuf;

	if (params_tx->outType == TY_SORA)
	{
		params_tx->outType = TY_MEM;
		buf_ctx_tx.mem_output_buf_size = maxOutSize;
		buf_ctx_tx.mem_output_buf = params_tx->TXBuffer;
	}


	if (inType != TY_FILE && inType != TY_DUMMY && inType != TY_IP)
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

	if (inType == TY_DUMMY)
	{
		buf_ctx_tx.mem_input_buf_size = headerSizeInBytes + params_tx->dummySamples / 8;
		if (buf_ctx_tx.mem_input_buf_size > maxInSize)
		{
			printf("Error: TX buffer too small!\n");
			exit(1);
		}
		memset(buf_ctx_tx.mem_input_buf, 0, buf_ctx_tx.mem_input_buf_size);
	}

	printf("Starting TX ...\n");

	// This is slow as it includes LUT generation
	wpl_global_init_tx(params_tx->heapSize);

	if (outType == TY_FILE)
	{
		memset(headerBuf, 0, 3);
		createHeader(headerBuf, PHY_MOD_BPSK, PHY_ENC_CR_12, buf_ctx_tx.mem_input_buf_size - headerSizeInBytes);

		// Run Ziria TX code to preapre the buffer
		resetBufCtxBlock(&buf_ctx_tx);						// reset context block (counters)
		wpl_init_heap(pheap_ctx_tx, params_tx->heapSize);	// reset memory management

		// Run Ziria TX code
		wpl_input_initialize_tx();
		wpl_go_tx();
		wpl_output_finalize_tx();
	}
	else
	{
		// SORA output
		uint16 pktCnt = 0;

		while (1) 
		{
			unsigned long payloadSizeInBytes;

			if (inType == TY_IP)
			{
				// NDIS read
				payloadSizeInBytes = 0;
				while (payloadSizeInBytes == 0)
				{
					payloadSizeInBytes = ReadFragment(payloadBuf, RADIO_MTU);
				}

				buf_ctx_tx.mem_input_buf_size = payloadSizeInBytes + headerSizeInBytes;
			}
			else
			{
				// Simple payload to check correctness
				payloadSizeInBytes = buf_ctx_tx.mem_input_buf_size - headerSizeInBytes;
				memset(payloadBuf, 0, payloadSizeInBytes);
				for (int i = 0; i<payloadSizeInBytes/2; i++)
					payloadBuf16[i] = pktCnt;
				pktCnt ++;
			}

			memset(headerBuf, 0, 3);
			createHeader(headerBuf, phy_rate.mod, phy_rate.enc, payloadSizeInBytes);

			if (params_tx->debug > 0)
			{
				printf("Sending packet of size %ld\n", payloadSizeInBytes);
				fflush(stdout);
			}

			// Run Ziria TX code to preapre the buffer
			resetBufCtxBlock(&buf_ctx_tx);						// reset context block (counters)
			wpl_init_heap(pheap_ctx_tx, params_tx->heapSize);	// reset memory management

			wpl_input_initialize_tx();
			wpl_go_tx();
			wpl_output_finalize_tx();

			// Sora TX
			HRESULT hr;
			ULONG TxID;

			unsigned long interpacketGap = 5000;
			hr = SoraURadioTransferEx(params_tx->radioParams.radioId, params_tx->TXBuffer, 
				4 * buf_ctx_tx.total_out + 4 * interpacketGap, &TxID);
			if (!SUCCEEDED(hr))
			{
				fprintf(stderr, "Error: Fail to transfer Sora Tx buffer: %lx!\n", hr);
				exit(1);
			}

			hr = SoraURadioTx(params_tx->radioParams.radioId, TxID);

			if (!SUCCEEDED(hr))
			{
				HRESULT hr1 = SoraURadioTxFree(params_tx->radioParams.radioId, TxID);
				fprintf(stderr, "Error: Fail to transmit Sora Tx buffer: %lx!\n", hr);
				exit(1);
			}

			hr = SoraURadioTxFree(params_tx->radioParams.radioId, TxID);

			// DEBUG
			volatile int tt = 0;
			// This delay is fine for correct reception!
			for (int i = 0; i < 1000000; i++) tt++;
			// This delay is fine for correct reception for 32B packets but not for the large ones!
			//for (int i = 0; i < 100000; i++) tt++;
			// This delay is too short and causes weird bugs!
			//for (int i = 0; i < 10000; i++) tt++;
		}
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
	const long maxOutSize = 4096;
	memsize_int sampleSize;


	if (inType != TY_FILE && inType != TY_SORA)
	{
		printf("Only TY_FILE or TY_SORA supported for input!\n");
		exit(1);
	}

	if (outType != TY_FILE && outType != TY_IP && outType != TY_DUMMY)
	{
		printf("Only TY_FILE or TY_IP or TY_DUMMY supported for output!\n");
		exit(1);
	}


	if (inType == TY_FILE)
	{
		// RX always first prepares the buffers in memory
		params_rx->inType = TY_MEM;

		char *filebuffer;
		try_read_filebuffer(pheap_ctx_rx, params_rx->inFileName, &filebuffer, &sampleSize);
		buf_ctx_rx.mem_input_buf = (void *)malloc(sampleSize * sizeof(complex16));

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


	if (outType == TY_IP || outType == TY_DUMMY)
	{
		params_rx->outType = TY_MEM;
		buf_ctx_rx.mem_output_buf_size = maxOutSize;
		buf_ctx_rx.mem_output_buf = (void *)malloc(maxOutSize);
	}


	printf("Starting RX ...\n");


	if (inType == TY_FILE)
	{
		// Run Ziria TX code
		resetBufCtxBlock(&buf_ctx_rx);				// reset context block (counters)
		wpl_global_init_rx(params_rx->heapSize);	// reset memory management
		wpl_input_initialize_rx();
		wpl_go_rx();
		wpl_output_finalize_rx();
	}
	else
	{
		unsigned char lastCRC;
		unsigned char lastMod;
		unsigned char lastEnc;
		unsigned int lastLen;
		uint16 pktCnt = 0;
		unsigned long cntOk = 0;
		unsigned long cntError = 0;
		unsigned long cntMiss = 0;
		unsigned long lastGap = 0;
		const unsigned long printDelay = 1000;

		unsigned char * payload = (unsigned char *)buf_ctx_rx.mem_output_buf;
		uint16 * payload16 = (uint16 *)buf_ctx_rx.mem_output_buf;


		// This is slow as it includes LUT generation
		wpl_global_init_rx(params_rx->heapSize);


		// DEBUG
		int lastError = 4;
		uint16 oldPkt[100];
		int lastError2 = 4;
		uint16 oldPkt2[100];
		memset(oldPkt, 0, 100 * sizeof(uint16));

		while (1)
		{
			// Avoid stale data, for debugging
			memset((void*)payload16, 0, 16 * sizeof(uint16));

			// Run Ziria TX code
			resetBufCtxBlock(&buf_ctx_rx);						// reset context block (counters)
			wpl_init_heap(pheap_ctx_rx, params_rx->heapSize);	// reset memory management
			wpl_input_initialize_rx();
			wpl_go_rx();
			wpl_output_finalize_rx();

			unsigned int lengthInBytes = buf_ctx_rx.total_out / 8 - 5;
			lastCRC = payload[lengthInBytes];
			lastMod = payload[lengthInBytes + 1];
			lastEnc = payload[lengthInBytes + 2];
			lastLen = payload[lengthInBytes + 3] + 256 * payload[lengthInBytes + 4];

			if (params_tx->debug > 0)
			{
				printf("Received packet: crc=%d, mod=%d, enc=%d, len=%d, buf_len=%d\n",
					lastCRC, lastMod, lastEnc, lastLen, lengthInBytes);
				fflush(stdout);
			}

			if (outType == TY_IP)
			{
				// IP
				if (lastCRC == 1)
				{
					int n = WriteFragment(payload);

					if (params_tx->debug > 0)
					{
						printf("Delivering pkt of size %ld to IP (ret=%d)\n", lengthInBytes, n);
						fflush(stdout);
					}
				}
			}
			else
			{
				uint16 pc = payload16[0];
				bool pktOK;

				pktOK = (lastCRC == 1);

				for (int i = 1; i < lengthInBytes / 2 && pktOK; i++)
					pktOK = pktOK && (payload16[i] == pc);

				if (pktOK)
				{
					cntOk++;

					int d = pc - pktCnt - 1;
					/*
					if (d < 0 || d > 100)
					{
					printf("pc=%d, pktCnt=%d, lastError=%d, lastError2=%d, d=%d\n",
					pc, pktCnt, lastError, lastError2, d);
					for (int i = 0; i < 16; i++)
					printf("%d ", payload16[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt2[i]);
					printf("\n");
					}
					*/

					lastError2 = lastError;
					if (d > 0) lastError = 2;
					else lastError = 0;

					cntMiss += (unsigned long)(d < 0) ? 0 : d;
					lastGap = d;

					pktCnt = pc;
				}
				else
				{
					cntError++;
					pktCnt++;

					/*
					{
					printf("Last packet: cnt=%d, crc=%d, mod=%d, enc=%d, len=%d, buf_len=%d, lastGap=%ld\n",
					pc, lastCRC, lastMod, lastEnc, lastLen, lengthInBytes, lastGap);
					printf("crc=%d, pc=%d, pktCnt=%d, lastError=%d, lastError=%d\n",
					(lastCRC == 1), pc, pktCnt, lastError, lastError2);
					for (int i = 0; i < 16; i++)
					printf("%d ", payload16[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt2[i]);
					printf("\n");
					}
					*/

					lastError2 = lastError;
					lastError = 1;
				}

				memcpy((void*)oldPkt2, (void*)oldPkt, 16 * sizeof(uint16));
				memcpy((void*)oldPkt, (void*)payload16, 16 * sizeof(uint16));

				if (printDelay < cntOk + cntError + cntMiss)
				{
					printf("%ld Last packet: SNR=%.2f(%d/%.0f), cnt=%d, crc=%d, mod=%d, enc=%d, len=%d, buf_len=%d, lastGap=%ld\n",
						SoraGetCPUTimestamp(&(params_tx->measurementInfo.tsinfo)),
						10 * log10((double)PHYenergy / PHYEWMAnoise), PHYenergy, PHYEWMAnoise,
						pc, lastCRC, lastMod, lastEnc, lastLen, lengthInBytes, lastGap);
					printf("OK: %ld, Error: %ld, Miss: %ld\n", cntOk, cntError, cntMiss);
					fflush(stdout);
					cntOk = 0;
					cntError = 0;
					cntMiss = 0;
				}
			}
		}
	}




	ti->fRunning = false;

	return false;
}
#endif

#ifdef __GNUC__
void * go_thread_tx(void * pParam)
{
	thread_info *ti = (thread_info *)pParam;
	//while(!ti->fRunning);
	pthread_mutex_unlock(&ti->lock);

	BlinkFileType inType = params_tx->inType;
	BlinkFileType outType = params_tx->outType;
	const long maxInSize = 2048;
	const long maxOutSize = 200000;
	const int headerSizeInBytes = 3;
	unsigned char * headerBuf;
	unsigned char * payloadBuf;
	uint16 * payloadBuf16;
	memsize_int payloadSize;


	// TX always first prepares the buffers in memory
	params_tx->inType = TY_MEM;
	buf_ctx_tx.mem_input_buf = (void *)malloc(maxInSize);
	headerBuf = (unsigned char*)buf_ctx_tx.mem_input_buf;
	payloadBuf = (unsigned char*)buf_ctx_tx.mem_input_buf + headerSizeInBytes;
	payloadBuf16 = (uint16 *)payloadBuf;

	if (params_tx->outType == TY_SDR)
	{
		//params_tx->outType = TY_MEM;
		//buf_ctx_tx.mem_output_buf_size = maxOutSize;
		//buf_ctx_tx.mem_output_buf = params_tx->TXBuffer;
	}


	if (inType != TY_FILE && inType != TY_DUMMY && inType != TY_IP)
	{
		printf("Only TY_FILE or TY_IP supported for input!\n");
		exit(1);
	}

	if (outType != TY_FILE && outType != TY_SDR)
	{
		printf("Only TY_FILE or TY_SDR supported for output!\n");
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

	if (inType == TY_DUMMY)
	{
		buf_ctx_tx.mem_input_buf_size = headerSizeInBytes + params_tx->dummySamples / 8;
		if (buf_ctx_tx.mem_input_buf_size > maxInSize)
		{
			printf("Error: TX buffer too small!\n");
			exit(1);
		}
		memset(buf_ctx_tx.mem_input_buf, 0, buf_ctx_tx.mem_input_buf_size);
	}

	printf("Starting TX ...\n");

	// This is slow as it includes LUT generation
	wpl_global_init_tx(params_tx->heapSize);

	if (outType == TY_FILE)
	{
		memset(headerBuf, 0, 3);
		createHeader(headerBuf, PHY_MOD_BPSK, PHY_ENC_CR_12, buf_ctx_tx.mem_input_buf_size - headerSizeInBytes);

		// Run Ziria TX code to preapre the buffer
		resetBufCtxBlock(&buf_ctx_tx);						// reset context block (counters)
		wpl_init_heap(pheap_ctx_tx, params_tx->heapSize);	// reset memory management

		// Run Ziria TX code
		wpl_input_initialize_tx();
		wpl_go_tx();
		wpl_output_finalize_tx();
	}
	else
	{
		// SDR output
		uint16 pktCnt = 0;

		while (1)
		{
			unsigned long payloadSizeInBytes;
			if (inType == TY_IP)
			{
				// NDIS read
				payloadSizeInBytes = 0;
				while (payloadSizeInBytes == 0)
				{
					payloadSizeInBytes = ReadFragment(payloadBuf, RADIO_MTU);
					printf("read %d bytes from tun/tap interface \n", payloadSizeInBytes);
				}

				buf_ctx_tx.mem_input_buf_size = payloadSizeInBytes + headerSizeInBytes;
			}
			else
			{
				// Simple payload to check correctness
				payloadSizeInBytes = buf_ctx_tx.mem_input_buf_size - headerSizeInBytes;
				memset(payloadBuf, 0, payloadSizeInBytes);
				for (int i = 0; i<payloadSizeInBytes/2; i++)
					payloadBuf16[i] = pktCnt;
				pktCnt ++;
			}

			memset(headerBuf, 0, 3);
			createHeader(headerBuf, phy_rate.mod, phy_rate.enc, payloadSizeInBytes);

			if (params_tx->debug > 0)
			{
				printf("Sending packet of size %ld\n", payloadSizeInBytes);
				fflush(stdout);
			}

			// Run Ziria TX code to preapre the buffer
			resetBufCtxBlock(&buf_ctx_tx);						// reset context block (counters)
			wpl_init_heap(pheap_ctx_tx, params_tx->heapSize);	// reset memory management
			if (buf_ctx_tx.mem_input_buf == NULL)
				printf("buf mem null\n");
			if (buf_ctx_tx.mem_input_buf_size == 0)
				printf("buf mem size 0\n");
			wpl_input_initialize_tx();
			wpl_go_tx();
			wpl_output_finalize_tx();

			/*
			unsigned long interpacketGap = 5000;
			hr = SoraURadioTransferEx(params_tx->radioParams.radioId, params_tx->TXBuffer,
				4 * buf_ctx_tx.total_out + 4 * interpacketGap, &TxID);
			if (!SUCCEEDED(hr))
			{
				fprintf(stderr, "Error: Fail to transfer Sora Tx buffer: %lx!\n", hr);
				exit(1);
			}

			hr = SoraURadioTx(params_tx->radioParams.radioId, TxID);

			if (!SUCCEEDED(hr))
			{
				HRESULT hr1 = SoraURadioTxFree(params_tx->radioParams.radioId, TxID);
				fprintf(stderr, "Error: Fail to transmit Sora Tx buffer: %lx!\n", hr);
				exit(1);
			}

			hr = SoraURadioTxFree(params_tx->radioParams.radioId, TxID);
			*/
		}
	}


	ti->fRunning = false;

	return (void *)0;
}




void * go_thread_rx(void * pParam)
{
	thread_info *ti = (thread_info *)pParam;
	//while(!ti->fRunning);
	pthread_mutex_unlock(&ti->lock);

	BlinkFileType inType = params_rx->inType;
	BlinkFileType outType = params_rx->outType;
	const long maxOutSize = 4096;
	memsize_int sampleSize;


	if (inType != TY_FILE && inType != TY_SDR)
	{
		printf("Only TY_FILE or TY_SDR supported for input!\n");
		exit(1);
	}

	if (outType != TY_FILE && outType != TY_IP && outType != TY_DUMMY)
	{
		printf("Only TY_FILE or TY_IP or TY_DUMMY supported for output!\n");
		exit(1);
	}


	if (inType == TY_FILE)
	{
		// RX always first prepares the buffers in memory
		params_rx->inType = TY_MEM;

		char *filebuffer;
		try_read_filebuffer(pheap_ctx_rx, params_rx->inFileName, &filebuffer, &sampleSize);
		buf_ctx_rx.mem_input_buf = (void *)malloc(sampleSize * sizeof(complex16));

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


	if (outType == TY_IP || outType == TY_DUMMY)
	{
		params_rx->outType = TY_MEM;
		buf_ctx_rx.mem_output_buf_size = maxOutSize;
		buf_ctx_rx.mem_output_buf = (void *)malloc(maxOutSize);
	}


	printf("Starting RX ...\n");


	if (inType == TY_FILE)
	{
		// Run Ziria TX code
		resetBufCtxBlock(&buf_ctx_rx);				// reset context block (counters)
		wpl_global_init_rx(params_rx->heapSize);	// reset memory management
		wpl_input_initialize_rx();
		wpl_go_rx();
		wpl_output_finalize_rx();
	}
	else
	{
		unsigned char lastCRC;
		unsigned char lastMod;
		unsigned char lastEnc;
		unsigned int lastLen;
		uint16 pktCnt = 0;
		unsigned long cntOk = 0;
		unsigned long cntError = 0;
		unsigned long cntMiss = 0;
		unsigned long lastGap = 0;
		const unsigned long printDelay = 1000;

		unsigned char * payload = (unsigned char *)buf_ctx_rx.mem_output_buf;
		uint16 * payload16 = (uint16 *)buf_ctx_rx.mem_output_buf;


		// This is slow as it includes LUT generation
		wpl_global_init_rx(params_rx->heapSize);


		// DEBUG
		int lastError = 4;
		uint16 oldPkt[100];
		int lastError2 = 4;
		uint16 oldPkt2[100];
		memset(oldPkt, 0, 100 * sizeof(uint16));

		while (1)
		{
			// Avoid stale data, for debugging
			memset((void*)payload16, 0, 16 * sizeof(uint16));

			// Run Ziria TX code
			resetBufCtxBlock(&buf_ctx_rx);						// reset context block (counters)
			wpl_init_heap(pheap_ctx_rx, params_rx->heapSize);	// reset memory management
			wpl_input_initialize_rx();
			wpl_go_rx();
			wpl_output_finalize_rx();

			unsigned int lengthInBytes = buf_ctx_rx.total_out / 8 - 5;
			lastCRC = payload[lengthInBytes];
			lastMod = payload[lengthInBytes + 1];
			lastEnc = payload[lengthInBytes + 2];
			lastLen = payload[lengthInBytes + 3] + 256 * payload[lengthInBytes + 4];

			if (params_tx->debug > 0)
			{
				printf("Received packet: crc=%d, mod=%d, enc=%d, len=%d, buf_len=%d\n",
					lastCRC, lastMod, lastEnc, lastLen, lengthInBytes);
				fflush(stdout);
			}

			if (outType == TY_IP)
			{
				// IP
				if (lastCRC == 1)
				{
					int n = WriteFragment(payload, lengthInBytes + 5);

					if (params_tx->debug > 0)
					{
						printf("Delivering pkt of size %ld to IP (ret=%d)\n", lengthInBytes, n);
						fflush(stdout);
					}
				}
			}
			else
			{
				uint16 pc = payload16[0];
				bool pktOK;

				pktOK = (lastCRC == 1);

				for (int i = 1; i < lengthInBytes / 2 && pktOK; i++)
					pktOK = pktOK && (payload16[i] == pc);

				if (pktOK)
				{
					cntOk++;

					int d = pc - pktCnt - 1;
					/*
					if (d < 0 || d > 100)
					{
					printf("pc=%d, pktCnt=%d, lastError=%d, lastError2=%d, d=%d\n",
					pc, pktCnt, lastError, lastError2, d);
					for (int i = 0; i < 16; i++)
					printf("%d ", payload16[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt2[i]);
					printf("\n");
					}
					*/

					lastError2 = lastError;
					if (d > 0) lastError = 2;
					else lastError = 0;

					cntMiss += (unsigned long)(d < 0) ? 0 : d;
					lastGap = d;

					pktCnt = pc;
				}
				else
				{
					cntError++;
					pktCnt++;

					/*
					{
					printf("Last packet: cnt=%d, crc=%d, mod=%d, enc=%d, len=%d, buf_len=%d, lastGap=%ld\n",
					pc, lastCRC, lastMod, lastEnc, lastLen, lengthInBytes, lastGap);
					printf("crc=%d, pc=%d, pktCnt=%d, lastError=%d, lastError=%d\n",
					(lastCRC == 1), pc, pktCnt, lastError, lastError2);
					for (int i = 0; i < 16; i++)
					printf("%d ", payload16[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt[i]);
					printf("\n");
					for (int i = 0; i < 16; i++)
					printf("%d ", oldPkt2[i]);
					printf("\n");
					}
					*/

					lastError2 = lastError;
					lastError = 1;
				}

				memcpy((void*)oldPkt2, (void*)oldPkt, 16 * sizeof(uint16));
				memcpy((void*)oldPkt, (void*)payload16, 16 * sizeof(uint16));
				/*
				if (printDelay < cntOk + cntError + cntMiss)
				{
					printf("%ld Last packet: SNR=%.2f(%d/%.0f), cnt=%d, crc=%d, mod=%d, enc=%d, len=%d, buf_len=%d, lastGap=%ld\n",
						SoraGetCPUTimestamp(&(params_tx->measurementInfo.tsinfo)),
						10 * log10((double)PHYenergy / PHYEWMAnoise), PHYenergy, PHYEWMAnoise,
						pc, lastCRC, lastMod, lastEnc, lastLen, lengthInBytes, lastGap);
					printf("OK: %ld, Error: %ld, Miss: %ld\n", cntOk, cntError, cntMiss);
					fflush(stdout);
					cntOk = 0;
					cntError = 0;
					cntMiss = 0;
				}
				*/
			}
		}
	}

	ti->fRunning = false;

	return (void *)0;
}
#endif
