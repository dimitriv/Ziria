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
#ifdef SORA_RF
#include <soratime.h>
#include <thread_func.h>
#include <stdlib.h>
#include <time.h>
#include <rxstream.h>
#include "params.h"
#include "numerics.h"


void RadioStart(BlinkParams *params) {
	// Initialize Sora user mode extension
    BOOLEAN succ = SoraUInitUserExtension("\\\\.\\HWTest");
    if (!succ) 
    {
        printf("Error: fail to find a Sora UMX capable device!\n");
        exit(1);
    }

	// always start radio first, it will reset radio to the default setting
    SoraURadioStart(params->radioParams.radioId);

    SoraURadioSetRxPA(params->radioParams.radioId, params->radioParams.RXpa);
    SoraURadioSetRxGain(params->radioParams.radioId, params->radioParams.RXgain);
    SoraURadioSetTxGain(params->radioParams.radioId, params->radioParams.TXgain);
    SoraURadioSetCentralFreq(params->radioParams.radioId, params->radioParams.CentralFrequency);
	SoraURadioSetFreqOffset(params->radioParams.radioId, params->radioParams.FreqencyOffset);					
    SoraURadioSetSampleRate(params->radioParams.radioId, params->radioParams.SampleRate);
	params->TXBuffer = NULL;
	params->pRxBuf = NULL;

	// DEBUG
	printf("RadioID: %ld, RX_PA: %ld, RX_Gain: %ld, TX_Gain: %ld, Central Freq: %ld, Freq Offset: %ld, Sample Rate: %ld\n", 
		params->radioParams.radioId, params->radioParams.RXpa, params->radioParams.RXgain,
		params->radioParams.TXgain, params->radioParams.CentralFrequency, params->radioParams.FreqencyOffset,
		params->radioParams.SampleRate);
}



void RadioStop(BlinkParams *params) {
	if (params->TXBuffer != NULL)
	{
		SoraUReleaseBuffer((PVOID)params->TXBuffer);
	}

	if (params->pRxBuf != NULL)
	{
		HRESULT hr;
		SoraURadioReleaseRxStream(&params->radioParams.dev->RxStream, params->radioParams.radioId);
        hr = SoraURadioUnmapRxSampleBuf(params->radioParams.radioId, params->pRxBuf);
	}

	SoraUCleanUserExtension();
}



void InitSoraRx(BlinkParams *params)
{
    HRESULT hr;
    ULONG nRxBufSize = 0;

	// Map Rx Buffer 
    hr = SoraURadioMapRxSampleBuf( params->radioParams.radioId, &params->pRxBuf, & nRxBufSize);
    if ( FAILED (hr) ) {
        fprintf (stderr, "Error: Fail to map Sora Rx buffer!\n" );
        exit(1);
    }
    
    // Generate a sample stream from mapped Rx buffer
	params->radioParams.dev = (SoraRadioParam *)inmem_malloc(sizeof(SoraRadioParam));
	SoraURadioAllocRxStream(&(params->radioParams.dev->RxStream), params->radioParams.radioId, (PUCHAR)params->pRxBuf, nRxBufSize);
}


void InitSoraTx(BlinkParams *params)
{
	params->TXBuffer = SoraUAllocBuffer(params->radioParams.TXBufferSize);					// alloc tx sample buffer
	if (params->TXBuffer == NULL) 
	{
		fprintf (stderr, "Error: Fail to allocate Sora Tx buffer memory!\n" );
		exit(1);
	}
	// Set to 0 as it is not expensive in init and can be useful for gaps between packets
	memset(params->TXBuffer, 0, params->radioParams.TXBufferSize);
}




// readSora reads <size> of __int16 inputs from Sora radio
// It is a blocking function and returns only once everything is read
void readSora(BlinkParams *params, complex16 *ptr, int size)
{
    HRESULT hr;
    FLAG fReachEnd;
	// Signal block contains 7 vcs = 28 int16
    static SignalBlock block;
	complex16* pSamples = (complex16*)(&block[0]);
    static int indexSrc = 28;
	int oldIndexSrc;
	complex16* ptrDst = ptr;
	int remaining = size;

	if (indexSrc < 28) {
		oldIndexSrc = indexSrc;
		// Something has already been read previously, so copy that first
		memcpy((void *)ptrDst, (void *)(pSamples + indexSrc), min(remaining, 28 - oldIndexSrc)*sizeof(complex16));
		indexSrc += min(remaining, 28 - oldIndexSrc);
		ptrDst += min(remaining, 28 - oldIndexSrc);
		remaining -= min(remaining, 28 - oldIndexSrc);
	}

	while (remaining > 0)
	{
		// Read from the radio
		hr = SoraRadioReadRxStream(&(params->radioParams.dev->RxStream), &fReachEnd, block);
		if (FAILED(hr)) {
			fprintf (stderr, "Error: Fail to read Sora Rx buffer!\n" );
			exit(1);
		}
		indexSrc = 0;

		// Copy
		memcpy((void *)ptrDst, (void *)(pSamples + indexSrc), min(remaining, 28)*sizeof(complex16));
		indexSrc += min(remaining, 28);
		ptrDst += min(remaining, 28);
		remaining -= min(remaining, 28);

	}
}




// Transit a buffer of <size> complex16 numbers
// ptr has to be 16 aligned and has to have space for x8 complex16 numbers, due to efficien packing to complex8
void writeSora(BlinkParams *params, complex16 *ptr, ULONG size)
{
    HRESULT hr;
	ULONG TxID;

	ULONG dbg=0;

	if (size*2 > params->radioParams.TXBufferSize)
	{
		fprintf (stderr, "Error: Sora Tx buffer too small (%ld needed)!\n", 2*size );
		exit(1);
	}

	// Saturated packing need 16-bit aligned pointers since it uses SSE
	if ((ULONG)ptr & 0xF > 0)
	{
		fprintf (stderr, "Error: Tx ptr has to be 16 aligned!\n");
		exit(1);
	}

	// Saturated pack from complex16 (default Blink TX type) to complex8
	vcs *vPtr = (vcs*)ptr;
	unsigned int index = 0;
	for(int i=0; i<size/4; i+=2)
	{
        vcs s1, s2;
        s1 = vPtr[i];
        s2 = vPtr[i+1];
        vcb b = (vcb)saturated_pack((vs&)s1, (vs&)s2);

		char *dst = (char *) params->TXBuffer;
		memcpy((void*)(dst+index), (void*) (&b), sizeof(vcb));
		index += sizeof(vcb);

	}


	// DEBUG: 
	//hr = SoraURadioTransferEx(params->radioParams.radioId, params->TXBuffer, 2*size, &TxID);	
	hr = SoraURadioTransferEx(params->radioParams.radioId, params->TXBuffer, 4*size, &TxID);	

    if (!SUCCEEDED(hr))
    {
		fprintf (stderr, "Error: Fail to transfer Sora Tx buffer!\n" );
		exit(1);
	}

	hr = SoraURadioTx(params->radioParams.radioId, TxID);
    if (!SUCCEEDED(hr))
    {
		fprintf (stderr, "Error: Fail to transmit Sora Tx buffer!\n" );
		exit(1);
	}

    hr = SoraURadioTxFree(params->radioParams.radioId, TxID);
}
#endif