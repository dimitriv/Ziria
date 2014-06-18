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
#include <soratime.h>
#include <thread_func.h>
#include <stdlib.h>
#include <time.h>
#include <rxstream.h>
#include "params.h"
#include "numerics.h"


void RadioStart(SoraParameters params) {	
	// Initialize Sora user mode extension
    BOOLEAN succ = SoraUInitUserExtension("\\\\.\\HWTest");
    if (!succ) 
    {
        printf("Error: fail to find a Sora UMX capable device!\n");
        exit(1);
    }

	// always start radio first, it will reset radio to the default setting
    SoraURadioStart(params.radioId);

    SoraURadioSetRxPA(params.radioId, params.RXpa);
    SoraURadioSetRxGain(params.radioId, params.RXgain);
    SoraURadioSetTxGain(params.radioId, params.TXgain);
    SoraURadioSetCentralFreq(params.radioId, params.CentralFrequency);
	SoraURadioSetFreqOffset(params.radioId, params.FreqencyOffset);					
    SoraURadioSetSampleRate(params.radioId, params.SampleRate);
	Globals.TXBuffer = NULL;
	Globals.pRxBuf = NULL;
}



void RadioStop(SoraParameters params) {	
	if (Globals.TXBuffer != NULL)
	{
		SoraUReleaseBuffer((PVOID)Globals.TXBuffer);
	}

	if (Globals.pRxBuf != NULL)
	{
		HRESULT hr;
		SoraURadioReleaseRxStream(&Globals.RxStream, params.radioId);
        hr = SoraURadioUnmapRxSampleBuf(params.radioId, Globals.pRxBuf);
	}

	SoraUCleanUserExtension();
}



void InitSoraRx(SoraParameters params)
{
    HRESULT hr;
    ULONG nRxBufSize = 0;

	// Map Rx Buffer 
    hr = SoraURadioMapRxSampleBuf( params.radioId, &Globals.pRxBuf, & nRxBufSize);
    if ( FAILED (hr) ) {
        fprintf (stderr, "Error: Fail to map Sora Rx buffer!\n" );
        exit(1);
    }
    
    // Generate a sample stream from mapped Rx buffer
	SoraURadioAllocRxStream( &(Globals.RxStream), params.radioId, (PUCHAR)Globals.pRxBuf, nRxBufSize);
}


void InitSoraTx(SoraParameters params)
{
	Globals.TXBuffer = SoraUAllocBuffer(Globals.radioParams.TXBufferSize);					// alloc tx sample buffer
	if (Globals.TXBuffer == NULL) 
	{
		fprintf (stderr, "Error: Fail to allocate Sora Tx buffer memory!\n" );
		exit(1);
	}
}




// readSora reads <size> of __int16 inputs from Sora radio
// It is a blocking function and returns only once everything is read
void readSora(complex16 *ptr, int size)
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
		hr = SoraRadioReadRxStream( &(Globals.RxStream), &fReachEnd, block);
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
void writeSora(complex16 *ptr, ULONG size)
{
    HRESULT hr;
	ULONG TxID;

	ULONG dbg=0;

	if (size*2 > Globals.radioParams.TXBufferSize)
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

		char *dst = (char *) Globals.TXBuffer;
		memcpy((void*)(dst+index), (void*) (&b), sizeof(vcb));
		index += sizeof(vcb);

	}


	// DEBUG: 
	//hr = SoraURadioTransferEx(Globals.radioParams.radioId, Globals.TXBuffer, 2*size, &TxID);	
	hr = SoraURadioTransferEx(Globals.radioParams.radioId, Globals.TXBuffer, 4*size, &TxID);	

    if (!SUCCEEDED(hr))
    {
		fprintf (stderr, "Error: Fail to transfer Sora Tx buffer!\n" );
		exit(1);
	}

	hr = SoraURadioTx(Globals.radioParams.radioId, TxID);
    if (!SUCCEEDED(hr))
    {
		fprintf (stderr, "Error: Fail to transmit Sora Tx buffer!\n" );
		exit(1);
	}

    hr = SoraURadioTxFree(Globals.radioParams.radioId, TxID);
}
