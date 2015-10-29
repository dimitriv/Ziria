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
// Abstract: Offline demodulation test for 802.11b (brick implementation, input data is from dump file

#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <windows.h>

#include <math.h>
#include <thread_func.h>

#include "bb/bbb.h"
#include "dspcomm.h"
#include "soratime.h"
#include "brickutil.h"
#include "stdbrick.hpp"

#include "cca.hpp"
#include "barkerspread.hpp"
#include "scramble.hpp"

#include "symtiming.hpp"
#include "sfd_sync.hpp"
#include "phy_11b.hpp"

#include "phy_11a.hpp"
#include "samples.hpp"
#include "freqoffset.hpp"
#include "channel_11a.hpp"
#include "demapper11a.hpp"
#include "pilot.hpp"
#include "deinterleaver.hpp"
#include "viterbi.hpp"
#include "fft.hpp"

#include "PerfEvalSource.hpp"
#include "test_config.hpp"

#define IN_BUF_SIZE (16*1024*1024)

static A16 UCHAR InputBuf [IN_BUF_SIZE ];

extern TIMESTAMPINFO tsinfo;


//DEBUG
long dcnt1 = 0;
long dcnt2 = 0;


HANDLE thread1;
HANDLE thread2;

bool fRunning;
//ISource * ssrc;
const uint max_no_sources = 100;
uint no_sources;
ISource * aSsrc[max_no_sources];
bool aIsBit[max_no_sources];
uint nReset[max_no_sources];
unsigned short dataRate[max_no_sources];
bool globalIsBit = false;
uint globalNReset = 0;
char aDesc[max_no_sources*str_len];


ISource* svit;


BOOLEAN ViterbiThread(void * pParam) {
	while (fRunning) {
		svit->Process();

	}

	//	if ( fRunning ) return TRUE;
	//	else return FALSE;
	return false;
}


BOOLEAN RxThread ( void * pParam ) 
{
	bool bRet;
	
	for(uint i=0; i<no_sources; i++)
	{
		printf("Time %-40s: ", aDesc + i*str_len);
		// I can't be bothered to figure out how to configure a brick so I just pass this as a global var.
		globalIsBit = aIsBit[i];
		globalNReset = nReset[i];
		BrickTestCtx.code_rate() = dataRate[i];
		bRet = aSsrc[i]->Process ();
	}
	fRunning = false;
	return false;
}



int BrickTest()
{
	static A16 UCHAR InputBuf [IN_BUF_SIZE ];

		//memset(InputBuf, 0, IN_BUF_SIZE * sizeof(UCHAR));
	for(ulong i=0; i < IN_BUF_SIZE; i++)
	{
		InputBuf[i] = (UCHAR) 0;
	}
	for(ulong i=0; i < max_no_sources; i++)
	{
		aIsBit[i] = false;
		nReset[i] = 0;
	}

	//const uint no_runs = 100000;
	//const uint no_runs = 100000;
	//const uint no_cont_run = 1000;
//	const uint no_runs = 200000000;
	const uint no_runs = 2000000;
	BrickTestCtx.Init((void*)InputBuf, IN_BUF_SIZE, no_runs, &tsinfo);
	no_sources = CreateTestGraph (aSsrc, aDesc, aIsBit, nReset, dataRate);


	// BOZIDAR
	if (svit != NULL)
	{
		thread2 = AllocStartThread(ViterbiThread, NULL);
	}

	fRunning = true;
	thread1 = AllocStartThread ( RxThread, NULL );

	while ( fRunning ) {
		Sleep (1);
	}

	StopFreeThread (thread1 );
	
	for(uint i=0; i<no_sources; i++)
	{
		IReferenceCounting::Release (aSsrc[i]);
	}

	return 0;
}

