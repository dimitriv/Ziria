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
#include <const.h>
#include <sora.h>
#include "sora_threads.h"
#include "sora_thread_queues.h"

#define BUF_SIZE	64
#define CACHE_LINE	64


// Assuming char is 1B
//#define __VALID(buf, size, i) *((bool*) ((buf) + (CACHE_LINE+(size))*(i)))
//#define __DATA(buf, size, i) ((buf) + (CACHE_LINE+(size))*(i) + CACHE_LINE)

volatile bool* valid(char *buf, int size, int i) {
	return ((bool*) ((buf) + (CACHE_LINE+(size))*(i)));
}
char *data(char *buf, int size, int i) {
	return ((buf) + (CACHE_LINE+(size))*(i) + CACHE_LINE);
}



struct ts_context
{
	// All pointers are char to facilitate pointer arithmetics
	MEM_ALIGN(CACHE_LINE) char *buf;
	MEM_ALIGN(CACHE_LINE) char *wptr;
	MEM_ALIGN(CACHE_LINE) char *rptr;

	MEM_ALIGN(CACHE_LINE) int size;
	MEM_ALIGN(CACHE_LINE) int alg_size;
		
	MEM_ALIGN(CACHE_LINE) volatile bool evReset;
	MEM_ALIGN(CACHE_LINE) volatile bool evFinish;
	MEM_ALIGN(CACHE_LINE) volatile bool evFlush;
	MEM_ALIGN(CACHE_LINE) volatile bool evProcessDone;
};


ts_context *contexts;
int no_contexts = 0;


// Sora needed to yield in WinXP because of the scheduler issues
// In Win7 this does not seem necessary. 
// However, after thorough measurements we concluded that 
// SoraThreadYield doesn't affect the performance on many cores
// and help performance on a few cores, so we decided to keep it
//#define USE_SORA_YIELD


//Uncomment to display statistics about ts_queues
//#define TS_DEBUG

#ifdef TS_DEBUG
#define MAX_TS	10
__declspec(align(16)) LONG queueSize[MAX_TS*16];
LONG queueCum[MAX_TS];
LONG queueSam[MAX_TS];
LONG almostFull[MAX_TS];
LONG full[MAX_TS];
LONG fstalled[MAX_TS];
LONG esamples[MAX_TS];
LONG empty[MAX_TS];
#endif



// Init <no> queues
int ts_init(int no, size_t *sizes)
{
	contexts = (ts_context *) (malloc(no * sizeof(ts_context)));
	if (contexts == NULL)
	{
		return 0;
	}

#ifdef TS_DEBUG
		memset(queueSize, 0, MAX_TS*16 * sizeof(LONG));
		memset(queueCum, 0, MAX_TS * sizeof(LONG));
		memset(queueSam, 0, MAX_TS * sizeof(LONG));
		memset(almostFull, 0, MAX_TS * sizeof(LONG));
		memset(full, 0, MAX_TS * sizeof(LONG));
		memset(fstalled, 0, MAX_TS * sizeof(LONG));
		memset(esamples, 0, MAX_TS * sizeof(LONG));
		memset(empty, 0, MAX_TS * sizeof(LONG));
#endif

	for (int j=0; j<no; j++)
	{
		// Buffer size should be a multiple of CACHE_LINE
		contexts[j].size = sizes[j];
		contexts[j].alg_size = CACHE_LINE * (sizes[j] / CACHE_LINE);
		if (sizes[j] % CACHE_LINE > 0) contexts[j].alg_size += CACHE_LINE;

		// Allocate one cache line for valid field and the rest for the data
		contexts[j].buf = (char *) _aligned_malloc((CACHE_LINE+contexts[j].alg_size)*BUF_SIZE, CACHE_LINE);
		if (contexts[j].buf == NULL)
		{
			printf("Cannot allocate thread separator buffer! Exiting... \n");
			exit (-1);
		}

		size_t i;
		for (i = 0; i < BUF_SIZE; i++)
		{
			* valid(contexts[j].buf, contexts[j].alg_size, i) = false;
		}
		contexts[j].wptr = contexts[j].rptr = contexts[j].buf;
		contexts[j].evReset = contexts[j].evFlush = contexts[j].evFinish = false;
		contexts[j].evProcessDone = true;
	}

	no_contexts = no;
	return no;
}







// Called by the uplink thread
//        BOOL_FUNC_PROCESS(ipin)
void ts_put(int nc, char *input)
{
	if (nc >= no_contexts) 
	{
        printf("There are only %d queues!, %d\n", no_contexts, nc);
		return;
	}

	/*
	// DEBUG
	printf("%u, nc: %d, wptr: %x, rptr: %x, buf: %x, wptr valid=%d\n", GetCurrentThreadId(), 
		nc, contexts[nc].wptr, contexts[nc].rptr, contexts[nc].buf,
		*valid(contexts[nc].wptr, contexts[nc].alg_size, 0));
	*/

#ifdef TS_DEBUG
	if (*valid(contexts[nc].wptr, contexts[nc].alg_size, 0)) {
		full[nc]++;
	}
#endif

    // spin wait if the synchronized buffer is full
    //while ((contexts[nc].wptr)->valid) { SoraThreadYield(TRUE); }
    while (*valid(contexts[nc].wptr, contexts[nc].alg_size, 0)) { 
#ifdef TS_DEBUG
		fstalled[nc]++;
#endif
#ifdef USE_SORA_YIELD
		SoraThreadYield(TRUE); 
#endif
	}


	// copy a burst of input data into the synchronized buffer
	//memcpy ((contexts[nc].wptr)->data, input, sizeof(char)*BURST);
	// Copy only the actual amount of data (size) and not the entire buffer (alg_size)
	memcpy (data(contexts[nc].wptr, contexts[nc].alg_size, 0), input, sizeof(char)*contexts[nc].size);

    * valid(contexts[nc].wptr, contexts[nc].alg_size, 0) = true;
    contexts[nc].evProcessDone = false;


#ifdef TS_DEBUG
	InterlockedIncrement(queueSize + (nc*16));
	queueCum[nc] += queueSize[nc*16];
	queueSam[nc]++;
	if (queueSize[nc*16] > (BUF_SIZE * 0.9)) almostFull[nc]++;
#endif


	// advance the write pointer
    //(contexts[nc].wptr)++;
    //if ((contexts[nc].wptr) == (contexts[nc].buf) + BUF_SIZE)
    //    (contexts[nc].wptr) = (contexts[nc].buf);
    contexts[nc].wptr += (CACHE_LINE+contexts[nc].alg_size);
    if ((contexts[nc].wptr) == (contexts[nc].buf) + BUF_SIZE*(CACHE_LINE+contexts[nc].alg_size))
        (contexts[nc].wptr) = (contexts[nc].buf);
}



// Called by the downlink thread
bool ts_get(int nc, char *output)
//FINL bool Process() // ISource::Process
{
	if (nc >= no_contexts) 
	{
	  printf("There are only %d queues! %d\n", no_contexts, nc);
		return false;
	}


#ifdef TS_DEBUG
	esamples[nc]++;
#endif

	// if the synchronized buffer has no data, 
    // check whether there is reset/flush request
    //if (!(contexts[nc].rptr)->valid)
    if (!(*valid(contexts[nc].rptr, contexts[nc].alg_size, 0)))
    {		
#ifdef TS_DEBUG
		empty[nc]++;
#endif
		if (contexts[nc].evReset)
        {
            //Next()->Reset();
            (contexts[nc].evReset) = false;
        }
        if (contexts[nc].evFlush)
        {
            //Next()->Flush();
            contexts[nc].evFlush = false;
        }
        contexts[nc].evProcessDone = true;
		// no data to process  
        return false;
    }
	else
	{
		// Otherwise, there are data. Pump the data to the output pin
		//memcpy ( output, (contexts[nc].rptr)->data, sizeof(char)*BURST);
		// Copy only the actual amount of data (size) and not the entire buffer (alg_size)
		memcpy ( output, data(contexts[nc].rptr, contexts[nc].alg_size, 0), sizeof(char)*contexts[nc].size);

        //(contexts[nc].rptr)->valid = false;
	    //(contexts[nc].rptr)++;
        //if ((contexts[nc].rptr) == (contexts[nc].buf) + BUF_SIZE)


#ifdef TS_DEBUG
		InterlockedDecrement(queueSize + (nc*16));
#endif

        * valid(contexts[nc].rptr, contexts[nc].alg_size, 0) = false;
		contexts[nc].rptr += (CACHE_LINE+contexts[nc].alg_size);
		if ((contexts[nc].rptr) == (contexts[nc].buf) + BUF_SIZE*(CACHE_LINE+contexts[nc].alg_size))
        {
            (contexts[nc].rptr) = (contexts[nc].buf);
#ifdef USE_SORA_YIELD
            // Periodically yielding in busy processing to prevent OS hanging
            SoraThreadYield(TRUE);
#endif
		}

        //bool rc = Next()->Process(opin0());
        //if (!rc) return rc;
		return true;
	}
    return false;
}




// Called by the downlink thread
bool ts_isFinished(int nc)
{
	if (nc >= no_contexts) 
	{
		printf("There are only %d queues!\n", no_contexts);
		return false;
	}

	// Return true to signal the end
	if (contexts[nc].evFinish)
	{
	  //previously: contexts[nc].evFinish = false;
	  contexts[nc].evProcessDone = true;
	  return true;
	}
	return false;
}

bool ts_isFull(int nc)
{
  return (*valid(contexts[nc].wptr, contexts[nc].alg_size, 0));
}


bool ts_isEmpty(int nc)
{
  return (!(*valid(contexts[nc].rptr, contexts[nc].alg_size, 0)));
}


// Issued from upstream to downstream
void ts_reset(int nc)
{
	if (nc >= no_contexts) 
	{
		printf("There are only %d queues!\n", no_contexts);
		return;
	}
    // Set the reset event, spin-waiting for 
    // the downstream to process the event
    contexts[nc].evReset = true;
    while (contexts[nc].evReset) { 
#ifdef USE_SORA_YIELD
		SoraThreadYield(TRUE); 
#endif
	}
}


// Issued from upstream to downstream
void ts_flush(int nc)
{
	if (nc >= no_contexts) 
	{
		printf("There are only %d queues!\n", no_contexts);
		return;
	}

	// Wait for all data in buf processed by downstreaming bricks
    while (!contexts[nc].evProcessDone) { 
#ifdef USE_SORA_YIELD
		SoraThreadYield(TRUE); 
#endif
	}

    // Set the flush event, spin-waiting for
    // the downstream to process the event
    contexts[nc].evFlush = true;
	while (contexts[nc].evFlush) { 
#ifdef USE_SORA_YIELD
		SoraThreadYield(TRUE); 
#endif
	}
}


// Issued from upstream to downstream
void ts_finish(int nc)
{
	if (nc >= no_contexts) 
	{
		printf("There are only %d queues!\n", no_contexts);
		return;
	}


	// Set the reset event, spin-waiting for 
    // the downstream to process the event
    contexts[nc].evFinish = true;
    //previously: while (contexts[nc].evFinish) { SoraThreadYield(TRUE); }
}

void ts_free()
{
	for (int nc=0; nc < no_contexts; nc++)
	{
#ifdef TS_DEBUG
	printf("queue=%u, avg queue=%.3f, avg alm.full=%.3f, full=%.3f (%u), fstalled=%u, avg_stall=%.3f, empty=%.3f(%u), total samples=%u\n", 
		(long) queueSize[nc*16], (double) queueCum[nc] / (double) queueSam[nc], 
		(double) almostFull[nc] / (double) queueSam[nc], 
		(double) full[nc] / (double) queueSam[nc], full[nc], fstalled[nc], 
		(double) fstalled[nc] / (double) full[nc], 
		(double) empty[nc] / (double) esamples[nc], empty[nc], 
		queueSam[nc]);
#endif
		_aligned_free(contexts[nc].buf);
	}
}




