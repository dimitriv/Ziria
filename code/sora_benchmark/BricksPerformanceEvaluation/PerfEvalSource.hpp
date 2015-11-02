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
#pragma once

#include <brick.h>
//#include <tpltrick.h>
#include <soratime.h>


class CF_PerfEvalSource {
	FACADE_FIELD (void *, mem_sample_buf);
	FACADE_FIELD (uint,  mem_sample_buf_size);
	FACADE_FIELD (uint,  no_runs);
	FACADE_FIELD (TIMESTAMPINFO*,  tsinfo);
public:
	FINL void Init ( void* sbuf, uint sbuf_size, uint nrun, TIMESTAMPINFO* tsi )
	{
		mem_sample_buf() = sbuf;
		mem_sample_buf_size() = sbuf_size;
		no_runs() = nrun;
		tsinfo() = tsi;
	}
};


// DEBUG
extern long dcnt1, dcnt2;


DEFINE_LOCAL_CONTEXT(PerfEvalSource, CF_PerfEvalSource, CF_Error, CF_11aRxVector);
template<class C_TYPE, ushort N_LEN>
class PerfEvalSource
{
public:
template < TSOURCE_ARGS > 
class source : public TSource<TSOURCE_PARAMS>
{
private: 
// BOZIDAR: Modif of the original function that works well with long time intervals as well
FINL
ULONGLONG Bozidar_SoraTimeElapsed ( ULONGLONG ts, PTIMESTAMPINFO tsinfo )
{
	if ( tsinfo->use_rdtsc ) {
		return ( ((ts * 1000) / tsinfo->TickPerSecond) * 1000000);
	} else {
		return ( ((ts * 1000) / tsinfo->CPUFreq.QuadPart) * 1000000 );
	}
}

protected: 
	CTX_VAR_RO (void*,			sample_buf );
	CTX_VAR_RO (uint,			sample_buf_size );	
	CTX_VAR_RO (uint,			no_runs );	
	CTX_VAR_RO (TIMESTAMPINFO*,	tsinfo );	
	CTX_VAR_RW (ushort,			remain_symbols);
	CTX_VAR_RW (ulong,			error_code);

public:
    DEFINE_OPORT(C_TYPE,   N_LEN); // each bit is one 8-bit soft-value

public:
    REFERENCE_LOCAL_CONTEXT(PerfEvalSource);

    STD_TSOURCE_CONSTRUCTOR(source)
    	BIND_CONTEXT (CF_PerfEvalSource::mem_sample_buf, sample_buf)
    	BIND_CONTEXT (CF_PerfEvalSource::mem_sample_buf_size, sample_buf_size)
    	BIND_CONTEXT (CF_PerfEvalSource::no_runs, no_runs)
    	BIND_CONTEXT (CF_PerfEvalSource::tsinfo, tsinfo)
    	BIND_CONTEXT (CF_Error::error_code, error_code)
		BIND_CONTEXT (CF_11aRxVector::remain_symbols, remain_symbols)
    { 
		error_code = E_ERROR_SUCCESS;
		remain_symbols = 0;
	}

    bool Process ()
    {
		ULONGLONG tts1, tts2; 
		uint no_rep;
		uint no_cont_runs;		// Reset after every no_cont_run (reset every 1000 introduces very small overhead but all blocks works)

		if (globalIsBit) {
			no_rep = no_runs/8;
		} else {
			no_rep = no_runs;
		}
		if (globalNReset == 0)
		{
			no_cont_runs = no_rep/N_LEN;
		}
		else
		{
			no_cont_runs = globalNReset;
		}

		tts1 = SoraGetCPUTimestamp ( tsinfo );
		for (uint ii=0; ii < (no_rep/N_LEN)/no_cont_runs; ii++)
		{
			// Some Bricks can't take too much data (e.g. Viterbi) so 
			// we need to reset occasionally
			Next()->Reset();
			error_code = E_ERROR_SUCCESS;
			remain_symbols = no_cont_runs + 1;
	
			for (uint i=0; i < min(no_rep/N_LEN, no_cont_runs); i++)
			{
				C_TYPE* output = opin().append();
				memcpy((void *) output, (void *) sample_buf, N_LEN * sizeof(C_TYPE));

				Next()->Process(opin());
			}
		}

		tts2 = SoraGetCPUTimestamp ( tsinfo );
		//printf("%.3f us \n", SoraTimeElapsed ((tts2-tts1), tsinfo) * 1.0 / 1000 / no_runs);
		printf("%.3f us (%.3f ns)\n", SoraTimeElapsed ((tts2-tts1), tsinfo) * 1.0 / 1000,
									  SoraTimeElapsed ((tts2-tts1), tsinfo) * 1.0 / no_runs);

		//printf("D1: %ld, D2: %ld\n", dcnt1, dcnt2);
        return true;
    }
}; };

