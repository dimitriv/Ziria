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

#include "params.h"
#include "utils.h"

typedef enum __GetStatus {
	GS_SUCCESS,
	GS_EOF
} GetStatus;


void init_getbit();
GetStatus buf_getbit(Bit *x);
GetStatus buf_getarrbit(BitArrPtr x, unsigned int vlen);
void init_putbit();
void buf_putbit(Bit x);
void buf_putarrbit(BitArrPtr x, unsigned int vlen);
void flush_putbit();


void init_getint32();
GetStatus buf_getint32(int32 *x);
GetStatus buf_getarrint32(int32 *x, unsigned int vlen);
void init_putint32();
void buf_putint32(int32 x);
void buf_putarrint32(int32 * x, unsigned int vlen);
void flush_putint32();


void init_getcomplex32();
GetStatus buf_getcomplex32(struct complex32 *x);
GetStatus buf_getarrcomplex32(struct complex32 * x, unsigned int vlen);
void init_putcomplex32();
void buf_putcomplex32(struct complex32 x);
void buf_putarrcomplex32(struct complex32 *x, unsigned int vlen);
void flush_putcomplex32();

void init_getint16();
GetStatus buf_getint16(int16 *x);
GetStatus buf_getarrint16(int16 *x, unsigned int vlen);
void init_putint16();
void buf_putint16(int16 x);
void buf_putarrint16(int16 * x, unsigned int vlen);
void flush_putint16();


void init_getcomplex16();
GetStatus buf_getcomplex16(struct complex16 *x);
GetStatus buf_getarrcomplex16(struct complex16 * x, unsigned int vlen);
void init_putcomplex16();
void buf_putcomplex16(struct complex16 x);
void buf_putarrcomplex16(struct complex16 *x, unsigned int vlen);
void flush_putcomplex16();




#ifdef SORA_PLATFORM
FINL
void write_time_stamp() {
	if (Globals.latencySampling > 0)
	{
		if (measurementInfo.nSamples % Globals.latencySampling == 0)
		{
			ULONGLONG time = SoraGetCPUTimestamp(&measurementInfo.tsinfo);
			ULONGLONG diff = time - measurementInfo.lastWrite;

			// Skip the first difference (the first two samples) as this one is usually an outlier
			if (measurementInfo.lastWrite > 0 && measurementInfo.nSamples / Globals.latencySampling > 1)
			{
				if (measurementInfo.aDiffPtr < Globals.latencyCDFSize)
				{
					measurementInfo.aDiff[measurementInfo.aDiffPtr] = diff;
					measurementInfo.aDiffPtr++;
				}
				if (diff < measurementInfo.minDiff)
				{
					measurementInfo.minDiff = diff;
				}
				if (diff > measurementInfo.maxDiff)
				{
					measurementInfo.maxDiff = diff;
				}
			}
			measurementInfo.lastWrite = time;
		}
		measurementInfo.nSamples++;
	}
}
#else
#define write_time_stamp()
#endif
