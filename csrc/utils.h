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

#ifdef SORA_PLATFORM
#include <basetsd.h>

// Most of this could be generic for different platforms but we need generic time types, 
// so we implement only for Sora at the moment
typedef struct {
	TIMESTAMPINFO tsinfo;
	ULONGLONG lastWrite;
	ULONGLONG minDiff;
	ULONGLONG maxDiff;
	ULONGLONG *aDiff;
	ulong aDiffPtr;
	ulong nSamples;
} TimeMeasurements;

extern TimeMeasurements measurementInfo;

FINL
void initMeasurementInfo(ulong size) {
	measurementInfo.tsinfo.use_rdtsc = 1;
	InitializeTimestampInfo(&measurementInfo.tsinfo, false);

	measurementInfo.aDiff = NULL;
	measurementInfo.aDiffPtr = 0;
	measurementInfo.nSamples = 0;
	if (size > 0) {
		measurementInfo.aDiff = (ULONGLONG*) malloc(size * sizeof(ULONGLONG));
		measurementInfo.minDiff = ((ULONGLONG)~((ULONGLONG)0));
		measurementInfo.maxDiff = 0;
		measurementInfo.lastWrite = 0;
	}
}

#endif

void bounds_check(int siz, int len, char *msg);
void blink_copy(void *dst, void *src, unsigned int siz);
