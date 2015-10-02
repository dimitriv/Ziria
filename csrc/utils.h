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

#include "types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef SORA_PLATFORM
#include <basetsd.h>
#include <sora.h>

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


FINL
void initMeasurementInfo(TimeMeasurements *measurementInfo, ulong size) {
	measurementInfo->tsinfo.use_rdtsc = 1;
	InitializeTimestampInfo(&measurementInfo->tsinfo, false);

	measurementInfo->aDiff = NULL;
	measurementInfo->aDiffPtr = 0;
	measurementInfo->nSamples = 0;
	if (size > 0) {
		measurementInfo->aDiff = (ULONGLONG*) malloc(size * sizeof(ULONGLONG));
		measurementInfo->minDiff = ((ULONGLONG)~((ULONGLONG)0));
		measurementInfo->maxDiff = 0;
		measurementInfo->lastWrite = 0;
	}
}

#endif


void bounds_check(memsize_int siz, memsize_int len, char *msg);

void blink_copy(void *dst, const void *src, memsize_int siz);


char* delete_trailing_comma(char *s);
void restore_trailing_comma(char* trailing_comma);


// Dummy functions that can be insterted to preven Ziria compiler to overdo with inlining
// 
inline int8 __ext_do_not_inline_int8(int8 x) { return x; };
inline int16 __ext_do_not_inline_int16(int16 x) { return x; };
inline int32 __ext_do_not_inline_int32(int32 x) { return x; };
inline int64 __ext_do_not_inline_int64(int64 x) { return x; };
inline complex8 __ext_do_not_inline_complex8(complex8 x) { return x; };
inline complex16 __ext_do_not_inline_complex16(complex16 x) { return x; };
inline complex32 __ext_do_not_inline_complex32(complex32 x) { return x; };
inline complex64 __ext_do_not_inline_complex64(complex64 x) { return x; };


