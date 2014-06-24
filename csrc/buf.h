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
#include "params.h"
#include "utils.h"
#include "bit.h"

typedef enum __GetStatus {
	GS_SUCCESS,
	GS_EOF
} GetStatus;



typedef struct _BufContextBlock { 
	// Bit buffers
	BitArrPtr input_buffer;
	unsigned int input_entries;
	unsigned int input_idx;
	unsigned int input_repetitions;
	unsigned int input_dummy_samples;
	unsigned int max_dummy_samples;

	int fst;
	unsigned char * output_buffer;
	unsigned int output_entries;
	unsigned int output_idx;
	FILE *output_file;


	// Byte buffer
	/* A buffer of elements, each element of size chunk_siz */
	char *chunk_input_buffer;
	unsigned int chunk_input_siz;
	unsigned int chunk_input_entries;
	unsigned int chunk_input_idx;
	unsigned int chunk_input_repeats;

	unsigned int chunk_input_dummy_samples;
	unsigned int chunk_max_dummy_samples;

	void(*parse_chunk_dbg)(char *buf, void *chunk);
	void(*print_chunk_dbg)(FILE *f, void *chunk);

	int chunk_fst;
	char *chunk_output_buffer;
	unsigned int chunk_output_siz;
	unsigned int chunk_output_entries;
	unsigned int chunk_output_idx;
	FILE *chunk_output_file;



	// Int8 buffers
	int8 *num8_input_buffer;
	unsigned int num8_input_entries;
	unsigned int num8_input_idx;
	unsigned int num8_input_repeats;

	unsigned int num8_input_dummy_samples;
	unsigned int num8_max_dummy_samples;

	int num8_fst;
	int8 *num8_output_buffer;
	unsigned int num8_output_entries;
	unsigned int num8_output_idx;
	FILE *num8_output_file;



	// Int16 buffers
	int16 *num16_input_buffer;
	unsigned int num16_input_entries;
	unsigned int num16_input_idx;
	unsigned int num16_input_repeats;

	unsigned int num16_input_dummy_samples;
	unsigned int num16_max_dummy_samples;

	int num16_fst;
	int16 *num16_output_buffer;
	unsigned int num16_output_entries;
	unsigned int num16_output_idx;
	FILE *num16_output_file;



	// Int32 buffers
	int32 *num_input_buffer;
	unsigned int num_input_entries;
	unsigned int num_input_idx;
	int num_input_repeats;

	unsigned int num_input_dummy_samples;
	unsigned int num_max_dummy_samples;

	int num_fst;
	int16 *num_output_buffer;
	unsigned int num_output_entries;
	unsigned int num_output_idx;
	FILE *num_output_file;

} BufContextBlock;



// Defined in buf_bytes.c for lack of better location
void initBufCtxBlock(BufContextBlock *blk);


void init_getbit(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getbit(BufContextBlock *blk, Bit *x);
GetStatus buf_getarrbit(BufContextBlock *blk, BitArrPtr x, unsigned int vlen);
void init_putbit(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putbit(BufContextBlock *blk, Bit x);
void buf_putarrbit(BufContextBlock *blk, BitArrPtr x, unsigned int vlen);
void flush_putbit(BufContextBlock *blk);


void init_getint32(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getint32(BufContextBlock *blk, int32 *x);
GetStatus buf_getarrint32(BufContextBlock *blk, int32 *x, unsigned int vlen);
void init_putint32(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putint32(BufContextBlock *blk, int32 x);
void buf_putarrint32(BufContextBlock *blk, int32 * x, unsigned int vlen);
void flush_putint32(BufContextBlock *blk);


void init_getcomplex32(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getcomplex32(BufContextBlock *blk, struct complex32 *x);
GetStatus buf_getarrcomplex32(BufContextBlock *blk, struct complex32 * x, unsigned int vlen);
void init_putcomplex32(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putcomplex32(BufContextBlock *blk, struct complex32 x);
void buf_putarrcomplex32(BufContextBlock *blk, struct complex32 *x, unsigned int vlen);
void flush_putcomplex32(BufContextBlock *blk);

void init_getint16(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getint16(BufContextBlock *blk, int16 *x);
GetStatus buf_getarrint16(BufContextBlock *blk, int16 *x, unsigned int vlen);
void init_putint16(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putint16(BufContextBlock *blk, int16 x);
void buf_putarrint16(BufContextBlock *blk, int16 * x, unsigned int vlen);
void flush_putint16(BufContextBlock *blk);


void init_getcomplex16(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getcomplex16(BufContextBlock *blk, struct complex16 *x);
GetStatus buf_getarrcomplex16(BufContextBlock *blk, struct complex16 * x, unsigned int vlen);
void init_putcomplex16(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putcomplex16(BufContextBlock *blk, struct complex16 x);
void buf_putarrcomplex16(BufContextBlock *blk, struct complex16 *x, unsigned int vlen);
void flush_putcomplex16(BufContextBlock *blk);



void init_getint8(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getint8(BufContextBlock *blk, int8 *x);
GetStatus buf_getarrint8(BufContextBlock *blk, int8 *x, unsigned int vlen);
void init_putint8(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putint8(BufContextBlock *blk, int8 x);
void buf_putarrint8(BufContextBlock *blk, int8 * x, unsigned int vlen);
void flush_putint8(BufContextBlock *blk);


void init_getcomplex8(BufContextBlock *blk, HeapContextBlock *hblk);
GetStatus buf_getcomplex8(BufContextBlock *blk, struct complex8 *x);
GetStatus buf_getarrcomplex8(BufContextBlock *blk, struct complex8 * x, unsigned int vlen);
void init_putcomplex8(BufContextBlock *blk, HeapContextBlock *hblk);
void buf_putcomplex8(BufContextBlock *blk, struct complex8 x);
void buf_putarrcomplex8(BufContextBlock *blk, struct complex8 *x, unsigned int vlen);
void flush_putcomplex8(BufContextBlock *blk);



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
