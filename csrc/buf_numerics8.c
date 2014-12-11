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
#include <errno.h>
#include <string.h>

#include "wpl_alloc.h"
#include "params.h"
#include "types.h"
#include "buf.h"


#ifdef SORA_PLATFORM
#include "sora_radio.h"
#endif



unsigned int parse_dbg_int8(char *dbg_buf, int8 *target)
{
	char *s = NULL;
	unsigned int i = 0;
	long val;

  char* trailing_comma = delete_trailing_comma(dbg_buf);
	s = strtok(dbg_buf, ",");

	if (s == NULL)
	{
		fprintf(stderr, "Input (debug) file contains no samples.");
		exit(1);
	}

	val = strtol(s, NULL, 10);
	if (errno == EINVAL)
	{
		fprintf(stderr, "Parse error when loading debug file.");
		exit(1);
	}

	target[i++] = (num8)val;

	while (s = strtok(NULL, ","))
	{
		val = strtol(s, NULL, 10);
		if (errno == EINVAL)
		{
			fprintf(stderr, "Parse error when loading debug file.");
			exit(1);
		}
		target[i++] = (num8)val;
	}

  restore_trailing_comma(trailing_comma);
	return i; // total number of entries
}

void init_getint8(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_in = 8;
	blk->total_in = 0;

	if (params->inType == TY_DUMMY)
	{
		blk->num8_max_dummy_samples = params->dummySamples;
	}

	if (params->inType == TY_MEM)
	{
		if (blk->mem_input_buf == NULL || blk->mem_input_buf_size == 0)
		{
			fprintf(stderr, "Error: input memory buffer not initialized\n");
			exit(1);
		}
		else
		{
			blk->num8_input_buffer = (int8 *)blk->mem_input_buf;
			blk->num8_input_entries = blk->mem_input_buf_size;
		}
	}

	if (params->inType == TY_FILE)
	{
		memsize_int sz;
		char *filebuffer;
		try_read_filebuffer(hblk, params->inFileName, params->inFileMode, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof should be enough
		blk->num8_input_buffer = (int8 *)try_alloc_bytes(hblk, sz * sizeof(int8));

		if (params->inFileMode == MODE_BIN)
		{
			unsigned int i;
			int8 *typed_filebuffer = (int8 *)filebuffer;
			for (i = 0; i < sz; i++)
			{
				blk->num8_input_buffer[i] = typed_filebuffer[i];
			}
			blk->num8_input_entries = i;
		}
		else
		{
			blk->num8_input_entries = parse_dbg_int8(filebuffer, blk->num8_input_buffer);
		}
	}

	/*
	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		InitSoraRx(params);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
	*/
}

FINL
GetStatus _buf_getint8(BlinkParams *params, BufContextBlock *blk, int8 *x)
{
	if (params->inType == TY_DUMMY)
	{
		if (blk->num8_input_dummy_samples >= blk->num8_max_dummy_samples && params->dummySamples != INF_REPEAT) return GS_EOF;
		blk->num8_input_dummy_samples++;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE || params->inType == TY_MEM)
	{
		// If we reached the end of the input buffer 
		if (blk->num8_input_idx >= blk->num8_input_entries)
		{
			// If no more repetitions are allowed 
			if (params->inFileRepeats != INF_REPEAT && blk->num8_input_repeats >= params->inFileRepeats)
			{
				return GS_EOF;
			}
			// Otherwise we set the index to 0 and increase repetition count
			blk->num8_input_idx = 0;
			blk->num8_input_repeats++;
		}

		*x = blk->num8_input_buffer[blk->num8_input_idx++];

		return GS_SUCCESS;
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}

GetStatus buf_getint8(BlinkParams *params, BufContextBlock *blk, int8 *x)
{
#ifdef STAMP_AT_READ
	write_time_stamp(params);
#endif	
	blk->total_in++;
	return _buf_getint8(params, blk, x);
}

FINL
GetStatus _buf_getarrint8(BlinkParams *params, BufContextBlock *blk, int8 *x, unsigned int vlen)
{
	if (params->inType == TY_DUMMY)
	{
		if (blk->num8_input_dummy_samples >= blk->num8_max_dummy_samples && params->dummySamples != INF_REPEAT) return GS_EOF;
		blk->num8_input_dummy_samples += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE || params->inType == TY_MEM)
	{
		if (blk->num8_input_idx + vlen > blk->num8_input_entries)
		{
			if (params->inFileRepeats != INF_REPEAT && blk->num8_input_repeats >= params->inFileRepeats)
			{
				if (blk->num8_input_idx != blk->num8_input_entries)
					fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
				return GS_EOF;
			}
			// Otherwise ignore trailing part of the file, not clear what that part may contain ...
			blk->num8_input_idx = 0;
			blk->num8_input_repeats++;
		}

		memcpy(x, &(blk->num8_input_buffer[blk->num8_input_idx]), vlen * sizeof(int8));
		blk->num8_input_idx += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only complex8 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}

GetStatus buf_getarrint8(BlinkParams *params, BufContextBlock *blk, int8 *x, unsigned int vlen)
{
#ifdef STAMP_AT_READ
	write_time_stamp(params);
#endif	
	blk->total_in += vlen;
	return _buf_getarrint8(params, blk, x, vlen);
}

void init_getcomplex8(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	// we just need to initialize the input buffer in the same way
	init_getint8(params, blk, hblk, unit_size*2);                              
	blk->size_in = 16;

	// since we will be doing this in integer granularity
	blk->num8_max_dummy_samples = params->dummySamples * 2; 
}

GetStatus buf_getcomplex8(BlinkParams *params, BufContextBlock *blk, complex8 *x)
{
#ifdef STAMP_AT_READ
	write_time_stamp(params);
#endif
	blk->total_in++;
	if (params->inType == TY_DUMMY || params->inType == TY_FILE || params->inType == TY_MEM)
	{
		GetStatus gs1 = _buf_getint8(params, blk, &(x->re));
		if (gs1 == GS_EOF)
		{
			return GS_EOF;
		}
		else
		{
			return (_buf_getint8(params, blk, &(x->im)));
		}
	}

	if (params->inType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora does not support 8-bit receive\n");
		exit(1);
	}

	return GS_EOF;
}

GetStatus buf_getarrcomplex8(BlinkParams *params, BufContextBlock *blk, complex8 *x, unsigned int vlen)
{
#ifdef STAMP_AT_READ
	write_time_stamp(params);
#endif
	blk->total_in += vlen;
	if (params->inType == TY_DUMMY || params->inType == TY_FILE || params->inType == TY_MEM)
	{
		return _buf_getarrint8(params, blk, (int8*)x, vlen * 2);
	}

	if (params->inType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora RX does not support Complex8\n");
		exit(1);
	}

	return GS_EOF;
}

void fprint_int8(BufContextBlock *blk, FILE *f, int8 val)
{
	if (blk->num8_fst)
	{
		fprintf(f, "%d", val);
		blk->num8_fst = 0;
	}
	else fprintf(f, ",%d", val);
}
void fprint_arrint8(BufContextBlock *blk, FILE *f, int8 *val, unsigned int vlen)
{
	unsigned int i;
	for (i = 0; i < vlen; i++)
	{
		fprint_int8(blk, f, val[i]);
	}
}


void init_putint8(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_out = 8;
	blk->total_out = 0;

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num8_output_buffer = (int8 *)malloc(params->outBufSize * sizeof(int8));
		blk->num8_output_entries = params->outBufSize;
		if (params->outType == TY_FILE)
		{
			if (params->outFileMode == MODE_BIN)
			{
				blk->num8_output_file = try_open(params->outFileName, "wb");
			}
			else
			{
				blk->num8_output_file = try_open(params->outFileName, "w");
			}
		}
	}

	if (params->outType == TY_MEM)
	{
		if (blk->mem_output_buf == NULL || blk->mem_output_buf_size == 0)
		{
			fprintf(stderr, "Error: output memory buffer not initialized\n");
			exit(1);
		}
		else
		{
			blk->num8_output_buffer = (int8*)blk->mem_output_buf;
			blk->num8_output_entries = blk->mem_output_buf_size;
		}
	}

	if (params->outType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only int16 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}

}


FINL
void _buf_putint8(BlinkParams *params, BufContextBlock *blk, int8 x)
{
	if (params->outType == TY_DUMMY)
	{
		return;
	}

	if (params->outType == TY_MEM)
	{
		blk->num8_output_buffer[blk->num8_output_idx++] = (int8)x;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
			fprint_int8(blk, blk->num8_output_file, x);
		else
		{
			if (blk->num8_output_idx == blk->num8_output_entries)
			{
				fwrite(blk->num8_output_buffer, blk->num8_output_entries, sizeof(int8), blk->num8_output_file);
				blk->num8_output_idx = 0;
			}
			blk->num8_output_buffer[blk->num8_output_idx++] = (int8)x;
		}
	}

	if (params->outType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only int16 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}


void buf_putint8(BlinkParams *params, BufContextBlock *blk, int8 x)
{
	blk->total_out ++;
#ifndef STAMP_AT_READ
	write_time_stamp(params);
#endif
	_buf_putint8(params, blk, x);
}


FINL
void _buf_putarrint8(BlinkParams *params, BufContextBlock *blk, int8 *x, unsigned int vlen)
{

	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_MEM)
	{
		memcpy((void*)(blk->num8_output_buffer + blk->num8_output_idx), (void*)x, vlen*sizeof(int8));
		blk->num8_output_idx += vlen;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
			fprint_arrint8(blk, blk->num8_output_file, x, vlen);
		else
		{
			if (blk->num8_output_idx + vlen >= blk->num8_output_entries)
			{
				// first write the first (num8_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = blk->num8_output_entries - blk->num8_output_idx;

				for (i = 0; i < m; i++)
					blk->num8_output_buffer[blk->num8_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(blk->num8_output_buffer, blk->num8_output_entries, sizeof(int8), blk->num8_output_file);

				// then write the rest
				for (blk->num8_output_idx = 0; blk->num8_output_idx < vlen - m; blk->num8_output_idx++)
					blk->num8_output_buffer[blk->num8_output_idx] = x[blk->num8_output_idx + m];
			}
			else
			{
				memcpy((void*)(blk->num8_output_buffer + blk->num8_output_idx), (void*)x, vlen*sizeof(int8));
				blk->num8_output_idx += vlen;
			}
		}
	}

	if (params->outType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only complex8 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}



void buf_putarrint8(BlinkParams *params, BufContextBlock *blk, int8 *x, unsigned int vlen)
{
	blk->total_out += vlen;
#ifndef STAMP_AT_READ
	write_time_stamp(params);
#endif
	_buf_putarrint8(params, blk, x, vlen);
}


void _flush_putint8(BlinkParams *params, BufContextBlock *blk, size_t size)
{
	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_BIN) {
			fwrite(blk->num8_output_buffer, size, blk->num8_output_idx, blk->num8_output_file);
			blk->num8_output_idx = 0;
		}
		fclose(blk->num8_output_file);
	}
}


void flush_putint8(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putint8(params, blk, sizeof(int8));
}


void init_putcomplex8(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_out = 16;
	blk->total_out = 0;

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num8_output_buffer = (int8 *)malloc(2 * params->outBufSize * sizeof(int8));
		blk->num8_output_entries = params->outBufSize * 2;
		if (params->outType == TY_FILE)
		{
			if (params->outFileMode == MODE_BIN)
			{
				blk->num8_output_file = try_open(params->outFileName, "wb");
			}
			else
			{
				blk->num8_output_file = try_open(params->outFileName, "w");
			}
		}
	}

	if (params->outType == TY_MEM)
	{
		if (blk->mem_output_buf == NULL || blk->mem_output_buf_size == 0)
		{
			fprintf(stderr, "Error: output memory buffer not initialized\n");
			exit(1);
		}
		else
		{
			blk->num8_output_buffer = (int8*)blk->mem_output_buf;
			blk->num8_output_entries = blk->mem_output_buf_size;
		}
	}

	/*
	if (params->outType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		InitSoraTx(params);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
	*/
}

void buf_putcomplex8(BlinkParams *params, BufContextBlock *blk, struct complex8 x)
{
	blk->total_out ++;
#ifndef STAMP_AT_READ
	write_time_stamp(params);
#endif

	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_FILE || params->outType == TY_MEM)
	{
		_buf_putint8(params, blk, x.re);
		_buf_putint8(params, blk, x.im);
	}

	if (params->outType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora TX does not support Complex8\n");
		exit(1);
	}


}
void buf_putarrcomplex8(BlinkParams *params, BufContextBlock *blk, struct complex8 *x, unsigned int vlen)
{
	blk->total_out += vlen;
#ifndef STAMP_AT_READ
	write_time_stamp(params);
#endif

	if (params->outType == TY_DUMMY || params->outType == TY_FILE || params->outType == TY_MEM)
	{
		_buf_putarrint8(params, blk, (int8 *)x, vlen * 2);
	}

	if (params->outType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora TX does not support Complex8\n");
		exit(1);
	}
}
void flush_putcomplex8(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putint8(params, blk, sizeof(complex8));
}
