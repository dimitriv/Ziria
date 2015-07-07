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



unsigned int parse_dbg_int32(char *dbg_buf, num32 *target)
{
	
  char *s = NULL;
  unsigned int i = 0;
  long val;

  char* trailing_comma = delete_trailing_comma(dbg_buf);
  s = strtok(dbg_buf, ",");

  if (s == NULL) 
  {
	  fprintf(stderr,"Input (debug) file contains no samples.");
	  exit(1);
  }

  val = strtol(s,NULL,10);
  if (errno == EINVAL) 
  {
      fprintf(stderr,"Parse error when loading debug file.");
      exit(1);
  }

  target[i++] = val; 

  while (s = strtok(NULL, ",")) 
  {
	  val = strtol(s,NULL,10);
	  if (errno == EINVAL) 
      {
		  fprintf(stderr,"Parse error when loading debug file.");
		  exit(1);
      }
	  target[i++] = val;
  }

  restore_trailing_comma(trailing_comma);
  return i; // total number of entries
}

void _init_getint32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->total_in = 0;

	if (params->inType == TY_DUMMY)
	{
		blk->num_max_dummy_samples = params->dummySamples;
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
			blk->num_input_buffer = (int32 *)blk->mem_input_buf;
			blk->num_input_entries = blk->mem_input_buf_size / (blk->size_in / 8);
		}
	}

	if (params->inType == TY_FILE)
	{
		memsize_int sz;
		char *filebuffer;
		try_read_filebuffer(hblk, params->inFileName, params->inFileMode, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof should be enough
		blk->num_input_buffer = (int32 *)try_alloc_bytes(hblk, sz * sizeof(int32));

		if (params->inFileMode == MODE_BIN)
		{ 
			unsigned int i;
			int16 *typed_filebuffer = (int16 *) filebuffer;
			blk->num_input_entries = sz / 4;					// We always count entries in ints and do two reads for complex
			for (i = 0; i < blk->num_input_entries; i++)
			{
				blk->num_input_buffer[i] = typed_filebuffer[i];
			}
		}
		else 
		{
			blk->num_input_entries = parse_dbg_int32(filebuffer, blk->num_input_buffer);
		}
	}

	if (params->inType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora does not support 32-bit receive\n");
		exit(1);
	}
}

void init_getint32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	// Change the values that differ in complex
	blk->size_in = 32;

	// we just need to initialize the input buffer in the same way
	_init_getint32(params, blk, hblk, unit_size);                              // we just need to initialize the input buffer in the same way
}


FINL
GetStatus _buf_getint32(BlinkParams *params, BufContextBlock *blk, int32 *x)
{
	if (params->inType == TY_DUMMY)
	{
		if (blk->num_input_dummy_samples >= blk->num_max_dummy_samples && params->dummySamples != INF_REPEAT) return GS_EOF;
		blk->num_input_dummy_samples++;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE || params->inType == TY_MEM)
	{
		// If we reached the end of the input buffer 
		if (blk->num_input_idx >= blk->num_input_entries)
		{
			// If no more repetitions are allowed 
			if (params->inFileRepeats != INF_REPEAT && blk->num_input_repeats >= params->inFileRepeats)
			{
				return GS_EOF;
			}
			// Otherwise we set the index to 0 and increase repetition count
			blk->num_input_idx = 0;
			blk->num_input_repeats++;
		}

		*x = blk->num_input_buffer[blk->num_input_idx++];

		return GS_SUCCESS;
	}

	if (params->inType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only Complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}

GetStatus buf_getint32(BlinkParams *params, BufContextBlock *blk, int32 *x)
{
	if (params->timeStampAtRead)
		write_time_stamp(params);
	blk->total_in++;
	return _buf_getint32(params, blk, x);
}

FINL
GetStatus _buf_getarrint32(BlinkParams *params, BufContextBlock *blk, int32 *x, unsigned int vlen)
{
	if (params->inType == TY_DUMMY)
	{
		if (blk->num_input_dummy_samples >= blk->num_max_dummy_samples && params->dummySamples != INF_REPEAT) return GS_EOF;
		blk->num_input_dummy_samples += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE || params->inType == TY_MEM)
	{
		if (blk->num_input_idx + vlen > blk->num_input_entries)
		{
			if (params->inFileRepeats != INF_REPEAT && blk->num_input_repeats >= params->inFileRepeats)
			{
				if (blk->num_input_idx != blk->num_input_entries)
					fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
				return GS_EOF;
			}
			// Otherwise ignore trailing part of the file, not clear what that part may contain ...
			blk->num_input_idx = 0;
			blk->num_input_repeats++;
		}

		memcpy(x, &(blk->num_input_buffer[blk->num_input_idx]), vlen * sizeof(int32));
		blk->num_input_idx += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only Complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}


GetStatus buf_getarrint32(BlinkParams *params, BufContextBlock *blk, int32 *x, unsigned int vlen)
{
	if (params->timeStampAtRead)
		write_time_stamp(params);
	blk->total_in += vlen;
	return _buf_getarrint32(params, blk, x, vlen);
}



void init_getcomplex32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	if (params->inType == TY_DUMMY || params->inType == TY_FILE || params->inType == TY_MEM)
	{
		// Change the values that differ in complex
		blk->size_in = 64;

		// we just need to initialize the input buffer in the same way
		_init_getint32(params, blk, hblk, unit_size*2);                              // we just need to initialize the input buffer in the same way

		// since we will be doing this in integer granularity
		blk->num_max_dummy_samples = params->dummySamples * 2; // since we will be doing this in integer granularity
	}

	if (params->inType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora RX does not support Complex32\n");
		exit(1);
	}
}


GetStatus buf_getcomplex32(BlinkParams *params, BufContextBlock *blk, complex32 *x)
{
	if (params->timeStampAtRead)
		write_time_stamp(params);
	blk->total_in++;
	GetStatus gs1 = _buf_getint32(params, blk, &(x->re));
	if (gs1 == GS_EOF) 
	{ 
		return GS_EOF;
	}
	else
	{
		return (_buf_getint32(params, blk, & (x->im)));
	}
}
GetStatus buf_getarrcomplex32(BlinkParams *params, BufContextBlock *blk, complex32 *x, unsigned int vlen)
{
	if (params->timeStampAtRead)
		write_time_stamp(params);
	blk->total_in += vlen;
	return (_buf_getarrint32(params, blk, (int32*)x, vlen * 2));
}

void fprint_int32(BufContextBlock *blk, FILE *f, int32 val)
{
	if (blk->num_fst == 1)
	{
		fprintf(f,"%d",val);
		blk->num_fst = 0;
	}
	else fprintf(f,",%d",val);
}
void fprint_arrint32(BufContextBlock *blk, FILE *f, int32 *val, unsigned int vlen)
{
	unsigned int i;
	for (i=0; i < vlen; i++)
	{
		fprint_int32(blk, f, val[i]);
	}
}


void init_putint32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_out = 32;
	blk->total_out = 0;

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num_output_buffer = (int32 *)malloc(params->outBufSize * sizeof(int32));
		blk->num_output_entries = params->outBufSize;
		if (params->outType == TY_FILE)
		{
			if (params->outFileMode == MODE_BIN)
			{
				blk->num_output_file = try_open(params->outFileName, "wb");
			}
			else
			{
				blk->num_output_file = try_open(params->outFileName, "w");
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
			blk->num_output_buffer = (int32*)blk->mem_output_buf;
			blk->num_output_entries = blk->mem_output_buf_size / (blk->size_out / 8);
		}
	}

	if (params->outType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora TX does not support Int32\n");
		exit(1);
	}
}

FINL
void _buf_putint32(BlinkParams *params, BufContextBlock *blk, int32 x)
{
	if (params->outType == TY_DUMMY)
	{
		return;
	}

	if (params->outType == TY_MEM)
	{
		blk->num_output_buffer[blk->num_output_idx++] = (int32)x;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
			fprint_int32(blk, blk->num_output_file, x);
		else 
		{
			if (blk->num_output_idx == blk->num_output_entries)
			{
				fwrite(blk->num_output_buffer, blk->num_output_entries, sizeof(int32), blk->num_output_file);
				blk->num_output_idx = 0;
			}
			blk->num_output_buffer[blk->num_output_idx++] = (int32)x;
		}
	}

	if (params->outType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only Complex16 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}


void buf_putint32(BlinkParams *params, BufContextBlock *blk, int32 x)
{
	blk->total_out++;
	if (!params->timeStampAtRead)
		write_time_stamp(params);
	_buf_putint32(params, blk, x);
}


FINL
void _buf_putarrint32(BlinkParams *params, BufContextBlock *blk, int32 *x, unsigned int vlen)
{
	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_MEM)
	{
		memcpy((void*)(blk->num_output_buffer + blk->num_output_idx), (void*)x, vlen*sizeof(int32));
		blk->num_output_idx += vlen;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG) 
			fprint_arrint32(blk, blk->num_output_file, x, vlen);
		else
		{
			if (blk->num_output_idx + vlen >= blk->num_output_entries)
			{
				// first write the first (num_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = blk->num_output_entries - blk->num_output_idx;

				for (i = 0; i < m; i++)
					blk->num_output_buffer[blk->num_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(blk->num_output_buffer, blk->num_output_entries, sizeof(int32), blk->num_output_file);

				// then write the rest
				for (blk->num_output_idx = 0; blk->num_output_idx < vlen - m; blk->num_output_idx++)
					blk->num_output_buffer[blk->num_output_idx] = x[blk->num_output_idx + m];
			}
			else
			{
				memcpy((void*)(blk->num_output_buffer + blk->num_output_idx), (void*)x, vlen*sizeof(int32));
				blk->num_output_idx += vlen;
			}
		}
	}

	if (params->outType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only Complex16 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}

void buf_putarrint32(BlinkParams *params, BufContextBlock *blk, int32 *x, unsigned int vlen)
{
	blk->total_out += vlen;
	if (!params->timeStampAtRead)
		write_time_stamp(params);
	_buf_putarrint32(params, blk, x, vlen);
}


void _flush_putint32(BlinkParams *params, BufContextBlock *blk, size_t size)
{
	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_BIN) {
			fwrite(blk->num_output_buffer, sizeof(int32), blk->num_output_idx, blk->num_output_file);
		}
	}
	blk->num_output_idx = 0;
}

void flush_putint32(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putint32(params, blk, sizeof(int32));
	if (params->outType == TY_FILE)
	{
		fclose(blk->num_output_file);
	}
}

void reset_putint32(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putint32(params, blk, sizeof(int32));
}

void init_putcomplex32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_out = 64;
	blk->total_out = 0;

	write_time_stamp(params);

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num_output_buffer = (int32 *)malloc(2 * params->outBufSize * sizeof(int32));
		blk->num_output_entries = params->outBufSize * 2;
		if (params->outType == TY_FILE)
		{
			if (params->outFileMode == MODE_BIN)
			{
				blk->num_output_file = try_open(params->outFileName, "wb");
			}
			else
			{
				blk->num_output_file = try_open(params->outFileName, "w");
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
			blk->num_output_buffer = (int32*)blk->mem_output_buf;
			blk->num_output_entries = blk->mem_output_buf_size / (2 * blk->size_out / 8);
		}
	}

	if (params->outType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora TX does not support Complex32\n");
		exit(1);
	}
}
void buf_putcomplex32(BlinkParams *params, BufContextBlock *blk, struct complex32 x)
{
	blk->total_out++;
	if (!params->timeStampAtRead)
		write_time_stamp(params);

	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_FILE || params->outType == TY_MEM)
	{
		_buf_putint32(params, blk, x.re);
		_buf_putint32(params, blk, x.im);
	}

	if (params->outType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora TX does not support Complex32\n");
		exit(1);
	}
}
void buf_putarrcomplex32(BlinkParams *params, BufContextBlock *blk, struct complex32 *x, unsigned int vlen)
{
	blk->total_out += vlen;
	if (!params->timeStampAtRead)
		write_time_stamp(params);

	if (params->outType == TY_DUMMY || params->outType == TY_FILE || params->outType == TY_MEM)
	{
		_buf_putarrint32(params, blk, (int32 *)x, vlen * 2);
	}

	if (params->outType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora TX does not support Complex32\n");
		exit(1);
	}
}
void flush_putcomplex32(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putint32(params, blk, sizeof(complex32));
	if (params->outType == TY_FILE)
	{
		fclose(blk->num_output_file);
	}
}

void reset_putcomplex32(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putint32(params, blk, sizeof(complex32));
}


/*************************************************************** 
                            uint32 
 ***************************************************************/

unsigned int parse_dbg_uint32(char *dbg_buf, uint32 *target)
{
	
  char *s = NULL;
  unsigned int i = 0;
  unsigned long val;

  char* trailing_comma = delete_trailing_comma(dbg_buf);
  s = strtok(dbg_buf, ",");

  if (s == NULL) 
  {
	  fprintf(stderr,"Input (debug) file contains no samples.");
	  exit(1);
  }

  val = strtoul(s,NULL,10);
  if (errno == EINVAL) 
  {
      fprintf(stderr,"Parse error when loading debug file.");
      exit(1);
  }

  target[i++] = (uint32)val; 

  while (s = strtok(NULL, ",")) 
  {
	  val = strtoul(s,NULL,10);
	  if (errno == EINVAL) 
      {
		  fprintf(stderr,"Parse error when loading debug file.");
		  exit(1);
      }
	  target[i++] = (uint32)val;
  }

  restore_trailing_comma(trailing_comma);
  return i; // total number of entries
}

void _init_getuint32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->total_in = 0;

	if (params->inType == TY_DUMMY)
	{
		blk->num_max_dummy_samples = params->dummySamples;
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
			blk->num_input_buffer = (int32 *)blk->mem_input_buf;
			blk->num_input_entries = blk->mem_input_buf_size / (blk->size_in / 8);
		}
	}

	if (params->inType == TY_FILE)
	{
		memsize_int sz;
		char *filebuffer;
		try_read_filebuffer(hblk, params->inFileName, params->inFileMode, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof should be enough
		blk->num_input_buffer = (int32 *)try_alloc_bytes(hblk, sz * sizeof(uint32));

		if (params->inFileMode == MODE_BIN)
		{ 
			unsigned int i;
			int16 *typed_filebuffer = (int16 *) filebuffer;
			blk->num_input_entries = sz / 4;					// We always count entries in ints and do two reads for complex
			for (i = 0; i < blk->num_input_entries; i++)
			{
				blk->num_input_buffer[i] = typed_filebuffer[i];
			}
		}
		else 
		{
			blk->num_input_entries = parse_dbg_uint32(filebuffer, (uint32 *) blk->num_input_buffer);
		}
	}

	if (params->inType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora does not support 32-bit receive\n");
		exit(1);
	}
}

void init_getuint32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	// Change the values that differ in complex
	blk->size_in = 32;

	// we just need to initialize the input buffer in the same way
	_init_getuint32(params, blk, hblk, unit_size);                              // we just need to initialize the input buffer in the same way
}


FINL
GetStatus _buf_getuint32(BlinkParams *params, BufContextBlock *blk, uint32 *x)
{
	if (params->inType == TY_DUMMY)
	{
		if (blk->num_input_dummy_samples >= blk->num_max_dummy_samples && params->dummySamples != INF_REPEAT) return GS_EOF;
		blk->num_input_dummy_samples++;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE || params->inType == TY_MEM)
	{
		// If we reached the end of the input buffer 
		if (blk->num_input_idx >= blk->num_input_entries)
		{
			// If no more repetitions are allowed 
			if (params->inFileRepeats != INF_REPEAT && blk->num_input_repeats >= params->inFileRepeats)
			{
				return GS_EOF;
			}
			// Otherwise we set the index to 0 and increase repetition count
			blk->num_input_idx = 0;
			blk->num_input_repeats++;
		}

		*x = blk->num_input_buffer[blk->num_input_idx++];

		return GS_SUCCESS;
	}

	if (params->inType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only Complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}

GetStatus buf_getuint32(BlinkParams *params, BufContextBlock *blk, uint32 *x)
{
	if (params->timeStampAtRead)
		write_time_stamp(params);
	blk->total_in++;
	return _buf_getuint32(params, blk, x);
}

FINL
GetStatus _buf_getarruint32(BlinkParams *params, BufContextBlock *blk, uint32 *x, unsigned int vlen)
{
	if (params->inType == TY_DUMMY)
	{
		if (blk->num_input_dummy_samples >= blk->num_max_dummy_samples && params->dummySamples != INF_REPEAT) return GS_EOF;
		blk->num_input_dummy_samples += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE || params->inType == TY_MEM)
	{
		if (blk->num_input_idx + vlen > blk->num_input_entries)
		{
			if (params->inFileRepeats != INF_REPEAT && blk->num_input_repeats >= params->inFileRepeats)
			{
				if (blk->num_input_idx != blk->num_input_entries)
					fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
				return GS_EOF;
			}
			// Otherwise ignore trailing part of the file, not clear what that part may contain ...
			blk->num_input_idx = 0;
			blk->num_input_repeats++;
		}

		memcpy(x, &(blk->num_input_buffer[blk->num_input_idx]), vlen * sizeof(uint32));
		blk->num_input_idx += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only Complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}


GetStatus buf_getarruint32(BlinkParams *params, BufContextBlock *blk, uint32 *x, unsigned int vlen)
{
	if (params->timeStampAtRead)
		write_time_stamp(params);
	blk->total_in += vlen;
	return _buf_getarruint32(params, blk, x, vlen);
}

void fprint_uint32(BufContextBlock *blk, FILE *f, uint32 val)
{
	if (blk->num_fst == 1)
	{
		fprintf(f,"%u",val);
		blk->num_fst = 0;
	}
	else fprintf(f,",%u",val);
}
void fprint_arruint32(BufContextBlock *blk, FILE *f, uint32 *val, unsigned int vlen)
{
	unsigned int i;
	for (i=0; i < vlen; i++)
	{
		fprint_uint32(blk, f, val[i]);
	}
}


void init_putuint32(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_out = 32;
	blk->total_out = 0;

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num_output_buffer = (int32 *)malloc(params->outBufSize * sizeof(uint32));
		blk->num_output_entries = params->outBufSize;
		if (params->outType == TY_FILE)
		{
			if (params->outFileMode == MODE_BIN)
			{
				blk->num_output_file = try_open(params->outFileName, "wb");
			}
			else
			{
				blk->num_output_file = try_open(params->outFileName, "w");
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
			blk->num_output_buffer = (int32*)blk->mem_output_buf;
			blk->num_output_entries = blk->mem_output_buf_size / (blk->size_out / 8);
		}
	}

	if (params->outType == TY_SDR)
	{
		fprintf(stderr, "Error: Sora TX does not support Uint32\n");
		exit(1);
	}
}

FINL
void _buf_putuint32(BlinkParams *params, BufContextBlock *blk, uint32 x)
{
	if (params->outType == TY_DUMMY)
	{
		return;
	}

	if (params->outType == TY_MEM)
	{
		blk->num_output_buffer[blk->num_output_idx++] = (uint32)x;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
			fprint_uint32(blk, blk->num_output_file, x);
		else 
		{
			if (blk->num_output_idx == blk->num_output_entries)
			{
				fwrite(blk->num_output_buffer, blk->num_output_entries, sizeof(uint32), blk->num_output_file);
				blk->num_output_idx = 0;
			}
			blk->num_output_buffer[blk->num_output_idx++] = (uint32)x;
		}
	}

	if (params->outType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only Complex16 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}


void buf_putuint32(BlinkParams *params, BufContextBlock *blk, uint32 x)
{
	blk->total_out++;
	if (!params->timeStampAtRead)
		write_time_stamp(params);
	_buf_putuint32(params, blk, x);
}


FINL
void _buf_putarruint32(BlinkParams *params, BufContextBlock *blk, uint32 *x, unsigned int vlen)
{
	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_MEM)
	{
		memcpy((void*)(blk->num_output_buffer + blk->num_output_idx), (void*)x, vlen*sizeof(uint32));
		blk->num_output_idx += vlen;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG) 
			fprint_arruint32(blk, blk->num_output_file, x, vlen);
		else
		{
			if (blk->num_output_idx + vlen >= blk->num_output_entries)
			{
				// first write the first (num_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = blk->num_output_entries - blk->num_output_idx;

				for (i = 0; i < m; i++)
					blk->num_output_buffer[blk->num_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(blk->num_output_buffer, blk->num_output_entries, sizeof(uint32), blk->num_output_file);

				// then write the rest
				for (blk->num_output_idx = 0; blk->num_output_idx < vlen - m; blk->num_output_idx++)
					blk->num_output_buffer[blk->num_output_idx] = x[blk->num_output_idx + m];
			}
			else
			{
				memcpy((void*)(blk->num_output_buffer + blk->num_output_idx), (void*)x, vlen*sizeof(uint32));
				blk->num_output_idx += vlen;
			}
		}
	}

	if (params->outType == TY_SDR)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora TX supports only Complex16 type.\n");
		exit(1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}

void buf_putarruint32(BlinkParams *params, BufContextBlock *blk, uint32 *x, unsigned int vlen)
{
	blk->total_out += vlen;
	if (!params->timeStampAtRead)
		write_time_stamp(params);
	_buf_putarruint32(params, blk, x, vlen);
}


void _flush_putuint32(BlinkParams *params, BufContextBlock *blk, size_t size)
{
	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_BIN) {
			fwrite(blk->num_output_buffer, sizeof(uint32), blk->num_output_idx, blk->num_output_file);
		}
	}
	blk->num_output_idx = 0;
}

void flush_putuint32(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putuint32(params, blk, sizeof(uint32));
	if (params->outType == TY_FILE)
	{
		fclose(blk->num_output_file);
	}
}

void reset_putuint32(BlinkParams *params, BufContextBlock *blk)
{
	_flush_putuint32(params, blk, sizeof(uint32));
}



