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



unsigned int parse_dbg_int16(char *dbg_buf, int16 *target)
{
	

  char *s = NULL;
  unsigned int i = 0;
  long val;

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

  target[i++] = (num16) val; 

  while (s = strtok(NULL, ",")) 
  {
	  val = strtol(s,NULL,10);
	  if (errno == EINVAL) 
      {
		  fprintf(stderr,"Parse error when loading debug file.");
		  exit(1);
      }
	  target[i++] = (num16) val;
  }
  return i; // total number of entries
}

void init_getint16(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk)
{
	if (params->inType == TY_DUMMY)
	{
		blk->num16_max_dummy_samples = params->dummySamples;
	}

	if (params->inType == TY_FILE)
	{
		unsigned int sz; 
		char *filebuffer;
		try_read_filebuffer(hblk, params->inFileName, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof should be enough
		blk->num16_input_buffer = (int16 *)try_alloc_bytes(hblk, sz * sizeof(int16));

		if (params->inFileMode == MODE_BIN)
		{ 
			unsigned int i;
			int16 *typed_filebuffer = (int16 *) filebuffer;
			for (i=0; i < sz; i++)
			{
				blk->num16_input_buffer[i] = typed_filebuffer[i];
			}
			blk->num16_input_entries = i;
		}
		else 
		{
			blk->num16_input_entries = parse_dbg_int16(filebuffer, blk->num16_input_buffer);
		}
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		InitSoraRx(*params);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}
GetStatus buf_getint16(BlinkParams *params, BufContextBlock *blk, int16 *x)
{

	if (params->inType == TY_DUMMY)
	{
		if (blk->num16_input_dummy_samples >= blk->num16_max_dummy_samples && params->dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->num16_input_dummy_samples++;
		*x = 0;
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE)
	{
		// If we reached the end of the input buffer 
		if (blk->num16_input_idx >= blk->num16_input_entries)
		{
			// If no more repetitions are allowed 
			if (params->inFileRepeats != INF_REPEAT && blk->num16_input_repeats >= params->inFileRepeats)
			{
				return GS_EOF;
			}
			// Otherwise we set the index to 0 and increase repetition count
			blk->num16_input_idx = 0;
			blk->num16_input_repeats++;
		}

		*x = blk->num16_input_buffer[blk->num16_input_idx++];

		return GS_SUCCESS;
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only Complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}
GetStatus buf_getarrint16(BlinkParams *params, BufContextBlock *blk, int16 *x, unsigned int vlen)
{

	if (params->inType == TY_DUMMY)
	{
		if (blk->num16_input_dummy_samples >= blk->num16_max_dummy_samples && params->dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->num16_input_dummy_samples += vlen;
		memset(x,0,vlen*sizeof(int16));
		return GS_SUCCESS;
	}

	if (params->inType == TY_FILE)
	{
		if (blk->num16_input_idx + vlen > blk->num16_input_entries)
		{
			if (params->inFileRepeats != INF_REPEAT && blk->num16_input_repeats >= params->inFileRepeats)
			{
				if (blk->num16_input_idx != blk->num16_input_entries)
					fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
				return GS_EOF;
			}
			// Otherwise ignore trailing part of the file, not clear what that part may contain ...
			blk->num16_input_idx = 0;
			blk->num16_input_repeats++;
		}
	
		memcpy(x, &(blk->num16_input_buffer[blk->num16_input_idx]), vlen * sizeof(int16));
		blk->num16_input_idx += vlen;
		return GS_SUCCESS;
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		fprintf(stderr, "Sora RX supports only Complex16 type.\n");
		exit(1);
#endif
	}

	return GS_EOF;
}

void init_getcomplex16(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk)
{
	init_getint16(params, blk, hblk);                                // we just need to initialize the input buffer in the same way
	blk->num16_max_dummy_samples = params->dummySamples * 2; // since we will be doing this in integer granularity
}

GetStatus buf_getcomplex16(BlinkParams *params, BufContextBlock *blk, complex16 *x)
{
	if (params->inType == TY_DUMMY || params->inType == TY_FILE)
	{
		GetStatus gs1 = buf_getint16(params, blk, & (x->re));
		if (gs1 == GS_EOF) 
		{ 
			return GS_EOF;
		}
		else
		{
			return (buf_getint16(params, blk, & (x->im)));
		}
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		readSora(*params, x, 1);
		return GS_SUCCESS;
#endif
	}

	return GS_EOF;
}

GetStatus buf_getarrcomplex16(BlinkParams *params, BufContextBlock *blk, complex16 *x, unsigned int vlen)
{
	if (params->inType == TY_DUMMY || params->inType == TY_FILE)
	{
		return (buf_getarrint16(params, blk, (int16*) x,vlen*2));
	}

	if (params->inType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		readSora(*params, x, vlen);
		return GS_SUCCESS;
#endif
	}

	return GS_EOF;
}

void fprint_int16(BufContextBlock *blk, FILE *f, int16 val)
{
	if (blk->num16_fst)
	{
		fprintf(f,"%d",val);
		blk->num16_fst = 0;
	}
	else fprintf(f,",%d",val);
}
void fprint_arrint16(BufContextBlock *blk, FILE *f, int16 *val, unsigned int vlen)
{
	unsigned int i;
	for (i=0; i < vlen; i++)
	{
		fprint_int16(blk,f,val[i]);
	}
}


void init_putint16(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk)
{
	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num16_output_buffer = (int16 *)malloc(params->outBufSize * sizeof(int16));
		blk->num16_output_entries = params->outBufSize;
		if (params->outType == TY_FILE)
			blk->num16_output_file = try_open(params->outFileName, "w");
	}

	if (params->outType == TY_SORA) 
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


FINL
void _buf_putint16(BlinkParams *params, BufContextBlock *blk, int16 x)
{
	if (params->outType == TY_DUMMY)
	{
		return;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
			fprint_int16(blk, blk->num16_output_file, x);
		else 
		{
			if (blk->num16_output_idx == blk->num16_output_entries)
			{
				fwrite(blk->num16_output_buffer, blk->num16_output_entries, sizeof(int16), blk->num16_output_file);
				blk->num16_output_idx = 0;
			}
			blk->num16_output_buffer[blk->num16_output_idx++] = (int16)x;
		}
	}

	if (params->outType == TY_SORA) 
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


void buf_putint16(BlinkParams *params, BufContextBlock *blk, int16 x)
{
	write_time_stamp(params);
	_buf_putint16(params, blk, x);
}


FINL
void _buf_putarrint16(BlinkParams *params, BufContextBlock *blk, int16 *x, unsigned int vlen)
{

	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG) 
			fprint_arrint16(blk, blk->num16_output_file, x, vlen);
		else
		{
			if (blk->num16_output_idx + vlen >= blk->num16_output_entries)
			{
				// first write the first (num16_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = blk->num16_output_entries - blk->num16_output_idx;

				for (i = 0; i < m; i++)
					blk->num16_output_buffer[blk->num16_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(blk->num16_output_buffer, blk->num16_output_entries, sizeof(int16), blk->num16_output_file);

				// then write the rest
				for (blk->num16_output_idx = 0; blk->num16_output_idx < vlen - m; blk->num16_output_idx++)
					blk->num16_output_buffer[blk->num16_output_idx] = x[blk->num16_output_idx + m];
			}
		}
	}

	if (params->outType == TY_SORA) 
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



void buf_putarrint16(BlinkParams *params, BufContextBlock *blk, int16 *x, unsigned int vlen)
{
	write_time_stamp(params);
	_buf_putarrint16(params, blk, x, vlen);
}


void flush_putint16(BlinkParams *params, BufContextBlock *blk)
{
	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_BIN) {
			fwrite(blk->num16_output_buffer, sizeof(int16), blk->num16_output_idx, blk->num16_output_file);
			blk->num16_output_idx = 0;
		}
		fclose(blk->num16_output_file);
	}
}


void init_putcomplex16(BlinkParams *params, BufContextBlock *blk, HeapContextBlock *hblk)
{
	write_time_stamp(params);

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->num16_output_buffer = (int16 *)malloc(2 * params->outBufSize * sizeof(int16));
		blk->num16_output_entries = params->outBufSize * 2;
		if (params->outType == TY_FILE)
			blk->num16_output_file = try_open(params->outFileName, "w");
	}

	if (params->outType == TY_SORA)
	{
#ifdef SORA_PLATFORM
		InitSoraTx(*params);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}

void buf_putcomplex16(BlinkParams *params, BufContextBlock *blk, struct complex16 x)
{
	write_time_stamp(params);

	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_FILE)
	{
		_buf_putint16(params, blk, x.re);
		_buf_putint16(params, blk, x.im);
	}

	if (params->outType == TY_SORA) 
	{
#ifdef SORA_PLATFORM
		writeSora(*params, &x, 1);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}

}
void buf_putarrcomplex16(BlinkParams *params, BufContextBlock *blk, struct complex16 *x, unsigned int vlen)
{
	write_time_stamp(params);

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		_buf_putarrint16(params, blk, (int16 *)x, vlen * 2);
	}

	if (params->outType == TY_SORA) 
	{
#ifdef SORA_PLATFORM
		writeSora(*params, x, vlen);
#else
		fprintf(stderr, "Sora supported only on WinDDK platform.\n");
		exit(1);
#endif
	}
}
void flush_putcomplex16(BlinkParams *params, BufContextBlock *blk)
{
	flush_putint16(params, blk);
}
