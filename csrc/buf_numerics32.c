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

void init_getint32(BufContextBlock *blk, HeapContextBlock *hblk)
{
	if (Globals.inType == TY_DUMMY)
	{
		blk->num_max_dummy_samples = Globals.dummySamples;
	}

	if (Globals.inType == TY_FILE)
	{
		unsigned int sz; 
		char *filebuffer;
		try_read_filebuffer(hblk, Globals.inFileName, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof should be enough
		blk->num_input_buffer = (int32 *)try_alloc_bytes(hblk, sz * sizeof(int32));

		if (Globals.inFileMode == MODE_BIN)
		{ 
			unsigned int i;
			int16 *typed_filebuffer = (int16 *) filebuffer;
			for (i=0; i < sz / 2; i++)
			{
				blk->num_input_buffer[i] = typed_filebuffer[i];
			}
			blk->num_input_entries = i;
		}
		else 
		{
			blk->num_input_entries = parse_dbg_int32(filebuffer, blk->num_input_buffer);
		}
	}

	if (Globals.inType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora does not support 32-bit receive\n");
		exit(1);
	}
}
GetStatus buf_getint32(BufContextBlock *blk, int32 *x)
{

	if (Globals.inType == TY_DUMMY)
	{
		if (blk->num_input_dummy_samples >= blk->num_max_dummy_samples && Globals.dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->num_input_dummy_samples++;
		*x = 0;
		return GS_SUCCESS;
	}


	// If we reached the end of the input buffer 
	if (blk->num_input_idx >= blk->num_input_entries)
	{
		// If no more repetitions are allowed 
		if (Globals.inFileRepeats != INF_REPEAT && blk->num_input_repeats >= Globals.inFileRepeats)
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
GetStatus buf_getarrint32(BufContextBlock *blk, int32 *x, unsigned int vlen)
{

	if (Globals.inType == TY_DUMMY)
	{
		if (blk->num_input_dummy_samples >= blk->num_max_dummy_samples && Globals.dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->num_input_dummy_samples += vlen;
		memset(x,0,vlen*sizeof(int32));
		return GS_SUCCESS;
	}

	if (blk->num_input_idx + vlen > blk->num_input_entries)
	{
		if (Globals.inFileRepeats != INF_REPEAT && blk->num_input_repeats >= Globals.inFileRepeats)
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

void init_getcomplex32(BufContextBlock *blk, HeapContextBlock *hblk)
{
	if (Globals.inType == TY_DUMMY || Globals.inType == TY_FILE)
	{
		init_getint32(blk, hblk);                              // we just need to initialize the input buffer in the same way
		blk->num_max_dummy_samples = Globals.dummySamples * 2; // since we will be doing this in integer granularity
	}

	if (Globals.inType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora RX does not support Complex32\n");
		exit(1);
	}
}
GetStatus buf_getcomplex32(BufContextBlock *blk, complex32 *x)
{
	GetStatus gs1 = buf_getint32(blk, & (x->re));
	if (gs1 == GS_EOF) 
	{ 
		return GS_EOF;
	}
	else
	{
		return (buf_getint32(blk, & (x->im)));
	}
}
GetStatus buf_getarrcomplex32(BufContextBlock *blk, complex32 *x, unsigned int vlen)
{
	return (buf_getarrint32(blk, (int32*) x,vlen*2));
}

void fprint_int32(BufContextBlock *blk, FILE *f, int32 val)
{
	if (blk->num_fst)
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


void init_putint32(BufContextBlock *blk, HeapContextBlock *hblk)
{
	if (Globals.outType == TY_DUMMY || Globals.outType == TY_FILE)
	{
		blk->num_output_buffer = (int16 *)malloc(Globals.outBufSize * sizeof(int16));
		blk->num_output_entries = Globals.outBufSize;
		if (Globals.outType == TY_FILE)
			blk->num_output_file = try_open(Globals.outFileName, "w");
	}

	if (Globals.outType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora TX does not support Int32\n");
		exit(1);
	}
}

FINL
void _buf_putint32(BufContextBlock *blk, int32 x)
{
	if (Globals.outType == TY_DUMMY)
	{
		return;
	}
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG)
			fprint_int32(blk, blk->num_output_file, x);
		else 
		{
			if (blk->num_output_idx == blk->num_output_entries)
			{
				fwrite(blk->num_output_buffer, blk->num_output_entries, sizeof(int16), blk->num_output_file);
				blk->num_output_idx = 0;
			}
			blk->num_output_buffer[blk->num_output_idx++] = (int16)x;
		}
	}
}


void buf_putint32(BufContextBlock *blk, int32 x)
{
	write_time_stamp();
	_buf_putint32(blk, x);
}


FINL
void _buf_putarrint32(BufContextBlock *blk, int32 *x, unsigned int vlen)
{
	if (Globals.outType == TY_DUMMY) return;

	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG) 
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
				fwrite(blk->num_output_buffer, blk->num_output_entries, sizeof(int16), blk->num_output_file);

				// then write the rest
				for (blk->num_output_idx = 0; blk->num_output_idx < vlen - m; blk->num_output_idx++)
					blk->num_output_buffer[blk->num_output_idx] = x[blk->num_output_idx + m];
			}
		}
	}
}

void buf_putarrint32(BufContextBlock *blk, int32 *x, unsigned int vlen)
{
	write_time_stamp();
	_buf_putarrint32(blk, x, vlen);
}


void flush_putint32(BufContextBlock *blk)
{
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_BIN) {
			fwrite(blk->num_output_buffer, sizeof(int16), blk->num_output_idx, blk->num_output_file);
			blk->num_output_idx = 0;
		}
		fclose(blk->num_output_file);
	}
}


void init_putcomplex32(BufContextBlock *blk, HeapContextBlock *hblk)
{
	if (Globals.outType == TY_DUMMY || Globals.outType == TY_FILE)
	{
		blk->num_output_buffer = (int16 *)malloc(2 * Globals.outBufSize * sizeof(int16));
		blk->num_output_entries = Globals.outBufSize * 2;
		if (Globals.outType == TY_FILE)
			blk->num_output_file = try_open(Globals.outFileName, "w");
	}

	if (Globals.outType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora TX does not support Complex32\n");
		exit(1);
	}
}
void buf_putcomplex32(BufContextBlock *blk, struct complex32 x)
{
	write_time_stamp();
	_buf_putint32(blk, x.re);
	_buf_putint32(blk, x.im);
}
void buf_putarrcomplex32(BufContextBlock *blk, struct complex32 *x, unsigned int vlen)
{
	write_time_stamp();
	_buf_putarrint32(blk, (int32 *)x, vlen * 2);
}
void flush_putcomplex32(BufContextBlock *blk)
{
	flush_putint32(blk);
}
