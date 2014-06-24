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



void initBufCtxBlock(BufContextBlock *blk)
{
	blk->input_idx = 0;
	blk->input_repetitions = 1;
	blk->input_dummy_samples = 0;
	blk->fst = 1;
	blk->output_idx = 0;
	blk->chunk_input_idx = 0;
	blk->chunk_input_repeats = 1;
	blk->chunk_input_dummy_samples = 0;
	blk->chunk_fst = 1;
	blk->chunk_output_idx = 0;
	blk->num8_input_idx = 0;
	blk->num8_input_repeats = 1;
	blk->num8_input_dummy_samples = 0;
	blk->num8_fst = 1;
	blk->num8_output_idx = 0;
	blk->num16_input_idx = 0;
	blk->num16_input_repeats = 1;
	blk->num16_input_dummy_samples = 0;
	blk->num16_fst = 1;
	blk->num16_output_idx = 0;
	blk->num_input_idx = 0;
	blk->num_input_repeats = 1;
	blk->num_input_dummy_samples = 0;
	blk->num_fst = 1;
	blk->num_output_idx = 0;
}


unsigned int parse_dbg_byte(BufContextBlock *blk, char *dbg_buf, char *target)
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

  //  copy a chunk onto target 
  memcpy((void*)target,&val,blk->chunk_input_siz * sizeof(char));
  target += blk->chunk_input_siz;
  i++;

  while (s = strtok(NULL, ",")) 
  {
	  val = strtol(s,NULL,10);
	  if (errno == EINVAL) 
      {
		  fprintf(stderr,"Parse error when loading debug file.");
		  exit(1);
      }
	  memcpy(target, &val, blk->chunk_input_siz);
	  target += blk->chunk_input_siz;
      i++;
  }
  return i; // total number of entries
}



void init_getchunk(BufContextBlock *blk, HeapContextBlock *hblk, unsigned int sz)
{
	blk->chunk_input_siz = sz;

	if (Globals.inType == TY_DUMMY)
	{
		blk->chunk_max_dummy_samples = Globals.dummySamples;
	}

	if (Globals.inType == TY_FILE)
	{
		unsigned int sz;
		char *filebuffer;
		try_read_filebuffer(hblk, Globals.inFileName, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof chunk should be more than enough
		blk->chunk_input_buffer = try_alloc_bytes(hblk, sz * blk->chunk_input_siz);

		if (Globals.inFileMode == MODE_BIN)
		{
			memcpy(blk->chunk_input_buffer, (void *)filebuffer, sz);
			blk->chunk_input_entries = sz / blk->chunk_input_siz;
		}
		else
		{
			blk->chunk_input_entries = parse_dbg_byte(blk, filebuffer, blk->chunk_input_buffer);
		}
	}
}


// Get into a buffer that has at least size chunk_sz

GetStatus buf_getchunk(BufContextBlock *blk, char *x)
{

	if (Globals.inType == TY_DUMMY)
	{
		if (blk->chunk_input_dummy_samples >= blk->chunk_max_dummy_samples && Globals.inFileRepeats != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->chunk_input_dummy_samples++;
		*x = 0;
		return GS_SUCCESS;
	}

	// If we reached the end of the input buffer 
	if (blk->chunk_input_idx >= blk->chunk_input_entries)
	{
		// If no more repetitions are allowed 
		if (Globals.inFileRepeats != INF_REPEAT && blk->chunk_input_repeats >= Globals.inFileRepeats)
		{
			return GS_EOF;
		}
		// Otherwise we set the index to 0 and increase repetition count
		blk->chunk_input_idx = 0;
		blk->chunk_input_repeats++;
	}

	memcpy(x, blk->chunk_input_buffer + blk->chunk_input_idx*blk->chunk_input_siz, blk->chunk_input_siz);
	blk->chunk_input_idx++;

	return GS_SUCCESS;
}

// Get an array of elements, each of size chunk_input_siz
GetStatus buf_getarrchunk(BufContextBlock *blk, char *x, unsigned int vlen)
{

	if (Globals.inType == TY_DUMMY)
	{
		if (blk->chunk_input_dummy_samples >= blk->chunk_max_dummy_samples && Globals.inFileRepeats != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->chunk_input_dummy_samples += vlen;
		memset(x, 0, vlen*blk->chunk_input_siz);
		return GS_SUCCESS;
	}

	if (blk->chunk_input_idx + vlen > blk->chunk_input_entries)
	{
		if (Globals.inFileRepeats != INF_REPEAT && blk->chunk_input_repeats >= Globals.inFileRepeats)
		{
			if (blk->chunk_input_idx != blk->chunk_input_entries)
				fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
			return GS_EOF;
		}
		// Otherwise ignore trailing part of the file, not clear what that part may contain ...
		blk->chunk_input_idx = 0;
		blk->chunk_input_repeats++;
	}
	
	memcpy(x, blk->chunk_input_buffer + blk->chunk_input_idx*blk->chunk_input_siz, vlen * blk->chunk_input_siz);
	blk->chunk_input_idx += vlen;
	return GS_SUCCESS;
}



void fprint_char(BufContextBlock *blk, FILE *f, char val)
{
	// FIX
	//static int isfst = 1;
	if (blk->chunk_fst)
	{
		fprintf(f, "%d", val);
		blk->chunk_fst = 0;
	}
	else fprintf(f, ",%d", val);
}
void fprint_arrchar(BufContextBlock *blk, FILE *f, char *val, unsigned int vlen)
{
	unsigned int i;
	for (i = 0; i < vlen; i++)
	{
		fprint_char(blk, f, val[i]);
	}
}



void init_putchunk(BufContextBlock *blk, unsigned int sz)
{

	blk->chunk_output_siz = sz;
	blk->chunk_output_buffer = (char*)malloc(Globals.outBufSize * blk->chunk_output_siz);
	blk->chunk_output_entries = Globals.outBufSize;

	if (Globals.outType == TY_FILE)
		blk->chunk_output_file = try_open(Globals.outFileName, "w");

}

void buf_putchunk(BufContextBlock *blk, void *x, void(*fprint)(FILE *f, void *val))
{
	if (Globals.outType == TY_DUMMY)
	{
		return;
	}
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG)
			fprint(blk->chunk_output_file, x);
		else 
		{
			if (blk->chunk_output_idx == blk->chunk_output_entries)
			{
				fwrite(blk->chunk_output_buffer, blk->chunk_output_entries, blk->chunk_output_siz, blk->chunk_output_file);
				blk->chunk_output_idx = 0;
			}
			memcpy(blk->chunk_output_buffer + blk->chunk_output_idx*blk->chunk_output_siz, x, blk->chunk_output_siz);
			blk->chunk_output_idx++;
		}
	}
}

void buf_putarrchunk(BufContextBlock *blk, char *x, unsigned int vlen)
{
	if (Globals.outType == TY_DUMMY) return;

	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG) 
			fprint_arrchar(blk, blk->chunk_output_file, x, vlen);
		else
		{
			if (blk->chunk_output_idx + vlen >= blk->chunk_output_entries)
			{
				// first write the first (num_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = blk->chunk_output_entries - blk->chunk_output_idx;

				for (i = 0; i < m; i++)
					blk->chunk_output_buffer[blk->chunk_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(blk->chunk_output_buffer, blk->chunk_output_entries, sizeof(int16), blk->chunk_output_file);

				// then write the rest
				for (blk->chunk_output_idx = 0; blk->chunk_output_idx < vlen - m; blk->chunk_output_idx++)
					blk->chunk_output_buffer[blk->chunk_output_idx] = x[blk->chunk_output_idx + m];
			}
		}
	}
}

void flush_putchar(BufContextBlock *blk)
{
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_BIN) {
			fwrite(blk->chunk_output_buffer, sizeof(char), blk->chunk_output_idx, blk->chunk_output_file);
			blk->chunk_output_idx = 0;
		}
		fclose(blk->chunk_output_file);
	}
}




