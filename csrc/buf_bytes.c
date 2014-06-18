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

#include "params.h"
#include "types.h"
#include "buf.h"
#include "wpl_alloc.h"

/* A buffer of elements, each element of size chunk_siz */
static char *chunk_input_buffer;
static unsigned int chunk_input_siz;
static unsigned int chunk_input_entries;
static unsigned int chunk_input_idx     = 0;
static unsigned int chunk_input_repeats = 1;

static unsigned int chunk_input_dummy_samples = 0;
static unsigned int chunk_max_dummy_samples; 


static void (* parse_chunk_dbg)(char *buf, void *chunk);
static void (* print_chunk_dbg)(FILE *f, void *chunk);




unsigned int parse_dbg_byte(char *dbg_buf, char *target)
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
  memcpy((void*)target,&val,chunk_input_siz * sizeof(char));
  target+= chunk_input_siz; 
  i++;

  while (s = strtok(NULL, ",")) 
  {
	  val = strtol(s,NULL,10);
	  if (errno == EINVAL) 
      {
		  fprintf(stderr,"Parse error when loading debug file.");
		  exit(1);
      }
          memcpy(target,&val,chunk_input_siz);
          target+= chunk_input_siz; 
          i++;
  }
  return i; // total number of entries
}



void init_getchunk(unsigned int sz)
{
	chunk_input_siz = sz;

	if (Globals.inType == TY_DUMMY)
	{
		chunk_max_dummy_samples = Globals.dummySamples;
	}

	if (Globals.inType == TY_FILE)
	{
		unsigned int sz;
		char *filebuffer;
		try_read_filebuffer(Globals.inFileName, &filebuffer, &sz);

		// How many bytes the file buffer has * sizeof chunk should be more than enough
		chunk_input_buffer = try_alloc_bytes(sz * chunk_input_siz);

		if (Globals.inFileMode == MODE_BIN)
		{
			memcpy(chunk_input_buffer, (void *)filebuffer, sz);
			chunk_input_entries = sz / chunk_input_siz;
		}
		else
		{
			chunk_input_entries = parse_dbg_byte(filebuffer, chunk_input_buffer);
		}
	}
}


// Get into a buffer that has at least size chunk_sz

GetStatus buf_getchunk(char *x) 
{

	if (Globals.inType == TY_DUMMY)
	{
		if (chunk_input_dummy_samples >= chunk_max_dummy_samples && Globals.inFileRepeats != INF_REPEAT) return GS_EOF;
		chunk_input_dummy_samples++;
		*x = 0;
		return GS_SUCCESS;
	}

	// If we reached the end of the input buffer 
        if (chunk_input_idx >= chunk_input_entries)
	{
		// If no more repetitions are allowed 
		if (Globals.inFileRepeats != INF_REPEAT && chunk_input_repeats >= Globals.inFileRepeats)
		{
			return GS_EOF;
		}
		// Otherwise we set the index to 0 and increase repetition count
		chunk_input_idx = 0;
		chunk_input_repeats++;
	}

        memcpy(x, chunk_input_buffer + chunk_input_idx*chunk_input_siz, chunk_input_siz);
		chunk_input_idx++;

	return GS_SUCCESS;
}

// Get an array of elements, each of size chunk_input_siz
GetStatus buf_getarrchunk(char *x, unsigned int vlen)
{

	if (Globals.inType == TY_DUMMY)
	{
		if (chunk_input_dummy_samples >= chunk_max_dummy_samples && Globals.inFileRepeats != INF_REPEAT) return GS_EOF;
		chunk_input_dummy_samples += vlen;
		memset(x,0,vlen*chunk_input_siz);
		return GS_SUCCESS;
	}

	if (chunk_input_idx + vlen > chunk_input_entries)
	{
		if (Globals.inFileRepeats != INF_REPEAT && chunk_input_repeats >= Globals.inFileRepeats)
		{
			if (chunk_input_idx != chunk_input_entries)
				fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
			return GS_EOF;
		}
		// Otherwise ignore trailing part of the file, not clear what that part may contain ...
		chunk_input_idx = 0;
		chunk_input_repeats++;
	}
	
	memcpy(x,chunk_input_buffer + chunk_input_idx*chunk_input_siz, vlen * chunk_input_siz);
        chunk_input_idx += vlen;
	return GS_SUCCESS;
}



void fprint_char(FILE *f, char val)
{
	static int isfst = 1;
	if (isfst)
	{
		fprintf(f, "%d", val);
		isfst = 0;
	}
	else fprintf(f, ",%d", val);
}
void fprint_arrchar(FILE *f, char *val, unsigned int vlen)
{
	unsigned int i;
	for (i = 0; i < vlen; i++)
	{
		fprint_char(f, val[i]);
	}
}


static char *chunk_output_buffer;
static unsigned int chunk_output_siz;
static unsigned int chunk_output_entries;
static unsigned int chunk_output_idx = 0;
static FILE *chunk_output_file;

void init_putchunk(unsigned int sz)
{

  chunk_output_siz = sz;
  chunk_output_buffer = (char*) malloc(Globals.outBufSize * chunk_output_siz);
  chunk_output_entries = Globals.outBufSize;

  if (Globals.outType == TY_FILE)
    chunk_output_file = try_open(Globals.outFileName,"w");

}

void buf_putchunk(void *x, void (*fprint)(FILE *f, void *val))
{
	if (Globals.outType == TY_DUMMY)
	{
		return;
	}
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG)
			fprint(chunk_output_file,x);
		else 
		{
			if (chunk_output_idx == chunk_output_entries)
			{
				fwrite(chunk_output_buffer,chunk_output_entries, chunk_output_siz,chunk_output_file);
				chunk_output_idx = 0;
			}
            memcpy(chunk_output_buffer+chunk_output_idx*chunk_output_siz,x,chunk_output_siz);
            chunk_output_idx++;
		}
	}
}

void buf_putarrchunk(char *x, unsigned int vlen)
{
	if (Globals.outType == TY_DUMMY) return;

	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG) 
			fprint_arrchar(chunk_output_file,x,vlen);
		else
		{
			if (chunk_output_idx + vlen >= chunk_output_entries)
			{
				// first write the first (num_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = chunk_output_entries - chunk_output_idx;

				for (i = 0; i < m; i++)
					chunk_output_buffer[chunk_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(chunk_output_buffer,chunk_output_entries,sizeof(int16),chunk_output_file);

				// then write the rest
				for (chunk_output_idx = 0; chunk_output_idx < vlen - m; chunk_output_idx++)
					chunk_output_buffer[chunk_output_idx] = x[chunk_output_idx + m];
			}
		}
	}
}

void flush_putchar()
{
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_BIN) {
			fwrite(chunk_output_buffer,sizeof(char), chunk_output_idx,chunk_output_file);
			chunk_output_idx = 0;
		}
		fclose(chunk_output_file);
	}
}


void init_putchunk()
{
	chunk_output_buffer = (char *) malloc(Globals.outBufSize * sizeof(char));
	chunk_output_entries = Globals.outBufSize;
	if (Globals.outType == TY_FILE)
		chunk_output_file = try_open(Globals.outFileName,"w");
}


