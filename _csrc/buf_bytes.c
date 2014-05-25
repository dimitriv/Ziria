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

/* A buffer of elements, each element of size chunk_siz */
static void *chunk_input_buffer;
static unsigned int chunk_input_siz;
static unsigned int chunk_input_entries;
static unsigned int chunk_input_idx     = 0;
static unsigned int chunk_input_repeats = 1;

static unsigned int chunk_input_dummy_samples = 0;
static unsigned int chunk_max_dummy_samples; 


static void (* parse_chunk_dbg)(char *buf, void *chunk);
static void (* print_chunk_dbg)(FILE *f, void *chunk);

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
	   	  memcpy(chunk_input_buffer, (void *) filebuffer, sz);
		  chunk_input_entries = sz / chunk_input_siz;
                }
 		else 
		{
		  num_input_entries = parse_dbg_int32(filebuffer, num_input_buffer);
		}
	}
}



unsigned int parse_dbg_chunk(char *dbg_buf, void *target)
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
  memcpy(target,&val,chunk_input_siz);
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


// Get into a buffer that has at least size chunk_sz

GetStatus buf_getchunk(void *x) 
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
		if (Globals.inFileRepeats != INF_REPEAT && num_input_repeats >= Globals.inFileRepeats)
		{
			return GS_EOF;
		}
		// Otherwise we set the index to 0 and increase repetition count
		chunk_input_idx = 0;
		chunk_input_repeats++;
	}

        memcpy(x, chunk_input_buffer + chunk_input_idx*chunk_input_siz, chunk_sz);
        num_input_idx++;

	return GS_SUCCESS;
}

// Get an array of elements, each of size chunk_input_siz
GetStatus buf_getarrchunk(void *x, unsigned int vlen)
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
		if (Globals.inFileRepeats != INF_REPEAT && num_input_repeats >= Globals.inFileRepeats)
		{
		  if (chunk_input_idx != num_input_entries)
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



static void *chunk_output_buffer;
static unsigned int chunk_output_siz;
static unsigned int chunk_output_entries;
static unsigned int chunk_output_idx = 0;
static FILE *chunk_output_file;

void init_putchunk(unsigned int sz)
{

  chunk_output_siz = sz;
  chunk_output_buffer = malloc(Globals.outBufSize * chunk_output_siz);
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
			if (num_output_idx == num_output_entries)
			{
				fwrite(chunk_output_buffer,chunk_output_entries, chunk_siz,chunk_output_file);
				chunk_output_idx = 0;
			}
                        memcpy(chunk_output_buffer+num_output_idx*chunk_siz,x,chunk_siz);
                        num_output_id++;
		}
	}
}

void buf_putarrchunk(int32 *x, unsigned int vlen)
{
	if (Globals.outType == TY_DUMMY) return;

	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG) 
			fprint_arrint32(num_output_file,x,vlen);
		else
		{
			if (num_output_idx + vlen >= num_output_entries)
			{
				// first write the first (num_output_entries - vlen) entries
				unsigned int i;
				unsigned int m = num_output_entries - num_output_idx;

				for (i = 0; i < m; i++)
					num_output_buffer[num_output_idx + i] = x[i];

				// then flush the buffer
				fwrite(num_output_buffer,num_output_entries,sizeof(int16),num_output_file);

				// then write the rest
				for (num_output_idx = 0; num_output_idx < vlen - m; num_output_idx++)
					num_output_buffer[num_output_idx] = x[num_output_idx + m];
			}
		}
	}
}

void flush_putint32()
{
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_BIN) {
			fwrite(num_output_buffer,sizeof(int16), num_output_idx,num_output_file);
			num_output_idx = 0;
		}
		fclose(num_output_file);
	}
}


void init_putchunk()
{
	num_output_buffer = (int16 *) malloc(Globals.outBufSize * sizeof(int16));
	num_output_entries = Globals.outBufSize;
	if (Globals.outType == TY_FILE)
		num_output_file = try_open(Globals.outFileName,"w");
}




void init_putcomplex32() 
{
	num_output_buffer = (int16 *) malloc(2*Globals.outBufSize * sizeof(int16));
	num_output_entries = Globals.outBufSize*2;
	if (Globals.outType == TY_FILE)
		num_output_file = try_open(Globals.outFileName,"w");

}
void buf_putcomplex32(struct complex32 x)
{
	buf_putint32(x.re);
	buf_putint32(x.im);
}
void buf_putarrcomplex32(struct complex32 *x, unsigned int vlen)
{
	buf_putarrint32((int32 *)x,vlen*2);
}
void flush_putcomplex32()
{
	flush_putint32();
}
