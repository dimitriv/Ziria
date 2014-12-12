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

#ifdef WINDDK
// These need to be included in WinDDK environment but not in VS
// Not sure why...
#include <winsock2.h> // ws2_32.lib required
#include <ws2tcpip.h>
#endif 

#include "wpl_alloc.h"
#include "params.h"
#include "types.h"
#include "buf.h"

#ifdef SORA_PLATFORM
#include "sora_ip.h"
#endif







void init_getchunk(BlinkParams *params, BufContextBlock* blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_in = unit_size*8;
	blk->total_in = 0;
	blk->chunk_size = unit_size;

	if (params->inType == TY_DUMMY)
	{
		blk->chunk_max_dummy_samples = params->dummySamples;
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
			blk->chunk_input_buffer = blk->mem_input_buf;
			blk->chunk_input_entries = blk->mem_input_buf_size / unit_size;
		}
	}

	if (params->inType == TY_FILE)
	{
		memsize_int sz; 
		char *filebuffer;
		try_read_filebuffer(hblk, params->inFileName, params->inFileMode, &filebuffer, &sz);

		if (params->inFileMode == MODE_BIN)
		{ 
			blk->chunk_input_buffer = (void *)filebuffer;
			blk->chunk_input_entries = sz / unit_size;
		}
		else 
		{
			fprintf(stderr, "Error: chunk receive is supported only in binary\n");
			exit(1);
		}
	}

	if (params->inType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora does not support chunk receive\n");
		exit(1);
	}


	if (params->inType == TY_IP)
	{
#ifdef SORA_PLATFORM
	  // Receiving from IP
	  //Ndis_init(NULL);
#endif
	}

}

GetStatus buf_getchunk(BlinkParams *params, BufContextBlock* blk, void *x)
{
#ifdef STAMP_AT_READ
	write_time_stamp(params);
#endif
	blk->total_in++;

	if (params->inType == TY_IP)
	{
		fprintf(stderr, "Error: IP does not support chunk receive\n");
		exit(1);
	}

	if (params->inType == TY_DUMMY)
	{
		if (blk->chunk_input_dummy_samples >= blk->chunk_max_dummy_samples && params->dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->chunk_input_dummy_samples++;
		// No real need to do this, and it slows down substantially
		//*x = 0;
		return GS_SUCCESS;
	}

	// If we reached the end of the input buffer 
	if (blk->chunk_input_idx >= blk->chunk_input_entries)
	{
		// If no more repetitions are allowed 
		if (params->inFileRepeats != INF_REPEAT && blk->chunk_input_repetitions >= params->inFileRepeats)
		{
			return GS_EOF;
		}
		// Otherwise we set the index to 0 and increase repetition count
		blk->chunk_input_idx = 0;
		blk->chunk_input_repetitions++;
	}

	memcpy(x, (void*)((char *)blk->chunk_input_buffer + blk->chunk_input_idx * blk->chunk_size), blk->chunk_size);
	blk->chunk_input_idx++;

	return GS_SUCCESS;
}

FORCE_INLINE
GetStatus buf_getarrchunk(BlinkParams *params, BufContextBlock* blk, void *x, unsigned int vlen)
{
#ifdef STAMP_AT_READ
	write_time_stamp(params);
#endif
	blk->total_in += vlen;

	if (params->inType == TY_IP)
	{
#ifdef SORA_PLATFORM
//	  UINT len = ReadFragment(x, RADIO_MTU);
	  return GS_SUCCESS;
#endif
	}

	if (params->inType == TY_DUMMY)
	{
		if (blk->chunk_input_dummy_samples >= blk->chunk_max_dummy_samples && params->dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->chunk_input_dummy_samples += vlen;
		// No real need to do this, and it slows down substantially
		//memset(x,0,(vlen+7)/8);
		return GS_SUCCESS;
	}

	if (blk->chunk_input_idx + vlen > blk->chunk_input_entries)
	{
		if (params->inFileRepeats != INF_REPEAT && blk->chunk_input_repetitions >= params->inFileRepeats)
		{
			if (blk->chunk_input_idx != blk->chunk_input_entries)
				fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
			return GS_EOF;
		}
		// Otherwise ignore trailing part of the file, not clear what that part may contain ...
		blk->chunk_input_idx = 0;
		blk->chunk_input_repetitions++;
	}
	
	memcpy(x, (void*)((char *)blk->chunk_input_buffer + blk->chunk_input_idx * blk->chunk_size), vlen * blk->chunk_size);
	blk->chunk_input_idx += vlen;

	return GS_SUCCESS;
}


void init_putchunk(BlinkParams *params, BufContextBlock* blk, HeapContextBlock *hblk, size_t unit_size)
{
	blk->size_out = 8*unit_size;
	blk->total_out = 0;
	blk->chunk_size = unit_size;

	if (params->outType == TY_DUMMY || params->outType == TY_FILE)
	{
		blk->chunk_output_buffer = (unsigned char *)malloc(params->outBufSize);
		blk->chunk_output_entries = params->outBufSize / unit_size;
		if (params->outType == TY_FILE)
		{
			if (params->outFileMode == MODE_BIN)
			{
				blk->chunk_output_file = try_open(params->outFileName, "wb");
			}
			else
			{
				fprintf(stderr, "Error: chunk I/O does not support DBG mode!\n");
				exit(1);
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
			blk->chunk_output_buffer = blk->mem_output_buf;
			blk->chunk_output_entries = blk->mem_output_buf_size / unit_size;
		}
	}

	if (params->outType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora TX does not support bits\n");
		exit(1);
	}

	if (params->outType == TY_IP)
	{
#ifdef SORA_PLATFORM
	  // Sending to IP
	  //Ndis_init(NULL);
#endif
	}

}
void buf_putchunk(BlinkParams *params, BufContextBlock* blk, void *x)
{
#ifndef STAMP_AT_READ
	write_time_stamp(params);
#endif
	blk->total_out++;

	if (params->outType == TY_IP)
	{
		fprintf(stderr, "Error: IP does not support single bit transmit\n");
		exit(1);
	}

	if (params->outType == TY_DUMMY)
	{
		return;
	}

	if (params->outType == TY_MEM)
	{
		//bitWrite(blk->output_buffer, blk->output_idx++, x);
		memcpy((void*)((char *)blk->chunk_output_buffer + blk->chunk_output_idx * blk->chunk_size), x, blk->chunk_size);
		blk->chunk_output_idx ++;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
		{
			fprintf(stderr, "Error: we do not support DBG write mode with chunks\n");
			exit(1);
		}
		else 
		{
			if (blk->chunk_output_idx == blk->chunk_output_entries)
			{
				fwrite(blk->chunk_output_buffer, blk->chunk_output_entries * blk->chunk_size, 1, blk->chunk_output_file);
				blk->chunk_output_idx = 0;
			}
			memcpy((void*)((char *)blk->chunk_output_buffer + blk->chunk_output_idx * blk->chunk_size), x, blk->chunk_size);
			blk->chunk_output_idx++;
		}
	}
}

FORCE_INLINE
void buf_putarrchunk(BlinkParams *params, BufContextBlock* blk, void *x, unsigned int vlen)
{
	blk->total_out+= vlen;
#ifndef STAMP_AT_READ
	write_time_stamp(params);
#endif

	if (params->outType == TY_IP)
	{
#ifdef SORA_PLATFORM
//	   int n = WriteFragment(x);
	   return;
#endif
	}

	if (params->outType == TY_DUMMY) return;

	if (params->outType == TY_MEM)
	{
		memcpy((void*)((char *)blk->chunk_output_buffer + blk->chunk_output_idx * blk->chunk_size), x, blk->chunk_size * vlen);
		blk->chunk_output_idx += vlen;
	}

	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_DBG)
		{
			fprintf(stderr, "Error: we do not support DBG write mode with chunks\n");
			exit(1);
		}
		else
		{ 
			if (blk->chunk_output_idx + vlen >= blk->chunk_output_entries)
			{
				// first write the first (output_entries - output_idx) entries
				unsigned int m = blk->chunk_output_entries - blk->chunk_output_idx;

				//bitArrWrite(x, blk->output_idx, m, blk->output_buffer);
				memcpy((void*)((char *)blk->chunk_output_buffer + m * blk->chunk_size), x, blk->chunk_size * vlen);

				// then flush the buffer
				fwrite(blk->chunk_output_buffer, blk->chunk_output_entries * blk->chunk_size, 1, blk->chunk_output_file);

				blk->chunk_output_idx = vlen - m;
			}
			else
			{
				memcpy((void*)((char *)blk->chunk_output_buffer + blk->chunk_output_idx * blk->chunk_size), x, blk->chunk_size * vlen);
				blk->chunk_output_idx += vlen;
			}
		}
	}
}

void reset_putchunk(BlinkParams *params, BufContextBlock* blk)
{
	if (params->outType == TY_FILE)
	{
		if (params->outFileMode == MODE_BIN) {
			fwrite(blk->chunk_output_buffer, blk->chunk_output_idx * blk->chunk_size, 1, blk->chunk_output_file);
		}
	}
	blk->chunk_output_idx = 0;
}

void flush_putchunk(BlinkParams *params, BufContextBlock* blk)
{
	reset_putchunk(params, blk);
	if (params->outType == TY_FILE)
	{
		fclose(blk->chunk_output_file);
	}
} 




