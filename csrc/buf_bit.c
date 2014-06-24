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


void fprint_bit(BufContextBlock* blk, FILE *f, Bit val)
{
	if (blk->fst) {
		fprintf(f,"%d",val);
		blk->fst = 0;
	}
	else
		fprintf(f,",%d",val);
}

void fprint_arrbit(BufContextBlock* blk, FILE *f, BitArrPtr src, unsigned int vlen)
{
	Bit x = 0;
	unsigned int i;
	
	for (i=0; i < vlen; i++) 
	{
		bitRead(src,i,&x);
		fprint_bit(blk, f,x);
	}
}

void parse_bit(char *token, Bit *val) 
{
  long x = strtol(token,NULL,10);
  if (errno == EINVAL) 
  {
      fprintf(stderr,"Parse error when loading debug file.");
      exit(1);
  }
  if (x <= 1) 
	  *val = ((Bit) x) & 1; 
  else {
      fprintf(stderr,"Debug file does not contain binary data (%s)\n", token);
      exit(1);
  }
}
unsigned int parse_dbg_bit(char *dbg_buf, BitArrPtr target)
{
	Bit val;
	long x;
	unsigned int c = 0;
	char *s = NULL;
	s = strtok(dbg_buf, ",");
	
 	if (s == NULL) 
	{ 
		fprintf(stderr,"Input (debug) file contains no samples.");
		exit(1);
	}

	//parse_bit(s,&val);
    x = strtol(s,NULL,10);
	if (errno == EINVAL) 
	{
      fprintf(stderr,"Parse error when loading debug file.");
      exit(1);
	}
	if (x <= 1) 
	  val = ((Bit) x) & 1; 
	else 
	{
		fprintf(stderr, "Debug file does not contain binary data (%s)\n", s);
		exit(1);
	}

	bitWrite(target, c++, val);
  
	while (s = strtok(NULL, ",")) 
	{
		x = strtol(s,NULL,10);

		if (errno == EINVAL) 
		{
			fprintf(stderr,"Parse error when loading debug file.");
			exit(1);
		}
		if (x <= 1)
		{
			val = ((Bit)x) & 1;
		}
		else 
		{
			fprintf(stderr, "Debug file does not contain binary data (%s)\n", s);
			exit(1);
		}

		bitWrite(target, c++, val);
	}

	return c;
}

void init_getbit(BufContextBlock* blk, HeapContextBlock *hblk)
{
	if (Globals.inType == TY_DUMMY)
	{
		blk->max_dummy_samples = Globals.dummySamples;
	}

	if (Globals.inType == TY_FILE)
	{
		unsigned int sz; 
		char *filebuffer;
		try_read_filebuffer(hblk, Globals.inFileName, &filebuffer, &sz);

		if (Globals.inFileMode == MODE_BIN)
		{ 
			blk->input_buffer = (BitArrPtr)filebuffer;
			blk->input_entries = 8 * sz;
		}
		else 
		{
			blk->input_buffer = (BitArrPtr)try_alloc_bytes(hblk, sz);
			blk->input_entries = parse_dbg_bit(filebuffer, blk->input_buffer);
		}
	}

	if (Globals.inType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora does not support bit receive\n");
		exit(1);
	}


	if (Globals.inType == TY_IP)
	{
#ifdef SORA_PLATFORM
	  // Receiving from IP
	  Ndis_init(NULL);
#endif
	}

}

GetStatus buf_getbit(BufContextBlock* blk, Bit *x)
{
	if (Globals.inType == TY_IP)
	{
		fprintf(stderr, "Error: IP does not support single bit receive\n");
		exit(1);
	}

	if (Globals.inType == TY_DUMMY)
	{
		if (blk->input_dummy_samples >= blk->max_dummy_samples && Globals.dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->input_dummy_samples++;
		// No real need to do this, and it slows down substantially
		//*x = 0;
		return GS_SUCCESS;
	}

	// If we reached the end of the input buffer 
	if (blk->input_idx >= blk->input_entries)
	{
		// If no more repetitions are allowed 
		if (Globals.inFileRepeats != INF_REPEAT && blk->input_repetitions >= Globals.inFileRepeats)
		{
			return GS_EOF;
		}
		// Otherwise we set the index to 0 and increase repetition count
		blk->input_idx = 0;
		blk->input_repetitions++;
	}

	bitRead(blk->input_buffer, blk->input_idx++, x);

	return GS_SUCCESS;
}
GetStatus buf_getarrbit(BufContextBlock* blk, BitArrPtr x, unsigned int vlen)
{
	if (Globals.inType == TY_IP)
	{
#ifdef SORA_PLATFORM
	  UINT len = ReadFragment(x, RADIO_MTU);
	  return GS_SUCCESS;
#endif
	}

	if (Globals.inType == TY_DUMMY)
	{
		if (blk->input_dummy_samples >= blk->max_dummy_samples && Globals.dummySamples != INF_REPEAT)
		{
			return GS_EOF;
		}
		blk->input_dummy_samples += vlen;
		// No real need to do this, and it slows down substantially
		//memset(x,0,(vlen+7)/8);
		return GS_SUCCESS;
	}

	if (blk->input_idx + vlen > blk->input_entries)
	{
		if (Globals.inFileRepeats != INF_REPEAT && blk->input_repetitions >= Globals.inFileRepeats)
		{
			if (blk->input_idx != blk->input_entries)
				fprintf(stderr, "Warning: Unaligned data in input file, ignoring final get()!\n");
			return GS_EOF;
		}
		// Otherwise ignore trailing part of the file, not clear what that part may contain ...
		blk->input_idx = 0;
		blk->input_repetitions++;
	}
	
	bitArrRead(blk->input_buffer, blk->input_idx, vlen, x);

	blk->input_idx += vlen;
	return GS_SUCCESS;
}


void init_putbit(BufContextBlock* blk, HeapContextBlock *hblk)
{
	if (Globals.outType == TY_DUMMY || Globals.outType == TY_FILE)
	{
		blk->output_buffer = (unsigned char *)malloc(Globals.outBufSize);
		blk->output_entries = Globals.outBufSize * 8;
		if (Globals.outType == TY_FILE)
			blk->output_file = try_open(Globals.outFileName, "w");
	}

	if (Globals.outType == TY_SORA)
	{
		fprintf(stderr, "Error: Sora TX does not support bits\n");
		exit(1);
	}

	if (Globals.outType == TY_IP)
	{
#ifdef SORA_PLATFORM
	  // Sending to IP
	  Ndis_init(NULL);
#endif
	}

}
void buf_putbit(BufContextBlock* blk, Bit x)
{
	write_time_stamp();

	if (Globals.outType == TY_IP)
	{
		fprintf(stderr, "Error: IP does not support single bit transmit\n");
		exit(1);
	}

	if (Globals.outType == TY_DUMMY)
	{
		return;
	}
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG)
			fprint_bit(blk, blk->output_file, x);
		else 
		{
			if (blk->output_idx == blk->output_entries)
			{
				fwrite(blk->output_buffer, blk->output_entries / 8, 1, blk->output_file);
				blk->output_idx = 0;
			}
			bitWrite(blk->output_buffer, blk->output_idx++, x);
		}
	}
}
void buf_putarrbit(BufContextBlock* blk, BitArrPtr x, unsigned int vlen)
{
	write_time_stamp();

	if (Globals.outType == TY_IP)
	{
#ifdef SORA_PLATFORM
	   int n = WriteFragment(x);
	   return;
#endif
	}

	if (Globals.outType == TY_DUMMY) return;

	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_DBG) 
			fprint_arrbit(blk, blk->output_file, x, vlen);
		else
		{ 
			if (blk->output_idx + vlen >= blk->output_entries)
			{
				// first write the first (output_entries - output_idx) entries
				unsigned int m = blk->output_entries - blk->output_idx;

				bitArrWrite(x, blk->output_idx, m, blk->output_buffer);

				// then flush the buffer
				fwrite(blk->output_buffer, blk->output_entries / 8, 1, blk->output_file);

				// then write the rest
				bitArrRead(x, m, vlen - m, blk->output_buffer);
				blk->output_idx = vlen - m;
			}
		}
	}
}
void flush_putbit(BufContextBlock* blk)
{
	if (Globals.outType == TY_FILE)
	{
		if (Globals.outFileMode == MODE_BIN) {
			fwrite(blk->output_buffer, 1, (blk->output_idx + 7) / 8, blk->output_file);
			blk->output_idx = 0;
		}
		fclose(blk->output_file);
	}
} 




