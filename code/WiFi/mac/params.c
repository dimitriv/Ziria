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
#include <string.h>
#include <stdlib.h>

#include <wpl_alloc.h>
#include <params.h>

#include "mac.h"


FILE * try_open(char *filename, char *mode) 
{
  FILE *h;
  h = fopen(filename,mode);
  if (h == NULL) {
    fprintf (stderr, "Error: could not open file %s\n", filename);
    exit(1);
  }
  return h;
}

/* Read the file as a null-terminated string */ 
void try_read_filebuffer(HeapContextBlock *hblk, char *filename, char **fb, unsigned int *len)
{
	char *filebuffer;
	unsigned int sz;

	FILE *f = try_open(filename,"r");
	fseek(f, 0L, SEEK_END);
	sz = ftell(f);
	fseek(f, 0L, SEEK_SET);
	filebuffer = try_alloc_bytes(hblk, 2*(sz+1));
	fread(filebuffer, 1, sz, f);
	fclose(f);
	filebuffer[sz] = 0;
	*fb = filebuffer;
	*len = sz;

}

Repetitions parse_repeat(char *rp) 
{
	Repetitions reps;
	if (strcmp(rp,"*") == 0)
		return INF_REPEAT;
	reps = strtol(rp,NULL,10); // TODO: Error handling? 
	return reps;
}



// Initializers for default values
BlinkFileType parse_type(char *typ) 
{
	if (strcmp(typ,"file") == 0)	return TY_FILE; 
	if (strcmp(typ,"dummy") == 0)	return TY_DUMMY;
	if (strcmp(typ,"sora") == 0)	return TY_SORA;
	if (strcmp(typ,"ip") == 0)		return TY_IP;
	if (strcmp(typ, "memory") == 0) return TY_MEM;
	fprintf(stderr, "Error: cannot parse type parameter %s\n", typ);
	exit(1);
}


MACType parse_MAC(char *typ)
{
	if (strcmp(typ, "TX-test") == 0)	return MAC_TX_TEST;
	if (strcmp(typ, "RX-test") == 0)	return MAC_RX_TEST;
	if (strcmp(typ, "TX-only") == 0)	return MAC_TX_ONLY;
	if (strcmp(typ, "RX-only") == 0)	return MAC_RX_ONLY;
	fprintf(stderr, "Error: cannot parse MAC parameter %s\n", typ);
	exit(1);
}


void parse_txPC(char *typ)
{
	//strcpy(txPC, typ);
}

BlinkFileMode parse_mode(char *md) {
  if (strcmp(md,"dbg") == 0)
    return MODE_DBG; 
  if (strcmp(md,"bin") == 0) 
    return MODE_BIN;
  fprintf(stderr, "Error: cannot parse mode parameter %s\n",md);
  exit(1);
}



#ifdef SORA_PLATFORM
// TODO: Add error handling
ULONG parse_radioID(char *rp) 
{
	return (ULONG) strtol(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_Amp(char *rp) 
{
	return (ULONG) strtol(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_CentralFrequency(char *rp) 
{
	return (ULONG) strtol(rp,NULL,10);
}


// TODO: Add error handling
LONG parse_FrequencyOffset(char *rp) 
{
	return (LONG) strtol(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_SampleRate(char *rp) 
{
	return (ULONG) strtol(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_TXBufferSize(char *rp) 
{
	return (ULONG) strtol(rp,NULL,10);
}
#endif



unsigned long parse_size (char *rp) {
  return (strtol(rp,NULL,10));
}


// Global function
void init_inTypeMAC(BlinkParams *params, char *typ)				{ mac_type = parse_MAC(typ); }
void init_txPC(BlinkParams *params, char *typ)					{ parse_txPC(typ); }


// TX Init functions
void init_inTypeTX(BlinkParams *params, char *typ)				{ params[0].inType = parse_type(typ); }
void init_outTypeTX(BlinkParams *params, char *typ)				{ params[0].outType = parse_type(typ); }
void init_inFileTX(BlinkParams *params, char *fn)				{ params[0].inFileName = fn; }
void init_outFileTX(BlinkParams *params, char *fn)				{ params[0].outFileName = fn; }
void init_inFileModeTX(BlinkParams *params, char *md)			{ params[0].inFileMode = parse_mode(md); }
void init_outFileModeTX(BlinkParams *params, char *md)			{ params[0].outFileMode = parse_mode(md); }
void init_outBufTX(BlinkParams *params, char *siz)				{ params[0].outBufSize = parse_size(siz); }
void init_inMemorySizeTX(BlinkParams *params, char *size)		{ params[0].inMemorySize = parse_size(size); }
void init_outMemorySizeTX(BlinkParams *params, char *size)		{ params[0].outMemorySize = parse_size(size); }
void init_dummySamplesTX(BlinkParams *params, char *siz)		{ params[0].dummySamples = parse_repeat(siz); }
void init_heapSizeTX(BlinkParams *params, char *siz)			{ params[0].heapSize = parse_size(siz); }
void init_inRepeatTX(BlinkParams *params, char *i)				{ params[0].inFileRepeats = parse_repeat(i); }
void init_LatencySamplingTX(BlinkParams *params, char *siz)		{ params[0].latencySampling = parse_size(siz); }
void init_LatencyCDFSizeTX(BlinkParams *params, char *siz)		{ params[0].latencyCDFSize = parse_size(siz); }

#ifdef SORA_PLATFORM
void init_radioIdTX(BlinkParams *params, char *i)				{ params[0].radioParams.radioId = parse_radioID(i); }
void init_gainTX(BlinkParams *params, char *i)					{ params[0].radioParams.TXgain = parse_Amp(i); }
void init_centralFrequencyTX(BlinkParams *params, char *i)		{ params[0].radioParams.CentralFrequency = parse_CentralFrequency(i); }
void init_freqencyOffsetTX(BlinkParams *params, char *i)		{ params[0].radioParams.FreqencyOffset = parse_FrequencyOffset(i); }
void init_sampleRateTX(BlinkParams *params, char *i)			{ params[0].radioParams.SampleRate = parse_SampleRate(i); }
void init_bufferSizeTX(BlinkParams *params, char *i)			{ params[0].radioParams.TXBufferSize = parse_TXBufferSize(i); }
#endif



// RX Init functions
void init_inTypeRX(BlinkParams *params, char *typ)				{ params[1].inType = parse_type(typ); }
void init_outTypeRX(BlinkParams *params, char *typ)				{ params[1].outType = parse_type(typ); }
void init_inFileRX(BlinkParams *params, char *fn)				{ params[1].inFileName = fn; }
void init_outFileRX(BlinkParams *params, char *fn)				{ params[1].outFileName = fn; }
void init_inFileModeRX(BlinkParams *params, char *md)			{ params[1].inFileMode = parse_mode(md); }
void init_outFileModeRX(BlinkParams *params, char *md)			{ params[1].outFileMode = parse_mode(md); }
void init_outBufRX(BlinkParams *params, char *siz)				{ params[1].outBufSize = parse_size(siz); }
void init_inMemorySizeRX(BlinkParams *params, char *size)		{ params[1].inMemorySize = parse_size(size); }
void init_outMemorySizeRX(BlinkParams *params, char *size)		{ params[1].outMemorySize = parse_size(size); }
void init_dummySamplesRX(BlinkParams *params, char *siz)		{ params[1].dummySamples = parse_repeat(siz); }
void init_heapSizeRX(BlinkParams *params, char *siz)			{ params[1].heapSize = parse_size(siz); }
void init_inRepeatRX(BlinkParams *params, char *i)				{ params[1].inFileRepeats = parse_repeat(i); }
void init_LatencySamplingRX(BlinkParams *params, char *siz)		{ params[1].latencySampling = parse_size(siz); }
void init_LatencyCDFSizeRX(BlinkParams *params, char *siz)		{ params[1].latencyCDFSize = parse_size(siz); }


#ifdef SORA_PLATFORM
// Here we use one radio, so we only use radioParams in params[0]
// For full duplex one might want to use radioParams in params[1] here instead
void init_radioIdRX(BlinkParams *params, char *i)				{ params[0].radioParams.radioId = parse_radioID(i); }
void init_paRX(BlinkParams* params, char *i)					{ params[0].radioParams.RXpa = parse_Amp(i); }
void init_gainRX(BlinkParams* params, char *i)					{ params[0].radioParams.RXgain = parse_Amp(i); }
void init_centralFrequencyRX(BlinkParams *params, char *i)		{ params[0].radioParams.CentralFrequency = parse_CentralFrequency(i); }
void init_freqencyOffsetRX(BlinkParams *params, char *i)		{ params[0].radioParams.FreqencyOffset = parse_FrequencyOffset(i); }
void init_sampleRateRX(BlinkParams *params, char *i)			{ params[0].radioParams.SampleRate = parse_SampleRate(i); }
#endif



// Here is where we declare the parameters
#ifdef SORA_PLATFORM
#define PARAM_TABLE_LENGTH		40
#else
#define PARAM_TABLE_LENGTH		27
#endif

BlinkParamInfo paramTable[PARAM_TABLE_LENGTH] = 
// TX
  {  { "--memory-size=",
	   "--memory-size=<bytes>",
       "Size of memory buffer between TX and RX blocks",
       "0",
	   init_outMemorySizeTX },
	 { "--MAC-type=",
	   "--MAC-type=TX-test|RX-test|TX-only|RX-only",
	   "Which MAC to run",
	   "TX-test",
	   init_inTypeMAC },
	   { "--TX-PC=",
	   "--TX-PC=string",
	   "Name of a PC to which to connect to",
	   "",
	   init_txPC },
	   { "--TX-input=",
       "--TX-input=file|dummy|sora|ip",
       "Input TX samples come from a file, radio, or are dummy samples",
       "file",
       init_inTypeTX },
     { "--TX-output=", 
       "--TX-output=memory|file|dummy|sora|ip",
       "Output TX samples written to file, radio, or are ignored (dummy)",
       "file",
       init_outTypeTX },
     { "--TX-input-file-name=", 
       "--TX-input-file-name=...",
       "Path to TX input file. Meaningful if --TX-input=file.",
       "infile", 
       init_inFileTX },
     { "--TX-output-file-name=", 
       "--TX-output-file-name=...",
       "Path to TX output file. Meaningful if --TX-output=file.",
       "outfile", 
       init_outFileTX },
     { "--TX-input-file-mode=", 
       "--TX-input-file-mode=bin|dbg",
       "TX Input file is binary/debug (human readable). Meaningful if --TX-input=file.",
       "bin",
       init_inFileModeTX },
     { "--TX-output-file-mode=", 
       "--TX-output-file-mode=bin|dbg",
       "Output file is binary/debug (human readable). Meaningful if --TX-output=file.",
       "bin",
       init_outFileModeTX },
     { "--TX-output-buffer-size=", 
       "--TX-output-buffer-size=...",
       "TX Output buffer out of which we write to the output.",
       "16777216", 
       init_outBufTX },
     { "--TX-dummy-samples=", 
       "--TX-dummy-samples==*|1|2|...",
       "Number of dummy TX samples (* means repeated indefinitely). Meaningful if --TX-input=dummy",
       "20000000",
       init_dummySamplesTX },
     { "--TX-heap-size=", 
       "--TX-heap-size=...",
       "Size of heap to use in the generated TX program.",
       "20971520",     // 20MB default
       init_heapSizeTX },
     { "--TX-input-file-repeat=",
       "--TX-input-file-repeat=*|1|2|....",
       "Repeat the TX input file indefinitely (*) or a fixed number of times",
       "1", 
       init_inRepeatTX },
     { "--TX-latency-sampling=",
	   "--TX-latency-sampling=0|1|2|...",
	   "Number of writes over which the TX latency is measured (0 - no latency measurements)",
	   "0",
	   init_LatencySamplingTX },
     { "--TX-latency-CDF-size=",
	   "--TX-latency-CDF-size=...",
	   "Number of TX latency samples to be stored and printed (for CDF calculation)",
	   "0",
	   init_LatencyCDFSizeTX },
//RX
	   { "--RX-input=",
	   "--RX-input=memory|file|dummy|sora|ip",
	   "Input RX samples come from a file, radio, or are dummy samples",
	   "file",
	   init_inTypeRX },
	   { "--RX-output=",
	   "--RX-output=file|dummy|sora|ip",
	   "Output RX samples written to file, radio, or are ignored (dummy)",
	   "file",
	   init_outTypeRX },
	   { "--RX-input-file-name=",
	   "--RX-input-file-name=...",
	   "Path to RX input file. Meaningful if --RX-input=file.",
	   "infile",
	   init_inFileRX },
	   { "--RX-output-file-name=",
	   "--RX-output-file-name=...",
	   "Path to RX output file. Meaningful if --RX-output=file.",
	   "outfile",
	   init_outFileRX },
	   { "--RX-input-file-mode=",
	   "--RX-input-file-mode=bin|dbg",
	   "RX Input file is binary/debug (human readable). Meaningful if --RX-input=file.",
	   "bin",
	   init_inFileModeRX },
	   { "--RX-output-file-mode=",
	   "--RX-output-file-mode=bin|dbg",
	   "Output file is binary/debug (human readable). Meaningful if --RX-output=file.",
	   "bin",
	   init_outFileModeRX },
	   { "--RX-output-buffer-size=",
	   "--RX-output-buffer-size=...",
	   "RX Output buffer out of which we write to the output.",
	   "16777216",
	   init_outBufRX },
	   { "--RX-dummy-samples=",
	   "--RX-dummy-samples==*|1|2|...",
	   "Number of dummy RX samples (* means repeated indefinitely). Meaningful if --RX-input=dummy",
	   "20000000",
	   init_dummySamplesRX },
	   { "--RX-heap-size=",
	   "--RX-heap-size=...",
	   "Size of heap to use in the generated RX program.",
	   "20971520",     // 20MB default
	   init_heapSizeRX },
	   { "--RX-input-file-repeat=",
	   "--RX-input-file-repeat=*|1|2|....",
	   "Repeat the RX input file indefinitely (*) or a fixed number of times",
	   "1",
	   init_inRepeatRX },
	   { "--RX-latency-sampling=",
	   "--RX-latency-sampling=0|1|2|...",
	   "Number of writes over which the RX latency is measured (0 - no latency measurements)",
	   "0",
	   init_LatencySamplingRX },
	   { "--RX-latency-CDF-size=",
	   "--RX-latency-CDF-size=...",
	   "Number of RX latency samples to be stored and printed (for CDF calculation)",
	   "0",
	   init_LatencyCDFSizeRX },


#ifdef SORA_PLATFORM
       { "--TX-sora-radio-id=",
       "--TX-sora-radio-id = ...",
       "Sora TX radio ID",
       "0", 
       init_radioIdTX }
     , { "--TX-sora-gain=",
       "--TX-sora-gain = ...",
       "Sora TX gain",
       "0", 
       init_gainTX }
     , { "--TX-sora-central-frequency=",
       "--TX-sora-central-frequency = ...",
       "Sora central frequency in MHz (default 578)",
       "578", 
       init_centralFrequencyTX }
     , { "--TX-sora-freqency-offset=",
       "--TX-sora-freqency-offset = ...",
       "Sora frequency offset",
       "0", 
       init_freqencyOffsetTX }
     , { "--TX-sora-sample-rate=",
       "--TX-sora-sample-rate = ...",
       "Sora sample rate",
       "40", 
       init_sampleRateTX }
     , { "--TX-sora-tx-buffer-size=",
       "--TX-sora-tx-buffer-size = ...",
       "Size of the TX buffer transmitted at once (in number of complex16)",
       "2097152", 
       init_bufferSizeTX }
// RX
	 , { "--RX-sora-radio-id=",
		"--RX-sora-radio-id = ...",
		"Sora RX radio ID",
		"0",
		init_radioIdRX }
	 , { "--RX-sora-rx-pa=",
		 "--RX-sora-rx-pa = ...",
		 "Sora RX power amplification",
		 "0",
		 init_paRX }
	 , { "--RX-sora-rx-gain=",
		 "--RX-sora-rx-gain = ...",
		 "Sora RX gain",
		 "0",
		 init_gainRX }
	 , { "--RX-sora-gain=",
		 "--RX-sora-gain = ...",
		 "Sora RX gain",
		 "0",
		 init_gainRX }
	 , { "--RX-sora-central-frequency=",
		 "--RX-sora-central-frequency = ...",
		 "Sora RX central frequency in MHz (default 578)",
		 "578",
		 init_centralFrequencyRX }
	 , { "--RX-sora-freqency-offset=",
		 "--RX-sora-freqency-offset = ...",
		 "Sora RX frequency offset",
		 "0",
		 init_freqencyOffsetRX }
	 , { "--RX-sora-sample-rate=",
		 "--RX-sora-sample-rate = ...",
		 "Sora RX sample rate",
		 "40",
		 init_sampleRateRX }
#endif


};


void print_blink_usage() {
  int i = 0;
  fprintf(stdout,"Usage: <cmd> OPTS...\n");
  fprintf(stdout,"OPTS\n");
  while (i < sizeof(paramTable)/sizeof(BlinkParamInfo)) {
    fprintf(stdout,"%s\n", paramTable[i].param_use);
    fprintf(stdout,"\t%s\n", paramTable[i].param_descr);
    fprintf(stdout,"\tDefault: %s\n", paramTable[i].param_dflt);
    i++; 
  }
}


void try_parse_args(BlinkParams *params, int argc, char ** argv) {
  int pi; 
  if (argc <= 1) {
    print_blink_usage();
    exit(1); 
  }

  // Initialize everything to default
  for (pi=0; pi < sizeof(paramTable) / sizeof(BlinkParamInfo); pi++) {
      paramTable[pi].param_init(params, paramTable[pi].param_dflt);
  }

  // For every command line parameter, try to initialize it or fail if unknown
  for (int ai=1; ai < argc; ai++) {
    int initialized = 0;
    for (pi=0; pi < sizeof(paramTable) / sizeof(BlinkParamInfo); pi++) {
	  char *pval;
      if (!strstr(argv[ai],paramTable[pi].param_str)) continue;
      pval = & argv[ai][strlen(paramTable[pi].param_str)];
      paramTable[pi].param_init(params, pval);
      initialized = 1;
      break;
    }
    if (!initialized)
	{
	  fprintf(stderr, "Unknown parameter: %s\n", argv[ai]);
	  print_blink_usage();
	  exit(1);
	}

  }


}
