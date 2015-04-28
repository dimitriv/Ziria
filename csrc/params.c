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

#include "types.h"
#include "wpl_alloc.h"
#include "params.h"


// Parse debug level
int parse_DEBUG(char *d) {
	return (int)strtol(d, NULL, 10);
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
	if (strcmp(typ,"file") == 0)  return TY_FILE; 
	if (strcmp(typ,"dummy") == 0) return TY_DUMMY;
	if (strcmp(typ,"sdr") == 0)   return TY_SDR;
	if (strcmp(typ, "ip") == 0)   return TY_IP;
	fprintf(stderr, "Error: cannot parse type parameter %s\n",typ);
	exit(1);
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
	return (ULONG) strtoul(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_Amp(char *rp) 
{
	return (ULONG) strtoul(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_CentralFrequency(char *rp) 
{
	return (ULONG) strtoul(rp,NULL,10);
}


// TODO: Add error handling
LONG parse_FrequencyOffset(char *rp) 
{
	return (LONG) strtol(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_SampleRate(char *rp) 
{
	return (ULONG) strtoul(rp,NULL,10);
}


// TODO: Add error handling
ULONG parse_TXBufferSize(char *rp) 
{
	return (ULONG) strtoul(rp,NULL,10);
}
#endif



unsigned long parse_size (char *rp) {
  return (strtol(rp,NULL,10));
}



memsize_int parse_mem_size(char *rp) {
	return (strtoll(rp, NULL, 10));
}



void init_DEBUG(BlinkParams *params, char *typ)				{ params->debug = parse_DEBUG(typ); }
void init_inType(BlinkParams *params, char *typ)			{ params->inType = parse_type(typ); }
void init_outType(BlinkParams *params, char *typ)			{ params->outType = parse_type(typ); }
void init_inFile(BlinkParams *params, char *fn)				{ params->inFileName = fn; }
void init_outFile(BlinkParams *params, char *fn)			{ params->outFileName = fn; }
void init_inFileMode(BlinkParams *params, char *md)			{ params->inFileMode = parse_mode(md); }
void init_outFileMode(BlinkParams *params, char *md)		{ params->outFileMode = parse_mode(md); }
void init_outBuf(BlinkParams *params, char *siz)			{ params->outBufSize = parse_size(siz); }
void init_dummySamples(BlinkParams *params, char *siz)		{ params->dummySamples = parse_repeat(siz); }
void init_heapSize(BlinkParams *params, char *siz)			{ params->heapSize = parse_mem_size(siz); }
void init_inRepeat(BlinkParams *params, char *i)			{ params->inFileRepeats = parse_repeat(i); }
void init_LatencySampling(BlinkParams *params, char *siz)   { params->latencySampling = parse_size(siz); }
void init_LatencyCDFSize(BlinkParams *params, char *siz)    { params->latencyCDFSize = parse_size(siz); }


// SDR related
void init_radioId(BlinkParams *params, char *i)				{ params->radioParams.radioId = parse_radioID(i); }
void init_RXpa(BlinkParams *params, char *i)				{ params->radioParams.RXpa = parse_Amp(i); }
void init_RXgain(BlinkParams *params, char *i)				{ params->radioParams.RXgain = parse_Amp(i); }
void init_TXgain(BlinkParams *params, char *i)				{ params->radioParams.TXgain = parse_Amp(i); }
void init_CentralFrequency(BlinkParams *params, char *i)	{ params->radioParams.CentralFrequency = parse_CentralFrequency(i); }
void init_FreqencyOffset(BlinkParams *params, char *i)		{ params->radioParams.FreqencyOffset = parse_FrequencyOffset(i); }
void init_SampleRate(BlinkParams *params, char *i)			{ params->radioParams.SampleRate = parse_SampleRate(i); 
															  params->radioParams.Bandwidth = params->radioParams.SampleRate; }
void init_TXBufferSize(BlinkParams *params, char *i)		{ params->radioParams.TXBufferSize = parse_TXBufferSize(i); }



// Here is where we declare the parameters
#define PARAM_TABLE_LENGTH		21

BlinkParamInfo paramTable[PARAM_TABLE_LENGTH] = 
  {  { "--DEBUG=",
	   "--DEBUG=...",
	   "Turn DEBUG display (0 - no debug output)",
	   "0",
	   init_DEBUG },
	 { "--input=", 
       "--input=file|dummy|sora|bladerf|ip",
       "Input samples come from a file, radio, or are dummy samples",
       "file",
       init_inType },
     { "--output=", 
       "--output=file|dummy|sora|bladerf|ip",
       "Output samples written to file, radio, or are ignored (dummy)",
       "file",
       init_outType },
     { "--input-file-name=", 
       "--input-file-name=...",
       "Path to input file. Meaningful if --input=file.",
       "infile", 
       init_inFile },
     { "--output-file-name=", 
       "--output-file-name=...",
       "Path to output file. Meaningful if --output=file.",
       "outfile", 
       init_outFile },
     { "--input-file-mode=", 
       "--input-file-mode=bin|dbg",
       "Input file is binary/debug (human readable). Meaningful if --input=file.",
       "bin",
       init_inFileMode },
     { "--output-file-mode=", 
       "--output-file-mode=bin|dbg",
       "Output file is binary/debug (human readable). Meaningful if --output=file.",
       "bin",
       init_outFileMode },
     { "--output-buffer-size=", 
       "--output-buffer-size=...",
       "Output buffer out of which we write to the output.",
       "16777216", 
       init_outBuf },
     { "--dummy-samples=", 
       "--dummy-samples==*|1|2|...",
       "Number of dummy samples (* means repeated indefinitely). Meaningful if --input=dummy",
       "20000000",
       init_dummySamples },
     { "--heap-size=", 
       "--heap-size=...",
       "Size of heap to use in the generated program.",
       "20971520",     // 20MB default
       init_heapSize },
     { "--input-file-repeat=",
       "--input-file-repeat=*|1|2|....",
       "Repeat the input file indefinitely (*) or a fixed number of times",
       "1", 
       init_inRepeat },
     { "--latency-sampling=",
	   "--latency-sampling=0|1|2|...",
	   "Number of read/writes (edit STAMP_AT_READ to choose between reads/writes) over which the latency is measured (0 - no latency measurements)",
	   "0",
	   init_LatencySampling },
     { "--latency-CDF-size=",
	   "--latency-CDF-size=...",
	   "Number of latency samples to be stored and printed (for CDF calculation)",
	   "0",
	   init_LatencyCDFSize }

// ***************
// SDR Related
// ***************

     , { "--sdr-radio-id=",
       "--sdr-radio-id = ...",
       "SDR radio ID",
       "0", 
       init_radioId }
     , { "--sdr-rx-pa=",
       "--sdr-rx-pa = ...",
       "SDR RX power amplification",
       "0", 
       init_RXpa }
     , { "--sdr-rx-gain=",
       "--sdr-rx-gain = ...",
       "SDR RX gain",
#ifdef SORA_RF
	   "0",
#else
	   "20",
#endif
       init_RXgain }
     , { "--sdr-tx-gain=",
       "--sdr-tx-gain = ...",
       "SDR TX gain",
       "0", 
       init_TXgain }
     , { "--sdr-central-frequency=",
       "--sdr-central-frequency = ...",
#ifdef SORA_RF
       "SDR central frequency in MHz (default 578)",
       "578", 
#else
	   "SDR central frequency in Hz (default 2412 MHz)",
	   "2412000000",
#endif
       init_CentralFrequency }
     , { "--sdr-freqency-offset=",
       "--sdr-freqency-offset = ...",
       "SDR frequency offset",
       "0", 
       init_FreqencyOffset }
     , { "--sdr-sample-rate=",
       "--sdr-sample-rate = ...",
       "SDR sample rate",
#ifdef SORA_RF
	   "40",
#else
	   "20000000",
#endif
       init_SampleRate }
     , { "--sdr-tx-buffer-size=",
       "--sdr-tx-buffer-size = ...",
       "Size of the TX buffer transmitted at once (in number of complex16)",
       "2097152", 
       init_TXBufferSize }


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
