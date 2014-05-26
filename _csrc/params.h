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
#pragma once 

// Input-output types
typedef enum  { 
  TY_FILE  = 0,
  TY_DUMMY = 1,
  TY_SORA = 2,
  TY_IP = 3
} BlinkFileType;

// Binary or debug (human-readable) file format
typedef enum {
  MODE_DBG = 0,
  MODE_BIN = 1
} BlinkFileMode;




/**********************************************
	Radio parameters
***********************************************/ 

#ifdef SORA_PLATFORM
#include <soratime.h>
#include <thread_func.h>
#include <stdlib.h>
#include <time.h>


typedef struct {
	ULONG radioId;
	ULONG RXpa;
	ULONG RXgain;
	ULONG TXgain;
	ULONG CentralFrequency;
	LONG FreqencyOffset;
	ULONG SampleRate;
	ULONG TXBufferSize;
} SoraParameters;
#endif





/* How many times should we iterate the input dummy sample, 
 *  or the input samples that come from a file? 
 *  - Some n=0 means an infinite number of times
 *  - Some n>0 means exactly n times
 **********************************************************/
typedef unsigned long Repetitions;
#define INF_REPEAT 0 

struct BlinkGlobals {
	BlinkFileType inType;       // type of input
	BlinkFileType outType;      // type of output
	BlinkFileMode inFileMode;   // input file mode 
	BlinkFileMode outFileMode;  // output file mode 
	char *inFileName;           // input file name 
	char *outFileName;          // output file name 
	Repetitions inFileRepeats;  // #times to repeat the input buffer
	Repetitions dummySamples;   // #dummy samples (if inType == TY_DUMMY)
	unsigned int outBufSize;    // size of buffer we serve output from
    unsigned int heapSize;      // heap size for blink/wpl program
	unsigned long latencySampling;		// space between latency sampling in #writes (0 - no latency measurements)
	unsigned long latencyCDFSize;		// How many latency samples to be stored in the CDF table

#ifdef SORA_PLATFORM
	SoraParameters radioParams;

	// RX
	SORA_RADIO_RX_STREAM RxStream;
	PVOID pRxBuf;

	// TX
    PVOID TXBuffer;
#endif
};


/* Global configuration parameters 
*************************************************************************/
extern struct BlinkGlobals Globals;

FILE * try_open(char *name, char *mode);
void try_parse_args(int argc, char ** argv);
void try_read_filebuffer(char *filename, char **fb, unsigned int *len);

typedef struct {
  char * param_str;               // Name of param (for parsing)
  char * param_use;               // Use
  char * param_descr;             // Description
  char * param_dflt;              // Initialization value as string
  void (*param_init)(char *prm);  // Initializer
} BlinkParamInfo;

void print_blink_usage(void);
