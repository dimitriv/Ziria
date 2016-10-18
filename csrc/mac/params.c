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

#include "../wpl_alloc.h"
#include "../params.h"

#include "mac.h"

#ifdef __GNUC__
//typedef uint64 ULONG;
#endif

/* Read the file as a null-terminated string */
void try_read_filebuffer(HeapContextBlock *hblk, char *filename, char **fb, memsize_int *len)
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
	if (strcmp(typ,"sdr") == 0)	return TY_SDR;
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
	if (strcmp(typ, "TX-RX") == 0)	return MAC_TX_RX;
	fprintf(stderr, "Error: cannot parse MAC parameter %s\n", typ);
	exit(1);
}

PHYRate parse_PHYRate(char *typ)
{
	PHYRate r;
	ULONG nr = (ULONG)strtol(typ, NULL, 10);
	switch (nr)
	{
	case 6:
		r.enc = PHY_ENC_CR_12;
		r.mod = PHY_MOD_BPSK;
		break;
	case 9:
		r.enc = PHY_ENC_CR_34;
		r.mod = PHY_MOD_BPSK;
		break;
	case 12:
		r.enc = PHY_ENC_CR_12;
		r.mod = PHY_MOD_QPSK;
		break;
	case 18:
		r.enc = PHY_ENC_CR_34;
		r.mod = PHY_MOD_QPSK;
		break;
	case 24:
		r.enc = PHY_ENC_CR_12;
		r.mod = PHY_MOD_16QAM;
		break;
	case 36:
		r.enc = PHY_ENC_CR_34;
		r.mod = PHY_MOD_16QAM;
		break;
	case 48:
		r.enc = PHY_ENC_CR_23;
		r.mod = PHY_MOD_64QAM;
		break;
	case 54:
		r.enc = PHY_ENC_CR_34;
		r.mod = PHY_MOD_64QAM;
		break;
	default:
		fprintf(stderr, "Error: cannot parse PHY rate parameter %s\n", typ);
		exit(1);
	}
	return r;
}


void parse_txPC(char *typ)
{
	if (strcmp(typ, "") != 0)
	{
		strcpy(txPCBuf, typ);
		txPC = txPCBuf;
	}
	else
	{
		txPC = NULL;
	}
}


int parse_DEBUG(char *d) {
	return (int) strtol(d, NULL, 10);
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
#else
// TODO: Add error handling
 unsigned long parse_radioID(char *rp)
{
	return (unsigned long) strtoul(rp,NULL,10);
}


// TODO: Add error handling
 unsigned long  parse_Amp(char *rp)
{
	return (long)strtoul(rp, NULL, 10);
}


// TODO: Add error handling
 unsigned long  parse_CentralFrequency(char *rp)
{
	return (unsigned long)strtoul(rp, NULL, 10);
}


// TODO: Add error handling
 long  parse_FrequencyOffset(char *rp)
{
	return (long)strtol(rp, NULL, 10);
}


// TODO: Add error handling
unsigned long  parse_SampleRate(char *rp)
{
	return (unsigned long)strtoul(rp, NULL, 10);
}


// TODO: Add error handling
unsigned long  parse_TXBufferSize(char *rp)
{
	return (unsigned long)strtoul(rp, NULL, 10);
}

#endif

#ifdef LIME_RF
unsigned long  parse_ClockRate(char *rp)
{
	return (unsigned long)strtoul(rp, NULL, 10);
}
long  parse_ShiftQ(char *rp)
{
	return (long)strtol(rp, NULL, 10);
}

#endif


unsigned long parse_size (char *rp) {
  return (strtol(rp,NULL,10));
}


// Global function
void init_inTypeMAC(BlinkParams *params, char *typ)				{ mac_type = parse_MAC(typ); }
void init_txPC(BlinkParams *params, char *typ)					{ parse_txPC(typ); }
void init_PHYRate(BlinkParams *params, char *typ)				{ phy_rate = parse_PHYRate(typ); }
void init_DEBUG(BlinkParams *params, char *typ)					{ params[0].debug = parse_DEBUG(typ); 
																  params[1].debug = params[0].debug; }

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
// TODO - DEBUG - WARNING
// Here we use two radios at the moment
// But for the full MAC we may want to use one
void init_radioIdTX(BlinkParams *params, char *i)				{ params[0].radioParams.radioId = parse_radioID(i); }
void init_gainTX(BlinkParams *params, char *i)					{ params[0].radioParams.TXgain = parse_Amp(i); }
void init_centralFrequencyTX(BlinkParams *params, char *i)		{ params[0].radioParams.CentralFrequency = parse_CentralFrequency(i); }
void init_freqencyOffsetTX(BlinkParams *params, char *i)		{ params[0].radioParams.FreqencyOffset = parse_FrequencyOffset(i); }
void init_sampleRateTX(BlinkParams *params, char *i)			{ params[0].radioParams.SampleRate = parse_SampleRate(i); }
void init_bufferSizeTX(BlinkParams *params, char *i)			{ params[0].radioParams.TXBufferSize = parse_TXBufferSize(i); }


void init_radioIdRX(BlinkParams *params, char *i)				{ params[1].radioParams.radioId = parse_radioID(i); }
void init_paRX(BlinkParams* params, char *i)					{ params[1].radioParams.RXpa = parse_Amp(i); }
void init_gainRX(BlinkParams* params, char *i)					{ params[1].radioParams.RXgain = parse_Amp(i); }
void init_centralFrequencyRX(BlinkParams *params, char *i)		{ params[1].radioParams.CentralFrequency = parse_CentralFrequency(i); }
void init_freqencyOffsetRX(BlinkParams *params, char *i)		{ params[1].radioParams.FreqencyOffset = parse_FrequencyOffset(i); }
void init_sampleRateRX(BlinkParams *params, char *i)			{ params[1].radioParams.SampleRate = parse_SampleRate(i); }
#else

void init_radioId(BlinkParams *params, char *i)				{ params[0].radioParams.radioId = parse_radioID(i);
															  params[1].radioParams.radioId = params[0].radioParams.radioId;}

void init_RXpa(BlinkParams *params, char *i)				{ params[0].radioParams.RXpa = parse_Amp(i);
															  params[1].radioParams.RXpa = params[0].radioParams.RXpa;}

void init_RXgain(BlinkParams *params, char *i)				{ params[0].radioParams.RXgain = parse_Amp(i);
															  params[1].radioParams.RXgain = params[0].radioParams.RXgain;}

void init_TXgain(BlinkParams *params, char *i)				{ params[0].radioParams.TXgain = parse_Amp(i);
														      params[1].radioParams.TXgain = params[0].radioParams.TXgain;}

void init_CentralFrequency(BlinkParams *params, char *i)	{ params[0].radioParams.CentralFrequency = parse_CentralFrequency(i);
															  params[1].radioParams.CentralFrequency = params[0].radioParams.CentralFrequency;}

void init_FreqencyOffset(BlinkParams *params, char *i)		{ params[0].radioParams.FreqencyOffset = parse_FrequencyOffset(i);
															  params[1].radioParams.FreqencyOffset = params[0].radioParams.FreqencyOffset;}

void init_SampleRate(BlinkParams *params, char *i)			{ params[0].radioParams.SampleRate = parse_SampleRate(i);
															  params[1].radioParams.SampleRate = params[0].radioParams.SampleRate;}
//bandwidth is same format with samplerate pasring
void init_Bandwidth(BlinkParams *params, char *i)			{ params[0].radioParams.Bandwidth = parse_SampleRate(i);
															  params[1].radioParams.Bandwidth = params[0].radioParams.Bandwidth;}

#endif
void init_TXBufferSize(BlinkParams *params, char *i)		{ params[0].radioParams.TXBufferSize = parse_TXBufferSize(i);
															  params[1].radioParams.TXBufferSize = params[0].radioParams.TXBufferSize;}

#ifdef LIME_RF
void init_ClockRate(BlinkParams *params, char *i)	{ params[0].radioParams.clockRate = parse_ClockRate(i);
													  params[1].radioParams.clockRate = params[0].radioParams.clockRate;}
void init_ShiftQ(BlinkParams *params, char *i)	{ params[0].radioParams.shiftQ = parse_ShiftQ(i);
												  params[1].radioParams.shiftQ = parse_ShiftQ(i);}

#ifdef PL_CS
void init_corr_thr(BlinkParams *params, char *i)	{ params[0].radioParams.corr_thr = i;
													  params[1].radioParams.corr_thr = i;}
void init_resetcount(BlinkParams *params, char *i)	{ params[0].radioParams.rst_countdown = i;
													  params[1].radioParams.rst_countdown = i;}
#endif
#endif

#if defined(ADI_RF) || defined(LIME_RF)
void init_SdrAddress(BlinkParams *params, char *i)	{
	if (!strcmp(i, ""))
		params[0].radioParams.host=NULL;
	else
		params[0].radioParams.host = i;
	params[1].radioParams.host = params[0].radioParams.host;}
#endif

// Here is where we declare the parameters
#ifdef SORA_PLATFORM
#define PARAM_TABLE_LENGTH		42
#else
#ifdef ADI_RF
#define PARAM_TABLE_LENGTH		29 + 10
#else
#ifdef LIME_RF
#ifdef PL_CS
#define PARAM_TABLE_LENGTH		29 + 12 + 2
#else
#define PARAM_TABLE_LENGTH		29 + 12
#endif
#else
#define PARAM_TABLE_LENGTH		38
#endif
#endif
#endif

BlinkParamInfo paramTable[PARAM_TABLE_LENGTH] = 
// TX
  {  { "--memory-size=",
	   "--memory-size=<bytes>",
       "Size of memory buffer between TX and RX blocks",
       "0",
	   init_outMemorySizeTX },
	 { "--MAC-type=",
	   "--MAC-type=TX-test|RX-test|TX-only|RX-only|TX-RX",
	   "Which MAC to run",
	   "TX-test",
	   init_inTypeMAC },
	 { "--TX-PHY-rate=",
	   "--TX-PHY-rate=6|9|12|18|24|36|48|54",
	   "PHY TX rate in Mbps",
	   "18",
	   init_PHYRate },
     { "--DEBUG=",
	   "--DEBUG=0|1",
	   "Turn DEBUG display on (1) or off (0)",
	   "0",
	   init_DEBUG },
	 { "--TX-PC=",
	   "--TX-PC=string",
	   "Name of a PC to which to connect to",
	   "",
	   init_txPC },
	 { "--TX-input=",
       "--TX-input=file|dummy|sdr|ip",
       "Input TX samples come from a file, radio, or are dummy samples",
       "file",
       init_inTypeTX },
     { "--TX-output=", 
       "--TX-output=memory|file|dummy|sdr|ip",
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
	   "--RX-input=memory|file|dummy|sdr|ip",
	   "Input RX samples come from a file, radio, or are dummy samples",
	   "file",
	   init_inTypeRX },
	   { "--RX-output=",
	   "--RX-output=file|dummy|sdr|ip",
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
#else
  { "--sdr-radio-id=",
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
	   , { "--sdr-bandwidth=",
	   "--sdr-bandwidth = ...",
	   "SDR bandwidth",
	   "20000000",
	   init_Bandwidth }
  , { "--sdr-tx-buffer-size=",
    "--sdr-tx-buffer-size = ...",
    "Size of the TX buffer transmitted at once (in number of complex16)",
    "2097152",
    init_TXBufferSize }
#ifdef LIME_RF
, { "--sdr-clock-rate=",
      "--sdr-clock-rate = ...",
      "SDR clock rate in MHz (default 40)",
      "40",
      init_ClockRate }
  , { "--shift-q=",
        "--shift-q= ...",
        "shift Q samples in time versus I samples",
        "0",
        init_ShiftQ }
#ifdef PL_CS
  , { "--corr-thr=",
        "--corr-thr = ...",
        "Correlation Threshold",
        "50000000",
        init_corr_thr }
  , { "--self-reset-countdown=",
        "--self-reset-countdown= ...",
        "PL self-reset sample countdown",
        "10000",
        init_resetcount }
#endif
#endif

#if defined(ADI_RF) || defined(LIME_RF)
, { "--sdr-ip-address=",
      "--sdr-ip-address = ...",
      "SDR IP address (default)",
      "",
      init_SdrAddress }
#endif
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
