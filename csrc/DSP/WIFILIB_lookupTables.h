/**
Atomix project, WIFILIB_lookupTables.h, TODO: insert summary here
Copyright (c) 2015 Stanford University
Released under the Apache License v2.0. See the LICENSE file for details.
Author(s): Manu Bansal
*/
#ifdef DEAL_WITH_ME_LATER

#ifndef WIFILIB_LOOKUPTABLES_H_
#define WIFILIB_LOOKUPTABLES_H_

#include "or_types.h"

extern const Uint32 wifilib_rot32[32];
//extern const Uint32 wifilib_minusRot32[32];

//#define DEROT_TABLE_SIZE 				1024
//#define DEROT_TABLE_SIZE_N_BITS		10

//table needed for better than 1kHz resolution is 20000 entries,
//so we create a 15-bit lookup table. Note that this table will
//be 4B * 2^5k = 128KB, and we are creating two such tables, one
//positive, one negative, which will eat up 256KB of L2 space!
//#define DEROT_TABLE_SIZE 				32768
//#define DEROT_TABLE_SIZE_N_BITS		15

//instead, if we want a least count of 1kHz that we can correct,
//to keep the resolution possible, we'll maintain a table good
//for one symbol duration, that is, 80 samples. every symbol
//will be corrected with the same starting point in the table,
//so that each symbol will have a random phase but no cfo. phase
//will then be handled by pilot phase tracking. then, the angle
//granularity required is that of 32768 divisions of [0,1) uniform
//angle range, and we would want to keep the first 80 entries of
//such a table. However, if we want to cater to upto 50kHz of
//maximum cfo, we can extend the table by a factor of 50 samples,
//and then cover it with a proportional stride given the cfo.

//If (least count) angle corresponds to (1/32768) on [0,1) scale,
//we are correcting f = 1/(32768*50ns) = 0.61kHz. So to correct
//50kHz, we need table length 80samples * (50/0.61) = 80 * 83.3,
//or about 6400 samples long, which will occupy 6.25*4kB = 25kB
//per table, which is much more affordable, even for two tables
//together occupying 50kB.

#define DEROT_TABLE_FULL_SIZE			32768			//least count
#define DEROT_TABLE_FULL_SIZE_N_BITS	15				//least count
#define DEROT_TABLE_SIZE				8192			//next power of 2 after 6400
#define DEROT_TABLE_SIZE_N_BITS			13
//NOTE: the above size of 8192 allows atleast 81 sample correction, which
//is used in relative alignment of two ltf sequences, since the second ltf
//will be 80-samples after the first one and will need to be matched in phase
//to the first ltf for proper channel estimation.
#define DEROT_TABLE_Q_VALUE				14


//size of this table sets the resolution of frequency offset error that we can correct,
//or, equivalently, the residual error that will remain
//#define DEROT_TABLE_SIZE	96
//#define DEROT_TABLE_SIZE	32

extern Cplx16U wifilib_minusRot[DEROT_TABLE_SIZE];
extern Cplx16U wifilib_plusRot[DEROT_TABLE_SIZE];

extern Uint16 WIFILIB_softSlicingTable16qam[];
extern Uint32 WIFILIB_softSlicingTable64qam[];

extern Int16 wifilib_ack_preamble_signal[];

extern Uint32 WIFILIB_crc32LookupTable[];

void WIFILIB_genDerotTable();

void WIFILIB_genCrc32LookupTable();

void WIFILIB_genMapperLUTs();

#endif /*WIFILIB_LOOKUPTABLES_H_*/
#endif
