/**
Atomix project, WIFILIB_lookupTables.c, TODO: insert summary here
Copyright (c) 2015 Stanford University
Released under the Apache License v2.0. See the LICENSE file for details.
Author(s): Manu Bansal
*/

#ifdef DEAL_WITH_ME_LATER


#include "WIFILIB_lookupTables.h"
//#include "IQmath_inline_all.h"
#include "IQmath_inline.h"
#include <src/DSP_fft16x16/c66/gen_twiddle_fft16x16.h>
#include "ORILIB_CRC32_algos.h"

#pragma DATA_SECTION(ORILIB_twiddle_factors_fft16x16_64, ".data:lookup:ORILIB_twiddle_factors_fft16x16_64")
#pragma DATA_SECTION(ORILIB_twiddle_factors_ifft16x16_64, ".data:lookup:ORILIB_twiddle_factors_ifft16x16_64")
Int16 ORILIB_twiddle_factors_fft16x16_64[128] __attribute__((aligned(8)));
Int16 ORILIB_twiddle_factors_ifft16x16_64[128] __attribute__((aligned(8)));

#define LUT_SIZE_SOFT_ESTIMATES 256
Uint16 WIFILIB_softSlicingTable16qam[LUT_SIZE_SOFT_ESTIMATES] __attribute__((aligned(8)));

#pragma DATA_SECTION(WIFILIB_softSlicingTable64qam, ".data:WIFILIB_softSlicingTable64qam")
Uint32 WIFILIB_softSlicingTable64qam[LUT_SIZE_SOFT_ESTIMATES] __attribute__((aligned(8)));

#pragma DATA_SECTION(WIFILIB_crc32LookupTable, ".data:WIFILIB_crc32LookupTable")
Uint32 WIFILIB_crc32LookupTable[256];

#pragma DATA_SECTION(".data:WIFILIB_mapperLUTbpsk")
Uint32 WIFILIB_mapperLUTbpsk[2]   __attribute__((aligned(8)));

//------------------------------- constellation map characteristics ---------------------------------
#define PHY_QPSK_NORMALIZATION_FACTOR_Q15			23170	//1/sqrt(2)
#define PHY_16QAM_NORMALIZATION_FACTOR_Q15			10362	//1/sqrt(10)
#define PHY_64QAM_NORMALIZATION_FACTOR_Q15			5056	//1/sqrt(42)

#define PHY_16QAM_SLAB_P1P3MSB						1		//+1,+3 slab occupies the MSB (see softSlicerTabGen.c)
#define PHY_16QAM_SLAB_P1P3_TO_SOFT_VAL				1		//+1,+3 slab bits map to +1 (see softSlicerTabGen.c)
#define PHY_16QAM_SLAB_M1P1_TO_SOFT_VAL				1		//-1,+1 slab bits map to +1 (see softSlicerTabGen.c)

#define PHY_SOFT_SLICER_UNNORM_UNIT_SCALE			6		//Q6  this is because the biggest normalized

//Ref. http://www.ti.com/lit/ug/sprugv6a/sprugv6a.pdf, sec 2.7.2
#define PHY_SOFT_BITS_PRECISION 					6

//  CrcPoly = (32),26,23,22,16,12,11,10,8,7,5,4,2,1,0
//		0000 0100 1100 0001 0001 1101 1011 0111
#define WIFI_CRC32_POLYNOMIAL 0x04C11DB7

//---------------------------------------------------------------------------------------------------



/** Table defines the interleaver bit mapping from input bits to output bits
 * for a BPSK-encoded OFDM symbol */
far const uint16_t WIFILIB_DeinterleaverLutWifi_BpskLUT[48] = 
    { 0, 16, 32, 1, 17, 33, 2, 18, 
    34, 3, 19, 35, 4, 20, 36, 5, 
    21, 37, 6, 22, 38, 7, 23, 39, 
    8, 24, 40, 9, 25, 41, 10, 26, 
    42, 11, 27, 43, 12, 28, 44, 13, 
    29, 45, 14, 30, 46, 15, 31, 47 };

/** Table defines the interleaver bit mapping from input bits to output bits
 * for a QPSK-encoded OFDM symbol */
far const uint16_t WIFILIB_DeinterleaverLutWifi_QpskLUT[96] = 
    { 0, 16, 32, 48, 64, 80, 1, 17, 
    33, 49, 65, 81, 2, 18, 34, 50, 
    66, 82, 3, 19, 35, 51, 67, 83, 
    4, 20, 36, 52, 68, 84, 5, 21, 
    37, 53, 69, 85, 6, 22, 38, 54, 
    70, 86, 7, 23, 39, 55, 71, 87, 
    8, 24, 40, 56, 72, 88, 9, 25, 
    41, 57, 73, 89, 10, 26, 42, 58, 
    74, 90, 11, 27, 43, 59, 75, 91, 
    12, 28, 44, 60, 76, 92, 13, 29, 
    45, 61, 77, 93, 14, 30, 46, 62, 
    78, 94, 15, 31, 47, 63, 79, 95 };
													    
/** Table defines the interleaver bit mapping from input bits to output bits
 * for a 16QAM-encoded OFDM symbol */
far const uint16_t WIFILIB_DeinterleaverLutWifi_16qamLUT[192] = 
    { 0, 16, 32, 48, 64, 80, 96, 112, 
    128, 144, 160, 176, 17, 1, 49, 33, 
    81, 65, 113, 97, 145, 129, 177, 161, 
    2, 18, 34, 50, 66, 82, 98, 114, 
    130, 146, 162, 178, 19, 3, 51, 35, 
    83, 67, 115, 99, 147, 131, 179, 163, 
    4, 20, 36, 52, 68, 84, 100, 116, 
    132, 148, 164, 180, 21, 5, 53, 37, 
    85, 69, 117, 101, 149, 133, 181, 165, 
    6, 22, 38, 54, 70, 86, 102, 118, 
    134, 150, 166, 182, 23, 7, 55, 39, 
    87, 71, 119, 103, 151, 135, 183, 167, 
    8, 24, 40, 56, 72, 88, 104, 120, 
    136, 152, 168, 184, 25, 9, 57, 41, 
    89, 73, 121, 105, 153, 137, 185, 169, 
    10, 26, 42, 58, 74, 90, 106, 122, 
    138, 154, 170, 186, 27, 11, 59, 43, 
    91, 75, 123, 107, 155, 139, 187, 171, 
    12, 28, 44, 60, 76, 92, 108, 124, 
    140, 156, 172, 188, 29, 13, 61, 45, 
    93, 77, 125, 109, 157, 141, 189, 173, 
    14, 30, 46, 62, 78, 94, 110, 126, 
    142, 158, 174, 190, 31, 15, 63, 47, 
    95, 79, 127, 111, 159, 143, 191, 175 };

#pragma DATA_SECTION(WIFILIB_DeinterleaverLutWifi_64qamLUT, ".const:WIFILIB_DeinterleaverLutWifi_64qamLUT")
/** Table defines the interleaver bit mapping from input bits to output bits
 * for a 64QAM-encoded OFDM symbol */															
far const uint16_t WIFILIB_DeinterleaverLutWifi_64qamLUT[288] = {
     0, 16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240,
   256, 272, 17, 33, 1, 65, 81, 49, 113, 129, 97, 161, 177, 145, 209, 225,
   193, 257, 273, 241, 34, 2, 18, 82, 50, 66, 130, 98, 114, 178, 146, 162,
   226, 194, 210, 274, 242, 258, 3, 19, 35, 51, 67, 83, 99, 115, 131, 147,
   163, 179, 195, 211, 227, 243, 259, 275, 20, 36, 4, 68, 84, 52, 116, 132,
   100, 164, 180, 148, 212, 228, 196, 260, 276, 244, 37, 5, 21, 85, 53, 69,
   133, 101, 117, 181, 149, 165, 229, 197, 213, 277, 245, 261, 6, 22, 38, 54,
    70, 86, 102, 118, 134, 150, 166, 182, 198, 214, 230, 246, 262, 278, 23, 39,
     7, 71, 87, 55, 119, 135, 103, 167, 183, 151, 215, 231, 199, 263, 279, 247,
    40, 8, 24, 88, 56, 72, 136, 104, 120, 184, 152, 168, 232, 200, 216, 280,
   248, 264, 9, 25, 41, 57, 73, 89, 105, 121, 137, 153, 169, 185, 201, 217,
   233, 249, 265, 281, 26, 42, 10, 74, 90, 58, 122, 138, 106, 170, 186, 154,
   218, 234, 202, 266, 282, 250, 43, 11, 27, 91, 59, 75, 139, 107, 123, 187,
   155, 171, 235, 203, 219, 283, 251, 267, 12, 28, 44, 60, 76, 92, 108, 124,
   140, 156, 172, 188, 204, 220, 236, 252, 268, 284, 29, 45, 13, 77, 93, 61,
   125, 141, 109, 173, 189, 157, 221, 237, 205, 269, 285, 253, 46, 14, 30, 94,
    62, 78, 142, 110, 126, 190, 158, 174, 238, 206, 222, 286, 254, 270, 15, 31,
    47, 63, 79, 95, 111, 127, 143, 159, 175, 191, 207, 223, 239, 255, 271, 287 };

#pragma DATA_SECTION(ORILIB_pilot_polarity_sequence, ".const:ORILIB_pilot_polarity_sequence")
const far int8_t ORILIB_pilot_polarity_sequence[127] =
{1,1,1,1, -1,-1,-1,1, -1,-1,-1,-1, 1,1,-1,1, -1,-1,1,1, -1,1,1,-1, 1,1,1,1, 1,1,-1,1,
1,1,-1,1, 1,-1,-1,1, 1,1,-1,1, -1,-1,-1,1, -1,1,-1,-1, 1,-1,-1,1, 1,1,1,1, -1,-1,1,1,
-1,-1,1,-1, 1,-1,1,1, -1,-1,-1,1, 1,-1,-1,-1, -1,1,-1,-1, 1,-1,1,1, 1,1,-1,1, -1,1,-1,1,
-1,-1,-1,-1, -1,1,-1,1, 1,-1,1,-1, 1,1,1,-1, -1,1,-1,-1, -1,1,1,1, -1,-1,-1,-1, -1,-1,-1};

const far Cplx16 ORILIB_expected_ltf_symbols[52] = 
{ {1,0}, {1,0}, {-1,0}, {-1,0}, {1,0}, {1,0}, {-1,0}, {1,0},
{-1,0}, {1,0}, {1,0}, {1,0}, {1,0}, {1,0}, {1,0}, {-1,0},
{-1,0}, {1,0}, {1,0}, {-1,0}, {1,0}, {-1,0}, {1,0}, {1,0},
{1,0}, {1,0}, {1,0}, {-1,0}, {-1,0}, {1,0}, {1,0}, {-1,0},
{1,0}, {-1,0}, {1,0}, {-1,0}, {-1,0}, {-1,0}, {-1,0}, {-1,0},
{1,0}, {1,0}, {-1,0}, {-1,0}, {1,0}, {-1,0}, {1,0}, {-1,0},
{1,0}, {1,0}, {1,0}, {1,0} };

const far Cplx16 ORILIB_reference_ltf_d_signs[52] =
		//same +1/-1 symbol copied to imag too, to
		//set the sign for both parts on the rx symbols

{ {1,1}, {1,1}, {-1,-1}, {-1,-1}, {1,1}, {1,1}, {-1,-1}, {1,1},
{-1,-1}, {1,1}, {1,1}, {1,1}, {1,1}, {1,1}, {1,1}, {-1,-1},
{-1,-1}, {1,1}, {1,1}, {-1,-1}, {1,1}, {-1,-1}, {1,1}, {1,1},
{1,1}, {1,1}, {1,1}, {-1,-1}, {-1,-1}, {1,1}, {1,1}, {-1,-1},
{1,1}, {-1,-1}, {1,1}, {-1,-1}, {-1,-1}, {-1,-1}, {-1,-1}, {-1,-1},
{1,1}, {1,1}, {-1,-1}, {-1,-1}, {1,1}, {-1,-1}, {1,1}, {-1,-1},
{1,1}, {1,1}, {1,1}, {1,1} };


/* Phase rotations e^(2.Pi.k.n/32) in Q15 format used for sounding 
 * coefficient sequences for cyclic shift separability
 */
const far Uint32 wifilib_rot32[32] =
{
  /* 0  0       */  0x7FFF0000, /* cos(0) sin(0) */
  /* 1  Pi/16   */  0x7D8A18F8,
  /* 2  Pi/8    */  0x764130FB,
  /* 3  3Pi/16  */  0x6A6D471C,
  /* 4  Pi/4    */  0x5A825A82,
  /* 5  5Pi/16  */  0x471C6A6D,
  /* 6  3Pi/8   */  0x30FB7641,
  /* 7  7Pi/16  */  0x18F87D8A,
  /* 8  Pi/2    */  0x00007FFF,
  /* 9  9Pi/16  */  0xE7087D8A,
  /* 10 5Pi/8   */  0xCF057641,
  /* 11 11Pi/16 */  0xB8E46A6D,
  /* 12 3Pi/4   */  0xA57E5A82,
  /* 13 13Pi/16 */  0x9593471C,
  /* 14 7Pi/8   */  0x89BF30FB,
  /* 15 15Pi/16 */  0x827618F8,
  /* 16 Pi      */  0x80000000, /* cos(Pi) sin(Pi) */
  /* 17 17Pi/16 */  0x8276E708,
  /* 18 9Pi/8   */  0x89BFCF05,
  /* 19 19Pi/16 */  0x9593B8E4,
  /* 20 5Pi/4   */  0xA57EA57E,
  /* 21 21Pi/16 */  0xB8E49593,
  /* 22 11Pi/8  */  0xCF0589BF,
  /* 23 23Pi/16 */  0xE7088276,
  /* 24 3Pi/2   */  0x00008000,
  /* 25 25Pi/16 */  0x18F88276,
  /* 26 13Pi/8  */  0x30FB89BF,
  /* 27 27Pi/16 */  0x471C9593,
  /* 28 7Pi/4   */  0x5A82A57E,
  /* 29 29Pi/16 */  0x6A6DB8E4,
  /* 30 15Pi/8  */  0x7641CF05,
  /* 31 31Pi/16 */  0x7D8AE708  /* cos(31Pi/16) sin(31Pi/16) */ 
}; /* End of wxlib_ulSndCoeffRot32[] */

#pragma DATA_SECTION(wifilib_ack_preamble_signal, ".data:lookup:wifilib_ack_preamble_signal")
Int16 wifilib_ack_preamble_signal[] = { //{{{
        94,         94,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
       188,        188,
      -543,         10,
       -55,       -322,
       585,        -52,
       377,          0,
       585,        -52,
       -55,       -322,
      -543,         10,
       188,        188,
        10,       -543,
      -322,        -55,
       -52,        585,
         0,        377,
       -52,        585,
      -322,        -55,
        10,       -543,
      -226,         94,
        50,       -400,
       376,       -434,
      -376,       -472,
       -11,       -220,
       308,        303,
      -522,         84,
      -499,         68,
      -144,        618,
      -231,         89,
      -247,       -333,
       285,        -58,
       337,       -378,
      -538,       -267,
      -234,       -161,
       151,       -403,
       256,        256,
       488,         17,
       -92,       -658,
       240,         61,
       100,        240,
      -560,        194,
         4,        471,
       218,        -17,
       400,        106,
      -157,        435,
      -472,        226,
       245,        359,
        86,       -114,
       397,       -339,
       163,        455,
       -21,        493,
       640,          0,
       -21,       -493,
       163,       -455,
       397,        339,
        86,        114,
       245,       -359,
      -472,       -226,
      -157,       -435,
       400,       -106,
       218,         17,
         4,       -471,
      -560,       -194,
       100,       -240,
       240,        -61,
       -92,        658,
       488,        -17,
       256,       -256,
       151,        403,
      -234,        161,
      -538,        267,
       337,        378,
       285,         58,
      -247,        333,
      -231,        -89,
      -144,       -618,
      -499,        -68,
      -522,        -84,
       308,       -303,
       -11,        220,
      -376,        472,
       376,        434,
        50,        400,
      -640,          0,
        50,       -400,
       376,       -434,
      -376,       -472,
       -11,       -220,
       308,        303,
      -522,         84,
      -499,         68,
      -144,        618,
      -231,         89,
      -247,       -333,
       285,        -58,
       337,       -378,
      -538,       -267,
      -234,       -161,
       151,       -403,
       256,        256,
       488,         17,
       -92,       -658,
       240,         61,
       100,        240,
      -560,        194,
         4,        471,
       218,        -17,
       400,        106,
      -157,        435,
      -472,        226,
       245,        359,
        86,       -114,
       397,       -339,
       163,        455,
       -21,        493,
       640,          0,
       -21,       -493,
       163,       -455,
       397,        339,
        86,        114,
       245,       -359,
      -472,       -226,
      -157,       -435,
       400,       -106,
       218,         17,
         4,       -471,
      -560,       -194,
       100,       -240,
       240,        -61,
       -92,        658,
       488,        -17,
       256,       -256,
       151,        403,
      -234,        161,
      -538,        267,
       337,        378,
       285,         58,
      -247,        333,
      -231,        -89,
      -144,       -618,
      -499,        -68,
      -522,        -84,
       308,       -303,
       -11,        220,
      -376,        472,
       376,        434,
        50,        400,
      -640,          0,
        50,       -400,
       376,       -434,
      -376,       -472,
       -11,       -220,
       308,        303,
      -522,         84,
      -499,         68,
      -144,        618,
      -231,         89,
      -247,       -333,
       285,        -58,
       337,       -378,
      -538,       -267,
      -234,       -161,
       151,       -403,
       256,        256,
       488,         17,
       -92,       -658,
       240,         61,
       100,        240,
      -560,        194,
         4,        471,
       218,        -17,
       400,        106,
      -157,        435,
      -472,        226,
       245,        359,
        86,       -114,
       397,       -339,
       163,        455,
       -21,        493,
      -164,       -128,
      -218,        201,
      -172,       -502,
      -517,       -152,
       -90,       -163,
       655,        325,
       459,        538,
       -25,       -177,
         0,        -90,
         8,        203,
      -132,        141,
       -65,       -204,
        90,        496,
       243,        495,
       226,       -556,
      -263,        199,
      -640,          0,
      -263,       -199,
       226,        556,
       243,       -495,
        90,       -496,
       -65,        204,
      -132,       -141,
         8,       -203,
         0,         90,
       -25,        177,
       459,       -538,
       655,       -325,
       -90,        163,
      -517,        152,
      -172,        502,
      -218,       -201,
      -384,        128,
       141,        763,
       330,        288,
      -171,        929,
       -90,        269,
       335,       -542,
        33,        232,
      -305,        201,
         0,         90,
       238,       -291,
       151,       -243,
       105,        544,
        90,        121,
       138,        115,
       127,        492,
      -298,        -69,
      -640,          0,
      -298,         69,
       127,       -492,
       138,       -115,
        90,       -121,
       105,       -544,
       151,        243,
       238,        291,
         0,        -90,
      -305,       -201,
        33,       -232,
       335,        542,
       -90,       -269,
      -171,       -929,
       330,       -288,
       141,       -763,
      -384,       -128,
      -218,        201,
      -172,       -502,
      -517,       -152,
       -90,       -163,
       655,        325,
       459,        538,
       -25,       -177,
         0,        -90,
         8,        203,
      -132,        141,
       -65,       -204,
        90,        496,
       243,        495,
       226,       -556,
      -263,        199
}; //}}}

//WARNING: the table below has wrong values
///* Phase rotations e^(-2.Pi.k.n/32) in Q15 format 
// */
//const Uint32 wifilib_minusRot32[32] =
//{
//  /* 0  0       */  0x7FFF0000, /* cos(0) -sin(0) */
//  /* 1  Pi/16   */  0x7D8A98F8,
//  /* 2  Pi/8    */  0x7641B0FB,
//  /* 3  3Pi/16  */  0x6A6DC71C,
//  /* 4  Pi/4    */  0x5A82DA82,
//  /* 5  5Pi/16  */  0x471CEA6D,
//  /* 6  3Pi/8   */  0x30FBF641,
//  /* 7  7Pi/16  */  0x18F8FD8A,
//  /* 8  Pi/2    */  0x0000FFFF,
//  /* 9  9Pi/16  */  0xE708FD8A,
//  /* 10 5Pi/8   */  0xCF05F641,
//  /* 11 11Pi/16 */  0xB8E4EA6D,
//  /* 12 3Pi/4   */  0xA57EDA82,
//  /* 13 13Pi/16 */  0x9593C71C,
//  /* 14 7Pi/8   */  0x89BFB0FB,
//  /* 15 15Pi/16 */  0x827698F8,
//  /* 16 Pi      */  0x80000000, /* cos(Pi) -sin(Pi) */
//  /* 17 17Pi/16 */  0x82766708,
//  /* 18 9Pi/8   */  0x89BF4F05,
//  /* 19 19Pi/16 */  0x959338E4,
//  /* 20 5Pi/4   */  0xA57E257E,
//  /* 21 21Pi/16 */  0xB8E41593,
//  /* 22 11Pi/8  */  0xCF0509BF,
//  /* 23 23Pi/16 */  0xE7080276,
//  /* 24 3Pi/2   */  0x00000000,
//  /* 25 25Pi/16 */  0x18F80276,
//  /* 26 13Pi/8  */  0x30FB09BF,
//  /* 27 27Pi/16 */  0x471C1593,
//  /* 28 7Pi/4   */  0x5A82257E,
//  /* 29 29Pi/16 */  0x6A6D38E4,
//  /* 30 15Pi/8  */  0x76414F05,
//  /* 31 31Pi/16 */  0x7D8A6708  /* cos(31Pi/16) -sin(31Pi/16) */ 
//}; /* End of wxlib_ulSndCoeffRot32[] */


//#pragma DATA_SECTION(wifilib_minusRot, ".derotTables");
//#pragma DATA_SECTION(wifilib_plusRot, ".derotTables");

#pragma DATA_ALIGN(wifilib_minusRot, 8);
#pragma DATA_ALIGN(wifilib_plusRot, 8);
//#pragma DATA_SECTION(wifilib_minusRot, ".data:lookup:wifilib_minusRot")
//#pragma DATA_SECTION(wifilib_plusRot, ".data:lookup:wifilib_plusRot")


Cplx16U far wifilib_minusRot[DEROT_TABLE_SIZE];
//if you want to introduce negative rotation to a sequence of samples

Cplx16U far wifilib_plusRot[DEROT_TABLE_SIZE];
//if you want to introduce positive rotation to a sequence of samples,
//for example, when cfo is negative

void WIFILIB_genTwiddleFactors() {
  gen_twiddle_fft16x16(ORILIB_twiddle_factors_fft16x16_64, 64);
  gen_twiddle_ifft16x16(ORILIB_twiddle_factors_ifft16x16_64, 64);
}


/* */
void WIFILIB_genDerotTable() {
	Uint32 i;
	Uint32 entry1, entry2;
	Uint32 angle;
	Int32 cos, sin;

	/* we are doing Q14 so that cos(0) = sin(2pi) = 1.0 and -1.0's are 
	 * stored correctly upon packing into 16 bits. If we used Q15, they
	 * would overflow */
	for (i = 0; i < DEROT_TABLE_SIZE; i++) {
//		angle = _IQ14div(i, DEROT_TABLE_SIZE);
		angle = _IQ14div(i, DEROT_TABLE_FULL_SIZE);

		//version 1
		cos = _IQ14cosPU(angle);
		sin = _IQ14sinPU(angle);
		entry1 = _pack2(cos, -sin);
		entry2 = _pack2(cos, sin);

		//version 2
//		cos = _sshvl(_IQ15cosPU(angle), 16);
//		sin = _sshvl(_IQ15sinPU(angle), 16);
//		entry1 = _packh2(cos, -sin);
//		entry2 = _packh2(cos, sin);

		wifilib_minusRot[i].realimag = entry1;
		wifilib_plusRot[i].realimag = entry2;
	}
}


void WIFILIB_genSoftSlicingTable64qam() {
//		uint32_t	unnormalizedUnitScale,		
//		/**< a value such that 2^unitScale in 8-bit input I or Q component 
//		corresponds to unity (+1.0) on the *unnormalized* constellation map,
//		i.e., the one without normalization factor applied
//		(thus, you can also think of unit pilot symbols). equivalently,
//		the 8-bit input I or Q is expressed in Q-unitScale format. */
//
//		uint16_t	normalizationFactorQ15,		
//		/**< value of the normalization factor applied to this constellation map.
//		for example, for 64qam in wifi, this value is 1/sqrt(42), which is
//		5056 (Q15). */
//
//		uint8_t	outputRange				
//		/**< number of bits in output scale, that is, if outputRange = 6, output
//		values of soft bits will lie in [-64,63] (so outputRange excludes 
//		the sign bit) */
//
//		//INOUT SoftDemapperWifiState * const state
//		) {

		
	uint32_t	unnormalizedUnitScale           = PHY_SOFT_SLICER_UNNORM_UNIT_SCALE;
	uint16_t	normalizationFactorQ15          = PHY_64QAM_NORMALIZATION_FACTOR_Q15;
	uint8_t		outputRange			= PHY_SOFT_BITS_PRECISION;
	
	int32_t b0val32, b1val32, b2val32;
	
	int32_t inOne, inTwo, inFour, inMax, inMin;
	
	int32_t outShift, b0LeftShift, b1LeftShift, b2LeftShift;
	uint32_t b0Mask, b1Mask, b2Mask;
	
//	int8_t y;
	int32_t y;		//this is important because our loop counter will
				//wrap around and the loop will never terminate if
				//we use int8_t y.
	
	assert(unnormalizedUnitScale <= 6);
	
	b0LeftShift = 16;
	b0Mask = 0xFF0000;
	b1LeftShift = 8;
	b1Mask = 0x00FF00;
	b2LeftShift = 0;
	b2Mask = 0x0000FF;
	

	//uint32_t * const softSlicingTable64qam = state->lut_64qam;
	
	//inOne32 = ((1 << unnormalizedUnitScale) * normalizationFactorQ15) >> 15;
	/* inOne32 refers to the fixed-point representation of a value 1.0 on the
	 * constellation map before it was normalized by the normalization factor
	 * of the constellation. For example, 64QAM has a normalization factor of
	 * sqrt(42), so that a 64QAM point p = 7 - j3 is transformed to
	 * 7/sqrt(42) - j3/sqrt(42) = 1.08 - j0.46. So 1 + j0 will get transformed
	 * to 0.154 + j0, or 1.0 goes to 0.154 in floating-point representation.
	 * Now, if after equalization, the constellation point (free of noise) is
	 * 0.154 floating and its is converted to fixed point by the transformation
	 * fixed-point-value = round or floor of (float-value * 2^6), it means that
	 * 0.154 will be represented as round(9.86) or floor(9.86), say 9. If that
	 * is the case, you should set unnormalizedUnitScale to 6, since unit value
	 * on the unnormalized constellation is transformed to 2^6 in going from
	 * floating to fixed point representation.
	 *
	 * A quick test to check that you are accessing this look up table correctly
	 * is to make sure that the fixed representation of the pilot subcarriers
	 * (assuming noise-free) after channel equalization is
	 * 1 << unnormalizedUnitScale.
	 */

	//Magnify the expected inOne32 by a factor of 1.5. 
	//This is because the equalizer also magnifies
	//the equalized points by a factor of 1.5 before converting to fixed-point.
	//This is so that the fixed-point constellation gets the most precision 
	//possible without distortion in an 8-bit container.
	//Refer to CONSTELLATION_MAGNIFY_FACTOR in ORILIB_ChEstimatorLTF_algorithms.c
	//inOne32 = (inOne32 * 3)>>1;

	//inOne = 0xFF & inOne32;

	//inTwo = inOne << 1;
	//inFour = inOne << 2;


	//Version for better precision of inOne, inTwo, inFour, including magnify 
	//by factor of 1.5
	inOne = ((1 << unnormalizedUnitScale) * normalizationFactorQ15);
	inTwo = inOne * 3;
	inFour = inTwo << 1;
	inOne = inTwo >> 1;			

	inOne >>= 15;
	inTwo >>= 15;
	inFour >>= 15;

	inMax = 127;
	inMin = -128;
	
//	DEBUG(LOG_PRINTF("%d %d 0x%X 0x%X\n", inMax, inMin, inMax, inMin);)

	//For 8-bit signed inputs, soft-bits are 8-bit quantities with 7 bit-magnitude.
	//For outputRange number of bits for output soft-bit magnitude, we must scale
	//down by 7 - outputRange number of bits.

	//outShift = 7 - outputRange + 1;		

	//Comment1: +1 is because the dynamic range of soft estimates is twice that of 
	//the input constellation range (under our choice of soft mapping [1])
	//Comment2: Not sure where Comment1 comes from, so I am disabling the +1. 
	//--MB, 12/15/13

	outShift = 7 - outputRange;	
	
	#pragma MUST_ITERATE(256,256,256);
	for (y = inMin; y <= inMax; y++) {
		uint32_t tVal;
		uint32_t i = (uint8_t)y;
		
//		LOG_PRINTF("%d\n", i);
		b0val32 = y;
		b0val32 = _ext(b0val32, b0LeftShift, outShift);
		
		b1val32 = _abs(y);
		b1val32 = inFour - b1val32;
		b1val32 = _ext(b1val32, b1LeftShift, outShift); 
		
		b2val32 = _abs(y);
		b2val32 = inTwo - _abs(b2val32 - inFour);
		b2val32 = _ext(b2val32, b2LeftShift, outShift);
		
		tVal  = b0Mask & b0val32;
		tVal |= b1Mask & b1val32;
		tVal |= b2Mask & b2val32;
		//_amem4(&softSlicingTable64qam[i]) = tVal;
		_amem4(&WIFILIB_softSlicingTable64qam[i]) = tVal;
	}
	

//	DEBUG(
//	for (y = 0; y < TABLE_SIZE; y++) {
//		if (!(y % 16)) LOG_PRINTF("\n");
//		LOG_PRINTF("0x%08X, ", softSlicingTable64qam[y]);
//	}
//	LOG_PRINTF("\n\n");
//	)
//	
//	DEBUG(
//	for (y = 0; y < TABLE_SIZE; y++) {
//		LOG_PRINTF("%03d  %03d  %03d  %03d\n", y, 
//				(int8_t)(softSlicingTable64qam[y] >> 16), 
//				(int8_t)(softSlicingTable64qam[y] >> 8), 
//				(int8_t)(softSlicingTable64qam[y] & 0xFF));
//	}
//	LOG_PRINTF("\n");
//	)
}

void WIFILIB_genCrc32LookupTable() {
  ORILIB_CRC32_crcCodecTableGenC64(WIFI_CRC32_POLYNOMIAL, WIFILIB_crc32LookupTable);
}

void WIFILIB_genMapperLUTs()
{
    /*Generate Mapper tables*/

    /*BPSK*/
    WIFILIB_mapperLUTbpsk[1] = (_pack2( 1, 0));
    WIFILIB_mapperLUTbpsk[0] = (_pack2(-1, 0));
}

#endif
