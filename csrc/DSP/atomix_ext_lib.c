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
#ifndef DSP

#include <c6x.h>
#include <stdio.h>
#include <stdlib.h>
#include "or_types.h"
#include "ORILIB_k.h"
#include "ORILIB_t.h"
#include "WIFILIB_lookupTables.h"
#include "WIFILIB_util.h"
#include "amem8cpy_inl.h"
#include "inc/vcpdriver/vcpdriver_api.h"
#include "inc/vitc/vitc_api.h"
#include "inc/vitdec/vitdec_profile.h"
#include "inc/vitc/spiralf72.h"

#if SYNC_BUFFER_SIZE_ENERGY != 80
#error: Need SYNC_BUFFER_SIZE_ENERGY to be 80
#endif

#define MOD1 (SYNC_BUFFER_SIZE_ENERGY + SYNC_WINDOW_SIZE_ENERGY)
#define MOD2 (SYNC_BUFFER_SIZE_ENERGY)

#define lsb_OLDEST	1
#define msb_OLDEST	2

typedef struct {
  Uint32 len;
} ORILIB_TailZeroer24_t_State;

#define CRC32_BITS_IN_BYTE_ORDER msb_OLDEST

#define N_BITS_IN_CRC 32
#ifdef DEAL_WITH_ME_LATER

void __ext_ORILIB_CRC32_update (Uint8 InputBytes[restrict], Uint32 NumInputBytes, Uint32 CRCTable[restrict], int __unused_1, Uint32 *arg_CrcValue) {
	Uint8 Byte1, Byte2, Byte3, Byte4;
	Uint32 CrcValue;
	Uint32 InputWord,NumInputWords,WordCnt;
	Uint32 *pInputWords;
	Uint32 NumNonWordBytes;
	CrcValue = *arg_CrcValue;
#if (CRC32_BITS_IN_BYTE_ORDER == lsb_OLDEST)
	CrcValue = reverseBitsInBytes32(CrcValue);
#endif
	CrcValue ^= 0xFFFFFFFF;
	pInputWords = ( Uint32 * ) &InputBytes[0];
	NumInputWords = NumInputBytes >> 2;
	NumNonWordBytes = NumInputBytes & 3;;
#ifdef _TMS320C6X
#endif


	for( WordCnt = 0; WordCnt < NumInputWords; WordCnt++ ) {
#if (CRC32_BITS_IN_BYTE_ORDER == lsb_OLDEST)
		InputWord = reverseBitsInBytes32(*pInputWords++);
#else
		InputWord = *pInputWords++;
#endif
		Byte1  = _extu(InputWord, 0, 24);
		CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte1 ^ _extu(CrcValue, 0, 24) ];
		Byte2  = _extu(InputWord, 8, 24);
		CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte2 ^ _extu(CrcValue, 0, 24) ];
		Byte3  = _extu(InputWord, 16, 24);
		CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte3 ^ _extu(CrcValue, 0, 24) ];
		Byte4  = _extu(InputWord, 24, 24);
		CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte4 ^ _extu(CrcValue, 0, 24) ];
	}
	if ( NumNonWordBytes )	{
#if (CRC32_BITS_IN_BYTE_ORDER == lsb_OLDEST)
		InputWord = reverseBitsInBytes32(*pInputWords++);
#else
		InputWord = *pInputWords++;
#endif
		Byte1  = _extu(InputWord, 0, 24);
		Byte2  = _extu(InputWord, 8, 24);
		Byte3  = _extu(InputWord, 16, 24);
		CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte1 ^ _extu(CrcValue, 0, 24) ];
		if ( NumNonWordBytes >= 2 )	{
			CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte2 ^ _extu(CrcValue, 0, 24) ];
		}
		if ( NumNonWordBytes == 3 )	{
			CrcValue   = ( CrcValue << 8 ) ^ CRCTable[ Byte3 ^ _extu(CrcValue, 0, 24) ];
		}
	}
#if (N_BITS_IN_CRC < 32)
	CrcValue >>= (32 - N_BITS_IN_CRC);
#endif
	CrcValue ^= 0xFFFFFFFF;
#if (CRC32_BITS_IN_BYTE_ORDER == lsb_OLDEST)
	CrcValue = reverseBitsInBytes32(CrcValue);
#endif
	*arg_CrcValue = CrcValue;
}

void __ext_ORILIB_CRC32_crcCodecTableGenC64(Uint32 CrcPoly, Uint32* CrcTablePtr, int __unused_1) {
	Uint32 CrcByte;
	Uint32 Byte;
	Uint8  Bit;
	/* Compute the CRC of each possible byte.  */
	for ( Byte = 0; Byte < 256; Byte++ )
	{
		CrcByte = Byte << 24;
		/* Perform modulo-2 division, a bit at a time. */
		for ( Bit = 8; Bit > 0; --Bit )
		{
			/* Divide the current data bit. */
			if ( CrcByte & 0x80000000 )
				CrcByte = ( CrcByte << 1 ) ^ CrcPoly;
			else
				CrcByte = ( CrcByte << 1 );
		}

		/* Store next table entry. */
		CrcTablePtr[Byte] = CrcByte;
	}
	return;
}

void __ext_WIFILIB_fftShift64(
	Cplx16 const * const restrict symbol_with_DC_at_idx_0,
	Cplx16 * const restrict symbol_with_DC_in_middle
	){



	Uint32 i;

	{
		Cplx16 * const restrict dst = symbol_with_DC_in_middle;
		Cplx16 const * const restrict src = symbol_with_DC_at_idx_0 + 38;
#define TRIPCOUNT1  13
#pragma MUST_ITERATE(TRIPCOUNT1, TRIPCOUNT1, TRIPCOUNT1);
		for (i = 0; i < TRIPCOUNT1; i++) {
			_amem8(&((Uint64 *)dst)[i]) = _amem8(&((Uint64 *)src)[i]);
		}
	}

	{
		Cplx16 * const restrict dst = symbol_with_DC_in_middle + 26;
		Cplx16 const * const restrict src = symbol_with_DC_at_idx_0 + 1;
#define TRIPCOUNT2  26
#pragma MUST_ITERATE(TRIPCOUNT2, TRIPCOUNT2, TRIPCOUNT2);
		for (i = 0; i < TRIPCOUNT2; i++) {
			_amem4(&((Uint32 *)dst)[i]) = _amem4(&((Uint32 *)src)[i]);
		}
	}
}

void __ext_ORILIB_TailZeroer24 (
	  Uint8 inp_packedBits[8],
	  Uint32 __unused1__,
	  Uint32 inpState_len,
	  Uint8 out_packedBits[8],
	  Uint32 __unused2__,
	){

	// tail is 6 bits after the end of the frame
	ASSERT(__unused1__ == 8 && __unused2__ == 8);
	Uint32 tail_start_idx = inpState_len % 24;

	_amem4(out_packedBits) = _amem4_const(inp_packedBits);
	_amem4(out_packedBits) = _clr(
		_amem4_const(out_packedBits),
		31-6-tail_start_idx,
		31-tail_start_idx
	);
}

void __ext_ORILIB_AckPacker(
			Uint8* mac,
			Uint32 mac_len,
			Uint8* ack,
			Uint32 ack_len,
			ORILIB_TailZeroer24_t_State * outTailZState,
			Uint32 __unused1__
            ){
	ASSERT(mac_len == 8 && ack_len == 8);
	Uint32 crc = 0;
	Uint32 torev = 0;
	int i = 0;

	// Clear out the ACK template
	memset(ack, 0, 24);

	// SERVICE (starts at 2 so the rest is 4 byte aligned for CRC.
	// must skip 2 bytes in bitcache!)
	// leave as 0, scrambler will fill it

	// Frame control
	ack[4] = 0xD4;    // type/subtype

	// Duration (leave as 0)

	// RA
	_amem8cpy(ack+8, mac, 1);

	// Reverse the bits for the air (before CRC because the CRC algo assumes bits are in air order)
	#pragma MUST_ITERATE(5)
	for (i = 0; i < 5; i++) {
		_amem4(((Uint32*)ack)+i) = reverseBitsInBytes32(_amem4_const(((Uint32*)ack)+i));
	}

	// CRC (everything but the SERVICE field)
	__ext_ORILIB_CRC32_update(ack+4, 10, WIFILIB_crc32LookupTable, 0, &crc);
	_amem2(ack+14) = _amem2_const(((Uint16*)&crc));
	_amem2(ack+16) = _amem2_const(((Uint16*)&crc)+1);;

	outTailZState->len = 128; // bits including service field and ending with CRC
}

static void _AutoGain_Cont_inner(
		 		Cplx16 const freshSampleBuf[restrict],
				Uint32 windowEnergyAgcBuf[restrict],
				Uint32 zwinSampleEnergyTermBuf_withGain[restrict],
				Uint32 zwinPositionE2,
				Uint32 *currWindowEnergyE2_withGain,
				Uint32 positionE2,
				Cplx16 raw_samples_with_gain[restrict],
				Uint32 currAgcGainBuf[restrict],
				Uint32 *currAgcGain,
				Uint32 windowEnergyE1andE2Buf_withGain[restrict]
		) {


    //Uint32 zwinSampleEnergyE2_withGain;
    Uint32 z1;//, z2;
    Uint64 z1z2, z1z12;

    //Uint32 currSampleEnergy_withGain;
    Uint32 e1, e2;
    Uint64 e1e2, e1e12;

    //Uint32 lo_currWindowEnergyE2_withGain;
    Uint32 w0;
    Uint64 w1w2;

    //Cplx16U currSample;
    Uint64 xo1xo2;
    Cplx16U xo1, xo2;

    //Cplx16U currSample_withGain;
    Cplx16U x1, x2;
    Uint64 x1x2;

    Int32 g = 0; //lo_currAgcGain, lo_newAgcGain;
    Int32 n1 = 0, n2 = 0;//, currAvgSampleNorm;
    Int64 z64 = 0;

    //Uint32 lo_currWindowEnergyAgc;
    Uint64 wo1wo2;

    Uint32 lo_zwinPositionE2, lo_positionE2;

    Uint32 ii;

    //lo_currWindowEnergyE2_withGain = _amem4(currWindowEnergyE2_withGain);
    w0 = _amem4(currWindowEnergyE2_withGain);
    w1w2 = _itoll(w0, w0);

    //lo_currAgcGain = *currAgcGain;
    g = *currAgcGain;


    lo_zwinPositionE2 = zwinPositionE2;
    lo_positionE2 = positionE2;

//#pragma MUST_ITERATE(SYNC_BUFFER_SIZE_ENERGY,SYNC_BUFFER_SIZE_ENERGY, SYNC_BUFFER_SIZE_ENERGY);
#pragma MUST_ITERATE(SYNC_BUFFER_SIZE_ENERGY/2,SYNC_BUFFER_SIZE_ENERGY/2, SYNC_BUFFER_SIZE_ENERGY/2);
    //for ( ii = 0; ii < SYNC_BUFFER_SIZE_ENERGY; ii++ ) {
    for ( ii = 0; ii < SYNC_BUFFER_SIZE_ENERGY; ii+=2) {
	//currSample.realimag = _amem4_const(&freshSampleBuf[ii]);
	//Q15 (16bit signed samples in (-1.0,1.0)
	//lo_currWindowEnergyAgc =_amem4(&windowEnergyAgcBuf[ii]);
	//zwinSampleEnergyE2_withGain = _amem4(&zwinSampleEnergyTermBuf_withGain[lo_zwinPositionE2]);
	xo1xo2 = _amem8_const(&freshSampleBuf[ii]);
	wo1wo2 = _amem8_const(&windowEnergyAgcBuf[ii]);
	z1z2 = _amem8(&zwinSampleEnergyTermBuf_withGain[lo_zwinPositionE2]);

#ifdef NO_AGC
	//lo_newAgcGain = 0;
	//lo_currAgcGain = 0;
	//g = 0;
	//currSample_withGain.realimag = currSample.realimag;
	x1x2 = xo1xo2;

	//_amem4(&currAgcGainBuf[ii]) = lo_currAgcGain;
	_amem8(&currAgcGainBuf[ii]) = z64;
#else
	//currAvgSampleNorm = (_norm(lo_currWindowEnergyAgc) >> 1) + SYNC_WINDOW_SIZE_AGC_N_BITS/2;
	n1 = (_norm(_hill(wo1wo2)) >> 1) + SYNC_WINDOW_SIZE_AGC_N_BITS/2;
	n2 = (_norm(_loll(wo1wo2)) >> 1) + SYNC_WINDOW_SIZE_AGC_N_BITS/2;

	//lo_newAgcGain = currAvgSampleNorm - SYNC_AGC_GAIN_SAFETY_MARGIN;
	n1 -= SYNC_AGC_GAIN_SAFETY_MARGIN;
	n2 -= SYNC_AGC_GAIN_SAFETY_MARGIN;

	//lo_currAgcGain = _subabs4(lo_newAgcGain, lo_currAgcGain) >= SYNC_AGC_GAIN_RELOCK_THRESHOLD ?
	//			lo_newAgcGain : lo_currAgcGain;
	//WIFILIB_util_applyGain(&currSample, &currSample_withGain, lo_currAgcGain);
	//_amem4(&currAgcGainBuf[ii]) = lo_currAgcGain;
	//THE FOLLOWING HAS NOT BEEN TESTED!!!
	g = _subabs4(n1, g) >= SYNC_AGC_GAIN_RELOCK_THRESHOLD ?  n1 : g;
	WIFILIB_util_applyGain(&_hill(xo1xo2), &_hill(x1x2), g);
	_amem4(&currAgcGainBuf[ii]) = g;

	g = _subabs4(n2, g) >= SYNC_AGC_GAIN_RELOCK_THRESHOLD ?  n2 : g;
	WIFILIB_util_applyGain(&_loll(xo1xo2), &_loll(x1x2), g);
	_amem4(&currAgcGainBuf[ii + 1]) = g;
#endif

	//currSampleEnergy_withGain = _dotp2(currSample_withGain.realimag, currSample_withGain.realimag);
	x1.realimag = _hill(x1x2);
	x2.realimag = _loll(x1x2);
	e1 = _dotp2(x1.realimag, x1.realimag);
	e2 = _dotp2(x2.realimag, x2.realimag);
	e1e2 = _itoll(e1, e2);

	//currSampleEnergy_withGain >>= SYNC_WINDOW_SIZE_ENERGY__ACCUMULATION_RSHIFT;
	e1e2 = _dshru(e1e2, SYNC_WINDOW_SIZE_ENERGY__ACCUMULATION_RSHIFT);
	e1 = _hill(e1e2);

	//lo_currWindowEnergyE2_withGain -= zwinSampleEnergyE2_withGain;
	//lo_currWindowEnergyE2_withGain += currSampleEnergy_withGain;
	z1 = _hill(z1z2);
	z1z12 = _dadd(z1z2, _itoll(0, z1));
	e1e12 = _dadd(e1e2, _itoll(0, e1));
	w1w2 = _dsub(w1w2, z1z12);
	w1w2 = _dadd(w1w2, e1e12);

	//_amem4(&raw_samples_with_gain[ii]) = currSample_withGain.realimag;
	_amem8(&raw_samples_with_gain[ii]) = x1x2;

	//_amem4(&windowEnergyE1andE2Buf_withGain[lo_positionE2]) = lo_currWindowEnergyE2_withGain;
	_amem8(&windowEnergyE1andE2Buf_withGain[lo_positionE2]) = w1w2;

	//_amem4(&zwinSampleEnergyTermBuf_withGain[lo_zwinPositionE2]) = currSampleEnergy_withGain;
	_amem8(&zwinSampleEnergyTermBuf_withGain[lo_zwinPositionE2]) = e1e2;

	//lo_positionE2 = (lo_positionE2 + 1) % (SYNC_BUFFER_SIZE_ENERGY + SYNC_WINDOW_SIZE_ENERGY);
	//lo_zwinPositionE2 = (lo_zwinPositionE2 + 1) % SYNC_WINDOW_SIZE_ENERGY;

	//lo_positionE2++; lo_zwinPositionE2++;
	lo_positionE2 += 2; lo_zwinPositionE2 += 2;
	lo_positionE2 = lo_positionE2 < MOD1 ? lo_positionE2 : (lo_positionE2 - MOD1);
	lo_zwinPositionE2 = lo_zwinPositionE2 < MOD2 ? lo_zwinPositionE2 : (lo_zwinPositionE2 - MOD2);

	w0 = _loll(w1w2);
	w1w2 = _itoll(w0, w0);

    }
    //_amem4(currWindowEnergyE2_withGain) = lo_currWindowEnergyE2_withGain;
    //*currAgcGain = lo_currAgcGain;
    _amem4(currWindowEnergyE2_withGain) = w0;
    *currAgcGain = g;

}

void __ext_ORILIB_AutoGainController_i (
		ComplexSamples const * const restrict raw_samples,
		Uint32 __unused1__,
		Uint32 windowEnergyBuf[SYNC_BUFFER_SIZE_ENERGY],
		Uint32 __unused2__,
		Uint32 zwinSampleEnergyTermBuf_withGain[SYNC_WINDOW_SIZE_ENERGY],
		Uint32 __unused3__,
		Uint32* zwinPositionE2,
		Uint32 __unused6__,
		Uint32* currWindowEnergyE2_withGain,
		Uint32 __unused4__,
		Uint32* positionE2,
		Uint32 __unused7__,
		Uint32 currAgcGainBuf[SYNC_BUFFER_SIZE_ENERGY],
		Uint32 __unused8__,
		Uint32* currAgcGain,
		Uint32 __unused9__,
		Uint32 windowEnergyE1andE2Buf_withGain[SYNC_BUFFER_SIZE_ENERGY + SYNC_WINDOW_SIZE_ENERGY],
		Uint32 __unused10__,
		ComplexSamples * const restrict raw_samples_with_gain,
		Uint32 __unused5__

	){
    _AutoGain_Cont_inner(
    		 	raw_samples,
			//new buffer of samples, length SYNC_BUFFER_SIZE_ENERGY

    		 	windowEnergyBuf,


    			zwinSampleEnergyTermBuf_withGain,

    			*zwinPositionE2,
			//next position of the zwinSampleEnergyTerm buffer to be used

    			currWindowEnergyE2_withGain,


    			*positionE2,

    			raw_samples_with_gain,

    		//-- the following is for debug only --
    			currAgcGainBuf,
			//buffer to hold norms of energy values - used for gain control

    			currAgcGain,

    			windowEnergyE1andE2Buf_withGain
			//buffer to hold window energy values, length = SYNC_BUFFER_SIZE_ENERGY
    		);

    *positionE2 =
    	(*positionE2 + SYNC_BUFFER_SIZE_ENERGY) % (SYNC_BUFFER_SIZE_ENERGY + SYNC_WINDOW_SIZE_ENERGY);
    *zwinPositionE2 =
    	(*zwinPositionE2 + SYNC_BUFFER_SIZE_ENERGY) % SYNC_WINDOW_SIZE_ENERGY;
}
#endif




#endif
