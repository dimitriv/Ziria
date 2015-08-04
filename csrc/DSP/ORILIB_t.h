/**
Atomix project, ORILIB_t.h, TODO: insert summary here
Copyright (c) 2015 Stanford University
Released under the Apache License v2.0. See the LICENSE file for details.
Author(s): Manu Bansal
*/

#ifndef __ORILIB_T_H__
#define __ORILIB_T_H__


#include "ORILIB_k.h"

extern const Uint8 NODE_ID;

typedef struct {
  Cplx16 samples[4]  __attribute__((aligned(8)));
} ORILIB_t_Cplx16Buf4;

typedef struct {
  Cplx16 samples[32]  __attribute__((aligned(8)));
} ORILIB_t_Cplx16Buf32;

typedef struct {
  Cplx16 samples[48]  __attribute__((aligned(8)));
} ORILIB_t_Cplx16Buf48;

typedef struct {
  Cplx32 samples[48]  __attribute__((aligned(8)));
} ORILIB_t_Cplx32Buf48;

typedef struct {
  Cplx16 samples[80]  __attribute__((aligned(8)));
} ORILIB_t_Cplx16Buf80;

typedef struct {
  Cplx16 samples[96]  __attribute__((aligned(8)));
} ORILIB_t_Cplx16Buf96;

typedef struct {
  Cplx16 samples[160]  __attribute__((aligned(8)));
} ORILIB_t_Cplx16Buf160;

typedef struct {
  Uint32 agcGain	__attribute__((aligned(8)));
} ORILIB_t_PktAgcState;

typedef struct {
  Uint32 phiU;
  Uint32 sign_cfo;
  Uint32 derot_table_stride;
  Cplx16U *derotTablePtr;
} ORILIB_t_CFOState;

extern const int8_t ORILIB_pilot_polarity_sequence[127];
extern Int16 ORILIB_twiddle_factors_fft16x16_64[128];

typedef struct {
  uint8_t pilot_polarity_counter;
} ORILIB_OFDMDemodulator_t_State;


extern const Cplx16 ORILIB_expected_ltf_symbols[52];
extern const Cplx16 ORILIB_reference_ltf_d_signs[52];

typedef struct {
  Cplx16 avgChannelGains[48];
  Cplx16 avgChannelGainsConj[48];
  Uint32 hhstar[48];

  float recip_hhstarF[48];

  Cplx16 avgChannelGains_Pilots[4];
  Cplx16 avgChannelGainsConj_Pilots[4];
  Uint32 hhstar_Pilots[4];

  float recip_hhstarF_Pilots[4];

} ORILIB_t_ChEstimatorLTF_Float_State;

typedef struct {
  Cplx16 avgChannelGains[48] __attribute__((aligned(8)));
  Cplx16 avgChannelGainsConj[48] __attribute__((aligned(8)));
  Uint32 hhstar[48] __attribute__((aligned(8)));

  Uint16 recip_hhstar_frac[48] __attribute__((aligned(8)));
  Int16 recip_hhstar_exp[48] __attribute__((aligned(8)));
  float recip_hhstarF[48] __attribute__((aligned(8)));

  Cplx16 avgChannelGains_Pilots[4] __attribute__((aligned(8)));
  Cplx16 avgChannelGainsConj_Pilots[4] __attribute__((aligned(8)));
  Uint32 hhstar_Pilots[4] __attribute__((aligned(8)));

  Uint16 recip_hhstar_frac_Pilots[4] __attribute__((aligned(8)));
  Int16 recip_hhstar_exp_Pilots[4] __attribute__((aligned(8)));
  float recip_hhstarF_Pilots[4] __attribute__((aligned(8)));

} ORILIB_t_ChEstimatorLTF_State;

typedef struct {
  Uint32 yhstar_norm_min;
} ORILIB_t_OFDMEqualizer_State;

typedef struct 	__attribute__((__packed__)) {
  Int8 softBits[48];
} ORILIB_t_SoftBitBuf48;

//typedef struct 	__attribute__((__packed__)) {
//  Int8 softBits[60];
//} ORILIB_t_SoftBitBuf60;

typedef struct 	__attribute__((__packed__)) {
  Int8 softBits[120];
} ORILIB_t_SoftBitBuf120;

typedef struct 	__attribute__((__packed__)) {
  Int8 softBits[288];
} ORILIB_t_SoftBitBuf288;

typedef struct 	__attribute__((__packed__)) {
  Int8 softBits[432];
} ORILIB_t_SoftBitBuf432;

typedef struct 	__attribute__((__packed__)) {
  Int8 softBits[800];
} ORILIB_t_SoftBitBuf800;

typedef struct __attribute__((__packed__)) {
  Uint8 bytes[4];
} ORILIB_t_ByteBuf4;

typedef struct __attribute__((__packed__)) {
  Uint8 bytes[8];
} ORILIB_t_ByteBuf8;

typedef struct __attribute__((__packed__)) {
  Uint8 bytes[48];
} ORILIB_t_ByteBuf48;

typedef struct {	
  Uint32 data_rate_4bits; 
  Uint32 packet_length_12bits;
  Uint32 nOfdmSymsInPkt;
} ORILIB_t_PLCPParserState;

//--------

typedef struct {
  Int8 bytes[120];
  Uint32 validLenInBytes;		//length of valid data (with circular wraparound)
} ORILIB_t_ByteCache120;

typedef struct {
  Uint32 nBytes;
} ORILIB_t_ByteCache120_Conf;

typedef struct {
  Int8 bytes[200];
  Uint32 validLenInBytes;		//length of valid data (with circular wraparound)
} ORILIB_t_ByteCache200;

typedef struct {
  Int8 bytes[2048];
  Uint32 validLenInBytes;		//length of valid data (with circular wraparound)
} ORILIB_t_ByteCache2048;

typedef struct {
  Int8 bytes[432];
  Uint32 validLenInBytes;		//length of valid data (with circular wraparound)
} ORILIB_t_ByteCache432;

typedef struct {
  Int8 bytes[432];
  Uint32 validLenInBytes;		//length of valid data (with circular wraparound)
  Uint32 totalConsumeCapInBytes;
  Uint32 totalConsumedInBytes;
} ORILIB_t_ByteCacheCapped432;

typedef struct {
  Int8 bytes[2048];
  Uint32 validLenInBytes;		//length of valid data (with circular wraparound)
  Uint32 totalConsumeCapInBytes;
  Uint32 totalConsumedInBytes;
} ORILIB_t_ByteCacheCapped2048;

//#typedef struct {
//#  Uint32 totalConsumeCapInBytes;
//#  Uint32 totalConsumedInBytes;
//#} ORILIB_t_ByteCacheCappedState;

typedef struct {
  Uint32 outFrameSizeInBytes;
  Uint32 outBufCapacityInBytes;
  Uint32 consumeSizeInBytes;
  Uint32 padSizeInWords;
  Uint32 padWord;
} ORILIB_t_ByteCacheGetFrameConf;

typedef struct {
  Uint32 copySizeInBytes;
} ORILIB_t_ByteCache_Conf;

typedef enum ORILIB_e_DataDecodeMode {
  DATA_DECODE_STATE_INIT,
  DATA_DECODE_STATE_CACHING_SYMS,
  DATA_DECODE_STATE_HEADTAIL_DECODING,
  DATA_DECODE_STATE_HEAD_DECODING,
  DATA_DECODE_STATE_MID_PRE_DECODING,
  DATA_DECODE_STATE_MID_DECODING,
  DATA_DECODE_STATE_TAIL_DECODING,
  DATA_DECODE_STATE_TAILfHEAD_DECODING,
  DATA_DECODE_STATE_TAIL_FINISH,
  DATA_DECODE_STATE_TAILfHEAD_FINISH,
  DATA_DECODE_STATE_HEAD_DECODING_ONESYMRX,
  DATA_DECODE_STATE_TAILfHEAD_DECODING_ONESYMRX,
  DATA_DECODE_STATE_TAILfHEAD_FINISH_ONESYMRX
} ORILIB_t_DataDecodeMode;

typedef struct {
  ORILIB_t_DataDecodeMode dataDecodeMode;
} ORILIB_t_DataDecodeState;

typedef struct {
  Uint32 transitionMap[4];
} ORILIB_t_PLCPParserConf;

typedef struct {
  Uint32 transitionMap[12];
} ORILIB_t_DataDecodeDecisionConf;

typedef struct {
  Uint32 transitionMap[2];
} ORILIB_t_CRC32_DecisionConf;


typedef struct __attribute__((__packed__)) {
  Uint8 packedBits[8];
} ORILIB_t_BitBucket24;

typedef struct __attribute__((__packed__)) {
  Uint8 packedBits[8];
} ORILIB_t_BitBucket64;

typedef struct __attribute__((__packed__)) {
  Uint8 packedBits[32]  __attribute__((aligned(8)));
} ORILIB_t_BitBucket256;

typedef struct __attribute__((__packed__)) {
  Uint8 packedBits[36]  __attribute__((aligned(8)));
} ORILIB_t_BitBucket288;

typedef struct __attribute__((__packed__)) {
  Uint8 packedBits[40]  __attribute__((aligned(8)));
} ORILIB_t_BitBucket320;		//320 is the smallest multiple of 64 larger than 288

typedef struct __attribute__((__packed__)) {
  Uint8 packedBits[64];
} ORILIB_t_BitBucket512;

typedef struct {
  Uint32 n;
} ORILIB_DebugPrintBitBucketConf;

typedef struct {
  Int8 bits[128];
  Uint32 validLenInBits;
} ORILIB_t_BitCache1024;

typedef struct {
  Int8 bits[128];
  Uint32 validLenInBits;
  Uint32 validBitsOffset;
} ORILIB_t_BitCache1024_circular;

typedef struct {
  Uint32 inpBufSizeInBits;
  Uint32 inpCopySizeInBits;
  Uint32 inpSkipSizeInBits;
} ORILIB_t_BitCacheSkipPutConf;

typedef struct {
  Uint32 outBufSizeInBits;
  Uint32 copySizeInBits;
} ORILIB_t_BitCacheGetConf;

typedef struct {
  Uint32 frameLength;
  Uint32 convergenceLength;
} ORILIB_t_ViterbiDecodingMapConf;


typedef struct {
  Uint32 crcValue;
  Uint32 nBytes;		//#bytes on which to update crc; used in the CRC32_VarBytes_i block
} ORILIB_t_CRC32_State;

typedef struct {
  Uint32 nBytes;		//#bytes on which to update crc; used in the CRC32_i block
} ORILIB_t_CRC32_Conf;		

typedef struct {
  Uint32 nDataBytesPerOfdmSymbol;
} ORILIB_t_CRC32_VarBytes_Conf;		

typedef struct {
  double softrate_ber;
} ORILIB_t_Softrate_BER;

typedef struct {
  double softrate_metric;
  uint32_t ofdm_count;
} ORILIB_t_Softrate_State;


typedef struct {
	Uint32 zwinSampleEnergyTermBuf[SYNC_WINDOW_SIZE_ENERGY]	__attribute__((aligned(8)));		
	//this is a window of trailing sample energy values, which

	Uint32 zwinPositionE2					__attribute__((aligned(8)));					
	//next position of the zwinSampleEnergyTerm buffer to be used

	Uint32 currWindowEnergyE2				__attribute__((aligned(8)));				
	//as input, energy of the window ending at the last sample
	
	Uint32 positionE2					__attribute__((aligned(8)));

	Uint32 windowEnergyE1andE2Buf[SYNC_BUFFER_SIZE_ENERGY + SYNC_WINDOW_SIZE_ENERGY]	
								__attribute__((aligned(8)));
	//buffer to hold window energy values;

	//we make length = SYNC_BUFFER_SIZE_ENERGY  + SYNC_BUFFER_SIZE_ENERGY
	//so that the window retains E1 values too; in order to use this
	//mechanism, we need to keep the index where E2 starts, and from it, we
	//can also infer where E1 starts.
	//[...E1 window for s0...][...E2 window for s0...][s0...current samples...]

} ORILIB_t_EnergyState;

typedef struct {
      Uint32 zwinPositionE2					__attribute__((aligned(8)));
      Uint32 currWindowEnergy			 		__attribute__((aligned(8)));
      Uint32 zwinSampleEnergyTermBuf[SYNC_WINDOW_SIZE_ENERGY]	__attribute__((aligned(8)));
      Uint32 windowEnergyBuf[SYNC_BUFFER_SIZE_ENERGY] 		__attribute__((aligned(8)));
} ORILIB_t_SimpleEnergyState;


typedef struct {
      Uint32 	positionE2					__attribute__((aligned(8)));
      Uint32 	zwinPositionE2					__attribute__((aligned(8)));
      INOUT	Uint32 currWindowEnergyE2_withGain		__attribute__((aligned(8)));
      INOUT 	Uint32 zwinSampleEnergyTermBuf_withGain[SYNC_WINDOW_SIZE_ENERGY]	
							      __attribute__((aligned(8)));
      OUT	Uint32 currAgcGainBuf[SYNC_BUFFER_SIZE_ENERGY]	__attribute__((aligned(8)));
} ORILIB_t_GainState;

typedef struct {
	Uint32 windowEnergyE1andE2Buf_withGain[
	  SYNC_BUFFER_SIZE_ENERGY + SYNC_WINDOW_SIZE_ENERGY]
	  __attribute__((aligned(8)));		

	Uint32 currAgcGain				__attribute__((aligned(8)));
} ORILIB_t_GainStateAux;

typedef struct {
	INOUT	Uint32 corr_positionNextE2Value 				__attribute__((aligned(8)));
	INOUT	Uint32 corr_zwinPosition					__attribute__((aligned(8)));
	INOUT	Cplx16 corr_zwinCorrTermBuf[SYNC_WINDOW_SIZE_ENERGY]		__attribute__((aligned(8)));
	INOUT	Cplx16 corr_zwinSampleBuf[SYNC_WINDOW_SIZE_ENERGY]		__attribute__((aligned(8)));
	INOUT	Cplx16 corr_currWinCorr						__attribute__((aligned(8)));

	INOUT	Uint32 corr_maxMetricRunLength					__attribute__((aligned(8)));
	INOUT	Uint32 corr_maxMetricIsHigh					__attribute__((aligned(8)));
	INOUT	Uint32 corr_maxMetricBufIdx					__attribute__((aligned(8)));
	INOUT	Uint32 corr_maxMetric						__attribute__((aligned(8)));

	INOUT	Uint32 corr_peakFound						__attribute__((aligned(8)));

	//OUT	Uint32 debug_isMetricHighBuf[SYNC_BUFFER_SIZE_ENERGY]		__attribute__((aligned(8)));
	OUT	Uint64 debug_magNumerBuf_64[SYNC_BUFFER_SIZE_ENERGY]		__attribute__((aligned(8)));
	OUT	Uint64 debug_magDenScBuf_64[SYNC_BUFFER_SIZE_ENERGY]		__attribute__((aligned(8)));
	OUT	Uint32 debug_magNumerBuf[SYNC_BUFFER_SIZE_ENERGY]		__attribute__((aligned(8)));
	OUT	Uint32 debug_magDenomBuf[SYNC_BUFFER_SIZE_ENERGY]		__attribute__((aligned(8)));
	//OUT	float  debug_metricFBuf[SYNC_BUFFER_SIZE_ENERGY]		__attribute__((aligned(8)));
	INOUT	Uint32 debug_metricBuf[SYNC_BUFFER_SIZE_ENERGY]			__attribute__((aligned(8)));
} ORILIB_t_CorrState;

typedef struct {
	Cplx16	winCorrBuf[SYNC_BUFFER_SIZE_ENERGY]				__attribute__((aligned(8)));
} ORILIB_t_CorrStateAux;

//typedef struct {
//	Cplx16 alignedSampleLookbackBuf[SYNC_ALIGNED_SAMPLE_BUF_LEN_ACTUAL]	__attribute__((aligned(8)));
//	Uint32 nAlignedSamplesAlreadyFilled					__attribute__((aligned(4)));
//	Uint32 nAlignedSamplesAvailable						__attribute__((aligned(4)));
//	Uint32 uaks1								__attribute__((aligned(4)));
//	Uint32 uaks2								__attribute__((aligned(4)));
//} ORILIB_t_AlignState;

typedef struct {
	Cplx16 alignedSampleLookbackBuf[SYNC_ALIGNED_SAMPLE_LOOKBACK_WINDOW_SIZE] __attribute__((aligned(8)));
	Uint32 nAlignedSamplesAlreadyFilled					__attribute__((aligned(4)));
	Uint32 nAlignedSamplesAvailable						__attribute__((aligned(4)));
	Uint32 uaks1								__attribute__((aligned(4)));
	Uint32 uaks2								__attribute__((aligned(4)));
//} ORILIB_t_beta_AlignState;
} ORILIB_t_AlignState;


typedef struct {
	OUT	Uint32 peakFound						__attribute__((aligned(8)));
	OUT	Uint32 agcGain							__attribute__((aligned(8)));
} ORILIB_t_DetectState; 

typedef struct {
	Uint32 byteOffset;
} ORILIB_t_Offset1;

typedef struct {
	Uint32 byteOffset;
	Uint32 nSamplesToSkip;
} ORILIB_t_beta_Offset1;


typedef struct {
	Uint32	inpByteOffset;
	Uint32	outByteOffset;
} ORILIB_t_Offset2;


#endif //__ORILIB_T_H__
