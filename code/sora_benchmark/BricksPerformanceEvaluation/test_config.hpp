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



//#define RX_FULL
//#define RX_FULL_PIPE
#define TX_FULL
//#define TX_FULL_PIPE
//#define RX_BLOCKS
//#define TX_BLOCKS



#include "ieee80211facade.hpp"
#include "depuncturer.hpp"
#include <soratime.h>
#include "mapper11a.hpp"
#include "interleave.hpp"
#include "conv_enc.hpp"
//#include "stdbrick.hpp"



#ifdef TX_FULL
static FINL ISource* CreateTestModGraph11a_6();
static FINL ISource* CreateTestModGraph11a_9();
static FINL ISource* CreateTestModGraph11a_12();
static FINL ISource* CreateTestModGraph11a_18();
static FINL ISource* CreateTestModGraph11a_24();
static FINL ISource* CreateTestModGraph11a_36();
static FINL ISource* CreateTestModGraph11a_48();
static FINL ISource* CreateTestModGraph11a_54();
#endif

#ifdef RX_FULL
static FINL ISource* CreateDemodGraph11a_6 ();
static FINL ISource* CreateDemodGraph11a_12 ();
static FINL ISource* CreateDemodGraph11a_24 ();
static FINL ISource* CreateDemodGraph11a_48 ();
static FINL ISource* CreateCCAGraph11a ();
#endif

#ifdef TX_FULL_PIPE
static FINL ISource* CreateTestModGraph11a_6_p();
static FINL ISource* CreateTestModGraph11a_9_p();
static FINL ISource* CreateTestModGraph11a_12_p();
static FINL ISource* CreateTestModGraph11a_18_p();
static FINL ISource* CreateTestModGraph11a_24_p();
static FINL ISource* CreateTestModGraph11a_36_p();
static FINL ISource* CreateTestModGraph11a_48_p();
static FINL ISource* CreateTestModGraph11a_54_p();
#endif

#ifdef RX_FULL_PIPE
static FINL ISource* CreateDemodGraph11a_6_p(ISource*& svit);
static FINL ISource* CreateDemodGraph11a_12_p(ISource*& svit);
static FINL ISource* CreateDemodGraph11a_24_p(ISource*& svit);
static FINL ISource* CreateDemodGraph11a_48_p(ISource*& svit);
//static FINL ISource* CreateCCAGraph11a();
#endif



typedef struct _tagBrickTestContext :
	  LOCAL_CONTEXT(TDropAny)
	, LOCAL_CONTEXT(TDownSample2)
    , LOCAL_CONTEXT(PerfEvalSource)        
	, LOCAL_CONTEXT(TDCRemove)  
    , LOCAL_CONTEXT(T11aLTS)        
	, LOCAL_CONTEXT(TCCA11a)  
	, LOCAL_CONTEXT(T11aDataSymbol)
  	, LOCAL_CONTEXT(T11aViterbi)									  
	, LOCAL_CONTEXT(T11aPLCPParser)	  		  		  		  		  	
	, LOCAL_CONTEXT(T11aDemap)	  		  		  		  	
	, LOCAL_CONTEXT(TPilotTrack)	  		  		  	
	, LOCAL_CONTEXT(TChannelEqualization)	  		  	
	, LOCAL_CONTEXT(T11aLTSymbol)	  	
    , LOCAL_CONTEXT(TFineCFOEst)	  
	, LOCAL_CONTEXT(TChannelEst)	
	, LOCAL_CONTEXT(TPhaseCompensate)  	
	, LOCAL_CONTEXT(TFFT64)  		
	, LOCAL_CONTEXT(TFreqCompensation)  			
	, LOCAL_CONTEXT(TIFFTx)
	, LOCAL_CONTEXT(TMap11aBPSK)
	, LOCAL_CONTEXT(TMap11aQPSK)
	, LOCAL_CONTEXT(TMap11aQAM16)
	, LOCAL_CONTEXT(TMap11aQAM64)
	, LOCAL_CONTEXT(T11aSc)
	, LOCAL_CONTEXT(TModSink)
{
	void Reset () {
		// Reset all CFacade data in the context
		// CF_Error
		CF_Error::error_code() = E_ERROR_SUCCESS;
	}
	
	void Init ( void* sbuf, uint sbuf_size, uint nrun, TIMESTAMPINFO* tsi ) 
	{
		// CF_PerfEvalSource
		CF_PerfEvalSource::Init ( sbuf, sbuf_size, nrun, tsi );

		Reset ();
	}

}BrickTestContext;
	  
	  
BrickTestContext BrickTestCtx;

extern ISource* svit;


const uint str_len = 80;


// Note: use "static inline" instead of "inline" to prevent silent function confliction during linking.
// Otherwise, it will introduce wrong linking result because these inline function share the same name,
// and indeed not inlined.
// ref: http://stackoverflow.com/questions/2217628/multiple-definition-of-inline-functions-when-linking-static-libs
static inline
uint CreateTestGraph (ISource** src, char *aDesc, bool* aIsBit, uint* nReset, unsigned short *dataRate)
//void CreateTestGraph (ISource*& src)
{
	uint src_cnt = 0;


#ifdef RX_FULL
	// Defaults (data rates actually change from RXThread)
	BrickTestCtx.code_rate() = CR_12;
	BrickTestCtx.frame_length() = 1000;

	sprintf(aDesc + src_cnt*str_len, "RX_6Mbps");
    src[src_cnt] = CreateDemodGraph11a_6();
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_9Mbps");
	src[src_cnt] = CreateDemodGraph11a_6();
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_12Mbps");
    src[src_cnt] = CreateDemodGraph11a_12 ();
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_18Mbps");
	src[src_cnt] = CreateDemodGraph11a_12();
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_24Mbps");
    src[src_cnt] = CreateDemodGraph11a_24 ();
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_36Mbps");
	src[src_cnt] = CreateDemodGraph11a_24();
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_48Mbps");
    src[src_cnt] = CreateDemodGraph11a_48 ();
	dataRate[src_cnt] = CR_23;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_54Mbps");
	src[src_cnt] = CreateDemodGraph11a_48();
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	/*
	sprintf(aDesc + src_cnt*str_len, "RX_CCA");
    src[src_cnt] = CreateCCAGraph11a ();
	src_cnt++;
	*/
#endif


#ifdef TX_FULL
	sprintf(aDesc + src_cnt*str_len, "TX_6Mbps");
    src[src_cnt] = CreateTestModGraph11a_6 ();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_9Mbps");
	src[src_cnt] = CreateTestModGraph11a_9();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_12Mbps");
    src[src_cnt] = CreateTestModGraph11a_12 ();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_18Mbps");
	src[src_cnt] = CreateTestModGraph11a_18();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_24Mbps");
    src[src_cnt] = CreateTestModGraph11a_24 ();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_36Mbps");
	src[src_cnt] = CreateTestModGraph11a_36();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_48Mbps");
    src[src_cnt] = CreateTestModGraph11a_48 ();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_54Mbps");
	src[src_cnt] = CreateTestModGraph11a_54();
	aIsBit[src_cnt] = true;
	src_cnt++;
#endif


#ifdef RX_FULL_PIPE
	// NOTE: We can currently run only one 2-threaded pipeline in one run!!!

	// Defaults (data rates actually change from RXThread)
	BrickTestCtx.code_rate() = CR_12;
	BrickTestCtx.frame_length() = 1000;

	/*
	sprintf(aDesc + src_cnt*str_len, "RX_6Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_6_p(svit);
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;
	*/

	/*
	sprintf(aDesc + src_cnt*str_len, "RX_9Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_6_p(svit);
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_12Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_12_p(svit);
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_18Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_12_p(svit);
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_24Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_24_p(svit);
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_36Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_24_p(svit);
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "RX_48Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_48_p(svit);
	dataRate[src_cnt] = CR_23;
	nReset[src_cnt] = 1000;
	src_cnt++;
	*/

	sprintf(aDesc + src_cnt*str_len, "RX_54Mbps_p");
	src[src_cnt] = CreateDemodGraph11a_48_p(svit);
	dataRate[src_cnt] = CR_34;
	nReset[src_cnt] = 1000;
	src_cnt++;

	/*
	sprintf(aDesc + src_cnt*str_len, "RX_CCA");
	src[src_cnt] = CreateCCAGraph11a ();
	src_cnt++;
	*/
#endif

#ifdef TX_FULL_PIPE
	/*
	sprintf(aDesc + src_cnt*str_len, "TX_6Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_6_p();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_9Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_9_p();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_12Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_12_p();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_18Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_18_p();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_24Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_24_p();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_36Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_36_p();
	aIsBit[src_cnt] = true;
	src_cnt++;

	sprintf(aDesc + src_cnt*str_len, "TX_48Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_48_p();
	aIsBit[src_cnt] = true;
	src_cnt++;
	*/

	sprintf(aDesc + src_cnt*str_len, "TX_54Mbps_p");
	src[src_cnt] = CreateTestModGraph11a_54_p();
	aIsBit[src_cnt] = true;
	src_cnt++;
	/*
	*/
#endif




    CREATE_BRICK_SINK  (drop, TDropAny,    BrickTestCtx );


#ifdef RX_BLOCKS

	CREATE_BRICK_FILTER (ds2, TDownSample2, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 8> PerfEvalSource0;
    CREATE_BRICK_SOURCE (fsrc0, PerfEvalSource0::source, BrickTestCtx, ds2 );
	sprintf(aDesc + src_cnt*str_len, "TDownSample2");

    src[src_cnt] = fsrc0;
	src_cnt++;



	CREATE_BRICK_FILTER (dc,    TDCRemoveEx<4>::Filter, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 4> PerfEvalSource1;
    CREATE_BRICK_SOURCE (fsrc1, PerfEvalSource1::source, BrickTestCtx, dc );
	sprintf(aDesc + src_cnt*str_len, "TDCRemoveEx");

    src[src_cnt] = fsrc1;
	src_cnt++;



	CREATE_BRICK_FILTER (cca,   TCCA11a,    BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 4> PerfEvalSource2;
    CREATE_BRICK_SOURCE (fsrc2, PerfEvalSource2::source, BrickTestCtx, cca );
	sprintf(aDesc + src_cnt*str_len, "TCCA11a");

    src[src_cnt] = fsrc2;
	src_cnt++;



	CREATE_BRICK_FILTER (dcest, TDCEstimator, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 4> PerfEvalSource3;
    CREATE_BRICK_SOURCE (fsrc3, PerfEvalSource3::source, BrickTestCtx, dcest );
	sprintf(aDesc + src_cnt*str_len, "TDCEstimator");

    src[src_cnt] = fsrc3;
	src_cnt++;




	CREATE_BRICK_SINK (lsym,	T11aLTS, BrickTestCtx );	
	typedef PerfEvalSource<COMPLEX16, (160-16)> PerfEvalSource4;
    CREATE_BRICK_SOURCE (fsrc4, PerfEvalSource4::source, BrickTestCtx, lsym );
	sprintf(aDesc + src_cnt*str_len, "T11aLTS");

    src[src_cnt] = fsrc4;
	src_cnt++;




	CREATE_BRICK_FILTER (dsym,   T11aDataSymbol, 		BrickTestCtx, drop );		
	typedef PerfEvalSource<COMPLEX16, 80> PerfEvalSource5;
    CREATE_BRICK_SOURCE (fsrc5, PerfEvalSource5::source, BrickTestCtx, dsym );
	sprintf(aDesc + src_cnt*str_len, "T11aDataSymbol");

    src[src_cnt] = fsrc5;
	src_cnt++;




	CREATE_BRICK_FILTER (fcomp,  TFreqCompensation, 	BrickTestCtx, drop );				
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource6;
    CREATE_BRICK_SOURCE (fsrc6, PerfEvalSource6::source, BrickTestCtx, fcomp );
	sprintf(aDesc + src_cnt*str_len, "TFreqCompensation");

    src[src_cnt] = fsrc6;
	src_cnt++;




	CREATE_BRICK_FILTER (fft,    TFFT64, 				BrickTestCtx, drop );		
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource7;
    CREATE_BRICK_SOURCE (fsrc7, PerfEvalSource7::source, BrickTestCtx, fft );
	sprintf(aDesc + src_cnt*str_len, "TFFT64");

    src[src_cnt] = fsrc7;
	src_cnt++;




	CREATE_BRICK_FILTER (chequ,  TChannelEqualization, 	BrickTestCtx, drop );		
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource8;
    CREATE_BRICK_SOURCE (fsrc8, PerfEvalSource8::source, BrickTestCtx, chequ );
	sprintf(aDesc + src_cnt*str_len, "TChannelEqualization");

    src[src_cnt] = fsrc8;
	src_cnt++;




	

	CREATE_BRICK_FILTER (pcomp,  TPhaseCompensate, 		BrickTestCtx, drop );	
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource9;
    CREATE_BRICK_SOURCE (fsrc9, PerfEvalSource9::source, BrickTestCtx, pcomp );
	sprintf(aDesc + src_cnt*str_len, "TPhaseCompensate");

    src[src_cnt] = fsrc9;
	src_cnt++;




	

	CREATE_BRICK_FILTER (pilot,  TPilotTrack, 	BrickTestCtx, drop );		
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource10;
    CREATE_BRICK_SOURCE (fsrc10, PerfEvalSource10::source, BrickTestCtx, pilot );
	sprintf(aDesc + src_cnt*str_len, "TPilotTrack");

    src[src_cnt] = fsrc10;
	src_cnt++;




	

	CREATE_BRICK_FILTER (dmplcp, 	T11aDemapBPSK::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource11;
    CREATE_BRICK_SOURCE (fsrc11, PerfEvalSource11::source, BrickTestCtx, dmplcp );
	sprintf(aDesc + src_cnt*str_len, "T11aDemapBPSK");

    src[src_cnt] = fsrc11;
	src_cnt++;




	

	CREATE_BRICK_FILTER (dibpsk, 	T11aDeinterleaveBPSK, BrickTestCtx, drop );		
	typedef PerfEvalSource<uchar, 48> PerfEvalSource12;
    CREATE_BRICK_SOURCE (fsrc12, PerfEvalSource12::source, BrickTestCtx, dibpsk );
	sprintf(aDesc + src_cnt*str_len, "T11aDeinterleaveBPSK");

    src[src_cnt] = fsrc12;
	src_cnt++;


	
	CREATE_BRICK_FILTER (sviterbik, T11aViterbiSig, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 48> PerfEvalSource13;
    CREATE_BRICK_SOURCE (fsrc13, PerfEvalSource13::source, BrickTestCtx, sviterbik );
	sprintf(aDesc + src_cnt*str_len, "T11aViterbiSig");

    src[src_cnt] = fsrc13;
	dataRate[src_cnt] = CR_12;
	nReset[src_cnt] = 1000;
	src_cnt++;




    CREATE_BRICK_SINK   (plcp,   T11aPLCPParser, BrickTestCtx );
	typedef PerfEvalSource<uint, 1> PerfEvalSource14;
    CREATE_BRICK_SOURCE (fsrc14, PerfEvalSource14::source, BrickTestCtx, plcp );
	sprintf(aDesc + src_cnt*str_len, "T11aPLCPParser");

    src[src_cnt] = fsrc14;
	src_cnt++;



	// Defaults (data rates actually change from RXThread)
	BrickTestCtx.code_rate() = CR_12;
	BrickTestCtx.frame_length() = 1000;

	typedef T11aViterbi<5000 * 8, 48, 24> T11aViterbi6M;
	CREATE_BRICK_FILTER (viterbi,  T11aViterbi6M::Filter,  BrickTestCtx, drop );				
	typedef PerfEvalSource<uchar, 48> PerfEvalSource15;
    CREATE_BRICK_SOURCE (fsrc15, PerfEvalSource15::source, BrickTestCtx, viterbi );
	sprintf(aDesc + src_cnt*str_len, "T11aViterbi1/2");

    src[src_cnt] = fsrc15;
	nReset[src_cnt] = BrickTestCtx.frame_length();
	dataRate[src_cnt] = CR_12;
	src_cnt++;

	
	typedef T11aViterbi<5000*8,   48, 256> T11aViterbi24M;
	CREATE_BRICK_FILTER (viterbi24,  T11aViterbi24M::Filter,  BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 48> PerfEvalSource19a;
	CREATE_BRICK_SOURCE (fsrc19a, PerfEvalSource19a::source, BrickTestCtx, viterbi24 );
	sprintf(aDesc + src_cnt*str_len, "T11aViterbi2/3");

	src[src_cnt] = fsrc19a;
	nReset[src_cnt] = BrickTestCtx.frame_length();
	dataRate[src_cnt] = CR_23;
	src_cnt++;
	


	typedef T11aViterbi<5000*8, 48, 256> T11aViterbi48M;
	CREATE_BRICK_FILTER (viterbi48,  T11aViterbi48M::Filter,  BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 48> PerfEvalSource20a;
	CREATE_BRICK_SOURCE (fsrc20a, PerfEvalSource20a::source, BrickTestCtx, viterbi48 );
	sprintf(aDesc + src_cnt*str_len, "T11aViterbi3/4");

	src[src_cnt] = fsrc20a;
	nReset[src_cnt] = BrickTestCtx.frame_length();
	dataRate[src_cnt] = CR_34;
	src_cnt++;



	CREATE_BRICK_FILTER (desc,    T11aDesc,	    BrickTestCtx, drop );	
	typedef PerfEvalSource<uchar, 1> PerfEvalSource16;
    CREATE_BRICK_SOURCE (fsrc16, PerfEvalSource16::source, BrickTestCtx, desc );
	sprintf(aDesc + src_cnt*str_len, "T11aDesc");

    src[src_cnt] = fsrc16;
	src_cnt++;
	


	CREATE_BRICK_FILTER (dm48, 	T11aDemapQAM64::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource17;
    CREATE_BRICK_SOURCE (fsrc17, PerfEvalSource17::source, BrickTestCtx, dm48 );
	sprintf(aDesc + src_cnt*str_len, "T11aDemapQAM64");

    src[src_cnt] = fsrc17;
	src_cnt++;

	

	CREATE_BRICK_FILTER (di48, 	T11aDeinterleaveQAM64, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 288> PerfEvalSource18;
    CREATE_BRICK_SOURCE (fsrc18, PerfEvalSource18::source, BrickTestCtx, di48 );
	sprintf(aDesc + src_cnt*str_len, "T11aDeinterleaveQAM64");

    src[src_cnt] = fsrc18;
	src_cnt++;



	CREATE_BRICK_FILTER (dm12, 	T11aDemapQPSK::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource20r;
    CREATE_BRICK_SOURCE (fsrc20r, PerfEvalSource20r::source, BrickTestCtx, dm12 );
	sprintf(aDesc + src_cnt*str_len, "T11aDemapQPSK");

    src[src_cnt] = fsrc20r;
	src_cnt++;



	CREATE_BRICK_FILTER (di12, 	T11aDeinterleaveQPSK, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 96> PerfEvalSource19r;
    CREATE_BRICK_SOURCE (fsrc19r, PerfEvalSource19r::source, BrickTestCtx, di12 );
	sprintf(aDesc + src_cnt*str_len, "T11aDeinterleaveQPSK");

    src[src_cnt] = fsrc19r;
	src_cnt++;
	


	CREATE_BRICK_FILTER (dm24, 	T11aDemapQAM16::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource21r;
    CREATE_BRICK_SOURCE (fsrc21r, PerfEvalSource21r::source, BrickTestCtx, dm24 );
	sprintf(aDesc + src_cnt*str_len, "T11aDemapQAM16");

    src[src_cnt] = fsrc21r;
	src_cnt++;



	CREATE_BRICK_FILTER (di24, 	T11aDeinterleaveQAM16, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 192> PerfEvalSource22r;
    CREATE_BRICK_SOURCE (fsrc22r, PerfEvalSource22r::source, BrickTestCtx, di24 );
	sprintf(aDesc + src_cnt*str_len, "T11aDeinterleaveQAM16");

    src[src_cnt] = fsrc22r;
	src_cnt++;






#endif



#ifdef TX_BLOCKS
// ******************** TX ***********************
	
	CREATE_BRICK_FILTER (ifft, 	TIFFTx, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 64> PerfEvalSource19;
    CREATE_BRICK_SOURCE (fsrc19, PerfEvalSource19::source, BrickTestCtx, ifft );
	sprintf(aDesc + src_cnt*str_len, "TIFFTx");

    src[src_cnt] = fsrc19;
	src_cnt++;



	CREATE_BRICK_FILTER (addpilot, T11aAddPilot, BrickTestCtx, drop );
	typedef PerfEvalSource<COMPLEX16, 48> PerfEvalSource20;
    CREATE_BRICK_SOURCE (fsrc20, PerfEvalSource20::source, BrickTestCtx, addpilot );
	sprintf(aDesc + src_cnt*str_len, "T11aAddPilot");

    src[src_cnt] = fsrc20;
	src_cnt++;



	CREATE_BRICK_FILTER (map_bpsk, TMap11aBPSK, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 1> PerfEvalSource21;
    CREATE_BRICK_SOURCE (fsrc21, PerfEvalSource21::source, BrickTestCtx, map_bpsk );
	sprintf(aDesc + src_cnt*str_len, "TMap11aBPSK");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc21;
	src_cnt++;


	
	CREATE_BRICK_FILTER (map_qpsk, TMap11aQPSK, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 1> PerfEvalSource22;
    CREATE_BRICK_SOURCE (fsrc22, PerfEvalSource22::source, BrickTestCtx, map_qpsk );
	sprintf(aDesc + src_cnt*str_len, "TMap11aQPSK");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc22;
	src_cnt++;



	CREATE_BRICK_FILTER (map_qam16, TMap11aQAM16, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 1> PerfEvalSource23;
    CREATE_BRICK_SOURCE (fsrc23, PerfEvalSource23::source, BrickTestCtx, map_qam16 );
	sprintf(aDesc + src_cnt*str_len, "TMap11aQAM16");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc23;
	src_cnt++;



	CREATE_BRICK_FILTER (map_qam64, TMap11aQAM64, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 1> PerfEvalSource24;
    CREATE_BRICK_SOURCE (fsrc24, PerfEvalSource24::source, BrickTestCtx, map_qam64 );
	sprintf(aDesc + src_cnt*str_len, "TMap11aQAM64");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc24;
	src_cnt++;



	CREATE_BRICK_FILTER (inter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 6> PerfEvalSource25;
    CREATE_BRICK_SOURCE (fsrc25, PerfEvalSource25::source, BrickTestCtx, inter_bpsk );
	sprintf(aDesc + src_cnt*str_len, "T11aInterleaveBPSK");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc25;
	src_cnt++;


	
	CREATE_BRICK_FILTER (inter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 12> PerfEvalSource26;
    CREATE_BRICK_SOURCE (fsrc26, PerfEvalSource26::source, BrickTestCtx, inter_qpsk );
	sprintf(aDesc + src_cnt*str_len, "T11aInterleaveQPSK");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc26;
	src_cnt++;

	

	CREATE_BRICK_FILTER (inter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 24> PerfEvalSource27;
    CREATE_BRICK_SOURCE (fsrc27, PerfEvalSource27::source, BrickTestCtx, inter_qam16 );
	sprintf(aDesc + src_cnt*str_len, "T11aInterleaveQAM16");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc27;
	src_cnt++;



	CREATE_BRICK_FILTER (inter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 36> PerfEvalSource27a;
    CREATE_BRICK_SOURCE (fsrc27a, PerfEvalSource27a::source, BrickTestCtx, inter_qam64 );
	sprintf(aDesc + src_cnt*str_len, "T11aInterleaveQAM64");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc27a;
	src_cnt++;



    CREATE_BRICK_FILTER (enc12, TConvEncode_12, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 1> PerfEvalSource28;
    CREATE_BRICK_SOURCE (fsrc28, PerfEvalSource28::source, BrickTestCtx, enc12 );
	sprintf(aDesc + src_cnt*str_len, "TConvEncode_12");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc28;
	src_cnt++;



	CREATE_BRICK_FILTER (enc23, TConvEncode_23, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 2> PerfEvalSource29;
    CREATE_BRICK_SOURCE (fsrc29, PerfEvalSource29::source, BrickTestCtx, enc12 );
	sprintf(aDesc + src_cnt*str_len, "TConvEncode_23");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc29;
	src_cnt++;



	CREATE_BRICK_FILTER (enc34, TConvEncode_34, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 3> PerfEvalSource30;
    CREATE_BRICK_SOURCE (fsrc30, PerfEvalSource30::source, BrickTestCtx, enc34 );
	sprintf(aDesc + src_cnt*str_len, "TConvEncode_34");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc30;
	src_cnt++;



	CREATE_BRICK_FILTER (sc, T11aSc, BrickTestCtx, drop );
	typedef PerfEvalSource<uchar, 1> PerfEvalSource31;
    CREATE_BRICK_SOURCE (fsrc31, PerfEvalSource31::source, BrickTestCtx, sc );
	sprintf(aDesc + src_cnt*str_len, "T11aSc");

	aIsBit[src_cnt] = true;
    src[src_cnt] = fsrc31;
	src_cnt++;
#endif

	
		

	return src_cnt;
}









#ifdef TX_FULL
#include "test_config_tx.hpp"
#endif

#ifdef TX_FULL_PIPE
#include "test_config_tx_p.hpp"
#endif

#ifdef RX_FULL
#include "test_config_rx.hpp"
#endif RX_FULL


#ifdef RX_FULL_PIPE
#include "test_config_rx_p.hpp"
#endif RX_FULL




