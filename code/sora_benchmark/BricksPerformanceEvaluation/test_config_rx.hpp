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
static inline
FINL ISource* CreateDemodGraph11a_6()
{
	CREATE_BRICK_SINK(drop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(desc, T11aDesc, BrickTestCtx, drop);

	// Single thread test here
	typedef T11aViterbi <5000 * 8, 48, 256> T11aViterbiComm;
	//CREATE_BRICK_FILTER (viterbi, T11aViterbiComm::Filter,	BrickTestCtx, desc );				
	//CREATE_BRICK_FILTER (vit0, TThreadSeparator<>::Filter, BrickTestCtx, viterbi);
	CREATE_BRICK_FILTER(vit0, T11aViterbiComm::Filter, BrickTestCtx, desc);

	// 6M
	CREATE_BRICK_FILTER(di6, T11aDeinterleaveBPSK, BrickTestCtx, vit0);
	CREATE_BRICK_FILTER(dm6, T11aDemapBPSK::filter, BrickTestCtx, di6);


	CREATE_BRICK_FILTER(pilot, TPilotTrack, BrickTestCtx, dm6);


	CREATE_BRICK_FILTER(pcomp, TPhaseCompensate, BrickTestCtx, pilot);
	CREATE_BRICK_FILTER(chequ, TChannelEqualization, BrickTestCtx, pcomp);
	CREATE_BRICK_FILTER(fft, TFFT64, BrickTestCtx, chequ);
	CREATE_BRICK_FILTER(fcomp, TFreqCompensation, BrickTestCtx, fft);
	CREATE_BRICK_FILTER(dsym, T11aDataSymbol, BrickTestCtx, fcomp);
	CREATE_BRICK_FILTER(dsym0, TNoInline, BrickTestCtx, dsym);


	// Skip CCA here as we test the receiver with dummy input
	// CREATE_BRICK_FILTER (ds2, TDownSample2, BrickTestCtx, rxswt );
	CREATE_BRICK_FILTER(ds2, TDownSample2, BrickTestCtx, dsym0);


	typedef PerfEvalSource<COMPLEX16, 8> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(RX, PerfEvalSourceTX::source, BrickTestCtx, ds2);

	return RX;
}





static inline
FINL ISource* CreateDemodGraph11a_12()
{
	CREATE_BRICK_SINK(drop, TDropAny, BrickTestCtx);

	//CREATE_BRICK_SINK   (fsink, TBB11aFrameSink, BrickTestCtx );	
	CREATE_BRICK_FILTER(desc, T11aDesc, BrickTestCtx, drop);

	// Single thread test here
	typedef T11aViterbi <5000 * 8, 48, 256> T11aViterbiComm;
	//CREATE_BRICK_FILTER (viterbi, T11aViterbiComm::Filter,	BrickTestCtx, desc );				
	//CREATE_BRICK_FILTER (vit0, TThreadSeparator<>::Filter, BrickTestCtx, viterbi);
	CREATE_BRICK_FILTER(vit0, T11aViterbiComm::Filter, BrickTestCtx, desc);

	// 12M
	CREATE_BRICK_FILTER(di12, T11aDeinterleaveQPSK, BrickTestCtx, vit0);
	CREATE_BRICK_FILTER(dm12, T11aDemapQPSK::filter, BrickTestCtx, di12);

	CREATE_BRICK_FILTER(pilot, TPilotTrack, BrickTestCtx, dm12);


	CREATE_BRICK_FILTER(pcomp, TPhaseCompensate, BrickTestCtx, pilot);
	CREATE_BRICK_FILTER(chequ, TChannelEqualization, BrickTestCtx, pcomp);
	CREATE_BRICK_FILTER(fft, TFFT64, BrickTestCtx, chequ);
	CREATE_BRICK_FILTER(fcomp, TFreqCompensation, BrickTestCtx, fft);
	CREATE_BRICK_FILTER(dsym, T11aDataSymbol, BrickTestCtx, fcomp);
	CREATE_BRICK_FILTER(dsym0, TNoInline, BrickTestCtx, dsym);


	// Skip CCA here as we test the receiver with dummy input
	// CREATE_BRICK_FILTER (ds2, TDownSample2, BrickTestCtx, rxswt );
	CREATE_BRICK_FILTER(ds2, TDownSample2, BrickTestCtx, dsym0);


	typedef PerfEvalSource<COMPLEX16, 8> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(RX, PerfEvalSourceTX::source, BrickTestCtx, ds2);

	return RX;
}




static inline
FINL ISource* CreateDemodGraph11a_24()
{
	CREATE_BRICK_SINK(drop, TDropAny, BrickTestCtx);

	//CREATE_BRICK_SINK   (fsink, TBB11aFrameSink, BrickTestCtx );	
	CREATE_BRICK_FILTER(desc, T11aDesc, BrickTestCtx, drop);

	// Single thread test here
	typedef T11aViterbi <5000 * 8, 48, 256> T11aViterbiComm;
	//CREATE_BRICK_FILTER (viterbi, T11aViterbiComm::Filter,	BrickTestCtx, desc );				
	//CREATE_BRICK_FILTER (vit0, TThreadSeparator<>::Filter, BrickTestCtx, viterbi);
	CREATE_BRICK_FILTER(vit0, T11aViterbiComm::Filter, BrickTestCtx, desc);

	// 24M
	CREATE_BRICK_FILTER(di24, T11aDeinterleaveQAM16, BrickTestCtx, vit0);
	CREATE_BRICK_FILTER(dm24, T11aDemapQAM16::filter, BrickTestCtx, di24);

	CREATE_BRICK_FILTER(pilot, TPilotTrack, BrickTestCtx, dm24);


	CREATE_BRICK_FILTER(pcomp, TPhaseCompensate, BrickTestCtx, pilot);
	CREATE_BRICK_FILTER(chequ, TChannelEqualization, BrickTestCtx, pcomp);
	CREATE_BRICK_FILTER(fft, TFFT64, BrickTestCtx, chequ);
	CREATE_BRICK_FILTER(fcomp, TFreqCompensation, BrickTestCtx, fft);
	CREATE_BRICK_FILTER(dsym, T11aDataSymbol, BrickTestCtx, fcomp);
	CREATE_BRICK_FILTER(dsym0, TNoInline, BrickTestCtx, dsym);


	// Skip CCA here as we test the receiver with dummy input
	// CREATE_BRICK_FILTER (ds2, TDownSample2, BrickTestCtx, rxswt );
	CREATE_BRICK_FILTER(ds2, TDownSample2, BrickTestCtx, dsym0);


	typedef PerfEvalSource<COMPLEX16, 8> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(RX, PerfEvalSourceTX::source, BrickTestCtx, ds2);

	return RX;
}




static inline
FINL ISource* CreateDemodGraph11a_48()
{
	CREATE_BRICK_SINK(drop, TDropAny, BrickTestCtx);

	//CREATE_BRICK_SINK   (fsink, TBB11aFrameSink, BrickTestCtx );	
	CREATE_BRICK_FILTER(desc, T11aDesc, BrickTestCtx, drop);

	// Single thread test here
	typedef T11aViterbi <5000 * 8, 48, 256> T11aViterbiComm;
	//CREATE_BRICK_FILTER(viterbi, T11aViterbiComm::Filter, BrickTestCtx, desc);
	//CREATE_BRICK_FILTER(vit0, TThreadSeparator<>::Filter, BrickTestCtx, viterbi);
	CREATE_BRICK_FILTER (vit0, T11aViterbiComm::Filter,	BrickTestCtx, desc );				


	// 48M
	CREATE_BRICK_FILTER(di48, T11aDeinterleaveQAM64, BrickTestCtx, vit0);
	CREATE_BRICK_FILTER(dm48, T11aDemapQAM64::filter, BrickTestCtx, di48);

	CREATE_BRICK_FILTER(pilot, TPilotTrack, BrickTestCtx, dm48);


	CREATE_BRICK_FILTER(pcomp, TPhaseCompensate, BrickTestCtx, pilot);
	CREATE_BRICK_FILTER(chequ, TChannelEqualization, BrickTestCtx, pcomp);
	CREATE_BRICK_FILTER(fft, TFFT64, BrickTestCtx, chequ);
	CREATE_BRICK_FILTER(fcomp, TFreqCompensation, BrickTestCtx, fft);
	CREATE_BRICK_FILTER(dsym, T11aDataSymbol, BrickTestCtx, fcomp);
	CREATE_BRICK_FILTER(dsym0, TNoInline, BrickTestCtx, dsym);


	// Skip CCA here as we test the receiver with dummy input
	// CREATE_BRICK_FILTER (ds2, TDownSample2, BrickTestCtx, rxswt );
	CREATE_BRICK_FILTER(ds2, TDownSample2, BrickTestCtx, dsym0);


	typedef PerfEvalSource<COMPLEX16, 8> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(RX, PerfEvalSourceTX::source, BrickTestCtx, ds2);

	return RX;
}

static inline
FINL ISource* CreateCCAGraph11a()
{
	CREATE_BRICK_SINK(drop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(dcest, TDCEstimator, BrickTestCtx, drop);
	CREATE_BRICK_FILTER(cca, TCCA11a, BrickTestCtx, dcest);
	CREATE_BRICK_FILTER(dc, TDCRemoveEx<4>::Filter, BrickTestCtx, cca);


	// Skip CCA here as we test the receiver with dummy input
	CREATE_BRICK_FILTER(ds2, TDownSample2, BrickTestCtx, dc);


	typedef PerfEvalSource<COMPLEX16, 8> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(RX, PerfEvalSourceTX::source, BrickTestCtx, ds2);

	return RX;
}

