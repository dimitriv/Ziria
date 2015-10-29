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
static FINL ISource* CreateTestModGraph11a_6_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc6, TConvEncode_12, BrickTestCtx, TXinter_bpsk);


	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc6);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}


static FINL ISource* CreateTestModGraph11a_9_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc6, TConvEncode_34, BrickTestCtx, TXinter_bpsk);


	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc6);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}



static FINL ISource* CreateTestModGraph11a_12_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc, TConvEncode_12, BrickTestCtx, TXinter_qpsk);


	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}


static FINL ISource* CreateTestModGraph11a_18_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc, TConvEncode_34, BrickTestCtx, TXinter_qpsk);


	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}


static FINL ISource* CreateTestModGraph11a_24_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc, TConvEncode_12, BrickTestCtx, TXinter_qam16);

	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}


static FINL ISource* CreateTestModGraph11a_36_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc, TConvEncode_34, BrickTestCtx, TXinter_qam16);

	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}


static FINL ISource* CreateTestModGraph11a_48_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc, TConvEncode_23, BrickTestCtx, TXinter_qam64);

	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}


static FINL ISource* CreateTestModGraph11a_54_p() {
	CREATE_BRICK_SINK(TXdrop, TDropAny, BrickTestCtx);

	CREATE_BRICK_FILTER(TXpack, TPackSample16to8, BrickTestCtx, TXdrop);
	CREATE_BRICK_FILTER(TXifft, TIFFTx, BrickTestCtx, TXpack);
	CREATE_BRICK_FILTER(TXaddpilot1, T11aAddPilot, BrickTestCtx, TXifft);
	CREATE_BRICK_FILTER(TXaddpilot0, TNoInline, BrickTestCtx, TXaddpilot1);
	CREATE_BRICK_FILTER(TXaddpilot, TThreadSeparator<>::Filter, BrickTestCtx, TXaddpilot0);

	CREATE_BRICK_FILTER(TXmap_bpsk, TMap11aBPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qpsk, TMap11aQPSK, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam16, TMap11aQAM16, BrickTestCtx, TXaddpilot);
	CREATE_BRICK_FILTER(TXmap_qam64, TMap11aQAM64, BrickTestCtx, TXaddpilot);


	CREATE_BRICK_FILTER(TXinter_bpsk, T11aInterleaveBPSK::filter, BrickTestCtx, TXmap_bpsk);
	CREATE_BRICK_FILTER(TXinter_qpsk, T11aInterleaveQPSK::filter, BrickTestCtx, TXmap_qpsk);
	CREATE_BRICK_FILTER(TXinter_qam16, T11aInterleaveQAM16::filter, BrickTestCtx, TXmap_qam16);
	CREATE_BRICK_FILTER(TXinter_qam64, T11aInterleaveQAM64::filter, BrickTestCtx, TXmap_qam64);

	CREATE_BRICK_FILTER(TXenc, TConvEncode_34, BrickTestCtx, TXinter_qam64);

	//CREATE_BRICK_FILTER ( TXenc6,	    TConvEncode_12,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc9,	    TConvEncode_34,    BrickTestCtx, TXinter_bpsk );
	//CREATE_BRICK_FILTER ( TXenc12,	TConvEncode_12,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc18,	TConvEncode_34,    BrickTestCtx, TXinter_qpsk );
	//CREATE_BRICK_FILTER ( TXenc24,	TConvEncode_12,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc36,    TConvEncode_34,    BrickTestCtx, TXinter_qam16 );
	//CREATE_BRICK_FILTER ( TXenc48,	TConvEncode_23,    BrickTestCtx, TXinter_qam64 );
	//CREATE_BRICK_FILTER ( TXenc54,	TConvEncode_34,    BrickTestCtx, TXinter_qam64 );

	//CREATE_BRICK_DEMUX8 ( mrsel,	TBB11aMRSelect,    BrickTestCtx,
	//                    TXenc6, TXenc9, TXenc12, TXenc18, TXenc24, TXenc36, TXenc48, TXenc54 );



	//CREATE_BRICK_FILTER ( TXsc,	T11aSc,    BrickTestCtx, TXmrsel );
	CREATE_BRICK_FILTER(TXsc, T11aSc, BrickTestCtx, TXenc);

	typedef PerfEvalSource<uchar, 1> PerfEvalSourceTX;
	CREATE_BRICK_SOURCE(TX6, PerfEvalSourceTX::source, BrickTestCtx, TXsc);

	svit = TXaddpilot;

	return TX6;
}
