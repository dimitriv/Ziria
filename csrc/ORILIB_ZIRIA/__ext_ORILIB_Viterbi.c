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

#include <ORILIB_ViterbiDecoderVCP_Dynamic_i.h>
#include <ORILIB_ViterbiDecoderVCP_InitTrueWFC_i.h>
#include <ORILIB_ViterbiDecoderVCP_WaitForCompletion_i.h>
#include <ORILIB_ViterbiDecoderVCP_i.h>
#include <ORILIB_ViterbiDecoder_CacheBranchMetricsR1by2F24_i.h>
#include <ORILIB_ViterbiDecoder_GenConfSL_i.h>
#include <ORILIB_ViterbiDecoder_GenConfSLip_i.h>
#include <ORILIB_ViterbiDecoder_GenConf_i.h>
#include <ORILIB_ViterbiDecoder_InitOnce_i.h>
#include <ORILIB_ViterbiDecodingMap_i.h>

void __ext_ORILIB_ViterbiDecoder_InitOnce(
		ORILIB_t_ViterbiDecoderState * const state, int __unused1__,
		ORILIB_t_ViterbiDecoderInitOnceConf * conf, int __unused2__) {
	ORILIB_ViterbiDecoder_InitOnce_i(state, conf);
}

void __ext_ORILIB_ViterbiDecoder_GenConfSL(
		ORILIB_t_ViterbiDecoderState * const inpState, int __unused1__,
		ORILIB_t_ViterbiDecoderState * const outState, int __unused2__,
		ORILIB_t_ViterbiDecoderGenConfConf * const conf, int __unused3__) {
	ORILIB_ViterbiDecoder_GenConfSL_i(inpState, outState, conf);
}

void __ext_ORILIB_ViterbiDecoderVCP_InitTrueWFC(
		ORILIB_t_ViterbiDecoderWFCState * wfcState, int __unused1__,
		ORILIB_t_ViterbiDecoderWFCConf * conf, int __unused2__) {
	ORILIB_ViterbiDecoderVCP_InitTrueWFC_i(wfcState, conf);
}

void __ext_ORILIB_ViterbiDecoder_CacheBranchMetricsR1by2F24(
		ORILIB_t_SoftBitBuf48 * inpBranchMetrics, int __unused1__,
		ORILIB_t_ViterbiDecoderState * const inpState, int __unused2__,
		ORILIB_t_ViterbiDecoderState * const outState, int __unused3__) {
	ORILIB_ViterbiDecoder_CacheBranchMetricsR1by2F24_i(inpBranchMetrics,
			inpState, outState);
}

void __ext_ORILIB_ViterbiDecoder_GenConf(
		Vitdec_ViterbiDecodingMap const * const restrict vitdecMap,
		int __unused1__, ORILIB_t_ViterbiDecoderState * const inpState,
		int __unused2__, ORILIB_t_ViterbiDecoderState * const outState,
		int __unused3__, ORILIB_t_ViterbiDecoderGenConfConf * const conf,
		int __unused4__) {
	ORILIB_ViterbiDecoder_GenConf_i(vitdecMap, inpState, outState, conf);
}

void __ext_ORILIB_ViterbiDecoder_GenConfSLip(
		ORILIB_t_ViterbiDecoderState * const inoutState, int __unused1__,
		ORILIB_t_ViterbiDecoderGenConfConf * const conf, int __unused2__) {
	ORILIB_ViterbiDecoder_GenConfSLip_i(inoutState, conf);
}

void __ext_ORILIB_ViterbiDecoderVCP_Dynamic(void* inpBufBranchMetrics,
		int __unused1__, ORILIB_t_ViterbiDecoderState* inpBufState,
		int __unused2__, ORILIB_t_ViterbiDecoderWFCState* inoutBufWfcState,
		int __unused3__, void* outBufDecisions, int __unused4__) {
	ORILIB_ViterbiDecoderVCP_Dynamic_i(inpBufBranchMetrics, inpBufState,
			inoutBufWfcState, outBufDecisions);
}

void __ext_ORILIB_ViterbiDecoderVCP(void* inpBufBranchMetrics, int __unused1__,
		ORILIB_t_ViterbiDecoderState* inpBufState, int __unused2__,
		void* outBufDecisions, int __unused3__,
		ORILIB_t_ViterbiDecoderWFCState * outBufWfcState, int __unused4__) {
	ORILIB_ViterbiDecoderVCP_i(inpBufBranchMetrics, inpBufState,
			outBufDecisions, outBufWfcState);
}

void __ext_ORILIB_ViterbiDecoderVCP_WaitForCompletion(
		ORILIB_t_ViterbiDecoderWFCState* const wfcState, int __unused__) {
	ORILIB_ViterbiDecoderVCP_WaitForCompletion_i(wfcState);
}

void __ext_ORILIB_ViterbiDecodingMap(
		WIFILIB_TxRxPktState * const restrict inpPktState, int __unused1__,
		Vitdec_ViterbiDecodingMap * const restrict vitdecMap, int __unused2__,
		ORILIB_t_ViterbiDecodingMapConf *conf, int __unused3__) {
	ORILIB_ViterbiDecodingMap_i(inpPktState, vitdecMap, conf);
}
