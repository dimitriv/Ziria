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

#include <ORILIB_OFDMDemodulator_InitPerPacket_i.h>
#include <ORILIB_OFDMDemodulator_i.h>
#include <ORILIB_OFDMEqualizer_Float_i.h>
#include <ORILIB_OFDMEqualizer_i.h>
#include <ORILIB_OFDMModulator_InitPerPacket_i.h>
#include <ORILIB_OFDMModulator_i.h>

void __ext_ORILIB_OFDMModulator_InitPerPacket(
		ORILIB_OFDMModulator_t_State * outState, int __unused1__,
		ORILIB_OFDMModulator_InitPerPacket_t_Conf * conf, int __unused2__) {
	ORILIB_OFDMModulator_InitPerPacket_i(outState, conf);
}

void __ext_ORILIB_OFDMDemodulator_InitPerPacket(
		ORILIB_OFDMDemodulator_t_State * outState, int __unused__) {
	ORILIB_OFDMDemodulator_InitPerPacket_i(outState);
}

void __ext_ORILIB_OFDMDemodulator(ORILIB_t_Cplx16Buf80 * inpSampleBuf,
		ORILIB_OFDMDemodulator_t_State * inpStateBuf, int __unused1__,
		ORILIB_t_Cplx16Buf48 * outDataSubcarriersBuf, int __unused2__,
		ORILIB_t_Cplx16Buf4 * outPilotSubcarriersBuf, int __unused3__,
		ORILIB_OFDMDemodulator_t_State * outStateBuf, int __unused4__) {
	ORILIB_OFDMDemodulator_i(inpSampleBuf, inpStateBuf, outDataSubcarriersBuf,
			outPilotSubcarriersBuf, outStateBuf);
}

void __ext_ORILIB_OFDMModulator(ORILIB_t_Cplx16Buf48* inpSampleBuf,
		int __unused1__, ORILIB_OFDMModulator_t_State* inpStateBuf,
		int __unused2__, ORILIB_t_Cplx16Buf80* outSampleBuf, int __unused3__,
		ORILIB_OFDMModulator_t_State* outStateBuf, int __unused4__,
		ORILIB_OFDMModulator_t_Conf* conf, int __unused5__) {
	ORILIB_OFDMModulator_i(inpSampleBuf, inpStateBuf, outSampleBuf, outStateBuf,
			conf);
}

void __ext_ORILIB_OFDMEqualizer(ORILIB_t_Cplx16Buf48* bufInpConstPnts,
		int __unused1__, ORILIB_t_Cplx16Buf4* bufInpSignNormalizedPilotPnts,
		int __unused2__, ORILIB_t_ChEstimatorLTF_State* bufInpChState,
		int __unused3__, ORILIB_t_Cplx16Buf48* bufOutEqualizedPnts,
		int __unused4__, ORILIB_t_Cplx32Buf48* bufOutyhstar, int __unused5__,
		ORILIB_t_OFDMEqualizer_State* bufOutEqState, int __unused6__) {
	ORILIB_OFDMEqualizer_i(bufInpConstPnts, bufInpSignNormalizedPilotPnts,
			bufInpChState, bufOutEqualizedPnts, bufOutyhstar, bufOutEqState);
}

void __ext_ORILIB_OFDMEqualizer_Float(ORILIB_t_Cplx16Buf48* bufInpConstPnts,
		int __unused1__, ORILIB_t_Cplx16Buf4* bufInpSignNormalizedPilotPnts,
		int __unused2__, ORILIB_t_ChEstimatorLTF_Float_State* bufInpChState,
		int __unused3__, ORILIB_t_Cplx16Buf48* bufOutEqualizedPnts,
		int __unused4__, ORILIB_t_Cplx32Buf48* bufOutyhstar, int __unused5__,
		ORILIB_t_OFDMEqualizer_State* bufOutEqState, int __unused6__) {
	ORILIB_OFDMEqualizer_Float_i(bufInpConstPnts, bufInpSignNormalizedPilotPnts,
			bufInpChState, bufOutEqualizedPnts, bufOutyhstar, bufOutEqState);
}

