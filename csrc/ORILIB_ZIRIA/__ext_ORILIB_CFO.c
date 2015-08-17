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

#include <ORILIB_CFOCorrectorGMOffset_i.h>
#include <ORILIB_CFOCorrectorGM_i.h>
#include <ORILIB_CFOCorrectorGOffset_i.h>
#include <ORILIB_CFOCorrectorG_i.h>
#include <ORILIB_CFOEstimatorLTF_i.h>

void __ext_ORILIB_CFOCorrectorG(ORILIB_t_Cplx16Buf80 *inBuf, int __unused1__,
		ORILIB_t_CFOState *cfoState, int __unused2__,
		ORILIB_t_PktAgcState *agcState, int __unused3__,
		ORILIB_t_Cplx16Buf80 *outBuf, int __unused4__) {
	ORILIB_CFOCorrectorG_i(inBuf, cfoState, agcState, outBuf);
}

void __ext_ORILIB_CFOCorrectorGM(ORILIB_t_Cplx16Buf80 *inBuf, int __unused1__,
		ORILIB_t_CFOState *cfoState, int __unused2__,
		ORILIB_t_PktAgcState *agcState, int __unused3__,
		ORILIB_t_Cplx16Buf80 *outBuf, int __unused4__) {
	ORILIB_CFOCorrectorGM_i(inBuf, cfoState, agcState, outBuf);
}

void __ext_ORILIB_CFOCorrectorGMOffset(void *inBuf, int __unused1__,
		ORILIB_t_CFOState *cfoState, int __unused2__,
		ORILIB_t_PktAgcState *agcState, int __unused3__, void *outBuf,
		int __unused4__, ORILIB_t_Offset2 *conf, int __unused5__) {
	ORILIB_CFOCorrectorGMOffset_i(inBuf, cfoState, agcState, outBuf, conf);
}

void __ext_ORILIB_CFOCorrectorGOffset(void *inBuf, int __unused1__,
		ORILIB_t_CFOState *cfoState, int __unused2__,
		ORILIB_t_PktAgcState *agcState, int __unused3__, void *outBuf,
		int __unused4__, ORILIB_t_Offset2 *conf, int __unused5__) {
	ORILIB_CFOCorrectorGOffset_i(inBuf, cfoState, agcState, outBuf, conf);
}

void __ext_ORILIB_CFOEstimatorLTF(
		ORILIB_t_Cplx16Buf160 const * const inpFullLtfBuf, int __unused1__,
		ORILIB_t_PktAgcState const * const restrict agcState, int __unused2__,
		ORILIB_t_CFOState * const restrict cfoState, int __unused3__) {
	ORILIB_CFOEstimatorLTF_i(inpFullLtfBuf, agcState, cfoState);
}

