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

#include <ORILIB_PeakDetector_i.h>

void __ext_ORILIB_PeakDetector(
		ORILIB_t_Cplx16Buf80 const * const restrict sampleBufWithGain,
		int __unused1__, ORILIB_t_GainStateAux * gainStateAux, int __unused2__,
		ORILIB_t_AlignState * alignState, int __unused3__,
		ORILIB_t_CorrState * corrState, int __unused4__,
		ORILIB_t_DetectState * outDetectState, int __unused5__) {
	ORILIB_PeakDetector_i(sampleBufWithGain, gainStateAux, alignState, corrState, outDetectState);
}

