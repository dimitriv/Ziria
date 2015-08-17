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

#include <ORILIB_WindowCorrelator_i.h>

void __ext_ORILIB_WindowCorrelator(
		ORILIB_t_Cplx16Buf80 const * const restrict sampleBufWithGain,
		int __unused1__, ORILIB_t_GainStateAux* inpGainState, int __unused2__,
		ORILIB_t_AlignState* inoutAlignState, int __unused3__,
		ORILIB_t_CorrState* inoutCorrState, int __unused4__,
		ORILIB_t_CorrStateAux* inoutCorrStateAux, int __unused5__) {
	ORILIB_WindowCorrelator_i(sampleBufWithGain, inpGainState, inoutAlignState,
			inoutCorrState, inoutCorrStateAux);
}

