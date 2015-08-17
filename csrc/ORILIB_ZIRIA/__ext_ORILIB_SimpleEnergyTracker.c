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

#include <ORILIB_SimpleEnergyTracker_Init_i.h>
#include <ORILIB_SimpleEnergyTracker_i.h>

void __ext_ORILIB_SimpleEnergyTracker_Init(
		ORILIB_t_SimpleEnergyState * simpleEnergyState, int __unused__) {
	ORILIB_SimpleEnergyTracker_Init_i(simpleEnergyState);
}

void __ext_ORILIB_SimpleEnergyTracker(
		ORILIB_t_Cplx16Buf80 const * const restrict unalignedRawSampleBuf,
		int __unused1__, ORILIB_t_SimpleEnergyState * simpleEnergyState,
		int __unused2__) {
	ORILIB_SimpleEnergyTracker_i(unalignedRawSampleBuf, simpleEnergyState);
}

