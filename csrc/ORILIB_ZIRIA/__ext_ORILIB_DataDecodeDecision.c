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

#include <ORILIB_DataDecodeDecisionInit_i.h>
#include <ORILIB_DataDecodeDecision_i.h>

void __ext_ORILIB_DataDecodeDecisionInit(
		ORILIB_t_DataDecodeState *bOutDecodeState, int __unused__) {
	ORILIB_DataDecodeDecisionInit_i(bOutDecodeState);
}

void __ext_ORILIB_DataDecodeDecision(
		Vitdec_ViterbiDecodingMap const * const restrict vitdecMap,
		int __unused1__, ORILIB_t_DataDecodeState *bInpDecodeState,
		int __unused2__, ORILIB_t_DataDecodeState *bOutDecodeState,
		int __unused3__, Decision_t *bOutDecision, int __unused4__,
		ORILIB_t_DataDecodeDecisionConf *bInpConf, int __unused5__) {
	ORILIB_DataDecodeDecision_i(vitdecMap, bInpDecodeState, bOutDecodeState,
			bOutDecision, bInpConf);
}

