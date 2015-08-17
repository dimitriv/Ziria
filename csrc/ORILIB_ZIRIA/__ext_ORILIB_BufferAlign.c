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

#include <ORILIB_BufferAlign_Init_i.h>
#include <ORILIB_BufferAlign_i.h>
#include <ORILIB_BufferAlignOffset_i.h>

void __ext_ORILIB_BufferAlign(
		ORILIB_t_Cplx16Buf80 const * const restrict unalignedRawSampleBuf,
		int __unused1__, ORILIB_t_AlignState * alignStateInpOut,
		int __unused2__,
		ORILIB_t_Cplx16Buf80 * const restrict alignedRawSampleBuf,
		int __unused3__) {
	ORILIB_BufferAlign_i(unalignedRawSampleBuf, alignStateInpOut,
			alignedRawSampleBuf);
}

void __ext_ORILIB_BufferAlign_Init(ORILIB_t_AlignState * outAlignState,
		int __unused__) {
	ORILIB_BufferAlign_Init_i(outAlignState);
}

void __ext_ORILIB_BufferAlignOffset(
		ORILIB_t_Cplx16Buf80 const * const restrict unalignedRawSampleBuf,
		int __unused1__, ORILIB_t_AlignState * alignStateInpOut,
		int __unused2__, void * const alignedRawSampleBuf, int __unused3__,
		ORILIB_t_Offset1 * conf, int __unused4__) {
	ORILIB_BufferAlignOffset_i(unalignedRawSampleBuf, alignStateInpOut,
			alignedRawSampleBuf, conf);
}
