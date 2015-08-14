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

#include <IO_MemWriter_Init_i.h>
#include <IO_MemWriter_i.h>

void __ext_IO_MemWriter_Init(IO_t_MemWriter_State * outStateBuf,
		int __unsused__) {
	IO_MemWriter_Init_i(outStateBuf);
}

void __ext_IO_MemWriter(void * byteBuf, int __unused1__,
		IO_t_MemWriter_State * const inpStateBuf, int __unused2__,
		IO_t_MemWriter_State * outStateBuf, int __unused3__,
		IO_t_MemWriter_Conf * conf, int __unused4__) {
	IO_MemWriter_i(byteBuf, inpStateBuf, outStateBuf, conf);
}

