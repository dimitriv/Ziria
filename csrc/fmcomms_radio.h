/*
Coyright (c) Northeastern University, GENESYS Lab
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

#pragma once
#ifdef ADI_RF
#include "params.h"
#include "numerics.h"
#ifdef __ARM_NEON__
#include "neon/types.h"
#endif
#include <iio.h>
#include <errno.h>

int Fmcomms_RadioStartTx(BlinkParams *params);
int Fmcomms_RadioStartRx(BlinkParams *params);
int Fmcomms_Init(BlinkParams *params);
void Fmcomms_RadioStop(BlinkParams *params);
void readFmcomms(BlinkParams *params, complex16 *ptr, int size);
void writeFmcomms(BlinkParams *params, complex16 *ptr, int size);
//void iio_to_ziria_memcpy(complex16* zbuf, void * ibuf, int iiosize);
#endif
