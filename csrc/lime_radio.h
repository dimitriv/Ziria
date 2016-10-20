/*
Coyright (c) Rice University, RECG Lab
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
#ifdef LIME_RF
#include "params.h"
#include "numerics.h"

#ifdef PL_CS
void startCarrierSense(BlinkParams *params);
#endif

int  LimeRF_RadioStart(BlinkParams *params);
int  LimeRF_ConfigureTX(BlinkParams *params);
int  LimeRF_ConfigureRX(BlinkParams *params);

void LimeRF_RadioStop(BlinkParams *params);
void readLimeRF(BlinkParams *params, complex16 *ptr, int size);
void writeLimeRF(BlinkParams *params, complex16 *ptr, unsigned long size);

#endif


