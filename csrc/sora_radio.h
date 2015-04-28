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
// Put to 0 to run tests on a PC without Sora board
#pragma once

#ifdef SORA_RF
#include <soratime.h>
#include <thread_func.h>
#include <stdlib.h>
#include <time.h>
#include <rxstream.h>
#include "params.h"
#include "numerics.h"



void RadioStart(BlinkParams *params);
void RadioStop(BlinkParams *params);
void InitSoraRx(BlinkParams *params);
void InitSoraTx(BlinkParams *params);
void readSora(BlinkParams *params, complex16 *ptr, int size);
void writeSora(BlinkParams *params, complex16 *ptr, ULONG size);


#define readSDR		readSora
#define writeSDR	writeSora
#endif

