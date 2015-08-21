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

#pragma once

/**************************************************************
 *                                                            *
 *  Main header to be included in Blink-generated C code      *
 *                                                            *
 **************************************************************/

#include "bit.h"
#include "numerics.h"

typedef num8  int8;
typedef num16 int16;
typedef num32 int32;
typedef num64 int64;

// FORCE_INLINE defined in numerics.h
/* #ifndef FINL */
/* #define FINL FORCE_INLINE  */
/* #endif  */

#ifdef SORA_PLATFORM

typedef unsigned __int8  uint8;
typedef unsigned __int16 uint16;
typedef unsigned __int32 uint32;
typedef unsigned __int64 uint64;

typedef unsigned __int64 memsize_int;
//typedef intptr_t memsize_int;

#define strtoll _strtoi64

/* Short term put these here, long term separate out */
#include "sora_threads.h"
#include "sora_thread_queues.h"

#else


#define PSORA_UTHREAD_PROC void

// No need for this with c++
//typedef int bool;

typedef unsigned char  uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned long long int uint64;

// Stackoverflow says we should use intptr_t on gcc
// but it is not 64 bit!
// So we use long long and we have -Wpointer-to-int-cast warnings
//typedef intptr_t memsize_int;
typedef unsigned long long int memsize_int;


// This should create aligned static allocation
// TODO: Figure out how to do this in gcc. For now empty
#define MEM_ALIGN(n) 

#endif


#define ORIGIN(s)


