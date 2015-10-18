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

#ifdef SORA_PLATFORM
/////////////////////////////////////////////////

#ifdef __GNUC__
#include <stdint.h>
typedef int8_t num8;
typedef int16_t num16;
typedef int32_t num32;
typedef int64_t num64;
typedef uint8_t unum8;
typedef uint16_t unum16;
typedef uint32_t unum32;
typedef uint64_t unum64;
#define calign __atribute__(aligned(16))
#define cthread __thread
// NOTE: __attribute__ ((weak)) is NOT the same as __declspec(selectany) and causes tests to fail!
//#define cselectany  __attribute__ ((weak))
#define cselectany 
#else
typedef __int8  num8;
typedef __int16 num16;
typedef __int32 num32;
typedef __int64 num64;
typedef unsigned __int8 unum8;
typedef unsigned __int16 unum16;
typedef unsigned __int32 unum32;
typedef unsigned __int64 unum64;
#define calign __declspec(align(16))
#define cthread __declspec(thread)
#define cselectany __declspec(selectany)
#endif


// INLINE_COMPILATION only defined in Wrapper.cpp
#ifdef INLINE_COMPILATION 
#define FORCE_INLINE __forceinline
#else 
#define FORCE_INLINE 
#endif

#else
////////////////////////////////////////////////
typedef char          num8;
typedef short         num16;
typedef int           num32;
typedef long long int num64; 

typedef unsigned char          unum8;
typedef unsigned short         unum16;
typedef unsigned int           unum32;
typedef unsigned long long int unum64; 

#define calign
#define cthread

#ifdef INLINE_COMPILATION
#ifdef __GNUC__
#define FORCE_INLINE __attribute__((always_inline))
#define FINL __attribute__((always_inline))
#else
#define FORCE_INLINE __inline
#define FINL __inline
#endif
#else 
#define FORCE_INLINE 
#define FINL 
#endif

////////////////////////////////////////////////

#endif


typedef enum __num_siz {
  NUM8  = 1,
  NUM16 = 2,
  NUM32 = 4
} num_siz;

typedef struct complex8 {
  num8 re;
  num8 im;
} complex8;

typedef struct complex16 {
  num16 re;
  num16 im;
} complex16;

typedef struct complex32 {
  num32 re;
  num32 im;
} complex32;

typedef struct complex64 {
  num64 re;
  num64 im;
} complex64;

struct complex8 complex8_plus(struct complex8 x, struct complex8 y);
struct complex8 complex8_minus(struct complex8 x, struct complex8 y);
struct complex8 complex8_mult(struct complex8 x, struct complex8 y);
struct complex8 complex8_div(struct complex8 x, struct complex8 y);
num8 complex8_creal(struct complex8 x);
num8 complex8_cimag(struct complex8 x);

struct complex16 complex16_plus(struct complex16 x, struct complex16 y);
struct complex16 complex16_minus(struct complex16 x, struct complex16 y);
struct complex16 complex16_mult(struct complex16 x, struct complex16 y);
struct complex16 complex16_div(struct complex16 x, struct complex16 y);
num16 complex16_creal(struct complex16 x);
num16 complex16_cimag(struct complex16 x);

struct complex32 complex32_plus(struct complex32 x, struct complex32 y);
struct complex32 complex32_minus(struct complex32 x, struct complex32 y);
struct complex32 complex32_mult(struct complex32 x, struct complex32 y);
struct complex32 complex32_div(struct complex32 x, struct complex32 y);
num32 complex32_creal(struct complex32 x);
num32 complex32_cimag(struct complex32 x);

struct complex64 complex64_plus(struct complex64 x, struct complex64 y);
struct complex64 complex64_minus(struct complex64 x, struct complex64 y);
struct complex64 complex64_mult(struct complex64 x, struct complex64 y);
struct complex64 complex64_div(struct complex64 x, struct complex64 y);
num64 complex64_creal(struct complex64 x);
num64 complex64_cimag(struct complex64 x);

/* Up-casting */
num16 num8to16(num8 x);
num32 num8to32(num8 x);
num64 num8to64(num8 x);

num32 num16to32(num16 x);
num64 num16to64(num16 x);

num64 num32to64(num32 x);

struct complex16 complex8_to_complex16(struct complex8 x);
struct complex32 complex8_to_complex32(struct complex8 x);
struct complex64 complex8_to_complex64(struct complex8 x);

struct complex32 complex16_to_complex32(struct complex16 x);
struct complex64 complex16_to_complex64(struct complex16 x);

struct complex64 complex32_to_complex64(struct complex32 x);

/* Down-casting */
num8 num16to8(num16 x);
num8 num32to8(num32 x);
num8 num64to8(num64 x);

num16 num32to16(num32 x);
num16 num64to16(num64 x);

num32 num64to32(num64 x);


struct complex32 complex64_to_complex32(struct complex64 x);

struct complex16 complex64_to_complex16(struct complex64 x);
struct complex16 complex32_to_complex16(struct complex32 x);

struct complex8 complex64_to_complex8(struct complex64 x);
struct complex8 complex32_to_complex8(struct complex32 x);
struct complex8 complex16_to_complex8(struct complex16 x);

