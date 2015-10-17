/*

Modified from Microsoft/Sora on Github.

Original license:



Microsoft Research Software Radio

Copyright (c) Microsoft Corporation

All rights reserved.

BSD License

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ""AS IS""
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#ifdef __GNUC__


#pragma once

#include <emmintrin.h>

#include "numerics.h"

#ifdef __SSSE3__
#include <tmmintrin.h>
#endif

// hepler to reverse vector for taking endianness into account
static inline __m128i
_mm_bswap_si128 (__m128i x)
{
	// Reverse order of all bytes in the 128-bit word.

#ifdef __SSSE3__
	return _mm_shuffle_epi8(x,
		_mm_set_epi8(
			 0,  1,  2,  3,
			 4,  5,  6,  7,
			 8,  9, 10, 11,
			12, 13, 14, 15));
#else
	// Swap bytes in each 16-bit word:
	__m128i a = _mm_or_si128(
		_mm_slli_epi16(x, 8),
		_mm_srli_epi16(x, 8));

	// Reverse all 16-bit words in 64-bit halves:
	a = _mm_shufflelo_epi16(a, _MM_SHUFFLE(0, 1, 2, 3));
	a = _mm_shufflehi_epi16(a, _MM_SHUFFLE(0, 1, 2, 3));

	// Reverse 64-bit halves:
	return _mm_shuffle_epi32(a, _MM_SHUFFLE(1, 0, 3, 2));
#endif
}


extern const __m128i ALL_INVERSE_ONE =
  _mm_bswap_si128(
  _mm_set_epi8 ( num8(0xFE), num8(0xFE), num8(0xFE), num8(0xFE), 
                 num8(0xFE), num8(0xFE), num8(0xFE), num8(0xFE),
                 num8(0xFE), num8(0xFE), num8(0xFE), num8(0xFE), 
                 num8(0xFE), num8(0xFE), num8(0xFE), num8(0xFE) )
);

extern const __m128i ALL_ONE =
  _mm_bswap_si128(
  _mm_set_epi8 ( 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
                 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01 )
);

extern const __m128i ALL_M128 =
  _mm_bswap_si128(
  _mm_set_epi8 ( num8(0x80), num8(0x80), num8(0x80), num8(0x80), 
                 num8(0x80), num8(0x80), num8(0x80), num8(0x80),
                 num8(0x80), num8(0x80), num8(0x80), num8(0x80), 
                 num8(0x80), num8(0x80), num8(0x80), num8(0x80) )
);

extern const __m128i ALL_INIT =
  _mm_bswap_si128(
  _mm_set_epi8 ( 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
                 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30 )
);

extern const __m128i ALL_INIT0 =
  _mm_bswap_si128(
  _mm_set_epi8( 0x00, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
                0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30 )
);

calign extern const __m128i INDEXES[4] = {
  _mm_bswap_si128(
  _mm_set_epi8 ( 0x00, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x18, 0x1C,
                 0x20, 0x24, 0x28, 0x2C, 0x30, 0x34, 0x38, 0x3C )
),
     
  _mm_bswap_si128(
  _mm_set_epi8 ( 0x40, 0x44, 0x48, 0x4C, 0x50, 0x54, 0x58, 0x5C,
                 0x60, 0x64, 0x68, 0x6C, 0x70, 0x74, 0x78, 0x7C )
),
     
  _mm_bswap_si128(
  _mm_set_epi8 ( num8(0x80), num8(0x84), num8(0x88), num8(0x8C), 
                 num8(0x90), num8(0x94), num8(0x98), num8(0x9C),
                 num8(0xA0), num8(0xA4), num8(0xA8), num8(0xAC), 
                 num8(0xB0), num8(0xB4), num8(0xB8), num8(0xBC) ) 
),
     
  _mm_bswap_si128(
  _mm_set_epi8 ( num8(0xC0), num8(0xC4), num8(0xC8), num8(0xCC), 
                 num8(0xD0), num8(0xD4), num8(0xD8), num8(0xDC),
                 num8(0xE0), num8(0xE4), num8(0xE8), num8(0xEC), 
                 num8(0xF0), num8(0xF4), num8(0xF8), num8(0xFC) )
)

};

//extern const unum8 __attribute__((weak)) VIT_MA[64][8 * SOFT_RANGE] =
extern const __m128i VIT_MA[64] = {
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  6,  8,  8,  6,  8,  6,  8,  6,  8,  6,  6,  8,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  8,  6,  6,  8,  6,  8,  6,  8,  6,  8,  8,  6,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10,  4, 10, 10,  4, 10,  4, 10,  4, 10,  4,  4, 10,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4, 10,  4,  4, 10,  4, 10,  4, 10,  4, 10, 10,  4, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12,  2, 12, 12,  2, 12,  2, 12,  2, 12,  2,  2, 12,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2, 12,  2,  2, 12,  2, 12,  2, 12,  2, 12, 12,  2, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14,  0, 14, 14,  0, 14,  0, 14,  0, 14,  0,  0, 14,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0, 14,  0,  0, 14,  0, 14,  0, 14,  0, 14, 14,  0, 14,  0)
),
};

//extern const unum8 __attribute__((weak)) VIT_MB[16*64] = {
extern const __m128i VIT_MB[64] = {
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2) 
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 8,  6,  6,  8,  6,  8,  8,  6,  6,  8,  8,  6,  8,  6,  6,  8)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 6,  8,  8,  6,  8,  6,  6,  8,  8,  6,  6,  8,  6,  8,  8,  6)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (10,  4,  4, 10,  4, 10, 10,  4,  4, 10, 10,  4, 10,  4,  4, 10)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 4, 10, 10,  4, 10,  4,  4, 10, 10,  4,  4, 10,  4, 10, 10,  4)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (12,  2,  2, 12,  2, 12, 12,  2,  2, 12, 12,  2, 12,  2,  2, 12)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 2, 12, 12,  2, 12,  2,  2, 12, 12,  2,  2, 12,  2, 12, 12,  2)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
),
  _mm_bswap_si128(
  _mm_set_epi8 (14,  0,  0, 14,  0, 14, 14,  0,  0, 14, 14,  0, 14,  0,  0, 14)
),
  _mm_bswap_si128(
  _mm_set_epi8 ( 0, 14, 14,  0, 14,  0,  0, 14, 14,  0,  0, 14,  0, 14, 14,  0)
)
};

#endif


