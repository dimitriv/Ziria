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

#ifdef __GNUC__
#include "ieee80211const.h"
#include <stdio.h>
#include <stdlib.h>
#ifndef __ARM_NEON__
#include <emmintrin.h>
#include "viterbicore.hpp"
#else
#include "viterbicore.h"
#endif
#else
#include <sora.h>
#include <viterbicore.h>
#include <ieee80211const.h>
#endif

//#include "ieee80211facade.hpp"
#include "types.h"
#include "numerics.h"

// Viterbi brick template parameters:
const size_t TRELLIS_MAX = 5000 * 8;
TViterbiCore<TRELLIS_MAX> m_viterbi;

unsigned long ob_count;
unum16 frame_length = 1500;
num16 code_rate = CR_12;
size_t TRELLIS_DEPTH = 256;
unsigned char *m_outbuf = NULL;

FORCE_INLINE
int __ext_viterbi_brick_init_fast(int32 frame_len, int16 code_r, int16 depth) {
	ob_count = 0;
	m_viterbi.Reset();
	frame_length = frame_len;
	code_rate = code_r;
	if (m_outbuf != NULL) free((void *)m_outbuf);
	TRELLIS_DEPTH = (size_t) depth;
	m_outbuf = (unsigned char*) malloc((size_t) (TRELLIS_DEPTH + 1));
	if (m_outbuf == NULL)
	{
	    printf("Viterbi memory allocation failure!\n");
	    exit(1);
	}
	return 0;
}


FORCE_INLINE
int16 __ext_viterbi_brick_decode_fast(signed char* intInput, int len1, unsigned char* bit, int len2)
{
	static const int trellis_prefix = 6; 	 // 6 bit zero prefix
	static const int trellis_lookahead = 24;

	//uchar  m_outbuf[TRELLIS_DEPTH / 8 + 1];


	unsigned int total_byte_count = 0;

	// vector128 constants
#if defined(__GNUC__) && !defined(__ARM_NEON__)
	static const __m128i * const pVITMA = VIT_MA; // Branch Metric A
	static const __m128i * const pVITMB = VIT_MB; // Branch Metric B
#else
	static const vub * const pVITMA = (const vub*)VIT_MA; // Branch Metric A
	static const vub * const pVITMB = (const vub*)VIT_MB; // Branch Metric B
#endif


	//uchar* input = ipin.peek();
	unsigned char* input = (unsigned char*)intInput;
	//uchar* input_end = input + NUM_INPUT;
	unsigned char* input_end = input + len1;
	while (input < input_end) {
		// advance branch
		if (code_rate == CR_12)
		{
			m_viterbi.BranchACS(pVITMA, input[0], pVITMB, input[1]);
			input += 2; // jump to data
		}
		else if (code_rate == CR_34)
		{
			m_viterbi.BranchACS(pVITMA, input[0], pVITMB, input[1]);
			m_viterbi.BranchACS(pVITMA, input[2]);
			m_viterbi.BranchACS(pVITMB, input[3]);
			input += 4;
		}
		else if (code_rate == CR_23)
		{
			m_viterbi.BranchACS(pVITMA, input[0], pVITMB, input[1]);
			m_viterbi.BranchACS(pVITMA, input[2]);
			input += 3;
		}

		unsigned int tr_index = m_viterbi.trellis_index();

		if ((tr_index & 7) == 0) {
			m_viterbi.Normalize();
		}

		// check trace_back
		unsigned int output_count = 0;
		unsigned int lookahead = 0;
		const unsigned int tr_index_end = frame_length * 8 + trellis_prefix;
		if (tr_index >= tr_index_end) {
			// all bytes have been processed - track back all of them
			output_count = tr_index_end - ob_count
				- trellis_prefix;
			lookahead = tr_index - tr_index_end;
		}
		else if (tr_index >= ob_count + TRELLIS_DEPTH + trellis_lookahead + trellis_prefix)
		{
			// trace back partially

			// Note: tr_index increase during 'BranchACS', to decode out complete bytes, we may lookahead the
			// trellis without output a few more than 'trellis_lookahead'
			unsigned int remain = (tr_index - (ob_count + TRELLIS_DEPTH + trellis_lookahead + trellis_prefix)) % 8;
			output_count = TRELLIS_DEPTH;
			lookahead = trellis_lookahead + remain;
		}

		if (output_count) {
			m_viterbi.Traceback((char*)m_outbuf, output_count, lookahead);
			ob_count += output_count;

			unsigned int last_byte_count = 0;
			while (last_byte_count < output_count / 8) {
				bit[total_byte_count] = m_outbuf[last_byte_count];
				last_byte_count++;
				total_byte_count++;
			}
		}

	}
	return total_byte_count * 8;
}




FORCE_INLINE
int __ext_viterbiSig11a_brick_init_fast(int32 frame_len, int16 code_r, int16 depth) {
	ob_count = 0;
	m_viterbi.Reset();
	frame_length = frame_len;
	code_rate = code_r;
	if (m_outbuf != NULL) free((void *)m_outbuf);
	TRELLIS_DEPTH = (size_t)depth;
	m_outbuf = (unsigned char*)malloc((size_t)(TRELLIS_DEPTH / 8 + 1));
	if (m_outbuf == NULL)
	{
		printf("Viterbi memory allocation failure!\n");
		exit(1);
	}
	return 0;
}


FORCE_INLINE
int16 __ext_viterbiSig11a_brick_decode_fast(signed char* intInput, int len1, unsigned char* bit, int len2)
{
	static const int state_size = 64;
	static const int input_size = 48; // always 48 soft-values

#if defined(__GNUC__) && !defined(__ARM_NEON__)
	__m128i trellis[state_size / 16 * input_size];
#else
	vub trellis[state_size / 16 * input_size];
#endif

	unsigned int output = 0;

	Viterbi_sig11(trellis, intInput, (char *)(bit));
	*((unum32 *)bit) >>= 6; // remove the prefix 6 zeros

	return 0;
}
