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
#include <sora.h>
#include "viterbicore.h"
#include <ieee80211const.h>
//#include "ieee80211facade.hpp"
#include "types.h"



// Viterbi brick template parameters:
const size_t TRELLIS_MAX = 5000 * 8;
TViterbiCore<TRELLIS_MAX> m_viterbi;

ulong ob_count;
uint16 frame_length = 1500;
int16 code_rate = CR_12;
size_t TRELLIS_DEPTH = 256;
uchar *m_outbuf = NULL;

FORCE_INLINE
int __ext_viterbi_brick_init_fast(int32 frame_len, int16 code_r, int16 depth) {
	ob_count = 0;
	m_viterbi.Reset();
	frame_length = frame_len;
	code_rate = code_r;
	if (m_outbuf != NULL) free((void *)m_outbuf);
	TRELLIS_DEPTH = (size_t) depth;
	m_outbuf = (uchar*) malloc((size_t) (TRELLIS_DEPTH + 1));
	if (m_outbuf == NULL)
	{
	    printf("Viterbi memory allocation failure!\n");
	    exit(1);
	}
	return 0;
}


FORCE_INLINE
int16 __ext_viterbi_brick_decode_fast(char* intInput, int len1, uchar* bit, int len2)
{
	static const int trellis_prefix = 6; 	 // 6 bit zero prefix
	static const int trellis_lookahead = 24;

	//uchar  m_outbuf[TRELLIS_DEPTH / 8 + 1];


	uint total_byte_count = 0;

	// vector128 constants
	static const vub * const pVITMA = (const vub*)VIT_MA; // Branch Metric A
	static const vub * const pVITMB = (const vub*)VIT_MB; // Branch Metric B


	//uchar* input = ipin.peek();
	uchar* input = (uchar*)intInput;
	//uchar* input_end = input + NUM_INPUT;
	uchar* input_end = input + len1;
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

		uint tr_index = m_viterbi.trellis_index();

		if ((tr_index & 7) == 0) {
			m_viterbi.Normalize();
		}

		// check trace_back
		uint output_count = 0;
		uint lookahead = 0;
		const uint tr_index_end = frame_length * 8 + trellis_prefix;
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
			uint remain = (tr_index - (ob_count + TRELLIS_DEPTH + trellis_lookahead + trellis_prefix)) % 8;
			output_count = TRELLIS_DEPTH;
			lookahead = trellis_lookahead + remain;
		}

		if (output_count) {
			m_viterbi.Traceback((char*)m_outbuf, output_count, lookahead);
			ob_count += output_count;

			uint last_byte_count = 0;
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
	m_outbuf = (uchar*)malloc((size_t)(TRELLIS_DEPTH / 8 + 1));
	if (m_outbuf == NULL)
	{
		printf("Viterbi memory allocation failure!\n");
		exit(1);
	}
	return 0;
}


FORCE_INLINE
int16 __ext_viterbiSig11a_brick_decode_fast(char* intInput, int len1, uchar* bit, int len2)
{
	static const int state_size = 64;
	static const int input_size = 48; // always 48 soft-values

	vub   trellis[state_size / 16 * input_size];
	uint output = 0;

	Viterbi_sig11(trellis, (char *)intInput, (char *)(bit));
	*((uint *)bit) >>= 6; // remove the prefix 6 zeros

	return 0;
}
