/*
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

This file is taken from https://github.com/Microsoft/Sora
*/
#pragma once
#include "numerics.h"
#include "sora_ext_lib_fft_coeffs.h"

//////////////////////////////////////////////////
// First stage of FFT Radix-4 decimation-in-frequency algorithm 
// with input in normal order and output in bit-reversed order
/////////////////////////////////////////////////

__m128i mul_shiftx(const __m128i &a, const __m128i &b, int nbits_right);

__m128i mul_jx(const __m128i &a);


template<int N>
DSP_INLINE1 void FFTSSE(struct complex16* pInput)
{
    const int INPUT_SHIFT = 2;
    const int OUTPUT_SHIFT = 15;

    struct complex16 *pi = pInput;
	for (int n = 0; n < N / 4; n += 4)
    {
		__m128i a = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + n)), INPUT_SHIFT);
		__m128i b = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 4 + n)), INPUT_SHIFT);
		__m128i c = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 2 + n)), INPUT_SHIFT);
		__m128i d = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 4 * 3 + n)), INPUT_SHIFT);

		__m128i ac = _mm_adds_epi16(a, c);
		__m128i bd = _mm_adds_epi16(b, d);
		__m128i a_c = _mm_subs_epi16(a, c);
		__m128i b_d = _mm_subs_epi16(b, d);

        // Calc X(4k) Start
		_mm_store_si128((__m128i *) (pi + n), _mm_adds_epi16(ac, bd));
		
		
		// Calc X(4k+2) Start
        __m128i x2 = _mm_subs_epi16(ac, bd);
		__m128i twd = _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 2>() + 2 * n));
		_mm_store_si128((__m128i *) (pi + N / 4 + n), mul_shiftx(x2, twd, OUTPUT_SHIFT));
		

        // Calc X(4k+1) Start
        __m128i jb_d = mul_jx(b_d);
		__m128i x4 = _mm_subs_epi16(a_c, jb_d);
		 twd = _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 1>() + 2 * n));
		_mm_store_si128((__m128i *) (pi + N / 2 +  n), mul_shiftx(x4, twd, OUTPUT_SHIFT));
		

        // Calc X(4k+3) Start
		__m128i x5 = _mm_adds_epi16(a_c, jb_d);
		 twd = _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 3>() + 2 * n));
		_mm_store_si128((__m128i *) (pi + N / 4 * 3 +  n), mul_shiftx(x5, twd, OUTPUT_SHIFT));

		
    }
}

template<int N>
DSP_INLINE1 void
FFTSSEEx(struct complex16* pInput)
{
   
    FFTSSE<N> (pInput);
    FFTSSEEx<N/4> (pInput);
    FFTSSEEx<N/4> (pInput + N / 4);
    FFTSSEEx<N/4> (pInput + N / 2);
    FFTSSEEx<N/4> (pInput + N / 4 * 3);
}

template<>
DSP_INLINE void
FFTSSEEx<4>(struct complex16* pInput)
{

	const int INPUT_SHIFT = 2;
    
	__m128i xmm0 = _mm_srai_epi16(_mm_load_si128((__m128i *)pInput), INPUT_SHIFT); // xmm0 = I0 Q0 I1 Q1 I2 Q2 I3 Q3
	
	__m128i xmm2 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(1, 0, 3, 2)); // xmm2 = I2 Q2 I3 Q3 I0 Q0 I1 Q1
	__m128i xmm5 = _mm_set_epi32( 0xFFFFFFFF, 0xFFFFFFFF,0x00000000, 0x00000000); // 0x0000000000000000FFFFFFFFFFFFFFFF
	xmm0 = _mm_xor_si128(xmm0, xmm5);								// xmm0 = I0 Q0 I1 Q1 -I2 -Q2 -I3 -Q3
	xmm0 = _mm_adds_epi16(xmm0, xmm2);							// xmm0 = I0+I2 Q0+Q2 I1+I3 Q1+Q3 I0-I2 Q0-Q2 I1-I3 Q1-Q3

	xmm5 = _mm_set_epi32(0xFFFF0000, 0x00000000, 0x00000000, 0x00000000); //0x0000000000000000000000000000FFFF
	xmm0 = _mm_xor_si128(xmm0, xmm5);			              // xmm0 = I0+I2 Q0+Q2 I1+I3 Q1+Q3 I0-I2 Q0-Q2 I1-I3 Q3-Q1
	
	xmm0 = _mm_shufflehi_epi16(xmm0, _MM_SHUFFLE(1, 0, 2, 3)); // xmm0 = I0+I2 Q0+Q2 I1+I3 Q1+Q3 Q3-Q1 I1-I3 I0-I2 Q0-Q2 
	xmm2 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2, 3, 0, 1));   // xmm2 =  I1+I3 Q1+Q3 I0+I2 Q0+Q2 I0-I2 Q0-Q2 Q3-Q1 I1-I3
	
	
	xmm5 = _mm_set_epi32( 0x00000000,0xFFFFFFFF, 0xFFFFFFFF, 0x00000000); //0x000000000FFFFFFFFFFFFFFFF00000000
	xmm0 = _mm_xor_si128(xmm0, xmm5);						  // xmm0 = I0+I2 Q0+Q2 -I1-I3 -Q1-Q3 Q1-Q3 I3-I1 I0-I2 Q0-Q2
																

	// xmm0 =  I0+I2 Q0+Q2 -I1-I3 -Q1-Q3 Q1-Q3 I3-I1 I0-I2 Q0-Q2
	// xmm2 =  I1+I3 Q1+Q3  I0+I2  Q0+Q2 I0-I2 Q0-Q2 Q3-Q1 I1-I3
	// slightly different order than expected, but dealt with by the butterfly at the end.
		_mm_store_si128((__m128i *) pInput, _mm_adds_epi16(xmm0, xmm2));
}

template<>
DSP_INLINE1 void
FFTSSEEx<8>(struct complex16* pInput)
{
    const int INPUT_SHIFT = 3;
    const int OUTPUT_SHIFT = 15;

	//vcs xmm0 = shift_right(pi[0], INPUT_SHIFT);     // xmm0 = a
	//vcs xmm1 = shift_right(pi[1], INPUT_SHIFT);		// xmm1 = b
	__m128i xmm0 = _mm_srai_epi16(_mm_load_si128((__m128i *)pInput), INPUT_SHIFT);
	__m128i xmm1 = _mm_srai_epi16(_mm_load_si128((__m128i *)(pInput + 4)), INPUT_SHIFT);
	

    //vcs xmm2 = saturated_sub(xmm0, xmm1); // xmm2 = I0-I4 Q0-Q4 I1-I5 Q1-Q5 I2-I6 Q2-Q6 I3-I7 Q3-Q7            // xmm2 = a - b
    //xmm0 = saturated_add(xmm0, xmm1);	  // xmm2 = I0+I4 Q0+Q4 I1+I5 Q1+Q5 I2+I6 Q2+Q6 I3+I7 Q3+Q7				// xmm0 = a + b, for 4-point FFT
	__m128i xmm2 = _mm_subs_epi16(xmm0, xmm1);
	xmm0 = _mm_adds_epi16(xmm0, xmm1);

	//vcs xmm3 = permutate_high<0xb1>(xmm2); // xmm3 = I3 Q3 I2 Q2 Q1 I1 Q0 I0
	//vcs xmm5 = vector128_consts::__0xFFFF0000FFFF00000000000000000000<vcs>();
	__m128i xmm3 = _mm_shufflehi_epi16(xmm2, _MM_SHUFFLE(2, 3, 0, 1));
	__m128i xmm5 = _mm_set_epi32(0xFFFF0000, 0xFFFF0000, 0x00000000, 0x00000000);
	

    //xmm3 = xor(xmm3, xmm5);                             // xmm3 = -I3 Q3 -I2 Q2 Q1 I1 Q0 I0 = (a-b).lower34*-j
	//xmm5 = permutate<0x4e>(xmm3);                     // xmm5 = Q1 I1 Q0 I0 I3 Q3 I2 Q2, xmm3 = Q3 I3 Q2 I2 Q1 I1 Q0 I0
	xmm3 = _mm_xor_si128(xmm3, xmm5);
	xmm5 = _mm_shuffle_epi32(xmm3, _MM_SHUFFLE(1, 0, 3, 2));// xmm0 and xmm3 store 4-point data

	//vcs xmm4 = vector128_consts::__0xFFFFFFFFFFFFFFFF0000000000000000<vcs>();
	//xmm3 = xor(xmm3, xmm4);                             // xmm3 = -Q3 -I3 -Q2 -I2 Q1 I1 Q0 I0
	//xmm3 = saturated_add(xmm3, xmm5);                  // xmm3 = xmm3 + xmm5
	__m128i xmm4 = _mm_set_epi32( 0xFFFFFFFF, 0xFFFFFFFF,0x0, 0x0);
	xmm3 = _mm_xor_si128(xmm3, xmm4);
    xmm3 = _mm_adds_epi16(xmm3, xmm5);

    //xmm1 = mul_shift(xmm3, FFT_GetTwiddleConst<8, 1>()[0], OUTPUT_SHIFT);                 // lower multiplied by wLUT
	const __m128i TwiddleConst8 = _mm_set_epi16(-23169, -23169, 0, 32767, -23169, 23169, 0, 32767);
	xmm1 = mul_shiftx(xmm3,TwiddleConst8 , OUTPUT_SHIFT);                 // lower multiplied by wLUT

    //xmm3 = permutate<0xc8>(xmm4);                     // xmm3 = 0xFFFFFFFF00000000FFFFFFFF00000000
	//xmm2 = permutate<0xb1>(xmm1);                     // xmm2 = I2 Q2 I3 Q3 I0 Q0 I1 Q1
	//xmm1 = xor(xmm1, xmm3);                             // xmm1 = -Q3 -I3 Q2 I2 -Q1 -I1 Q0 I0
	//xmm1 = saturated_add(xmm1, xmm2);                  // 4-DFT over
	xmm3 = _mm_shuffle_epi32(xmm4, _MM_SHUFFLE(3, 1, 2, 0));
    xmm2 = _mm_shuffle_epi32(xmm1, _MM_SHUFFLE(2, 3, 0, 1));
    xmm1 = _mm_xor_si128(xmm1, xmm3);
    xmm1 = _mm_adds_epi16(xmm1, xmm2);

    //xmm5 = permutate<0x4e>(xmm0);                     // xmm5 = Q1 I1 Q0 I0 Q3 I3 Q2 I2 xmm0 = Q3 I3 Q2 I2 Q1 I1 Q0 I0
	//xmm0 = xor(xmm0, xmm4);                             // xmm0 = -Q3 -I3 -Q2 -I2 Q1 I1 Q0 I0
	//xmm0 = saturated_add(xmm0, xmm5);                  // A-B A+B
	//xmm4 = vector128_consts::__0xFFFF0000000000000000000000000000<vcs>();
	xmm5 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(1, 0, 3, 2));
    xmm0 = _mm_xor_si128(xmm0, xmm4);
    xmm0 = _mm_adds_epi16(xmm0, xmm5);
	xmm4 = _mm_set_epi32(0xFFFF0000, 0x00000000, 0x00000000, 0x00000000);


	//xmm0 = permutate_high<0xb4>(xmm0);                // xmm0 = I3 Q3 Q2 I2 Q1 I1 Q0 I0
	//xmm0 = xor(xmm0, xmm4);                             // xmm0 = upper.4 * -j
	//xmm2 = permutate<0xb1>(xmm0);                     // xmm2 = I2 Q2 I3 Q3 I0  Q0 I1 Q1
	//xmm0 = xor(xmm0, xmm3);                             // xmm0 = -Q3 -I3 Q2 I2 -Q1 -I1 Q0 I0
	//xmm0 = saturated_add(xmm0, xmm2);                  // 4-FFT Over
	xmm0 = _mm_shufflehi_epi16(xmm0, _MM_SHUFFLE(2, 3, 1, 0));
    xmm0 = _mm_xor_si128(xmm0, xmm4);
	xmm2 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2, 3, 0, 1));
    xmm0 = _mm_xor_si128(xmm0, xmm3);
    xmm0 = _mm_adds_epi16(xmm0, xmm2);


    //pi[0] = xmm0;                                   // output upper 2 2-point DFT
	//pi[1] = xmm1;                                   // output lower2 2-point DFT
	_mm_store_si128((__m128i *) pInput, xmm0);
    _mm_store_si128((__m128i *) (pInput + 4), xmm1);
}

// Note: side-effect: pInput[] will be destroyed after calling
template<int N>
DSP_INLINE1 void FFT(struct complex16* pInput, struct complex16* pOutput)
{
    FFTSSEEx<N>(pInput);
    int i;
    for (i = 0; i < N; i++)
		((struct complex16*)pOutput)[i] = ((struct complex16*)pInput)[FFTLUTButterflyTable<N>(i)];

}

// Note: no side-effect
template<int N>
DSP_INLINE1 void FFTSafe(const struct complex16* pInput, struct complex16* pOutput)
{
	calign struct complex16 temp[N];
    memcpy(temp, pInput, sizeof(temp));
    FFT<N>(temp, pOutput);
}
