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
#include <vector128.h>
#include <fft.h>


// Twiddle and butteffly constants for x3 FFTs
#include "sora_ext_lib_fft_coeffs.h"





// General radix-3 breakdown
template<int N>
DSP_INLINE1 void FFTSSE_3(vcs* pInput)
{
	const int INPUT_SHIFT = 2;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;

	// TBD: multiplication with k1 and k2 can be further optimized since k1 = conj(k2) 
	__declspec(align(16)) const COMPLEX16 k1[] =
	{
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 }
	};
	__declspec(align(16)) const COMPLEX16 k2[] =
	{
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 }
	};


	vcs *pi = pInput;
	for (int n = 0; n < nArray / 3; n++, pi++)
	{
		vcs a = shift_right(pi[0], INPUT_SHIFT);
		vcs b = shift_right(pi[nArray / 3], INPUT_SHIFT);
		vcs c = shift_right(pi[nArray / 3 * 2], INPUT_SHIFT);

		vcs bk1 = mul_shift(b, k1, OUTPUT_SHIFT);
		vcs ck2 = mul_shift(c, k2, OUTPUT_SHIFT);
		vcs bk2 = mul_shift(b, k2, OUTPUT_SHIFT);
		vcs ck1 = mul_shift(c, k1, OUTPUT_SHIFT);

		// Calc X(3k) Start
		pi[0] = saturated_add(saturated_add(a, b), c);

		// Calc X(3k+1) Start
		vcs x2 = saturated_add(saturated_add(a, bk1), ck2);
		pi[nArray / 3] = mul_shift(x2, FFT_GetTwiddleConst<N, 1>()[n], OUTPUT_SHIFT);

		// Calc X(4k+1) Start
		vcs x4 = saturated_add(saturated_add(a, bk2), ck1);
		pi[nArray / 3 * 2] = mul_shift(x4, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);

	}
}


// Specialized cases
template<>
DSP_INLINE void
FFTSSEEx<12>(vcs* pInput)
{
	const int nArray = 12 / vcs::size;

	FFTSSE_3<12>(pInput);
	FFTSSEEx<4>(pInput);
	FFTSSEEx<4>(pInput + nArray / 3);
	FFTSSEEx<4>(pInput + nArray / 3 * 2);
}

template<>
DSP_INLINE void
FFTSSEEx<24>(vcs* pInput)
{
	const int nArray = 24 / vcs::size;

	FFTSSE_3<24>(pInput);
	FFTSSEEx<8>(pInput);
	FFTSSEEx<8>(pInput + nArray / 3);
	FFTSSEEx<8>(pInput + nArray / 3 * 2);
}



// IFFT

// General radix-3 breakdown
template<int N>
DSP_INLINE1 void IFFTSSE_3(vcs* pInput)
{
	const int INPUT_SHIFT = 2;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;

	// TBD: multiplication with k1 and k2 can be further optimized since k1 = conj(k2) 
	__declspec(align(16)) const COMPLEX16 k1[] =
	{
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 }
	};
	__declspec(align(16)) const COMPLEX16 k2[] =
	{
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 }
	};


	vcs *pi = pInput;
	for (int n = 0; n < nArray / 3; n++, pi++)
	{
		vcs a = shift_right(pi[0], INPUT_SHIFT);
		vcs b = shift_right(pi[nArray / 3], INPUT_SHIFT);
		vcs c = shift_right(pi[nArray / 3 * 2], INPUT_SHIFT);

		vcs bk1 = mul_shift(b, k1, OUTPUT_SHIFT);
		vcs ck2 = mul_shift(c, k2, OUTPUT_SHIFT);
		vcs bk2 = mul_shift(b, k2, OUTPUT_SHIFT);
		vcs ck1 = mul_shift(c, k1, OUTPUT_SHIFT);

		// Calc X(3k) Start
		pi[0] = saturated_add(saturated_add(a, b), c);

		// Calc X(3k+1) Start
		vcs x2 = saturated_add(saturated_add(a, bk1), ck2);
		pi[nArray / 3] = conj_mul_shift(x2, FFT_GetTwiddleConst<N, 1>()[n], OUTPUT_SHIFT);

		// Calc X(4k+1) Start
		vcs x4 = saturated_add(saturated_add(a, bk2), ck1);
		pi[nArray / 3 * 2] = conj_mul_shift(x4, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);

	}
}

// Specialized cases
template<>
DSP_INLINE void
IFFTSSEEx<12>(vcs* pInput)
{
	const int nArray = 12 / vcs::size;

	IFFTSSE_3<12>(pInput);
	IFFTSSEEx<4>(pInput);
	IFFTSSEEx<4>(pInput + nArray / 3);
	IFFTSSEEx<4>(pInput + nArray / 3 * 2);
}

template<>
DSP_INLINE void
IFFTSSEEx<24>(vcs* pInput)
{
	const int nArray = 24 / vcs::size;

	IFFTSSE_3<24>(pInput);
	IFFTSSEEx<8>(pInput);
	IFFTSSEEx<8>(pInput + nArray / 3);
	IFFTSSEEx<8>(pInput + nArray / 3 * 2);
}

