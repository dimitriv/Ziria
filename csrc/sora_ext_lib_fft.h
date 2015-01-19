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


// Twiddle constants for x3 FFTs
__declspec(selectany) __declspec(align(16)) extern const
COMPLEX16 wFFTLUT12_1[] =
{
	/*0*/{ 32767, 0 },
	/*1*/{ 28378, -16384 },
	/*2*/{ 16384, -28378 },
	/*3*/{ 0, -32768 }
};
__declspec(selectany) __declspec(align(16)) extern const
COMPLEX16 wFFTLUT12_2[] =
{
	/*0*/{ 32767, 0 },
	/*2*/{ 16384, -28378 },
	/*4*/{ -16384, -28378 },
	/*6*/{ -32767, -0 }
};

__declspec(selectany) __declspec(align(16)) extern const
COMPLEX16 wFFTLUT24_1[] =
{
	/*0*/{ 16384, 0 },
	/*1*/{ 31651, -8481 },
	/*2*/{ 28378, -16384 },
	/*3*/{ 23170, -23170 },
	/*4*/{ 16384, -28378 },
	/*5*/{ 8481, -31651 },
	/*6*/{ 0, -32767 },
	/*7*/{ -8481, -31651 }
};
__declspec(selectany) __declspec(align(16)) extern const
COMPLEX16 wFFTLUT24_2[] =
{
	/*0*/{ 32767, 0 },
	/*2*/{ 28378, -16384 },
	/*4*/{ 16384, -28378 },
	/*6*/{ 0, -32767 },
	/*0*/{ -10240, -17736 },
	/*2*/{ -17736, -10240 },
	/*4*/{ -20480, 0},
	/*6*/{ -17736, 10240 }
};


template<> DSP_INLINE const vcs* FFT_GetTwiddleConst<12, 1>() { return (vcs*)wFFTLUT12_1; }
template<> DSP_INLINE const vcs* FFT_GetTwiddleConst<12, 2>() { return (vcs*)wFFTLUT12_2; }
template<> DSP_INLINE const vcs* FFT_GetTwiddleConst<24, 1>() { return (vcs*)wFFTLUT24_1; }
template<> DSP_INLINE const vcs* FFT_GetTwiddleConst<24, 2>() { return (vcs*)wFFTLUT24_2; }



// Sorting maps for x3 FFTs
__declspec(selectany) extern const short FFT12LUTMap[] =
{
	0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11
};
__declspec(selectany) extern const short FFT24LUTMap[] =
{
	0, 8, 16, 1, 9, 17, 2, 10, 18, 3, 11, 19, 4, 12, 20, 5, 13, 21, 6, 14, 22, 7, 15, 23
};
template<> DSP_INLINE short FFTLUTMapTable<12>(int i) { return FFT12LUTMap[i]; }
template<> DSP_INLINE short FFTLUTMapTable<24>(int i) { return FFT24LUTMap[i]; }


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

