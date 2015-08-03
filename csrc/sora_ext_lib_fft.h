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


// Twiddle and butterfly constants for x3 FFTs
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

		// Calc X(3k+2) Start
		vcs x3 = saturated_add(saturated_add(a, bk2), ck1);
		pi[nArray / 3 * 2] = mul_shift(x3, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);

	}
}

// General radix-3 wrapper
template<int N>
DSP_INLINE1 void FFTSSE_3W(vcs* pInput)
{
	const int nArray = N / vcs::size;

	FFTSSE_3<N>(pInput);

	FFTSSEEx<N/3>(pInput);
	FFTSSEEx<N/3>(pInput + nArray / 3);
	FFTSSEEx<N/3>(pInput + nArray / 3 * 2);

}

// Specialized cases

template<>
DSP_INLINE void
FFTSSEEx<12>(vcs* pInput)
{
	FFTSSE_3W<12>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<24>(vcs* pInput)
{
	FFTSSE_3W<24>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<36>(vcs* pInput)
{
	FFTSSE_3W<36>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<72>(vcs* pInput)
{
	FFTSSE_3W<72>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<108>(vcs* pInput)
{
	FFTSSE_3W<108>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<216>(vcs* pInput)
{
	FFTSSE_3W<216>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<324>(vcs* pInput)
{
	FFTSSE_3W<324>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<648>(vcs* pInput)
{
	FFTSSE_3W<648>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<972>(vcs* pInput)
{
	FFTSSE_3W<972>(pInput);
}


// General radix-5 breakdown
template<int N>
DSP_INLINE1 void FFTSSE_5(vcs* pInput)
{
	const int INPUT_SHIFT = 3;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;

	
	__declspec(align(16)) const COMPLEX16 k1[] =
	{
		{ 10126, -31164 },
		{ 10126, -31164 },
		{ 10126, -31164 },
		{ 10126, -31164 },
	};
	__declspec(align(16)) const COMPLEX16 k2[] =
	{
		{ -26510, -19261 },
		{ -26510, -19261 },
		{ -26510, -19261 },
		{ -26510, -19261 },
	};
	__declspec(align(16)) const COMPLEX16 k3[] =
	{
		{ -26510, 19261 },
		{ -26510, 19261 },
		{ -26510, 19261 },
		{ -26510, 19261 },
	};
	__declspec(align(16)) const COMPLEX16 k4[] =
	{
		{ 10126, 31164 },
		{ 10126, 31164 },
		{ 10126, 31164 },
		{ 10126, 31164 },
	};

	vcs *pi = pInput;
	for (int n = 0; n < nArray / 5; n++, pi++)
	{
		vcs a = shift_right(pi[0], INPUT_SHIFT);
		vcs b = shift_right(pi[nArray / 5], INPUT_SHIFT);
		vcs c = shift_right(pi[nArray / 5 * 2], INPUT_SHIFT);
		vcs d = shift_right(pi[nArray / 5 * 3], INPUT_SHIFT);
		vcs e = shift_right(pi[nArray / 5 * 4], INPUT_SHIFT);

		vcs bk1 = mul_shift(b, k1, OUTPUT_SHIFT);
		vcs bk2 = mul_shift(b, k2, OUTPUT_SHIFT);
		vcs bk3 = mul_shift(b, k3, OUTPUT_SHIFT);
		vcs bk4 = mul_shift(b, k4, OUTPUT_SHIFT);

		vcs ck1 = mul_shift(c, k1, OUTPUT_SHIFT);
		vcs ck2 = mul_shift(c, k2, OUTPUT_SHIFT);
		vcs ck3 = mul_shift(c, k3, OUTPUT_SHIFT);
		vcs ck4 = mul_shift(c, k4, OUTPUT_SHIFT);

		vcs dk1 = mul_shift(d, k1, OUTPUT_SHIFT);
		vcs dk2 = mul_shift(d, k2, OUTPUT_SHIFT);
		vcs dk3 = mul_shift(d, k3, OUTPUT_SHIFT);
		vcs dk4 = mul_shift(d, k4, OUTPUT_SHIFT);

		vcs ek1 = mul_shift(e, k1, OUTPUT_SHIFT);
		vcs ek2 = mul_shift(e, k2, OUTPUT_SHIFT);
		vcs ek3 = mul_shift(e, k3, OUTPUT_SHIFT);
		vcs ek4 = mul_shift(e, k4, OUTPUT_SHIFT);

		// Calc X(5k) Start
		pi[0] = saturated_add(saturated_add(saturated_add(a, b), saturated_add(c,d)),e);

		// Calc X(5k+1) Start
		vcs x2 = saturated_add(saturated_add(saturated_add(a, bk1), saturated_add(ck2, dk3)), ek4);
		pi[nArray / 5] = mul_shift(x2, FFT_GetTwiddleConst<N, 1>()[n], OUTPUT_SHIFT);

		// Calc X(5k+2) Start
		vcs x3 = saturated_add(saturated_add(saturated_add(a, bk2), saturated_add(ck4, dk1)), ek3);
		pi[nArray / 5 * 2] = mul_shift(x3, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);

		// Calc X(5k+3) Start
		vcs x4 = saturated_add(saturated_add(saturated_add(a, bk3), saturated_add(ck1, dk4)), ek2);
		pi[nArray / 5 * 3] = mul_shift(x4, FFT_GetTwiddleConst<N, 3>()[n], OUTPUT_SHIFT);

		// Calc X(5k+4) Start
		vcs x5 = saturated_add(saturated_add(saturated_add(a, bk4), saturated_add(ck3, dk2)), ek1);
		pi[nArray / 5 * 4] = mul_shift(x5, FFT_GetTwiddleConst<N, 4>()[n], OUTPUT_SHIFT);

	}
}

// General radix-5 wrapper
template<int N>
DSP_INLINE1 void FFTSSE_5W(vcs* pInput)
{
	const int nArray = N / vcs::size;

	FFTSSE_5<N>(pInput);

	FFTSSEEx<N/5>(pInput);
	FFTSSEEx<N/5>(pInput + nArray / 5);
	FFTSSEEx<N/5>(pInput + nArray / 5 * 2);
	FFTSSEEx<N/5>(pInput + nArray / 5 * 3);
	FFTSSEEx<N/5>(pInput + nArray / 5 * 4);

}

// Specialised cases:
template<>
DSP_INLINE void
FFTSSEEx<60>(vcs* pInput)
{
	FFTSSE_5W<60>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<120>(vcs* pInput)
{
	FFTSSE_5W<120>(pInput);
}
template<>

DSP_INLINE void
FFTSSEEx<180>(vcs* pInput)
{
	FFTSSE_5W<180>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<300>(vcs* pInput)
{
	FFTSSE_5W<300>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<360>(vcs* pInput)
{
	FFTSSE_5W<360>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<540>(vcs* pInput)
{
	FFTSSE_5W<540>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<600>(vcs* pInput)
{
	FFTSSE_5W<600>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<900>(vcs* pInput)
{
	FFTSSE_5W<900>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<1080>(vcs* pInput)
{
	FFTSSE_5W<1080>(pInput);
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


// General radix-3 wrapper
template<int N>
DSP_INLINE1 void IFFTSSE_3W(vcs* pInput)
{
	const int nArray = N / vcs::size;

	IFFTSSE_3<N>(pInput);

	IFFTSSEEx<N / 3>(pInput);
	IFFTSSEEx<N / 3>(pInput + nArray / 3);
	IFFTSSEEx<N / 3>(pInput + nArray / 3 * 2);

}
// Specialized cases

template<>
DSP_INLINE void
IFFTSSEEx<12>(vcs* pInput)
{
	IFFTSSE_3W<12>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<24>(vcs* pInput)
{
	IFFTSSE_3W<24>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<36>(vcs* pInput)
{
	IFFTSSE_3W<36>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<72>(vcs* pInput)
{
	IFFTSSE_3W<72>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<108>(vcs* pInput)
{
	IFFTSSE_3W<108>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<216>(vcs* pInput)
{
	IFFTSSE_3W<216>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<324>(vcs* pInput)
{
	IFFTSSE_3W<324>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<648>(vcs* pInput)
{
	IFFTSSE_3W<648>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<972>(vcs* pInput)
{
	IFFTSSE_3W<972>(pInput);
}



// General radix-5 breakdown
template<int N>
DSP_INLINE1 void IFFTSSE_5(vcs* pInput)
{
	const int INPUT_SHIFT = 3;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;


	__declspec(align(16)) const COMPLEX16 k1[] =
	{
		{ 10126, 31164 },
		{ 10126, 31164 },
		{ 10126, 31164 },
		{ 10126, 31164 },
	};
	__declspec(align(16)) const COMPLEX16 k2[] =
	{
		{ -26510, 19261 },
		{ -26510, 19261 },
		{ -26510, 19261 },
		{ -26510, 19261 },
	};
	__declspec(align(16)) const COMPLEX16 k3[] =
	{
		{ -26510, -19261 },
		{ -26510, -19261 },
		{ -26510, -19261 },
		{ -26510, -19261 },
	};
	__declspec(align(16)) const COMPLEX16 k4[] =
	{
		{ 10126, -31164 },
		{ 10126, -31164 },
		{ 10126, -31164 },
		{ 10126, -31164 },
	};

	vcs *pi = pInput;
	for (int n = 0; n < nArray / 5; n++, pi++)
	{
		vcs a = shift_right(pi[0], INPUT_SHIFT);
		vcs b = shift_right(pi[nArray / 5], INPUT_SHIFT);
		vcs c = shift_right(pi[nArray / 5 * 2], INPUT_SHIFT);
		vcs d = shift_right(pi[nArray / 5 * 3], INPUT_SHIFT);
		vcs e = shift_right(pi[nArray / 5 * 4], INPUT_SHIFT);

		vcs bk1 = mul_shift(b, k1, OUTPUT_SHIFT);
		vcs bk2 = mul_shift(b, k2, OUTPUT_SHIFT);
		vcs bk3 = mul_shift(b, k3, OUTPUT_SHIFT);
		vcs bk4 = mul_shift(b, k4, OUTPUT_SHIFT);

		vcs ck1 = mul_shift(c, k1, OUTPUT_SHIFT);
		vcs ck2 = mul_shift(c, k2, OUTPUT_SHIFT);
		vcs ck3 = mul_shift(c, k3, OUTPUT_SHIFT);
		vcs ck4 = mul_shift(c, k4, OUTPUT_SHIFT);

		vcs dk1 = mul_shift(d, k1, OUTPUT_SHIFT);
		vcs dk2 = mul_shift(d, k2, OUTPUT_SHIFT);
		vcs dk3 = mul_shift(d, k3, OUTPUT_SHIFT);
		vcs dk4 = mul_shift(d, k4, OUTPUT_SHIFT);

		vcs ek1 = mul_shift(e, k1, OUTPUT_SHIFT);
		vcs ek2 = mul_shift(e, k2, OUTPUT_SHIFT);
		vcs ek3 = mul_shift(e, k3, OUTPUT_SHIFT);
		vcs ek4 = mul_shift(e, k4, OUTPUT_SHIFT);

		// Calc X(5k) Start
		pi[0] = saturated_add(saturated_add(saturated_add(a, b), saturated_add(c, d)), e);

		// Calc X(5k+1) Start
		vcs x2 = saturated_add(saturated_add(saturated_add(a, bk1), saturated_add(ck2, dk3)), ek4);
		pi[nArray / 5] = conj_mul_shift(x2, FFT_GetTwiddleConst<N, 1>()[n], OUTPUT_SHIFT);

		// Calc X(5k+2) Start
		vcs x3 = saturated_add(saturated_add(saturated_add(a, bk2), saturated_add(ck4, dk1)), ek3);
		pi[nArray / 5 * 2] = conj_mul_shift(x3, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);

		// Calc X(5k+3) Start
		vcs x4 = saturated_add(saturated_add(saturated_add(a, bk3), saturated_add(ck1, dk4)), ek2);
		pi[nArray / 5 * 3] = conj_mul_shift(x4, FFT_GetTwiddleConst<N, 3>()[n], OUTPUT_SHIFT);

		// Calc X(5k+4) Start
		vcs x5 = saturated_add(saturated_add(saturated_add(a, bk4), saturated_add(ck3, dk2)), ek1);
		pi[nArray / 5 * 4] = conj_mul_shift(x5, FFT_GetTwiddleConst<N, 4>()[n], OUTPUT_SHIFT);

	}
}

// General radix-5 wrapper
template<int N>
DSP_INLINE1 void IFFTSSE_5W(vcs* pInput)
{
	const int nArray = N / vcs::size;

	IFFTSSE_5<N>(pInput);

	IFFTSSEEx<N / 5>(pInput);
	IFFTSSEEx<N / 5>(pInput + nArray / 5);
	IFFTSSEEx<N / 5>(pInput + nArray / 5 * 2);
	IFFTSSEEx<N / 5>(pInput + nArray / 5 * 3);
	IFFTSSEEx<N / 5>(pInput + nArray / 5 * 4);

}

// Specialised cases:
template<>
DSP_INLINE void
IFFTSSEEx<60>(vcs* pInput)
{
	IFFTSSE_5W<60>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<120>(vcs* pInput)
{
	IFFTSSE_5W<120>(pInput);
}
template<>

DSP_INLINE void
IFFTSSEEx<180>(vcs* pInput)
{
	IFFTSSE_5W<180>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<300>(vcs* pInput)
{
	IFFTSSE_5W<300>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<360>(vcs* pInput)
{
	IFFTSSE_5W<360>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<540>(vcs* pInput)
{
	IFFTSSE_5W<540>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<600>(vcs* pInput)
{
	IFFTSSE_5W<600>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<900>(vcs* pInput)
{
	IFFTSSE_5W<900>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<1080>(vcs* pInput)
{
	IFFTSSE_5W<1080>(pInput);
}
