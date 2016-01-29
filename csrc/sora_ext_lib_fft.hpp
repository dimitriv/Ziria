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

// Contains the radix 3 & 5 part of the fft library, the rest is in the included files


// DSP_INLINE usually defined in vector128.h so defined here for gcc
#ifndef __ARM_NEON__
#ifdef __GNUC__
#define DSP_INLINE1		inline
#define DSP_INLINE __attribute__((always_inline))
	
#endif
#endif

#include "fft_r4difx.hpp"
#include "ifft_r4difx.hpp"
#ifndef __ARM_NEON__
#include "sora_ext_lib_fft_coeffs.hpp"
#else
#include "numerics.h"
#include "neon/vector128.h"
#endif

#ifndef __ARM_NEON__
DSP_INLINE __m128i mul_shiftx(const __m128i &a, const __m128i &b, int nbits_right)
{
	const  __m128i xmm0 = _mm_set1_epi32(0xFFFF0000);// 0x0000FFFF0000FFFF0000FFFF0000FFFF;
	const  __m128i xmm1 = _mm_set1_epi32(0x0000FFFF);// 0xFFFF0000FFFF0000FFFF0000FFFF0000;

	__m128i conj = _mm_xor_si128(b, xmm0); 

	__m128i flip = _mm_shufflehi_epi16(b, _MM_SHUFFLE(2, 3, 0, 1));
			flip = _mm_shufflelo_epi16(flip, _MM_SHUFFLE(2, 3, 0, 1));	

	__m128i vre32 = _mm_madd_epi16(a, conj); 
	__m128i vim32 = _mm_madd_epi16(a, flip); 
	
	// Shift right to normalize
	vre32 = _mm_srai_epi32(vre32, nbits_right); //real part
	vim32 = _mm_srai_epi32(vim32, nbits_right); //imaginary part
	
	// pack into 16 bits
	__m128i vre16 = _mm_and_si128(vre32, xmm1);
	__m128i vim16 = _mm_and_si128(vim32, xmm1);

	// pack into one register
	vim16 = _mm_slli_epi32(vim16, 0x10);
	return  _mm_or_si128(vre16, vim16); 

}
DSP_INLINE __m128i conj_mul_shiftx(const __m128i &a, const __m128i &b, int nbits_right)
{
	const  __m128i xmm0 = _mm_set1_epi32(0xFFFF0000);// 0x0000FFFF0000FFFF0000FFFF0000FFFF;
	const  __m128i xmm1 = _mm_set1_epi32(0x0000FFFF);// 0xFFFF0000FFFF0000FFFF0000FFFF0000;

	__m128i flip = _mm_shufflehi_epi16(a, _MM_SHUFFLE(2, 3, 0, 1));
	flip = _mm_shufflelo_epi16(flip, _MM_SHUFFLE(2, 3, 0, 1));

	__m128i conj = _mm_xor_si128(flip, xmm0);

	__m128i vre32 = _mm_madd_epi16(a, b);
	__m128i vim32 = _mm_madd_epi16(conj, b);


	// Shift right to normalize
	vre32 = _mm_srai_epi32(vre32, nbits_right); //real part
	vim32 = _mm_srai_epi32(vim32, nbits_right); //imaginary part
	

	// pack into 16 bits
	__m128i vre16 = _mm_and_si128(vre32, xmm1);
	__m128i vim16 = _mm_and_si128(vim32, xmm1);

	// pack into one register
	vim16 = _mm_slli_epi32(vim16, 0x10);
	return _mm_or_si128(vre16, vim16);


}

DSP_INLINE __m128i mul_jx(const __m128i &a)
{
	// Approximately multiplies by imaginary unit

	__m128i flip = _mm_shufflehi_epi16(a, _MM_SHUFFLE(2, 3, 0, 1));
	flip = _mm_shufflelo_epi16(flip, _MM_SHUFFLE(2, 3, 0, 1));

	return  _mm_xor_si128(flip, _mm_set1_epi32(0x0000FFFF)); // xor with 0xFFFF0000FFFF0000FFFF0000FFFF0000;


}

// General radix-3 breakdown
template<int N>
DSP_INLINE1 void FFTSSE_3(struct complex16*  pInput)
{
	const int INPUT_SHIFT = 2;
	const int OUTPUT_SHIFT = 15;
	
	struct complex16* pi = pInput;


	const __m128i k1 = _mm_setr_epi16(
		-16384, -28378,
		-16384, -28378,
		-16384, -28378,
		-16384, -28378
		);
	const __m128i k2 = _mm_setr_epi16(
		-16384, 28378,
		-16384, 28378,
		-16384, 28378,
		-16384, 28378
		);
	


	for (int n = 0; n < N / 3; n+=4 )
	{	
		__m128i a = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + n)), INPUT_SHIFT);
	
		__m128i b = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 3 + n)), INPUT_SHIFT);
		
		__m128i c = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 3 * 2 + n)), INPUT_SHIFT);
			

	
		__m128i bk1 = mul_shiftx(b, k1, OUTPUT_SHIFT);
		__m128i bk2 = mul_shiftx(b, k2, OUTPUT_SHIFT);

	
		__m128i ck1 = mul_shiftx(c, k1, OUTPUT_SHIFT);
		__m128i ck2 = mul_shiftx(c, k2, OUTPUT_SHIFT);
		
		// Calc X(3k) Start
		_mm_store_si128((__m128i *) (pi + n), _mm_adds_epi16(_mm_adds_epi16(a, b), c));

		
		// Calc X(3k+1) Start
		__m128i x2 = _mm_adds_epi16(_mm_adds_epi16(a, bk1), ck2);
		x2 = mul_shiftx(x2, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 1>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 3 + n), x2);
		

		// Calc X(3k+2) Start
		__m128i x3 = _mm_adds_epi16(_mm_adds_epi16(a, bk2), ck1);
		x3 = mul_shiftx(x3, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 2>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 3 * 2 + n), x3);
		
		

	}
}

// General radix-3 wrapper
template<int N>
DSP_INLINE1 void FFTSSE_3W(struct complex16* pInput)
{

	
	FFTSSE_3<N>(pInput);
	


	FFTSSEEx<N / 3>(pInput);
	FFTSSEEx<N / 3>(pInput + N / 3 );
	FFTSSEEx<N / 3>(pInput + N / 3 * 2 );

}

// Specialized cases

template<>
DSP_INLINE void
FFTSSEEx<12>(struct complex16*  pInput)
{
	FFTSSE_3W<12>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<24>(struct complex16*  pInput)
{
	FFTSSE_3W<24>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<36>(struct complex16* pInput)
{
	FFTSSE_3W<36>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<72>(struct complex16* pInput)
{
	FFTSSE_3W<72>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<108>(struct complex16* pInput)
{
	FFTSSE_3W<108>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<216>(struct complex16* pInput)
{
	FFTSSE_3W<216>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<324>(struct complex16* pInput)
{
	FFTSSE_3W<324>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<648>(struct complex16* pInput)
{
	FFTSSE_3W<648>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<972>(struct complex16* pInput)
{
	FFTSSE_3W<972>(pInput);
}

template<int N>
DSP_INLINE1 void FFTSSE_5(struct complex16* pInput)
{
	const int INPUT_SHIFT = 3;
	const int OUTPUT_SHIFT = 15;
	//const int nArray = N /4;

	
	const __m128i k1 = _mm_setr_epi16(
		 10126, -31164 ,
		 10126, -31164 ,
		 10126, -31164 ,
		 10126, -31164 
	);
	const __m128i k2 = _mm_setr_epi16(
		 -26510, -19261 ,
		 -26510, -19261 ,
		 -26510, -19261 ,
		 -26510, -19261 
	);
	const __m128i k3 = _mm_setr_epi16(
		 -26510, 19261 ,
		 -26510, 19261 ,
		 -26510, 19261 ,
		 -26510, 19261 
	);
	const __m128i k4 = _mm_setr_epi16(
	 10126, 31164 ,
	 10126, 31164 ,
	 10126, 31164 ,
	 10126, 31164 
	 );

	struct complex16 *pi = pInput;
	for (int n = 0; n < N / 5; n+=4)
	{

		__m128i a = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + n)), INPUT_SHIFT);

		__m128i b = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 + n)), INPUT_SHIFT);

		__m128i c = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 * 2 + n)), INPUT_SHIFT);

		__m128i d = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 * 3 + n)), INPUT_SHIFT);

		__m128i e = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 * 4 + n)), INPUT_SHIFT);

		
		__m128i bk1 = mul_shiftx(b, k1, OUTPUT_SHIFT);
		__m128i bk2 = mul_shiftx(b, k2, OUTPUT_SHIFT);
		__m128i bk3 = mul_shiftx(b, k3, OUTPUT_SHIFT);
		__m128i bk4 = mul_shiftx(b, k4, OUTPUT_SHIFT);

		
		__m128i ck1 = mul_shiftx(c, k1, OUTPUT_SHIFT);
		__m128i ck2 = mul_shiftx(c, k2, OUTPUT_SHIFT);
		__m128i ck3 = mul_shiftx(c, k3, OUTPUT_SHIFT);
		__m128i ck4 = mul_shiftx(c, k4, OUTPUT_SHIFT);

		__m128i dk1 = mul_shiftx(d, k1, OUTPUT_SHIFT);
		__m128i dk2 = mul_shiftx(d, k2, OUTPUT_SHIFT);
		__m128i dk3 = mul_shiftx(d, k3, OUTPUT_SHIFT);
		__m128i dk4 = mul_shiftx(d, k4, OUTPUT_SHIFT);

		
		__m128i ek1 = mul_shiftx(e, k1, OUTPUT_SHIFT);
		__m128i ek2 = mul_shiftx(e, k2, OUTPUT_SHIFT);
		__m128i ek3 = mul_shiftx(e, k3, OUTPUT_SHIFT);
		__m128i ek4 = mul_shiftx(e, k4, OUTPUT_SHIFT);

		// Calc X(5k) Start
		
		_mm_store_si128((__m128i *) (pi + n), _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, b), _mm_adds_epi16(c, d)), e));

		// Calc X(5k+1) Start
		__m128i x2 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk1), _mm_adds_epi16(ck2, dk3)), ek4);
		x2 = mul_shiftx(x2, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 1>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 +  n), x2);

		// Calc X(5k+2) Start
		__m128i x3 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk2), _mm_adds_epi16(ck4, dk1)), ek3);
		x3 = mul_shiftx(x3, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 2>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 * 2 +  n), x3);

		// Calc X(5k+3) Start
		__m128i x4 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk3), _mm_adds_epi16(ck1, dk4)), ek2);
		x4 = mul_shiftx(x4, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 3>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 * 3 +  n), x4);

		// Calc X(5k+4) Start
		__m128i x5 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk4), _mm_adds_epi16(ck3, dk2)), ek1);
		x5 = mul_shiftx(x5, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 4>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 * 4 +  n), x5);

	}
}

// General radix-5 wrapper
template<int N>
DSP_INLINE1 void FFTSSE_5W(struct complex16* pInput)
{

	FFTSSE_5<N>(pInput);

	FFTSSEEx<N/5>(pInput);
	FFTSSEEx<N/5>(pInput + N / 5);
	FFTSSEEx<N/5>(pInput + N / 5 * 2);
	FFTSSEEx<N/5>(pInput + N / 5 * 3);
	FFTSSEEx<N/5>(pInput + N / 5 * 4);

}

// Specialised cases:
template<>
DSP_INLINE void
FFTSSEEx<60>(struct complex16* pInput)
{
	FFTSSE_5W<60>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<120>(struct complex16* pInput)
{
	FFTSSE_5W<120>(pInput);
}
template<>

DSP_INLINE void
FFTSSEEx<180>(struct complex16* pInput)
{
	FFTSSE_5W<180>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<300>(struct complex16* pInput)
{
	FFTSSE_5W<300>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<360>(struct complex16* pInput)
{
	FFTSSE_5W<360>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<540>(struct complex16* pInput)
{
	FFTSSE_5W<540>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<600>(struct complex16* pInput)
{
	FFTSSE_5W<600>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<900>(struct complex16* pInput)
{
	FFTSSE_5W<900>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<1080>(struct complex16* pInput)
{
	FFTSSE_5W<1080>(pInput);
}


// IFFT

// General radix-3 breakdown
template<int N>
DSP_INLINE1 void IFFTSSE_3(struct complex16* pInput)
{
	const int INPUT_SHIFT = 2;
	const int OUTPUT_SHIFT = 15;

	// TBD: multiplication with k1 and k2 can be further optimized since k1 = conj(k2) 
	const __m128i k1 = _mm_setr_epi16(
	
		 -16384, 28378 ,
		 -16384, 28378 ,
		 -16384, 28378 ,
		 -16384, 28378 
	);
	const __m128i k2 = _mm_setr_epi16(
		 -16384, -28378 ,
		 -16384, -28378 ,
		 -16384, -28378 ,
		 -16384, -28378 
	);


	struct complex16 *pi = pInput;
	for (int n = 0; n < N / 3; n += 4)
	{
		__m128i a = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + n)), INPUT_SHIFT);

		__m128i b = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 3 + n)), INPUT_SHIFT);

		__m128i c = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 3 * 2 + n)), INPUT_SHIFT);


		__m128i bk1 = mul_shiftx(b, k1, OUTPUT_SHIFT);
		__m128i bk2 = mul_shiftx(b, k2, OUTPUT_SHIFT);

		__m128i ck1 = mul_shiftx(c, k1, OUTPUT_SHIFT);
		__m128i ck2 = mul_shiftx(c, k2, OUTPUT_SHIFT);

		// Calc X(3k) Start
		_mm_store_si128((__m128i *) (pi + n), _mm_adds_epi16(_mm_adds_epi16(a, b), c));


		// Calc X(3k+1) Start
		__m128i x2 = _mm_adds_epi16(_mm_adds_epi16(a, bk1), ck2);
		x2 = conj_mul_shiftx(x2, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 1>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 3 + n), x2);


		// Calc X(3k+2) Start
		__m128i x3 = _mm_adds_epi16(_mm_adds_epi16(a, bk2), ck1);
		x3 = conj_mul_shiftx(x3, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 2>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 3 * 2 + n), x3);

	}
}


// General radix-3 wrapper
template<int N>
DSP_INLINE1 void IFFTSSE_3W(struct complex16* pInput)
{
	IFFTSSE_3<N>(pInput);

	IFFTSSEEx<N / 3>(pInput);
	IFFTSSEEx<N / 3>(pInput + N / 3);
	IFFTSSEEx<N / 3>(pInput + N / 3 * 2);

}
// Specialized cases

template<>
DSP_INLINE void
IFFTSSEEx<12>(struct complex16* pInput)
{
	IFFTSSE_3W<12>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<24>(struct complex16* pInput)
{
	IFFTSSE_3W<24>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<36>(struct complex16* pInput)
{
	IFFTSSE_3W<36>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<72>(struct complex16* pInput)
{
	IFFTSSE_3W<72>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<108>(struct complex16* pInput)
{
	IFFTSSE_3W<108>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<216>(struct complex16* pInput)
{
	IFFTSSE_3W<216>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<324>(struct complex16* pInput)
{
	IFFTSSE_3W<324>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<648>(struct complex16* pInput)
{
	IFFTSSE_3W<648>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<972>(struct complex16* pInput)
{
	IFFTSSE_3W<972>(pInput);
}



// General radix-5 breakdown
template<int N>
DSP_INLINE1 void IFFTSSE_5(struct complex16* pInput)
{
	const int INPUT_SHIFT = 3;
	const int OUTPUT_SHIFT = 15;



	const __m128i k1 = _mm_setr_epi16(
		 10126, 31164 ,
		 10126, 31164 ,
		 10126, 31164 ,
		 10126, 31164 
	);
	const __m128i k2 = _mm_setr_epi16(
		 -26510, 19261 ,
		 -26510, 19261 ,
		 -26510, 19261 ,
		 -26510, 19261 
	);
	const __m128i k3 = _mm_setr_epi16(
	
		 -26510, -19261 ,
		 -26510, -19261 ,
		 -26510, -19261 ,
		 -26510, -19261 
	);
	const __m128i k4 = _mm_setr_epi16(

		 10126, -31164 ,
		 10126, -31164 ,
		 10126, -31164 ,
		 10126, -31164 
	);

	struct complex16 *pi = pInput;

	for (int n = 0; n < N / 5; n+=4)
	{
		__m128i a = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + n)), INPUT_SHIFT);
		__m128i b = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 + n)), INPUT_SHIFT);
		__m128i c = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 * 2 + n)), INPUT_SHIFT);
		__m128i d = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 * 3 + n)), INPUT_SHIFT);
		__m128i e = _mm_srai_epi16(_mm_load_si128((__m128i *)(pi + N / 5 * 4 + n)), INPUT_SHIFT);

		__m128i bk1 = mul_shiftx(b, k1, OUTPUT_SHIFT);
		__m128i bk2 = mul_shiftx(b, k2, OUTPUT_SHIFT);
		__m128i bk3 = mul_shiftx(b, k3, OUTPUT_SHIFT);
		__m128i bk4 = mul_shiftx(b, k4, OUTPUT_SHIFT);

		__m128i ck1 = mul_shiftx(c, k1, OUTPUT_SHIFT);
		__m128i ck2 = mul_shiftx(c, k2, OUTPUT_SHIFT);
		__m128i ck3 = mul_shiftx(c, k3, OUTPUT_SHIFT);
		__m128i ck4 = mul_shiftx(c, k4, OUTPUT_SHIFT);

		__m128i dk1 = mul_shiftx(d, k1, OUTPUT_SHIFT);
		__m128i dk2 = mul_shiftx(d, k2, OUTPUT_SHIFT);
		__m128i dk3 = mul_shiftx(d, k3, OUTPUT_SHIFT);
		__m128i dk4 = mul_shiftx(d, k4, OUTPUT_SHIFT);

		__m128i ek1 = mul_shiftx(e, k1, OUTPUT_SHIFT);
		__m128i ek2 = mul_shiftx(e, k2, OUTPUT_SHIFT);
		__m128i ek3 = mul_shiftx(e, k3, OUTPUT_SHIFT);
		__m128i ek4 = mul_shiftx(e, k4, OUTPUT_SHIFT);

		// Calc X(5k) Start
		_mm_store_si128((__m128i *) (pi + n), _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, b), _mm_adds_epi16(c, d)), e));

		// Calc X(5k+1) Start
		__m128i x2 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk1), _mm_adds_epi16(ck2, dk3)), ek4);
		x2 = conj_mul_shiftx(x2, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 1>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 + n), x2);

		// Calc X(5k+2) Start
		__m128i x3 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk2), _mm_adds_epi16(ck4, dk1)), ek3);
		x3 = conj_mul_shiftx(x3, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 2>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 * 2 + n), x3);

		// Calc X(5k+3) Start
		__m128i x4 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk3), _mm_adds_epi16(ck1, dk4)), ek2);
		x4 = conj_mul_shiftx(x4, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 3>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 * 3 + n), x4);

		// Calc X(5k+4) Start
		__m128i x5 = _mm_adds_epi16(_mm_adds_epi16(_mm_adds_epi16(a, bk4), _mm_adds_epi16(ck3, dk2)), ek1);
		x5 = conj_mul_shiftx(x5, _mm_load_si128((__m128i *)(FFT_GetTwiddleConstx<N, 4>() + 2 * n)), OUTPUT_SHIFT);
		_mm_store_si128((__m128i *) (pi + N / 5 * 4 + n), x5);

	}
}

 //General radix-5 wrapper
template<int N>
DSP_INLINE1 void IFFTSSE_5W(struct complex16* pInput)
{

	IFFTSSE_5<N>(pInput);

	IFFTSSEEx<N / 5>(pInput);
	IFFTSSEEx<N / 5>(pInput + N / 5);
	IFFTSSEEx<N / 5>(pInput + N / 5 * 2);
	IFFTSSEEx<N / 5>(pInput + N / 5 * 3);
	IFFTSSEEx<N / 5>(pInput + N / 5 * 4);

}

// Specialised cases:
template<>
DSP_INLINE void
IFFTSSEEx<60>(struct complex16* pInput)
{
	IFFTSSE_5W<60>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<120>(struct complex16* pInput)
{
	IFFTSSE_5W<120>(pInput);
}
template<>

DSP_INLINE void
IFFTSSEEx<180>(struct complex16* pInput)
{
	IFFTSSE_5W<180>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<300>(struct complex16* pInput)
{
	IFFTSSE_5W<300>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<360>(struct complex16* pInput)
{
	IFFTSSE_5W<360>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<540>(struct complex16* pInput)
{
	IFFTSSE_5W<540>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<600>(struct complex16* pInput)
{
	IFFTSSE_5W<600>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<900>(struct complex16* pInput)
{
	IFFTSSE_5W<900>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<1080>(struct complex16* pInput)
{
	IFFTSSE_5W<1080>(pInput);
}

#else

// General radix-3 breakdown
template<int N>
DSP_INLINE1 void FFTSSE_3(vcs* pInput)
{
	const int INPUT_SHIFT = 2;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;

	// TBD: multiplication with k1 and k2 can be further optimized since k1 = conj(k2)
	//__declspec(align(16))
	//const COMPLEX16 k1[] =
	calign const COMPLEX16 k1[] =
	{
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 }
	};
	//__declspec(align(16))
	calign const COMPLEX16 k2[] =
	{
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 }
	};


	vcs *pi = pInput;
	for (int n = 0; n < nArray / 3; n++, pi++)
	{
		vcs a = (vcs)shift_right(pi[0], INPUT_SHIFT);
		vcs b = (vcs)shift_right(pi[nArray / 3], INPUT_SHIFT);
		vcs c = (vcs)shift_right(pi[nArray / 3 * 2], INPUT_SHIFT);

		vcs bk1 = mul_shift(b, k1, OUTPUT_SHIFT);
		vcs ck2 = mul_shift(c, k2, OUTPUT_SHIFT);
		vcs bk2 = mul_shift(b, k2, OUTPUT_SHIFT);
		vcs ck1 = mul_shift(c, k1, OUTPUT_SHIFT);

		// Calc X(3k) Start
		pi[0] = saturated_add(saturated_add(a, b), c);

		// Calc X(3k+1) Start
		vcs x2 = saturated_add(saturated_add(a, bk1), ck2);
		pi[nArray / 3] = mul_shift(x2, FFT_GetTwiddleConst<N, 1>()[n], OUTPUT_SHIFT);
		//pi[nArray / 3] = mul_shift(x2, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 1>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(4k+1) Start
		vcs x4 = saturated_add(saturated_add(a, bk2), ck1);
		pi[nArray / 3 * 2] = mul_shift(x4, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);
		//pi[nArray / 3 * 2] = mul_shift(x4, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 2>() + 2 * n), OUTPUT_SHIFT);

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

/*
// General radix-3 wrapper
template<int N>
DSP_INLINE1 void FFTSSE_3W(struct vcs* pInput)
{
	const int nArray = N / vcs::size;

	FFTSSE_3<N>(pInput);



	FFTSSEEx<N / 3>(pInput);
	FFTSSEEx<N / 3>(pInput + nArray / 3 );
	FFTSSEEx<N / 3>(pInput + nArray / 3 * 2 );

}

// Specialized cases

template<>
DSP_INLINE void
FFTSSEEx<12>(struct vcs*  pInput)
{
	FFTSSE_3W<12>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<24>(struct vcs*  pInput)
{
	FFTSSE_3W<24>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<36>(struct vcs* pInput)
{
	FFTSSE_3W<36>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<72>(struct vcs* pInput)
{
	FFTSSE_3W<72>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<108>(struct vcs* pInput)
{
	FFTSSE_3W<108>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<216>(struct vcs* pInput)
{
	FFTSSE_3W<216>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<324>(struct vcs* pInput)
{
	FFTSSE_3W<324>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<648>(struct vcs* pInput)
{
	FFTSSE_3W<648>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<972>(struct vcs* pInput)
{
	FFTSSE_3W<972>(pInput);
}


template<int N>
DSP_INLINE1 void FFTSSE_5(struct vcs* pInput)
{
	const int INPUT_SHIFT = 3;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;
	//const int nArray = N /4;


	//const __m128i k1 = _mm_setr_epi16(
    calign const COMPLEX16 k1[] = {
		 { 10126, -31164 } ,
		 { 10126, -31164 } ,
		 { 10126, -31164 } ,
		 { 10126, -31164 }
    };
	//const __m128i k2 = _mm_setr_epi16(
	calign const COMPLEX16 k2[] = {
		 { -26510, -19261 } ,
		 { -26510, -19261 } ,
		 { -26510, -19261 } ,
		 { -26510, -19261 }
	};
	//const __m128i k3 = _mm_setr_epi16(
	calign const COMPLEX16 k3[] = {
		 { -26510, 19261 } ,
		 { -26510, 19261 } ,
		 { -26510, 19261 } ,
		 { -26510, 19261 }
	};
	//const __m128i k4 = _mm_setr_epi16(
	calign const COMPLEX16 k4[] = {
		 { 10126, 31164 } ,
		 { 10126, 31164 } ,
		 { 10126, 31164 } ,
		 { 10126, 31164 }
	 };

	vcs *pi = pInput;
	for (int n = 0; n < nArray / 5; n++, pi++)
	{

		vcs a = (vcs)shift_right(pi[0], INPUT_SHIFT);

		vcs b = (vcs)shift_right(pi[nArray / 5], INPUT_SHIFT);

		vcs c = (vcs)shift_right(pi[nArray / 5 * 2], INPUT_SHIFT);

		vcs d = (vcs)shift_right(pi[nArray / 5 * 3], INPUT_SHIFT);

		vcs e = (vcs)shift_right(pi[nArray / 5 * 4], INPUT_SHIFT);


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
		pi[0] = saturated_add(saturated_add(saturated_add(a, b), saturated_add(c,d)), e);

		// Calc X(5k+1) Start
		vcs x2 = saturated_add(saturated_add(saturated_add(a, bk1), saturated_add(ck2, dk3)), ek4);
		pi[nArray / 5] = mul_shift(x2, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 1>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(5k+2) Start
		vcs x3 = saturated_add(saturated_add(saturated_add(a, bk2), saturated_add(ck4, dk1)), ek3);
		pi[nArray / 5 * 2] = mul_shift(x3, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 2>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(5k+3) Start
		vcs x4 = saturated_add(saturated_add(saturated_add(a, bk3), saturated_add(ck1, dk4)), ek2);
		pi[nArray / 5 * 3] = mul_shift(x4, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 3>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(5k+4) Start
		vcs x5 = saturated_add(saturated_add(saturated_add(a, bk4), saturated_add(ck3, dk2)), ek1);
		pi[nArray / 5 * 4] = mul_shift(x5, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 4>() + 2 * n), OUTPUT_SHIFT);

	}
}

// General radix-5 wrapper
template<int N>
DSP_INLINE1 void FFTSSE_5W(struct vcs* pInput)
{
	const int nArray = N / vcs::size;

	FFTSSE_5<N>(pInput);

	FFTSSEEx<N / 5>(pInput);
	FFTSSEEx<N / 5>(pInput + nArray / 5);
	FFTSSEEx<N / 5>(pInput + nArray / 5 * 2);
	FFTSSEEx<N / 5>(pInput + nArray / 5 * 3);
	FFTSSEEx<N / 5>(pInput + nArray / 5 * 4);

}

// Specialised cases:
template<>
DSP_INLINE void
FFTSSEEx<60>(struct vcs* pInput)
{
	FFTSSE_5W<60>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<120>(struct vcs* pInput)
{
	FFTSSE_5W<120>(pInput);
}
template<>

DSP_INLINE void
FFTSSEEx<180>(struct vcs* pInput)
{
	FFTSSE_5W<180>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<300>(struct vcs* pInput)
{
	FFTSSE_5W<300>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<360>(struct vcs* pInput)
{
	FFTSSE_5W<360>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<540>(struct vcs* pInput)
{
	FFTSSE_5W<540>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<600>(struct vcs* pInput)
{
	FFTSSE_5W<600>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<900>(struct vcs* pInput)
{
	FFTSSE_5W<900>(pInput);
}

template<>
DSP_INLINE void
FFTSSEEx<1080>(struct vcs* pInput)
{
	FFTSSE_5W<1080>(pInput);
}
*/

// IFFT

// General radix-3 breakdown
template<int N>
DSP_INLINE1 void IFFTSSE_3(vcs* pInput)
{
	const int INPUT_SHIFT = 2;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;

	// TBD: multiplication with k1 and k2 can be further optimized since k1 = conj(k2)
	//__declspec(align(16))
	calign const COMPLEX16 k1[] =
	{
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 },
		{ -16384, 28378 }
	};
	//__declspec(align(16))
	calign const COMPLEX16 k2[] =
	{
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 },
		{ -16384, -28378 }
	};


	vcs *pi = pInput;
	for (int n = 0; n < nArray / 3; n++, pi++)
	{
		vcs a = (vcs)shift_right(pi[0], INPUT_SHIFT);
		vcs b = (vcs)shift_right(pi[nArray / 3], INPUT_SHIFT);
		vcs c = (vcs)shift_right(pi[nArray / 3 * 2], INPUT_SHIFT);

		vcs bk1 = mul_shift(b, k1, OUTPUT_SHIFT);
		vcs ck2 = mul_shift(c, k2, OUTPUT_SHIFT);
		vcs bk2 = mul_shift(b, k2, OUTPUT_SHIFT);
		vcs ck1 = mul_shift(c, k1, OUTPUT_SHIFT);

		// Calc X(3k) Start
		pi[0] = saturated_add(saturated_add(a, b), c);

		// Calc X(3k+1) Start
		vcs x2 = saturated_add(saturated_add(a, bk1), ck2);
		pi[nArray / 3] = conj_mul_shift(x2, FFT_GetTwiddleConst<N, 1>()[n], OUTPUT_SHIFT);
		//pi[nArray / 3] = conj_mul_shift(x2, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 1>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(4k+1) Start
		vcs x4 = saturated_add(saturated_add(a, bk2), ck1);
		pi[nArray / 3 * 2] = conj_mul_shift(x4, FFT_GetTwiddleConst<N, 2>()[n], OUTPUT_SHIFT);
		//pi[nArray / 3 * 2] = conj_mul_shift(x4, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 2>() + 2 * n), OUTPUT_SHIFT);

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

/*
// General radix-3 wrapper
template<int N>
DSP_INLINE1 void IFFTSSE_3W(struct vcs* pInput)
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
IFFTSSEEx<12>(struct vcs* pInput)
{
	IFFTSSE_3W<12>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<24>(struct vcs* pInput)
{
	IFFTSSE_3W<24>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<36>(struct vcs* pInput)
{
	IFFTSSE_3W<36>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<72>(struct vcs* pInput)
{
	IFFTSSE_3W<72>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<108>(struct vcs* pInput)
{
	IFFTSSE_3W<108>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<216>(struct vcs* pInput)
{
	IFFTSSE_3W<216>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<324>(struct vcs* pInput)
{
	IFFTSSE_3W<324>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<648>(struct vcs* pInput)
{
	IFFTSSE_3W<648>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<972>(struct vcs* pInput)
{
	IFFTSSE_3W<972>(pInput);
}



// General radix-5 breakdown
template<int N>
DSP_INLINE1 void IFFTSSE_5(struct vcs* pInput)
{

	const int INPUT_SHIFT = 3;
	const int OUTPUT_SHIFT = 15;
	const int nArray = N / vcs::size;



    calign const COMPLEX16 k1[] =
    {
		 { 10126, 31164 } ,
		 { 10126, 31164 } ,
		 { 10126, 31164 } ,
		 { 10126, 31164 }
    };
    calign const COMPLEX16 k2[] =
    {
		 { -26510, 19261 } ,
		 { -26510, 19261 } ,
		 { -26510, 19261 } ,
		 { -26510, 19261 }
    };
    calign const COMPLEX16 k3[] =
    {
		 { -26510, -19261 } ,
		 { -26510, -19261 } ,
		 { -26510, -19261 } ,
		 { -26510, -19261 }
    };
    calign const COMPLEX16 k4[] =
    {
		 { 10126, -31164 } ,
		 { 10126, -31164 } ,
		 { 10126, -31164 } ,
		 { 10126, -31164 }
    };

	vcs *pi = pInput;
	for (int n = 0; n < nArray / 5; n++, pi++)
	{
		vcs a = (vcs)shift_right(pi[0], INPUT_SHIFT);
		vcs b = (vcs)shift_right(pi[nArray / 5], INPUT_SHIFT);
		vcs c = (vcs)shift_right(pi[nArray / 5 * 2], INPUT_SHIFT);
		vcs d = (vcs)shift_right(pi[nArray / 5 * 3], INPUT_SHIFT);
		vcs e = (vcs)shift_right(pi[nArray / 5 * 4], INPUT_SHIFT);

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
		pi[nArray / 5] = conj_mul_shift(x2, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 1>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(5k+2) Start
		vcs x3 = saturated_add(saturated_add(saturated_add(a, bk2), saturated_add(ck4, dk1)), ek3);
		pi[nArray / 5 * 2] = conj_mul_shift(x3, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 2>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(5k+3) Start
		vcs x4 = saturated_add(saturated_add(saturated_add(a, bk3), saturated_add(ck1, dk4)), ek2);
		pi[nArray / 5 * 3] = conj_mul_shift(x4, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 3>() + 2 * n), OUTPUT_SHIFT);

		// Calc X(5k+4) Start
		vcs x5 = saturated_add(saturated_add(saturated_add(a, bk4), saturated_add(ck3, dk2)), ek1);
		pi[nArray / 5 * 4] = conj_mul_shift(x5, (vcs)vld1q_s16(FFT_GetTwiddleConstx<N, 4>() + 2 * n), OUTPUT_SHIFT);

	}
}

 //General radix-5 wrapper
template<int N>
DSP_INLINE1 void IFFTSSE_5W(struct vcs* pInput)
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
IFFTSSEEx<60>(struct vcs* pInput)
{
	IFFTSSE_5W<60>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<120>(struct vcs* pInput)
{
	IFFTSSE_5W<120>(pInput);
}
template<>

DSP_INLINE void
IFFTSSEEx<180>(struct vcs* pInput)
{
	IFFTSSE_5W<180>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<300>(struct vcs* pInput)
{
	IFFTSSE_5W<300>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<360>(struct vcs* pInput)
{
	IFFTSSE_5W<360>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<540>(struct vcs* pInput)
{
	IFFTSSE_5W<540>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<600>(struct vcs* pInput)
{
	IFFTSSE_5W<600>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<900>(struct vcs* pInput)
{
	IFFTSSE_5W<900>(pInput);
}

template<>
DSP_INLINE void
IFFTSSEEx<1080>(struct vcs* pInput)
{
	IFFTSSE_5W<1080>(pInput);
}
*/

#endif
