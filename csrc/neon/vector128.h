/*
 *    Copyright (c) Microsoft Corporation
	Coyright (c) Northeastern University
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


// Fundamental operators defined on SSE vector registers
#pragma once

// WARNING: be careful when including non default header files, in order to maintain the independency


#include "sse_to_neon.h"



#define VSHR_S16_U32_S32( A , N )	\
	vreinterpret_s32_u32( vshr_n_u32(vreinterpret_u32_s16( A ) , N) )

#define VSHRQ_S32_U32_S32( A , N )	\
	vreinterpretq_s32_u32( vshrq_n_u32(vreinterpretq_u32_s32( A ) , N) )

#define VSHRQ_S16_U32_S32( A, N )	\
	vreinterpretq_s32_u32( vshrq_n_u32(vreinterpretq_u32_s16( A ) , N) )

#define VSHRQ_S16_U64_S64( A , N )	\
	vreinterpretq_s64_u64( vshrq_n_u64(vreinterpretq_u64_s16( A ) , N) )

#define VSHRQ_S32_U64_S64( A , N )	\
	vreinterpretq_s64_u64( vshrq_n_u64(vreinterpretq_u64_s32( A ) , N) )

#define VSHRQ_S32_U64_S32( A , N )	\
	vreinterpretq_s32_u64( vshrq_n_u64(vreinterpretq_u64_s32( A ) , N) )

inline __attribute__((always_inline)) int32x4_t __0x0000FFFF0000FFFF0000FFFF0000FFFF()
{
	return VSHRQ_S32_U32_S32(  vmvnq_s32( vdupq_n_s32( 0 ) ) , 16);
};

inline __attribute__((always_inline)) int32x4_t __0xFFFF0000FFFF0000FFFF0000FFFF0000()
{
	return vshlq_n_s32( vmvnq_s32( vdupq_n_s32( 0 ) ), 16);
};

inline __attribute__((always_inline)) int16x8_t __0xFFFFFFFF00000000FFFFFFFF00000000()
{
	int64x2_t q11 = vdupq_n_s64(0);
	q11 = vreinterpretq_s64_s32 ( vmvnq_s32(vreinterpretq_s32_s64( q11 ) ) );
	q11 = vshlq_n_s64(q11, 32);
	return vreinterpretq_s16_s64( q11 ) ;
};

inline __attribute__((always_inline)) int16x8_t __0xFFFFFFFFFFFFFFFF0000000000000000()
{
	int64x2_t q11 = vdupq_n_s64(0);
	q11 = vreinterpretq_s64_s32 ( vmvnq_s32(vreinterpretq_s32_s64( q11 ) ) );
	q11 = vsetq_lane_s64(0, q11, 0);
	return vreinterpretq_s16_s64( q11 );
};


inline __attribute__((always_inline)) int16x8_t __0xFFFF0000FFFF00000000000000000000()
{
	int32x4_t q8 = vdupq_n_s32(0);
	q8 = vmvnq_s32(q8);
	q8 = vshlq_n_s32(q8, 16);
	q8 = vsetq_lane_s32(0, q8, 1);
	q8 = vsetq_lane_s32(0, q8, 0);
	return vreinterpretq_s16_s32( q8 );
};

inline __attribute__((always_inline)) int16x8_t __0xFFFF0000000000000000000000000000()
{
	int32x4_t q8 = vdupq_n_s32(0);
	q8 = vmvnq_s32(q8);
	q8 = vshlq_n_s32(q8, 16);
	q8 = vsetq_lane_s32(0, q8, 2);
	q8 = vsetq_lane_s32(0, q8, 1);
	q8 = vsetq_lane_s32(0, q8, 0);
	return vreinterpretq_s16_s32( q8 );
};

inline __attribute__((always_inline)) int16x8_t __0x00000000000000000000000000000000()
{
	return vdupq_n_s16(0);
};

// Clear all bits in a vector
DSP_INLINE int16x8_t set_zero_s16() { return vdupq_n_s16(0); }



//////////////////////////////////////////////////////////////////////////////
// Private macros

// Dummy declaration
#define DECLARE_PUBLIC_OP(OP) 

// Macro to define operators with 1 operand and 1 result, which are of the same type
// Note: non template version
#define DEFINE_OP_ARITHMETIC1(OP, T, INSTRINSIC)        \
    DSP_INLINE T OP(const T& a) { return (T)INSTRINSIC(a); }

// Macro to define operators with 2 operands and 1 result, which are of the same type
// Using template to define operations are all m128 wrapper types
#define DEFINE_TEMPLATE_OP_ARITHMETIC2(OP, INSTRINSIC)              \
    template<typename T>                                            \
    DSP_INLINE T OP(const T& a, const T& b, typename vector128::details::traits<T>::tag * = 0) {    \
        return (T)INSTRINSIC(a, b);                                 \
    }

// Macro to define operators with 2 operands and 1 result, which are of the same type
// Note: non template version
#define DEFINE_OP_ARITHMETIC2(OP, T, INSTRINSIC)        \
    DSP_INLINE T OP(const T& a, const T& b) { return (T)INSTRINSIC(a, b); }

// Macro to define operators with 3 operands and 1 result, which are of the same type
// Note: non template version
#define DEFINE_OP_ARITHMETIC3(OP, T, INSTRINSIC)        \
    DSP_INLINE T OP(const T& a, const T& b, const T& c) { return (T)INSTRINSIC(a, b, c); }


// Macro to define permutation operators
#define DEFINE_OP_PERMUTATION(OP, T, INSTRINSIC)        \
    template<int n>                                     \
    DSP_INLINE T OP(const T& a) { return (T)INSTRINSIC(a, n); }

// Macro to define 4-element permutation operators
// Note: it is indeed 2 templates, including DEFINE_OP_PERMUTATION
#define DEFINE_OP_PERMUTATION4(OP, T, INSTRINSIC)       \
    template<int a0, int a1, int a2, int a3>            \
    DSP_INLINE T OP(const T& a) { return (T)INSTRINSIC(a, _MM_SHUFFLE(a3, a2, a1, a0)); }   \
    DEFINE_OP_PERMUTATION(OP, T, INSTRINSIC)

// Macro to define shift operatiors
#define DEFINE_OP_SHIFT(OP, T, INSTRINSIC)              \
    DSP_INLINE T OP(const T& a, int nbits) { return (T)INSTRINSIC(a, nbits); }
#define DEFINE_OP_SHIFT_LEFT(OP, T, INSTRINSIC)         \
    DEFINE_OP_SHIFT(OP, T, INSTRINSIC)                  \
    DSP_INLINE T operator <<(const T& a, int nbits) { return OP(a, nbits); }
#define DEFINE_OP_SHIFT_RIGHT(OP, T, INSTRINSIC)        \
    DEFINE_OP_SHIFT(OP, T, INSTRINSIC)                  \
    DSP_INLINE T operator >>(const T& a, int nbits) { return OP(a, nbits); }


// Macro to define 4-way reducing operators
// eg. Get sum of 4 elements at the same index in each operand and comprise the returned vector by them
// Note:
//   1. all operands, returned wrapper and all intermediate wrapper are of the same type T
//   2. REDUCE_OP is the underlying 2-way reducing operator
#define DEFINE_OP_REDUCE4(OP, T, REDUCE_OP)                             \
DSP_INLINE T OP(const T& a0, const T& a1, const T& a2, const T& a3)     \
{                                                                       \
    T temp1, temp2, temp3;                                              \
    temp1 = interleave_low(a0, a2);         /* 2.1 0.1 2.0 0.0*/        \
    temp2 = interleave_high(a0, a2);        /* 2.3 0.3 2.2 0.2*/        \
    temp3 = REDUCE_OP(temp1, temp2);        /* 2.13 0.13 2.02 0.02*/    \
                                                                        \
    temp1 = interleave_low(a1, a3);         /* 3.1 1.1 3.0 1.0*/        \
    temp2 = interleave_high(a1, a3);        /* 3.3 1.3 3.2 1.2*/        \
    temp1 = REDUCE_OP(temp1, temp2);        /* 3.13 1.13 3.02 1.02*/    \
                                                                        \
    temp2 = interleave_low(temp3, temp1);   /* 3.02 2.02 1.02 0.02*/    \
    temp3 = interleave_high(temp3, temp1);  /* 3.13 2.13 1.13 0.13*/    \
    return REDUCE_OP(temp2, temp3);                                     \
}

// Iterate a binary operation (eg. maximal/minimal) on all 4 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
#define DEFINE_OP_DUPLICATION4_OPERATION(OP, T, REDUCE_OP)								\
DSP_INLINE T OP(const T& a)												\
{																		\
    T t = REDUCE_OP(a, permutate<0xb1>(a));							    \
    return REDUCE_OP(t, permutate<0x4e>(t));							\
}
/*
#define DEFINE_OP_DUPLICATION4_OPERATION(OP, T, REDUCE_OP)								\
DSP_INLINE T OP(const T& a)												\
{																		\
    T t = REDUCE_OP(a, (T)vrev64q_s32(a));							    \
    return REDUCE_OP(t, permutate<0x4e>(t));							\
}*/

// Iterate a binary operation (eg. maximal/minimal) on all 16 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
#define DEFINE_OP_DUPLICATION16_OPERATION(OP, T, OPER)      \
DSP_INLINE T OP(const T& a)                                 \
{                                                           \
    T xmm0, xmm1 = a;                                       \
    xmm0 = (T)permutate<0x4E>((vcs)xmm1);                   \
    xmm0 = OPER(xmm0, xmm1);                                \
    xmm1 = (T)permutate<0xB1>((vcs)xmm0);                   \
    xmm0 = OPER(xmm0, xmm1);                                \
    xmm0 = interleave_low(xmm0, xmm0);                      \
    xmm0 = interleave_low(xmm0, xmm0);                      \
    xmm1 = (T)permutate<0x4E>((vcs)xmm0);                   \
    xmm0 = OPER(xmm0, xmm1);                                \
    xmm1 = (T)permutate<0xB1>((vcs)xmm0);                   \
    xmm0 = OPER(xmm0, xmm1);                                \
    return xmm0;                                            \
}

// Iterate a binary operation (eg. maximal/minimal) on all 8 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
#define DEFINE_OP_DUPLICATION8_OPERATION(OP, T, OPER)       \
DSP_INLINE T OP(const T& a)                                 \
{                                                           \
    T r1, r2, r0 = a;                                       \
    r1 = (T)permutate<0x4E>((vcs&)r0);                      \
    r2 = OPER(r0, r1);                                      \
    r1 = (T)permutate<0xB1>((vcs&)r2);                      \
    r2 = OPER(r2, r1);                                      \
    r1 = (T)permutate_low<0xB1>(r2);                        \
    r1 = (T)permutate<1, 0, 1, 0>((vcs&)r1);                \
    r2 = OPER(r2, r1);                                      \
    return r2;                                              \
}




// Interleave the elements in lower half of 2 source vectors to a resulting vector
// The first source operand will be interleaved to the even indices, and the second source
// to the odd indices.
DEFINE_OP_ARITHMETIC2(interleave_low, vb, _mm_unpacklo_epi8);
DEFINE_OP_ARITHMETIC2(interleave_low, vs, _mm_unpacklo_epi16);
DEFINE_OP_ARITHMETIC2(interleave_low, vi, _mm_unpacklo_epi32);
DEFINE_OP_ARITHMETIC2(interleave_low, vq, _mm_unpacklo_epi64);
//DEFINE_OP_ARITHMETIC2(interleave_low, vcb, _mm_unpacklo_epi16);
__forceinline vcb  interleave_low(const vcb& a, const vcb& b) { return (vcb)_mm_unpacklo_epi16((vus&)a, (vus&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_low, vcs, _mm_unpacklo_epi32);
__forceinline vcs  interleave_low(const vcs& a, const vcs& b) { return (vcs)_mm_unpacklo_epi32((vui&)a, (vui&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_low, vci, _mm_unpacklo_epi64);
__forceinline vci  interleave_low(const vci& a, const vci& b) { return (vci)_mm_unpacklo_epi64((vuq&)a, (vuq&)b); }
DEFINE_OP_ARITHMETIC2(interleave_low, vub, _mm_unpacklo_epi8);
DEFINE_OP_ARITHMETIC2(interleave_low, vus, _mm_unpacklo_epi16);
DEFINE_OP_ARITHMETIC2(interleave_low, vui, _mm_unpacklo_epi32);
DEFINE_OP_ARITHMETIC2(interleave_low, vuq, _mm_unpacklo_epi64);
//DEFINE_OP_ARITHMETIC2(interleave_low, vcub, _mm_unpacklo_epi16);
__forceinline vcub  interleave_low(const vcub& a, const vcub& b) { return (vcub)_mm_unpacklo_epi16((vus&)a, (vus&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_low, vcus, _mm_unpacklo_epi32);
__forceinline vcus  interleave_low(const vcus& a, const vcus& b) { return (vcus)_mm_unpacklo_epi32((vui&)a, (vui&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_low, vcui, _mm_unpacklo_epi64);
__forceinline vcui  interleave_low(const vcui& a, const vcui& b) { return (vcui)_mm_unpacklo_epi64((vuq&)a, (vuq&)b); }

// Interleave the elements in higher half of 2 source vectors to a resulting vector
// The first source operand will be interleaved to the even indices, and the second source
// to the odd indices.
DEFINE_OP_ARITHMETIC2(interleave_high, vb, _mm_unpackhi_epi8);
DEFINE_OP_ARITHMETIC2(interleave_high, vs, _mm_unpackhi_epi16);
DEFINE_OP_ARITHMETIC2(interleave_high, vi, _mm_unpackhi_epi32);
DEFINE_OP_ARITHMETIC2(interleave_high, vq, _mm_unpackhi_epi64);
//DEFINE_OP_ARITHMETIC2(interleave_high, vcb, _mm_unpackhi_epi16);
__forceinline vcb  interleave_high(const vcb& a, const vcb& b) { return (vcb)_mm_unpackhi_epi16((vus&)a, (vus&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_high, vcs, _mm_unpackhi_epi32);
__forceinline vcs  interleave_high(const vcs& a, const vcs& b) { return (vcs)_mm_unpackhi_epi32((vui&)a, (vui&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_high, vci, _mm_unpackhi_epi64);
__forceinline vci  interleave_high(const vci& a, const vci& b) { return (vci)_mm_unpackhi_epi64((vuq&)a, (vuq&)b); }
DEFINE_OP_ARITHMETIC2(interleave_high, vub, _mm_unpackhi_epi8);
DEFINE_OP_ARITHMETIC2(interleave_high, vus, _mm_unpackhi_epi16);
DEFINE_OP_ARITHMETIC2(interleave_high, vui, _mm_unpackhi_epi32);
DEFINE_OP_ARITHMETIC2(interleave_high, vuq, _mm_unpackhi_epi64);
//DEFINE_OP_ARITHMETIC2(interleave_high, vcub, _mm_unpackhi_epi16);
__forceinline vcub  interleave_high(const vcub& a, const vcub& b) { return (vcub)_mm_unpackhi_epi16((vus&)a, (vus&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_high, vcus, _mm_unpackhi_epi32);
__forceinline vcus  interleave_high(const vcus& a, const vcus& b) { return (vcus)_mm_unpackhi_epi32((vui&)a, (vui&)b); }
//DEFINE_OP_ARITHMETIC2(interleave_high, vcui, _mm_unpackhi_epi64);
__forceinline vcui  interleave_high(const vcui& a, const vcui& b) { return (vcui)_mm_unpackhi_epi64((vuq&)a, (vuq&)b); }


DEFINE_OP_ARITHMETIC2(is_great, vb, vcgtq_s8);
DEFINE_OP_ARITHMETIC2(is_great, vs, vcgtq_s16);
DEFINE_OP_ARITHMETIC2(is_great, vi, vcgtq_s32);

DEFINE_OP_ARITHMETIC2(is_less, vb, vcltq_s8);
DEFINE_OP_ARITHMETIC2(is_less, vs, vcltq_s16);
DEFINE_OP_ARITHMETIC2(is_less, vi, vcltq_s32);



// Element-wise polarization on the first operand based on the second operand, ie.
// r[n] := (b[n] < 0) ? -a[n] : ((b[n] == 0) ? 0 : a[n])
DEFINE_OP_ARITHMETIC3(sign, vb, _mm_sign_epi8);
DEFINE_OP_ARITHMETIC3(sign, vs, _mm_sign_epi16);
DEFINE_OP_ARITHMETIC3(sign, vi, _mm_sign_epi32);
//DEFINE_OP_ARITHMETIC2(sign, vq, _mm_sign_epi64);
DEFINE_OP_ARITHMETIC3(sign, vcs, _mm_sign_epi16);

DEFINE_OP_ARITHMETIC1(negate, vb, vnegq_s8);
DEFINE_OP_ARITHMETIC1(negate, vs, vnegq_s16);
DEFINE_OP_ARITHMETIC1(negate, vi, vnegq_s32);
//DEFINE_OP_ARITHMETIC2(sign, vq, _mm_sign_epi64);
DEFINE_OP_ARITHMETIC1(negate, vcb, vnegq_s8);
DEFINE_OP_ARITHMETIC1(negate, vcs, vnegq_s16);


template<int index> DSP_INLINE typename vs::elem_type extract(const vs& a) { return (typename vs::elem_type)vgetq_lane_s16(a, index); }
template<int index> DSP_INLINE typename vus::elem_type extract(const vus& a) { return (typename vus::elem_type)vgetq_lane_u16(a, index); }



// Bitwise OR
DSP_INLINE uint8x16_t bitwise_or(const uint8x16_t &a, const uint8x16_t &b) 	{ return vorrq_u8(a, b); }
DSP_INLINE int16x8_t bitwise_or(const int16x8_t &a, const int16x8_t &b) 	{ return vorrq_s16(a, b); }

// Bitwise XOR
DSP_INLINE int16x8_t bitwise_xor(const int16x8_t &a, const int16x8_t &b) 	{ return veorq_s16(a, b); }

// Bitwise AND
DSP_INLINE uint8x16_t bitwise_and(const uint8x16_t &a, const uint8x16_t &b) 	{ return vandq_u8(a, b); }
DSP_INLINE int16x8_t bitwise_and(const int16x8_t &a, const int16x8_t &b) 	{ return vandq_s16(a, b); }





// Element-wise add
DEFINE_OP_ARITHMETIC2(add, vb, vaddq_s8);
DEFINE_OP_ARITHMETIC2(add, vub, vaddq_u8);
DEFINE_OP_ARITHMETIC2(add, vs, vaddq_s16);
DEFINE_OP_ARITHMETIC2(add, vus, vaddq_u16);
DEFINE_OP_ARITHMETIC2(add, vi, vaddq_s32);
DEFINE_OP_ARITHMETIC2(add, vui, vaddq_u32);
DEFINE_OP_ARITHMETIC2(add, vq, vaddq_s64);
DEFINE_OP_ARITHMETIC2(add, vuq, vaddq_u64);

DEFINE_OP_ARITHMETIC2(add, vcb, vaddq_s8);
DEFINE_OP_ARITHMETIC2(add, vcub, vaddq_u8);
DEFINE_OP_ARITHMETIC2(add, vcs, vaddq_s16);
DEFINE_OP_ARITHMETIC2(add, vcus, vaddq_u16);
DEFINE_OP_ARITHMETIC2(add, vci, vaddq_s32);
DEFINE_OP_ARITHMETIC2(add, vcui, vaddq_u32);
DEFINE_OP_ARITHMETIC2(add, vcq, vaddq_s64);
DEFINE_OP_ARITHMETIC2(add, vcuq, vaddq_u64);

// Element-wise Subtract
DEFINE_OP_ARITHMETIC2(sub, vb, vsubq_s8);
DEFINE_OP_ARITHMETIC2(sub, vub, vsubq_u8);
DEFINE_OP_ARITHMETIC2(sub, vs, vsubq_s16);
DEFINE_OP_ARITHMETIC2(sub, vus, vsubq_u16);
DEFINE_OP_ARITHMETIC2(sub, vi, vsubq_s32);
DEFINE_OP_ARITHMETIC2(sub, vui, vsubq_u32);
DEFINE_OP_ARITHMETIC2(sub, vq, vsubq_s64);
DEFINE_OP_ARITHMETIC2(sub, vuq, vsubq_u64);

DEFINE_OP_ARITHMETIC2(sub, vcb, vsubq_s8);
DEFINE_OP_ARITHMETIC2(sub, vcub, vsubq_u8);
DEFINE_OP_ARITHMETIC2(sub, vcs, vsubq_s16);
DEFINE_OP_ARITHMETIC2(sub, vcus, vsubq_u16);
DEFINE_OP_ARITHMETIC2(sub, vci, vsubq_s32);
DEFINE_OP_ARITHMETIC2(sub, vcui, vsubq_u32);
DEFINE_OP_ARITHMETIC2(sub, vcq, vsubq_s64);
DEFINE_OP_ARITHMETIC2(sub, vcuq, vsubq_u64);




// Element-wise saturated add
DSP_INLINE vb saturated_add(const vb &a, const vb &b) {return (vb)vqaddq_s8( a, b); }
DSP_INLINE vub saturated_add(const vub &a, const vub &b) {return (vub)vqaddq_u8( a, b); }
DSP_INLINE vs saturated_add(const vs &a, const vs &b) {return (vs)vqaddq_s16( a, b); }
DSP_INLINE vus saturated_add(const vus &a, const vus &b) {return (vus)vqaddq_u16( a, b); }
DSP_INLINE vcb saturated_add(const vcb &a, const vcb &b) {return (vcb)vqaddq_s8( a, b); }
DSP_INLINE vcub saturated_add(const vcub &a, const vcub &b) {return (vcub)vqaddq_u8( a, b); }
DSP_INLINE vcs saturated_add(const vcs &a, const vcs &b) {return (vcs)vqaddq_s16( a, b); }
DSP_INLINE vcus saturated_add(const vcus &a, const vcus &b) {return (vcus)vqaddq_u16( a, b); }

// Element-wise saturated subtract
DSP_INLINE vb saturated_sub(const vb &a, const vb &b) {return (vb)vqsubq_s8( a, b); }
DSP_INLINE vub saturated_sub(const vub &a, const vub &b) {return (vub)vqsubq_u8( a, b); }
DSP_INLINE vs saturated_sub(const vs &a, const vs &b) {return (vs)vqsubq_s16( a, b); }
DSP_INLINE vus saturated_sub(const vus &a, const vus &b) {return (vus)vqsubq_u16( a, b); }
DSP_INLINE vcb saturated_sub(const vcb &a, const vcb &b) {return (vcb)vqsubq_s8( a, b); }
DSP_INLINE vcub saturated_sub(const vcub &a, const vcub &b) {return (vcub)vqsubq_u8( a, b); }
DSP_INLINE vcs saturated_sub(const vcs &a, const vcs &b) {return (vcs)vqsubq_s16( a, b); }
DSP_INLINE vcus saturated_sub(const vcus &a, const vcus &b) {return (vcus)vqsubq_u16( a, b); }

// Element-wise average of 2 operands vector
DSP_INLINE vs average(const vs &a, const vs &b) { return (vs)vrhaddq_s16(a, b); }
DSP_INLINE vus average(const vus &a, const vus &b) { return (vus)vrhaddq_u16(a, b); }


// Permutate 4 elements in the lower half vector
// Template parameter n: each 2-bit field (from LSB) selects the contents of one element location
// (from low address) in the destination operand. ie.
// r[0] := a[n(1:0)]
// r[1] := a[n(3:2)]
// r[2] := a[n(5:4)]
// r[3] := a[n(7:6)]
// Template parameter a0 ~ a3: selects the contents of one element location in the destination operand. ie.
// r[0] := a[a0]
// r[1] := a[a1]
// r[2] := a[a2]
// r[3] := a[a3]
DEFINE_OP_PERMUTATION4(permutate_low, vs, _mm_shufflelo_epi16);
DEFINE_OP_PERMUTATION4(permutate_low, vcs, _mm_shufflelo_epi16);
DEFINE_OP_PERMUTATION4(permutate_low, vus, _mm_shufflelo_epi16);
DEFINE_OP_PERMUTATION4(permutate_low, vcus, _mm_shufflelo_epi16);
// Permutate 4 elements in the higher half vector
// The definitions of template parameters are similar to permutate_low
DEFINE_OP_PERMUTATION4(permutate_high, vs, _mm_shufflehi_epi16);
DEFINE_OP_PERMUTATION4(permutate_high, vcs, _mm_shufflehi_epi16);
DEFINE_OP_PERMUTATION4(permutate_high, vus, _mm_shufflehi_epi16);
DEFINE_OP_PERMUTATION4(permutate_high, vcus, _mm_shufflehi_epi16);
// Permutate 4 elements in a vector
// The definitions of template parameters are similar to permutate_low
DEFINE_OP_PERMUTATION4(permutate, vcs, _mm_shuffle_epi32);
DEFINE_OP_PERMUTATION4(permutate, vi, _mm_shuffle_epi32);
DEFINE_OP_PERMUTATION4(permutate, vui, _mm_shuffle_epi32);

// Compute sum of all 2 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
__forceinline vci hadd(const vci& a)
{
    vci temp, sum;
    //temp = (vci)permutate<2, 3, 0, 1>((vi&)a); // Rahman: this is probably wrong
    temp = (vci)permutate<1, 0, 3, 2>((vi&)a);
    sum  = add(a, temp);
    return sum;
}
// Compute sum of all 4 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
DEFINE_OP_DUPLICATION4_OPERATION(hadd,       vcs, add);
/*__forceinline vcs hadd(const vcs& a)
{
    vcs sum;
    sum  = add(a, (vcs)vrev64q_s32(a));
    return add(sum, permutate<0x4e>(sum));
}*/
DEFINE_OP_DUPLICATION4_OPERATION(hadd,       vi,  add);
DEFINE_OP_DUPLICATION4_OPERATION(hadd,       vui, add);


// Assign the same value to all elements in a vector
DSP_INLINE int32x4_t set_all(const COMPLEX16 &a) { 	return vdupq_n_s32((((int32_t)a.im) << 16) + a.re); }
DSP_INLINE void set_all(vb& x, int8_t a) { x = (vb)vdupq_n_s8(a); }
DSP_INLINE void set_all(vs& x, int16_t a) { x = (vs)vdupq_n_s16(a); }
DSP_INLINE void set_all(vi& x, int32_t a) { x = (vi)vdupq_n_s32(a); }
DSP_INLINE void set_all(vcs& x, COMPLEX16 a) { x = (vcs)vreinterpretq_s16_s32(vdupq_n_s32((((int32_t)a.im) << 16) + a.re)); }
DSP_INLINE void set_all(vq& x, int64_t a) { x = (vq)vdupq_n_s64(a); }





// Element-wise arithmetic left shift
DSP_INLINE int16x8_t shift_left(const int16x8_t &a, const int &n)
{
	// int16x8_t  vshlq_n_s16(int16x8_t a, __constrange(0,15) int b);  // VSHL.I16 q0,q0,#0
	//return vshlq_n_s16(a, 0);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshlq_n_s16(a, 1);
	case 2:
		return vshlq_n_s16(a, 2);
	case 3:
		return vshlq_n_s16(a, 3);
	case 4:
		return vshlq_n_s16(a, 4);
	case 5:
		return vshlq_n_s16(a, 5);
	case 6:
		return vshlq_n_s16(a, 6);
	case 7:
		return vshlq_n_s16(a, 7);
	case 8:
		return vshlq_n_s16(a, 8);
	default: // return input as is for now
		return a;
	}

}

DSP_INLINE int32x4_t shift_left(const int32x4_t &a, const int &n)
{
	//int32x4_t  vshlq_n_s32(int32x4_t a, __constrange(0,31) int b);  // VSHL.I32 q0,q0,#0
	//return vshlq_n_s16(a, 0);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshlq_n_s32(a, 1);
	case 2:
		return vshlq_n_s32(a, 2);
	case 3:
		return vshlq_n_s32(a, 3);
	case 4:
		return vshlq_n_s32(a, 4);
	case 5:
		return vshlq_n_s32(a, 5);
	case 6:
		return vshlq_n_s32(a, 6);
	case 7:
		return vshlq_n_s32(a, 7);
	case 8:
		return vshlq_n_s32(a, 8);
	case 15:
		return vshlq_n_s32(a, 15);
	case 16:
		return vshlq_n_s32(a, 16);
	default: // return input as is for now
		return a;
	}

}

// Element-wise arithmetic right shift
DSP_INLINE int8x16_t shift_right(const int8x16_t &a, const int &n)
{
	// int8x16_t  vshrq_n_s8(int8x16_t a, __constrange(1,8) int b);    // VSHR.S8 q0,q0,#8
	//return vshrq_n_s8(a, n);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshrq_n_s8(a, 1);
	case 2:
		return vshrq_n_s8(a, 2);
	case 3:
		return vshrq_n_s8(a, 3);
	case 4:
		return vshrq_n_s8(a, 4);
	case 5:
		return vshrq_n_s8(a, 5);
	case 6:
		return vshrq_n_s8(a, 6);
	case 7:
		return vshrq_n_s8(a, 7);
	case 8:
		return vshrq_n_s8(a, 8);
	default: // return input as is for now
		return a;
	}

}
DSP_INLINE int16x8_t shift_right(const int16x8_t &a, const int &n)
{
	// int16x8_t  vshrq_n_s16(int16x8_t a, __constrange(1,16) int b);  // VSHR.S16 q0,q0,#16
	//return vshrq_n_s16(a, n);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshrq_n_s16(a, 1);
	case 2:
		return vshrq_n_s16(a, 2);
	case 3:
		return vshrq_n_s16(a, 3);
	case 4:
		return vshrq_n_s16(a, 4);
	case 5:
		return vshrq_n_s16(a, 5);
	case 6:
		return vshrq_n_s16(a, 6);
	case 7:
		return vshrq_n_s16(a, 7);
	case 8:
		return vshrq_n_s16(a, 8);
	default: // return input as is for now
		return a;
	}
}

DSP_INLINE int32x4_t shift_right(const int32x4_t &a, const int &n)
{
	// uint32x4_t vshrq_n_u32(uint32x4_t a, __constrange(1,32) int b); // VSHR.U32 q0,q0,#32
	//return vshrq_n_u32(a, n);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshrq_n_s32(a, 1);
	case 2:
		return vshrq_n_s32(a, 2);
	case 3:
		return vshrq_n_s32(a, 3);
	case 4:
		return vshrq_n_s32(a, 4);
	case 5:
		return vshrq_n_s32(a, 5);
	case 6:
		return vshrq_n_s32(a, 6);
	case 7:
		return vshrq_n_s32(a, 7);
	case 8:
		return vshrq_n_s32(a, 8);
	case 15:
		return vshrq_n_s32(a, 15);
	case 16:
		return vshrq_n_s32(a, 16);
	default: // return input as is for now
		return a;
	}
}

DSP_INLINE uint32x4_t shift_right(const uint32x4_t &a, const int &n)
{
	// uint32x4_t vshrq_n_u32(uint32x4_t a, __constrange(1,32) int b); // VSHR.U32 q0,q0,#32
	//return vshrq_n_u32(a, n);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshrq_n_u32(a, 1);
	case 2:
		return vshrq_n_u32(a, 2);
	case 3:
		return vshrq_n_u32(a, 3);
	case 4:
		return vshrq_n_u32(a, 4);
	case 5:
		return vshrq_n_u32(a, 5);
	case 6:
		return vshrq_n_u32(a, 6);
	case 7:
		return vshrq_n_u32(a, 7);
	case 8:
		return vshrq_n_u32(a, 8);
	case 16:
		return vshrq_n_u32(a, 16);
	default: // return input as is for now
		return a;
	}
}

DSP_INLINE uint64x2_t shift_right(const uint64x2_t &a, const int &n)
{
	// uint64x2_t vshrq_n_u64(uint64x2_t a, __constrange(1,64) int b); // VSHR.U64 q0,q0,#64
	//return vshrq_n_u32(a, n);
	switch(n){
	case 0:
		return a;
	case 1:
		return vshrq_n_u64(a, 1);
	case 2:
		return vshrq_n_u64(a, 2);
	case 3:
		return vshrq_n_u64(a, 3);
	case 4:
		return vshrq_n_u64(a, 4);
	case 5:
		return vshrq_n_u64(a, 5);
	case 6:
		return vshrq_n_u64(a, 6);
	case 7:
		return vshrq_n_u64(a, 7);
	case 8:
		return vshrq_n_u64(a, 8);
	case 32:
		return vshrq_n_u64(a, 32);
	default: // return input as is for now
		return a;
	}
}


// Saturated packs the 2 source vectors into one. The elements in resulting vector has half
// length of the source elements.
DSP_INLINE vs saturated_pack(const vi& a, const vi& b) { return (vs)_mm_packs_epi32(a, b); }
DSP_INLINE vcs saturated_pack(const vci& a, const vci& b) { return (vcs)_mm_packs_epi32(a, b); }
DSP_INLINE vb saturated_pack(const vs& a, const vs& b) { return (vb)_mm_packs_epi16(a, b); }
DSP_INLINE vcb saturated_pack(const vcs& a, const vcs& b) { return (vcb)_mm_packs_epi16(a, b); }

// Pack elements in 2 source vectors into returned vector
// Note:
//    1. each element will be packed to a half-length field, eg. __int32 --> __int16
DSP_INLINE vs pack(const vi& a, const vi& b)
{
/*
	const vi xmm6 = (vi)__0x0000FFFF0000FFFF0000FFFF0000FFFF();
	int32x4_t q8 = vandq_s32( a, xmm6);
	int32x4_t q9 = vandq_s32( b, xmm6);
	int32x4_t q10 = vshlq_n_s32(q9, 0x10);

	q9 = vorrq_s32( q8, q10 );

	return (vs)vreinterpretq_s16_s32( q9);
	*/
	int16x4x2_t t = vzip_s16(vmovn_s32(a), vmovn_s32(b));
	return (vs)vcombine_s16(t.val[0], t.val[1]);
}

// obtain a packed complex vector through two integer vector holds RE and IM values
DSP_INLINE 
void pack(vcs& r, const vi& re, const vi& im)
{	
	r = (vcs) pack(re, im);
}




// Add pair-wisely of element-wise multiplication product
// ie. (vs, vs) --> vi
//     r0 := (a0 * b0) + (a1 * b1)
//     r1 := (a2 * b2) + (a3 * b3)
//     r2 := (a4 * b4) + (a5 * b5)
//     r3 := (a6 * b6) + (a7 * b7)
DSP_INLINE vi pairwise_muladd(const vs& a, const vs& b) { const vi z0 = (vi)__0x00000000000000000000000000000000();return (vi)_mm_madd_epi16(z0, a, b); }
DSP_INLINE vi muladd(const vcs& a, const vcs& b) { const vi z0 = (vi)__0x00000000000000000000000000000000(); return (vi)_mm_madd_epi16(z0 ,a, b); }
DSP_INLINE vq muladd(const vci& a, const vci& b)
{
    vq q0, q1, c;
    q0 = _mm_mul_epi32(a, b);
    q1 = _mm_mul_epi32(shift_right((vuq&)a, 32), shift_right((vuq&)b, 32));
    c  = add(q0, q1);
    return c;
}

// Compute the squared norm of a complex vector
DSP_INLINE vi SquaredNorm(const vcs& a) { return muladd(a, a); }
DSP_INLINE vq SquaredNorm(const vci& a) { return muladd(a, a); }

#ifndef __ARM_NEON__
// Element-wise multiplication, leaving only lower half part product
DEFINE_OP_ARITHMETIC2(mul_low, vs, _mm_mullo_epi16);
DEFINE_OP_ARITHMETIC2(mul_low, vi, _mm_mullo_epi32);

// Element-wise multiplication, leaving only higher half part product
DEFINE_OP_ARITHMETIC2(mul_high, vs, _mm_mulhi_epi16);
#endif
// Comprise a real number vector and an imaginary vector into 2 complex vectors.
// r1 got the comprised complex numbers by comprising the low half of real and imaginary vectors,
// and r2 got high half.
DSP_INLINE void comprise(vci& r1, vci& r2, const vi& re, const vi& im)
{
    r1 = (vci)interleave_low(re, im);
    r2 = (vci)interleave_high(re, im);
}

// Negative real part of each complex numbers
DSP_INLINE vcs conjre(const vcs& a)
{ 
    const static vub::data_type value =
    {
        0x00, 0x80, 0x01, 0x00,
        0x00, 0x80, 0x01, 0x00,
        0x00, 0x80, 0x01, 0x00,
        0x00, 0x80, 0x01, 0x00,
    };
    const vcs z0 = (vcs)__0x00000000000000000000000000000000();
    return sign(z0, a, (vcs&)value);
}

// Compute approximate conjugate of each complex numbers in a vector, using xor to implement subtraction.
// This operation is used for better performance than the accurate one.
DSP_INLINE vcs conj(const vcs& a) {
	int16x8_t temp = vreinterpretq_s16_s32(__0xFFFF0000FFFF0000FFFF0000FFFF0000());
	return (vcs)veorq_s16(   temp  , a);
}

// Compute accurate conjugate of each complex numbers in a vector
DSP_INLINE vcs conj0(const vcs& a)
{
    const static vub::data_type value =
    {
        0x01, 0x00, 0x00, 0x80,
        0x01, 0x00, 0x00, 0x80,
        0x01, 0x00, 0x00, 0x80,
        0x01, 0x00, 0x00, 0x80,
    };
    const vcs z0 = (vcs)__0x00000000000000000000000000000000();
    return sign(z0, a, (vcs&)value);
}

// Swap real and image part of each complex number
DSP_INLINE vcs flip(const vcs& a)
{
    /*vcs b = permutate_low<0xb1>(a);
    return permutate_high<0xb1>(b);*/
	return (vcs)vrev32q_s16(a);
}
DSP_INLINE vcus flip(const vcus& a)
{
    return (vcus)flip((vcs&)a);
}
DSP_INLINE vci flip(const vci& a)
{
    //return (vci)permutate<1, 0, 3, 2>((vi&)a);
	return (vci)vrev64q_s32(a);
}

// Multiply the first source vector by the conjugate of the second source vector
// ie. re + j * im = a * conj(b)
// Returns:
//   1. re got all real parts of products, im got all imaginary parts
//   2. OR, low got all the lower elements of products, high got all the higher
//   3. elements of the resulting product has double-length elements compared to input vector
DSP_INLINE void conj_mul(vi& re, vi& im, const vcs& a, const vcs& b)
{
    vcs vs1 = flip(b);
    vs1 = conjre(vs1);
    re = pairwise_muladd((vs&)a, (vs&)b);
    im = pairwise_muladd((vs&)vs1, (vs&)a);
}
DSP_INLINE void conj_mul(vci& low, vci& high, const vcs& a, const vcs& b)
{
    vi re, im;
    conj_mul(re, im, a, b);
    low   = (vci)interleave_low(re, im);
    high  = (vci)interleave_high(re, im);
}
DSP_INLINE void conj_mul(vq& re, vq& im, const vci& a, const vci& b)
{
    vq temp1, temp2, temp3;
    temp3 = flip(a);
    temp1 = _mm_mul_epi32(temp3, b);
    temp2 = _mm_mul_epi32(shift_right((vuq&)temp3, 32), shift_right((vuq&)b, 32));
    im    = sub(temp1, temp2);
    re    = muladd(a, b);
}
DSP_INLINE void conj_mul(vcq& low, vcq& high, const vci& a, const vci& b)
{
    vq re, im;
    conj_mul(re, im, a, b);
    low   = (vcq)interleave_low(re, im);
    high  = (vcq)interleave_high(re, im);
}

// Multiply the first source vector by the second source vector
// ie. re + j * im = a * b
// Returns:
//   1. re got all real parts of products, im got all imaginary parts
//   2. OR, low got all the lower elements of products, high got all the higher
//   3. elements of the resulting product has double-length elements compared to input vector
DSP_INLINE void mul (vi& re, vi& im, const vcs& a, const vcs& b)
{
	vcs vs1 = conj0(b);
    vcs vs2 = flip(b);
	re = muladd(a, vs1);
	im = muladd(a, vs2);
}
DSP_INLINE void mul(vci& low, vci& high, const vcs& a, const vcs& b)
{ 
    vi re, im;
    mul(re, im, a, b);
    low   = interleave_low(re, im);
    high  = interleave_high(re, im);
}
DSP_INLINE void mul(vq& low, vq& high, const vi& a, const vi& b)
{
    vq q13, q24;
    q13 = _mm_mul_epi32(a, b);
    q24 = _mm_mul_epi32(shift_right((vuq&)a, 32), shift_right((vuq&)b, 32));
    low = interleave_low(q13, q24); 
    high = interleave_high(q13, q24);
}

//
// mul - perform multiple of two complex vectors and return
// the resulting complex vector
//
DSP_INLINE 
vcs mul (const vcs& a, const vcs& b)
{
	vi re, im;
	mul (re, im, a, b );
	
    re = (vi)shift_right (re, 15);
    im = (vi)shift_right (im, 15);

	return (vcs) pack(re, im);
}

// Multiply the first source by the conjugate of the second source, leaving low part product after right-shifting
// ie. return a * conj(b) >> nbits_right
DSP_INLINE vcs conj_mul_shift(const vcs& a, const vcs& b, int nbits_right)
{
    // b = Q3 I3 Q2 I2 Q1 I1 Q0 I0
    // vcs1 = -I3 Q3 -I2 Q2 -I1 Q1 -I0 Q0
    vcs vcs1 = conjre(a);
    vcs1 = flip(vcs1);
    
    vi vi0 = pairwise_muladd((vs&)a, (vs&)b);       // vi0 = I3 I2 I1 I0, where Ii=ac-bd (a+bj)*(c+dj)=(ac-bd)+(ad+bc)j
    vi vi1 = pairwise_muladd((vs&)vcs1, (vs&)b);    // vi1 = Q3 Q2 Q1 Q0

    // Shift right to normalize
    vi0 = (vi)shift_right(vi0, nbits_right);
    vi1 = (vi)shift_right(vi1, nbits_right);

    // Q3 I3 Q2 I2 Q1 I1 Q0 I0
    return (vcs)pack(vi0, vi1);
}

// Multiply and leave low part product after right-shifting
// ie. return a * b >> nbits_right
DSP_INLINE vcs mul_shift(const vcs& a, const vcs& b, int nbits_right)
{
    vi vi0 = pairwise_muladd((vs&)a, (vs)conj(b));
    vi vi1 = pairwise_muladd((vs&)a, (vs)flip(b));

    // Shift right to normalize
    vi0 = (vi)shift_right(vi0, nbits_right);
    vi1 = (vi)shift_right(vi1, nbits_right);

    // Q3 I3 Q2 I2 Q1 I1 Q0 I0
    return (vcs)pack(vi0, vi1);
}



// Approximately multiply by imaginary unit
DSP_INLINE vcs mul_j(const vcs& a)
{
	return (vcs)veorq_s16(flip(a), vreinterpretq_s16_s32(__0x0000FFFF0000FFFF0000FFFF0000FFFF()));
}



// Accurate element-wise absolute value of a vector
DEFINE_OP_ARITHMETIC1(abs0, vb, vabsq_s8);
DEFINE_OP_ARITHMETIC1(abs0, vs, vabsq_s16);
DEFINE_OP_ARITHMETIC1(abs0, vi, vabsq_s32);
//DEFINE_OP_ARITHMETIC1(abs0, vq, vabsq_s64);

// Compute element-wise minimal
DEFINE_OP_ARITHMETIC2(smin, vs, vminq_s16);
DEFINE_OP_ARITHMETIC2(smin, vcs, vminq_s16);  // what does min mean in complex case?
DEFINE_OP_ARITHMETIC2(smin, vub, vminq_u8);
DEFINE_OP_ARITHMETIC2(smin, vcub, vminq_u8);

//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smin, vb, vub);
//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smin, vcb, vub);
//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smin, vus, vs);
//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smin, vcus, vs);
DEFINE_OP_ARITHMETIC2(smin, vb, vminq_s8);
DEFINE_OP_ARITHMETIC2(smin, vcb, vminq_s8);  // what does min mean in complex case?
DEFINE_OP_ARITHMETIC2(smin, vus, vminq_u16);
DEFINE_OP_ARITHMETIC2(smin, vcus, vminq_u16);


DEFINE_OP_ARITHMETIC2(smax, vs, vmaxq_s16);
DEFINE_OP_ARITHMETIC2(smax, vcs, vmaxq_s16);
DEFINE_OP_ARITHMETIC2(smax, vub, vmaxq_u8);
DEFINE_OP_ARITHMETIC2(smax, vcub, vmaxq_u8);

//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smax, vb, vub);
//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smax, vcb, vub);
//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smax, vus, vs);
//DEFINE_OP_MINMAX_SIGNED_UNSIGNED(smax, vcus, vs);
DEFINE_OP_ARITHMETIC2(smax, vb, vmaxq_s8);
DEFINE_OP_ARITHMETIC2(smax, vcb, vmaxq_s8);
DEFINE_OP_ARITHMETIC2(smax, vus, vmaxq_u16);
DEFINE_OP_ARITHMETIC2(smax, vcus, vmaxq_u16);

DEFINE_OP_DUPLICATION16_OPERATION(hmin, vb, smin);
DEFINE_OP_DUPLICATION16_OPERATION(hmin, vub, smin);
DEFINE_OP_DUPLICATION8_OPERATION(hmin, vs, smin);
DEFINE_OP_DUPLICATION8_OPERATION(hmin, vus, smin);

// Duplicate the maximal element in the source vector to all elements of a vector128 type
DEFINE_OP_DUPLICATION16_OPERATION(hmax, vb, smax);
DEFINE_OP_DUPLICATION16_OPERATION(hmax, vub, smax);
DEFINE_OP_DUPLICATION8_OPERATION(hmax, vs, smax);
DEFINE_OP_DUPLICATION8_OPERATION(hmax, vus, smax);




//////////////////////////////////////////////////////////////////////////////
// Public APIs
DECLARE_PUBLIC_OP(abs);
DECLARE_PUBLIC_OP(abs0);
DECLARE_PUBLIC_OP(add);
//DECLARE_PUBLIC_OP(and);
//DECLARE_PUBLIC_OP(andnot);
DECLARE_PUBLIC_OP(average);
DECLARE_PUBLIC_OP(comprise);
DECLARE_PUBLIC_OP(conj);
DECLARE_PUBLIC_OP(conj0);
DECLARE_PUBLIC_OP(conj_mul);
DECLARE_PUBLIC_OP(conj_mul_shift);
DECLARE_PUBLIC_OP(conjre);
DECLARE_PUBLIC_OP(extract);
DECLARE_PUBLIC_OP(flip);
DECLARE_PUBLIC_OP(hadd);
//DECLARE_PUBLIC_OP(hadd4);
DECLARE_PUBLIC_OP(hmax);
DECLARE_PUBLIC_OP(hmin);
//DECLARE_PUBLIC_OP(insert);
DECLARE_PUBLIC_OP(interleave_high);
DECLARE_PUBLIC_OP(interleave_low);
DECLARE_PUBLIC_OP(is_great);
DECLARE_PUBLIC_OP(is_less);
DECLARE_PUBLIC_OP(load);
//DECLARE_PUBLIC_OP(move_mask);
//DECLARE_PUBLIC_OP(mul_high);
DECLARE_PUBLIC_OP(mul_j);
//DECLARE_PUBLIC_OP(mul_low);
DECLARE_PUBLIC_OP(mul_shift);
//DECLARE_PUBLIC_OP(or);
DECLARE_PUBLIC_OP(pack);
DECLARE_PUBLIC_OP(pairwise_muladd);
DECLARE_PUBLIC_OP(permutate);
//DECLARE_PUBLIC_OP(permutate16);
DECLARE_PUBLIC_OP(permutate_high);
DECLARE_PUBLIC_OP(permutate_low);
DECLARE_PUBLIC_OP(saturated_add);
DECLARE_PUBLIC_OP(saturated_hadd);
//DECLARE_PUBLIC_OP(saturated_hadd4);
DECLARE_PUBLIC_OP(saturated_pack);
DECLARE_PUBLIC_OP(saturated_sub);
DECLARE_PUBLIC_OP(set_all);
//DECLARE_PUBLIC_OP(set_all_bits);
//DECLARE_PUBLIC_OP(set_zero);
//DECLARE_PUBLIC_OP(shift_element_left);
//DECLARE_PUBLIC_OP(shift_element_right);
DECLARE_PUBLIC_OP(shift_left);
DECLARE_PUBLIC_OP(shift_right);
DECLARE_PUBLIC_OP(sign);
DECLARE_PUBLIC_OP(smax);
DECLARE_PUBLIC_OP(smin);
DECLARE_PUBLIC_OP(SquaredNorm);
DECLARE_PUBLIC_OP(store);
//DECLARE_PUBLIC_OP(store_nt);
DECLARE_PUBLIC_OP(sub);
DECLARE_PUBLIC_OP(unpack);
//DECLARE_PUBLIC_OP(xor);

//////////////////////////////////////////////////////////////////////////////
// Remove private macros from symbol table
#undef DECLARE_PUBLIC_OP
#undef PVECTOR_STRUCT
#undef DEFINE_OP_ARITHMETIC1
#undef DEFINE_TEMPLATE_OP_ARITHMETIC2
#undef DEFINE_OP_ARITHMETIC2
#undef DEFINE_OP_PERMUTATION
#undef DEFINE_OP_PERMUTATION4
#undef DEFINE_OP_SHIFT
#undef DEFINE_OP_REDUCE4
#undef DEFINE_OP_REDUCE
#undef DEFINE_OP_DUPLICATION16_OPERATION
#undef DEFINE_OP_DUPLICATION8_OPERATION
#undef DEFINE_OP_MINMAX_SIGNED_UNSIGNED
#undef DEFINE_OP_EXTRACT


struct SignalBlock
{
    static const int8_t size=7;
    int16x8_t _data[7];
    int16x8_t& operator[](unsigned short  index) { return _data[index]; }
    const int16x8_t& operator[](unsigned short  index) const { return _data[index]; }
    SignalBlock* operator&() { return (SignalBlock*)&_data[0]; }
};

