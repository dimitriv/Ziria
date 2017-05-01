/*
Coyright (c) Northeastern University, GENESYS Lab
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



#include "basic_types.h"


#define _MM_SHUFFLE(z, y, x, w) (z<<6) | (y<<4) | (x<<2) | w


__forceinline uint8x16_t _mm_set_epi8(int8_t p1, int8_t p2, int8_t p3, int8_t p4,
		int8_t p5, int8_t p6, int8_t p7, int8_t p8,
		int8_t p9, int8_t p10, int8_t p11, int8_t p12,
		int8_t p13, int8_t p14, int8_t p15, int8_t p16)
{
	const int8_t p[16] = {p1, p2, p3, p4, p5, p6, p7, p8,
			p9, p10, p11, p12, p13, p14, p15, p16};
	return vreinterpretq_u8_s8( vld1q_s8(p) );
}

__forceinline int8x16_t _mm_loadu_si128 (int8_t *p)
{
	return vld1q_s8( p );
}

__forceinline void _mm_storeu_si128 (int8_t *p, int8x16_t a)
{
	vst1q_s8( p, a );
}


__forceinline int8x16_t _mm_add_epi8(int8x16_t a, int8x16_t b)
{
	return vaddq_s8(a, b);
}

__forceinline int16x8_t _mm_add_epi16(int16x8_t a, int16x8_t b)
{
	return vaddq_s16(a, b);
}

__forceinline int32x4_t _mm_add_epi32(int32x4_t a, int32x4_t b)
{
	return vaddq_s32 (a, b);
}

__forceinline int8x16_t _mm_sub_epi8(int8x16_t a, int8x16_t b)
{
	return vsubq_s8(a, b);
}

__forceinline int16x8_t _mm_sub_epi16(int16x8_t a, int16x8_t b)
{
	return vsubq_s16(a, b);
}

__forceinline int32x4_t _mm_sub_epi32(int32x4_t a, int32x4_t b)
{
	return vsubq_s32(a, b);
}

__forceinline int64x2_t _mm_mul_epi32(int32x4_t a, int32x4_t b)
{
/*
	int32x4_t q8 = a;
	int32_t lane2 = vgetq_lane_s32(q8 , 2);
	q8 = vsetq_lane_s32(lane2, q8 ,1);

	int32x4_t q0 = b;
	lane2 = vgetq_lane_s32(q0 , 2);
	q0 = vsetq_lane_s32(lane2, q0 ,1);
	return vmull_s32(vget_low_s32(q0),vget_low_s32(q8));
*/
	int32x4x2_t temp;
	temp = vuzpq_s32(a, b);

	return vmull_s32(vget_low_s32(temp.val[0]),vget_high_s32(temp.val[0]));
}

#define VSHRQ_S32_U32_S32( A , N )	\
	vreinterpretq_s32_u32( vshrq_n_u32(vreinterpretq_u32_s32( A ) , N) )

__forceinline int32x4_t _mm_madd_epi16(const int32x4_t &z0, const int16x8_t &a, const int16x8_t &b)
{

	int16x8x2_t temp;
	temp = vuzpq_s16(a, b);
	//int32x4_t a_high = vmovl_s16 ( vget_high_s16( temp.val[0]) );
	//int32x4_t a_low  = vmovl_s16 ( vget_low_s16( temp.val[0] ) );
	int32x4_t result_a = vmlal_s16 (z0, vget_high_s16( temp.val[0]), vget_low_s16( temp.val[0] ));

	//int32x4_t b_high = vmovl_s16 ( vget_high_s16( temp.val[1]) );
	//int32x4_t b_low  = vmovl_s16 ( vget_low_s16( temp.val[1] ) );
	int32x4_t result_b = vmlal_s16 (z0, vget_high_s16( temp.val[1]), vget_low_s16( temp.val[1] ));

	//int32x4_t result_a = vmulq_s32( a_high , a_low   );
	//int32x4_t result_b = vmulq_s32( b_high , b_low );

	return vaddq_s32 ( result_a,  result_b );

}

__forceinline int8x16_t _mm_sign_epi8(int8x16_t zerovec8, int8x16_t a, int8x16_t b)
{

	//int8x16_t zerovec8 = vdupq_n_s8(0); //{ 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 };
	int8x16_t c1 = vcgtq_s8(b, zerovec8);
	int8x16_t ret = vandq_s8(a, c1);
	int8x16_t c2 = vcltq_s8(b, zerovec8);
	return vaddq_s8(ret, vsubq_s8(zerovec8, vandq_s8(a, c2)));

}

__forceinline int16x8_t _mm_sign_epi16(int16x8_t zerovec16, int16x8_t a, int16x8_t b)
{

	//int16x8_t zerovec16 = vdupq_n_s16(0); //{ 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 };
	int16x8_t c1 = vcgtq_s16(b, zerovec16);
	int16x8_t ret = vandq_s16(a, c1);
	int16x8_t c2 = vcltq_s16(b, zerovec16);
	return vaddq_s16(ret, vsubq_s16(zerovec16, vandq_s16(a, c2)));
// asm
/*
	asm volatile
		"vld1.16   {q0}, [a] 	            \n\t"
		"vld1.16   {q1}, [b] 	            \n\t"
		"vld1.16   {q7}, [zerovec16]        \n\t"
		"vcgt.s16  {q2}, {q1}, #0	    \n\t"
		"vclt.s16  {q3}, {q1}, #0           \n\t"
		"vand      {q2}, {q2}, {q0}         \n\t"
		"vand      {q3}, {q3}, {q0}         \n\t"
                "vsub.i16  {q3}, {q7}, {q3}         \n\t"
                "vadd.i16  {q2}, {q2}, {q3}         \n\t"
		: "=r" (y)
		: "0" (zerovec16), "1" (a), "2" (b) 
	);
*/

}

__forceinline int32x4_t _mm_sign_epi32(int32x4_t zerovec32, int32x4_t a, int32x4_t b)
{

	//int32x4_t zerovec32 = vdupq_n_s32(0); //{ 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 };
	int32x4_t c1 = vcgtq_s32(b, zerovec32);
	int32x4_t ret = vandq_s32(a, c1);
	int32x4_t c2 = vcltq_s32(b, zerovec32);
	return vaddq_s32(ret, vsubq_s32(zerovec32, vandq_s32(a, c2)));
}

__forceinline int8x16_t _mm_packs_epi16(int16x8_t a, int16x8_t b)
{
	return vcombine_s8(vqmovn_s16(a), vqmovn_s16(b));
}


__forceinline int16x8_t _mm_packs_epi32(int32x4_t a, int32x4_t b)
{
	return vcombine_s16(vqmovn_s32(a), vqmovn_s32(b));
}

__forceinline uint8x16_t _mm_andnot_si128(uint8x16_t a, uint8x16_t b)
{
	//return vandq_u8( vmvnq_u8 ( a ), b);
	return vbicq_u8(b, a);
}

__forceinline int8x16_t _mm_andnot_si128(int8x16_t a, int8x16_t b)
{
	return vbicq_s8(b, a);
	//return vandq_s8( vmvnq_s8 ( a ), b);
}

__forceinline int16x8_t _mm_andnot_si128(int16x8_t a, int16x8_t b)
{
	return vbicq_s16(b, a);
}

__forceinline uint8x16_t _mm_and_si128(uint8x16_t a, uint8x16_t b)
{
	return vandq_u8(a, b);
}

__forceinline int8x16_t _mm_and_si128(int8x16_t a, int8x16_t b)
{
	return vandq_s8(a, b);
}

__forceinline int16x8_t _mm_and_si128(int16x8_t a, int16x8_t b)
{
	return vandq_s16(a, b);
}

__forceinline int8x16_t _mm_xor_si128(int8x16_t a, int8x16_t b)
{
	return veorq_s8(a, b);
}

__forceinline int16x8_t _mm_xor_si128(int16x8_t a, int16x8_t b)
{
	return veorq_s16(a, b);
}

__forceinline uint8x16_t _mm_or_si128(uint8x16_t a, uint8x16_t b)
{
	return vorrq_u8(a, b);
}

__forceinline int8x16_t _mm_or_si128(int8x16_t a, int8x16_t b)
{
	return vorrq_s8(a, b);
}

__forceinline int16x8_t _mm_or_si128(int16x8_t a, int16x8_t b)
{
	return vorrq_s16(a, b);
}

__forceinline int32x4_t _mm_or_si128(int32x4_t a, int32x4_t b)
{
	return vorrq_s32(a, b);
}

__forceinline int32x4_t _mm_shuffle_epi_1032(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vget_high_s32(a), vget_low_s32(b));
}

__forceinline int32x4_t _mm_shuffle_epi_2301(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vrev64_s32(vget_low_s32(a)), vrev64_s32(vget_high_s32(b)));
}

__forceinline int32x4_t _mm_shuffle_epi_0321(int32x4_t a, int32x4_t b)
{
	return vextq_s32(a, b, 1);
}

__forceinline int32x4_t _mm_shuffle_epi_2103(int32x4_t a, int32x4_t b)
{
	return vextq_s32(a, b, 3);
}

__forceinline int32x4_t _mm_shuffle_epi_1010(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vget_low_s32(a), vget_low_s32(a));
}

__forceinline int32x4_t _mm_shuffle_epi_1001(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vrev64_s32(vget_low_s32(a)), vget_low_s32(b));
}

__forceinline int32x4_t _mm_shuffle_epi_0101(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vrev64_s32(vget_low_s32(a)), vrev64_s32(vget_low_s32(b)));
}

__forceinline int32x4_t _mm_shuffle_epi_2211(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vdup_n_s32(vgetq_lane_s32(a, 1)), vdup_n_s32(vgetq_lane_s32(b, 2)));
}

__forceinline int32x4_t _mm_shuffle_epi_0122(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vdup_n_s32(vgetq_lane_s32(a, 2)), vrev64_s32(vget_low_s32(b)));
}

__forceinline int32x4_t _mm_shuffle_epi_3332(int32x4_t a, int32x4_t b)
{
	return vcombine_s32(vget_high_s32(a), vdup_n_s32(vgetq_lane_s32(b, 3)));
}


template <int i >
__forceinline int32x4_t _mm_shuffle_epi32_default(int32x4_t a, int32x4_t b)
{
	int32x4_t ret = vmovq_n_s32(vgetq_lane_s32(a, i & 0x3));
	ret = vsetq_lane_s32(vgetq_lane_s32(a, (i >> 2) & 0x3), ret, 1);
	ret = vsetq_lane_s32(vgetq_lane_s32(b, (i >> 4) & 0x3), ret, 2);
	ret = vsetq_lane_s32(vgetq_lane_s32(b, (i >> 6) & 0x3), ret, 3);
	return ret;
}

template <int i >
__forceinline int32x4_t _mm_shuffle_epi32_function(int32x4_t a, int32x4_t b)
{
	switch (i)
	{
		case _MM_SHUFFLE(1, 0, 3, 2): return _mm_shuffle_epi_1032(a, b); break;
		case _MM_SHUFFLE(2, 3, 0, 1): return _mm_shuffle_epi_2301(a, b); break;
		case _MM_SHUFFLE(0, 3, 2, 1): return _mm_shuffle_epi_0321(a, b); break;
		case _MM_SHUFFLE(2, 1, 0, 3): return _mm_shuffle_epi_2103(a, b); break;
		case _MM_SHUFFLE(1, 0, 1, 0): return _mm_shuffle_epi_1010(a, b); break;
		case _MM_SHUFFLE(1, 0, 0, 1): return _mm_shuffle_epi_1001(a, b); break;
		case _MM_SHUFFLE(0, 1, 0, 1): return _mm_shuffle_epi_0101(a, b); break;
		case _MM_SHUFFLE(2, 2, 1, 1): return _mm_shuffle_epi_2211(a, b); break;
		case _MM_SHUFFLE(0, 1, 2, 2): return _mm_shuffle_epi_0122(a, b); break;
		case _MM_SHUFFLE(3, 3, 3, 2): return _mm_shuffle_epi_3332(a, b); break;
		default: return _mm_shuffle_epi32_default<i>(a, b);
	}
}

template <int i >
__forceinline int32x4_t _mm_shuffle_epi32_splat(int32x4_t a)
{
	return vdupq_n_s32(vgetq_lane_s32(a, i));
}

template <int i>
__forceinline int32x4_t _mm_shuffle_epi32_single(int32x4_t a)
{
	switch (i)
	{
		case _MM_SHUFFLE(0, 0, 0, 0): return _mm_shuffle_epi32_splat<0>(a); break;
		case _MM_SHUFFLE(1, 1, 1, 1): return _mm_shuffle_epi32_splat<1>(a); break;
		case _MM_SHUFFLE(2, 2, 2, 2): return _mm_shuffle_epi32_splat<2>(a); break;
		case _MM_SHUFFLE(3, 3, 3, 3): return _mm_shuffle_epi32_splat<3>(a); break;
		default: return _mm_shuffle_epi32_function<i>(a, a);
	}
}

#define _mm_shuffle_epi32(a,i) _mm_shuffle_epi32_single<i>(a)


template <int i >
__forceinline int16x8_t _mm_shufflehi_epi16_default(int16x8_t a){
	int16x8_t ret = a;
	int16x4_t highBits = vget_high_s16(ret);
	ret = vsetq_lane_s16(vget_lane_s16(highBits, i & 0x3), ret, 4);
	ret = vsetq_lane_s16(vget_lane_s16(highBits, (i >> 2) & 0x3), ret, 5);
	ret = vsetq_lane_s16(vget_lane_s16(highBits, (i >> 4) & 0x3), ret, 6);
	ret = vsetq_lane_s16(vget_lane_s16(highBits, (i >> 6) & 0x3), ret, 7);
	return ret;
}

template <int i>
__forceinline int16x8_t _mm_shufflehi_epi16_function(int16x8_t a)
{
	switch (i)
	{
		case _MM_SHUFFLE(1, 0, 3, 2):
			return vreinterpretq_s16_s32(vcombine_s32( vget_low_s32(a), vrev64_s32(vget_high_s32(a)) )); break;
		default:
			return _mm_shufflehi_epi16_default<i>(a);
	}
}

#define _mm_shufflehi_epi16(a,i) _mm_shufflehi_epi16_function<i>(a)

template <int i >
__forceinline int16x8_t _mm_shufflelo_epi16_default(int16x8_t a){
	int16x8_t ret = a;
	int16x4_t lowBits = vget_low_s16(ret);
	ret = vsetq_lane_s16(vget_lane_s16(lowBits, i & 0x3), ret, 0);
	ret = vsetq_lane_s16(vget_lane_s16(lowBits, (i >> 2) & 0x3), ret, 1);
	ret = vsetq_lane_s16(vget_lane_s16(lowBits, (i >> 4) & 0x3), ret, 2);
	ret = vsetq_lane_s16(vget_lane_s16(lowBits, (i >> 6) & 0x3), ret, 3);
	return ret;
}

template <int i>
__forceinline int16x8_t _mm_shufflelo_epi16_function(int16x8_t a)
{
	switch (i)
	{
		case _MM_SHUFFLE(1, 0, 3, 2):
			return vreinterpretq_s16_s32(vcombine_s32( vrev64_s32(vget_low_s32(a)), vget_high_s32(a) )); break;
		default:
			return _mm_shufflelo_epi16_default<i>(a);
	}
}

#define _mm_shufflelo_epi16(a,i) _mm_shufflelo_epi16_function<i>(a)


#define _mm_slli_si128( a, imm ) (__m128i)vextq_s8(vdupq_n_s8(0), (int8x16_t)a, 16 - (imm))

__forceinline int8x16_t _mm_unpacklo_epi8(int8x16_t a, int8x16_t b)
{
	int8x16x2_t inter_low = vzipq_s8 (a, b);

	return inter_low.val[0];

}

__forceinline uint8x16_t _mm_unpacklo_epi8(uint8x16_t &a, uint8x16_t &b)
{
	uint8x16x2_t inter_low = vzipq_u8 (a, b);

	return inter_low.val[0];
}

__forceinline int16x8_t _mm_unpacklo_epi16(int16x8_t a, int16x8_t b)
{
	int16x8x2_t inter_low = vzipq_s16 (a, b);

	return inter_low.val[0];

}

__forceinline uint16x8_t _mm_unpacklo_epi16(uint16x8_t a, uint16x8_t b)
{
	uint16x8x2_t inter_low = vzipq_u16 (a, b);

	return inter_low.val[0];

}

__forceinline int32x4_t _mm_unpacklo_epi32(int32x4_t a, int32x4_t b)
{
	int32x2_t a1 = vget_low_s32(a);
	int32x2_t b1 = vget_low_s32(b);

	int32x2x2_t result = vzip_s32(a1, b1);

	return vcombine_s32(result.val[0], result.val[1]);
}

__forceinline uint32x4_t _mm_unpacklo_epi32(uint32x4_t a, uint32x4_t b)
{
	uint32x2_t a1 = vget_low_u32(a);
	uint32x2_t b1 = vget_low_u32(b);

	uint32x2x2_t result = vzip_u32(a1, b1);

	return vcombine_u32(result.val[0], result.val[1]);
}

__forceinline int64x2_t _mm_unpacklo_epi64(int64x2_t a, int64x2_t b)
{
	int64x1_t a1 = vget_low_s64(a);
	int64x1_t b1 = vget_low_s64(b);

	return vcombine_s64(a1, b1);
}

__forceinline uint64x2_t _mm_unpacklo_epi64(uint64x2_t a, uint64x2_t b)
{
	uint64x1_t a1 = vget_low_u64(a);
	uint64x1_t b1 = vget_low_u64(b);

	return vcombine_u64(a1, b1);
}

__forceinline int8x16_t _mm_unpackhi_epi8(int8x16_t a, int8x16_t b)
{
	int8x16x2_t inter_high = vzipq_s8 (a, b);

	return inter_high.val[1];

}

__forceinline uint8x16_t _mm_unpackhi_epi8(uint8x16_t a, uint8x16_t b)
{
	uint8x16x2_t inter_high = vzipq_u8 (a, b);

	return inter_high.val[1];

}

__forceinline int16x8_t _mm_unpackhi_epi16(int16x8_t a, int16x8_t b)
{
	int16x8x2_t inter_high = vzipq_s16 ( vcombine_s16( vget_high_s16(a) , vget_low_s16(a) ), b);

	return inter_high.val[0];

}

__forceinline uint16x8_t _mm_unpackhi_epi16(uint16x8_t a, uint16x8_t b)
{
	uint16x8x2_t inter_high = vzipq_u16 ( vcombine_u16( vget_high_u16(a) , vget_low_u16(a) ), b);

	return inter_high.val[0];

}

__forceinline int32x4_t _mm_unpackhi_epi32(int32x4_t a, int32x4_t b)
{
	int32x2_t a1 = vget_high_s32(a);
	int32x2_t b1 = vget_high_s32(b);

	int32x2x2_t result = vzip_s32(a1, b1);

	return vcombine_s32(result.val[0], result.val[1]);
}

__forceinline uint32x4_t _mm_unpackhi_epi32(uint32x4_t a, uint32x4_t b)
{
	uint32x2_t a1 = vget_high_u32(a);
	uint32x2_t b1 = vget_high_u32(b);

	uint32x2x2_t result = vzip_u32(a1, b1);

	return vcombine_u32(result.val[0], result.val[1]);
}

__forceinline int64x2_t _mm_unpackhi_epi64(int64x2_t a, int64x2_t b)
{
	int64x1_t a1 = vget_high_s64(a);
	int64x1_t b1 = vget_high_s64(b);

	return vcombine_s64(a1, b1);
}

__forceinline uint64x2_t _mm_unpackhi_epi64(uint64x2_t a, uint64x2_t b)
{
	uint64x1_t a1 = vget_high_u64(a);
	uint64x1_t b1 = vget_high_u64(b);

	return vcombine_u64(a1, b1);
}

#define _mm_extract_epi16( a, imm ) vgetq_lane_s16((int16x8_t)a, imm)


__forceinline int16x8_t _mm_mullo_epi16(int16x8_t a, int16x8_t b)
{
	return vmulq_s16(a, b);
}

__forceinline int32x4_t _mm_mullo_epi32 (int32x4_t a, int32x4_t b)
{
	return vmulq_s32(a, b);
}

