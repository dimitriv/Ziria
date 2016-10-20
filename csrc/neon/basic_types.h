
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

#pragma once
#include <arm_neon.h>
#include "complex.h"

#define __forceinline inline __attribute__ ((always_inline))
#define DSP_INLINE __forceinline
#define DSP_INLINE1 inline


//typedef float32x4_t __m128;
//typedef int32x4_t __m128i;

typedef struct {
    int8_t _data[16] __attribute__ ((aligned(16)));
} __m128i;


typedef struct {
    int8_t _data[16] __attribute__ ((aligned(16)));
} __m128;

namespace vector128 { namespace details {
    template <class T> struct IsComplexType { enum { value = 0 }; };
    template <> struct IsComplexType<COMPLEX8> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEX16> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEX32> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEX64> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEXU8> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEXU16> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEXU32> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEXU64> { enum { value = 1 }; };
    template <> struct IsComplexType<COMPLEXF> { enum { value = 1 }; };

    template <bool condition, class T> struct Constraint;
    template <class T> struct Constraint<true, T> { typedef T type; };
} }

// Use SFINAE principle to constrain operator overload on only complex types
// ref: http://www.wambold.com/Martin/writings/template-parameter-constraints.html
template<typename TO, typename T>
typename vector128::details::Constraint<
vector128::details::IsComplexType<TO>::value && vector128::details::IsComplexType<T>::value,
TO>::type
 operator+=(TO& a, const T& b) { a.re += b.re; a.im += b.im; return a; }

template<typename TO, typename T>
typename vector128::details::Constraint<
vector128::details::IsComplexType<TO>::value && vector128::details::IsComplexType<T>::value,
TO>::type
 operator-=(TO& a, const T& b) { a.re -= b.re; a.im -= b.im; return a; }

template<typename T>
typename vector128::details::Constraint<
vector128::details::IsComplexType<T>::value,
T>::type
 operator>>(const T& a, int shift) { T t; t.re = a.re >> shift; t.im = a.im >> shift; return t; }

namespace vector128 { namespace details {
    template<typename T> struct traits { };
} }

//////////////////////////////////////////////////////////////////////////////
// Data structure definition for m128 wrapper types
// Note:
// 1. normally we use template to define similiar structs, but in this case,
// align() parameter is hard to defined by template parameter, so we use macro
// instead.
// 2. the structs all have constructors, so don't define global static objects
// in kernel mode, otherwise, .CRT section will be introduced.

#define PVECTOR_STRUCT(NEWSTRUCT, TRAW, TDATA, ALIGN)												\
struct NEWSTRUCT																					\
{																									\
    static const int8_t size = sizeof(TRAW) / sizeof(TDATA);										\
    typedef TDATA data_type[size] __attribute ((aligned(16)));										\
    typedef TRAW raw_type;																			\
    typedef TDATA elem_type;																		\
    NEWSTRUCT() { } 																				\
    explicit NEWSTRUCT(TRAW r) : _raw(r) { }														\
    template <typename TA> __forceinline explicit NEWSTRUCT(const TA& a) : _raw(TRAW(a)) { } 	\
    __forceinline NEWSTRUCT(const data_type& pa) : _raw(*(TRAW*)pa) { }								\
    __forceinline TDATA& operator[](size_t index) { return this->_data[index]; }					\
    __forceinline const TDATA& operator[](size_t index) const { return this->_data[index]; }		\
    __forceinline NEWSTRUCT& operator=(const NEWSTRUCT& a) { this->_raw = a._raw; return *this; }	\
    __forceinline NEWSTRUCT& operator=(const data_type& pa) {_raw = *(TRAW*)pa; return *this; }		\
    __forceinline NEWSTRUCT& operator=(const raw_type& r) {_raw = r; return *this; }				\
    __forceinline operator TRAW&() {return _raw; }													\
    __forceinline operator const TRAW&() const {return _raw;}										\
	private:																						\
    union{																							\
    	TRAW _raw;																					\
    	TDATA _data[size] __attribute__ ((aligned(16)));											\
    };																								\
};																									\
namespace vector128 { namespace details {															\
    template<> struct traits<NEWSTRUCT> {															\
        typedef NEWSTRUCT::raw_type tag;															\
    };																								\
}}
/*
PVECTOR_STRUCT(vb,   __m128i, int8_t,           16);
PVECTOR_STRUCT(vub,  __m128i, uint8_t,  		16);
PVECTOR_STRUCT(vs,   __m128i, int16_t,          16);
PVECTOR_STRUCT(vus,  __m128i, uint16_t, 		16);
PVECTOR_STRUCT(vi,   __m128i, int32_t,          16);
PVECTOR_STRUCT(vui,  __m128i, uint32_t, 		16);
PVECTOR_STRUCT(vq,   __m128i, int64_t,          16);
PVECTOR_STRUCT(vuq,  __m128i, uint64_t, 		16);
PVECTOR_STRUCT(vcb,  __m128i, COMPLEX8,         16);
PVECTOR_STRUCT(vcub, __m128i, COMPLEXU8,        16);
PVECTOR_STRUCT(vcs,  __m128i, COMPLEX16,        16);
PVECTOR_STRUCT(vcus, __m128i, COMPLEXU16,       16);
PVECTOR_STRUCT(vci,  __m128i, COMPLEX32,        16);
PVECTOR_STRUCT(vcui, __m128i, COMPLEXU32,       16);
PVECTOR_STRUCT(vcq,  __m128i, COMPLEX64,        16);
PVECTOR_STRUCT(vcuq, __m128i, COMPLEXU64,       16);
*/

PVECTOR_STRUCT(vb,   int8x16_t, int8_t,           16);
PVECTOR_STRUCT(vub,  uint8x16_t, uint8_t,  		16);
PVECTOR_STRUCT(vs,   int16x8_t, int16_t,          16);
PVECTOR_STRUCT(vus,  uint16x8_t, uint16_t, 		16);
PVECTOR_STRUCT(vi,   int32x4_t, int32_t,          16);
PVECTOR_STRUCT(vui,  uint32x4_t, uint32_t, 		16);
PVECTOR_STRUCT(vq,   int64x2_t, int64_t,          16);
PVECTOR_STRUCT(vuq,  uint64x2_t, uint64_t, 		16);
PVECTOR_STRUCT(vcb,  int8x16_t, COMPLEX8,         16);
PVECTOR_STRUCT(vcub, uint8x16_t, COMPLEXU8,        16);
PVECTOR_STRUCT(vcs,  int16x8_t, COMPLEX16,        16);
PVECTOR_STRUCT(vcus, uint16x8_t, COMPLEXU16,       16);
PVECTOR_STRUCT(vci,  int32x4_t, COMPLEX32,        16);
PVECTOR_STRUCT(vcui, uint32x4_t, COMPLEXU32,       16);
PVECTOR_STRUCT(vcq,  int64x2_t, COMPLEX64,        16);
PVECTOR_STRUCT(vcuq, uint64x2_t, COMPLEXU64,       16);

