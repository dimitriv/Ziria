// Data structure definition for complex numbers
#pragma once

#include <stdint.h>

typedef struct _COMPLEX8   { int8_t		re, im; } COMPLEX8,   *PCOMPLEX8;
typedef struct _COMPLEX16  { int16_t	re, im; } COMPLEX16,  *PCOMPLEX16;
typedef struct _COMPLEX32  { int32_t	re, im; } COMPLEX32,  *PCOMPLEX32;
typedef struct _COMPLEX64  { int64_t	re, im; } COMPLEX64,  *PCOMPLEX64;
typedef struct _COMPLEXU8  { uint8_t	re, im; } COMPLEXU8,  *PCOMPLEXU8;
typedef struct _COMPLEXU16 { uint16_t	re, im; } COMPLEXU16, *PCOMPLEXU16;
typedef struct _COMPLEXU32 { uint32_t	re, im; } COMPLEXU32, *PCOMPLEXU32;
typedef struct _COMPLEXU64 { uint64_t	re, im; } COMPLEXU64, *PCOMPLEXU64;
typedef struct _COMPLEXF   { float		re, im; } COMPLEXF,   *PCOMPLEXF;
