#pragma once

#include <stdio.h>
#include <arm_neon.h>

#define ASSERT(x)                       (assert(x))

typedef int HRESULT;
typedef char FLAG, *PFLAG;

typedef char            *PCHAR;
typedef unsigned char   *PUCHAR;
typedef unsigned long   *PULONG;
typedef void            *PVOID;
typedef unsigned int	*PUINT;
typedef unsigned char   UCHAR;
typedef unsigned short  USHORT, *PUSHORT;

typedef short			SHORT, *PSHORT;

typedef unsigned int	UINT;
typedef unsigned long   ULONG;
typedef unsigned short	WORD;
typedef unsigned long   DWORD;
typedef long            LONG;
typedef uint8_t            BOOL;
typedef uint8_t            BOOLEAN;
typedef unsigned long LARGE_INTEGER;
/*
typedef struct {
    int8_t _data[16] __attribute__ ((aligned(16)));
} __m128i;
*/
typedef short*  m128i;

#define M128_BYTE_NUM                       16	//(sizeof(__m128i)/sizeof(BYTE))
#define M128_WORD_NUM                       8	//(sizeof(__m128i)/sizeof(WORD))
#define M128_DWORD_NUM                      4	//(sizeof(__m128i)/sizeof(DWORD))
#define M128_COMPLEX16_NUM                  4	//(sizeof(__m128i)/sizeof(COMPLEX16))

typedef struct _MDL
{
    struct _MDL *Next;
    short MdlSize;
    short MdlFlags;
    void * Process;
    ULONG *MappedSystemVa;
    ULONG *StartVa;
    ULONG ByteCount;
    ULONG ByteOffset;
} MDL;
typedef MDL *PMDL;

#define IN
#define OUT

#define FALSE                           0
#define TRUE                            1
