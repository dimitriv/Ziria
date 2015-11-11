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
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include <xmmintrin.h>
#include <emmintrin.h>

#include "types.h"
#include "bit.h"


/* Effectively implements:  tgt := src[vstart ... (vstart+vlen-1)] 
 * Precondition: tgt has allocated (vlen+7)/8 bytes already
 ************************************************************************************/
FORCE_INLINE
void bitArrRead(BitArrPtr src, unsigned int vstart, unsigned int vlen, BitArrPtr tgt) 
{

  	unsigned int sidx = vstart >> 3;    // Index of first relevant byte
  	unsigned char off = vstart & 7;    // Offset

	// Fast path for aligned short reads
	if (off == 0) {
		if (vlen <= 8) {
			tgt[0] = src[sidx];
			return;
		}
		if (vlen <= 16) {
			*((int16 *)tgt) = *((int16 *)&src[sidx]);
			return;
		}
		if (vlen <= 24) {
			*((int16 *)tgt) = *((int16 *)&src[sidx]);
			tgt[2] = src[sidx + 2];
			return;
		}
		if (vlen <= 32) {
			*(int32 *)tgt = *(int32 *)&src[sidx];
			return;
		}
		if (vlen <= 40) {
			*(int32 *) tgt = *(int32 *)&src[sidx];
			tgt[4] = src[sidx + 4];
			return;
		}
		if (vlen <= 48) {
			*(int32 *)tgt = *(int32 *)&src[sidx];
			*(int16 *)&tgt[4] = *(int16 *)&src[sidx+4];
			return;
		}
	}


	if (off == 0 && (vlen & 7) == 0) {
		for (unsigned int i = 0; i < vlen / 8; i++)
			tgt[i] = src[sidx + i];
		return;
	}

	unsigned char ffo = 8 - off;       // Reverse offset

	unsigned int vend = vstart+vlen-1; // Index of last bit to be included
	unsigned int eidx = vend >> 3;      // Index of last relevant byte

	int i;
	unsigned char carry;


	carry = src[eidx] << ffo; 

	for (i = eidx-1; i >= (int) sidx; i--) 
	{
		tgt[i-sidx] = (src[i] >> off) | carry;
		carry = src[i] << ffo;
	}

	// Store in the final bit only if we have to.
	// The conditional is important as the target may not have enough space.
	if (vend % 8 >= off) tgt[eidx-sidx] = src[eidx] >> off;
}


/* Effectively implements: tgt[vstart...(vstart+vlen-1)] := src 
 * Precondition: tgt has allocated (vlen+7)/8 bytes already 
 **************************************************************************************/
FORCE_INLINE
void bitArrWrite(BitArrPtr src, unsigned int vstart, unsigned int vlen, BitArrPtr tgt)
{
  	unsigned int sidx = vstart >> 3;  // Start index
  	unsigned char off = vstart & 7;  // Offset (width of carry)

	// Fast copy for aligned pointers
	if (off == 0 && (vlen & 7) == 0) {
		for (unsigned int i = 0; i < vlen / 8; i++)
			tgt[sidx + i] = src[i];
		return;
	}

	// Specialized code for width smaller 16 that can be aligned, since they are frequent
	// Note, this is guaranteed to access only one tgt mem location 
	// and requires only one copy
	if (off + vlen <= 16) {
		unsigned short * src16 = (unsigned short *)src;
		unsigned short * tgt16 = (unsigned short *)(tgt + sidx);
		unsigned short mask16 = ~0;       // 111111111111111

		if (off == 0) 
		{
			// Having this branch separately saves 2% on TX48
			tgt16[0] = (tgt16[0] & (mask16 << vlen))
				| (src16[0] & (mask16 >> (16 - vlen)));
		}
		else
		{
			tgt16[0] = (tgt16[0] & (mask16 << (vlen + off)))
				| ((src16[0] & (mask16 >> (16 - vlen))) << off)
				| (tgt16[0] & (mask16 >> (16 - off)));
		}
		return;
	}

	unsigned char ffo = 8 - off;     // Opposite of offset 
	unsigned int n    = vlen & 7;       // #bits used in the last byte of src

	unsigned int res = n + off; // total # of bits we have to patch in the end

	unsigned int i;
	unsigned char carry;
	unsigned char mask = ~ 0;       // 1111111


	/* First carry is whatever was already in the "off" part of target */ 
	if (ffo == 8)
	{
		carry = 0;
	}
	else
	{
		carry = tgt[sidx] & (mask >> ffo);
	}

	for (i = 0; i < vlen/8; i++)
	{
		if (ffo == 8)
		{
			tgt[sidx + i] = src[i];
		}
		else
		{
			tgt[sidx + i] = (src[i] << off) | carry;
			carry = src[i] >> ffo;
		}
	}

	// patch target with whatever is needed in the end
	if (res)
	{ 
		unsigned char hsrc = src[i] & (mask >> (8 - n));
		unsigned char loval = (hsrc << off) | carry;
		unsigned int lowidth = (res>8) ? 8 : res;
		unsigned char hival = hsrc >> ffo;
		unsigned int hiwidth = (res>8) ? (res - 8) : 0;

		if (lowidth < 8)
		{
			tgt[sidx + i] = (tgt[sidx + i] & (mask << lowidth)) | loval;
		}
		else
		{
			tgt[sidx + i] = loval;
		}
		if (hiwidth)
		{
			tgt[sidx + i + 1] = (tgt[sidx + i + 1] & (mask << hiwidth)) | hival;
		}
	}
}


void bitRead(BitArrPtr src,unsigned int vpos, Bit *tgt)
{
	*tgt = (src[vpos/8] >> (vpos % 8)) & 1;
	// NB: We could call: bitArrRead(src,pos,1,tgt) 
	// but its a bit indirect and slightly more expensive.
}

void bitWrite(BitArrPtr tgt,unsigned int vpos, Bit val)
{
	Bit hval = val & 1;
	unsigned char msk = 1 << (vpos % 8);
	if (hval)
		tgt[vpos/8] |= msk;
	else
		tgt[vpos/8] &= ~msk;
}


void printBitArrLn(BitArrPtr arr, unsigned int vlen)
{
	printBitArr(arr, vlen);
	printf("\n");
}

void printBitArr(BitArrPtr arr, unsigned int vlen)
{
	unsigned int i;
	for (i = 0; i < vlen; i++)
	{
		Bit tmp;
		bitRead(arr, i, &tmp);
		printf("%d", tmp);
	}
}



// LUT-specific operations
// v = l & m | v & ~ m
void lutmask128(BitArrPtr v, BitArrPtr m, BitArrPtr l)
{
		// could be unaligned ...
		__m128i mv = _mm_loadu_si128((__m128i *) v);
		__m128i mm = _mm_loadu_si128((__m128i *) m);
		__m128i ml = _mm_loadu_si128((__m128i *) l);

		mv = _mm_or_si128(_mm_and_si128(ml, mm), _mm_andnot_si128(mm, mv));
		_mm_storeu_si128((__m128i *) v, mv);
}

