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


	if (off == 0 && vlen == 8) {
	  *tgt = src[sidx];
	  return;
	}

	unsigned char ffo = 8 - off;       // Reverse offset

	unsigned int vend = vstart+vlen-1; // Index of last bit to be included
	unsigned int eidx = vend >> 8;      // Index of last relevant byte

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

	if (off == 0 && vlen == 8) {
	  tgt[sidx] = *src;
	  return;
	}

	unsigned char ffo = 8 - off;     // Opposite of offset 
	unsigned int n    = vlen & 7;       // #bits used in the last byte of src

	unsigned int res = n + off; // total # of bits we have to patch in the end

	unsigned int i;
	unsigned char carry;
	unsigned char mask = ~ 0;       // 1111111


	/* First carry is whatever was already in the "off" part of target */ 
	carry = tgt[sidx] & (mask >> ffo);

	for (i = 0; i < vlen/8; i++)
	{
		tgt[sidx+i] = (src[i] << off) | carry;
		carry = src[i] >> ffo;
	}

	// patch target with whatever is needed in the end
	if (res)
	{ 
		unsigned char hsrc   = src[i] & (mask >> (8-n));
		unsigned char loval  = (hsrc << off) | carry;
		unsigned int lowidth = (res>8)?8:res;
		unsigned char hival  = hsrc >> ffo;
		unsigned int hiwidth = (res>8)?(res-8):0;

		tgt[sidx+i] = (tgt[sidx+i] & (mask << lowidth)) | loval;
		if (hiwidth) 
			tgt[sidx+i+1] = (tgt[sidx+i+1] & (mask << hiwidth)) | hival;
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
	unsigned int i;
	for (i =0; i < vlen; i++)
	{
		Bit tmp;
		bitRead(arr,i,&tmp);
		printf("%d",tmp);
	}
	printf("\n");
}

