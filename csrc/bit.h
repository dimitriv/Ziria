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
#pragma once

typedef unsigned char Bit;
typedef unsigned char * BitArrPtr;


/* Note: Representation of bits and bit arrays
 * 
 * Bits are represented as 1 byte (char), with the LSB denoting the actual bit.
 *
 * Bit arrays (of a statically known length N) are represented as arrays of chars. 
 * Indexing starts from the LSB of lowest-address byte (index 0), towards the MSB,
 * and continues this way for higher addressed. The final byte may only be "partially"
 * filled in, with the payload residing in its least significant bits.
 *
 *************************************************************************************/

void bitArrRead(BitArrPtr src, unsigned int vstart, unsigned int vlen, BitArrPtr tgt);
void bitArrWrite(BitArrPtr src, unsigned int vstart, unsigned int vlen, BitArrPtr tgt);
void bitRead(BitArrPtr src,unsigned int vpos, Bit *tgt);
void bitWrite(BitArrPtr tgt,unsigned int vpos, Bit val);


void printBitArrLn(BitArrPtr arr, unsigned int vlen);
