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
// Fixed point trigonometry 
#pragma once

//
// We support Fixed Point Radians in this file.
// FP_RAD maps PI (and -PI) to 0x8000 and it represents only radians [-PI, PI)
//
typedef short FP_RAD;  // fixed point radians

// FP_FRAC fixed point fraction - [-1,1)
//
typedef short FP_FRAC; // fixed point fraction



#include "intalglutx.h"
// Where the LUT's are stored


FINL 
FP_FRAC sinx ( FP_RAD r ) {
	return sinx_lut[(unsigned short)r];
}

FINL 
FP_FRAC cosx ( FP_RAD r ) {
	return cosx_lut[(unsigned short)r];
}


// bit_scope - find the highest bit position of an integer
FINL
unsigned char bit_scope_ub (unsigned char x) {
	return bit_high_pos_lutx[x];
}

FINL
unsigned char bit_scope_us (unsigned short x) {
	unsigned char tt;
	if ( tt = (x >> 8) ) {
		return bit_scope_ub (tt) + 8;
	} else {
		return bit_scope_ub ((unsigned char)(x));
	}	
}

FINL
unsigned char bit_scope_ui (unsigned int x) {
	unsigned short tt;
	if ( tt = (x >> 16) ) {
		return bit_scope_us (tt) + 16;
	} else {
		return bit_scope_us ((unsigned short)(x));
	}	
}


FINL
unsigned char bit_scope_s (int x) {
	if ( x>0 ) {
		// positive value
		return bit_scope_ui ((unsigned int)x);		
	} else {
		// negative value
		return bit_scope_ui ((unsigned int)(-x));		
	}
}

FINL
FP_FRAC atan2x ( int y, int x ) {
	int ys = bit_scope_s (y);
	int xs = bit_scope_s (x); 
	
	int shift = ((xs>ys)?xs:ys) - 6;
	
	if ( shift > 0 ) {
		return atan2x_lut[(unsigned char)(y>>shift)][(unsigned char)(x>>shift)];
	}	
	else 
		return atan2x_lut[(unsigned char)(y)][(unsigned char)(x)];
}

