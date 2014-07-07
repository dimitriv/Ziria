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
#include <math.h>
#include "ext_math.h"


// Floating point trigonometry (slow)
int16 __ext_sine_double_int16(int16 x, int16 prec)
{ 
  return (int16) (sin(((float)x) / prec)*prec);
}

int16 __ext_cosine_double_int16(int16 x, int16 prec)
{ 
  return (int16) (cos(((float)x) / prec)*prec);
}

int32 __ext_sine_double_int32(int32 x, int32 prec)
{
	return (int32)(sin(((float)x) / prec)*prec);
}

int32 __ext_cosine_double_int32(int32 x, int32 prec)
{
	return (int32)(cos(((float)x) / prec)*prec);
}



int16 __ext_imin_int16(int16 x, int16 y){
  return (((x) < (y))?(x):(y));
}

int32 __ext_imin_int32(int32 x, int32 y){
	return (((x) < (y)) ? (x) : (y));
}

int16 __ext_imax_int16(int16 x, int16 y){
	return (((x) > (y)) ? (x) : (y));
}

int32 __ext_imax_int32(int32 x, int32 y){
  return (((x) > (y))?(x):(y));
}

int32 __ext_ceil_int32(double d){
	return (int32) ceil(d);
}

int16 __ext_ceil_int16(double d){
	return (int16)ceil(d);
}

double __ext_log2(double d){
	return log(d) / log(2.0);
}

double __ext_sqrt(double d){
	return sqrt(d);
}

int16 __ext_sqrt_int16(int16 d){
  return (int16) sqrt((double) d);
}

int32 __ext_sqrt_int32(int32 d){
  return (int32) sqrt((double) d);
}
