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
#include "types.h"


int32 __ext_sine(int32 x, int32 prec);
int32 __ext_cosine(int32 x, int32 prec);
int32 __ext_sum(int32 *arr, int alen);
int32 __ext_imin(int32 x, int32 y);
int32 __ext_imax(int32 x, int32 y);
int32 __ext_ceil(double d);
double __ext_log2(double d);
double __ext_sqrt(double d);
int16 __ext_sqrt_int16(int16 d);
int32 __ext_sqrt_int32(int32 d);
