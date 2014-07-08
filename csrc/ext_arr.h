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
#include "types.h"
#include "numerics.h"

int32 __ext_zero_int16(int16* arr, int length);
int32 __ext_zero_int32(int32* arr, int length);
int32 __ext_zero_complex16(complex16* arr, int length);
int32 __ext_zero_complex32(complex32* arr, int length);
int32 __ext_zero_bit(BitArrPtr arr, int length);



int32 __ext_copy_int16(int16* dst, int len, int16* src, int lens, int32 length);
int32 __ext_copy_int32(int32* dst, int len, int32* src, int lens, int32 length);
int32 __ext_copy_complex16(complex16* dst, int len, complex16* src, int lens, int32 length);
int32 __ext_copy_complex32(complex32* dst, int len, complex32* src, int lens, int32 length);

int32 __ext_int_to_bit(int32 input, BitArrPtr arr, int alength, int outlen);
