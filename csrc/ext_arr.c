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
#include <string.h>
#include "ext_arr.h"

// External functions that operate on arrays
int32 __ext_zero_int16(int16* arr, int length)
{ 
  memset((void *)arr, 0, length*sizeof(int16));
  return 0;
}

int32 __ext_zero_int32(int32* arr, int length)
{ 
  memset((void *)arr, 0, length*sizeof(int32));
  return 0;
}

int32 __ext_zero_complex16(complex16* arr, int length)
{ 
  memset((void *)arr, 0, length*sizeof(complex16));
  return 0;
}

int32 __ext_zero_complex32(complex32* arr, int length)
{ 
  memset((void *)arr, 0, length*sizeof(complex32));
  return 0;
}

int32 __ext_zero_bit(BitArrPtr arr, int length)
{
	Bit mask = 0;
	memset((void *)arr, 0, (length/8)*sizeof(Bit));
	if (length % 8 > 0)
	{
		mask = ~((1 << (length % 8)) - 1);
		arr[length / 8 + 1] &= mask;
	}
	return 0;

}





int32 __ext_copy_int16(int16* dst, int len, int16* src, int lens, int32 length)
{ 
  memcpy((void *)dst, (void*) src, length*sizeof(int16));
  return 0;
}

int32 __ext_copy_int32(int32* dst, int len, int32* src, int lens, int32 length)
{ 
  memcpy((void *)dst, (void*) src, length*sizeof(int32));
  return 0;
}

int32 __ext_copy_complex16(complex16* dst, int len, complex16* src, int lens, int32 length)
{ 
  memcpy((void *)dst, (void*) src, length*sizeof(complex16));
  return 0;
}

int32 __ext_copy_complex32(complex32* dst, int len, complex32* src, int lens, int32 length)
{ 
  memcpy((void *)dst, (void*) src, length*sizeof(complex32));
  return 0;
}

