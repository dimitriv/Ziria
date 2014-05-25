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
#include <string.h>


void bounds_check(int siz, int len, char *msg)
{
  if (siz <= len) {
    printf("Bounds check violation: %s\n", msg);
    printf("Size   = %d\n", siz);
    printf("Access = %d\n", len); 
    exit(-1);
  }
}

unsigned long bytes_copied = 0;

void blink_copy(void *dst, void *src, unsigned int siz) 
{
  bytes_copied += siz;
  memcpy(dst,src,siz);
}
