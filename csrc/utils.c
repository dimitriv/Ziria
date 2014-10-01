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

unsigned long long bytes_copied = 0;

void blink_copy(void *dst, void *src, unsigned int siz) 
{
  bytes_copied += siz;
  memcpy(dst,src,siz);
}

/**
 * If `s` contains a trailing comma (possibly followed by whitespace) overwrite
 * it with 0 (that is, 0-terminate the string at that point) and return the
 * corresponding address so that the string can be restored later.
 *
 * Returns NULL if the string does not contain a trailing comma.
 *
 * The return value of `delete_trailing_comma` can be passed to 
 * `restore_trailing_comma`.
 */
char* delete_trailing_comma(char *s) {
  char* p = s + strlen(s) - 1;

  while(p > s) {
    switch(*p) {
      case ' ':
      case '\t':
      case '\n':
      case '\r':
        // Skip trailing whitespace
        p--; 
        break;

      case ',':
        // Overwrite trailing comma
        *p = 0;
        return p;

      default:
        // Found a different character. No trailing comma.
        return NULL;
    }
  }

  return NULL;
}

void restore_trailing_comma(char* trailing_comma) {
  if(trailing_comma != 0) {
    *trailing_comma = ',';
  }
}
