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


#define INLINE_COMPILATION 1

#include "sora_ip.c"
#include "sora_ext_lib.c"
#include "sora_ext_viterbi.c"
#include "params.c"
#include "bit.c"
#include "wpl_alloc.c"
#include "numerics.c"
#include "buf_bit.c"
#include "buf_chunk.c"
#include "buf_numerics8.c"
#include "buf_numerics16.c"
#include "buf_numerics32.c"
#include "sora_threads.c"
#include "sora_thread_queues.c"
#include "ext_arr.c"
#include "ext_math.c"
//Sora only supports debugPlot lib in WIN32 and this is the fast 64-bit version
//#include "sora_ext_visual.c"
#include "sora_radio.c"
#include "bladerf_radio.c"
#include "single_thread_queues.c"
#include "utils.c"
#include "driver.c"
#include "test.c"


