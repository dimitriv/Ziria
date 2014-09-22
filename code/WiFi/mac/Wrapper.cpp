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

#include "../../../csrc/sora_ip.c"
#include "../../../csrc/sora_ext_lib.c"
#include "../../../csrc/sora_ext_viterbi.c"
#include "../../../csrc/bit.c"
#include "../../../csrc/wpl_alloc.c"
#include "../../../csrc/numerics.c"
#include "../../../csrc/buf_bit.c"
#include "../../../csrc/buf_numerics8.c"
#include "../../../csrc/buf_numerics16.c"
#include "../../../csrc/buf_numerics32.c"
#include "../../../csrc/sora_threads.c"
#include "../../../csrc/sora_thread_queues.c"
#include "../../../csrc/ext_arr.c"
#include "../../../csrc/ext_math.c"
//Sora only supports debugPlot lib in WIN32 and this is the fast 64-bit version
#ifdef WIN32
	#include "sora_ext_visual.c"
#endif
#include "../../../csrc/sora_radio.c"
#include "../../../csrc/utils.c"

// New Sora specific - DEBUG
#include "sora_RegisterRW.cpp"

#include "params.c"
#include "mac_2threads.c"
#include "driver.c"
#include "tx_thread.c"
#include "rx_thread.c"


