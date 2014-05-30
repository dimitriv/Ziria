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

#include<sora.h>



// Init the queue
int ts_init(int nc, size_t *sizes);

// Called by the uplink thread
void ts_put(int nc, char *input);

// Called by the downlink thread
bool ts_isFinished(int nc);

 // Called by the downlink thread
bool ts_get(int nc, char *output);

// Issued from upstream to downstream
void ts_reset(int nc);

// Issued from upstream to downstream
void ts_flush(int nc);

// Issued from upstream to downstream
void ts_finish(int nc);

bool ts_isEmpty(int nc);
bool ts_isFull(int nc);

// Free memory allocated for queues
void ts_free();
