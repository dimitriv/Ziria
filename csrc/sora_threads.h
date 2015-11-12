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

#include <const.h>
#include <sora.h>
#include <soratime.h>

#define MAX_THREADS  8

struct thread_info {
	int threadID;
	bool fRunning;

	ULONGLONG ttLastUpdate;

	int onCycle;
	int offCycle;
	int interval;
	int algorithm;
	bool updated;
	CRITICAL_SECTION critSec;
};


int StartThreads(ULONGLONG * ttstart, 
                 ULONGLONG * ttend, 
		 TIMESTAMPINFO *tsinfo, 
                 int no_threads, PSORA_UTHREAD_PROC* User_Routines);
