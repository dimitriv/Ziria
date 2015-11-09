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
#include <math.h>
#include <time.h>
#include <string.h>

#include <const.h>


#include "sora_thread_queues.h"

#include "wpl_alloc.h"
#include "buf.h"
#include "utils.h"



#ifdef __GNUC__
#ifndef __cdecl
#define __cdecl
#endif
#endif


// This is otherwise defined in test.cpp
bool atomix = 1;


#define QUEUE_LEN	16

int __cdecl main(int argc, char **argv) 
{
	ts_context *queues;
	size_t sizes[1] = { sizeof(int16) };
	int queue_sizes[1] = { QUEUE_LEN };

	queues = s_ts_init_var(1, sizes, queue_sizes);

	int32 val1 = 0, val2 = 0;
	int32 valA[3 * QUEUE_LEN];
	char *pVal1 = (char *)&val1;
	char *pVal2 = (char *)&val2;
	char *pValA = (char *)valA;
	int indA = 0;
	for (int i = 0; i < QUEUE_LEN; i++)
	{
		s_ts_put(queues, 0, pVal1);
		val1++;
	}

	for (int i = 0; i < QUEUE_LEN/2; i++)
	{
		s_ts_get(queues, 0, (char *) (valA + indA));
		indA++;
	}

	for (int i = 0; i < QUEUE_LEN/2; i++)
	{
		s_ts_put(queues, 0, pVal1);
		val1++;
	}

	for (int i = 0; i < QUEUE_LEN; i++)
	{
		s_ts_get(queues, 0, (char *)(valA + indA));
		indA++;
	}

	for (int i = 0; i < indA; i++)
	{
		if (valA[i] != i)
		{
			printf("Error!\n");
		}
	}


}
