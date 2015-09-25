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

#include "UnitTests.h"





bool __cdecl test_int32()
{
	ts_context *queues;
	size_t sizes[1] = { sizeof(int32) };
	int queue_sizes[1] = { QUEUE_LEN };
	bool error = 0;

	queues = s_ts_init_var(1, sizes, queue_sizes);

	int32 val1 = 0;
	int32 valA[3 * QUEUE_LEN];
	char *pVal1 = (char *)&val1;
	char *pValA = (char *)valA;
	int indA = 0;
	
	if (!s_ts_isEmpty(queues, 0))
	{
		printf("Error in test_int32: s_ts_isEmpty\n");
		error = 1;
	}
	
	for (int i = 0; i < QUEUE_LEN; i++)
	{
		s_ts_put(queues, 0, pVal1);
		val1++;
	}

	if (!s_ts_isFull(queues, 0))
	{
		printf("Error in test_int32: s_ts_isFull\n");
		error = 1;
	}



	for (int i = 0; i < QUEUE_LEN / 2; i++)
	{
		s_ts_get(queues, 0, (char *)(valA + indA));
		indA++;
	}

	for (int i = 0; i < QUEUE_LEN / 2; i++)
	{
		s_ts_put(queues, 0, pVal1);
		val1++;
	}

	if (!s_ts_isFull(queues, 0))
	{
		printf("Error in test_int32: s_ts_isFull\n");
		error = 1;
	}



	for (int i = 0; i < QUEUE_LEN; i++)
	{
		s_ts_get(queues, 0, (char *)(valA + indA));
		indA++;
	}

	if (!s_ts_isEmpty(queues, 0))
	{
		printf("Error in test_int32: s_ts_isEmpty\n");
		error = 1;
	}

	for (int i = 0; i < indA; i++)
	{
		if (valA[i] != i)
		{
			printf("Error in test_int32: data!\n");
			error = 1;
		}
	}

	return error;
}





bool __cdecl test_int32_many()
{
	ts_context *queues;
	size_t sizes[1] = { sizeof(int32) };
	int queue_sizes[1] = { QUEUE_LEN };
	bool error = 0;

	queues = s_ts_init_var(1, sizes, queue_sizes);

	int32 val1[4] = { 0, 1, 2, 3 };
	int32 valA[3 * QUEUE_LEN];
	char *pVal1 = (char *)val1;
	char *pValA = (char *)valA;
	int indA = 0;

	if (!s_ts_isEmpty(queues, 0))
	{
		printf("Error in test_int32_many: s_ts_isEmpty\n");
		error = 1;
	}

	for (int i = 0; i < QUEUE_LEN/4; i++)
	{
		s_ts_putMany(queues, 0, 4, pVal1);
		for (int j = 0; j < 4; j++)
		{
			val1[j] = val1[j] + 4;
		}
	}

	if (!s_ts_isFull(queues, 0))
	{
		printf("Error in test_int32_many: s_ts_isFull\n");
		error = 1;
	}



	s_ts_getMany(queues, 0, QUEUE_LEN / 2, (char *)valA);

	for (int i = 0; i < QUEUE_LEN/8; i++)
	{
		s_ts_putMany(queues, 0, 4, pVal1);
		for (int j = 0; j < 4; j++)
		{
			val1[j] = val1[j] + 4;
		}
	}


	if (!s_ts_isFull(queues, 0))
	{
		printf("Error in test_int32_many: s_ts_isFull\n");
		error = 1;
	}



	s_ts_getMany(queues, 0, QUEUE_LEN, (char *)(valA + QUEUE_LEN / 2));

	if (!s_ts_isEmpty(queues, 0))
	{
		printf("Error in test_int32_many: s_ts_isEmpty\n");
		error = 1;
	}

	for (int i = 0; i < QUEUE_LEN + QUEUE_LEN/2; i++)
	{
		if (valA[i] != i)
		{
			printf("Error in test_int32_many: data!\n");
			error = 1;
		}
	}

	return error;
}
