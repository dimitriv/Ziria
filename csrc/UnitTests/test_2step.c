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





bool __cdecl test_2step_int32(int queue_len)
{
	ts_context *queues;
	size_t sizes[1] = { sizeof(int32) };
	int queue_sizes[1];
	bool error = 0;

	queue_sizes[0] = queue_len;

	queues = ts_init(1, sizes, queue_sizes);

	int32 val1 = 0;
	int32 valA[3 * QUEUE_LEN];
	char *pVal1 = (char *)&val1;
	char *pValA = (char *)valA;
	int indA = 0;


	// NOOP
	if (!ts_isEmpty(queues))
	{
		printf("Error in test_int32: s_ts_isEmpty\n");
		error = 1;
	}
	for (int i = 0; i < queue_len; i++)
	{
		int32 *buf = (int32*)ts_reserve(&queues[0], 1);
		buf[0] = val1;
		ts_push(&queues[0], 1);
	}
	ts_clear(queues);




	if (!ts_isEmpty(queues))
	{
		printf("Error in test_int32: s_ts_isEmpty\n");
		error = 1;
	}

	for (int i = 0; i < queue_len; i++)
	{
		int32 *buf = (int32*)ts_reserve(&queues[0], 1);
		buf[0] = val1;
		ts_push(&queues[0], 1);
		val1++;
	}

	if (!ts_isFull(queues))
	{
		printf("Error in test_int32: s_ts_isFull\n");
		error = 1;
	}


	// NOOP
	ts_acquire(&queues[0], 1);
	ts_release(&queues[0], 1);
	ts_acquire(&queues[0], 1);
	ts_release(&queues[0], 1);
	ts_rollback(queues, 2);



	for (int i = 0; i < queue_len / 2; i++)
	{
		// NOOP
		ts_acquire(&queues[0], 1);
		ts_release(&queues[0], 1);
		ts_rollback(queues, 1);

		int32 *buf = (int32*)ts_acquire(&queues[0], 1);
		valA[indA] = buf[0];
		ts_release(&queues[0], 1);
		indA++;
	}

	for (int i = 0; i < queue_len / 2; i++)
	{
		int32 *buf = (int32*)ts_reserve(&queues[0], 1);
		buf[0] = val1;
		ts_push(&queues[0], 1);
		val1++;
	}

	if (!ts_isFull(queues))
	{
		printf("Error in test_int32: s_ts_isFull\n");
		error = 1;
	}



	for (int i = 0; i < queue_len; i++)
	{
		// NOOP
		ts_acquire(&queues[0], 1);
		ts_release(&queues[0], 1);
		ts_rollback(queues, 1);

		int32 *buf = (int32*)ts_acquire(&queues[0], 1);
		valA[indA] = buf[0];
		ts_release(&queues[0], 1);
		indA++;
	}

	if (!ts_isEmpty(queues))
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




