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

#include "UnitTests.h"


#ifdef __GNUC__
#ifndef __cdecl
#define __cdecl
#endif
#endif


// This is otherwise defined in test.c
bool atomix = 1;


#define QUEUE_LEN	16

int __cdecl main(int argc, char **argv) 
{
	bool error = 0;


	error = test_int32() | error;
	error = test_int32_many() | error;
	error = test_2step_int32() | error;
	error = test_2step_int32_many() | error;

	if (error)
	{
		printf("TESTS FAILED!!!\n");
		return 1;
	}

	return 0;
}
