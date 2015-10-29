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
#pragma warning(disable:4530)

#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <sora.h>
#include <brick.h>
#include <dspcomm.h>
#include <soratime.h>

#include "thread_if.h"
#include "thread_func.h"



TIMESTAMPINFO tsinfo;



extern int BrickTest();



int __cdecl main(int argc, const char *argv[])
{

	tsinfo.use_rdtsc = 1;
	InitializeTimestampInfo ( &tsinfo, false );

	BrickTest();

	//free(DataBuffer);
	//free(SymbolBuffer);
 	return 0;

}
