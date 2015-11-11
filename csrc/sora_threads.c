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
#include <assert.h>
#include <limits.h>
#include <windows.h>
//#include <conio.h>

#include <thread_func.h>

#include "sora_threads.h"
#include "sora_thread_queues.h"


HANDLE threads[MAX_THREADS];
thread_info t_info[MAX_THREADS];


// Global variable set by init if we are running atomix version
extern bool atomix;


// Ideally, we might want to rewrite the threading support to use user mode threads in Win7+
// http://msdn.microsoft.com/en-us/library/windows/desktop/dd627187(v=vs.85).aspx


extern int stop_program;
// Handler function will be called on separate thread!
static BOOL WINAPI console_ctrl_handler(DWORD dwCtrlType)
{
	printf("Breaking...\n");
	fflush(stdout);
	stop_program = 1;
	return TRUE;
}




// Core info example from: https://msdn.microsoft.com/en-us/library/ms683194(VS.85).aspx

typedef BOOL(WINAPI *LPFN_GLPI)(
	PSYSTEM_LOGICAL_PROCESSOR_INFORMATION,
	PDWORD);

// Return up to noCode indices of logical cores that are located on different (stored into coreMask)
int getCoreInfo(int noCore, int * coreMask)
{
	LPFN_GLPI glpi;
	BOOL done = FALSE;
	PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer = NULL;
	PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr = NULL;
	DWORD returnLength = 0;
	DWORD logicalProcessorCount = 0;
	DWORD numaNodeCount = 0;
	DWORD processorCoreCount = 0;
	DWORD processorL1CacheCount = 0;
	DWORD processorL2CacheCount = 0;
	DWORD processorL3CacheCount = 0;
	DWORD processorPackageCount = 0;
	DWORD byteOffset = 0;
	PCACHE_DESCRIPTOR Cache;
	int coreMaskInd = 0;


	glpi = (LPFN_GLPI)GetProcAddress(
		GetModuleHandle(TEXT("kernel32")),
		"GetLogicalProcessorInformation");
	if (NULL == glpi)
	{
		printf("\nGetLogicalProcessorInformation is not supported.\n");
		return -1;
	}

	while (!done)
	{
		DWORD rc = glpi(buffer, &returnLength);

		if (FALSE == rc)
		{
			if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
			{
				if (buffer)
					free(buffer);

				buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION)malloc(
					returnLength);

				if (NULL == buffer)
				{
					printf("\nError: Allocation failure\n");
					return -1;
				}
			}
			else
			{
				printf("\nError %d\n", GetLastError());
				return -1;
			}
		}
		else
		{
			done = TRUE;
		}
	}

	ptr = buffer;

	while (byteOffset + sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= returnLength)
	{
		switch (ptr->Relationship)
		{
		case RelationNumaNode:
			// Non-NUMA systems report a single record of this type.
			numaNodeCount++;
			break;

		case RelationProcessorCore:
			processorCoreCount++;

			// A hyperthreaded core supplies more than one logical processor.
			// ptr->ProcessorMask is a bitmap with 1s at logical cores that belong to a specific physical core
			{
				DWORD LSHIFT = sizeof(ULONG_PTR)* 8 - 1;
				DWORD bitSetCount = 0;
				ULONG_PTR bitTest = (ULONG_PTR)1 << LSHIFT;
				DWORD i;

				coreMask[coreMaskInd] = -1;

				for (i = 0; i <= LSHIFT; ++i)
				{
					if ((ptr->ProcessorMask & bitTest) && coreMask[coreMaskInd] < 0)
					{
						coreMask[coreMaskInd] = (LSHIFT - i);
					}
					bitSetCount += ((ptr->ProcessorMask & bitTest) ? 1 : 0);
					bitTest /= 2;
				}


				logicalProcessorCount += bitSetCount;
				coreMaskInd++;
			}
			break;

		case RelationCache:
			// Cache data is in ptr->Cache, one CACHE_DESCRIPTOR structure for each cache. 
			Cache = &ptr->Cache;
			if (Cache->Level == 1)
			{
				processorL1CacheCount++;
			}
			else if (Cache->Level == 2)
			{
				processorL2CacheCount++;
			}
			else if (Cache->Level == 3)
			{
				processorL3CacheCount++;
			}
			break;

		case RelationProcessorPackage:
			// Logical processors share a physical package.
			processorPackageCount++;
			break;

		default:
			printf("\nError: Unsupported LOGICAL_PROCESSOR_RELATIONSHIP value.\n");
			break;
		}
		byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
		ptr++;
	}

	printf("\nGetLogicalProcessorInformation results:\n");
	printf("Number of NUMA nodes: %d\n", numaNodeCount);
	printf("Number of physical processor packages: %d\n", processorPackageCount);
	printf("Number of processor cores: %d\n", processorCoreCount);
	printf("Number of logical processors: %d\n", logicalProcessorCount);
	printf("Number of processor L1/L2/L3 caches: %d/%d/%d\n",
		processorL1CacheCount,
		processorL2CacheCount,
		processorL3CacheCount);

	free(buffer);

	return coreMaskInd;
}




// ***************
// We use standard Windows thread with core affinity and real-time priority
// There doesn't seem to be much difference between the two.
// Sora threads were using fibers to move the running thread from a core to another core
// occasionally, to avoid Windows killing it for overusing a CPU
// This does not seem to happen in Win8 anymore. 
// A real-time thread gets preemted only by other real-time threads (e.g. svchost and dvm)
// and there doesn't seem to be any other negative implication of sticking forever to one core. 
// Below are the measurements from MSRC-Sora12 (Dell T3500):
// Sora single thread (no calls to SoraThreadYield): 
//   preempted: (53.4us, 8us, 8us, 139us) each ~30ms by processes: svchost, dwm
// Sora with SoraThreadYield (moved from core to core to avoid punishments): 
//   preempted: (46us, 30us, 10us) each ~30ms by processes: svchost, dwm
// Win single thread: 
//   preemted: (9.3us, 8.9us, 18.6us) each ~90ms by processes : svchost, dwm
// ***************
//
bool StartWinUThread(int no_threads, PWIN_UTHREAD_PROC *function, 
	ULONGLONG * ttstart, ULONGLONG * ttend, TIMESTAMPINFO *tsinfo)
{
	bool init_ok = TRUE;
	DWORD dwThreadId;
	HANDLE *handles;

	// Get a list of logical cores that are stored on different physical cores
	int cores[16];
	int no_cores = getCoreInfo(16, cores);
	if (no_threads > no_cores)
	{
		printf("There are more threads (%d) than cores (%d)!\n", no_threads, no_cores);
	}


	handles = (HANDLE *)malloc(no_threads * sizeof(HANDLE *));

	for (int thr = 0; thr < no_threads; thr++)
	{
		t_info[thr].threadID = thr;
		t_info[thr].fRunning = true;

		HANDLE hThread = CreateThread(NULL, 0, function[thr], (void *)(t_info + thr), 0, &dwThreadId);

		if (hThread != NULL)
		{
			int core = cores[thr];

			//assigns the thread to a processor/core
			DWORD_PTR m_mask = 1 << core;
			if (SetThreadAffinityMask(hThread, m_mask) == 0)
			{
				printf("Failed to set affinity to core %d: 0x%x\n", core, GetLastError());
				return 0;
			}

			// The one below requires Admin priviledges and it is dangerous for threads that don't yield
			if (SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS) == 0)
			// if (SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS) == 0)
			{
				printf("Not able to set priority class: 0x%x\nTry running as Administrator\n", GetLastError());
			}

			DWORD dwPriClass = GetPriorityClass(GetCurrentProcess());
			printf("Current priority class is 0x%x\n", dwPriClass);

			
			if (SetThreadPriority(hThread, THREAD_PRIORITY_TIME_CRITICAL) == 0)
			{
				printf("Failed to set priority: 0x%x\n", GetLastError());
				return 0;
			}

			int priority = GetThreadPriority(hThread);
			printf("Current thead priority class is 0x%x\n", priority);

			printf("Thread %d, id=%ld started on core %d.\n", thr, dwThreadId, core);
		}
		else
		{
			printf("Error starting thread %d!\n", thr);
		}
		handles[thr] = hThread;
		init_ok = init_ok && (hThread != NULL);
	}

	LARGE_INTEGER cstart;
	LARGE_INTEGER cend;

	QueryPerformanceCounter(&cstart);
	// Start measuring time
	*ttstart = SoraGetCPUTimestamp(tsinfo);

	if (init_ok)
	{

		if (atomix)
		{
			// In atomix, all threads are in sync.
			// Only one thread will finish (the one that reads EOF)
			// So if we see on thread finished, we stop
			bool finished = false;
			while (!finished)
			{
				// Threads will exit when stop_program = true
				// So no need to close them explicitly

				finished = false;
				for (int i = 0; i < no_threads; i++)
				{
					finished = finished || (!t_info[i].fRunning);
				}
				Sleep(1); // NB: not Sleep(100) (!!)
				// Flush stdout to get any printout that could be out there
				fflush(stdout);
			}
		}
		else
		{
			// In the conventional execution model, we need to wait for all threads
			bool not_finished = true;
			while (not_finished)
			{
				// Threads will exit when stop_program = true
				// So no need to close them explicitly

				not_finished = false;
				for (int i = 0; i < no_threads; i++)
				{
					not_finished = not_finished || t_info[i].fRunning;
				}
				Sleep(1); // NB: not Sleep(100) (!!)
				// Flush stdout to get any printout that could be out there
				fflush(stdout);
			}
		}
	}
	else
	{
		printf("Initialization error: couldn't start a thread!\n");
	}

	QueryPerformanceCounter(&cend);

	// Stop measuring time
	*ttend = SoraGetCPUTimestamp(tsinfo);

	printf("QueryPerformanceCounter = %lld\n", cend.QuadPart - cstart.QuadPart);


	for (int thr = 0; thr < no_threads; thr++)
	{
		CloseHandle(handles[thr]);
	}

	free(handles);

	return 0;
}


/*
// no_threads - number of threads to start
// User_Routines[no_threads] - pointers to functions to be started
// sizes[no_threads-1] - size of data structure to be held by each buffer (in bytes)
int StartThreads(ULONGLONG * ttstart, 
                 ULONGLONG * ttend, 
		 TIMESTAMPINFO *tsinfo,
                 int no_threads, PSORA_UTHREAD_PROC* User_Routines)
{
	stop_program = 0;
	bool init_error = false;

	SetConsoleCtrlHandler(console_ctrl_handler, TRUE);

	for (int i=0; i<no_threads; i++)
	{
		if (User_Routines[i] != NULL)
		{
			t_info[i].threadID = i;
			t_info[i].fRunning = true;
			threads[i] = AllocStartThread ( User_Routines[i], (void *) (t_info+i) );
			if (threads[i] == NULL) init_error = true;
		}
		else
		{
			threads[i] = NULL;
		}
	}

	// Start measuring time
	*ttstart = SoraGetCPUTimestamp ( tsinfo );	
	
	if (!init_error)
	{

		if (atomix)
		{
			// In atomix, all threads are in sync.
			// Only one thread will finish (the one that reads EOF)
			// So if we see on thread finished, we stop
			bool finished = false;
			while (!finished)
			{
				// Threads will exit when stop_program = true
				// So no need to close them explicitly

				finished = false;
				for (int i = 0; i < no_threads; i++)
				{
					finished = finished || (!t_info[i].fRunning);
				}
				Sleep(1); // NB: not Sleep(100) (!!)
				// Flush stdout to get any printout that could be out there
				fflush(stdout);
			}
		}
		else
		{
			// In the conventional execution model, we need to wait for all threads
			bool not_finished = true;
			while (not_finished)
			{
				// Threads will exit when stop_program = true
				// So no need to close them explicitly

				not_finished = false;
				for (int i = 0; i < no_threads; i++)
				{
					not_finished = not_finished || t_info[i].fRunning;
				}
				Sleep(1); // NB: not Sleep(100) (!!)
				// Flush stdout to get any printout that could be out there
				fflush(stdout);
			}
		}
	}
	else
	{
		printf("Initialization error: couldn't start a thread!\n");
		exit(1);
	}


	// Stop measuring time
	*ttend = SoraGetCPUTimestamp ( tsinfo );

	return no_threads;
}
*/
