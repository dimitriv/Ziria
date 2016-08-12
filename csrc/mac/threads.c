/*********************************************************************************

MIT License

Copyright (c) 2016 Microsoft

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*********************************************************************************/


#include <stdlib.h>
#include <time.h>
#include <sys/sysinfo.h>
#include "threads.h"

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

#ifndef __GNUC__
HANDLE StartWinUThread(PWIN_UTHREAD_PROC function, PVOID context, int core, int highpri)
{
	DWORD dwThreadId;
	HANDLE hThread = CreateThread(NULL, 0, function, context, 0, &dwThreadId);

	if (core >= 0 && core <= 3)
	{
		//assigns the thread to a processor/core
		DWORD_PTR m_mask = 1 << core;
		if (SetThreadAffinityMask(hThread, m_mask) == 0)
		{
			printf("Failed to set affinity to core %d: 0x%x\n", core, GetLastError());
			return 0;
		}

		if (highpri)
		{
			// The one below requires Admin priviledges and it is dangerous for threads that don't yield
			if (SetPriorityClass(GetCurrentProcess(), REALTIME_PRIORITY_CLASS) == 0)
				//if (SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS) == 0)
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
		}
		int priority = GetThreadPriority(hThread);
		printf("Current thead priority class is 0x%x\n", priority);
	}
	else
	{
		printf("Cannot place on core %d: %x\n", core, GetLastError());
		return 0;
	}

	printf("Thread %ld started.\n", dwThreadId);

	return hThread;
}

#else


pthread_t StartPosixThread(void *(*function) (void *), void * arg, int core, int highpri)
{
	cpu_set_t cpuset;
	pthread_attr_t thAttr;
	pthread_t mac_thread;
	struct sched_param param;
	int policy = 0;

	int s = pthread_attr_init(&thAttr);

	if (highpri)
	{
		pthread_attr_getschedpolicy(&thAttr, &policy);
		int max_prio_for_policy = sched_get_priority_max(policy);
		param.sched_priority = max_prio_for_policy;
		pthread_attr_setschedparam(&thAttr, &param);
	}

	int ret = pthread_create(&mac_thread, &thAttr, function, arg);
	if (ret)
	{
		fprintf(stderr, "Failed to create thread: %s\n", strerror(-ret));
	}

	int priority = pthread_getschedparam(mac_thread, &policy, &param);
	printf("Current thead priority class is 0x%x\n", param.sched_priority);

	pthread_attr_destroy(&thAttr);

	//int num_cores = sysconf(_SC_NPROCESSORS_ONLN);
	if (core >= 0 && core <= 4)
	{
		//assigns the thread to a processor/core

		CPU_ZERO(&cpuset);
		CPU_SET(core, &cpuset);

		if (pthread_setaffinity_np(mac_thread, sizeof(cpu_set_t), &cpuset) == 0)
		{
			printf("Failed to set affinity to core %d\n", core);
			//return 0;
		}
	}
	else
	{
		printf("Cannot place on core %d\n", core);
		//return 0;
	}

	thread_info *ti = (thread_info *)arg;
	ti->fRunning = true;
	return mac_thread;
}
#endif
