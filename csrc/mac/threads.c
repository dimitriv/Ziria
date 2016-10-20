/*
Coyright (c) Rice University, RECG Lab
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

#include <stdlib.h>
#include <time.h>
#include <sys/sysinfo.h>
#include "threads.h"




pthread_t StartPosixThread(void *(*function) (void *), void * arg, int core, int highpri)
{
	cpu_set_t cpuset;
	pthread_attr_t thAttr;
	pthread_t mac_thread;
	struct sched_param param;
	int policy(SCHED_RR);

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
	if (highpri && core >= 0 && core <= 4)
	{
		//assigns the thread to a processor/core

		CPU_ZERO(&cpuset);
		CPU_SET(core, &cpuset);

		if (pthread_setaffinity_np(mac_thread, sizeof(cpu_set_t), &cpuset) != 0)
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

