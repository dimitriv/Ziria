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

#pragma once
#include <stdlib.h>
#include <time.h>

#ifdef __GNUC__
#include <pthread.h>
#endif



#ifdef __GNUC__
struct thread_info {
	pthread_t mThr;
	pthread_mutex_t lock;
	bool fRunning;
};

pthread_t StartPosixThread( void *(*function) (void *), void * arg, int core, int highpri);
#endif
