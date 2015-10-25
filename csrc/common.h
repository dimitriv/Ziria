#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <xmmintrin.h>
#include <emmintrin.h>


#include "types.h"
#include "wpl_alloc.h"
#include "utils.h"
#include "buf.h"

#include "sora_thread_queues.h"
#include "single_thread_queues.h"
#include "mitigator_queues.h"

#if defined(__GNUC__) || defined(__linux__)
#include "sora_ext_lib.c"

typedef void(*PWIN_UTHREAD_PROC)(void *);
#endif
