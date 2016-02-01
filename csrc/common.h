#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef __SSE__
#include <xmmintrin.h>
#endif

#ifdef __SSE2__
#include <emmintrin.h>
#endif

#ifdef __ARM_NEON__
#include "neon/sse_to_neon.h"
#endif


#include "types.h"
#include "wpl_alloc.h"
#include "utils.h"
#include "buf.h"

#ifdef __GNUC__
#include "sora_ext_lib.cpp"
#ifdef __ARM_NEON__
#include "sora_ext_viterbi.cpp"
#endif
#endif

