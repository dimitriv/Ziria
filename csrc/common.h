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

#include "types.h"
#include "wpl_alloc.h"
#include "utils.h"
#include "buf.h"

#ifdef __GNUC__
#include "sora_ext_lib.cpp"
#endif

