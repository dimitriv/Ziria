

#pragma once
#ifdef ADI_RF
#include "params.h"
#include "numerics.h"
#ifdef __ARM_NEON__
#include "neon/types.h"
#endif
#include <iio.h>
#include <errno.h>

int Fmcomms_RadioStartTx(BlinkParams *params);
int Fmcomms_RadioStartRx(BlinkParams *params);
int Fmcomms_Init(BlinkParams *params);
void Fmcomms_RadioStop(BlinkParams *params);
void readFmcomms(BlinkParams *params, complex16 *ptr, int size);
void writeFmcomms(BlinkParams *params, complex16 *ptr, int size);
//void iio_to_ziria_memcpy(complex16* zbuf, void * ibuf, int iiosize);
#endif
