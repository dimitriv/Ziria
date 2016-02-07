

#pragma once
#ifdef ZYNQ_RF
#include "params.h"
#include "numerics.h"
#include <iio.h>

int Fmcomms_RadioStartTx(BlinkParams *params);
int Fmcomms_RadioStartRx(BlinkParams *params);
int Fmcomms_Init(BlinkParams *params);
void Fmcomms_RadioStop(BlinkParams *params);
void readFmcomms(BlinkParams *params, complex16 *ptr, int size);
void writeFmcomms(BlinkParams *params, complex16 *ptr, int size);

#endif
