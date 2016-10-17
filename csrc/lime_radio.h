/*
 * lime_radio.h
 *
 *  Created on: May 19, 2016
 *      Author: rubuntun
 */
#pragma once
#ifdef LIME_RF
#include "params.h"
#include "numerics.h"

#ifdef PL_CS
void startCarrierSense(BlinkParams *params);
#endif

int  LimeRF_RadioStart(BlinkParams *params);
int  LimeRF_ConfigureTX(BlinkParams *params);
int  LimeRF_ConfigureRX(BlinkParams *params);

void LimeRF_RadioStop(BlinkParams *params);
void readLimeRF(BlinkParams *params, complex16 *ptr, int size);
void writeLimeRF(BlinkParams *params, complex16 *ptr, unsigned long size);

#endif


