/*
 * lime_radio.cpp
 *
 *  Created on: May 19, 2016
 *      Author: rubuntun
 */



#ifdef LIME_RF
#include "lime_radio.h"
#include <string>
#include <iostream>
#define TX_BUFF_SIZE 0x400
#define RX_BUFF_SIZE 0x400

static unsigned int timeAdv;
static complex16 *src_ptr;
static complex16 *dst_ptr;

int  LimeRF_RadioStart(BlinkParams *params)
{
	if (!params->radioParams.host)
		params->radioParams.iris = SoapySDR::Device::make();
	else
	{
		std::string arg_ip(params->radioParams.host);
		std::string arg = "driver=remote,remote=" + arg_ip;
		params->radioParams.iris = SoapySDR::Device::make(arg);
	}
	SoapySDR::Device *iris;
	if (params->radioParams.iris)
		iris = params->radioParams.iris;
	else
		exit(1);

	iris->setMasterClockRate((double)params->radioParams.clockRate);


	return 0;
}

void LimeRF_RadioStop(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;
	SoapySDR::Device::unmake(iris);

	if (src_ptr) free(src_ptr);
	if (dst_ptr) free(dst_ptr);

	if (params->TXBuffer != NULL)
	{
		free(params->TXBuffer);
	}

	if (params->pRxBuf != NULL)
	{
		free(params->pRxBuf);
	}

}

int  LimeRF_ConfigureTX(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;

	iris->setFrequency(SOAPY_SDR_TX, 0, "RF", (double) params->radioParams.CentralFrequency);
	iris->setSampleRate(SOAPY_SDR_TX, 0, (double) params->radioParams.SampleRate);
	iris->setBandwidth(SOAPY_SDR_TX, 0, (double) params->radioParams.Bandwidth);
    iris->setGain(SOAPY_SDR_TX, 0, "PAD", params->radioParams.TXgain); 
    iris->setDCOffsetMode(SOAPY_SDR_TX, 0, true);

	const SoapySDR::Kwargs &args = SoapySDR::Kwargs();
	std::vector<size_t> ch = {0};
	params->radioParams.txStream = iris->setupStream(SOAPY_SDR_TX, SOAPY_SDR_CS16, ch, args);

	int ret = iris->activateStream(params->radioParams.txStream);
	if (ret < 0)
	{
		printf("Unable to activate stream!\n");
		exit(1);
	}
	timeAdv = 1e8;
	src_ptr = (complex16*)malloc(sizeof(complex16) * TX_BUFF_SIZE);

	// TX buffer
	params->TXBuffer = malloc(params->radioParams.TXBufferSize);
	if (params->TXBuffer == NULL) {
		perror("malloc");
		exit(1);
	}


	return ret;
}

int  LimeRF_ConfigureRX(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;
	iris->setFrequency(SOAPY_SDR_RX, 0, "RF", (double) params->radioParams.CentralFrequency);
	iris->setSampleRate(SOAPY_SDR_RX, 0, (double) params->radioParams.SampleRate);
	iris->setBandwidth(SOAPY_SDR_RX, 0, (double) params->radioParams.Bandwidth);
	// set manual for now
    iris->setGain(SOAPY_SDR_RX, 0, "LNA", params->radioParams.RXgain);
    iris->setGain(SOAPY_SDR_RX, 0, "TIA", 5.000000);
    iris->setGain(SOAPY_SDR_RX, 0, "PGA", params->radioParams.RXpa);
    iris->setDCOffsetMode(SOAPY_SDR_RX, 0, true);

    const SoapySDR::Kwargs &args = SoapySDR::Kwargs();
	std::vector<size_t> ch = {0};
	params->radioParams.rxStream = iris->setupStream(SOAPY_SDR_RX, SOAPY_SDR_CS16, ch, args);

 	int ret = iris->activateStream(params->radioParams.rxStream);
	if (ret < 0)
	{
		printf("Unable to activate stream!\n");
		exit(1);
	}
	dst_ptr = (complex16*)malloc(sizeof(complex16) * RX_BUFF_SIZE);

	// RX buffer
	params->pRxBuf = malloc(RX_BUFF_SIZE);
	if (params->pRxBuf == NULL) {
		perror("malloc");
		exit(1);
	}


	return ret;
}


void readLimeRF(BlinkParams *params, complex16 *ptr, int size)
{
    int flags = 0;
	int ret;
	long long time = 0;
	SoapySDR::Device *iris = params->radioParams.iris;

	int samps = size;
	complex16 * readPtr = ptr;
	while (samps > 0)
	{
		flags = 0;
		ret = iris->readStream(params->radioParams.rxStream, (void **)&readPtr, (size_t)samps, flags, time, 1000000);
		if (ret < 0)
		{
			printf("Unable to read stream!\n");
			break;
		}
		else
		{
			samps -= ret;
			readPtr += ret;
		}

	}

}

// uses time flag to send samples at a future time
void writeLimeRF(BlinkParams *params, complex16 *ptr, unsigned long size)
{
	SoapySDR::Device *iris = params->radioParams.iris;
    int flags = 0;
	flags |= SOAPY_SDR_HAS_TIME;
	int ret = 0;
	static unsigned int set_time = 0;
	static long long time = 0;

	// set time, 1MTU (worth of samples) * sample_time (400ns at 2.5 MHz) = ,
	if (!set_time)
	{
		time = iris->getHardwareTime("") + timeAdv;
		set_time = 1;
	}

	if (time < iris->getHardwareTime(""))
		time = iris->getHardwareTime("") + timeAdv;



	int samps = size;
	complex16 * readPtr = ptr;
	while (samps > 0)
	{
		flags = 0;
		flags |= SOAPY_SDR_HAS_TIME;
		ret = iris->writeStream(params->radioParams.txStream, (void **)&readPtr, (size_t)samps, flags, time, 1000000);
		if (ret < 0)
		{
			printf("Unable to write stream!\n");
			break;
		}
		else
		{
			samps -= ret;
			readPtr += ret;
		}

	}
}

void writeBurstLimeRF(BlinkParams *params, void *ptr, unsigned long size)
{
	SoapySDR::Device *iris = params->radioParams.iris;
    int flags = SOAPY_SDR_END_BURST | SOAPY_SDR_HAS_TIME;
	int ret = 0;
	long long time = 0;
	time = iris->getHardwareTime("") + 1e8;

	int samps = size;
	void * readPtr = ptr;
	while (samps > 0)
	{
		flags |= SOAPY_SDR_HAS_TIME;
		ret = iris->writeStream(params->radioParams.txStream, (void **)&readPtr, (size_t)samps, flags, time, 1000000);
		if (ret < 0)
		{
			printf("Unable to write stream!\n");
			break;
		}
		else
		{
			samps -= ret;
			readPtr += ret;
		}
	}
	size_t chanMask = 0;
	time = iris->getHardwareTime("") + 1e8 + size / params->radioParams.SampleRate;
	int sta = iris->readStreamStatus(params->radioParams.txStream, chanMask, flags, time, 0);
	printf("\nStream Status %d\n\n", sta);
}


#endif
