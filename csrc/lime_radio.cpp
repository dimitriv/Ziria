/*
 * lime_radio.cpp
 *
 *  Created on: May 19, 2016
 *      Author: rubuntun
 */



#ifdef LIME_RF
#include "lime_radio.h"


int  LimeRF_RadioStart(BlinkParams *params)
{
	params->radioParams.iris = SoapySDR::Device::make();
	SoapySDR::Device *iris = params->radioParams.iris;

	iris->setMasterClockRate(80e6);

	iris->setFrequency(SOAPY_SDR_TX, 0, "RF", (double) params->radioParams.CentralFrequency);
	iris->setFrequency(SOAPY_SDR_RX, 0, "RF", (double) params->radioParams.CentralFrequency);

	iris->setSampleRate(SOAPY_SDR_TX, 0, (double) params->radioParams.SampleRate);
	iris->setSampleRate(SOAPY_SDR_RX, 0, (double) params->radioParams.SampleRate);

	iris->setBandwidth(SOAPY_SDR_TX, 0, (double) params->radioParams.Bandwidth);
	iris->setBandwidth(SOAPY_SDR_RX, 0, (double) params->radioParams.Bandwidth);

	iris->setGain(SOAPY_SDR_TX, 0, "PAD", (double) params->radioParams.TXgain); // Set Tx Gain
	iris->setGain(SOAPY_SDR_RX, 0, "LNA", (double) params->radioParams.RXgain); // Set Rx Gain

    iris->setDCOffsetMode(SOAPY_SDR_TX, 0, true);
    iris->setDCOffsetMode(SOAPY_SDR_RX, 0, true);


	return 0;
}

void LimeRF_RadioStop(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;
    int flags = 0;
	flags |= SOAPY_SDR_END_BURST;
	iris->deactivateStream(params->radioParams.txStream);
	iris->deactivateStream(params->radioParams.rxStream);
	iris->closeStream(params->radioParams.txStream);
	iris->closeStream(params->radioParams.rxStream);
	delete iris;
}

int  LimeRF_ConfigureTX(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;
	const SoapySDR::Kwargs &args = SoapySDR::Kwargs();
	std::vector<size_t> ch = {0};
	params->radioParams.txStream = iris->setupStream(SOAPY_SDR_TX, SOAPY_SDR_CS16, ch, args);

	int ret = iris->activateStream(params->radioParams.txStream);
	if (ret < 0)
		printf("Unable to activate stream!\n");

	return ret;
}

int  LimeRF_ConfigureRX(BlinkParams *params)
{

	SoapySDR::Device *iris = params->radioParams.iris;
	const SoapySDR::Kwargs &args = SoapySDR::Kwargs();
	std::vector<size_t> ch = {0};
	params->radioParams.rxStream = iris->setupStream(SOAPY_SDR_RX, SOAPY_SDR_CS16, ch, args);

 	int ret = iris->activateStream(params->radioParams.rxStream);
	if (ret < 0)
		printf("Unable to activate stream!\n");

	return ret;
}

void readLimeRF(BlinkParams *params, complex16 *ptr, int size)
{
    int flags = 0;
	flags |= SOAPY_SDR_END_BURST;
	int ret;
	long long time = 0; //iris->getHardwareTime("");
	SoapySDR::Device *iris = params->radioParams.iris;
	//ret = iris->activateStream(params->radioParams.rxStream, flags, 0, (size_t)size);
	//if (ret < 0)
	//	printf("Unable to activate stream!\n");

	int samps = size;
	complex16 * readPtr = ptr;
	while (samps > 0)
	{
		flags = 0;
		flags |= SOAPY_SDR_END_BURST;
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

void writeLimeRF(BlinkParams *params, complex16 *ptr, unsigned long size)
{
	SoapySDR::Device *iris = params->radioParams.iris;
    int flags = 0;
	flags |= SOAPY_SDR_END_BURST;
	int ret = 0;

	int samps = size;
	complex16 * readPtr = ptr;
	while (samps > 0)
	{
		flags = 0;
		flags |= SOAPY_SDR_END_BURST;
		ret = iris->writeStream(params->radioParams.txStream, (void **)&readPtr, (size_t)samps, flags, iris->getHardwareTime(""), 1000000);
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

#endif
