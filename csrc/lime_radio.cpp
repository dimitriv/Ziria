/*
 * lime_radio.cpp
 *
 *  Created on: May 19, 2016
 *      Author: rubuntun
 */



#ifdef LIME_RF
#include "lime_radio.h"


#define TX_BUFF_SIZE 0x400
#define RX_BUFF_SIZE 0x400

static unsigned int timeAdv;
//static complex16 *src_ptr;
//static complex16 *dst_ptr;
int  LimeRF_RadioStart(BlinkParams *params)
{
	params->radioParams.iris = SoapySDR::Device::make();
	SoapySDR::Device *iris = params->radioParams.iris;

	iris->setMasterClockRate((double)params->radioParams.clockRate);


	return 0;
}

void LimeRF_RadioStop(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;
	//iris->deactivateStream(params->radioParams.txStream);
	//iris->deactivateStream(params->radioParams.rxStream);
	//iris->closeStream(params->radioParams.txStream);
	//iris->closeStream(params->radioParams.rxStream);

	SoapySDR::Device::unmake(iris);
	//if (src_ptr) free(src_ptr);
	//if (dst_ptr) free(dst_ptr);
}

int  LimeRF_ConfigureTX(BlinkParams *params)
{
	SoapySDR::Device *iris = params->radioParams.iris;

	iris->setFrequency(SOAPY_SDR_TX, 0, "RF", (double) params->radioParams.CentralFrequency);
	iris->setSampleRate(SOAPY_SDR_TX, 0, (double) params->radioParams.SampleRate);
	iris->setBandwidth(SOAPY_SDR_TX, 0, (double) params->radioParams.Bandwidth);
    iris->setGain(SOAPY_SDR_TX, 0, "PAD", -10); // Set Tx Gain
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
	timeAdv = 1e7;
	//src_ptr = (complex16*)malloc(sizeof(complex16) * TX_BUFF_SIZE);

	return ret;
}

int  LimeRF_ConfigureRX(BlinkParams *params)
{

	SoapySDR::Device *iris = params->radioParams.iris;
	iris->setFrequency(SOAPY_SDR_RX, 0, "RF", (double) params->radioParams.CentralFrequency);
	iris->setSampleRate(SOAPY_SDR_RX, 0, (double) params->radioParams.SampleRate);
	iris->setBandwidth(SOAPY_SDR_RX, 0, (double) params->radioParams.Bandwidth);
	// set manual for now
    iris->setGain(SOAPY_SDR_RX, 0, "LNA", 30.000000);
    iris->setGain(SOAPY_SDR_RX, 0, "TIA", 5.000000);
    iris->setGain(SOAPY_SDR_RX, 0, "PGA", -7.000000);
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

	//dst_ptr = (complex16*)malloc(sizeof(complex16) * RX_BUFF_SIZE);

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

/*
void readLimeRF(BlinkParams *params, complex16 *ptr, int size)
{
	static int iris_index = 0;
	int ziria_index = 0;
    size_t buf_step = sizeof(complex16);

    int flags = 0;
	int ret;
	long long time = 0;
	SoapySDR::Device *iris = params->radioParams.iris;

	if (iris_index == 0)
	{
		int samps = RX_BUFF_SIZE;
		complex16 * readPtr = dst_ptr;
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

    int iris_buf_left = RX_BUFF_SIZE - iris_index;

    if (iris_buf_left >= size)
    {
    	// just memcpy from existing buffer
    	memcpy(ptr, dst_ptr + iris_index, buf_step * size);

    	// update iris_buf_left and iris_index
    	iris_index = (iris_index + size) % RX_BUFF_SIZE;
    	return;
    }
    else
    {

    	if (iris_buf_left > 0) // =0 does not happen anyways
    	{
    		// copy whatever is left
    		memcpy(ptr, dst_ptr + iris_index, buf_step * iris_buf_left);
    		ziria_index += iris_buf_left;
    	}

		int samps = RX_BUFF_SIZE;
		complex16 * readPtr = dst_ptr;
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

		iris_buf_left = RX_BUFF_SIZE;
		// just memcpy from existing buffer

		int ziria_buf_left = size - ziria_index;
		memcpy(ptr + ziria_index, dst_ptr, buf_step * ziria_buf_left);
		iris_index = ziria_buf_left % RX_BUFF_SIZE;

    }
}
*/


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

/*
// uses time flag to send samples at a future time, also amalgomate 1024 samples for each send
 * this is probably buggy, no successful packet reception on the other end
void writeLimeRF(BlinkParams *params, complex16 *ptr, unsigned long size)
{
	SoapySDR::Device *iris = params->radioParams.iris;

	static unsigned int set_time = 0;
	static long long time = 0;
	static int buf_index = 0;

	bool batch_complete = false;
	int buf_left = 0;
    int flags = 0;
	flags |= SOAPY_SDR_HAS_TIME;
	int ret = 0;

	if (buf_index + size >= TX_BUFF_SIZE)
	{
		buf_left = (TX_BUFF_SIZE - buf_index);
		memcpy(src_ptr + buf_index, ptr, buf_left * sizeof(complex16));
		buf_index = buf_index + size - TX_BUFF_SIZE;
		batch_complete = true;
	}
	else
	{
	    memcpy(src_ptr + buf_index, ptr, size * sizeof(complex16));
	    buf_index += size;
	}

	if (batch_complete)
	{
		// set time, 1MTU (worth of samples) * sample_time (400ns at 2.5 MHz) = ,
		if (!set_time)
		{
			time = iris->getHardwareTime("") + timeAdv;
			set_time = 1;
		}

		if (time < iris->getHardwareTime(""))
			time = iris->getHardwareTime("") + timeAdv;

		int samps = TX_BUFF_SIZE;
		complex16 * readPtr = src_ptr;
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

		if (buf_index)
		{
			memcpy(src_ptr, ptr + buf_left, buf_index * sizeof(complex16));
			batch_complete = false;
		}
	}
}
*/
#endif
