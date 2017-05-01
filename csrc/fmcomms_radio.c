
/*
Coyright (c) Northeastern University, GENESYS Lab
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


#ifdef ADI_RF
#include <stdio.h>
#include "fmcomms_radio.h"

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>


#include "params.h"
#include "numerics.h"

#define FMCOMMS_RXBUFF_L 12288 //49152//786432
#define FMCOMMS_TXBUFF_L 0x200
#define AGC_SLOW    "slow_attack"
#define AGC_FAST    "fast_attack"
#define AGC_HYBRID    "HYBRID"
#define AGC_MANUAL    "MANUAL"
#define PORT_SELECT1 "A_BALANCED"
#define PORT_SELECT_TX "A"

//#define OUV_THREAD
#define RX 1
static complex16 *src_ptr;
static complex16 *dst_ptr;

static int num_buf_write;

#ifdef OUV_THREAD

#include <pthread.h>
#include <signal.h>
#include <unistd.h>

static bool flow_monitor = true;
pthread_t monitor_thread;

struct device_info {
	char * device_name;
	char * host_ip;

};

static struct iio_device *get_device(const struct iio_context *ctx,
		const char *id)
{

	unsigned int i, nb_devices = iio_context_get_devices_count(ctx);
	struct iio_device *device;

	for (i = 0; i < nb_devices; i++) {
		const char *name;
		device = iio_context_get_device(ctx, i);
		name = iio_device_get_name(device);
		if (name && !strcmp(name, id))
			break;
		if (!strcmp(id, iio_device_get_id(device)))
			break;
	}

	if (i < nb_devices)
		return device;

	fprintf(stderr, "Device %s not found\n", id);
	return NULL;
}

static void *flow_monitor_thread(void *dev_info)
{
	struct iio_context *ctx;
	struct iio_device *dev;
	uint32_t val;
	int ret;
	struct device_info * info = (struct device_info *)dev_info;
	if (!(info->host_ip))
		ctx = iio_create_default_context();
	else
		ctx = iio_create_network_context(info->host_ip);
	if (!ctx) {
		if (info->host_ip)
			fprintf(stderr, "Unable to create IIO context for host %s \n", info->host_ip);
		else
			fprintf(stderr, "Unable to create IIO context\n");
		return (void *)-1;
	}

	dev = get_device(ctx, info->device_name);
	if (!dev) {
		fprintf(stderr, "Unable to find IIO device\n");
		iio_context_destroy(ctx);
		return (void *)-1;
	}

	// Give the main thread a moment to start the DMA
	sleep(1);

	// Clear all status bits
	iio_device_reg_write(dev, 0x80000088, 0x6);

	while (flow_monitor) {

		ret = iio_device_reg_read(dev, 0x80000088, &val);
		if (ret) {
			fprintf(stderr, "Failed to read status register: %s\n",
					strerror(-ret));
			continue;
		}
#ifdef RX
		if (val & 4) // rx
			fprintf(stderr, "Overflow detected\n");
#else
		if (val & 1) // tx
			fprintf(stderr, "Underflow detected\n");
#endif
		if (val)
			iio_device_reg_write(dev, 0x80000088, val);


		sleep(1);
	}

	return (void *)0;
}

static void set_handler(int signal_nb, void (*handler)(int))
{
	struct sigaction sig;
	sigaction(signal_nb, NULL, &sig);
	sig.sa_handler = handler;
	sigaction(signal_nb, &sig, NULL);
}

static void quit_all(int sig)
{
	flow_monitor = false;
	pthread_join(monitor_thread, NULL);
	exit(0);
}

#endif




// use only when FMCOMMS_RXBUFF_L is bigger than ziria size
void readFmcomms(BlinkParams *params, complex16 *ptr, int size)
{
	static int iio_index = 0;
	int ziria_index = 0;


	if (iio_index == 0)
	{
		// refill buffer and copy
		int ret = iio_buffer_refill(params->radioParams.Rxbuf);
		if (ret < 0)
		{
			fprintf(stderr, "Unable to fill rx buffer %d\n", errno);
			exit(1);
		}

	}

    int iio_buf_left = FMCOMMS_RXBUFF_L - iio_index;
	void *buf_start0 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch0);
	ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Rxbuf);

    if (iio_buf_left >= size)
    {
    	// just memcpy from existing buffer
    	memcpy(ptr, buf_start0 + (buf_step * iio_index), buf_step * size);

    	// update iio_buf_left and iio_index
    	iio_index = (iio_index + size) % FMCOMMS_RXBUFF_L;
    	return;
    }
    else
    {

    	if (iio_buf_left > 0) // =0 does not happen anyways
    	{
    		// copy whatever is left
    		memcpy(ptr, buf_start0 + (buf_step * iio_index), buf_step * iio_buf_left);
    		ziria_index += iio_buf_left;
    	}
		// refill buffer and copy
		int ret = iio_buffer_refill(params->radioParams.Rxbuf);
		if (ret < 0)
		{
			fprintf(stderr, "Unable to fill rx buffer %d\n", errno);
			exit(1);
		}
		iio_buf_left = FMCOMMS_RXBUFF_L;
		// just memcpy from existing buffer
		void *buf_start0 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch0);
		ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Rxbuf);

		int ziria_buf_left = size - ziria_index;
		memcpy(ptr + ziria_index, buf_start0, buf_step * ziria_buf_left);
		iio_index = ziria_buf_left % FMCOMMS_RXBUFF_L;

    }
}


void writeFmcomms(BlinkParams *params, complex16 *ptr, int size)
{
	static int buf_index = 0;
	bool batch_complete = false;
	int buf_left = 0;
	int ret;

	if (buf_index + size >= FMCOMMS_TXBUFF_L)
	{
		buf_left = (FMCOMMS_TXBUFF_L - buf_index);
		memcpy(src_ptr + buf_index, ptr, buf_left * sizeof(complex16));
		buf_index = buf_index + size - FMCOMMS_TXBUFF_L;
		batch_complete = true;
	}
	else
	{
	    memcpy(src_ptr + buf_index, ptr, size * sizeof(complex16));
	    buf_index += size;
	}

	if (batch_complete)
	{
		complex16 * tmp_src = src_ptr;
		uintptr_t buf_start0 = (uintptr_t) iio_buffer_first(params->radioParams.Txbuf, params->radioParams.txch0);
		uintptr_t buf_start1 = (uintptr_t) iio_buffer_first(params->radioParams.Txbuf, params->radioParams.txch1);
		uintptr_t buf_end = (uintptr_t) iio_buffer_end(params->radioParams.Txbuf);
		ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Txbuf);
		uintptr_t dst_ptr0, dst_ptr1;
		unsigned int length = iio_channel_get_data_format(params->radioParams.txch0)->length / 8;

	    for (dst_ptr0 = buf_start0, dst_ptr1 = buf_start1; dst_ptr0 < buf_end && dst_ptr1 < buf_end; dst_ptr0 += buf_step, dst_ptr1 += buf_step)  // for each two sample received from I channel we write one sample (int32)
	    {
	    	tmp_src->re = tmp_src->re * 8;
	    	tmp_src->im = tmp_src->im * 8;
	    	iio_channel_convert_inverse(params->radioParams.txch0, (void *) dst_ptr0, (const void *) &(tmp_src->re));
	    	iio_channel_convert_inverse(params->radioParams.txch1, (void *) dst_ptr1, (const void *) &(tmp_src->im));
	    	tmp_src = tmp_src + 1;
	    }

		int ret = iio_buffer_push(params->radioParams.Txbuf);
		num_buf_write++;
		if (ret < 0)
		{
			fprintf(stderr, "Unable to push tx buffer\n");
			exit(1);
		}

		if (buf_index)
		{
			memcpy(src_ptr, ptr + buf_left, buf_index * sizeof(complex16));
			batch_complete = false;
		}
	}
}


int Fmcomms_Init(BlinkParams *params)
{
    int i;
	char *device_rx = "cf-ad9361-lpc";
	char *device_phy = "ad9361-phy";
	char *device_tx = "cf-ad9361-dds-core-lpc";
#ifdef OUV_THREAD
	set_handler(SIGINT, &quit_all);
	set_handler(SIGSEGV, &quit_all);
	set_handler(SIGTERM, &quit_all);
#endif
	if (!params->radioParams.host) {
		params->radioParams.ctx = iio_create_default_context();
	    if (!params->radioParams.ctx)
	    	params->radioParams.ctx = iio_create_network_context(NULL);
    } else {
    	params->radioParams.ctx = iio_create_network_context(params->radioParams.host);
    	printf("Connecting to %s ... \n", params->radioParams.host);
    }

    if (params->radioParams.ctx) {
    	params->radioParams.rxdev = iio_context_find_device(params->radioParams.ctx, device_rx);
    	params->radioParams.phy = iio_context_find_device(params->radioParams.ctx, device_phy);
    	params->radioParams.txdev = iio_context_find_device(params->radioParams.ctx, device_tx);
    }
    if (params->radioParams.ctx && (!params->radioParams.rxdev || !params->radioParams.phy || !params->radioParams.txdev )) {
	    iio_context_destroy(params->radioParams.ctx);
		fprintf(stderr, "Failed to open device!\n");
		return -1;
    }

    /* First disable all channels */

    int nb_channels = iio_device_get_channels_count(params->radioParams.txdev);
    for (i = 0; i < nb_channels; i++)
	    iio_channel_disable(iio_device_get_channel(params->radioParams.txdev, i));

    nb_channels = iio_device_get_channels_count(params->radioParams.rxdev);
    for (i = 0; i < nb_channels; i++)
	    iio_channel_disable(iio_device_get_channel(params->radioParams.rxdev, i));

    params->radioParams.rxch0 = iio_device_find_channel(params->radioParams.rxdev, "voltage0", false); // input
    params->radioParams.rxch1 = iio_device_find_channel(params->radioParams.rxdev, "voltage1", false); // input
    params->radioParams.txch0 = iio_device_find_channel(params->radioParams.txdev, "voltage0", true); // output
    params->radioParams.txch1 = iio_device_find_channel(params->radioParams.txdev, "voltage1", true); // output

    if (!params->radioParams.txch0 || !params->radioParams.txch1 || !params->radioParams.rxch0 || !params->radioParams.rxch1) {
    	fprintf(stderr, "Channel not found\n");
    	return -1;
    }

    iio_channel_enable(params->radioParams.rxch0);
    iio_channel_enable(params->radioParams.rxch1);
    iio_channel_enable(params->radioParams.txch0);
    iio_channel_enable(params->radioParams.txch1);
#ifdef OUV_THREAD
    struct device_info * info = (struct device_info *)calloc(1, sizeof(struct device_info));;
    info[0].host_ip = params->radioParams.host;
#ifdef RX
    info[0].device_name = device_rx;
    int ret = pthread_create(&monitor_thread, NULL, flow_monitor_thread, &info[0]);
#else
    info[0].device_name = device_tx;
    int ret = pthread_create(&monitor_thread, NULL, flow_monitor_thread, &info[0]);
#endif
	if (ret) {
		fprintf(stderr, "Failed to create monitor thread: %s\n",
				strerror(-ret));
	}
#endif
    return 0;
}

int Fmcomms_RadioStartTx(BlinkParams *params)
{
	int ret = 0;
	const char *attr = NULL;
    ret = iio_device_identify_filename(params->radioParams.phy, "out_altvoltage1_TX_LO_frequency", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_longlong(params->radioParams.phych0, attr, (long long) params->radioParams.CentralFrequency);
	if (ret < 0)
    {
	    fprintf(stderr, "Unable to set LO frequency (%i)\n", ret);
	    return ret;
    }

    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage_sampling_frequency", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_longlong(params->radioParams.phych0, attr, (long long) params->radioParams.SampleRate);
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set samplerate (%i)\n", ret);
	    return ret;
    }

    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage_rf_bandwidth", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_longlong(params->radioParams.phych0, attr, (long long) params->radioParams.Bandwidth);
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set bandwidth (%i)\n", ret);
	    return ret;
    }

    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage0_rf_port_select", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write(params->radioParams.phych0, attr, PORT_SELECT_TX);
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set RF port select 0  (%i)\n", ret);
	    return ret;
    }
/*
    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage1_rf_port_select", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write(params->radioParams.phych0, attr, PORT_SELECT_TX);
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set RF port select 1  (%i)\n", ret);
	    return ret;
    }
*/
    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage0_hardwaregain", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, 0);  // this accepts values <= 0 which means attenuation rather than gain
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set attenuation (%i)\n", ret);
	    return ret;
    }
/*
    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage1_hardwaregain", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, 0); // this accepts values <= 0 which means attenuation rather than gain
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set attenuation (%i)\n", ret);
		return ret;
	}
*/
	params->radioParams.Txbuf = iio_device_create_buffer(params->radioParams.txdev, FMCOMMS_TXBUFF_L, false);
	if (!params->radioParams.Txbuf)
	{
		fprintf(stderr, "Unable to create tx buffer\n");
		return -1;
	}

	src_ptr = (complex16*)malloc(sizeof(complex16) * FMCOMMS_TXBUFF_L);
	num_buf_write = 0;
	return 0;

}
int Fmcomms_RadioStartRx(BlinkParams *params)
{

	int ret = 0;
	const char *attr = NULL;

	ret = iio_device_identify_filename(params->radioParams.phy, "out_altvoltage0_RX_LO_frequency", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_longlong(params->radioParams.phych0, attr, (long long) params->radioParams.CentralFrequency);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set LO frequency (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_sampling_frequency", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_longlong(params->radioParams.phych0, attr, (long long) params->radioParams.SampleRate);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set samplerate (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_rf_bandwidth", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_longlong(params->radioParams.phych0, attr, (long long) params->radioParams.Bandwidth);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set bandwidth (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage0_rf_port_select", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write(params->radioParams.phych0, attr, PORT_SELECT1);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set RF port select (%i)\n", ret);
		return ret;
	}
	/*
	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage0_gain_control_mode", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write(params->radioParams.phych0, attr, AGC_SLOW);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set gain (%i)\n", ret);
		return ret;
	}
	*/
	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage0_hardwaregain", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, params->radioParams.RXgain);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set gain (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage1_hardwaregain", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, params->radioParams.RXgain);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set gain (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_quadrature_tracking_en", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_bool(params->radioParams.phych0, attr, true);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to enable quadrature (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_rf_dc_offset_tracking_en", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_bool(params->radioParams.phych0, attr, true);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to enable RF DC (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_bb_dc_offset_tracking_en", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_bool(params->radioParams.phych0, attr, true);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to enable BB DC (%i)\n", ret);
		return ret;
	}

	params->radioParams.Rxbuf = iio_device_create_buffer(params->radioParams.rxdev, FMCOMMS_RXBUFF_L, false);
	if (!params->radioParams.Rxbuf)
	{
		fprintf(stderr, "Unable to create rx buffer\n");
		return -1;
	}

	dst_ptr = (complex16*)malloc(sizeof(complex16) * FMCOMMS_RXBUFF_L);
/*
	ret = iio_buffer_set_blocking_mode(params->radioParams.Rxbuf, false);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set non-blocking mode\n", ret);
		return ret;
	}
*/
	return 0;

}



void Fmcomms_RadioStop(BlinkParams *params)
{
#ifdef OUV_THREAD
	flow_monitor = false;
	pthread_join(monitor_thread, NULL);
#endif

	if (params->radioParams.Txbuf != NULL)
	{
		iio_buffer_destroy(params->radioParams.Txbuf);
		params->radioParams.Txbuf = NULL;
	}

	if (params->radioParams.Rxbuf != NULL)
	{
		iio_buffer_destroy(params->radioParams.Rxbuf);
		params->radioParams.Rxbuf = NULL;
	}

	if (params->radioParams.rxch0)
		iio_channel_disable(params->radioParams.rxch0);

	if (params->radioParams.rxch1)
		iio_channel_disable(params->radioParams.rxch1);

	if (params->radioParams.txch0)
		iio_channel_disable(params->radioParams.txch0);

	if (params->radioParams.txch1)
		iio_channel_disable(params->radioParams.txch1);

	if (params->radioParams.ctx)
		iio_context_destroy(params->radioParams.ctx);

	free(src_ptr);
	free(dst_ptr);
}

#endif
