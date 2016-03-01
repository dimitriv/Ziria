
#ifdef ZYNQ_RF
#include <stdio.h>
#include "fmcomms_radio.h"

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include <pthread.h>
#include <signal.h>
#include <unistd.h>

#include "params.h"
#include "numerics.h"

#define FMCOMMS_RXBUFF_L 0x600
#define FMCOMMS_TXBUFF_L 0x600
#define AGC_SLOW1    "SLOW"
#define AGC_SLOW2    "FAST"
#define AGC_SLOW3    "HYBRID"
#define AGC_SLOW4    "MANUAL"
#define PORT_SELECT1 "A_BALANCED"
#define PORT_SELECT_TX "A"
#define True 1
#define False 1

static complex16 *src_ptr;
static int num_buf_write;
/*
static bool flow_monitor = true;
static bool device_is_tx = false;
static bool device_is_rx = false;
pthread_t monitor_thread;

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

static void *flow_monitor_thread(void *device_name)
{
	struct iio_context *ctx;
	struct iio_device *dev;
	uint32_t val;
	int ret;

	ctx = iio_create_default_context();
	if (!ctx) {
		fprintf(stderr, "Unable to create IIO context\n");
		return (void *)-1;
	}

	dev = get_device(ctx, (char *)device_name);
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
		if (val & 4) // 1 for tx
			fprintf(stderr, "Overflow detected\n");
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
}

*/

int Fmcomms_Init(BlinkParams *params)
{
    int i;
	char *device_rx = "cf-ad9361-lpc";
	char *device_phy = "ad9361-phy";
	char *device_tx = "cf-ad9361-dds-core-lpc";
/*
	set_handler(SIGINT, &quit_all);
	set_handler(SIGSEGV, &quit_all);
	set_handler(SIGTERM, &quit_all);
*/
	if (!params->radioParams.host) {
		params->radioParams.ctx = iio_create_default_context();
	    if (!params->radioParams.ctx)
	    	params->radioParams.ctx = iio_create_network_context(NULL);
    } else {
    	params->radioParams.ctx = iio_create_network_context(params->radioParams.host);
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
/*
    int ret = pthread_create(&monitor_thread, NULL, flow_monitor_thread, (void *)device_rx);
	if (ret) {
		fprintf(stderr, "Failed to create monitor thread: %s\n",
				strerror(-ret));
	}
*/
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
	    fprintf(stderr, "Unable to set baudwidth (%i)\n", ret);
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
    ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, 0);  // this is in fact attenuation for fmcomm, leave 0 for now, mean high power
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set attenuation (%i)\n", ret);
	    return ret;
    }
/*
    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage1_hardwaregain", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, 0); // this is in fact attenuation for fmcomm, leave 0 for now, mean high power
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set attenuation (%i)\n", ret);
		return ret;
	}
*/
	params->radioParams.Txbuf = iio_device_create_buffer(params->radioParams.txdev, FMCOMMS_TXBUFF_L, false); //params->radioParams.TXBufferSize
	if (!params->radioParams.Txbuf)
	{
		fprintf(stderr, "Unable to create tx buffer\n");
		return -1;
	}

	src_ptr = (complex16*)malloc(sizeof(complex16) * FMCOMMS_TXBUFF_L);
	num_buf_write = 0;
	//device_is_tx = true;
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
		fprintf(stderr, "Unable to set baudwidth (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage0_rf_port_select", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write(params->radioParams.phych0, attr, PORT_SELECT1);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set RF port select (%i)\n", ret);
		return ret;
	}

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
	ret = iio_channel_attr_write_bool(params->radioParams.phych0, attr, True);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to enable quadrature (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_rf_dc_offset_tracking_en", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_bool(params->radioParams.phych0, attr, True);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to enable RF DC (%i)\n", ret);
		return ret;
	}

	ret = iio_device_identify_filename(params->radioParams.phy, "in_voltage_bb_dc_offset_tracking_en", &params->radioParams.phych0, &attr);
	ret = iio_channel_attr_write_bool(params->radioParams.phych0, attr, True);
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

	//device_is_rx = true;
	return 0;

}



void Fmcomms_RadioStop(BlinkParams *params)
{
	/*
	flow_monitor = false;
	pthread_join(monitor_thread, NULL);
	*/

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
}
/*
void readFmcomms(BlinkParams *params, complex16 *ptr, int size){

	static int batch = 0;
	int batch_num = FMCOMMS_RXBUFF_L/size;
	static complex16 *dst_ptr = NULL;
	int ret;
	if (!batch)
	{
		ret = iio_buffer_refill(params->radioParams.Rxbuf);

		if (ret < 0)
		{
			fprintf(stderr, "Unable to fill rx buffer\n");
			//flow_monitor = false;
			exit(1);
		}

		dst_ptr = (complex16*)realloc(dst_ptr, sizeof(complex16) * FMCOMMS_RXBUFF_L);
		void *buf_start0 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch0);
		void *buf_start1 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch1);
		void *buf_end = iio_buffer_end(params->radioParams.Rxbuf);
		ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Rxbuf);
	    void * src_ptr0, *src_ptr1;

	    memset(dst_ptr, 0, sizeof(complex16) * FMCOMMS_RXBUFF_L);

	    int i = 0;
	    for (src_ptr0 = buf_start0, src_ptr1 = buf_start1; src_ptr0 < buf_end && src_ptr1 < buf_end; src_ptr0 += buf_step, src_ptr1 += buf_step)
	    {
	    	int16_t  i_s, q_s;
	    	iio_channel_convert(params->radioParams.rxch0, (void *)&i_s, (const void *) src_ptr0);
	    	iio_channel_convert(params->radioParams.rxch1, (void *)&q_s, (const void *) src_ptr1);
	    	dst_ptr[i].re = i_s;
	    	dst_ptr[i].im = q_s;
	    	i++;
	    }
	}

    memcpy(ptr, dst_ptr + (batch * size), size * sizeof(complex16));
    batch = (++batch) % batch_num;

}
*/
void readFmcomms(BlinkParams *params, complex16 *ptr, int size)
{
	static complex16 *dst_ptr = NULL;
	static int buf_size = 0;
	static bool buf_empty = true;
	int buf_frac = 0;

	if (size > FMCOMMS_RXBUFF_L)
	{ // I am not sure of this condition, small buffer size for iio still has to be tested
		fprintf (stderr, "Error: FMCOMMS Rx buffer too small (%ld needed)!\n", size );
		exit(1);
	}

    if (buf_size + size >= FMCOMMS_RXBUFF_L)
	{
		buf_frac = (FMCOMMS_RXBUFF_L - buf_size);
		memcpy(ptr, dst_ptr + buf_size, buf_frac * sizeof(complex16));
		buf_size = buf_size + size - FMCOMMS_RXBUFF_L;
		buf_empty = true;
	}

	if (buf_empty)
	{
		int ret = iio_buffer_refill(params->radioParams.Rxbuf);

		if (ret < 0)
		{
			fprintf(stderr, "Unable to fill rx buffer\n");
			exit(1);
		}

		dst_ptr = (complex16*)realloc(dst_ptr, sizeof(complex16) * FMCOMMS_RXBUFF_L);
		void *buf_start0 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch0);
		void *buf_start1 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch1);
		void *buf_end = iio_buffer_end(params->radioParams.Rxbuf);
		ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Rxbuf);
	    void * src_ptr0, *src_ptr1;

	    memset(dst_ptr, 0, sizeof(complex16) * FMCOMMS_RXBUFF_L);

	    int i = 0;
	    for (src_ptr0 = buf_start0, src_ptr1 = buf_start1; src_ptr0 < buf_end && src_ptr1 < buf_end; src_ptr0 += buf_step, src_ptr1 += buf_step)
	    {
	    	int16_t  i_s, q_s;
	    	iio_channel_convert(params->radioParams.rxch0, (void *)&i_s, (const void *) src_ptr0);
	    	iio_channel_convert(params->radioParams.rxch1, (void *)&q_s, (const void *) src_ptr1);
	    	dst_ptr[i].re = i_s;
	    	dst_ptr[i].im = q_s;
	    	i++;
	    }
	    buf_empty = false;
	}

	if (buf_frac)
	{
	    memcpy(ptr + buf_frac, dst_ptr, buf_size * sizeof(complex16));
	}
	else
	{
	    memcpy(ptr, dst_ptr + buf_size, size * sizeof(complex16));
	    buf_size += size;
	}

}
/*
void writeFmcomms(BlinkParams *params, complex16 *ptr, int size)
{
	static int batch = 0;
	int batch_num = FMCOMMS_RXBUFF_L/size;
	memcpy(src_ptr + (batch * size), ptr, size * sizeof(complex16));
	batch = (++batch) % batch_num;

	if (!batch)
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
	    	//tmp_src->re = tmp_src->re * 128;
	    	//tmp_src->im = tmp_src->im * 128;
	    	iio_channel_convert_inverse(params->radioParams.txch0, (void *) dst_ptr0, (const void *) &(tmp_src->re));
	    	iio_channel_convert_inverse(params->radioParams.txch1, (void *) dst_ptr1, (const void *) &(tmp_src->im));
	    	tmp_src = tmp_src + 1;
	    }

		int ret = iio_buffer_push(params->radioParams.Txbuf);
		if (ret < 0)
		{
			fprintf(stderr, "Unable to push tx buffer\n");
			//flow_monitor = false;
			exit(1);
		}
	}
}
*/
void writeFmcomms(BlinkParams *params, complex16 *ptr, int size)
{
	static int buf_size = 0;
	bool batch_complete = false;
	int buf_frac = 0;
	int ret;

	if (buf_size + size >= FMCOMMS_TXBUFF_L)
	{
		buf_frac = (FMCOMMS_TXBUFF_L - buf_size);
		memcpy(src_ptr + buf_size, ptr, buf_frac * sizeof(complex16));
		buf_size = buf_size + size - FMCOMMS_TXBUFF_L;
		batch_complete = true;
	}
	else
	{
	    memcpy(src_ptr + buf_size, ptr, size * sizeof(complex16));
	    buf_size += size;
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
	    	tmp_src->re = tmp_src->re * 384;
	    	tmp_src->im = tmp_src->im * 384;
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

		if (buf_size)
		{
			memcpy(src_ptr, ptr + buf_frac, buf_size * sizeof(complex16));
			batch_complete = false;
		}
	}
}

#endif
