
#ifdef ZYNQ_RF
#include <stdio.h>
#include "fmcomms_radio.h"

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "params.h"
#include "numerics.h"

#define FMCOMMS_RXBUFF_L 0x4000
#define AGC_SLOW1    "SLOW"
#define AGC_SLOW2    "FAST"
#define AGC_SLOW3    "HYBRID"
#define AGC_SLOW4    "MANUAL"
#define PORT_SELECT1 "A_BALANCED"
#define PORT_SELECT_TX "A"
#define True 1
#define False 1

int Fmcomms_Init(BlinkParams *params)
{
    int i;
	char *device_rx = "cf-ad9361-lpc";
	char *device_phy = "ad9361-phy";
	char *device_tx = "cf-ad9361-dds-core-lpc";

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

    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage1_rf_port_select", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write(params->radioParams.phych0, attr, PORT_SELECT_TX);
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set RF port select 1  (%i)\n", ret);
	    return ret;
    }

    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage0_hardwaregain", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, 0);  // this is in fact attenuation for fmcomm, leave 0 for now, mean high power
    if (ret < 0)
    {
	    fprintf(stderr, "Unable to set attenuation (%i)\n", ret);
	    return ret;
    }

    ret = iio_device_identify_filename(params->radioParams.phy, "out_voltage1_hardwaregain", &params->radioParams.phych0, &attr);
    ret = iio_channel_attr_write_double(params->radioParams.phych0, attr, 0); // this is in fact attenuation for fmcomm, leave 0 for now, mean high power
	if (ret < 0)
	{
		fprintf(stderr, "Unable to set attenuation (%i)\n", ret);
		return ret;
	}

	params->radioParams.Txbuf = iio_device_create_buffer(params->radioParams.txdev, params->radioParams.TXBufferSize, false);
	if (!params->radioParams.Txbuf)
	{
		fprintf(stderr, "Unable to create tx buffer\n");
		return -1;
	}

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

}



void Fmcomms_RadioStop(BlinkParams *params)
{
	if (params->radioParams.Txbuf != NULL)
	{
		iio_buffer_destroy(params->radioParams.Txbuf);
	}

	if (params->radioParams.Rxbuf != NULL)
	{
		iio_buffer_destroy(params->radioParams.Rxbuf);
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
}

void readFmcomms(BlinkParams *params, complex16 *ptr, int size){

	int ret = iio_buffer_refill(params->radioParams.Rxbuf);


	if (ret < 0)
	{
		fprintf(stderr, "Unable to fill rx buffer\n");
		exit(1);
	}


	void *buf_start0 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch0);
	void *buf_start1 = iio_buffer_first(params->radioParams.Rxbuf, params->radioParams.rxch1);
	void *buf_end = iio_buffer_end(params->radioParams.Rxbuf);
	ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Rxbuf);
    void * src_ptr0, *src_ptr1;
    complex16 *dst_ptr = (complex16*)malloc(sizeof(complex16)*size);
    memset(dst_ptr, 0, sizeof(complex16)*size);

    int i = 0;
    for (src_ptr0 = buf_start0, src_ptr1 = buf_start1; src_ptr0 < buf_end && src_ptr1 < buf_end && i < size; src_ptr0 += buf_step, src_ptr1 += buf_step, i++)
    {
    	int16_t  i_s, q_s;
    	iio_channel_convert(params->radioParams.rxch0, (void *)&i_s, (const void *) src_ptr0);
    	iio_channel_convert(params->radioParams.rxch1, (void *)&q_s, (const void *) src_ptr1);
    	dst_ptr[i].re = i_s;
    	dst_ptr[i].im = q_s;
    }
    memcpy(ptr, dst_ptr, i * sizeof(complex16));
    free(dst_ptr);
    //return i;


}

void writeFmcomms(BlinkParams *params, complex16 *ptr, int size)
{

	complex16 *src_ptr = ptr;
	void *buf_start0 = iio_buffer_first(params->radioParams.Txbuf, params->radioParams.txch0);
	void *buf_start1 = iio_buffer_first(params->radioParams.Txbuf, params->radioParams.txch1);
	void *buf_end = iio_buffer_end(params->radioParams.Txbuf);
	ptrdiff_t buf_step = iio_buffer_step(params->radioParams.Txbuf);
	void * dst_ptr0, *dst_ptr1;
	int i = 0;
	unsigned int length = iio_channel_get_data_format(params->radioParams.txch0)->length / 8;

    for (dst_ptr0 = buf_start0, dst_ptr1 = buf_start1; dst_ptr0 < buf_end && dst_ptr1 < buf_end && i < size; dst_ptr0 += buf_step, dst_ptr1 += buf_step, i++)  // for each two sample received from I channel we write one sample (int32)
    {
    	iio_channel_convert_inverse(params->radioParams.txch0, dst_ptr0, (const void *) &(src_ptr->re));
    	iio_channel_convert_inverse(params->radioParams.txch1, dst_ptr1, (const void *) &(src_ptr->im));
    	src_ptr = src_ptr + 1;
    }

	int ret = iio_buffer_push(params->radioParams.Txbuf);
	if (ret < 0)
	{
		fprintf(stderr, "Unable to push tx buffer\n");
		exit(1);
	}



}

#endif
