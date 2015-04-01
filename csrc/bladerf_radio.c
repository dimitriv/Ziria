#ifdef BLADE_RF

#include <stdio.h>
#include "bladerf_radio.h"

#include "params.h"
#include "numerics.h"


// Currently hardcoded, should be put in params
//#define BLADE_RF_RX_FREQ     2412000000
#define BLADE_RF_RX_LNA      BLADERF_LNA_GAIN_MAX
#define BLADE_RF_RX_VGA1     20
#define BLADE_RF_RX_VGA2     BLADERF_RXVGA2_GAIN_MIN
#define BLADE_RF_RX_BUFF_L   8096



void BladeRF_RadioStart(BlinkParams *params)
{
	int status;

	printf("Opening and initializing device...\n\n");

	status = bladerf_open(&(params->radioParams.dev), "");
	if (status != 0) {
		fprintf(stderr, "Failed to open device: %s\n",
			bladerf_strerror(status));
		goto out;
	}

	status = bladerf_set_frequency((params->radioParams.dev), BLADERF_MODULE_RX, params->radioParams.CentralFrequency);
	if (status != 0) {
		fprintf(stderr, "Failed to set RX frequency: %s\n",
			bladerf_strerror(status));
		goto out;
	}
	else {
		printf("RX frequency: %u Hz\n", params->radioParams.CentralFrequency);
	}

	status = bladerf_set_sample_rate((params->radioParams.dev), BLADERF_MODULE_RX, params->radioParams.SampleRate, NULL);
	if (status != 0) {
		fprintf(stderr, "Failed to set RX sample rate: %s\n",
			bladerf_strerror(status));
		goto out;
	}
	else {
		printf("RX samplerate: %u sps\n", params->radioParams.SampleRate);
	}

	status = bladerf_set_bandwidth((params->radioParams.dev), BLADERF_MODULE_RX,
		params->radioParams.Bandwidth, NULL);
	if (status != 0) {
		fprintf(stderr, "Failed to set RX bandwidth: %s\n",
			bladerf_strerror(status));
		goto out;
	}
	else {
		printf("RX bandwidth: %u Hz\n", params->radioParams.Bandwidth);
	}

	status = bladerf_set_lna_gain((params->radioParams.dev), BLADE_RF_RX_LNA);
	if (status != 0) {
		fprintf(stderr, "Failed to set RX LNA gain: %s\n",
			bladerf_strerror(status));
		goto out;
	}
	else {
		printf("RX LNA Gain: Max\n");
	}

	status = bladerf_set_rxvga1((params->radioParams.dev), BLADE_RF_RX_VGA1);
	if (status != 0) {
		fprintf(stderr, "Failed to set RX VGA1 gain: %s\n",
			bladerf_strerror(status));
		goto out;
	}
	else {
		printf("RX VGA1 gain: %d\n", BLADE_RF_RX_VGA1);
	}

	status = bladerf_set_rxvga2(params->radioParams.dev, BLADE_RF_RX_VGA2);
	if (status != 0) {
		fprintf(stderr, "Failed to set RX VGA2 gain: %s\n",
			bladerf_strerror(status));
		goto out;
	}
	else {
		printf("RX VGA2 gain: %d\n\n", BLADE_RF_RX_VGA2);
	}


	// TX buffer
	params->TXBuffer = malloc(params->radioParams.TXBufferSize);
	if (params->TXBuffer == NULL) {
		perror("malloc");
		exit(1);
	}

	// RX buffer
	params->pRxBuf = malloc(BLADE_RF_RX_BUFF_L);
	if (params->pRxBuf == NULL) {
		perror("malloc");
		exit(1);
	}

out:
	if (status != 0) {
		bladerf_close(params->radioParams.dev);
	}
}



void BladeRF_RadioStop(BlinkParams *params) 
{
	int status;

	// Disable RX module, shutting down our underlying RX stream 
	status = bladerf_enable_module(params->radioParams.dev, BLADERF_MODULE_RX, false);
	if (status != 0) {
		fprintf(stderr, "Failed to disable RX module: %s\n",
			bladerf_strerror(status));
	}

	bladerf_close(params->radioParams.dev);

	if (params->TXBuffer != NULL)
	{
		free(params->TXBuffer);
	}

	if (params->pRxBuf != NULL)
	{
		free(params->pRxBuf);
	}

}

#endif