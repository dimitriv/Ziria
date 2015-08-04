/**
Atomix project, ORILIB_k.h, TODO: insert summary here
Copyright (c) 2015 Stanford University
Released under the Apache License v2.0. See the LICENSE file for details.
Author(s): Manu Bansal
*/

#ifndef __ORILIB__K_H__
#define __ORILIB__K_H__


//uncomment the following to turn off gain selection
//comment it to let the agc loop pick gain based on agc algorithm
#define NO_AGC


#define WIFI_OFDM_SYMBOL_N_SAMPLES_WITH_CP	80

#define SYNC_BUFFER_SIZE_ENERGY			WIFI_OFDM_SYMBOL_N_SAMPLES_WITH_CP		//input and output buffer size
#define SYNC_WINDOW_SIZE_ENERGY			80			//window 1 size
#define SYNC_WINDOW_SIZE_ENERGY_N_BITS		7

#define SYNC_WINDOW_SIZE_AGC						8
#define SYNC_WINDOW_SIZE_AGC_N_BITS					3

//a longer window will be more stable in
//energy estimates and more robust to variations due to noise, but a smaller window will be more reactive. resilience to
//noise can also be controlled with the agc gain relock threshold (see below), which determines the minimum step size to
//relock. hence, in general, it may work better to keep the window small but threshold higher, since the arrival of packet
//would actually correspond to a sharp rise, thus matching this combination, where noise variations will be exact opposite
//in behavior. the better the agc responds, the less is the distortion in training sequence due to delay in agc reponse,
//and better will be peak correlation values.
//Also note that if the window is shorter and agc responds faster, on an ascending energy curve (as in a windowed reception
//of a packet signal), the agc will end up locking earlier than it would in a longer window, and likely lock to a higher
//gain value, since the gain value is on a descending curve as received power is on an ascending curve. this means it's
//important to keep a higher safety margin in gain value, so that we can accommodate the larger gain value. thus, we still
//get the benefit of quick locking and more uniform gain value across the packet preamble with short window, and avoid
//saturation with higher safety margin.
//#define SYNC_AGC_GAIN_SAFETY_MARGIN_FOR_AGC_INACCURACIES		3	//leave 3-bit (use 2 or 3-bit) room in sample gain
#define SYNC_AGC_GAIN_SAFETY_MARGIN_FOR_AGC_INACCURACIES		2	//leave 3-bit (use 2 or 3-bit) room in sample gain

//agc gain is still estimated based on the average power seen in a window of samples. however, individual samples (instantaneous
//power) may still have higher magnitudes than average power. if the PAPR is high, room must be kept in picking the agc gain so
//that even the peak-samples (in power sense) can be kept undistorted.
#define SYNC_AGC_GAIN_SAFETY_MARGIN_FOR_HIGH_PAPR			2

#define SYNC_AGC_GAIN_SAFETY_MARGIN	(SYNC_AGC_GAIN_SAFETY_MARGIN_FOR_AGC_INACCURACIES + SYNC_AGC_GAIN_SAFETY_MARGIN_FOR_HIGH_PAPR)

#define SYNC_WINDOW_SIZE_ENERGY__ACCUMULATION_RSHIFT  MAX((SYNC_WINDOW_SIZE_ENERGY_N_BITS - SYNC_AGC_GAIN_SAFETY_MARGIN), 0)
//this right shift is used before adding a term to an accumulation buffer so that accumulation buffer does not overflow


//using a margin of 1 could lead to a lot of saturated samples, though peak detection could work better
//3 is the safest, but may require lower peak thresholds because the correlation values will distort more
//and will flatten out the peak, as well as reduce absolute correlation. again, there is a tradeoff with
//agc window size too, as documented above.

#define SYNC_AGC_FURTHER_SAFETY_MARGIN_FOR_ABSOLUTELY_NO_SATURATION		1
//this is a margin on top of agc gain margin picked (possibly aggressively) for peak detection. this margin
//is used for more sensitive computation such as cfo estimation, where we don't want any saturation

#define SYNC_AGC_GAIN_RELOCK_THRESHOLD				3	
//3-bit difference in avg sample norm (norm of avg) to relock
//a higher relock threshold means better stability against noise variations and coarser agc operation, and more stable
//gain during packet preamble reception, since the agc will converge sooner. however, it also means more variation in
//best-possible-gain-value and locked-gain-value, so it will end up preserving less dynamic range. depending on the use
//of samples-with-gain, this may be better to keep tight to cause less loss-of-precision distortion (say, for limited-bit-width
//peak detection), or to keep loose to prevent saturation distortion when dynamic range is sufficient anyway (like for
//actual receive chain, where you don't want to distort samples and degrade SNR).

#define ENERGY_THRESHOLD			0x7000

#define	SYNC_PEAK_DOMINANCE_LENGTH	40

//at 1.20x
//#define SYNC_BUFFER_SIZE_ENERGY					96		//input and output buffer size
//#define WINDOW_SIZE					96		//window 1 size
//#define WINDOW_SIZE_Q15				0x300000

//NOTE: maximum allowed buffer size is 64K (2^16), otherwise angle computation will lead to overflow

//#define DELAYED_HISTORY_SIZE		W1_SIZE		//setting to the larger of largest 
										//window size and buffer size 

#define SYNC_POINT_PREPONE			4		//Number of samples by which the sync lock point is 
									//shifted to absorb noise effects.
									//Currently to set to 1/4 of the cyclic prefix length.

#define SYNC_POST_LOCK_BUFFER_SIZE					80

//#define SYNC_ALIGNED_SAMPLE_BUF_LEN_ACTUAL			(SYNC_PEAK_DOMINANCE_LENGTH + SYNC_BUFFER_SIZE_ENERGY + 3 * SYNC_POST_LOCK_BUFFER_SIZE + 2)
//	//we want at least max(SYNC_PEAK_DOMINANCE_LENGTH, SYNC_BUFFER_SIZE_ENERGY, 2 * SYNC_ALIGNED_BUFFER_SIZE) + 2,
//	//so that we can store two aligned buffers in it as well, and the addition 2 sample length is because
////CORRECTION: WE NEED SYNC_POST_LOCK_BUFFER_SIZE * 3 if we want two full aligned buffers to be available,
////because it may span three such non-aligned buffers, which means that the last buffer will have some residue
////when we fill part of it to complete our needed two buffers. In order for that residue to not overwrite the
////head of two useful buffers by wrapping around, we need at least one more buffer length.
//	//the peak detection block starts filling from the sample at which peak became maximum, which is the
//	//point where stf ends, whereas we want the point where ltf starts. So we can pass in alignedSampleBuf[1],
//	//and then use alignedSampleBuf[2] in the future, which will be 8-byte aligned as well as properly symbol-
//	//aligned to ltf start. The length that we will advertise to getAlignedSamples will be a the largest multiple of
//	//SYNC_ALIGNED_BUFFER_SIZE that is at most SYNC_PEAK_DOMINANCE_LENGTH + SYNC_BUFFER_SIZE_ENERGY + 2 * SYNC_ALIGNED_BUFFER_SIZE,
//	//.i.e. ((SYNC_PEAK_DOMINANCE_LENGTH + SYNC_BUFFER_SIZE_ENERGY + 2 * SYNC_ALIGNED_BUFFER_SIZE)/SYNC_ALIGNED_BUFFER_SIZE) * SYNC_ALIGNED_BUFFER_SIZE

//to work with the new creative alignment scheme
//#define SYNC_ALIGNED_SAMPLE_BUF_LEN_ACTUAL			(SYNC_BUFFER_SIZE_ENERGY * 4)


//#define SYNC_UNALIGNED_INPUT_SAMPLE_BUFFER_SIZE		80
#define SYNC_UNALIGNED_INPUT_SAMPLE_BUFFER_SIZE_FOR_PKT_DETECT	SYNC_BUFFER_SIZE_ENERGY
#define SYNC_UNALIGNED_INPUT_SAMPLE_BUFFER_SIZE			96
#define SYNC_ALIGNED_OUTPUT_SAMPLE_BUFFER_SIZE			80
#define SYNC_ALIGNED_SAMPLE_LOOKBACK_WINDOW_SIZE		(SYNC_UNALIGNED_INPUT_SAMPLE_BUFFER_SIZE * 4)


	//the additional 2 in buffer length is so that we can pass the block
	//&alignedSampleBuf[1], where the extra sample to be discarded will
	//occupy alignedSampleBuf[1]. Then, useful aligned samples will be
	//in &alignedSampleBuf[2], which will be 8-byte aligned if alignedSampleBuf
	//is also 8-byte aligned.									

#endif
