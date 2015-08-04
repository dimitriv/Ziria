/**
Atomix project, WIFILIB_util.h, TODO: insert summary here
Copyright (c) 2015 Stanford University
Released under the Apache License v2.0. See the LICENSE file for details.
Author(s): Manu Bansal
*/

#ifndef WIFILIB_UTIL_H_
#define WIFILIB_UTIL_H_
#ifdef DEAL_WITH_ME_LATER

#include <or_types.h>


void WIFILIB_fftShift64(
	Cplx16 const * const restrict symbol_with_DC_at_idx_0,
	Cplx16 * const restrict symbol_with_DC_in_middle
	);

/* ========================================================================== *
 *			 Inline Functions	      *
 * ========================================================================== */
static inline void WIFILIB_util_applyGain(
		IN	Cplx16U *sample_without_gain,
		OUT	Cplx16U *sample_with_gain,
		IN 	Uint32  gain_as_bits_to_shift_left
		) {
	sample_with_gain->realimag = _dpackl2(
	  //_sshvr(_sshvl(sample_without_gain->cplx16.real, gain_as_bits_to_shift_left + 16), 17),
	  //_sshvr(_sshvl(sample_without_gain->cplx16.imag, gain_as_bits_to_shift_left + 16), 17)
	  //      		);
	  ////shift right by one extra bit is to ensure that the maximum negative value
	  ////is -32768/2, not -32768, so that it can be negated when finding the conjugate.

	  _sshvr(_sshvl(sample_without_gain->cplx16.real, gain_as_bits_to_shift_left + 16), 16),
	  _sshvr(_sshvl(sample_without_gain->cplx16.imag, gain_as_bits_to_shift_left + 16), 16)
				);
	  //the maximum negative value is -32768/2, not -32768, so negation can
	  //result in overflow error if the existing value is -32768.
}

/** Description:
 * @param nus is the unnormalized soft estimate
 * @param sc tells the scale on which these values are aligned
 * 	sc = 0 corresponds to unit scale
 * 	sc = 1 means you take the unit scale values and multiply them by 2
 *  so if you have Q16 estimates with only 6 bits of precision, you 
 *  want to say that the scale is 10, that is, if you had the 6 bits
 *  of precision all lined up to unit scale, you would bump it up by
 *  10 bits to get the input you are passing in.
 * @param a tells the size of soft-estimate domain you want to map down to
 *  the normalized soft-estimate dynamic range. thus, if you want to
 *  map [-256,255] to your dynamic range, a is 8. any input with more
 *  than 8 bits worth of magnitude will end up getting saturated.
 * @param b tells the dynamic range of normalized soft-estimates, so that 
 *  if you want output in [-64,63], your b is 6. with a = 8 and b
 *  = 6, [-256,255]->[-64,63], and, for example, 256->63 too, and 
 *  -257->-64 too, assuming sc = 0. if sc = 1, -256->-32, since 
 *  -256 in the input is really meant to be understood as -128.
 */
static inline void WIFILIB_util_saturate(
	int32_t nus, 
	uint8_t input_scale, 
	uint8_t input_range, 
	uint8_t output_range, 
	BitsSoft *est
	){
//	Int8 est;
//	DEBUG(printf("a: %d  b: %d  sc: %d\n", a, b, sc);)
//	DEBUG(printf("nus: %08x  %012d\n", nus, nus);)

	nus = _sshvl(nus, 31 - input_range - input_scale);

//	DEBUG(printf("nus: %08x  %012d\n", nus, nus);)
	nus = _ext(nus, 0, 31 - output_range);
//	DEBUG(printf("nus: %08x  %012d\n", nus, nus);)
//	est = nus;
	*est = (int8_t)nus;
//	DEBUG(printf("est: %08x  %012d\n", est, est);)
//	DEBUG(printf("\n");)
}

static inline void WIFILIB_util_saturate_given_norm(
	int32_t nus,
	uint8_t input_range_norm,
	uint8_t output_range,
	BitsSoft *est) {
//	Int8 est;
//	DEBUG(printf("a: %d  b: %d  sc: %d\n", a, b, sc);)
//	DEBUG(printf("nus: %08x  %012d\n", nus, nus);)

	/////////nus = _sshvl(nus, 31 - input_range - input_scale);
	nus = _sshvl(nus, input_range_norm);

//	DEBUG(printf("nus: %08x  %012d\n", nus, nus);)
	nus = _ext(nus, 0, 31 - output_range);
//	DEBUG(printf("nus: %08x  %012d\n", nus, nus);)
//	est = nus;
	*est = (int8_t)nus;
//	DEBUG(printf("est: %08x  %012d\n", est, est);)
//	DEBUG(printf("\n");)
}

#endif
#endif
