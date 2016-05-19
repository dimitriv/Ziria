

#pragma once
#ifdef ADI_RF
#include "params.h"
#include "numerics.h"
#include "neon/types.h"
#include <iio.h>
#include <errno.h>

int Fmcomms_RadioStartTx(BlinkParams *params);
int Fmcomms_RadioStartRx(BlinkParams *params);
int Fmcomms_Init(BlinkParams *params);
void Fmcomms_RadioStop(BlinkParams *params);
void readFmcomms(BlinkParams *params, complex16 *ptr, int size);

/*
inline void readFmcomms(BlinkParams *params, complex16 *ptr, int size)
{
	int ret = iio_buffer_refill(params->radioParams.Rxbuf);

	uint64_t * src_ptr0 = (uint64_t *)iio_buffer_start(params->radioParams.Rxbuf);
	//ptrdiff_t diff = iio_buffer_step(params->radioParams.Rxbuf);
	//unsigned long items_in_buffer = (unsigned long) ret / diff;
	//uint32_t midbuf_size = items_in_buffer < size ? size : items_in_buffer;
	//uint64_t * buf = (uint64_t *)malloc(ret);

	dest_ptr = (uint32_t *) ptr;
	memcpy(dest_ptr, src_ptr0, size * sizeof(complex16));


	//dest_ptr = (uint32_t *)malloc(ret);
	//memcpy(dest_ptr, src_ptr0, size * sizeof(complex16));
	//size_t vectors = ret / 32;
	//uint32_t* ptr0 = (uint32_t *)ptr;
	//while (vectors-- > 0) {
	//	const uint32x4_t src0 = vld1q_u32(dest_ptr);
	//    const uint32x4_t src1 = vld1q_u32(dest_ptr + 4);
	//    const uint32x4x2_t dst = vuzpq_u32(src0, src1);
	//    vst1q_u32(ptr0, dst.val[0]);
	//    dest_ptr += 8;
	//    ptr0 += 4;
	// }
}
*/
void writeFmcomms(BlinkParams *params, complex16 *ptr, int size);
//void iio_to_ziria_memcpy(complex16* zbuf, void * ibuf, int iiosize);
#endif
