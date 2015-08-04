/* ======================================================================= */
/* DSP_ifft16x16.h -- 16x16 Mixed Radix Inverse FFT                        */
/*                    Intrinsic C Implementation                           */
/*                                                                         */
/* Rev 0.0.1                                                               */
/*                                                                         */
/* Copyright (C) 2011 Texas Instruments Incorporated - http://www.ti.com/  */ 
/*                                                                         */
/*                                                                         */
/*  Redistribution and use in source and binary forms, with or without     */
/*  modification, are permitted provided that the following conditions     */
/*  are met:                                                               */
/*                                                                         */
/*    Redistributions of source code must retain the above copyright       */
/*    notice, this list of conditions and the following disclaimer.        */
/*                                                                         */
/*    Redistributions in binary form must reproduce the above copyright    */
/*    notice, this list of conditions and the following disclaimer in the  */
/*    documentation and/or other materials provided with the               */
/*    distribution.                                                        */
/*                                                                         */
/*    Neither the name of Texas Instruments Incorporated nor the names of  */
/*    its contributors may be used to endorse or promote products derived  */
/*    from this software without specific prior written permission.        */
/*                                                                         */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  */
/*  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/*  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,  */
/*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       */
/*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  */
/*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  */
/*  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  */
/*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   */
/*                                                                         */
/* ======================================================================= */

#ifndef DSP_IFFT16X16_H_
#define DSP_IFFT16X16_H_ 1

/** @ingroup FFT */
/* @{ */

/** @defgroup  DSP_ifft16x16 */
/** @ingroup DSP_ifft16x16 */
/* @{ */

/**
 *      This code performs a Radix-4 IFFT with digit reversal.  The code
 *      uses a special ordering of twiddle factors and memory accesses  
 *      to improve performance in the presence of cache.  It operates   
 *      largely in-place, but the final digit-reversed output is written
 *      out-of-place.                                                   
 *
 *      @param ptr_w:   Pointer to vector of Q.15 FFT coefficients of size 2*nx elements.
 *      @param npoints: Number of complex elements in vector x.       
 *      @param ptr_x:   Pointer to input vector of size 2*nx elements.
 *      @param ptr_y:   Pointer to output vector of size 2*nx elements.
 *
 * @par Algorithm:
 * DSP_ifft16x16_cn.c is the natural C equivalent of the optimized intrinsic C code without
 * restrictions. Note that the intrinsic C code is optimized and restrictions may
 * apply.  
 * 
 * @par Assumptions:
 *     In-place computation is not allowed. <BR>                             
 *     Size of FFT, nx, must be power of 2 and 16<=nx<=32768. <BR>                                 
 *     The arrays for the complex input data x[], complex output data y[]
 *        and twiddle factor w[] must be double word aligned. <BR>
 *     The input and output data are complex, with the real/imaginary 
 *        components stored in adjacent locations in the array.  The real
 *        components are stored at even array indices, and the imaginary 
 *        components are stored at odd array indices. <BR>                    
 *
 * @par Implementation Notes:
 * @b Endian Support: 
 *    The intrinsic optimized C code supports both big and little endian modes. <BR> 
 *    The linear assembly code supports little endian mode only. <BR> 
 * @b Interruptibility: The code is interruptible. <BR>
 *
 */

void DSP_ifft16x16 (
    const short * restrict ptr_w,
    int npoints,
    short * restrict ptr_x,
    short * restrict ptr_y
);

#endif

/* ======================================================================== */
/*  End of file:  DSP_ifft16x16.h                                           */
/* ------------------------------------------------------------------------ */
/*            Copyright (c) 2011 Texas Instruments, Incorporated.           */
/*                           All Rights Reserved.                           */
/* ======================================================================== */

