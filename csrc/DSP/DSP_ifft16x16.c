/* ======================================================================= */
/*  TEXAS INSTRUMENTS, INC.                                                */
/*                                                                         */
/*  DSPLIB  DSP Signal Processing Library                                  */
/*                                                                         */
/*  This library contains proprietary intellectual property of Texas       */
/*  Instruments, Inc.  The library and its source code are protected by    */
/*  various copyrights, and portions may also be protected by patents or   */
/*  other legal protections.                                               */
/*                                                                         */
/*  This software is licensed for use with Texas Instruments TMS320        */
/*  family DSPs.  This license was provided to you prior to installing     */
/*  the software.  You may review this license by consulting the file      */
/*  TI_license.PDF which accompanies the files in this library.            */
/*                                                                         */
/* ----------------------------------------------------------------------- */
/*                                                                         */
/*  TEXAS INSTRUMENTS, INC.                                                */
/*                                                                         */
/*  NAME                                                                   */
/*      DSP_ifft16x16 -- ifft16x16                                         */
/*                                                                         */
/*  USAGE                                                                  */
/*                                                                         */
/*      This routine is C-callable and can be called as:                   */
/*                                                                         */
/*      void DSP_ifft16x16 (                                               */
/*          short * w,                                                     */
/*          int nx,                                                        */
/*          short * x,                                                     */
/*          short * y                                                      */
/*      )                                                                  */
/*                                                                         */
/*      w[2*nx]:    Pointer to vector of Q.15 FFT coefficients of size     */
/*                  2*nx elements.                                         */
/*                                                                         */
/*      nx:         Number of complex elements in vector x.                */
/*                                                                         */
/*      x[2*nx]:    Pointer to input vector of size 2*nx elements.         */
/*                                                                         */
/*      y[2*nx]:    Pointer to output vector of size 2*nx elements.        */
/*                                                                         */
/*                                                                         */
/*  DESCRIPTION                                                            */
/*                                                                         */
/*      This code performs a Radix-4 IFFT with digit reversal.  The code   */
/*      uses a special ordering of twiddle factors and memory accesses     */
/*      to improve performance in the presence of cache.  It operates      */
/*      largely in-place, but the final digit-reversed output is written   */
/*      out-of-place.                                                      */
/*                                                                         */
/*  ASSUMPTIONS                                                            */
/*                                                                         */
/*      The size of the IFFT, n, must be a power of 2 and greater than     */
/*      or equal to 16 and less than 32768.                                */
/*                                                                         */
/*      The arrays 'x[]', 'y[]', and 'w[]' all must be aligned on a        */
/*      double-word boundary for the "optimized" implementations.          */
/*                                                                         */
/*      The input and output data are complex, with the real/imaginary     */
/*      components stored in adjacent locations in the array.  The real    */
/*      components are stored at even array indices, and the imaginary     */
/*      components are stored at odd array indices.                        */
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

#pragma CODE_SECTION(DSP_ifft16x16, ".text:optimized");

#include "DSP_ifft16x16.h"
#include "c6x.h"

static inline void radix_2(short *restrict ptr_x, short *restrict ptr_y, int npoints);
static inline void radix_4(short *restrict ptr_x, short *restrict ptr_y, int npoints);

#ifdef _LITTLE_ENDIAN
void DSP_ifft16x16 (
    const short * restrict ptr_w,
    int npoints,
    short * restrict ptr_x,
    short * restrict ptr_y
)
{
    short * restrict x, * restrict x1, * restrict x2;
    const short * restrict tw;
    long long *restrict Twiddles;

    long long x_3210, x_7654, xl2_3210, xl2_7654, xl1_3210, xl1_7654, xh2_3210, xh2_7654;
    long long co11si11_co10si10, co13si13_co12si12, co13si13_co12si12_tmp;
    long long co1si1_1_co1si1_0, co2si2_1_co2si2_0, co3si3_1_co3si3_0;
    long long xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0;
    long long xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0;
    long long x_3o_x_2o_x_1o_x_0o, yt2_1_xt1_1_yt2_0_xt1_0, yt0_1_xt0_1_yt0_0_xt0_0, yt1_1_xt2_1_yt1_0_xt2_0;
    long long x_7o_x_6o_x_5o_x_4o, yt2_3_xt1_3_yt2_2_xt1_2, yt0_3_xt0_3_yt0_2_xt0_2, yt1_3_xt2_3_yt1_2_xt2_2;  

    int j, i, l1, l2, h2, fft_jmp, tw_jmp;
    int radix, predj, tw_offset, stride;

    /*----------------------------------------------------------------------*/
    /* Determine the magnitude of the number of points to be transformed.   */
    /* Check whether we can use a radix4 decomposition or a mixed radix     */
    /* transformation, by determining modulo 2.                             */
    /*----------------------------------------------------------------------*/
    radix = _norm(npoints) & 1 ? 8 : 4;

    /*----------------------------------------------------------------------*/
    /* The stride is quartered with every iteration of the outer loop. It   */
    /* denotes the seperation between any two adjacent inputs to the butter */
    /* -fly. This should start out at N/4, hence stride is initially set to */
    /* N. For every stride, 2*stride twiddle factors are accessed. The      */
    /* "tw_offset" is the offset within the current twiddle factor sub-     */
    /* table. This is set to zero, at the start of the code and is used to  */
    /* obtain the appropriate sub-table twiddle pointer by offseting it     */
    /* with the base pointer "ptr_w".                                       */
    /*----------------------------------------------------------------------*/
    stride  = npoints;
    fft_jmp = 6 * stride;
    tw_jmp  = 2 * stride;
    tw_offset = 0;

    _nassert(stride > 8);
    #pragma MUST_ITERATE(1,,1);

    while (stride > 8) {
        /*------------------------------------------------------------------*/
        /* At the start of every iteration of the outer loop, "j" is set    */
        /* to zero, as "tw" is pointing to the correct location within the  */
        /* twiddle factor array. For every iteration of the inner loop      */
        /* 2 * stride twiddle factors are accessed. For eg,                 */
        /*                                                                  */
        /* #Iteration of outer loop  # twiddle factors    #times cycled     */
        /*  1                          2 N/4               1                */
        /*  2                          2 N/16              4                */
        /*  ...                                                             */
        /*------------------------------------------------------------------*/
        j = 0;
        fft_jmp >>= 2;
        tw_jmp  >>= 2;

        /*------------------------------------------------------------------*/
        /* Set up offsets to access "N/4", "N/2", "3N/4" complex point or   */
        /* "N/2", "N", "3N/2" half word                                     */
        /*------------------------------------------------------------------*/
        h2 = stride >> 1;
        l1 = stride;
        l2 = stride + (stride >> 1);

        /*------------------------------------------------------------------*/
        /*  Reset "x" to point to the start of the input data array.        */
        /* "tw_offset" starts off at 0, and increments by "2 * stride"      */
        /*------------------------------------------------------------------*/
        x  = ptr_x;
        tw = ptr_w + tw_offset;
        Twiddles = (void*)tw;
        tw_offset += tw_jmp;

        /*----------------------------------------------------------------*/
        /* The following loop iterates through the different butterflies, */
        /* within a given stage. Recall that there are logN to base 4     */
        /* stages. Certain butterflies share the twiddle factors. These   */
        /* are grouped together. On the very first stage there are no     */
        /* butterflies that share the twiddle factor, all N/4 butter-     */
        /* flies have different factors. On the next stage two sets of    */
        /* N/8 butterflies share the same twiddle factor. Hence after     */
        /* half the butterflies are performed, j the index into the       */
        /* factor array resets to 0, and the twiddle factors are reused.  */
        /* When this happens, the data pointer 'x' is incremented by the  */
        /* fft_jmp amount. In addition the following code is unrolled to  */
        /* perform "4" radix4 butterflies in parallel.                    */
        /*----------------------------------------------------------------*/

        _nassert(npoints >= 16);
        #pragma MUST_ITERATE(1,,1);
        for (i = 0; i < npoints; i += 16) {

            /*-----------------------------------------------------------*/
            /* Read in complex input for the butterflies.                */
            /*-----------------------------------------------------------*/
            x_3210   = _amem8(&x[0]);
            x_7654   = _amem8(&x[4]);
            xl1_3210 = _amem8(&x[l1]);
            xl1_7654 = _amem8(&x[l1+4]);
            xl2_3210 = _amem8(&x[l2]);
            xl2_7654 = _amem8(&x[l2+4]);
            xh2_3210 = _amem8(&x[h2]);
            xh2_7654 = _amem8(&x[h2+4]);

            /*-----------------------------------------------------------*/
            /* Derive output pointers using the input pointer "x"        */
            /*-----------------------------------------------------------*/
            x2 = (short *)_mvd((int)x);

            /*-----------------------------------------------------------*/
            /* Read in complex input for twiddle factors.                */
            /*-----------------------------------------------------------*/
            co11si11_co10si10 = *Twiddles++;
            co13si13_co12si12_tmp = *Twiddles++;
            co13si13_co12si12 = _dmvd(_hill(co13si13_co12si12_tmp), _loll(co13si13_co12si12_tmp));

            /*-----------------------------------------------------------*/
            /* When the twiddle factors are not to be re-used, j is      */
            /* incremented by 24, to reflect the fact that 24 half words */
            /* are consumed in every iteration. The input data pointer   */
            /* increments by 8. Note that within a stage, the stride     */
            /* does not change and hence the offsets for the other three */
            /* legs, 0, h2, l1, l2.                                      */
            /*-----------------------------------------------------------*/
            j += 24;
            x += 8;

            predj = (fft_jmp - j);
            if (!predj) x += fft_jmp;
            if (!predj) j = 0;
            if (!predj) Twiddles= (void*)tw;

            /*-----------------------------------------------------------*/
            /* First 2-Radix4's                                          */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_l2_1 - x_h2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_l2_3 - x_h2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh1_0_xh0_1_xh1_0_xh0_0     = _davg2(xl1_3210, x_3210);
            xh21_0_xh20_1_xh21_0_xh20_0 = _davg2(xl2_3210, xh2_3210);
            xl1_0_xl0_1_xl1_0_xl0_0     = _dshr2(_dssub2(x_3210, xl1_3210), 1);
            xl20_1_xl21_1_xl20_0_xl21_0 = _dcrot90(_dshr2(_dssub2(xl2_3210, xh2_3210), 1));

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_1 = xl0_1 + xl21_1;      yt2_1 = xl1_1 + xl20_1;    */
            /*   xt1_0 = xl0_0 + xl21_0;      yt2_0 = xl1_0 + xl20_0;    */
            /*                                                           */
            /*   xt0_0 = xh0_0 - xh20_0;      xt0_1 = xh0_1 - xh20_1;    */
            /*   yt0_0 = xh1_0 - xh21_0;      yt0_1 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_0 = xl0_0 - xl21_0;      xt2_1 = xl0_1 - xl21_1;    */
            /*   yt1_0 = xl1_0 - xl20_0;      yt1_1 = xl1_1 - xl20_1;    */
            /*-----------------------------------------------------------*/

            x_3o_x_2o_x_1o_x_0o     = _dsadd2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt2_1_xt1_1_yt2_0_xt1_0 = _dsadd2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);
            yt0_1_xt0_1_yt0_0_xt0_0 = _dssub2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt1_1_xt2_1_yt1_0_xt2_0 = _dssub2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);

            /*-----------------------------------------------------------*/
            /* Second 2-Radix4's                                         */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_l2_1 - x_h2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_l2_3 - x_h2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh1_0_xh0_1_xh1_0_xh0_0     = _davg2(xl1_7654, x_7654);
            xh21_0_xh20_1_xh21_0_xh20_0 = _davg2(xl2_7654, xh2_7654);
            xl1_0_xl0_1_xl1_0_xl0_0     = _dshr2(_dssub2(x_7654, xl1_7654), 1);
            xl20_1_xl21_1_xl20_0_xl21_0 = _dshr2(_dcrot90(_dssub2(xl2_7654, xh2_7654)), 1);

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_3 = xl0_1 + xl21_1;      yt2_3 = xl1_1 + xl20_1;    */
            /*   xt1_2 = xl0_0 + xl21_0;      yt2_2 = xl1_0 + xl20_0;    */
            /*                                                           */
            /*   xt0_2 = xh0_0 - xh20_0;      xt0_3 = xh0_1 - xh20_1;    */
            /*   yt0_2 = xh1_0 - xh21_0;      yt0_3 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_2 = xl0_0 - xl21_0;      xt2_3 = xl0_1 - xl21_1;    */
            /*   yt1_2 = xl1_0 - xl20_0;      yt1_3 = xl1_1 - xl20_1;    */
            /*-----------------------------------------------------------*/

            x_7o_x_6o_x_5o_x_4o     = _dsadd2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt2_3_xt1_3_yt2_2_xt1_2 = _dsadd2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);
            yt0_3_xt0_3_yt0_2_xt0_2 = _dssub2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt1_3_xt2_3_yt1_2_xt2_2 = _dssub2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);
   
            /*-----------------------------------------------------------*/
            /* Compute and apply twiddle factor for first 2-Radix4's     */
            /*-----------------------------------------------------------*/
            co2si2_1_co2si2_0 = _dcmpyr1(co11si11_co10si10, co11si11_co10si10);
            co3si3_1_co3si3_0 = _dcmpyr1(co11si11_co10si10, co2si2_1_co2si2_0);
            _amem8(&x2[0])    = x_3o_x_2o_x_1o_x_0o;
            _amem8(&x2[h2])   = _dcmpyr1(yt2_1_xt1_1_yt2_0_xt1_0, co11si11_co10si10);
            _amem8(&x2[l1])   = _dcmpyr1(yt0_1_xt0_1_yt0_0_xt0_0, co2si2_1_co2si2_0);
            _amem8(&x2[l2])   = _dcmpyr1(yt1_1_xt2_1_yt1_0_xt2_0, co3si3_1_co3si3_0);
   
            /*-----------------------------------------------------------*/
            /* Compute and apply twiddle factor for second 2-Radix4's    */
            /*-----------------------------------------------------------*/
            co2si2_1_co2si2_0 = _dcmpyr1(co13si13_co12si12, co13si13_co12si12);
            co3si3_1_co3si3_0 = _dcmpyr1(co13si13_co12si12, co2si2_1_co2si2_0);
            _amem8(&x2[4])    = x_7o_x_6o_x_5o_x_4o;
            _amem8(&x2[h2+4]) = _dcmpyr1(yt2_3_xt1_3_yt2_2_xt1_2, co13si13_co12si12);
            _amem8(&x2[l1+4]) = _dcmpyr1(yt0_3_xt0_3_yt0_2_xt0_2, co2si2_1_co2si2_0);
            _amem8(&x2[l2+4]) = _dcmpyr1(yt1_3_xt2_3_yt1_2_xt2_2, co3si3_1_co3si3_0);

        }
        /*  The stride quarters with every iteration of the outer loop   */
        stride >>= 2;
    }

    if (radix == 8) {
        /*---------------------------------------------------------------*/
        /*  Last stage of Radix4's is handled here                       */
        /*---------------------------------------------------------------*/
        x1 = ptr_x;
        tw = ptr_w + tw_offset;
        Twiddles = (void*)tw;
  
        /*---------------------------------------------------------------*/
        /* Load and compute twiddle factors                              */
        /*---------------------------------------------------------------*/
        co1si1_1_co1si1_0 = *Twiddles;
        co2si2_1_co2si2_0 = _dcmpyr1(co1si1_1_co1si1_0, co1si1_1_co1si1_0);
        co3si3_1_co3si3_0 = _dcmpyr1(co1si1_1_co1si1_0, co2si2_1_co2si2_0);

        _nassert(npoints >= 16);
        #pragma MUST_ITERATE(1,,1);
        for (i = 0; i < npoints; i += 16) {

            /*-----------------------------------------------------------*/
            /* Read in complex input for the butterflies.                */
            /*-----------------------------------------------------------*/
            x_3210   = _amem8(&x1[0]);
            xl1_3210 = _amem8(&x1[8]);
            xl2_3210 = _amem8(&x1[12]);
            xh2_3210 = _amem8(&x1[4]);
            x_7654   = _amem8(&x1[16]);
            xl1_7654 = _amem8(&x1[24]);
            xl2_7654 = _amem8(&x1[28]);
            xh2_7654 = _amem8(&x1[20]);

            /*-----------------------------------------------------------*/
            /* Derive output pointers using the input pointer "x"        */
            /*-----------------------------------------------------------*/
            x2 = (short *)_mvd((int)x1);

            /*-----------------------------------------------------------*/
            /* When the twiddle factors are not to be re-used, j is      */
            /* incremented by 12, to reflect the fact that 12 half words */
            /* are consumed in every iteration. The input data pointer   */
            /* increments by 4. Note that within a stage, the stride     */
            /* does not change and hence the offsets for the other three */
            /* legs, 0, h2, l1, l2.                                      */
            /*-----------------------------------------------------------*/
            x1 += 32;

            /*-----------------------------------------------------------*/
            /* First 2-Radix4's                                          */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_h2_1 - x_l2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_h2_3 - x_l2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh1_0_xh0_1_xh1_0_xh0_0     = _davg2(xl1_3210, x_3210);
            xh21_0_xh20_1_xh21_0_xh20_0 = _davg2(xl2_3210, xh2_3210);
            xl1_0_xl0_1_xl1_0_xl0_0     = _dshr2(_dssub2(x_3210, xl1_3210), 1);
            xl20_1_xl21_1_xl20_0_xl21_0 = _dshr2(_dcrot270(_dssub2(xl2_3210, xh2_3210)), 1);

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_3 = xl0_1 - xl21_1;      yt2_3 = xl1_1 - xl20_1;    */
            /*   xt1_2 = xl0_0 - xl21_0;      yt2_2 = xl1_0 - xl20_0;    */
            /*                                                           */
            /*   xt0_2 = xh0_0 - xh20_0;      xt0_3 = xh0_1 - xh20_1;    */
            /*   yt0_2 = xh1_0 - xh21_0;      yt0_3 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_2 = xl0_0 + xl21_0;      xt2_3 = xl0_1 + xl21_1;    */
            /*   yt1_2 = xl1_0 + xl20_0;      yt1_3 = xl1_1 + xl20_1;    */
            /*-----------------------------------------------------------*/

            x_3o_x_2o_x_1o_x_0o     = _dsadd2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt2_1_xt1_1_yt2_0_xt1_0 = _dssub2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);
            yt0_1_xt0_1_yt0_0_xt0_0 = _dssub2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt1_1_xt2_1_yt1_0_xt2_0 = _dsadd2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);

             /*-----------------------------------------------------------*/
            /* Second 2-Radix4's                                         */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_h2_1 - x_l2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_h2_3 - x_l2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh1_0_xh0_1_xh1_0_xh0_0     = _davg2(xl1_7654, x_7654);
            xh21_0_xh20_1_xh21_0_xh20_0 = _davg2(xl2_7654, xh2_7654);
            xl1_0_xl0_1_xl1_0_xl0_0     = _dshr2(_dssub2(x_7654, xl1_7654), 1);
            xl20_1_xl21_1_xl20_0_xl21_0 = _dshr2(_dcrot270(_dssub2(xl2_7654, xh2_7654)), 1);

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_3 = xl0_1 - xl21_1;      yt2_3 = xl1_1 - xl20_1;    */
            /*   xt1_2 = xl0_0 - xl21_0;      yt2_2 = xl1_0 - xl20_0;    */
            /*                                                           */
            /*   xt0_2 = xh0_0 - xh20_0;      xt0_3 = xh0_1 - xh20_1;    */
            /*   yt0_2 = xh1_0 - xh21_0;      yt0_3 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_2 = xl0_0 + xl21_0;      xt2_3 = xl0_1 + xl21_1;    */
            /*   yt1_2 = xl1_0 + xl20_0;      yt1_3 = xl1_1 + xl20_1;    */
            /*-----------------------------------------------------------*/

            x_7o_x_6o_x_5o_x_4o     = _dsadd2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt2_3_xt1_3_yt2_2_xt1_2 = _dssub2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);
            yt0_3_xt0_3_yt0_2_xt0_2 = _dssub2(xh1_0_xh0_1_xh1_0_xh0_0, xh21_0_xh20_1_xh21_0_xh20_0);
            yt1_3_xt2_3_yt1_2_xt2_2 = _dsadd2(xl1_0_xl0_1_xl1_0_xl0_0, xl20_1_xl21_1_xl20_0_xl21_0);
   
            /*-----------------------------------------------------------*/
            /* apply twiddle factor for first 2-Radix4's                 */
            /*-----------------------------------------------------------*/
            _amem8(&x2[0])  = x_3o_x_2o_x_1o_x_0o;
            _amem8(&x2[4])  = _dcmpyr1(yt2_1_xt1_1_yt2_0_xt1_0, co1si1_1_co1si1_0);
            _amem8(&x2[8])  = _dcmpyr1(yt0_1_xt0_1_yt0_0_xt0_0, co2si2_1_co2si2_0);
            _amem8(&x2[12]) = _dcmpyr1(yt1_1_xt2_1_yt1_0_xt2_0, co3si3_1_co3si3_0);

            /*-----------------------------------------------------------*/
            /* apply twiddle factor for second 2-Radix4's                */
            /*-----------------------------------------------------------*/
            _amem8(&x2[16]) = x_7o_x_6o_x_5o_x_4o;
            _amem8(&x2[20]) = _dcmpyr1(yt2_3_xt1_3_yt2_2_xt1_2, co1si1_1_co1si1_0);
            _amem8(&x2[24]) = _dcmpyr1(yt0_3_xt0_3_yt0_2_xt0_2, co2si2_1_co2si2_0);
            _amem8(&x2[28]) = _dcmpyr1(yt1_3_xt2_3_yt1_2_xt2_2, co3si3_1_co3si3_0);
           }
        radix_2(ptr_x, ptr_y, npoints);
    }
    else if (radix == 4) {
        radix_4(ptr_x, ptr_y, npoints);
    }
    return;
}

void radix_2 (
    short *restrict ptr_x,
    short *restrict ptr_y,
    int npoints
)
{
    short *restrict x0;
    short *restrict y0, *restrict y1, *restrict y2, *restrict y3;
    int i, j, k, n0, h2, l1, l2;
    int x_10, x_32, x_54, x_76, x_98, x_BA, x_DC, x_FE;
    long long x_3210, x_7654, x_BA98, x_FEDC;
    long long y_10_y_54, y_98_y_DC, y_BA_y_FE, y_32_y_76;

    /*-----------------------------------------------------------------*/
    /* The following code performs either a standard radix2 pass       */
    /*-----------------------------------------------------------------*/
    y0 = ptr_y;
    y2 = ptr_y + (int)npoints;
    x0 = ptr_x;
    l2 = (int)(npoints >> 1);

    /*-----------------------------------------------------------------*/
    /* The pointers are set at the following locations which are half  */
    /* the offsets of a radix4 FFT.                                    */
    /*-----------------------------------------------------------------*/
    y1 = y0 + (int)(npoints >> 2);
    y3 = y2 + (int)(npoints >> 2);
    l1 = _norm(npoints) + 2;
    n0 = npoints >> 1;
    j = 0;

    _nassert((int)(n0) % 4  == 0);
    _nassert((int)(ptr_x) % 8 == 0);
    _nassert((int)(ptr_y) % 8 == 0);
    _nassert((int)(x0) % 8 == 0);
    _nassert((int)(y0) % 8 == 0);
    _nassert(npoints >= 16);

    /*-------------------------------------------------------------------*/
    /* Loop twice for Top and bottom half of radix2                      */
    /*-------------------------------------------------------------------*/
    for (k = 0; k < 2; k++, j += n0, x0 += n0) {

        #pragma MUST_ITERATE(1,,1);
        for (i = 0; i < n0; i += 8) {

            /*---------------------------------------------------------------*/
            /* The following code taks an index "j" and generates the bit    */
            /* reversed index. It does through the "deal", "bitr" and "shfl" */
            /* instructions. The shift by "l1" is used to return the result  */
            /* in the right number of bits depending on the size of the FFT  */
            /*---------------------------------------------------------------*/
            h2 = _deal(j);
            h2 = _bitr(h2);
            h2 = _rotl(h2, 16);
            h2 = _shfl(h2);
            h2 >>= l1;

            /*-------------------------------------------------------------*/
            /* Load 8 data values from the top and middle halves using the */
            /* _amemd8 intrinsic. These can be accessed using the _lo and  */
            /* _hi intrinsic.                                              */
            /*-------------------------------------------------------------*/
            x_3210 = _amem8(&x0[0]);
            x_7654 = _amem8(&x0[4]);
            x_BA98 = _amem8(&x0[l2]);
            x_FEDC = _amem8(&x0[l2+4]);

            x_10 = _loll(x_3210);
            x_32 = _hill(x_3210);
            x_54 = _loll(x_7654);
            x_76 = _hill(x_7654);
            x_98 = _loll(x_BA98);
            x_BA = _hill(x_BA98);
            x_DC = _loll(x_FEDC);
            x_FE = _hill(x_FEDC);

            /*-------------------------------------------------------------*/
            /* radix = 2  y0 = x0 + x2                                     */
            /*            y1 = x1 + x3                                     */
            /*            y4 = x0 - x2                                     */
            /*            y5 = x1 - x3                                     */
            /*-------------------------------------------------------------*/
            y_10_y_54 = _addsub2(x_10, x_32);

            /*-------------------------------------------------------------*/
            /* radix = 2  y2 = x4 + x6                                     */
            /*            y3 = x5 + x7                                     */
            /*            y6 = x4 - x6                                     */
            /*            y7 = x5 - x7                                     */
            /*-------------------------------------------------------------*/
            y_32_y_76 = _addsub2(x_54, x_76);

            /*-------------------------------------------------------------*/
            /* radix = 2  y8 = x8 + xa                                     */
            /*            y9 = x9 + xb                                     */
            /*            yc = x8 - xa                                     */
            /*            yd = x9 - xb                                     */
            /*-------------------------------------------------------------*/
            y_98_y_DC = _addsub2(x_98, x_BA);

            /*-------------------------------------------------------------*/
            /* radix = 2  ya = xc + xe                                     */
            /*            yb = xd + xf                                     */
            /*            ye = xc - xe                                     */
            /*            yf = xd - xf                                     */
            /*-------------------------------------------------------------*/
            y_BA_y_FE = _addsub2(x_DC, x_FE);

            /*-------------------------------------------------------------*/
            /*  Store out the results of all four butterflies as double    */
            /*  words.                                                     */
            /*-------------------------------------------------------------*/
            _amem8(&y0[4 * h2]) = _itoll(_hill(y_98_y_DC), _hill(y_10_y_54));
            _amem8(&y1[4 * h2]) = _itoll(_hill(y_BA_y_FE), _hill(y_32_y_76));
            _amem8(&y2[4 * h2]) = _itoll(_loll(y_98_y_DC), _loll(y_10_y_54));
            _amem8(&y3[4 * h2]) = _itoll(_loll(y_BA_y_FE), _loll(y_32_y_76));

            j  += 8;
            x0 += 8;
        }
    }
}

void radix_4 (
    short *restrict ptr_x,
    short *restrict ptr_y,
    int npoints
)
{
    short *restrict x0, *restrict x1;
    short *restrict y0, *restrict y1, *restrict y2, *restrict y3;
    int n0, j0, j1, i, h0, h1, l1, l2;
    long long x0_3210, x0_7654, x0_BA98, x0_FEDC;
    long long x1_3210, x1_7654, x1_BA98, x1_FEDC;
    long long x0l21_0_l20_0_l1_0_l0_0, x1l21_0_l20_0_l1_0_l0_0;
    long long x0h21_0_h20_0_h1_0_h0_0, x1h21_0_h20_0_h1_0_h0_0;
    long long x0l21_1_l20_1_l1_1_l0_1, x1l21_1_l20_1_l1_1_l0_1;
    long long x0h21_1_h20_1_h1_1_h0_1, x1h21_1_h20_1_h1_1_h0_1;
    long long y0_10_54, y0_98_DC, y0_32_76, y0_BA_FE;
    long long y1_10_54, y1_98_DC, y1_32_76, y1_BA_FE;

    /*-----------------------------------------------------------------*/
    /* The following code performs either a standard radix4 pass       */
    /*-----------------------------------------------------------------*/
    y0 = ptr_y;
    y2 = ptr_y + (int)npoints;
    y1 = y0 + (int)(npoints >> 1);
    y3 = y2 + (int)(npoints >> 1);

    x0 = ptr_x;
    x1 = ptr_x + (int)npoints;
    l1 = _norm(npoints) + 3;
    l2 = (int)(npoints >> 1);
    n0 = npoints >> 2;
    j0 = 0;
    j1 = (int)(npoints >> 1);

    _nassert((int)(n0) % 4 == 0);
    _nassert((int)(x0) % 8 == 0);
    _nassert((int)(y0) % 8 == 0);
    _nassert(npoints >= 16);
    #pragma MUST_ITERATE(1,,1);

    /*-------------------------------------------------------------------*/
    /* Perform top half and Bottom half of radix4 in a single iteration  */
    /*-------------------------------------------------------------------*/
    for (i = 0; i < l2; i += 8) {
        /*---------------------------------------------------------------*/
        /* The following code taks an index "j" and generates the bit    */
        /* reversed index. It does through the "deal", "bitr" and "shfl" */
        /* instructions. The shift by "l1" is used to return the result  */
        /* in the right number of bits depending on the size of the FFT  */
        /*---------------------------------------------------------------*/
        h0 = _deal(j0);
        h0 = _bitr(h0);
        h0 = _rotl(h0, 16);
        h0 = _shfl(h0);
        h0 >>= l1;

        h1 = _deal(j1);
        h1 = _bitr(h1);
        h1 = _rotl(h1, 16);
        h1 = _shfl(h1);
        h1 >>= l1;

        /*---------------------------------------------------------------*/
        /* Load 4 radix-4 data values                                    */
        /*---------------------------------------------------------------*/
        x0_3210 = _amem8(&x0[0]);
        x0_7654 = _amem8(&x0[4]);
        x0_BA98 = _amem8(&x0[l2]);
        x0_FEDC = _amem8(&x0[l2+4]);

        x1_3210 = _amem8(&x1[0]);
        x1_7654 = _amem8(&x1[4]);
        x1_BA98 = _amem8(&x1[l2]);
        x1_FEDC = _amem8(&x1[l2+4]);

        /*------------------------------------------------------------*/
        /*  h1_0  = x_1 + x_5;        h0_0  = x_0 + x_4;              */
        /*  h21_0 = x_3 + x_7;        h20_0 = x_2 + x_6;              */
        /*  l1_0  = x_1 - x_5;        l0_0  = x_0 - x_4;              */
        /*  l21_0 = x_3 - x_7;        l20_0 = x_2 - x_6;              */
        /*------------------------------------------------------------*/
        x0h21_0_h20_0_h1_0_h0_0 = _dsadd2(x0_3210, x0_7654);
        x0l21_0_l20_0_l1_0_l0_0 = _dssub2(x0_3210, x0_7654);

        x1h21_0_h20_0_h1_0_h0_0 = _dsadd2(x1_3210, x1_7654);
        x1l21_0_l20_0_l1_0_l0_0 = _dssub2(x1_3210, x1_7654);

        /*------------------------------------------------------------*/
        /*  y0 = h0_0 + h20_0 = x_0 + x_4 + x_2 + x_6                 */
        /*  y1 = h1_0 + h21_0 = x_1 + x_5 + x_3 + x_7                 */
        /*  y4 = h0_0 - h21_0 = x_0 + x_4 - x_2 - x_6                 */
        /*  y5 = h1_0 - h20_0 = x_1 + x_5 - x_3 - x_7                 */
        /*------------------------------------------------------------*/
        y0_10_54 = _addsub2(_loll(x0h21_0_h20_0_h1_0_h0_0),
                            _hill(x0h21_0_h20_0_h1_0_h0_0));

        y1_10_54 = _addsub2(_loll(x1h21_0_h20_0_h1_0_h0_0),
                            _hill(x1h21_0_h20_0_h1_0_h0_0));

        /*------------------------------------------------------------*/
        /*  y6 = l0_0 + l21_0 = x_0 - x_4 + x_3 - x_7                 */
        /*  y7 = l1_0 - l20_0 = x_1 - x_5 - x_2 + x_6                 */
        /*  y2 = l0_0 - l21_0 = x_0 - x_4 - x_3 + x_7                 */
        /*  y3 = l1_0 + l20_0 = x_1 - x_5 + x_2 - x_6                 */
        /*------------------------------------------------------------*/
        y0_32_76 = _addsub2(_loll(x0l21_0_l20_0_l1_0_l0_0), 
                   _crot270(_hill(x0l21_0_l20_0_l1_0_l0_0)));

        y1_32_76 = _addsub2(_loll(x1l21_0_l20_0_l1_0_l0_0), 
                   _crot270(_hill(x1l21_0_l20_0_l1_0_l0_0)));

        /*------------------------------------------------------------*/
        /*  h0_1  = x_8 + x_c;        h1_1  = x_9 + x_d;              */
        /*  h20_1 = x_a + x_e;        h21_1 = x_b + x_f;              */
        /*  l0_1  = x_8 - x_c;        l1_1  = x_9 - x_d;              */
        /*  l20_1 = x_a - x_e;        l21_1 = x_b - x_f;              */
        /*------------------------------------------------------------*/
        x0h21_1_h20_1_h1_1_h0_1 = _dsadd2(x0_BA98, x0_FEDC);
        x0l21_1_l20_1_l1_1_l0_1 = _dssub2(x0_BA98, x0_FEDC);

        x1h21_1_h20_1_h1_1_h0_1 = _dsadd2(x1_BA98, x1_FEDC);
        x1l21_1_l20_1_l1_1_l0_1 = _dssub2(x1_BA98, x1_FEDC);

        /*------------------------------------------------------------*/
        /*  y8 = h0_1 + h20_1 = x_8 + x_c + x_a + x_e                 */
        /*  y9 = h1_1 + h21_1 = x_9 + x_d + x_b + x_f                 */
        /*  yc = h0_1 - h21_1 = x_8 + x_c - x_a - x_e                 */
        /*  yd = h1_1 - h20_1 = x_9 + x_d - x_b - x_f                 */
        /*------------------------------------------------------------*/
        y0_98_DC = _addsub2(_loll(x0h21_1_h20_1_h1_1_h0_1),
                            _hill(x0h21_1_h20_1_h1_1_h0_1));

        y1_98_DC = _addsub2(_loll(x1h21_1_h20_1_h1_1_h0_1),
                            _hill(x1h21_1_h20_1_h1_1_h0_1));

        /*------------------------------------------------------------*/
        /*  ya = l0_1 + l21_1 = x_8 - x_c + x_b - x_f                 */
        /*  yb = l1_1 - l20_1 = x_9 - x_d - x_a + x_e                 */
        /*  ye = l0_1 - l21_1 = x_8 - x_c - x_b + x_f                 */
        /*  yf = l1_1 + l20_1 = x_9 - x_d + x_a - x_e                 */
        /*------------------------------------------------------------*/
        y0_BA_FE = _addsub2(_loll(x0l21_1_l20_1_l1_1_l0_1), 
                   _crot270(_hill(x0l21_1_l20_1_l1_1_l0_1)));

        y1_BA_FE = _addsub2(_loll(x1l21_1_l20_1_l1_1_l0_1), 
                   _crot270(_hill(x1l21_1_l20_1_l1_1_l0_1)));

        /*------------------------------------------------------------*/
        /*  Store the results of all 4 butterflies as double words.   */
        /*------------------------------------------------------------*/
        _amem8(&y0[4 * h0]) = _itoll(     _hill(y0_98_DC) , _mvd(_hill(y0_10_54)));
        _amem8(&y1[4 * h0]) = _itoll(     _hill(y0_BA_FE) , _mvd(_hill(y0_32_76)));
        _amem8(&y2[4 * h0]) = _itoll(_mvd(_loll(y0_98_DC)),      _loll(y0_10_54));
        _amem8(&y3[4 * h0]) = _itoll(_mvd(_loll(y0_BA_FE)),      _loll(y0_32_76));
        _amem8(&y0[4 * h1]) = _itoll(     _hill(y1_98_DC) , _mvd(_hill(y1_10_54)));
        _amem8(&y1[4 * h1]) = _itoll(     _hill(y1_BA_FE) , _mvd(_hill(y1_32_76)));
        _amem8(&y2[4 * h1]) = _itoll(_mvd(_loll(y1_98_DC)),      _loll(y1_10_54));
        _amem8(&y3[4 * h1]) = _itoll(_mvd(_loll(y1_BA_FE)),      _loll(y1_32_76));

        j0 += 4;
        j1 += 4;
        x0 += 8;
        x1 += 8;
    }
}
#else
void DSP_ifft16x16 (
    const short * restrict ptr_w,
    int npoints,
    short * restrict ptr_x,
    short * restrict ptr_y
)
{
    short * restrict x, * restrict x1, * restrict x2;
    const short * restrict tw;
    long long *restrict Twiddles;

    long long x_0123, x_4567, xl2_0123, xl2_4567, xl1_0123, xl1_4567, xh2_0123, xh2_4567;
    long long co10si10_co11si11, co12si12_co13si13, co12si12_co13si13_tmp;
    long long co1si1_0_co1si1_1, co2si2_0_co2si2_1, co3si3_0_co3si3_1;
    long long xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1;
    long long xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1;
    long long x_0o_x_1o_x_2o_x_3o, xt1_0_yt2_0_xt1_1_yt2_1, xt0_0_yt0_0_xt0_1_yt0_1, xt2_0_yt1_0_xt2_1_yt1_1;
    long long x_4o_x_5o_x_6o_x_7o, xt1_2_yt2_2_xt1_3_yt2_3, xt0_2_yt0_2_xt0_3_yt0_3, xt2_2_yt1_2_xt2_3_yt1_3;  

    int j, i, l1, l2, h2, fft_jmp, tw_jmp;
    int radix, predj, tw_offset, stride;

    /*----------------------------------------------------------------------*/
    /* Determine the magnitude of the number of points to be transformed.   */
    /* Check whether we can use a radix4 decomposition or a mixed radix     */
    /* transformation, by determining modulo 2.                             */
    /*----------------------------------------------------------------------*/
    radix = _norm(npoints) & 1 ? 8 : 4;

    /*----------------------------------------------------------------------*/
    /* The stride is quartered with every iteration of the outer loop. It   */
    /* denotes the seperation between any two adjacent inputs to the butter */
    /* -fly. This should start out at N/4, hence stride is initially set to */
    /* N. For every stride, 2*stride twiddle factors are accessed. The      */
    /* "tw_offset" is the offset within the current twiddle factor sub-     */
    /* table. This is set to zero, at the start of the code and is used to  */
    /* obtain the appropriate sub-table twiddle pointer by offseting it     */
    /* with the base pointer "ptr_w".                                       */
    /*----------------------------------------------------------------------*/
    stride  = npoints;
    fft_jmp = 6 * stride;
    tw_jmp  = 2 * stride;
    tw_offset = 0;

    _nassert(stride > 8);
    #pragma MUST_ITERATE(1,,1);

    while (stride > 8) {
        /*------------------------------------------------------------------*/
        /* At the start of every iteration of the outer loop, "j" is set    */
        /* to zero, as "tw" is pointing to the correct location within the  */
        /* twiddle factor array. For every iteration of the inner loop      */
        /* 2 * stride twiddle factors are accessed. For eg,                 */
        /*                                                                  */
        /* #Iteration of outer loop  # twiddle factors    #times cycled     */
        /*  1                          2 N/4               1                */
        /*  2                          2 N/16              4                */
        /*  ...                                                             */
        /*------------------------------------------------------------------*/
        j = 0;
        fft_jmp >>= 2;
        tw_jmp  >>= 2;

        /*------------------------------------------------------------------*/
        /* Set up offsets to access "N/4", "N/2", "3N/4" complex point or   */
        /* "N/2", "N", "3N/2" half word                                     */
        /*------------------------------------------------------------------*/
        h2 = stride >> 1;
        l1 = stride;
        l2 = stride + (stride >> 1);

        /*------------------------------------------------------------------*/
        /*  Reset "x" to point to the start of the input data array.        */
        /* "tw_offset" starts off at 0, and increments by "2 * stride"      */
        /*------------------------------------------------------------------*/
        x  = ptr_x;
        tw = ptr_w + tw_offset;
        Twiddles = (void*)tw;
        tw_offset += tw_jmp;

        /*----------------------------------------------------------------*/
        /* The following loop iterates through the different butterflies, */
        /* within a given stage. Recall that there are logN to base 4     */
        /* stages. Certain butterflies share the twiddle factors. These   */
        /* are grouped together. On the very first stage there are no     */
        /* butterflies that share the twiddle factor, all N/4 butter-     */
        /* flies have different factors. On the next stage two sets of    */
        /* N/8 butterflies share the same twiddle factor. Hence after     */
        /* half the butterflies are performed, j the index into the       */
        /* factor array resets to 0, and the twiddle factors are reused.  */
        /* When this happens, the data pointer 'x' is incremented by the  */
        /* fft_jmp amount. In addition the following code is unrolled to  */
        /* perform "4" radix4 butterflies in parallel.                    */
        /*----------------------------------------------------------------*/

        _nassert(npoints >= 16);
        #pragma MUST_ITERATE(1,,1);
        for (i = 0; i < npoints; i += 16) {

            /*-----------------------------------------------------------*/
            /* Read in complex input for the butterflies.                */
            /*-----------------------------------------------------------*/
            x_0123   = _amem8(&x[0]);
            x_4567   = _amem8(&x[4]);
            xl1_0123 = _amem8(&x[l1]);
            xl1_4567 = _amem8(&x[l1+4]);
            xl2_0123 = _amem8(&x[l2]);
            xl2_4567 = _amem8(&x[l2+4]);
            xh2_0123 = _amem8(&x[h2]);
            xh2_4567 = _amem8(&x[h2+4]);

            /*-----------------------------------------------------------*/
            /* Derive output pointers using the input pointer "x"        */
            /*-----------------------------------------------------------*/
            x2 = (short *)_mvd((int)x);

            /*-----------------------------------------------------------*/
            /* Read in complex input for twiddle factors.                */
            /*-----------------------------------------------------------*/
            co10si10_co11si11 = *Twiddles++;
            co12si12_co13si13_tmp = *Twiddles++;
            co12si12_co13si13 = _dmvd(_hill(co12si12_co13si13_tmp), _loll(co12si12_co13si13_tmp));

            /*-----------------------------------------------------------*/
            /* When the twiddle factors are not to be re-used, j is      */
            /* incremented by 24, to reflect the fact that 24 half words */
            /* are consumed in every iteration. The input data pointer   */
            /* increments by 8. Note that within a stage, the stride     */
            /* does not change and hence the offsets for the other three */
            /* legs, 0, h2, l1, l2.                                      */
            /*-----------------------------------------------------------*/
            j += 24;
            x += 8;

            predj = (fft_jmp - j);
            if (!predj) x += fft_jmp;
            if (!predj) j = 0;
            if (!predj) Twiddles= (void*)tw;

            /*-----------------------------------------------------------*/
            /* First 2-Radix4's                                          */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_l2_1 - x_h2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_l2_3 - x_h2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/
		
            xh0_0_xh1_0_xh0_1_xh1_1     = _davg2(xl1_0123, x_0123);
            xh20_0_xh21_0_xh20_1_xh21_1 = _davg2(xl2_0123, xh2_0123);
            xl0_0_xl1_0_xl0_1_xl1_1     = _dshr2(_dssub2(x_0123, xl1_0123), 1);
            xl21_0_xl20_0_xl21_1_xl20_1 = _dcrot270(_dshr2(_dssub2(xl2_0123, xh2_0123), 1));

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_1 = xl0_1 + xl21_1;      yt2_1 = xl1_1 + xl20_1;    */
            /*   xt1_0 = xl0_0 + xl21_0;      yt2_0 = xl1_0 + xl20_0;    */
            /*                                                           */
            /*   xt0_0 = xh0_0 - xh20_0;      xt0_1 = xh0_1 - xh20_1;    */
            /*   yt0_0 = xh1_0 - xh21_0;      yt0_1 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_0 = xl0_0 - xl21_0;      xt2_1 = xl0_1 - xl21_1;    */
            /*   yt1_0 = xl1_0 - xl20_0;      yt1_1 = xl1_1 - xl20_1;    */
            /*-----------------------------------------------------------*/

            x_0o_x_1o_x_2o_x_3o     = _dsadd2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt1_0_yt2_0_xt1_1_yt2_1 = _dsadd2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);
            xt0_0_yt0_0_xt0_1_yt0_1 = _dssub2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt2_0_yt1_0_xt2_1_yt1_1 = _dssub2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);

            /*-----------------------------------------------------------*/
            /* Second 2-Radix4's                                         */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_l2_1 - x_h2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_l2_3 - x_h2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh0_0_xh1_0_xh0_1_xh1_1     = _davg2(xl1_4567, x_4567);
            xh20_0_xh21_0_xh20_1_xh21_1 = _davg2(xl2_4567, xh2_4567);
            xl0_0_xl1_0_xl0_1_xl1_1     = _dshr2(_dssub2(x_4567, xl1_4567), 1);
            xl21_0_xl20_0_xl21_1_xl20_1 = _dshr2(_dcrot270(_dssub2(xl2_4567, xh2_4567)), 1);

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_3 = xl0_1 + xl21_1;      yt2_3 = xl1_1 + xl20_1;    */
            /*   xt1_2 = xl0_0 + xl21_0;      yt2_2 = xl1_0 + xl20_0;    */
            /*                                                           */
            /*   xt0_2 = xh0_0 - xh20_0;      xt0_3 = xh0_1 - xh20_1;    */
            /*   yt0_2 = xh1_0 - xh21_0;      yt0_3 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_2 = xl0_0 - xl21_0;      xt2_3 = xl0_1 - xl21_1;    */
            /*   yt1_2 = xl1_0 - xl20_0;      yt1_3 = xl1_1 - xl20_1;    */
            /*-----------------------------------------------------------*/

            x_4o_x_5o_x_6o_x_7o     = _dsadd2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt1_2_yt2_2_xt1_3_yt2_3 = _dsadd2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);
            xt0_2_yt0_2_xt0_3_yt0_3 = _dssub2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt2_2_yt1_2_xt2_3_yt1_3 = _dssub2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);
   
            /*-----------------------------------------------------------*/
            /* Compute and apply twiddle factor for first 2-Radix4's     */
            /*-----------------------------------------------------------*/
            co2si2_0_co2si2_1 = _dcmpyr1(co10si10_co11si11, co10si10_co11si11);
            co3si3_0_co3si3_1 = _dcmpyr1(co10si10_co11si11, co2si2_0_co2si2_1);
            _amem8(&x2[0])    = x_0o_x_1o_x_2o_x_3o;
            _amem8(&x2[h2])   = _dcmpyr1(xt1_0_yt2_0_xt1_1_yt2_1, co10si10_co11si11);
            _amem8(&x2[l1])   = _dcmpyr1(xt0_0_yt0_0_xt0_1_yt0_1, co2si2_0_co2si2_1);
            _amem8(&x2[l2])   = _dcmpyr1(xt2_0_yt1_0_xt2_1_yt1_1, co3si3_0_co3si3_1);
   
            /*-----------------------------------------------------------*/
            /* Compute and apply twiddle factor for second 2-Radix4's    */
            /*-----------------------------------------------------------*/
            co2si2_0_co2si2_1 = _dcmpyr1(co12si12_co13si13, co12si12_co13si13);
            co3si3_0_co3si3_1 = _dcmpyr1(co12si12_co13si13, co2si2_0_co2si2_1);
            _amem8(&x2[4])    = x_4o_x_5o_x_6o_x_7o;
            _amem8(&x2[h2+4]) = _dcmpyr1(xt1_2_yt2_2_xt1_3_yt2_3, co12si12_co13si13);
            _amem8(&x2[l1+4]) = _dcmpyr1(xt0_2_yt0_2_xt0_3_yt0_3, co2si2_0_co2si2_1);
            _amem8(&x2[l2+4]) = _dcmpyr1(xt2_2_yt1_2_xt2_3_yt1_3, co3si3_0_co3si3_1);

        }
        /*  The stride quarters with every iteration of the outer loop   */
        stride >>= 2;
    }

    if (radix == 8) {
        /*---------------------------------------------------------------*/
        /*  Last stage of Radix4's is handled here                       */
        /*---------------------------------------------------------------*/
        x1 = ptr_x;
        tw = ptr_w + tw_offset;
        Twiddles = (void*)tw;
  
        /*---------------------------------------------------------------*/
        /* Load and compute twiddle factors                              */
        /*---------------------------------------------------------------*/
        co1si1_0_co1si1_1 = *Twiddles;
        co2si2_0_co2si2_1 = _dcmpyr1(co1si1_0_co1si1_1, co1si1_0_co1si1_1);
        co3si3_0_co3si3_1 = _dcmpyr1(co1si1_0_co1si1_1, co2si2_0_co2si2_1);

        _nassert(npoints >= 16);
        #pragma MUST_ITERATE(1,,1);
        for (i = 0; i < npoints; i += 16) {

            /*-----------------------------------------------------------*/
            /* Read in complex input for the butterflies.                */
            /*-----------------------------------------------------------*/
            x_0123   = _amem8(&x1[0]);
            xl1_0123 = _amem8(&x1[8]);
            xl2_0123 = _amem8(&x1[12]);
            xh2_0123 = _amem8(&x1[4]);
            x_4567   = _amem8(&x1[16]);
            xl1_4567 = _amem8(&x1[24]);
            xl2_4567 = _amem8(&x1[28]);
            xh2_4567 = _amem8(&x1[20]);

            /*-----------------------------------------------------------*/
            /* Derive output pointers using the input pointer "x"        */
            /*-----------------------------------------------------------*/
            x2 = (short *)_mvd((int)x1);

            /*-----------------------------------------------------------*/
            /* When the twiddle factors are not to be re-used, j is      */
            /* incremented by 12, to reflect the fact that 12 half words */
            /* are consumed in every iteration. The input data pointer   */
            /* increments by 4. Note that within a stage, the stride     */
            /* does not change and hence the offsets for the other three */
            /* legs, 0, h2, l1, l2.                                      */
            /*-----------------------------------------------------------*/
            x1 += 32;

            /*-----------------------------------------------------------*/
            /* First 2-Radix4's                                          */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_h2_1 - x_l2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_h2_3 - x_l2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh0_0_xh1_0_xh0_1_xh1_1     = _davg2(xl1_0123, x_0123);
            xh20_0_xh21_0_xh20_1_xh21_1 = _davg2(xl2_0123, xh2_0123);
            xl0_0_xl1_0_xl0_1_xl1_1     = _dshr2(_dssub2(x_0123, xl1_0123), 1);
            xl21_0_xl20_0_xl21_1_xl20_1 = _dshr2(_dcrot90(_dssub2(xl2_0123, xh2_0123)), 1);

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_3 = xl0_1 - xl21_1;      yt2_3 = xl1_1 - xl20_1;    */
            /*   xt1_2 = xl0_0 - xl21_0;      yt2_2 = xl1_0 - xl20_0;    */
            /*                                                           */
            /*   xt0_2 = xh0_0 - xh20_0;      xt0_3 = xh0_1 - xh20_1;    */
            /*   yt0_2 = xh1_0 - xh21_0;      yt0_3 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_2 = xl0_0 + xl21_0;      xt2_3 = xl0_1 + xl21_1;    */
            /*   yt1_2 = xl1_0 + xl20_0;      yt1_3 = xl1_1 + xl20_1;    */
            /*-----------------------------------------------------------*/

            x_0o_x_1o_x_2o_x_3o     = _dsadd2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt1_0_yt2_0_xt1_1_yt2_1 = _dssub2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);
            xt0_0_yt0_0_xt0_1_yt0_1 = _dssub2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt2_0_yt1_0_xt2_1_yt1_1 = _dsadd2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);

             /*-----------------------------------------------------------*/
            /* Second 2-Radix4's                                         */
            /*                                                           */
            /*   xh0_0 = x_0 + x_l1_0;        xh1_0 = x_1 + x_l1_1;      */
            /*   xh0_1 = x_2 + x_l1_2;        xh1_1 = x_3 + x_l1_3;      */
            /*                                                           */
            /*   xh20_0 = x_h2_0 + x_l2_0;    xh21_0 = x_h2_1 + x_l2_1;  */
            /*   xh20_1 = x_h2_2 + x_l2_2;    xh21_1 = x_h2_3 + x_l2_3;  */
            /*                                                           */
            /*   xl0_0 = x_0 - x_l1_0;        xl1_0 = x_1 - x_l1_1;      */
            /*   xl0_1 = x_2 - x_l1_2;        xl1_1 = x_3 - x_l1_3;      */
            /*                                                           */
            /*   xl20_0 = x_h2_0 - x_l2_0;    xl21_0 = x_h2_1 - x_l2_1;  */
            /*   xl20_1 = x_h2_2 - x_l2_2;    xl21_1 = x_h2_3 - x_l2_3;  */
            /*                                                           */
            /*-----------------------------------------------------------*/

            xh0_0_xh1_0_xh0_1_xh1_1     = _davg2(xl1_4567, x_4567);
            xh20_0_xh21_0_xh20_1_xh21_1 = _davg2(xl2_4567, xh2_4567);
            xl0_0_xl1_0_xl0_1_xl1_1     = _dshr2(_dssub2(x_4567, xl1_4567), 1);
            xl21_0_xl20_0_xl21_1_xl20_1 = _dshr2(_dcrot90(_dssub2(xl2_4567, xh2_4567)), 1);

            /*-----------------------------------------------------------*/
            /*   x0[0] = xh0_0 + xh20_0;      x0[2] = xh0_1 + xh20_1;    */
            /*   x0[1] = xh1_0 + xh21_0;      x0[3] = xh1_1 + xh21_1;    */
            /*                                                           */
            /*   xt1_3 = xl0_1 - xl21_1;      yt2_3 = xl1_1 - xl20_1;    */
            /*   xt1_2 = xl0_0 - xl21_0;      yt2_2 = xl1_0 - xl20_0;    */
            /*                                                           */
            /*   xt0_2 = xh0_0 - xh20_0;      xt0_3 = xh0_1 - xh20_1;    */
            /*   yt0_2 = xh1_0 - xh21_0;      yt0_3 = xh1_1 - xh21_1;    */
            /*                                                           */
            /*   xt2_2 = xl0_0 + xl21_0;      xt2_3 = xl0_1 + xl21_1;    */
            /*   yt1_2 = xl1_0 + xl20_0;      yt1_3 = xl1_1 + xl20_1;    */
            /*-----------------------------------------------------------*/

            x_4o_x_5o_x_6o_x_7o     = _dsadd2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt1_2_yt2_2_xt1_3_yt2_3 = _dssub2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);
            xt0_2_yt0_2_xt0_3_yt0_3 = _dssub2(xh0_0_xh1_0_xh0_1_xh1_1, xh20_0_xh21_0_xh20_1_xh21_1);
            xt2_2_yt1_2_xt2_3_yt1_3 = _dsadd2(xl0_0_xl1_0_xl0_1_xl1_1, xl21_0_xl20_0_xl21_1_xl20_1);
   
            /*-----------------------------------------------------------*/
            /* apply twiddle factor for first 2-Radix4's                 */
            /*-----------------------------------------------------------*/
            _amem8(&x2[0])  = x_0o_x_1o_x_2o_x_3o;
            _amem8(&x2[4])  = _dcmpyr1(xt1_0_yt2_0_xt1_1_yt2_1, co1si1_0_co1si1_1);
            _amem8(&x2[8])  = _dcmpyr1(xt0_0_yt0_0_xt0_1_yt0_1, co2si2_0_co2si2_1);
            _amem8(&x2[12]) = _dcmpyr1(xt2_0_yt1_0_xt2_1_yt1_1, co3si3_0_co3si3_1);

            /*-----------------------------------------------------------*/
            /* apply twiddle factor for second 2-Radix4's                */
            /*-----------------------------------------------------------*/
            _amem8(&x2[16]) = x_4o_x_5o_x_6o_x_7o;
            _amem8(&x2[20]) = _dcmpyr1(xt1_2_yt2_2_xt1_3_yt2_3, co1si1_0_co1si1_1);
            _amem8(&x2[24]) = _dcmpyr1(xt0_2_yt0_2_xt0_3_yt0_3, co2si2_0_co2si2_1);
            _amem8(&x2[28]) = _dcmpyr1(xt2_2_yt1_2_xt2_3_yt1_3, co3si3_0_co3si3_1);
           }
        radix_2(ptr_x, ptr_y, npoints);
    }
    else if (radix == 4) {
        radix_4(ptr_x, ptr_y, npoints);
    }
    return;
}

void radix_2 (
    short *restrict ptr_x,
    short *restrict ptr_y,
    int npoints
)
{
    short *restrict x0;
    short *restrict y0, *restrict y1, *restrict y2, *restrict y3;
    int i, j, k, n0, h2, l1, l2;
    int x_01, x_23, x_45, x_67, x_89, x_AB, x_CD, x_EF;
    long long x_0123, x_4567, x_89AB, x_CDEF;
    long long y_01_y_45, y_89_y_CD, y_AB_y_EF, y_23_y_67;

    /*-----------------------------------------------------------------*/
    /* The following code performs either a standard radix2 pass       */
    /*-----------------------------------------------------------------*/
    y0 = ptr_y;
    y2 = ptr_y + (int)npoints;
    x0 = ptr_x;
    l2 = (int)(npoints >> 1);

    /*-----------------------------------------------------------------*/
    /* The pointers are set at the following locations which are half  */
    /* the offsets of a radix4 FFT.                                    */
    /*-----------------------------------------------------------------*/
    y1 = y0 + (int)(npoints >> 2);
    y3 = y2 + (int)(npoints >> 2);
    l1 = _norm(npoints) + 2;
    n0 = npoints >> 1;
    j = 0;

    _nassert((int)(n0) % 4  == 0);
    _nassert((int)(ptr_x) % 8 == 0);
    _nassert((int)(ptr_y) % 8 == 0);
    _nassert((int)(x0) % 8 == 0);
    _nassert((int)(y0) % 8 == 0);
    _nassert(npoints >= 16);

    /*-------------------------------------------------------------------*/
    /* Loop twice for Top and bottom half of radix2                      */
    /*-------------------------------------------------------------------*/
    for (k = 0; k < 2; k++, j += n0, x0 += n0) {

        #pragma MUST_ITERATE(1,,1);
        for (i = 0; i < n0; i += 8) {

            /*---------------------------------------------------------------*/
            /* The following code taks an index "j" and generates the bit    */
            /* reversed index. It does through the "deal", "bitr" and "shfl" */
            /* instructions. The shift by "l1" is used to return the result  */
            /* in the right number of bits depending on the size of the FFT  */
            /*---------------------------------------------------------------*/
            h2 = _deal(j);
            h2 = _bitr(h2);
            h2 = _rotl(h2, 16);
            h2 = _shfl(h2);
            h2 >>= l1;

            /*-------------------------------------------------------------*/
            /* Load 8 data values from the top and middle halves using the */
            /* _amemd8 intrinsic. These can be accessed using the _lo and  */
            /* _hi intrinsic.                                              */
            /*-------------------------------------------------------------*/
            x_0123 = _amem8(&x0[0]);
            x_4567 = _amem8(&x0[4]);
            x_89AB = _amem8(&x0[l2]);
            x_CDEF = _amem8(&x0[l2+4]);

            x_01 = _hill(x_0123);
            x_23 = _loll(x_0123);
            x_45 = _hill(x_4567);
            x_67 = _loll(x_4567);
            x_89 = _hill(x_89AB);
            x_AB = _loll(x_89AB);
            x_CD = _hill(x_CDEF);
            x_EF = _loll(x_CDEF);

            /*-------------------------------------------------------------*/
            /* radix = 2  y0 = x0 + x2                                     */
            /*            y1 = x1 + x3                                     */
            /*            y4 = x0 - x2                                     */
            /*            y5 = x1 - x3                                     */
            /*-------------------------------------------------------------*/
            y_01_y_45 = _addsub2(x_01, x_23);

            /*-------------------------------------------------------------*/
            /* radix = 2  y2 = x4 + x6                                     */
            /*            y3 = x5 + x7                                     */
            /*            y6 = x4 - x6                                     */
            /*            y7 = x5 - x7                                     */
            /*-------------------------------------------------------------*/
            y_23_y_67 = _addsub2(x_45, x_67);

            /*-------------------------------------------------------------*/
            /* radix = 2  y8 = x8 + xa                                     */
            /*            y9 = x9 + xb                                     */
            /*            yc = x8 - xa                                     */
            /*            yd = x9 - xb                                     */
            /*-------------------------------------------------------------*/
            y_89_y_CD = _addsub2(x_89, x_AB);

            /*-------------------------------------------------------------*/
            /* radix = 2  ya = xc + xe                                     */
            /*            yb = xd + xf                                     */
            /*            ye = xc - xe                                     */
            /*            yf = xd - xf                                     */
            /*-------------------------------------------------------------*/
            y_AB_y_EF = _addsub2(x_CD, x_EF);

            /*-------------------------------------------------------------*/
            /*  Store out the results of all four butterflies as double    */
            /*  words.                                                     */
            /*-------------------------------------------------------------*/
            _amem8(&y0[4 * h2]) = _itoll(_hill(y_01_y_45), _hill(y_89_y_CD));
            _amem8(&y1[4 * h2]) = _itoll(_hill(y_23_y_67), _hill(y_AB_y_EF));
            _amem8(&y2[4 * h2]) = _itoll(_loll(y_01_y_45), _loll(y_89_y_CD));
            _amem8(&y3[4 * h2]) = _itoll(_loll(y_23_y_67), _loll(y_AB_y_EF));

            j  += 8;
            x0 += 8;
        }
    }
}

void radix_4 (
    short *restrict ptr_x,
    short *restrict ptr_y,
    int npoints
)
{
    short *restrict x0, *restrict x1;
    short *restrict y0, *restrict y1, *restrict y2, *restrict y3;
    int n0, j0, j1, i, h0, h1, l1, l2;
    long long x0_0123, x0_4567, x0_89AB, x0_CDEF;
    long long x1_0123, x1_4567, x1_89AB, x1_CDEF;
    long long x0l0_0_l1_0_l20_0_l21_0, x1l0_0_l1_0_l20_0_l21_0;
    long long x0h0_0_h1_0_h20_0_h21_0, x1h0_0_h1_0_h20_0_h21_0;
    long long x0l0_1_l1_1_l20_1_l21_1, x1l0_1_l1_1_l20_1_l21_1;
    long long x0h0_1_h1_1_h20_1_h21_1, x1h0_1_h1_1_h20_1_h21_1;
    long long y0_01_45, y0_89_CD, y0_23_67, y0_AB_EF;
    long long y1_01_45, y1_89_CD, y1_23_67, y1_AB_EF;

    /*-----------------------------------------------------------------*/
    /* The following code performs either a standard radix4 pass       */
    /*-----------------------------------------------------------------*/
    y0 = ptr_y;
    y2 = ptr_y + (int)npoints;
    y1 = y0 + (int)(npoints >> 1);
    y3 = y2 + (int)(npoints >> 1);

    x0 = ptr_x;
    x1 = ptr_x + (int)npoints;
    l1 = _norm(npoints) + 3;
    l2 = (int)(npoints >> 1);
    n0 = npoints >> 2;
    j0 = 0;
    j1 = (int)(npoints >> 1);

    _nassert((int)(n0) % 4 == 0);
    _nassert((int)(x0) % 8 == 0);
    _nassert((int)(y0) % 8 == 0);
    _nassert(npoints >= 16);
    #pragma MUST_ITERATE(1,,1);

    /*-------------------------------------------------------------------*/
    /* Perform top half and Bottom half of radix4 in a single iteration  */
    /*-------------------------------------------------------------------*/
    for (i = 0; i < l2; i += 8) {
        /*---------------------------------------------------------------*/
        /* The following code taks an index "j" and generates the bit    */
        /* reversed index. It does through the "deal", "bitr" and "shfl" */
        /* instructions. The shift by "l1" is used to return the result  */
        /* in the right number of bits depending on the size of the FFT  */
        /*---------------------------------------------------------------*/
        h0 = _deal(j0);
        h0 = _bitr(h0);
        h0 = _rotl(h0, 16);
        h0 = _shfl(h0);
        h0 >>= l1;

        h1 = _deal(j1);
        h1 = _bitr(h1);
        h1 = _rotl(h1, 16);
        h1 = _shfl(h1);
        h1 >>= l1;

        /*---------------------------------------------------------------*/
        /* Load 4 radix-4 data values                                    */
        /*---------------------------------------------------------------*/
        x0_0123 = _amem8(&x0[0]);
        x0_4567 = _amem8(&x0[4]);
        x0_89AB = _amem8(&x0[l2]);
        x0_CDEF = _amem8(&x0[l2+4]);

        x1_0123 = _amem8(&x1[0]);
        x1_4567 = _amem8(&x1[4]);
        x1_89AB = _amem8(&x1[l2]);
        x1_CDEF = _amem8(&x1[l2+4]);

        /*------------------------------------------------------------*/
        /*  h1_0  = x_1 + x_5;        h0_0  = x_0 + x_4;              */
        /*  h21_0 = x_3 + x_7;        h20_0 = x_2 + x_6;              */
        /*  l1_0  = x_1 - x_5;        l0_0  = x_0 - x_4;              */
        /*  l21_0 = x_3 - x_7;        l20_0 = x_2 - x_6;              */
        /*------------------------------------------------------------*/
        x0h0_0_h1_0_h20_0_h21_0 = _dsadd2(x0_0123, x0_4567);
        x0l0_0_l1_0_l20_0_l21_0 = _dssub2(x0_0123, x0_4567);

        x1h0_0_h1_0_h20_0_h21_0 = _dsadd2(x1_0123, x1_4567);
        x1l0_0_l1_0_l20_0_l21_0 = _dssub2(x1_0123, x1_4567);

        /*------------------------------------------------------------*/
        /*  y0 = h0_0 + h20_0 = x_0 + x_4 + x_2 + x_6                 */
        /*  y1 = h1_0 + h21_0 = x_1 + x_5 + x_3 + x_7                 */
        /*  y4 = h0_0 - h21_0 = x_0 + x_4 - x_2 - x_6                 */
        /*  y5 = h1_0 - h20_0 = x_1 + x_5 - x_3 - x_7                 */
        /*------------------------------------------------------------*/
        
        y0_01_45 = _addsub2(_hill(x0h0_0_h1_0_h20_0_h21_0),
                            _loll(x0h0_0_h1_0_h20_0_h21_0));

        y1_01_45 = _addsub2(_hill(x1h0_0_h1_0_h20_0_h21_0),
                            _loll(x1h0_0_h1_0_h20_0_h21_0));

        /*------------------------------------------------------------*/
        /*  y6 = l0_0 + l21_0 = x_0 - x_4 + x_3 - x_7                 */
        /*  y7 = l1_0 - l20_0 = x_1 - x_5 - x_2 + x_6                 */
        /*  y2 = l0_0 - l21_0 = x_0 - x_4 - x_3 + x_7                 */
        /*  y3 = l1_0 + l20_0 = x_1 - x_5 + x_2 - x_6                 */
        /*------------------------------------------------------------*/
        y0_23_67 = _addsub2(_hill(x0l0_0_l1_0_l20_0_l21_0), 
                   _crot90(_loll(x0l0_0_l1_0_l20_0_l21_0)));

        y1_23_67 = _addsub2(_hill(x1l0_0_l1_0_l20_0_l21_0), 
                   _crot90(_loll(x1l0_0_l1_0_l20_0_l21_0)));

        /*------------------------------------------------------------*/
        /*  h0_1  = x_8 + x_c;        h1_1  = x_9 + x_d;              */
        /*  h20_1 = x_a + x_e;        h21_1 = x_b + x_f;              */
        /*  l0_1  = x_8 - x_c;        l1_1  = x_9 - x_d;              */
        /*  l20_1 = x_a - x_e;        l21_1 = x_b - x_f;              */
        /*------------------------------------------------------------*/
        x0h0_1_h1_1_h20_1_h21_1 = _dsadd2(x0_89AB, x0_CDEF);
        x0l0_1_l1_1_l20_1_l21_1 = _dssub2(x0_89AB, x0_CDEF);

        x1h0_1_h1_1_h20_1_h21_1 = _dsadd2(x1_89AB, x1_CDEF);
        x1l0_1_l1_1_l20_1_l21_1 = _dssub2(x1_89AB, x1_CDEF);

        /*------------------------------------------------------------*/
        /*  y8 = h0_1 + h20_1 = x_8 + x_c + x_a + x_e                 */
        /*  y9 = h1_1 + h21_1 = x_9 + x_d + x_b + x_f                 */
        /*  yc = h0_1 - h21_1 = x_8 + x_c - x_a - x_e                 */
        /*  yd = h1_1 - h20_1 = x_9 + x_d - x_b - x_f                 */
        /*------------------------------------------------------------*/
        y0_89_CD = _addsub2(_hill(x0h0_1_h1_1_h20_1_h21_1),
                            _loll(x0h0_1_h1_1_h20_1_h21_1));

        y1_89_CD = _addsub2(_hill(x1h0_1_h1_1_h20_1_h21_1),
                            _loll(x1h0_1_h1_1_h20_1_h21_1));

        /*------------------------------------------------------------*/
        /*  ya = l0_1 + l21_1 = x_8 - x_c + x_b - x_f                 */
        /*  yb = l1_1 - l20_1 = x_9 - x_d - x_a + x_e                 */
        /*  ye = l0_1 - l21_1 = x_8 - x_c - x_b + x_f                 */
        /*  yf = l1_1 + l20_1 = x_9 - x_d + x_a - x_e                 */
        /*------------------------------------------------------------*/
        y0_AB_EF = _addsub2(_hill(x0l0_1_l1_1_l20_1_l21_1), 
                   _crot90(_loll(x0l0_1_l1_1_l20_1_l21_1)));

        y1_AB_EF = _addsub2(_hill(x1l0_1_l1_1_l20_1_l21_1), 
                   _crot90(_loll(x1l0_1_l1_1_l20_1_l21_1)));

        /*------------------------------------------------------------*/
        /*  Store the results of all 4 butterflies as double words.   */
        /*------------------------------------------------------------*/
        _amem8(&y0[4 * h0]) = _itoll(_mvd(_hill(y0_01_45)), _hill(y0_89_CD));
        _amem8(&y1[4 * h0]) = _itoll(_mvd(_hill(y0_23_67)), _hill(y0_AB_EF));
        _amem8(&y2[4 * h0]) = _itoll(_loll(y0_01_45), _mvd(_loll(y0_89_CD)));
        _amem8(&y3[4 * h0]) = _itoll(_loll(y0_23_67), _mvd(_loll(y0_AB_EF)));
        _amem8(&y0[4 * h1]) = _itoll(_mvd(_hill(y1_01_45)), _hill(y1_89_CD));
        _amem8(&y1[4 * h1]) = _itoll(_mvd(_hill(y1_23_67)), _hill(y1_AB_EF));
        _amem8(&y2[4 * h1]) = _itoll(_loll(y1_01_45), _mvd(_loll(y1_89_CD)));
        _amem8(&y3[4 * h1]) = _itoll(_loll(y1_23_67), _mvd(_loll(y1_AB_EF)));

        j0 += 4;
        j1 += 4;
        x0 += 8;
        x1 += 8;
    }
}
#endif
/* ======================================================================== */
/*  End of file: DSP_ifft16x16.c                                            */
/* ------------------------------------------------------------------------ */
/*          Copyright (C) 2011 Texas Instruments, Incorporated.             */
/*                          All Rights Reserved.                            */
/* ======================================================================== */

