/*

Modified from Microsoft/Sora on Github.

Original license:



Microsoft Research Software Radio

Copyright (c) Microsoft Corporation

All rights reserved.

BSD License

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ""AS IS""
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#ifdef __GNUC__

#pragma once
#include "viterbilut.h"
#include "numerics.h"

#ifndef __ARM_NEON__

#include <emmintrin.h>
#include <stdio.h>

#ifdef __SSE4_1__
#include "smmintrin.h"
#endif

#endif

#ifndef __ARM_NEON__
// Based off DEFINE_OP_DUPLICATION16_OPERATION in vector128.h
// Iterate a binary operation (min) on all 16 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
FORCE_INLINE __m128i hmin8(const __m128i &a) {
  __m128i xmm0, xmm1 = a;
  xmm0 = _mm_shuffle_epi32(xmm1, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  xmm1 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  xmm0 = _mm_unpacklo_epi8(xmm0, xmm0);
  xmm0 = _mm_unpacklo_epi8(xmm0, xmm0);
  xmm1 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  xmm1 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  return xmm0;
}

// Based off DEFINE_OP_DUPLICATION16_OPERATION in vector128.h
// Iterate a binary operation (min) on all 16 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
FORCE_INLINE __m128i hmin16(const __m128i &a) {
#ifdef __SSE4_1__
  return _mm_minpos_epu16(a);
#else
  __m128i xmm0, xmm1 = a;
  xmm0 = _mm_shuffle_epi32(xmm1, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  xmm1 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  xmm0 = _mm_unpacklo_epi16(xmm0, xmm0);
  xmm0 = _mm_unpacklo_epi16(xmm0, xmm0);
  xmm1 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  xmm1 = _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  return xmm0;
#endif
}

FORCE_INLINE void init_trellis(__m128i* trellis) {
  trellis[0] = ALL_INIT0;
  trellis[1] = ALL_INIT;
  trellis[2] = ALL_INIT;
  trellis[3] = ALL_INIT;
}

FORCE_INLINE void branchACSAdvance (__m128i* rub0, __m128i* rub1,
                       unum32* i_ma, unum32* i_mb,
                       const unum8 trellisIndex,
                       const __m128i* pVITMA,
                       const __m128i* pVITMB,
                       __m128i* pTrellis)
{
  __m128i r0 = *rub0;
  __m128i r1 = *rub1;
  // branch 0
  r0 = _mm_add_epi8  (r0, pVITMA[(*i_ma)++]);
  r0 = _mm_add_epi8  (r0, pVITMB[(*i_mb)++]);
  r0 = _mm_and_si128 (r0, ALL_INVERSE_ONE); // mark the path

  // branch 1
  r1 = _mm_add_epi8 (r1, pVITMA[(*i_ma)++]);
  r1 = _mm_add_epi8 (r1, pVITMB[(*i_mb)++]);
  r1 = _mm_or_si128 (r1, ALL_ONE); // mark the path

  // store the shortest path, state:[from (i-4)*16 to (i-3)*16-1]
  // where i is the trellisIndex
  pTrellis[trellisIndex] = _mm_min_epu8 (r0, r1);
}

FORCE_INLINE void branchACSAdvance (__m128i* rub0, __m128i* rub1,
                       unum32* i_ma, const unum8 trellisIndex,
                       const __m128i* pVITMA,
                       __m128i* pTrellis)
{
  __m128i r0 = *rub0;
  __m128i r1 = *rub1;
  // branch 0
  r0 = _mm_add_epi8  (r0, pVITMA[(*i_ma)++]);
  r0 = _mm_and_si128 (r0, ALL_INVERSE_ONE); // mark the path

  // branch 1
  r1 = _mm_add_epi8 (r1, pVITMA[(*i_ma)++]);
  r1 = _mm_or_si128 (r1, ALL_ONE); // mark the path

  // store the shortest path, state:[from (i-4)*16 to (i-3)*16-1]
  // where i is the trellisIndex
  pTrellis[trellisIndex] = _mm_min_epu8 (r0, r1);
}

FORCE_INLINE void normalize (__m128i* pTrellis)
{
  __m128i first8, last8, min8;

  // find the smallest component and extract it from all states
  first8 = _mm_min_epu8 (pTrellis[0], pTrellis[1]);
  last8  = _mm_min_epu8 (pTrellis[2], pTrellis[3]);
  min8   = _mm_min_epu8 (first8, last8);

  min8 = hmin8 (min8);

  // make sure to clear the marker bit
  min8 = _mm_and_si128 (min8, ALL_INVERSE_ONE);

  // normalize
  pTrellis[0] = _mm_sub_epi8 (pTrellis[0], min8);
  pTrellis[1] = _mm_sub_epi8 (pTrellis[1], min8);
  pTrellis[2] = _mm_sub_epi8 (pTrellis[2], min8);
  pTrellis[3] = _mm_sub_epi8 (pTrellis[3], min8);
}

FORCE_INLINE void traceback (num8 * pbOutput, unum64 output_bits,
                             unum64 lookahead, __m128i* pTrellis)
{
    __m128i first8, last8;
    __m128i min16;

    // Calculate the size of output
    unum32 output_bytes = output_bits >> 3;

    // we need to find the index
    // the algorithm to find the right index is to embed the index at the least
    // significant bits of state value, then we just find the minimal value

    // first initialize the 0th index
    first8 = _mm_unpacklo_epi8 (*INDEXES, *pTrellis);
    last8  = _mm_unpackhi_epi8 (*INDEXES, *pTrellis);
    min16  = _mm_min_epi16 (first8, last8);

    // then, loop over indices 1-3
    for(unum8 i=1; i < 4; i++) {
      first8 = _mm_unpacklo_epi8 (INDEXES[i], pTrellis[i]);
      last8  = _mm_unpackhi_epi8 (INDEXES[i], pTrellis[i]);
      min16  = _mm_min_epi16 (min16, _mm_min_epi16(first8, last8));
    }

    // now min16 contains the minimal 8
    min16 = hmin16 (min16);

    // now the first word contains the index and value
    // index: bit [7:2];
    // value: bit [15:8]
    num32 i_minpos = _mm_extract_epi16(min16, 0); // the minimal path position

    unum8 i_tpos; // for traceback

    // now we can trace back ...
    unum8 * pTraceBk = (unum8*) pTrellis;

    // first part - trace back without output
    i_minpos = (i_minpos >> 2) & 0x7F;

    for (unum64 i = 0; i < lookahead; i++) {
      pTraceBk -= 64;
      i_minpos = (i_minpos >> 1) & 0x3F;
      i_tpos = pTraceBk[i_minpos];
      i_minpos |= (i_tpos & 1) << 6;  // now i_minpos 6:0 is the new index
    }


    // second part - trace back output_bits worth of bits

    unum8 outchar = 0;
    pbOutput += output_bytes;
    for (unum32 i = 0; i < output_bytes; i++) {
        for (unum8 j = 0; j < 8; j++ ) {
            outchar <<= 1;
            outchar |= (i_minpos >> 6) & 1;

            // next bit
            pTraceBk -= 64;
            i_minpos = (i_minpos >> 1) & 0x3F;
            i_tpos = pTraceBk [i_minpos];
            i_minpos |= (i_tpos & 1) << 6;  // now i_minpos 6:0 is the new index
        }

        pbOutput--;
        *pbOutput = outchar;
        outchar = 0;
    }
}

FORCE_INLINE void computeNextACSState (__m128i* pTrellis,
                                       unum32* i_ma, unum32* i_mb,
                                       const __m128i* pVITMA,
                                       const __m128i* pVITMB) {
  __m128i rub0, rub1;
  // Compute the new states for lower half of 0,2
  rub0 = _mm_unpacklo_epi8 (pTrellis[0], pTrellis[0]);
  rub1 = _mm_unpacklo_epi8 (pTrellis[2], pTrellis[2]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 4, pVITMA, pVITMB, pTrellis);

  // Compute the new states for upper half of 0,2
  rub0 = _mm_unpackhi_epi8 (pTrellis[0], pTrellis[0]);
  rub1 = _mm_unpackhi_epi8 (pTrellis[2], pTrellis[2]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 5, pVITMA, pVITMB, pTrellis);

  // Compute the new states for lower half of 1,3
  rub0 = _mm_unpacklo_epi8 (pTrellis[1], pTrellis[1]);
  rub1 = _mm_unpacklo_epi8 (pTrellis[3], pTrellis[3]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 6, pVITMA, pVITMB, pTrellis);

  // Compute the new states for upper half of 1,3
  rub0 = _mm_unpackhi_epi8 (pTrellis[1], pTrellis[1]);
  rub1 = _mm_unpackhi_epi8 (pTrellis[3], pTrellis[3]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 7, pVITMA, pVITMB, pTrellis);
}

/************************************************
Viterbi_asig: decode the signal (PLCP header) 
1/2 coded 24bits
N.B. The prefix 6 zeros are included
*************************************************/
FORCE_INLINE
void Viterbi_sig11 (__m128i *pTrellis, const char * pbInput, char * pbOutput)
{
    const unum64 OUTPUT_BITS = 24;

    const num8 * psbit = pbInput;     // pointer to the s-bit stream;

    // temporal variables
    __m128i rub0, rub1;

    unum32 i_ma = 0; // index to the Branch Metric LUT table
    unum32 i_mb = 0;

    init_trellis(pTrellis);

    unum32 i_trellis = 0;           // index of trellis
    // We first fully expand the trellis
    while (i_trellis < OUTPUT_BITS) {
        // We have to decode 24 bits for Signal (PLCP Header)

        // Compute the bench metric
        i_ma = (*psbit++) << 3;
        i_mb = (*psbit++) << 3;

        computeNextACSState(pTrellis, &i_ma, &i_mb, VIT_MA, VIT_MB);

        // Move to next state
        pTrellis += 4;
        i_trellis++;

        // Normalize
        if ((i_trellis & 7) == 0 ) {
          normalize(pTrellis);
        }
    }

    // Then, we have processed all 48 soft value

    // do normalization first
    normalize(pTrellis);

    // track back
    traceback(pbOutput, OUTPUT_BITS, 0, pTrellis);
}

//
// template Viterbi - a more general viterbi implementation for 
// industry-standard encoder g_0=133g g_1=171g
// TR_MAX   - maximum trellis size
//
template<size_t TR_MAX>
class TViterbiCore {
protected:
  __m128i m_trellis [TR_MAX*4];
  __m128i* m_pTrellis;
  unum32 m_iTrellis;   // the trellis index

  FORCE_INLINE
  void __init () {
    m_pTrellis = m_trellis;
    m_iTrellis = 0;

    init_trellis(m_pTrellis);
  }
public:
  TViterbiCore ()    { __init (); }
  FORCE_INLINE void Reset () { __init (); }

  FORCE_INLINE unsigned int trellis_index () { return m_iTrellis; }

  // Advance trellis, branch compare and select
  // N.B. char* pbInput - must contain two soft-value
  FORCE_INLINE
  void BranchACS (const __m128i* pVITMA, unum32 i_ma, const __m128i* pVITMB, unum32 i_mb)
  {
      // Compute the bench metric
      i_ma <<= 3;
      i_mb <<= 3;

      computeNextACSState(m_pTrellis, &i_ma, &i_mb, pVITMA, pVITMB);

      // Move to next state
      m_pTrellis += 4;
      m_iTrellis++;
  }

  FORCE_INLINE
  void BranchACS (const __m128i* pVITMA, unum32 i_ma)
  {
      // temporal variables
      __m128i rub0, rub1;

      // Compute the bench metric
      i_ma <<= 3;

      // Compute the new states for lower half of 0,2
      rub0 = _mm_unpacklo_epi8 (m_pTrellis[0], m_pTrellis[0]);
      rub1 = _mm_unpacklo_epi8 (m_pTrellis[2], m_pTrellis[2]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 4, pVITMA, m_pTrellis);

      // Compute the new states for upper half of 0,2
      rub0 = _mm_unpackhi_epi8 (m_pTrellis[0], m_pTrellis[0]);
      rub1 = _mm_unpackhi_epi8 (m_pTrellis[2], m_pTrellis[2]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 5, pVITMA, m_pTrellis);

      // Compute the new states for lower half of 1,3
      rub0 = _mm_unpacklo_epi8 (m_pTrellis[1], m_pTrellis[1]);
      rub1 = _mm_unpacklo_epi8 (m_pTrellis[3], m_pTrellis[3]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 6, pVITMA, m_pTrellis);

      // Compute the new states for upper half of 1,3
      rub0 = _mm_unpackhi_epi8 (m_pTrellis[1], m_pTrellis[1]);
      rub1 = _mm_unpackhi_epi8 (m_pTrellis[3], m_pTrellis[3]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 7, pVITMA, m_pTrellis);

      // advance the trellis pointer and index
      m_pTrellis += 4;
      m_iTrellis++;
  }

  FORCE_INLINE void Normalize () { normalize(m_pTrellis); }

  FORCE_INLINE
  void Traceback (num8 * pbOutput, unum64 output_bits, unum64 lookahead)
  {
    traceback(pbOutput, output_bits, lookahead, m_pTrellis);
  }
};

#else

static const vub ALLONE    (ALL_ONE);
static const vub ALLINVONE (ALL_INVERSE_ONE );
static const vub ALLINE    (ALL_M128);

FORCE_INLINE void init_trellis(vub* trellis) {
  trellis[0] = ALL_INIT0;
  trellis[1] = ALL_INIT;
  trellis[2] = ALL_INIT;
  trellis[3] = ALL_INIT;
}

FORCE_INLINE void branchACSAdvance (vub* rub0, vub* rub1,
                       unum32* i_ma, unum32* i_mb,
                       const unum8 trellisIndex,
                       const vub* pVITMA,
                       const vub* pVITMB,
                       vub* pTrellis)
{
  vub r0 = *rub0;
  vub r1 = *rub1;
  // branch 0
  r0 = add  (r0, pVITMA[(*i_ma)++]);
  r0 = add  (r0, pVITMB[(*i_mb)++]);
  r0 = bitwise_and (r0, ALLINVONE); // mark the path

  // branch 1
  r1 = add (r1, pVITMA[(*i_ma)++]);
  r1 = add (r1, pVITMB[(*i_mb)++]);
  r1 = bitwise_or (r1, ALLONE); // mark the path

  // store the shortest path, state:[from (i-4)*16 to (i-3)*16-1]
  // where i is the trellisIndex
  pTrellis[trellisIndex] = smin (r0, r1);
}

FORCE_INLINE void branchACSAdvance (vub* rub0, vub* rub1,
                       unum32* i_ma, const unum8 trellisIndex,
                       const vub* pVITMA,
                       vub* pTrellis)
{
  vub r0 = *rub0;
  vub r1 = *rub1;
  // branch 0
  r0 = add  (r0, pVITMA[(*i_ma)++]);
  r0 = bitwise_and (r0, ALLINVONE); // mark the path

  // branch 1
  r1 = add (r1, pVITMA[(*i_ma)++]);
  r1 = bitwise_or (r1, ALLONE); // mark the path

  // store the shortest path, state:[from (i-4)*16 to (i-3)*16-1]
  // where i is the trellisIndex
  pTrellis[trellisIndex] = smin (r0, r1);
}

FORCE_INLINE void normalize (vub* pTrellis)
{
  vub first8, last8, min8;

  // find the smallest component and extract it from all states
  first8 = smin (pTrellis[0], pTrellis[1]);
  last8  = smin (pTrellis[2], pTrellis[3]);
  min8   = smin (first8, last8);

  min8 = hmin (min8);

  // make sure to clear the marker bit
  min8 = bitwise_and (min8, ALLINVONE);

  // normalize
  pTrellis[0] = sub (pTrellis[0], min8);
  pTrellis[1] = sub (pTrellis[1], min8);
  pTrellis[2] = sub (pTrellis[2], min8);
  pTrellis[3] = sub (pTrellis[3], min8);
}


FORCE_INLINE void traceback (num8 * pbOutput, unum64 output_bits,
                             unum64 lookahead, vub* pTrellis)
{
    vub first8, last8;
    vus min16;

    // Calculate the size of output
    unum32 output_bytes = output_bits >> 3;

    // we need to find the index
    // the algorithm to find the right index is to embed the index at the least
    // significant bits of state value, then we just find the minimal value

    // first initialize the 0th index
    first8 = interleave_low (*INDEXES, *pTrellis);
    last8  = interleave_high (*INDEXES, *pTrellis);
    min16  = (vus)smin (first8, last8);

    // then, loop over indices 1-3
    for(unum8 i=1; i < 4; i++) {
      first8 = interleave_low (INDEXES[i], pTrellis[i]);
      last8  = interleave_high (INDEXES[i], pTrellis[i]);
      min16  = smin (min16, (vus)smin (first8, last8));
    }

    // now min16 contains the minimal 8
    min16 = hmin (min16);

    // now the first word contains the index and value
    // index: bit [7:2];
    // value: bit [15:8]
    num32 i_minpos = extract<0>(min16); // the minimal path position

    unum8 i_tpos; // for traceback

    // now we can trace back ...
    unum8 * pTraceBk = (unum8*) pTrellis;

    // first part - trace back without output
    i_minpos = (i_minpos >> 2) & 0x7F;

    for (unum64 i = 0; i < lookahead; i++) {
      pTraceBk -= 64;
      i_minpos = (i_minpos >> 1) & 0x3F;
      i_tpos = pTraceBk[i_minpos];
      i_minpos |= (i_tpos & 1) << 6;  // now i_minpos 6:0 is the new index
    }


    // second part - trace back output_bits worth of bits

    unum8 outchar = 0;
    pbOutput += output_bytes;
    for (unum32 i = 0; i < output_bytes; i++) {
        for (unum8 j = 0; j < 8; j++ ) {
            outchar <<= 1;
            outchar |= (i_minpos >> 6) & 1;

            // next bit
            pTraceBk -= 64;
            i_minpos = (i_minpos >> 1) & 0x3F;
            i_tpos = pTraceBk [i_minpos];
            i_minpos |= (i_tpos & 1) << 6;  // now i_minpos 6:0 is the new index
        }

        pbOutput--;
        *pbOutput = outchar;
        outchar = 0;
    }
}

FORCE_INLINE void computeNextACSState (vub* pTrellis,
                                       unum32* i_ma, unum32* i_mb,
                                       const vub* pVITMA,
                                       const vub* pVITMB) {
  vub rub0, rub1;
  // Compute the new states for lower half of 0,2
  rub0 = interleave_low (pTrellis[0], pTrellis[0]);
  rub1 = interleave_low (pTrellis[2], pTrellis[2]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 4, pVITMA, pVITMB, pTrellis);

  // Compute the new states for upper half of 0,2
  rub0 = interleave_high (pTrellis[0], pTrellis[0]);
  rub1 = interleave_high (pTrellis[2], pTrellis[2]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 5, pVITMA, pVITMB, pTrellis);

  // Compute the new states for lower half of 1,3
  rub0 = interleave_low (pTrellis[1], pTrellis[1]);
  rub1 = interleave_low (pTrellis[3], pTrellis[3]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 6, pVITMA, pVITMB, pTrellis);

  // Compute the new states for upper half of 1,3
  rub0 = interleave_high (pTrellis[1], pTrellis[1]);
  rub1 = interleave_high (pTrellis[3], pTrellis[3]);
  branchACSAdvance(&rub0, &rub1, i_ma, i_mb, 7, pVITMA, pVITMB, pTrellis);
}




/************************************************
Viterbi_asig: decode the signal (PLCP header)
1/2 coded 24bits
N.B. The prefix 6 zeros are included
*************************************************/
FORCE_INLINE
void Viterbi_sig11 (vub *pTrellis, const signed char * pbInput, signed char * pbOutput)
{
    const unum64 OUTPUT_BITS = 24;

    const num8 * psbit = pbInput;     // pointer to the s-bit stream;

    // temporal variables
    vub rub0, rub1;

    unum32 i_ma = 0; // index to the Branch Metric LUT table
    unum32 i_mb = 0;

    init_trellis(pTrellis);

    unum32 i_trellis = 0;           // index of trellis
    // We first fully expand the trellis
    while (i_trellis < OUTPUT_BITS) {
        // We have to decode 24 bits for Signal (PLCP Header)

        // Compute the bench metric
        i_ma = (*psbit++) << 3;
        i_mb = (*psbit++) << 3;

        computeNextACSState(pTrellis, &i_ma, &i_mb, (vub *)VIT_MA, (vub *)VIT_MB);

        // Move to next state
        pTrellis += 4;
        i_trellis++;

        // Normalize
        if ((i_trellis & 7) == 0 ) {
          normalize(pTrellis);
        }
    }

    // Then, we have processed all 48 soft value

    // do normalization first
    normalize(pTrellis);

    // track back
    traceback(pbOutput, OUTPUT_BITS, 0, pTrellis);
}


//
// template Viterbi - a more general viterbi implementation for
// industry-standard encoder g_0=133g g_1=171g
// TR_MAX   - maximum trellis size
//
template<size_t TR_MAX>
class TViterbiCore {
protected:
  vub m_trellis [TR_MAX*4];
  vub* m_pTrellis;
  unum32 m_iTrellis;   // the trellis index

  FORCE_INLINE
  void __init () {
    m_pTrellis = m_trellis;
    m_iTrellis = 0;

    init_trellis(m_pTrellis);
  }
public:
  TViterbiCore ()    { __init (); }
  FORCE_INLINE void Reset () { __init (); }

  FORCE_INLINE unsigned int trellis_index () { return m_iTrellis; }

  // Advance trellis, branch compare and select
  // N.B. char* pbInput - must contain two soft-value
  FORCE_INLINE
  void BranchACS (const vub* pVITMA, unum32 i_ma, const vub* pVITMB, unum32 i_mb)
  {
      // Compute the bench metric
      i_ma <<= 3;
      i_mb <<= 3;

      computeNextACSState(m_pTrellis, &i_ma, &i_mb, pVITMA, pVITMB);

      // Move to next state
      m_pTrellis += 4;
      m_iTrellis++;
  }

  FORCE_INLINE
  void BranchACS (const vub* pVITMA, unum32 i_ma)
  {
      // temporal variables
      vub rub0, rub1;

      // Compute the bench metric
      i_ma <<= 3;

      // Compute the new states for lower half of 0,2
      rub0 = interleave_low (m_pTrellis[0], m_pTrellis[0]);
      rub1 = interleave_low (m_pTrellis[2], m_pTrellis[2]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 4, pVITMA, m_pTrellis);

      // Compute the new states for upper half of 0,2
      rub0 = interleave_high (m_pTrellis[0], m_pTrellis[0]);
      rub1 = interleave_high (m_pTrellis[2], m_pTrellis[2]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 5, pVITMA, m_pTrellis);

      // Compute the new states for lower half of 1,3
      rub0 = interleave_low (m_pTrellis[1], m_pTrellis[1]);
      rub1 = interleave_low (m_pTrellis[3], m_pTrellis[3]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 6, pVITMA, m_pTrellis);

      // Compute the new states for upper half of 1,3
      rub0 = interleave_high (m_pTrellis[1], m_pTrellis[1]);
      rub1 = interleave_high (m_pTrellis[3], m_pTrellis[3]);
      branchACSAdvance(&rub0, &rub1, &i_ma, 7, pVITMA, m_pTrellis);

      // advance the trellis pointer and index
      m_pTrellis += 4;
      m_iTrellis++;
  }

  FORCE_INLINE void Normalize () { normalize(m_pTrellis); }

  FORCE_INLINE
  void Traceback (num8 * pbOutput, unum64 output_bits, unum64 lookahead)
  {
    traceback(pbOutput, output_bits, lookahead, m_pTrellis);
  }
};

#endif

#endif

