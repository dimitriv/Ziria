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

#define BITS_PER_BYTE_SHIFT 3


// We use 3 bit SoftBit
#define SOFT_RANGE      8

/*
// The path metric range
#define METRIC_SIZE     128
#define METRIC_OFFSET   7
#define NOR_MASK_VITAS  0x7
#define BYTE_MAX_VITAS  6
#define BIT_MAX_VITAS   (BYTE_MAX_VITAS * 8)
*/

#include "viterbilut.h"
#include "numerics.h"
#include <emmintrin.h>


#include <stdio.h>

// Based off DEFINE_OP_DUPLICATION16_OPERATION in vector128.h
// Iterate a binary operation (min) on all 16 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
FORCE_INLINE __m128i hmin8(const __m128i &a) {
  __m128i xmm0, xmm1 = a;
  xmm0 = (__m128i) _mm_shuffle_epi32(xmm1, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  xmm1 = (__m128i) _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  xmm0 = _mm_unpacklo_epi8(xmm0, xmm0);
  xmm0 = _mm_unpacklo_epi8(xmm0, xmm0);
  xmm1 = (__m128i) _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  xmm1 = (__m128i) _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epu8(xmm0, xmm1);
  return xmm0;
}


// Based off DEFINE_OP_DUPLICATION16_OPERATION in vector128.h
// Iterate a binary operation (min) on all 16 components in a vector128 type,
// and duplicate the final value to all elements in the returned vector
FORCE_INLINE __m128i hmin16(const __m128i &a) {
  __m128i xmm0, xmm1 = a;
  xmm0 = (__m128i) _mm_shuffle_epi32(xmm1, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  xmm1 = (__m128i) _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  xmm0 = _mm_unpacklo_epi16(xmm0, xmm0);
  xmm0 = _mm_unpacklo_epi16(xmm0, xmm0);
  xmm1 = (__m128i) _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(1,0,3,2));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  xmm1 = (__m128i) _mm_shuffle_epi32(xmm0, _MM_SHUFFLE(2,3,0,1));
  xmm0 = _mm_min_epi16(xmm0, xmm1);
  return xmm0;
}


/************************************************
Viterbi_asig: decode the signal (PLCP header) 
1/2 coded 24bits
N.B. The prefix 6 zeros are included
*************************************************/
FORCE_INLINE
void Viterbi_sig11 (__m128i *pTrellis, const char * pbInput, char * pbOutput, int output_bit = 24)
{
    int i, j;

    calign unsigned char outchar = 0;   // the output(decoded) char

    int i_trellis = 0;           // index of trellis

    // for trace back  
    calign __m128i * pTraceBk;           // trace back pointer in trellis
    int i_minpos = 0;           // the minimal path position
    unsigned char i_tpos   = 0;


    const char * psbit = pbInput;     // pointer to the s-bit stream;

    // temporal variables
    __m128i rub0, rub1, rub2, rub3;
    __m128i rus0, rus1, rus2, rus3;
    __m128i rus4, rus5, rus6, rus7;

    unsigned int i_ma = 0; // index to the Branch Metric LUT table
    unsigned int i_mb = 0;

    pTrellis[0] = ALL_INIT0; 
    pTrellis[1] = ALL_INIT; 
    pTrellis[2] = ALL_INIT; 
    pTrellis[3] = ALL_INIT;

  // We first fully expand the trellis
    while (i_trellis < output_bit ) {
        // We have to decode 24 bits for Signal (PLCP Header)

        // Compute the bench metric
        i_ma = (unsigned int)(unsigned char)(* psbit    ) << 3;
        i_mb = (unsigned int)(unsigned char)(* (psbit+1)) << 3;

        psbit += 2;

        // Compute the new states

        rub0 = _mm_unpacklo_epi8 (pTrellis[0], pTrellis[0]);
        rub1 = _mm_unpacklo_epi8 (pTrellis[2], pTrellis[2]);

        // branch 0
        rub0 = _mm_add_epi8 ( rub0, VIT_MA[i_ma] );
        rub0 = _mm_add_epi8 ( rub0, VIT_MB[i_mb] );  
        rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

        // branch 1
        rub1 = _mm_add_epi8 ( rub1, VIT_MA[i_ma+1] );
        rub1 = _mm_add_epi8 ( rub1, VIT_MB[i_mb+1] ); 
        rub1 = _mm_or_si128 ( rub1, ALL_ONE );

        // store the shortest path, state:[0-15]
        pTrellis[4] = _mm_min_epu8 (rub0, rub1); 

        rub0 = _mm_unpackhi_epi8 (pTrellis[0], pTrellis[0]);
        rub1 = _mm_unpackhi_epi8 (pTrellis[2], pTrellis[2]);

        // branch 0
        rub0 = _mm_add_epi8 ( rub0, VIT_MA[i_ma+2] );
        rub0 = _mm_add_epi8 ( rub0, VIT_MB[i_mb+2] );  
        rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

        // branch 1
        rub1 = _mm_add_epi8 ( rub1, VIT_MA[i_ma+3] );
        rub1 = _mm_add_epi8 ( rub1, VIT_MB[i_mb+3] ); 
        rub1 = _mm_or_si128  ( rub1, ALL_ONE );

        // store the shortest path, state:[16-31]    
        pTrellis[5] = _mm_min_epu8 (rub0, rub1); 

        rub0 = _mm_unpacklo_epi8 (pTrellis[1], pTrellis[1]);
        rub1 = _mm_unpacklo_epi8 (pTrellis[3], pTrellis[3]);

        // branch 0
        rub0 = _mm_add_epi8 ( rub0, VIT_MA[i_ma+4] );
        rub0 = _mm_add_epi8 ( rub0, VIT_MB[i_mb+4] );  
        rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

        // branch 1
        rub1 = _mm_add_epi8 ( rub1, VIT_MA[i_ma+5] );
        rub1 = _mm_add_epi8 ( rub1, VIT_MB[i_mb+5] ); 
        rub1 = _mm_or_si128  ( rub1, ALL_ONE );

        // store the shortest path, state:[32-47]    
        pTrellis[6] = _mm_min_epu8 (rub0, rub1); 

        rub0 = _mm_unpackhi_epi8 (pTrellis[1], pTrellis[1]);
        rub1 = _mm_unpackhi_epi8 (pTrellis[3], pTrellis[3]);

        // branch 0
        rub0 = _mm_add_epi8 ( rub0, VIT_MA[i_ma+6] );
        rub0 = _mm_add_epi8 ( rub0, VIT_MB[i_mb+6] );  
        rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

        // branch 1
        rub1 = _mm_add_epi8 ( rub1, VIT_MA[i_ma+7] );
        rub1 = _mm_add_epi8 ( rub1, VIT_MB[i_mb+7] ); 
        rub1 = _mm_or_si128  ( rub1, ALL_ONE );

        // store the shortest path, state:[48-63]        
        pTrellis[7] = _mm_min_epu8 (rub0, rub1); 

        // Move to next state
        pTrellis += 4;  
        i_trellis ++;

        // Normalize
        if ((i_trellis & 7) == 0 ) {
            // normalization\
            // find the smallest component and extract it from all states
            rub0 = _mm_min_epu8 (pTrellis[0], pTrellis[1] );
            rub1 = _mm_min_epu8 (pTrellis[2], pTrellis[3] );
            rub2 = _mm_min_epu8 (rub0, rub1);

            rub3 = hmin8 (rub2);

            // make sure to clear the marker bit
            rub3 = _mm_and_si128  (rub3, ALL_INVERSE_ONE );

            // normalize
            pTrellis[0] = _mm_sub_epi8 ( pTrellis[0], rub3);
            pTrellis[1] = _mm_sub_epi8 ( pTrellis[1], rub3);
            pTrellis[2] = _mm_sub_epi8 ( pTrellis[2], rub3);
            pTrellis[3] = _mm_sub_epi8 ( pTrellis[3], rub3);        
        }
    }


    // Then, 
  // We have processed all 48 soft value
    // track back


    // do normalization first
    rub0 = _mm_min_epu8 (pTrellis[0], pTrellis[1] );
    rub1 = _mm_min_epu8 (pTrellis[2], pTrellis[3] );
    rub2 = _mm_min_epu8 (rub0, rub1);

    rub3 = hmin8 (rub2);
    rub3 = _mm_and_si128 (rub3, ALL_INVERSE_ONE );

    // normalize
    pTrellis[0] = _mm_sub_epi8 ( pTrellis[0], rub3);
    pTrellis[1] = _mm_sub_epi8 ( pTrellis[1], rub3);
    pTrellis[2] = _mm_sub_epi8 ( pTrellis[2], rub3);
    pTrellis[3] = _mm_sub_epi8 ( pTrellis[3], rub3);        


    // rub3 has the minimal value, we need to find the index
    // the algorithm to find the right index is to embed the index at the least
    // significant bits of state value, then we just find the minimal value

    // ensure to use pminsw - not needed
    rub0 = INDEXES[0];
    rub1 = pTrellis[0];

    rus2 = _mm_unpacklo_epi8 ( rub0, rub1 );
    rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );
    rus4 = _mm_min_epi16 ( rus2, rus3);

    rub0 = INDEXES[1];
    rub1 = pTrellis[1];
    rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
    rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );

    rus5 = _mm_min_epi16 (rus2, rus3);
    rus4 = _mm_min_epi16 (rus4, rus5);

    rub0 = INDEXES[2];
    rub1 = pTrellis[2];
    rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
    rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );

    rus6 = _mm_min_epi16 (rus2, rus3);
    rus4 = _mm_min_epi16 (rus4, rus6);

    rub0 = INDEXES[3];
    rub1 = pTrellis[3];
    rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
    rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );

    rus7 = _mm_min_epi16 (rus2, rus3);
    rus4 = _mm_min_epi16 (rus4, rus7);

    // now rus4 contains the minimal 8 
    rus0 = hmin16 (rus4);

    // now the first word contains the index and value
    // index: bit [7:2]; 
    // value: bit [15:8]
    i_minpos = _mm_extract_epi16(rus0, 0);

    // now we can trace back ...
    pTraceBk = pTrellis;

    i_minpos = (i_minpos >> 2) & 0x7F; // bit 6: the branch id, index 5:0

    // trace back output_bit worth of bits
    pbOutput += (output_bit >> 3);
    for ( i = 0; i < output_bit >> 3; i++) {
        for ( j = 0; j < 8; j++ ) {
            outchar = outchar << 1;
            outchar |= (i_minpos >> 6) & 1;

            // next bit
            pTraceBk -= 4;
            i_minpos = (i_minpos >> 1) & 0x3F;
            i_tpos = ((char*) pTraceBk)[i_minpos] ;
      // now i_minpos 6: branch id; 5:0 is the new index
            i_minpos |= (i_tpos & 1) << 6;  
        }
        
        pbOutput --;
        * pbOutput = outchar;
        outchar = 0;
    }
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
  unsigned int m_iTrellis; // the trellis index

  FORCE_INLINE
  void __init () {
    m_pTrellis = m_trellis;
    m_iTrellis = 0;
    
    m_pTrellis[0] = ALL_INIT0; 
    m_pTrellis[1] = ALL_INIT; 
    m_pTrellis[2] = ALL_INIT; 
    m_pTrellis[3] = ALL_INIT;
  }
public:
  TViterbiCore ()    { __init (); }
  FORCE_INLINE void Reset () { __init (); }

  FORCE_INLINE unsigned int trellis_index () { return m_iTrellis; }

  // Advance trellis, branch compare and select
  // N.B. char* pbInput - must contain two soft-value
  FORCE_INLINE
  void BranchACS (const __m128i pVITMA[], unsigned int i_ma, const __m128i pVITMB[], unsigned int i_mb)
  {
      // temporal variables
      __m128i rub0, rub1, rub2, rub3;

      // Compute the bench metric
      i_ma <<= 3;
      i_mb <<= 3;

      // Compute the new states
      rub0 = _mm_unpacklo_epi8 (m_pTrellis[0], m_pTrellis[0]);
      rub1 = _mm_unpacklo_epi8 (m_pTrellis[2], m_pTrellis[2]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma] );
      rub0 = _mm_add_epi8 ( rub0, pVITMB[i_mb] );  
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+1] );
      rub1 = _mm_add_epi8 ( rub1, pVITMB[i_mb+1] ); 
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[0-15]
      m_pTrellis[4] = _mm_min_epu8 (rub0, rub1); 

      rub0 = _mm_unpackhi_epi8 (m_pTrellis[0], m_pTrellis[0]);
      rub1 = _mm_unpackhi_epi8 (m_pTrellis[2], m_pTrellis[2]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma+2] );
      rub0 = _mm_add_epi8 ( rub0, pVITMB[i_mb+2] );  
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+3] );
      rub1 = _mm_add_epi8 ( rub1, pVITMB[i_mb+3] ); 
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[16-31]    
      m_pTrellis[5] = _mm_min_epu8 (rub0, rub1); 

      rub0 = _mm_unpacklo_epi8 (m_pTrellis[1], m_pTrellis[1]);
      rub1 = _mm_unpacklo_epi8 (m_pTrellis[3], m_pTrellis[3]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma+4] );
      rub0 = _mm_add_epi8 ( rub0, pVITMB[i_mb+4] );  
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+5] );
      rub1 = _mm_add_epi8 ( rub1, pVITMB[i_mb+5] ); 
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[32-47]    
      m_pTrellis[6] = _mm_min_epu8 (rub0, rub1); 

      rub0 = _mm_unpackhi_epi8 (m_pTrellis[1], m_pTrellis[1]);
      rub1 = _mm_unpackhi_epi8 (m_pTrellis[3], m_pTrellis[3]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma+6] );
      rub0 = _mm_add_epi8 ( rub0, pVITMB[i_mb+6] );  
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+7] );
      rub1 = _mm_add_epi8 ( rub1, pVITMB[i_mb+7] ); 
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[48-63]        
      m_pTrellis[7] = _mm_min_epu8 (rub0, rub1); 

    // advance the trellis pointer and index
    m_pTrellis += 4;
    m_iTrellis ++;
  }

  FORCE_INLINE
  void BranchACS (const __m128i pVITMA[], unsigned int i_ma)
  {
      // temporal variables
      __m128i rub0, rub1, rub2, rub3;

      // Compute the bench metric
      i_ma <<= 3;

      // Compute the new states
      rub0 = _mm_unpacklo_epi8 (m_pTrellis[0], m_pTrellis[0]);
      rub1 = _mm_unpacklo_epi8 (m_pTrellis[2], m_pTrellis[2]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma] );
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+1] );
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[0-15]
      m_pTrellis[4] = _mm_min_epu8 (rub0, rub1); 

      rub0 = _mm_unpackhi_epi8 (m_pTrellis[0], m_pTrellis[0]);
      rub1 = _mm_unpackhi_epi8 (m_pTrellis[2], m_pTrellis[2]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma+2] );
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+3] );
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[16-31]    
      m_pTrellis[5] = _mm_min_epu8 (rub0, rub1); 

      rub0 = _mm_unpacklo_epi8 (m_pTrellis[1], m_pTrellis[1]);
      rub1 = _mm_unpacklo_epi8 (m_pTrellis[3], m_pTrellis[3]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma+4] );
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+5] );
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[32-47]    
      m_pTrellis[6] = _mm_min_epu8 (rub0, rub1); 

      rub0 = _mm_unpackhi_epi8 (m_pTrellis[1], m_pTrellis[1]);
      rub1 = _mm_unpackhi_epi8 (m_pTrellis[3], m_pTrellis[3]);

      // branch 0
      rub0 = _mm_add_epi8 ( rub0, pVITMA[i_ma+6] );
      rub0 = _mm_and_si128 ( rub0, ALL_INVERSE_ONE); // mark the path

      // branch 1
      rub1 = _mm_add_epi8 ( rub1, pVITMA[i_ma+7] );
      rub1 = _mm_or_si128  ( rub1, ALL_ONE );

      // store the shortest path, state:[48-63]        
      m_pTrellis[7] = _mm_min_epu8 (rub0, rub1); 

    // advance the trellis pointer and index
    m_pTrellis += 4;
    m_iTrellis ++;
  }

    FORCE_INLINE
  void Normalize ()
  {
      __m128i rub0, rub1, rub2, rub3;

    // normalization\
    // find the smallest component and extract it from all states
    rub0 = _mm_min_epu8 (m_pTrellis[0], m_pTrellis[1] );
    rub1 = _mm_min_epu8 (m_pTrellis[2], m_pTrellis[3] );
    rub2 = _mm_min_epu8 (rub0, rub1);
    
    rub3 = hmin8 (rub2);
    
    // make sure to clear the marker bit
    rub3 = _mm_and_si128  (rub3, ALL_INVERSE_ONE );
    
    // normalize
    m_pTrellis[0] = _mm_sub_epi8 ( m_pTrellis[0], rub3);
    m_pTrellis[1] = _mm_sub_epi8 ( m_pTrellis[1], rub3);
    m_pTrellis[2] = _mm_sub_epi8 ( m_pTrellis[2], rub3);
    m_pTrellis[3] = _mm_sub_epi8 ( m_pTrellis[3], rub3);      
  }


  FORCE_INLINE
  void Traceback (char * pbOutput, unsigned long output_bits, unsigned long lookahead )
  {
    __m128i rub0, rub1, rub2, rub3;
    __m128i rus0, rus1, rus2, rus3;
    __m128i rus4, rus5, rus6, rus7;
  
      // rub3 has the minimal value, we need to find the index
      // the algorithm to find the right index is to embed the index at the least
      // significant bits of state value, then we just find the minimal value

      rub0 = INDEXES[0];
      rub1 = m_pTrellis[0];

      rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
      rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );
      rus4 = _mm_min_epi16 ( rus2, rus3);

      rub0 = INDEXES[1];
      rub1 = m_pTrellis[1];
      rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
      rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );

      rus5 = _mm_min_epi16 (rus2, rus3);
      rus4 = _mm_min_epi16 (rus4, rus5);

      rub0 = INDEXES[2];
      rub1 = m_pTrellis[2];
      rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
      rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );

    rus6 = _mm_min_epi16 (rus2, rus3);
    rus4 = _mm_min_epi16 (rus4, rus6);

      rub0 = INDEXES[3];
      rub1 = m_pTrellis[3];
      rus2 = _mm_unpacklo_epi8  ( rub0, rub1 );
      rus3 = _mm_unpackhi_epi8 ( rub0, rub1 );

      rus7 = _mm_min_epi16 (rus2, rus3);
      rus4 = _mm_min_epi16 (rus4, rus7);

      // now rus4 contains the minimal 8 
      rus0 = hmin16 (rus4);

      // now the first word contains the index and value
      // index: bit [7:2];
      // value: bit [15:8]
      int i_minpos = _mm_extract_epi16(rus0, 0);

    unsigned char i_tpos;   

      // now we can trace back ...
      __m128i* pTraceBk = m_pTrellis;

      // first part - trace back without output
    i_minpos = (i_minpos >> 2) & 0x7F;

    for (unsigned int i = 0; i < lookahead; i++)
    {
      pTraceBk -= 4;
      i_minpos = (i_minpos >> 1) & 0x3F;
      i_tpos = ((char*) pTraceBk)[i_minpos] ;
      i_minpos |= (i_tpos & 1) << 6;  // now i_minpos 6:0 is the new index
    }


      // second part - trace back output_bits worth of bits

    unsigned char outchar = 0;
      pbOutput += (output_bits >> 3);
      for (unsigned int i = 0; i < output_bits >> 3; i++) {
          for ( int j = 0; j < 8; j++ ) {
              outchar = outchar << 1;
              outchar |= (i_minpos >> 6) & 1;

              // next bit
              pTraceBk -= 4;
              i_minpos = (i_minpos >> 1) & 0x3F;
              i_tpos = ((char*) pTraceBk)[i_minpos] ;
              i_minpos |= (i_tpos & 1) << 6;  // now i_minpos 6:0 is the new index
          }

          pbOutput --;
          * pbOutput = outchar;
          outchar = 0;
      }
  } 
};

#endif

