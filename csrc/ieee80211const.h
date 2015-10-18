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

#define DOT11A_RATE_6M  0xB // 1-011
#define DOT11A_RATE_9M  0XF // 1-111
#define DOT11A_RATE_12M 0xA // 1-010
#define DOT11A_RATE_18M 0xE // 1-110
#define DOT11A_RATE_24M 0x9 // 1-001
#define DOT11A_RATE_36M 0xD // 1-101
#define DOT11A_RATE_48M 0x8 // 1-000
#define DOT11A_RATE_54M 0xC // 1-100

// here defines the coding rate
// CR_12 = 1/2 code
enum CodingRateEnum
{
    CR_12 = 0,
    CR_23,
    CR_34,
    CR_56,
};

static const char LTS_Positive_table [64] = {
    0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 
    1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 
    1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1
};

struct dot11n_rate_param
{
    int cdbps;
    int ndbps;
};

dot11n_rate_param __attribute__((weak)) DOT11N_RATE_PARAMS[16] =
{
    /* MCS 0~7: for single spatial stream*/
    {52, 26},
    {104, 52},
    {104, 78},
    {208, 104},
    {208, 156},
    {312, 208},
    {312, 234},
    {312, 260},
    /* MCS 8~15: for two spatial streams*/
    {104, 52},
    {208, 104},
    {208, 156},
    {416, 208},
    {416, 312},
    {624, 416},
    {624, 468},
    {624, 520},
};

const int service_length = 16;
const int padding = 6;

#endif

