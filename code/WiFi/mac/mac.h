#pragma once
/*
Copyright (c) Microsoft Corporation
All rights reserved.

Licensed under the Apache License, Version 2.0 (the ""License""); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

See the Apache Version 2.0 License for specific language governing
permissions and limitations under the License.
*/


typedef enum __PHYMod {
	PHY_MOD_BPSK,
	PHY_MOD_QPSK,
	PHY_MOD_16QAM,
	PHY_MOD_64QAM
} PHYMod;

typedef enum __PHYEnc {
	PHY_ENC_CR_12,
	PHY_ENC_CR_23,
	PHY_ENC_CR_34
} PHYEnc;

typedef struct __PHYRate {
	PHYMod mod;
	PHYEnc enc;
} PHYRate;

typedef enum __MACType {
	MAC_TX_TEST,
	MAC_RX_TEST,
	MAC_TX_ONLY,
	MAC_RX_ONLY,
	MAC_TX_RX
} MACType;

// TX or RX MAC type
extern MACType mac_type;
extern PHYRate phy_rate;

// PC name for wired uplink
extern char txPC[];
