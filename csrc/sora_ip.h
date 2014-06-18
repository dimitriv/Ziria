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
#pragma once

#ifdef WINDDK
// These need to be included in WinDDK environment but not in VS
// Not sure why...
#include <winsock2.h> // ws2_32.lib required
#include <ws2tcpip.h>
#endif 

const UINT RADIO_HEADER_LENGTH = 2;
const UINT ETHERNET_HEADER_LENGTH = 14; // 6 DstMAC, 6 SrcMAC, 2 EtherType/length
const UINT ETHERNET_MTU = 1600;         // Ethernet frame plus our header
const UINT RADIO_MTU = 1000;            // max bytes in one radio frame
const BOOLEAN USE_RADIO = TRUE;

extern HANDLE hUplinkThread;
extern SOCKET ConnectSocket;

int Ndis_init(char *str);
int WriteFragment(PUCHAR buf);
int ReadFragment(PUCHAR buf, UINT mtu);
