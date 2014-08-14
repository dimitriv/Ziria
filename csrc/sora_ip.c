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
#include <winsock2.h> // ws2_32.lib required
#include <ws2tcpip.h>
#include <conio.h>

#include "sora.h"
#include "sora_ip.h"

HANDLE hUplinkThread;

const char* DEFAULT_PORT = "37015";
UINT bytesTx = 0, bytesRx = 0;
SOCKET ConnectSocket = INVALID_SOCKET;

#undef assert
#define assert(_Expression) (void)( (!!(_Expression)) || (DebugBreak(), 0) )

int sendall(SOCKET s, PUCHAR buf, UINT len)
{
	UINT total = 0;        // how many bytes we've sent
	UINT bytesleft = len;  // how many we have left to send
	UINT n;

	while (total < len) {
		n = send(s, (char*)buf + total, bytesleft, 0);
		if (n == -1) { return -1; }
		total += n;
		bytesleft -= n;
	}

	return total; // return -1 on failure, bytes sent on success
}

int recvall(SOCKET s, PUCHAR buf, UINT len)
{
	UINT total = 0;        // how many bytes we've received
	UINT bytesleft = len;  // how many we have left to receive
	UINT n;

	while (total < len) {
		n = recv(s, (char*)buf + total, bytesleft, 0);
		if (n == -1) { return -1; }
		total += n;
		bytesleft -= n;
	}

	return total; // return -1 on failure, bytes received on success
}

int recvfragment(SOCKET s, PUCHAR buf)
{
	int n = recvall(s, buf, RADIO_HEADER_LENGTH);
	if (n == SOCKET_ERROR) return n;
	int len = buf[0] + ((buf[1] & 0x7F) << 8);
	assert(len > 0);
	assert(len + RADIO_HEADER_LENGTH <= ETHERNET_MTU);
	return recvall(s, buf + RADIO_HEADER_LENGTH, len);
}

void DumpPacket(const char* msg, PUCHAR addr, UINT len)
{
	printf("%s: %02X-%02X-%02X-%02X-%02X-%02X <- %02X-%02X-%02X-%02X-%02X-%02X type %04X bytes %d\n",
		msg, addr[0], addr[1], addr[2], addr[3], addr[4], addr[5],
		addr[6], addr[7], addr[8], addr[9], addr[10], addr[11],
		(addr[12] << 8) + addr[13], len);
}

PACKET_HANDLE ReadEthernetFramePacketHandle;
PUCHAR ReadEthernetFramePointer = NULL;
UINT ReadEthernetFrameBytesRemaining = 0;

int ReadFragment(PUCHAR buf, UINT mtu)
{
	// Do we need to get a new Ethernet frame?
	if (ReadEthernetFrameBytesRemaining == 0)
	{
		assert(ReadEthernetFramePointer == 0);
		DWORD timeout = 0; // milliseconds
		HRESULT hResult = SoraUGetTxPacket(&ReadEthernetFramePacketHandle, (void**)&ReadEthernetFramePointer,
			&ReadEthernetFrameBytesRemaining, timeout);
		if (hResult != S_OK)
			return 0;
		assert(ReadEthernetFrameBytesRemaining <= ETHERNET_MTU);
		assert(ReadEthernetFrameBytesRemaining > ETHERNET_HEADER_LENGTH);
		// DEBUG
		// DumpPacket("Tx", ReadEthernetFramePointer, ReadEthernetFrameBytesRemaining);
		bytesTx += ReadEthernetFrameBytesRemaining;
	}

	// Process current Ethernet frame.
	UINT lenFragment = mtu - RADIO_HEADER_LENGTH;
	BOOLEAN lastFragment = 0;
	if (ReadEthernetFrameBytesRemaining <= lenFragment)
	{
		lenFragment = ReadEthernetFrameBytesRemaining;
		lastFragment = 1;
	}
	buf[0] = lenFragment & 0xFF;
	buf[1] = ((lenFragment >> 8) & 0xFF) | (lastFragment << 7);
	memcpy(buf + RADIO_HEADER_LENGTH, ReadEthernetFramePointer, lenFragment);
	ReadEthernetFramePointer += lenFragment;
	ReadEthernetFrameBytesRemaining -= lenFragment;

	// If we finished the Ethernet frame, release it.
	if (lastFragment)
	{
		assert(ReadEthernetFrameBytesRemaining == 0);
		HRESULT hResult = SoraUCompleteTxPacket(ReadEthernetFramePacketHandle, STATUS_SUCCESS);
		assert(hResult == S_OK);
		ReadEthernetFramePointer = NULL;
	}

	return lenFragment + RADIO_HEADER_LENGTH; // amount of buffer space used
}

UCHAR WriteEthernetFrameBuffer[ETHERNET_MTU];
UINT WriteEthernetFrameLen = 0;

int WriteFragment(PUCHAR buf)
{
	// Inspect fragment to get len, lastFragment.
	UINT len = buf[0] + ((buf[1] & 0x7F) << 8);
	BOOLEAN lastFragment = (buf[1] >> 7);
	assert(len > 0);
	assert(len + WriteEthernetFrameLen <= ETHERNET_MTU);

	// Append fragment to current Ethernet frame.
	memcpy(WriteEthernetFrameBuffer + WriteEthernetFrameLen, buf + RADIO_HEADER_LENGTH, len);
	WriteEthernetFrameLen += len;
	len = WriteEthernetFrameLen; // copy for return value

	// If we finished the Ethernet frame, pass it to NDIS.
	if (lastFragment)
	{
		HRESULT hResult = SoraUIndicateRxPacket(WriteEthernetFrameBuffer, WriteEthernetFrameLen);
		assert(hResult == S_OK);
		WriteEthernetFrameLen = 0;
		assert(len > ETHERNET_HEADER_LENGTH);
		//DEBUG
		DumpPacket("Rx", WriteEthernetFrameBuffer, len);
		bytesRx += len;
	}
	return len;
}

BOOLEAN DownlinkTxProc(void* param)
{
	UCHAR buf[ETHERNET_MTU];
	UINT mtu = USE_RADIO ? RADIO_MTU : ETHERNET_MTU;
	UINT len = ReadFragment(buf, mtu);
	if (len > 0)
	{
        if (USE_RADIO)
		{
			assert(FALSE); // not implemented radio tx yet
		}
		else
		{
			int n = sendall(ConnectSocket, buf, len);
			assert(n > 0);
		}
	}
	return TRUE;
}

BOOLEAN DownlinkRxProc(void* param)
{
	UCHAR buf[ETHERNET_MTU];
	int len = 0;
	if (USE_RADIO)
	{
		assert(FALSE); // not implemented radio rx yet
	}
	else
	{
		len = recvfragment(ConnectSocket, buf);
		assert(len != SOCKET_ERROR);
	}
	if (len > 0)
	{
		int n = WriteFragment(buf);
		assert(n > 0);
	}
	return TRUE;
}

BOOLEAN __stdcall UplinkTxProc(void* param)
{
	UCHAR buf[ETHERNET_MTU];
	UINT len = ReadFragment(buf, sizeof(buf));
	if (len > 0)
	{
		int n = sendall(ConnectSocket, buf, len);
		assert(n > 0);
	}
    return TRUE;
}

BOOLEAN __stdcall UplinkRxProc(void* param)
{
	UCHAR buf[ETHERNET_MTU];
	int len = recvfragment(ConnectSocket, buf);
	assert(len != SOCKET_ERROR);
	if (len > 0)
	{
		int n = WriteFragment(buf);
		assert(n > 0);
	}
    return TRUE;
}


int Ndis_init(char *str)
{
	// Winsock initialisation.
	WSADATA wsaData;
	int iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
	if (iResult != 0) {
		printf("WSAStartup failed with error: %d\n", iResult);
		return 1;
	}

	// If a name is supplied, connect to another PC.
	struct addrinfo *result = NULL, hints;
	BOOLEAN isUE = (str != NULL); 
	if (isUE)
	{
		ZeroMemory(&hints, sizeof(hints));
		hints.ai_family = AF_UNSPEC;
		hints.ai_socktype = SOCK_STREAM;
		hints.ai_protocol = IPPROTO_TCP;

		// Resolve the server address and port
		iResult = getaddrinfo(str, DEFAULT_PORT, &hints, &result);
		if (iResult != 0) {
			printf("getaddrinfo failed with error: %d\n", iResult);
			WSACleanup();
			return 1;
		}

		// Attempt to connect to an address until one succeeds
		for (struct addrinfo *ptr = result; ptr != NULL; ptr = ptr->ai_next) {

			// Create a SOCKET for connecting to server
			ConnectSocket = socket(ptr->ai_family, ptr->ai_socktype,
				ptr->ai_protocol);
			if (ConnectSocket == INVALID_SOCKET) {
				printf("socket failed with error: %ld\n", WSAGetLastError());
				WSACleanup();
				return 1;
			}

			// Connect to server.
			iResult = connect(ConnectSocket, ptr->ai_addr, (int)ptr->ai_addrlen);
			if (iResult == SOCKET_ERROR) {
				closesocket(ConnectSocket);
				ConnectSocket = INVALID_SOCKET;
				continue;
			}
			break;
		}

		freeaddrinfo(result);

		if (ConnectSocket == INVALID_SOCKET) {
			printf("Unable to connect to server!\n");
			WSACleanup();
			return 1;
		}
	}
	// Otherwise, start listening for connections.
	else
	{
		ZeroMemory(&hints, sizeof(hints));
		hints.ai_family = AF_INET;
		hints.ai_socktype = SOCK_STREAM;
		hints.ai_protocol = IPPROTO_TCP;
		hints.ai_flags = AI_PASSIVE;

		// Resolve the server address and port
		iResult = getaddrinfo(NULL, DEFAULT_PORT, &hints, &result);
		if (iResult != 0) {
			printf("getaddrinfo failed with error: %d\n", iResult);
			WSACleanup();
			return 1;
		}

		// Create a SOCKET for connecting to server
		SOCKET ListenSocket = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
		if (ListenSocket == INVALID_SOCKET) {
			printf("socket failed with error: %ld\n", WSAGetLastError());
			freeaddrinfo(result);
			WSACleanup();
			return 1;
		}

		// Setup the TCP listening socket
		iResult = bind(ListenSocket, result->ai_addr, (int)result->ai_addrlen);
		if (iResult == SOCKET_ERROR) {
			printf("bind failed with error: %d\n", WSAGetLastError());
			freeaddrinfo(result);
			closesocket(ListenSocket);
			WSACleanup();
			return 1;
		}

		freeaddrinfo(result);

		iResult = listen(ListenSocket, SOMAXCONN);
		if (iResult == SOCKET_ERROR) {
			printf("listen failed with error: %d\n", WSAGetLastError());
			closesocket(ListenSocket);
			WSACleanup();
			return 1;
		}

		// Accept a client socket
		ConnectSocket = accept(ListenSocket, NULL, NULL);
		if (ConnectSocket == INVALID_SOCKET) {
			printf("accept failed with error: %d\n", WSAGetLastError());
			closesocket(ListenSocket);
			WSACleanup();
			return 1;
		}

		// No longer need server socket
		closesocket(ListenSocket);
	}


	assert(hUplinkThread == NULL);
	hUplinkThread = SoraUThreadAlloc();
	assert(hUplinkThread != NULL);
	if (isUE){
	  // Assign thread procedures appropriately
	  BOOLEAN bResult = SoraUThreadStart(hUplinkThread, UplinkTxProc, NULL);
	  assert(bResult);
	  printf("Started thread at RX\n");
	} else {
	  // Assign thread procedures appropriately
	  BOOLEAN bResult = SoraUThreadStart(hUplinkThread, UplinkRxProc, NULL);
	  assert(bResult);
	  printf("Started thread at TX\n");
	}

	return 0;
}
