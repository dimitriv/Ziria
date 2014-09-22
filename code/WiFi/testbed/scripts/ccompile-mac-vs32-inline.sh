# 
# Copyright (c) Microsoft Corporation
# All rights reserved. 
#
# Licensed under the Apache License, Version 2.0 (the ""License""); you
# may not use this file except in compliance with the License. You may
# obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
# LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
# A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.
#
# See the Apache Version 2.0 License for specific language governing
# permissions and limitations under the License.
#
#

#!/bin/bash

# Ziria compilation script for MAC example
# Since there are multiple Ziria files, 
# we first compile each using preprocesscompile-mac
# and then link everything together using this script.

set -e

# export TOP=$(cd $(dirname $0)/.. && pwd -P)
export TOP=../../..
source $TOP/scripts/common.sh


echo "Compiling C code (VS) ..."
pushd .
cd ../mac
if [ "$MAC_TYPE" = 'TX' ]; then
    echo "TX only ..."
    cp tx.c tx_thread.c
    cp dummy_rx.c rx_thread.c
else
  if [ "$MAC_TYPE" = 'RX' ]; then
      echo "RX only ..."
      cp dummy_tx.c tx_thread.c
      cp rx.c rx_thread.c
  elif [ "$MAC_TYPE" = 'TEST_CCA' ]; then
      echo "TEST CCA only ..."
      cp dummy_tx.c tx_thread.c
      cp test_cca.c rx_thread.c
  else
      echo "TX/RX ..."
      cp tx.c tx_thread.c
      cp rx.c rx_thread.c
  fi
fi
cd CompilerVS 
./build32-inline.bat
popd



if [ "$MAC_TYPE" = 'TX' ]; then
  cp -f ../mac/CompilerVS/CompilerVS13-mac-inline/Win32/Release/CompilerVS13-mac-inline.exe tx.out
else
  if [ "$MAC_TYPE" = 'RX' ]; then
    cp -f ../mac/CompilerVS/CompilerVS13-mac-inline/Win32/Release/CompilerVS13-mac-inline.exe rx.out
  else
    if [ "$MAC_TYPE" = 'TEST_CCA' ]; then
      cp -f ../mac/CompilerVS/CompilerVS13-mac-inline/Win32/Release/CompilerVS13-mac-inline.exe test_cca.out
    else
      cp -f ../mac/CompilerVS/CompilerVS13-mac-inline/Win32/Release/CompilerVS13-mac-inline.exe mac.out
    fi
  fi
fi
