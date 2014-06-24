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

export TOP=$(cd $(dirname $0)/.. && pwd -P)
source $TOP/scripts/common.sh


#echo "Compiling C code (VS) ..."
pushd . && cd $TOP/csrc/mac/CompilerVS && ./build.bat && popd

cp -f $TOP/csrc/mac/CompilerVS/CompilerVS13-mac/x64/Release/CompilerVS13.exe mac.out


