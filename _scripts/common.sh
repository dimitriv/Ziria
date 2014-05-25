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

export UNAME=$(uname -s)
export WPLC="$TOP/wplc"

if [ "$UNAME" = "Linux" ]
then 
   export CSRC_NATIVE=${TOP}/_csrc
   export CSRC_POSIX=${TOP}/_csrc
else
   export CSRC_NATIVE=`cygpath ${TOP}/_csrc -w`
   export CSRC_POSIX=${TOP}/_csrc
fi

export WPLCFLAGS="-x -v --csrc-native=${CSRC_NATIVE} --csrc-posix=${CSRC_POSIX}"

export CC="gcc"
export CFLAGS="-std=c99 -msse3"

if [ "$UNAME" = "Linux" ]
then
    export LIBS="-lm"
fi

export SILENT=0
export PERF=0
export VECT=0

# Initialize the compout log file
echo "Testing... " > compout
# Initialize the perfout log file
echo "Perf results " > perfout

export DDKDIR="$TOP/_csrc/CompilerDDK"
