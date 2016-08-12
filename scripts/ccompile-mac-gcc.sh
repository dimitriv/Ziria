#!/bin/bash
set -e

export TOP=$(cd $(dirname $0)/.. && pwd -P)
source $TOP/scripts/common.sh


#echo "Compiling C code (VS) ..."
pushd . && cd $TOP/csrc/mac && eval "make -B" && popd

cp -f $TOP/csrc/mac/driver mac.out


