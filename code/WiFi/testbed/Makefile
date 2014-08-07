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

# Disable default rules for C files
.SUFFIXES:

# Uncomment the following if you want winddk to be the default compiler. 
# NB: you can override this from the command line, e.g: 
# prompt> COMPILER=gcc make
#
# COMPILER?=winddk
# COMPILER?=gcc
#
COMPILER?=winddk

PP    = ../../../scripts/preprocesscompile-$(COMPILER)-inline.sh
PPDEB = ../../../scripts/preprocesscompile-$(COMPILER)-debug.sh
DIFF  = ../../../tools/BlinkDiff

# Targets
TGTS = $(patsubst %.blk, %.run, $(wildcard *.blk))

all: $(TGTS)

%.out : %.blk
	EXTRAOPTS='--vectorize --autolut --native-mitigators $(EXTRAOPTS)' $(PP) $< $@

%.deb : %.blk
	EXTRAOPTS=' $(EXTRAOPTS)' $(PPDEB) $< $@


%.outfile: %.deb %.infile
	./$< --input=file \
             --input-file-name=$*.infile \
             --input-file-mode=dbg \
             --output-file-name=$@ \
             --output-file-mode=dbg


test_continuous_tx.run: test_continuous_tx.out
	./$< --input=file \
             --input-file-name=test_continuous_tx.infile \
             --input-file-mode=dbg \
	     --output=sora \
	     --sora-radio-id=0 \
	     --sora-central-frequency=739

test_continuous_rx.run: test_continuous_rx.out
	./$< --input=sora \
	     --output=dummy \
	     --sora-radio-id=0 \
	     --sora-central-frequency=739

clean:	
	rm -f ./*expanded
	rm -f ./*exe ./*exec ./*out
	rm -f ./*.outfile
	rm -f ./compout ./perfout
	rm -f ./*.c

all-clean: clean
	rm -f ./*~
	rm -f ./*dump
