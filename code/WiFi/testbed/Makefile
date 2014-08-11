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
COMPILER?=vs

PP    = ../../scripts/preprocesscompile-mac-$(COMPILER).sh
PPDEB = ../../scripts/preprocesscompile-mac-$(COMPILER).sh
CP    = ../../scripts/ccompile-mac-$(COMPILER).sh
DIFF  = ../../tools/BlinkDiff

# Targets
vpath %.c ../../csrc/mac
TGTS = $(patsubst %.blk, %.run, $(wildcard *.blk))

all: $(TGTS)

%.out : %.blk
	EXTRAOPTS='--vectorize --autolut --native-mitigators $(EXTRAOPTS) --name=_$(basename $@)' $(PP) $< $(basename $@)

%.deb : %.blk
	EXTRAOPTS='$(EXTRAOPTS) --name=_$(basename $@)' $(PPDEB) $< $(basename $@)


# TX test
test_continuous_tx.outfile: test_continuous_tx.deb test_continuous_tx.infile
	./$< --MAC-type=TX-only
	     --TX-input=file \
             --TX-input-file-name=tx.infile \
             --TX-input-file-mode=dbg \
             --TX-output=file \
             --TX-output-file-name=tx.outfile \
             --TX-output-file-mode=dbg \
             $(EXTRARUNOPT)


# RX test
test_continuous_rx.outfile: test_continuous_rx.deb test_continuous_rx.infile
	./$< --MAC-type=RX-only
	     --RX-input=file \
             --RX-input-file-name=rx.infile \
             --RX-input-file-mode=dbg \
             --RX-output=file \
             --RX-output-file-name=rx.outfile \
             --RX-output-file-mode=dbg \
             $(EXTRARUNOPT)


# TX real
test_continuous_tx.run: test_continuous_tx.out
	./$< --MAC-type=TX-only
	     --TX-input=file \
             --TX-input-file-name=test_continuous_tx.infile \
             --TX-input-file-mode=dbg \
	     --TX-output=sora \
	     --TX-sora-radio-id=0 \
	     --TX-sora-central-frequency=739

# RX real
test_continuous_rx.run: test_continuous_rx.out
	./$< --MAC-type=RX-only
	     --RX-input=sora \
	     --RX-output=dummy \
	     --RX-sora-radio-id=0 \
	     --RX-sora-central-frequency=739


# Not implemented yet:
# mac.outfile: mac.out mac.infile
# 	./$< --TX-input=file \
#              --TX-input-file-name=mac.infile \
#              --TX-input-file-mode=dbg \
#              --TX-output=memory \
#              --RX-input=memory \
#              --RX-output=file \
#              --RX-output-file-name=$@ \
#              --RX-output-file-mode=dbg \
#              --memory-size=1000 \
#              $(EXTRARUNOPT)


clean:	
	rm -f ./*expanded
	rm -f ./*exe ./*exec ./*out
	rm -f ./*.outfile
	rm -f ./compout ./perfout
	rm -f ./*.c

all-clean: clean
	rm -f ./*~
	rm -f ./*dump
