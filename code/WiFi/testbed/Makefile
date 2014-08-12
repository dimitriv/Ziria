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

PP    = scripts/preprocesscompile-mac-$(COMPILER).sh
PPDEB = scripts/preprocesscompile-mac-$(COMPILER).sh
CP    = scripts/ccompile-mac-$(COMPILER).sh
DIFF  = ../../../tools/BlinkDiff

# Targets
vpath %.c ../../csrc/mac
TGTS = $(patsubst %.blk, %.test, $(wildcard *.blk))

all: $(TGTS)

%.c : %.blk
	EXTRAOPTS='--vectorize --autolut --native-mitigators $(EXTRAOPTS) --name=_$(basename $@)' $(PP) $< $(basename $@)

#%.c : %.blk
#	EXTRAOPTS='$(EXTRAOPTS) --name=_$(basename $@)' $(PPDEB) $< $(basename $@)

tx.out: tx.c
	MAC_TYPE=TX EXTRAOPTS='$(EXTRAOPTS)' $(CP) 

rx.out: rx.c
	MAC_TYPE=RX EXTRAOPTS='$(EXTRAOPTS)' $(CP) 

# TX test
tx.outfile: tx.out tx.infile
	./$< --MAC-type=TX-only \
             --TX-input=file \
             --TX-input-file-name=tx.infile \
             --TX-input-file-mode=dbg \
             --TX-output=file \
             --TX-output-file-name=tx.outfile \
             --TX-output-file-mode=dbg \
             $(EXTRARUNOPT)


# RX test
rx.outfile: rx.out rx.infile
	./$< --MAC-type=RX-only \
             --RX-input=file \
             --RX-input-file-name=rx.infile \
             --RX-input-file-mode=dbg \
             --RX-output=file \
             --RX-output-file-name=rx.outfile \
             --RX-output-file-mode=dbg \
             $(EXTRARUNOPT)


%.test: %.outfile
	$(DIFF) -f $< -g $<.ground -d -v -n 0.9 -p


# TX real
tx.run: tx.out
	./$< --MAC-type=TX-only \
	     --TX-input=file \
             --TX-input-file-name=tx.infile \
             --TX-input-file-mode=dbg \
	     --TX-output=sora \
	     --TX-sora-gain=4096 \
	     --TX-sora-radio-id=0 \
	     --TX-sora-central-frequency=739

# RX real
rx.run: rx.out
	./$< --MAC-type=RX-only \
	     --RX-input=sora \
	     --RX-output=dummy \
	     --RX-sora-radio-id=1 \
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
