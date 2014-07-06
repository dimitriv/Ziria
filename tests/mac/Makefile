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
#
COMPILER?=vs

PP   = ../../scripts/preprocesscompile-mac-$(COMPILER).sh
CP   = ../../scripts/ccompile-mac-$(COMPILER).sh
DIFF = ../../tools/BlinkDiff


vpath %.c ../../csrc/mac
CSRC = $(patsubst %.zir, %.c, $(wildcard *.zir))

all: mac.test

%.c : %.zir
	EXTRAOPTS='$(EXTRAOPTS) --name=_$(basename $@)' $(PP) $< $(basename $@)

mac.out: $(CSRC)
	EXTRAOPTS='$(EXTRAOPTS)' $(CP) 

mac.outfile: mac.out mac.infile
	./$< --TX-input=file \
             --TX-input-file-name=mac.infile \
             --TX-input-file-mode=dbg \
             --TX-output=memory \
             --RX-input=memory \
             --RX-output=file \
             --RX-output-file-name=$@ \
             --RX-output-file-mode=dbg \
             --memory-size=1000

mac.test: mac.outfile
	$(DIFF) -f $< -g $<.ground -d -v -n 0.9 -p


mac.accept : mac.outfile
	cp $< $<.ground


clean:
	rm -f ./*expanded
	rm -f ./*exe ./*exec
	rm -f ./*.outfile
	rm -f ./compout ./perfout
	rm -f ./*.c

all-clean: clean
	rm -f ./*~
	rm -f ./*dump
