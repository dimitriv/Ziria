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
COMPILER?=vs

PP   = ../../../scripts/preprocesscompile-$(COMPILER)-debug.sh
DIFF = ../../../tools/BlinkDiff

# Do not delete outfiles to allow quick recheck of failed tests
# use make clean to get rid of them
.PRECIOUS: %.outfile %.out


# Targets
TGTS = $(patsubst %.blk, %.test, $(wildcard *.blk))

all: $(TGTS)

%.out : %.blk
	EXTRAOPTS='--bounds-check $(EXTRAOPTS)' $(PP) $< $@

%.outfile: %.out pkt1.infile
	./$< --input=file \
             --input-file-name=pkt1.infile \
             --input-file-mode=dbg \
             --output-file-name=$@ \
             --output-file-mode=dbg

%.test: %.outfile
	$(DIFF) -f $< -g $<.ground -d -v -n 0.9 -p


%.accept : %.outfile
	cp $< $<.ground


clean:
	rm -f ./*expanded
	rm -f ./*exe ./*exec ./*out
	rm -f ./*.outfile
	rm -f ./compout ./perfout
	rm -f ./*.c

all-clean: clean
	rm -f ./*~
	rm -f ./*dump

