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
COMPILER?=gcc

PP   = ../../scripts/preprocesscompile-$(COMPILER).sh

# Uncomment the following if you want to just run wplc
#PP   = ../../scripts/compile.sh

# Targets
TGTS = $(patsubst %.wpl, %.out, $(wildcard *.wpl))

all: $(TGTS)

%.out : %.wpl
	EXTRAOPTS='$(EXTRAOPTS)' $(PP) $< $@

clean:
	rm -f ./*expanded
	rm -f ./*exe ./*exec
	rm -f ./*.outfile
	rm -f ./compout ./perfout
	rm -f ./*.c
	rm -f ./*.out

all-clean: clean
	rm -f ./*~
	rm -f ./*dump

# Parser tests

.PRECIOUS: %.wpl.expanded.ast.dump

PARSERTESTS = $(patsubst %.wpl, %.parsertest, $(wildcard *.wpl))

parsertests: $(PARSERTESTS)

%.parsertest: %.wpl.expanded.ast.dump 
	-diff -Nur $< $<.ground 

%.wpl.expanded.ast.dump: %.wpl
	EXTRAOPTS='--ddump-ast'	../../scripts/preprocesscompile.sh $< 

%.ast.accept: %.wpl.expanded.ast.dump
	cp $< $<.ground

# vi:set noexpandtab:
