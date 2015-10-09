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

CABAL?=cabal

UNAME := $(shell uname)

SS = compiler.sh

ifeq ($(UNAME), Linux)
all: gcc install chart
else
all: gcc vs winddk install chart
endif

gcc:
	sh $(SS) gcc

vs:
	sh $(SS) vs

winddk:
	sh $(SS) winddk

install:
	$(CABAL) sandbox init
	$(CABAL) install chart-diagrams
	$(CABAL) install parsec
	$(CABAL) install directory

chart:
	$(CABAL) exec runhaskell chart.hs

clean: 
	rm -f output.svg
	rm -rf perf/*

clean-sandbox: clean
	$(CABAL) sandbox delete
	rm -f cabal.sandbox.config

