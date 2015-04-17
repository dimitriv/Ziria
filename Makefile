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

all: create-sandbox
	cabal install
	cp .cabal-sandbox/bin/wplc* .
	cp .cabal-sandbox/bin/BlinkDiff* tools/

# intentionally no dependency on create-sandbox here, so that we can run
# `make quick`. Need to make sure to initialize the sandbox first and run
# `cabal configure`.
quick:
	cabal build
	cp dist/build/wplc/wplc .
	cp dist/build/BlinkDiff/BlinkDiff tools/

create-sandbox:
	cabal sandbox init
	cabal install --dependencies-only

# A profiling sandbox 
# create-sandbox-prof:
# 	cabal sandbox init
#         cabal configure --enable-executable-profiling --enable-library-profiling
# 	cabal install --dependencies-only


clean:
	-rm -rf wplc
	-rm -rf wplc.exe
	-rm -rf tools/BlinkDiff
	-rm -rf tools/BlinkDiff.exe

clean-sandbox:
	-rm -rf .cabal-sandbox
	-rm -rf dist

test: test-parser test-backend test-lut test-WiFi-all
test-clean: test-parser-clean test-backend-clean test-lut-clean test-WiFi-all-clean

test-backend:
	@echo ">>>>>>>>>>>>>>> Backend tests"
	make -C tests/backend
	@echo "<<<<<<<<<<<<<<< Backend tests"

test-backend-clean:
	make -C tests/backend clean

test-parser:
	@echo ">>>>>>>>>>>>>>> Parser tests"
	make -C tests/parser
	@echo "<<<<<<<<<<<<<<< Parser tests"

test-parser-clean:
	make -C tests/parser clean

test-lut:
	@echo ">>>>>>>>>>>>>>> LUT tests"
	make -C tests/lut
	@echo "<<<<<<<<<<<<<<< LUT tests"

test-lut-clean:
	make -C tests/lut clean


# WiFi tests
################################################################

test-WiFi-all: test-WiFi test-WiFi-TX test-WiFi-RX
test-WiFi-all-clean: test-WiFi-clean test-WiFi-TX-clean test-WiFi-RX-clean

test-WiFi:
	@echo ">>>>>>>>>>>>>>> WiFi tests"
	make -C code/WiFi/tests
	@echo "<<<<<<<<<<<<<<< WiFi  tests"

test-WiFi-clean:
	make -C code/WiFi/tests clean

test-WiFi-TX:
	@echo ">>>>>>>>>>>>>>> WiFi TX tests"
	make -C code/WiFi/transmitter/tests
	@echo "<<<<<<<<<<<<<<< WiFi TX tests"

test-WiFi-TX-clean:
	make -C code/WiFi/transmitter/tests clean

test-WiFi-RX:
	@echo ">>>>>>>>>>>>>>> WiFi RX tests"
	make -C code/WiFi/receiver/tests
	@echo "<<<<<<<<<<<<<<< WiFi RX tests"

test-WiFi-RX-clean:
	make -C code/WiFi/receiver/tests clean


# WiFi pedantic tests
#############################################################

test-WiFi-pedantic: 
	@echo ">>>>>>>>>>>>>>> Pedantic WiFi tests"

	@echo "Default"
	make test-WiFi && make test-WiFi-clean
	make test-WiFi-RX && make test-WiFi-RX-clean 
	make test-WiFi-TX && make test-WiFi-TX-clean 

	@echo "EXTRAOPTS=--no-fold --no-exp-fold"
	EXTRAOPTS='--no-fold --no-exp-fold' make test-WiFi && make test-WiFi-clean
	EXTRAOPTS='--no-exp-fold --no-fold' make test-WiFi-RX && make test-WiFi-RX-clean 
	EXTRAOPTS='--no-exp-fold --no-fold' make test-WiFi-TX && make test-WiFi-TX-clean 

	@echo "EXTRAOPTS=--vectorize"
	DIFFOPTS='-p' EXTRAOPTS='--vectorize'             make test-WiFi && make test-WiFi-clean
	DIFFOPTS='-p' EXTRAOPTS='--vectorize'             make test-WiFi-RX && make test-WiFi-RX-clean 
	DIFFOPTS='-p' EXTRAOPTS='--vectorize'             make test-WiFi-TX && make test-WiFi-TX-clean 

	@echo "EXTRAOPTS=--autolut"
	EXTRAOPTS='--autolut'               make test-WiFi && make test-WiFi-clean
	EXTRAOPTS='--autolut'               make test-WiFi-RX && make test-WiFi-RX-clean 
	EXTRAOPTS='--autolut'               make test-WiFi-TX && make test-WiFi-TX-clean 

	@echo "EXTRAOPTS=--vectorize --autolut"
	DIFFOPTS='-p' EXTRAOPTS='--vectorize --autolut'   make test-WiFi && make test-WiFi-clean
	DIFFOPTS='-p' EXTRAOPTS='--vectorize --autolut'   make test-WiFi-RX && make test-WiFi-RX-clean
	DIFFOPTS='-p' EXTRAOPTS='--vectorize --autolut'   make test-WiFi-TX && make test-WiFi-TX-clean


test-WiFi-pedantic-clean: test-WiFi-clean test-WiFi-TX-clean test-WiFi-RX-clean


# WiFi performance tests
#############################################################

test-WiFi-perf-all: test-WiFi-perf test-WiFi-TX-perf test-WiFi-RX-perf
test-WiFi-perf-all-clean: test-WiFi-perf-clean test-WiFi-TX-perf-clean test-WiFi-RX-perf-clean

test-WiFi-perf:
	@echo ">>>>>>>>>>>> WiFi performance tests" 
	make -C code/WiFi/perf > perf.txt
	cat perf.txt
	@echo "<<<<<<<<<<<< WiFi performance tests" 

test-WiFi-perf-clean:
	make -C code/WiFi/perf clean

test-WiFi-RX-perf:
	@echo ">>>>>>>>>>>> WiFi RX performance tests" 
	make -C code/WiFi/receiver/perf > rx-perf.txt
	cat rx-perf.txt
	@echo "<<<<<<<<<<<< WiFi RX performance tests" 

test-WiFi-RX-perf-clean: 
	make -C code/WiFi/receiver/perf clean

test-WiFi-TX-perf:
	@echo ">>>>>>>>>>>> WiFi TX performance tests" 
	make -C code/WiFi/transmitter/perf > tx-perf.txt
	cat tx-perf.txt
	@echo "<<<<<<<<<<<< WiFi TX performance tests" 

test-WiFi-TX-perf-clean: 
	make -C code/WiFi/transmitter/perf clean

# vi:set noexpandtab:
