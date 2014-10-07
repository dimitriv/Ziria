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

GHCFLAGS += -fwarn-incomplete-patterns -Werror
GHCFLAGS += -isrc:src/Typecheck:src/Codegen:src/Vectorize:src/Parse:src/Optimize:src/Pipeline:src/BasicTypes:src/Utils:src/TaskGen
GHCFLAGS += -odir obj -hidir obj

ifneq ($(wildcard .cabal-sandbox/*-packages.conf.d),)
GHCFLAGS += \
	-no-user-package-db \
	-package-db $(wildcard .cabal-sandbox/*-packages.conf.d)
endif

all:
	ghc $(GHCFLAGS) --make Main -o wplc
	ghc --make tools/BlinkDiff.hs -o tools/BlinkDiff


clean:
	rm -rf obj wplc wplc.exe


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

test-WiFi-all: test-WiFi test-WiFi-TX test-WiFi-RX
test-WiFi-all-clean: test-WiFi-clean test-WiFi-TX-clean test-WiFi-RX-clean

test-WiFi:
	@echo ">>>>>>>>>>>>>>> Backend tests"
	EXTRAOPTS="--no-exp-fold" make -C code/WiFi/tests
	@echo "<<<<<<<<<<<<<<< Backend tests"

test-WiFi-clean:
	make -C code/WiFi/tests clean

test-WiFi-TX:
	@echo ">>>>>>>>>>>>>>> Backend tests"
	make -C code/WiFi/transmitter/tests
	@echo "<<<<<<<<<<<<<<< Backend tests"

test-WiFi-TX-clean:
	make -C code/WiFi/transmitter/tests clean

test-WiFi-RX:
	@echo ">>>>>>>>>>>>>>> Backend tests"
	make -C code/WiFi/receiver/tests
	@echo "<<<<<<<<<<<<<<< Backend tests"

test-WiFi-RX-clean:
	make -C code/WiFi/receiver/tests clean



# examples: all
# 	@echo "*** Examples (with optimization):"
# 	cd examples; ./runTest s && cd ../
# 	@echo ""
# 	@echo "*** Examples (without optimization):"
# 	cd examples; ./runTestsNoOpt && cd ../

# allclean: clean
# 	find tests -name '*.wpl.c' | xargs rm -f
# 	find tests -name '*.wpl.exe' | xargs rm -f
# 	find tests -name '*.wpl.expanded' | xargs rm -f
# 	find tests -name '*.wpl.outfile' | xargs rm -f

# test-frontend: all
# 	@echo ""
# 	@echo "*** Frontend Tests:"
# 	(cd tests && ./runTests)

# test-vectorize: all
# 	@echo ""
# 	@echo "*** Vectorization Tests:"
# 	(cd tests/vectorize && ./runTests)

# test-lut: all
# 	@echo ""
# 	@echo "*** LUT Tests:"
# 	(cd tests/lut && ./runTests)

# test-wifi-tx: all 
# 	@echo ""
# 	@echo "*** WiFi Tx Tests (with -x -v):"
# #	(cd Code/802.11a/transmitter && ../../../scripts/runGCCTests 10000000)
# 	(cd Code/802.11a/transmitter && ./runTests)

# test-wifi-rx: all 
# 	@echo ""
# 	@echo "*** WiFi Rx Tests (with -x -v):"
# 	(cd Code/802.11a/receiver && ../../../scripts/runDDKTests)

# test-wifi-rx-vector: all 
# 	@echo ""
# 	@echo "*** WiFi Vectorized - Rx Tests (with -x -v):"
# 	(cd Code/802.11a/receiver/vectorized && ../../../../scripts/runDDKTests)

# test-backend: all 
# 	@echo ""
# 	@echo "*** Backend Tests (with optimization):"
# 	(cd tests/backend && ../../scripts/runGCCTests 1000)

# test-pipeline: all
# 	@echo ""
# 	@echo "*** Pipeline Tests:"
# 	(cd tests/pipeline && ../../scripts/runDDKPipeTests)

# test-pipeline-aggressive: all
# 	@echo ""
# 	@echo "*** Aggressive Pipeline Tests:"
# 	(cd tests/pipeline/aggressive && ../../../scripts/runDDKAggrPipeTests)

# test-wifi-rx-pipeline: all 
# 	@echo ""
# 	@echo "*** WiFi Pipelined on 2 Cores - Rx Tests (with -x -v):"
# 	(cd Code/802.11a/receiver/pipelined && ../../../../scripts/runDDKPipeTests)

# test-wifi-rx-vector-pipeline: all 
# 	@echo ""
# 	@echo "*** WiFi Vectorized & Pipelined on 2 Cores - Rx Tests (with -x -v):"
# 	(cd Code/802.11a/receiver/vectorized/pipelined && ../../../../../scripts/runDDKPipeTests)


# test-wifi-all: test-wifi-tx test-wifi-rx \
#                test-wifi-rx-vector test-wifi-rx-pipeline test-wifi-rx-vector-pipeline 

# test: all \
#       test-frontend test-backend test-pipeline test-pipeline-aggressive \
#       test-wifi-all test-vectorize test-lut
# 	@echo "Testsuite run completed!"

# mini-test: all \
#            test-frontend test-backend test-pipeline test-pipeline-aggressive \
#            test-wifi-all test-lut
# 	@echo "Testsuite run completed!"

# # No need for this as non-vectorized is run by avect script anyway
# #test-performance-wifi-rx: all 
# #	@echo ""
# #	@echo "*** Test Wifi RX non-vectorized performance:"
# #	(cd Code/802.11a/receiver/ && ../../../scripts/runDDKTestsPerf)

# test-performance-micro: all 
# 	@echo ""
# 	@echo "*** Test micro-benchmark performance (results in performance dir):"
# 	(cd performance && ./runAll)

# test-performance-wifi-rx-avect: all 
# 	@echo ""
# 	@echo "*** Test Wifi RX auto-vectorized performance:"
# 	(cd Code/802.11a/receiver/ && ../../../scripts/runDDKVectTestsPerf)

# test-performance-wifi-rx-avect-pipeline: all 
# 	@echo ""
# 	@echo "*** Test Wifi RX auto-vectorized & pipelined performance:"
# 	(cd Code/802.11a/receiver/ && ../../../scripts/runDDKVectPipeTestsPerf)

# test-performance-wifi-rx-mvect: all 
# 	@echo ""
# 	@echo "*** Test Wifi RX manually-vectorized performance:"
# 	(cd Code/802.11a/receiver/vectorized/ && ../../../../scripts/runDDKTestsPerf)

# test-performance-wifi-tx-avect-alut-pipeline: all
# 	@echo ""
# 	@echo "*** Test Wifi TX auto-vectorized aut-lutted pipelined performance:"
# 	(cd Code/802.11a/transmitter/ && ../../../scripts/runDDKAVectPipeTestsPerf)

# test-performance-wifi-rx-all: all \
# 	test-performance-wifi-rx-avect \
# 	test-performance-wifi-rx-mvect test-performance-wifi-rx-avect-pipeline
# 	@echo "Wifi RX performance testsuite run completed!"

# test-performance: all \
# 	test-performance-micro test-performance-wifi-rx-avect \
# 	test-performance-wifi-rx-mvect test-performance-wifi-rx-avect-pipeline
# 	@echo "Performance testsuite run completed!"

# test-thorough-performance-wifi: all 
# 	@echo ""
# 	@echo "*** Thorough performance measurement of WiFi (results in TOP/processedPerfResults.txt):"
# 	(./scripts/runPerformanceAll)

# test-thorough-performance: all \
# 	test-thorough-performance-wifi test-performance-micro 
# 	@echo "Thorough performance suite finished"
