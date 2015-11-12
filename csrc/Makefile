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

COMPILER ?= gcc

ifeq ($(COMPILER), clang)
CXX = clang++
else
CXX = g++
endif

CFLAGS = -Wall -Wno-write-strings -Wno-comment
CXXFLAGS = -std=c++11 -Wall -Wno-unused-label -Wno-unused-variable -Wno-attributes -Wno-comment -O3

LIBS = -lm
OBJDIR = .

OBJ_INT = bit.o numerics.o params.o test.o driver.o wpl_alloc.o utils.o
OBJ_IO  = buf_bit.o buf_chunk.o buf_numerics8.o buf_numerics32.o buf_numerics16.o
OBJ_LIB = 
OBJ_EXT = ext_math.o ext_arr.o sora_ext_viterbi.o

# lut test.cpp only contains a main function but has to be linked with everything
OBJ_LUT_ONLY = bit.o numerics.o test.o wpl_alloc.o utils.o

EXECUTABLE = driver
LUT_EXECUTABLE = lutexec

all : driver

driver : $(OBJ_INT) $(OBJ_IO) $(OBJ_LIB) $(OBJ_EXT)
	$(CXX) $(EXTRACOPTS) -o $(EXECUTABLE) $+ $(LIBS)

lutexec: $(OBJ_LUT_ONLY) $(OBJ_LIB) $(OBJ_EXT)
	$(CXX) $(EXTRACOPTS) -o $(LUT_EXECUTABLE) $+ $(LIBS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CXX) $(CFLAGS) -c $< -o $@

clean:
	-rm -f *.o ./*/*.o
	-rm -f *~
	-rm -f $(EXECUTABLE)
	-rm -f *.out

