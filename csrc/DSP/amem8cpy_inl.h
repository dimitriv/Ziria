/**
Atomix project, amem8cpy_inl.h, TODO: insert summary here
Copyright (c) 2015 Stanford University
Released under the Apache License v2.0. See the LICENSE file for details.
Author(s): Manu Bansal
*/
#ifndef AMEM8CPY_INL_H_
#define AMEM8CPY_INL_H_

static inline void _amem8cpy(void * restrict dst, void * restrict src, Uint32 numberOfDblWords) {
	Uint32 i = 0;
	for (i = 0; i < numberOfDblWords; i++) {
		_amem8(&((Uint64 *)dst)[i]) = _amem8(&((Uint64 *)src)[i]);
	}
}

#endif

