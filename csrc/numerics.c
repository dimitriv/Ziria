/* 
   Copyright (c) Microsoft Corporation
   All rights reserved. 

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
*/
#include "numerics.h"
#include <math.h>

/* Primitive operations on complex numbers */
struct complex8 complex8_plus(struct complex8 x, struct complex8 y) {
  struct complex8 r;
  r.re = x.re + y.re;
  r.im = x.im + y.im;
  return r;
}
struct complex8 complex8_minus(struct complex8 x, struct complex8 y) {
  struct complex8 r;
  r.re = x.re - y.re;
  r.im = x.im - y.im;
  return r;
}
struct complex8 complex8_mult(struct complex8 x, struct complex8 y) {
  struct complex8 r;
  r.re = x.re*y.re - x.im*y.im;
  r.im = x.im*y.re + x.re*y.im;
  return r;
}
struct complex8 complex8_div(struct complex8 x, struct complex8 y) {
  struct complex8 r;
  num8 a = x.re;
  num8 b = x.im;
  num8 c = y.re;
  num8 d = y.im;
  r.re = (a*c + b*d) / (c*c + d*d);
  r.im = (b*c - a*d) / (c*c + d*d);
  return r;
}
num8 complex8_creal(struct complex8 x) {
  return x.re;
}
num8 complex8_cimag(struct complex8 x) {
  return x.im;
}

struct complex16 complex16_plus(struct complex16 x, struct complex16 y) {
  struct complex16 r;
  r.re = x.re + y.re;
  r.im = x.im + y.im;
  return r;
}
struct complex16 complex16_minus(struct complex16 x, struct complex16 y) {
  struct complex16 r;
  r.re = x.re - y.re;
  r.im = x.im - y.im;
  return r;
}
struct complex16 complex16_mult(struct complex16 x, struct complex16 y) {
  struct complex16 r;
  r.re = x.re*y.re - x.im*y.im;
  r.im = x.im*y.re + x.re*y.im;
  return r;
}
struct complex16 complex16_div(struct complex16 x, struct complex16 y) {
  struct complex16 r;
  num16 a = x.re;
  num16 b = x.im;
  num16 c = y.re;
  num16 d = y.im;
  r.re = (a*c + b*d) / (c*c + d*d);
  r.im = (b*c - a*d) / (c*c + d*d);
  return r;
}
num16 complex16_creal(struct complex16 x) {
  return x.re;
}
num16 complex16_cimag(struct complex16 x) {
  return x.im;
}

struct complex32 complex32_plus(struct complex32 x, struct complex32 y) 
{
  struct complex32 r;
  r.re = x.re + y.re;
  r.im = x.im + y.im;
  return r;
}
struct complex32 complex32_minus(struct complex32 x, struct complex32 y) 
{
  struct complex32 r;
  r.re = x.re - y.re;
  r.im = x.im - y.im;
  return r;
}
struct complex32 complex32_mult(struct complex32 x, struct complex32 y) 
{
  struct complex32 r;
  r.re = x.re*y.re - x.im*y.im;
  r.im = x.im*y.re + x.re*y.im;
  return r;
}
struct complex32 complex32_div(struct complex32 x, struct complex32 y) 
{
  struct complex32 r;
  num32 a = x.re;
  num32 b = x.im;
  num32 c = y.re;
  num32 d = y.im;
  r.re = (a*c + b*d) / (c*c + d*d);
  r.im = (b*c - a*d) / (c*c + d*d);
  return r;
}
num32 complex32_creal(struct complex32 x) 
{
  return x.re;
}
num32 complex32_cimag(struct complex32 x) 
{
  return x.im;
}

/* 64-bit */
struct complex64 complex64_plus(struct complex64 x, struct complex64 y) 
{
  struct complex64 r;
  r.re = x.re + y.re;
  r.im = x.im + y.im;
  return r;
}
struct complex64 complex64_minus(struct complex64 x, struct complex64 y) 
{
  struct complex64 r;
  r.re = x.re - y.re;
  r.im = x.im - y.im;
  return r;
}
struct complex64 complex64_mult(struct complex64 x, struct complex64 y) 
{
  struct complex64 r;
  r.re = x.re*y.re - x.im*y.im;
  r.im = x.im*y.re + x.re*y.im;
  return r;
}
struct complex64 complex64_div(struct complex64 x, struct complex64 y) 
{
  struct complex64 r;
  num64 a = x.re;
  num64 b = x.im;
  num64 c = y.re;
  num64 d = y.im;
  r.re = (a*c + b*d) / (c*c + d*d);
  r.im = (b*c - a*d) / (c*c + d*d);
  return r;
}
num64 complex64_creal(struct complex64 x) 
{
  return x.re;
}
num64 complex64_cimag(struct complex64 x) 
{
  return x.im;
}


/* Up-casting */
num16 num8to16(num8 x)
{
	num16 ret;
	ret = x;
	return ret;
}
num32 num8to32(num8 x)
{
	num32 ret;
	ret = x;
	return ret;
}

num64 num8to64(num8 x)
{
	num64 ret;
	ret = x;
	return ret;
}


num32 num16to32(num16 x)
{
	num32 ret;
	ret = x;
	return ret;
}

num64 num16to64(num16 x)
{
	num64 ret;
	ret = x;
	return ret;
}

num64 num32to64(num32 x)
{
	num64 ret;
	ret = x;
	return ret;
}


struct complex16 complex8_to_complex16(struct complex8 x)
{
	struct complex16 ret;
	ret.re = num8to16(x.re);
	ret.im = num8to16(x.im);
	return ret;
}
struct complex32 complex8_to_complex32(struct complex8 x)
{
	struct complex32 ret;
	ret.re = num8to32(x.re);
	ret.im = num8to32(x.im);
	return ret;
}

struct complex64 complex8_to_complex64(struct complex8 x)
{
	struct complex64 ret;
	ret.re = num8to64(x.re);
	ret.im = num8to64(x.im);
	return ret;
}


struct complex32 complex16_to_complex32(struct complex16 x)
{
	struct complex32 ret;
	ret.re = num16to32(x.re);
	ret.im = num16to32(x.im);
	return ret;
}
struct complex64 complex16_to_complex64(struct complex16 x)
{
	struct complex64 ret;
	ret.re = num16to64(x.re);
	ret.im = num16to64(x.im);
	return ret;
}

struct complex64 complex32_to_complex64(struct complex32 x)
{
	struct complex64 ret;
	ret.re = num32to64(x.re);
	ret.im = num32to64(x.im);
	return ret;
}


/* Down-casting */
num8 num16to8(num16 x)
{
	num8 ret;
	if (x > 0x7f)
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (positive) cornercase\n");
#endif
		ret = 0x7f;
	}
	else if (x < - (0x80))
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (negative) cornercase\n");
#endif
		ret = - (0x80);
	}
	else
		ret = (num8) x;
	return ret;

}
num8 num32to8(num32 x)
{
	num8 ret;
	if (x > 0x7f)
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (positive) cornercase\n");
#endif
		ret = 0x7f;
	}
	else if (x < - (0x80))
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (negative) cornercase\n");
#endif
		ret = - (0x80);
	}
	else
		ret = x;
	return ret;
}

num8 num64to8(num64 x)
{
	num8 ret;
	if (x > 0x7f)
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (positive) cornercase\n");
#endif
		ret = 0x7f;
	}
	else if (x < - (0x80))
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (negative) cornercase\n");
#endif
		ret = - (0x80);
	}
	else
		ret = x;
	return ret;
}



num16 num32to16(num32 x)
{
	num16 ret;
	if (x > 0x7fff)
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (positive) cornercase\n");
#endif
		ret = 0x7fff;
	}
	else if (x < - (0x8000))
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (negative) cornercase\n");
#endif
		ret = - (0x8000);
	}
	else
		ret = x;
	return ret;
}

num16 num64to16(num64 x)
{
	num16 ret;
	if (x > 0x7fff)
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (positive) cornercase\n");
#endif
		ret = 0x7fff;
	}
	else if (x < - (0x8000))
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 16-8 (negative) cornercase\n");
#endif
		ret = - (0x8000);
	}
	else
		ret = x;
	return ret;
}

num32 num64to32(num64 x)
{
	num32 ret;
	if (x > 0x7fffffff)
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 32-8 (positive) cornercase\n");
#endif
		ret = 0x7fffffff;
	}
	else if (x < - (0x80000000))
	{
#ifdef DEBUG
		fprintf ("Warning: truncating 32-8 (negative) cornercase\n");
#endif
		ret = - (0x80000000);
	}
	else
		ret = x;
	return ret;
}


struct complex32 complex64_to_complex32(struct complex64 x)
{
	struct complex32 ret;
	ret.re = num64to32(x.re);
	ret.im = num64to32(x.im);
	return ret;
}
struct complex16 complex64_to_complex16(struct complex64 x)
{
	struct complex16 ret;
	ret.re = num64to16(x.re);
	ret.im = num64to16(x.im);
	return ret;
}

struct complex8 complex64_to_complex8(struct complex64 x)
{
	struct complex8 ret;
	ret.re = num64to8(x.re);
	ret.im = num64to8(x.im);
	return ret;
}


struct complex16 complex32_to_complex16(struct complex32 x)
{
	struct complex16 ret;
	ret.re = num32to16(x.re);
	ret.im = num32to16(x.im);
	return ret;
}


struct complex8 complex32_to_complex8(struct complex32 x)
{
	struct complex8 ret;
	ret.re = num32to8(x.re);
	ret.im = num32to8(x.im);
	return ret;
}

struct complex8 complex16_to_complex8(struct complex16 x)
{
	struct complex8 ret;
	ret.re = num16to8(x.re);
	ret.im = num16to8(x.im);
	return ret;
}


