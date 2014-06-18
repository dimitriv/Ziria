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
#pragma once
#include <brick.h>
#include "viterbi.hpp"



typedef struct {
	ISource * aSsrc[1];
	uchar retVal[12000];
	int retSize;
	int16 inVal[48];
	int inSize;
} ViterbiContext;


// Interface source

class CF_BrickSource {
	FACADE_FIELD(ViterbiContext *, ctx);
public:
	FINL void Init(ViterbiContext *c)
	{
		ctx() = c;
	}
};

DEFINE_LOCAL_CONTEXT(BrickSource, CF_BrickSource);
template<class C_TYPE, ushort N_LEN>
class BrickSource
{
public:
	template < TSOURCE_ARGS >
	class source : public TSource<TSOURCE_PARAMS>
	{

	protected:
		CTX_VAR_RW(ViterbiContext *, ctx);

	public:
		DEFINE_OPORT(C_TYPE, N_LEN); // each bit is one 8-bit soft-value

	public:
		REFERENCE_LOCAL_CONTEXT(BrickSource);

		STD_TSOURCE_CONSTRUCTOR(source)
			BIND_CONTEXT(CF_BrickSource::ctx, ctx)
		{

		}

		void Reset()
		{
			Next()->Reset();
		}

		bool Process()
		{
			ViterbiContext *c = ctx;
			C_TYPE* output = opin().append();
			memcpy((void *)output, (void *)c->inVal, N_LEN * sizeof(C_TYPE));

			Next()->Process(opin());
			return true;
		}
	};
};


// Interface sink

class CF_BrickSink {
	FACADE_FIELD(ViterbiContext *, ctx);
public:
	FINL void Init(ViterbiContext *c)
	{
		ctx() = c;
	}
};


DEFINE_LOCAL_CONTEXT(BrickSink, CF_BrickSink);
template< TSINK_ARGS >
class BrickSink : public TSink<TSINK_PARAMS>
{
protected:
	CTX_VAR_RW(ViterbiContext *, ctx);

public:
	REFERENCE_LOCAL_CONTEXT(BrickSink);

	DEFINE_IPORT(uchar, 1); 
	STD_TSINK_CONSTRUCTOR(BrickSink) 
		BIND_CONTEXT(CF_BrickSink::ctx, ctx)
	{
	}
	STD_TSINK_RESET() {}
	STD_TSINK_FLUSH() {}

	BOOL_FUNC_PROCESS(ipin) {
		while (ipin.check_read())
		{
			uchar* input = ipin.peek();
			ctx->retVal[ctx->retSize] = *input;
			ctx->retSize++;
			ipin.pop();
		}
		return true;
	}

};


DEFINE_LOCAL_CONTEXT(BrickSinkSig11a, CF_BrickSink);
template< TSINK_ARGS >
class BrickSinkSig11a : public TSink<TSINK_PARAMS>
{
protected:
	CTX_VAR_RW(ViterbiContext *, ctx);

public:
	REFERENCE_LOCAL_CONTEXT(BrickSinkSig11a);

	DEFINE_IPORT(uint, 1);
	STD_TSINK_CONSTRUCTOR(BrickSinkSig11a)
		BIND_CONTEXT(CF_BrickSink::ctx, ctx)
	{
		}
	STD_TSINK_RESET() {}
	STD_TSINK_FLUSH() {}

	BOOL_FUNC_PROCESS(ipin) {
		while (ipin.check_read())
		{
			uint* input = ipin.peek();
			memcpy((void *)(ctx->retVal + ctx->retSize), (void *)input, sizeof(uint));
			ctx->retSize+=4;		// 32 bits
			ipin.pop();
		}
		return true;
	}

};



typedef struct _tagBrickViterbiContext : 
	LOCAL_CONTEXT(T11aViterbi), 
	LOCAL_CONTEXT(T11aViterbiSig),
	LOCAL_CONTEXT(BrickSink),
	LOCAL_CONTEXT(BrickSinkSig11a),
	LOCAL_CONTEXT(BrickSource)
{
	void Reset() {
		// Reset all CFacade data in the context
	}
	
	void Init(ViterbiContext *ctx, ushort frame_len, ushort code_rate)
	{
		CF_11aRxVector::frame_length() = frame_len;
		CF_11aRxVector::code_rate() = code_rate;
		CF_BrickSource::Init(ctx);
		CF_BrickSink::Init(ctx);
		Reset();
	}
}BrickViterbiContext;
	  
BrickViterbiContext BrickViterbiCtx;
BrickViterbiContext BrickViterbiSig11aCtx;



static inline int
CreateViterbiGraph(ViterbiContext *ctx)
{
	CREATE_BRICK_SINK  (output, BrickSink, BrickViterbiCtx );
	typedef T11aViterbi<5000*8,   48, 256> T11aViterbi6M;
	CREATE_BRICK_FILTER (viterbi,  T11aViterbi6M::Filter,  BrickViterbiCtx, output);
	typedef BrickSource<uchar, 48> BrickSourceInst;
	CREATE_BRICK_SOURCE (viterbi_src, BrickSourceInst::source, BrickViterbiCtx, viterbi );


	ctx->aSsrc[0] = viterbi_src;
	//nReset[0] = 1000;

	return 1;
}



static inline int 
CreateViterbiSig11aGraph(ViterbiContext *ctx)
{
	CREATE_BRICK_SINK(outputSig11a, BrickSinkSig11a, BrickViterbiSig11aCtx);
	CREATE_BRICK_FILTER(viterbiSig11a, T11aViterbiSig, BrickViterbiSig11aCtx, outputSig11a);
	typedef BrickSource<uchar, 48> BrickSourceInstSig11a;
	CREATE_BRICK_SOURCE(viterbiSig11a_src, BrickSourceInstSig11a::source, BrickViterbiSig11aCtx, viterbiSig11a);


	ctx->aSsrc[0] = viterbiSig11a_src;
	//nReset[0] = 1000;

	return 1;
}




FINL void initViterbi(ViterbiContext *ctx, int frame_len, ushort code_rate)
{
	BrickViterbiCtx.Init (ctx, frame_len, code_rate);
	CreateViterbiGraph (ctx);
	ctx->retSize = 0;
}


FINL void initViterbiSig11a(ViterbiContext *ctx, int frame_len, ushort code_rate)
{
	BrickViterbiSig11aCtx.Init(ctx, frame_len, code_rate);
	CreateViterbiSig11aGraph(ctx);
	ctx->retSize = 0;
}


FINL int16 processViterbi(ViterbiContext *ctx, char *inVal, uchar *outVal)
{
	int rets;
	memcpy((void *)ctx->inVal, (void *)inVal, sizeof(uchar)* 48);
	ctx->inSize = 48;
	bool bRet = ctx->aSsrc[0]->Process();
	rets = ctx->retSize;
	if (ctx->retSize > 0)
	{
		memcpy((void *)outVal, (void *)ctx->retVal, sizeof(uchar)*ctx->retSize);
		ctx->retSize = 0;
	}
	return rets*8;		// in bits
}

FINL void releaseViterbi(ViterbiContext *ctx)
{
	IReferenceCounting::Release(ctx->aSsrc[0]);
}
