#pragma once

// #define QUEUE_CHECKS_ENABLED // comment this out to disable run-time checks.

#include "types.h"
#include "numerics.h" // include for the FORCE_INLINE macro

#include "single_thread_queues.h"
#include "sora_thread_queues.h"

typedef struct {
	char * mit_slot;
	// NULL means no mitigation is in progress, 
	// Not null means we are either reading or writing from 
	// somewhere inside this slot. Where from? It's this index:
	int mit_ziria_idx;
} mit_queue;


#define MIT_QUEUE_RESET(mq) \
	mq->mit_slot = NULL;    \
	mq->mit_ziria_idx = 0;

#define MIT_QUEUE_RESERVE_PRE(mq,reserve,rq) \
if (mq->mit_slot == NULL)                    \
{                                            \
	mq->mit_slot = reserve(rq);              \
	mq->mit_ziria_idx = 0;                   \
}

#define MIT_QUEUE_PUSH(mq)                   \
	mq->mit_ziria_idx++;

#define MIT_QUEUE_SLOT(mq,slice_siz) \
	&(mq->mit_slot[mq->mit_ziria_idx * slice_siz])

#define MIT_QUEUE_MITIGATOR_PUSH(mq,push,rq) \
	push(rq);                                \
	MIT_QUEUE_RESET(mq);

#define MIT_QUEUE_RELEASE_POST(mq,n,release,rq) \
if (mq->mit_ziria_idx >= n - 1)                 \
{                                               \
	release(rq);                                \
	mq->mit_ziria_idx = 0;                      \
}                                               \
else mq->mit_ziria_idx++;          


#define MIT_QUEUE_MITIGATOR_ACQUIRE(mq,acquire,rq) \
	mq->mit_slot = acquire(rq);              \
	mq->mit_ziria_idx = 0;                   

//Real signatures:
//void mit_queue_reset(mit_queue *mq);
//unsigned char* mit_queue_reserve(mit_queue *mq, unsigned char * (*reserve)(void *), void *rq, int slice_siz);
//unsigned char* mit_queue_push(mit_queue *mq);
//void mit_queue_mitigator_push(mit_queue *mq, void(*push)(void *), void * rq);


