# Thread Design Notes

WARNING: these notes are just an internal design doc.

The purpose of these notes is to outline how pipeline parallelism can introduced in Blink via the use of the `|>>>|` combinator. The ideal goal is that the programmer should be able to replace any use of `>>>` with `|>>>|` and get pipeline parallelism. First of all it is relatively easy to see how to do that for a pipeline
of the form:
```
read >>> c1 |>>>| c2 >>> c3 |>>>| c4 >>> c5 >>> write
```
The compiler can break up such a pipeline to the following:
```
let p1 = read >>> c1 >>> write q1
in
let p2 = read q1 >>> c2 >>> c3 >>> write q2
in
let p3 = read q2 >>> c5 >>> write
```

Notice that `read` and `write` read from the input and output, but the so-called _internal_ reads and writes introduce inter-thread synchronized FIFOs, whose
actual implementation follows the SORA SFIFO design. 

Now, Blink can basically compile `p1`, `p2`, and `p3` independently,start them up in different threads that execute on different cores, and that's it.


## The problem

The basic problem is that there exist many contexts under which a `|>>>|` operator can appear and hence we want to allow the programmer to take advantage of pipeline parallelization in many more contexts. A characteristic example may be some initialization code:

```
seq { sk <- initialize_frame()
    ; encode >>> modulate |>>>| ofdm
	}
```

Above, `initialize_frame()` may of may not take or emit values but the important bit is that the programmer wishes to run the `encode >>> modulate` and the `ofdm`
on two different cores.

## Generalized pipeline parallelization

Hence, let us consider all possible contexts under which `|>>>|` may appear. In this discussion we will ignore higher-order combinators and let-bindings for computations or computation functions as these will be inlined away by the time the pipeline parallelization kicks in. The syntax of contexts that we need to deal with is:

```
E ::= repeat E
    | c >>> E
	| E >>> c
	| c |>>>| E
	| E |>>>| c
	| x <- m ; E
	| x <- E; m
	| until e E
	| times x er E
	| while e E
	| if e then c else E
	| if e then E else c
	| *
```
Hence, let us start systematically describing how to lift out the `|>>>|` combinator. The most difficult cases are those for monadic bind, so let's focus on them first.

Suppose we are given:
```
seq { x <- m
    ; c1 |>>>| t1
	}
```
Where we assume that `c1 : ST (C v) a b` and `t1 : ST T b c`, that is `c1` is a computer, and `t1` is a transformer. We know that `m : ST (C u) a c` for the whole computation to type check.
The essense of the transformation will be to create an intermediate stream of
```
s1,v1,s2,v2,s3,v3,s4,v4,...
```
where the `si` represent signalling information, that is whether the first or the second component of the bind is running, and the `vi` represent the value that is emitted which is of type `b + c`.
In terms of implementation we can choose a type for this mixed signal-value stream that represents the largest type of `b` and `c` as well a small integer that denotes which of the two is running.
Then we have to do a bit of work. First, we will need a combinator that lifts all values that come from `m` to this signal-value stream. Similarly for `c1`.
```
let liftl() =
  repeat seq { c <- take
             ; emit (toSignal(0))
             ; emit (toLeft(c))
             }
in
let liftr() =
  repeat seq { b <- take
             ; emit (toSignal(1))
             ; emit (toRight(b))
             }
in
```
Then we will use those to rewrite the original sequence:
```
seq { x <- m >>> liftl()
    ; c1 >>> liftr()
	} |>>>| transform(t1)
```
So, what is `transform(t1)`? As long as the signalling in the channel says that `m` is running then we should simply be considering the values as coming from the left and re-emitting them.
But as soon as the signalling switches to indicate that `c1` is running, we should (a) initialize `t1` and (b) start passing all data through `t1`. Notice that it is important that we only
initialize `t1` _once_, and _only_ when bind switches, to maintain the original semantics.

Here is how we can do this:

```haskell
let transform(t1 : ST T b c) =
  var st : int := 0;
  seq { while(st == 0)
  	  	    seq { s <- take
  	  	 	    ; if (fromSignal(s) == 0) then
  	  	 		      seq { v <- take
  	  	 			      ; emit (fromLeft(s)) }
  	  	 	       else
  	  	 		      do { st := 1 }
  	  	 	     }
	   ; repeat seq { v <- take
	                ; emit (fromRight(v))
	                ; _unused_signalling <- take 
    			    } >>> t1
	   }
in ...
```
The functions `fromSignal`, `fromLeft`, `fromRight` and `toSignal`, `toLeft`, `toRight` are obvious.

### Variations

Consider
```
seq { x <- m
    ; t1 |>>>| c1
	}
```
The translation is identical really, swapping `t1` and `c1`.

Consider now:
```
seq { x <- c1 |>>>| t1
    ; m
	}
```
This is not different really but the roles of left and right have to be swapped.


### A substantially difficult situation

Finally:
```
seq { x <- t1 |>>>| c1
    ; m
	}
```
This is trickier because `t1` is upstream. But we can't arbitarily pull the `t1` out of both components. We need to deactivate `t1` as soon as the sequence switches. But we have no mechanism for doing that.
We could report an error "Can't pipeline parallelize to the programmer" though it is hard to explain why exactly it is that we cannot pipeline parallelize. 

If we really want to attempt a translation, a possibility may be:

```haskell
 seq { t1 >>> while (st == 0)
                seq { v <- take
				    ; emit(fromT(v))
					}
	 ; map fromStream
	 }

 |>>>|
 
 seq { x <- map toT >>> c1
     ; do { st := 1 }
	 ; map toStream >>> m
	 }
```

where the functions `fromT`, `fromStream`, and `toT` and `toStream` are obvious. 

But that means that we have a shared synchronization variable `st` intevening between |>>>| and sending a signal backwards. The type system will surely reject this and I am not even convinced it is safe (i.e. hat it preserves the original semantics.)

The situation that we wish to avoid is that `t1` takes more elements off the input stream while `c1` has switched so `m` will actually receive fewer inputs than it would originally! But imagine that `c1` finishes but the first thread checks the `while` condition as being true and enters the take-emit sequence, before the second thread sets the `st` variable to 1. Catastrophic, as we will have potentially taken more from the input stream. 

This is a more general problem. In any computation of this form: 

```
read >>> t |>>>| c >>> write
```
Our pipeline parallelization plan will create:
```
let p1 = read >>> t >>> write q1
in
let p2 = read q2 >>> c >>> write
```
and will compile the two parts independently (and run them on different cores). This is -- just barely (!) -- OK because once `c` is done, then no-one is supposed to
pick up the rest of the input stream and continue processing. So `t` will be running on its own thread independently and consuming an unknown number of input elements. But because we will never need to do something *after* `c`, we don't care.

On the other hand, for
```
seq { x <- t |>>>| c1
    ; c2
	}
```
we do care!


We are a bit unsure of how exactly to allow pipeline parallelization for such computations -- we are thinking about introducing some form of speculative
execution for `t` where inputs are buffered so that when `c1` declares that he is done we have consume no more elements than those needed for causing this
particular behaviour. Then `c2` can be activated and take over the rest of the input.



## Restricted parallelization transformation (i.e. what is currently implemented)

What we will describe below is basically the naive -- restricted --
class of pipeline parallization that we support, which is basically
only top-level (and last-in-a-sequence). 
