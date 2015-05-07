# Blink Language User Guide



## Blink by example

Blink is a 2-layer language: 

1. The lower layer is the _expression_ language. It is a language with imperative fieatures, similar to C and Matlab, for manipulating basic data types, such as bits, bytes, integers, complex numbers, structured types, and arrays.

2. The higher layer is the _computation_ language. The computation language is useful for structuring complex stream processing pipelines out of simpler stream processing blocks -- these blocks ultimately execute functions from the lower-level expression language fragment. The design of the computation language builds on the abstractions of _arrows_ and _monads_ from functional progamming, though familiarity with these concepts is definitely *not* a prerequisite for mastering Blink.

### The scrambler example
Here is a starter example with Blink, a simple implementation of a scrambler:

```gcc

{-
   A scrambler implementation. This is our first toy Blink program.
-}

fun comp scrambler() {
  var scrmbl_st: arr[7] bit := {'1,'1,'1,'1,'1,'1,'1}; 
  var tmp: bit;
  var y:bit;
  
  repeat {
      x <- take;
    
      do {
        tmp := (scrmbl_st[3] ^ scrmbl_st[0]);
        scrmbl_st[0:5] := scrmbl_st[1:6];
        scrmbl_st[6] := tmp;
        y := x ^ tmp;
        print y;
      }; 

      emit (y)
    }
}

-- main computation goes here
let comp main = read >>> scrambler() >>> write
```

The above code defines a _computation_ function called `scrambler`, that accepts the empty list of parameters.
The fact that this is a `comp`-utation function is apparent from the `fun comp scrambler() { ... }` line. Ignoring
for a moment the actual definition of the scrambler, let us first see how it is being used:

```gcc
fun comp scrambler() { 
  ...
}
let comp main = read >>> scrambler() >>> write
```
What is happening in the last line above? We have used the `>>>` combinator to stage our first pipeline:

1. we `read` from the input buffer (we shall see later how to configure where that input is coming from);
2. we pass every single bit of that input through our `scrambler()` component;
3. and we `write` every single bit of the output of our scrambler to the output buffer. (we shall see later on how to configure where that output should be going)

The fact that we pipe the output of the primitive `read` combinator to the input of the `scrambler()` block and its output to the input of the `write` block is clear from our use of `>>>` in-between these blocks. 

Finally, a rule: *every Blink program must be performing `read` from some input and `write` to some output*. The Blink type checker will reject programs with unspecified input or output.

Now, it is time to dive into the implementation of the scrambler.
```gcc
fun comp scrambler() {
  var scrmbl_st: arr[7] bit := {'1,'1,'1,'1,'1,'1,'1}; 
  var tmp: bit;
  var y:bit;
  
  repeat {
      x <- take;
    
      do {
        tmp := (scrmbl_st[3] ^ scrmbl_st[0]);
        scrmbl_st[0:5] := scrmbl_st[1:6];
        scrmbl_st[6] := tmp;
        y := x ^ tmp;
      }; 

      emit y
    }
}
...
```

We declare some local state, namely `scrmbl_st` (an array of 7 bits), `tmp` (a bit), and `y` (a bit).
In fact we _initialize_ the `scrmbl_st` to be an array of set bits. (Re)-initialization of the state of
a block happens once the block becomes "active" in the computation. In our example, initialization is
going to happen at the very beginning and the scrambler state will never be re-initialized. This happens
because the `c1 >>> c2` combinator initializes `c1` and `c2` _before_ data starts flowing through the
pipeline.

After we have declared some local state, we `repeat`-edly, meaning for as long as there is input, perform
the following `seq`-uence of computations:

1. We `take` one element off the input stream and _bind_ its value to variable `x`.
2. We `do` a block of imperative computation. The code inside the block comes from the lower-level imperative fragment (the _expression_ language) and we will explain its syntax in more detail later.
3. Finally, we `emit` the value `y` that has been computed from this imperative computation.


Now, the `seq`-uence operator is the temporal analogue of the `>>>` combinator. In any sequence of the form:
```gcc
{
  c1;
  c2;
  c3;
}
```

The `c1`, `c2`, and `c3` represent components that ** all read or write to the same input and output **. But at every point in time only one of the components is actively reading or writing to the input or output queue. For instance the command
```gcc
x <- take 
```
runs just enough to consume one value from the input stream and does not even emit anything in the output stream. Once it's read a value however, control is transferred to the next block -- the `do { ...}` part. This part does not even read nor write to the input and output stream respectively. It merely executes the inner sequence of statements and transfers control to the next block, that is the `emit y` block. This final block does not read anything from the input stream but rather just emits the value `y` to the output stream. Once the final command returns,
the `repeat` combinator makes sure that we **re-initialize** the sequence and restart. Hence:

```
repeat c
```

is equivalent to:

```
{
  c;
  repeat c;
}
```

which is also equivalent to:

```
{
  c;
  c;
  ...
}
```

### Going in more depth: structuring stream processors

Here is a summary of the constructs we have seen so far for building stream processors:

1. `take`: takes one element from the input stream.
2. `emit`: emits one element to the output stream.
3. `{ c1; c2 }` sequences some stream processors, all of which read/write to the same stream, by
    first initializing and running `c1`. Whenever `c1` returns, `c2` is initialized and control is transferred
	to `c2` which then becomes the active block. The computation returns when `c2` returns. 
4. `c1 >>> c2` streams the output of `c1` to the input of `c2`
5. `repeat c`: Activates `c` and runs until `c` returns. Upon return, `c`, is re-initialized and restarted.
6. `do { ... }`: Provides a way to execute some low-level code statements without reading or writing anything
to input or output.

In what follows we will see more examples of these -- and more -- combinators in action:

#### `map`: Mapping (expression) functions on streams

Our `map f` combinator simply applies an expression-level function `f` to every element of the stream.

```
fun f(x : int) {
  var y : int = 42;
  y := y + 1;
  return (x+y);
}

let comp main = read >>> map f >>> write
```

We will go to more details about expression-level functions later
on. For the moment you may think of them as ordinary C functions.

For computations that are structured as sequences of `take`-ing one element, `do`-ing some computation without keeping local state and `emit`-ing the result on the output, it is recommended that they are structured as `map`s of
some function, because this fuses 3 dataflow processing blocks to just one so the execution overheads are smaller. 
That said, the Blink compiler will typically detect and optimize this pattern anyway. 
	

#### `while`: Running computations until some condition is met

Our `while` combinator allows you to run a stream processor while some condition is true.

```haskell
fun comp f(x: int) {
  var y : int;
  { do { y := x; }    -- init
	  ; while (y > 0)
	    { w <- take
	    ; emit (w+1)
	    }
	  ; do { y := y-1 }   -- decrement
       }
}

let comp main = read >>> f(42) >>> write
```

#### Just `return` a value

We have seen that we can embed an imperative piece of code using the `do {...}` notation.
Sometimes we simply need to be able to `return` a value without executing some statements.
Here is an example:

```
fun comp f(x : int) {
  var y : int := 0;
      { a <-     { do { y := y + x; }
                 ; z <- take
                 ; emit (z+y);
                 ; return y
                 }
      ; emit a
      }
} ...
```

In sequence, we first perform another sequence of commands, which involve updating the value of `y`, then
`take`-ing one value from the input stream, emiting `z+y` on the output stream and `return`-ing the updated
value. That updated value becomes the return value of the whole sub-sequence

```
    { do { y := y + x; }
    ; z <- take
    ; emit (z+y);
    ; return y
    }
```

that we bind to variable `a` and finally `emit` it on the output stream.


#### `if`: Conditionally run one of two processors

Here's an example of conditionals:

```
fun comp f() {
       { x <- take
       ; if (x > 0) then
            emit (x+1)
         else return ()
       }
}
let comp main = read >>> f() >>> write
```

In `seq`-uence, we first consume an element from the input stream with `x <- take`. Subsequently, if `x > 0`
then we emit an element in the output stream (`emit (x+1)`), or `return` control otherwise. The effect of this
computer is that it will keep on incrementing all positive values of the input stream and emitting those to the
output stream. However, once it meets the first non-positive one the whole computation will stop.

What will the following code do then?

```
let comp main = read >>> repeat f() >>> write
```
It will run `f()` to completion, that is, incrementing all positive input values and emitting them. Once a non-positive value we will reinitialize `f()` and restart. The effect will be that the code above keeps only the positive values of the input stream and emits them, incremented, on the output stream.


#### `for`: loops

Blink provides a simple looping structure where you do not have to provide the increment value.

```
fun comp f() {
  for n in [0:4]
    { x <- take; emit x }
}
fun comp g(y : inr) }
  for n in [0,y]
    emit n -- n is bound in the body
}
let comp main = read >>> { f() ; g(100) } >>> write
```
The syntax `for n in [0:4]` executes the processor in the body for `n = 0, n = 1, ... n = 4` times. The
syntaxi `for n in [0,y]` executes the processor in the body from `n = 0`, incrementing `n` in the end of the
loop, and as long as `n < y`. The syntactic forms `[0:4]` and `[0,y]` are _range_ specifications and will also
be useful when we describe array manipulation later on.

Notice finally that in every iteration the computer in the body may take or emit an arbitrary number of elements,
the `for`-loop makes sure that the inner computer will be re-initialized and iterated a specific number of times.
