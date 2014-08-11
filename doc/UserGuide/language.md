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

let comp scrambler() =
  var scrmbl_st: arr[7] bit := {'1,'1,'1,'1,'1,'1,'1}; 
  var tmp: bit;
  var y:bit;
  
  repeat
    seq {
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
in

-- main computation goes here
read >>> scrambler() >>> write
```

The above code defines a _computation_ function called `scrambler`, that accepts the empty list of parameters.
The fact that this is a `comp`-utation function is apparent from the `let comp scrambler() = ...` line. Ignoring
for a moment the actual definition of the scrambler, let us first see how it is being used:

```gcc
let comp scrambler() = ...
in
read >>> scrambler() >>> write
```
What is happening in the last line above? We have used the `>>>` combinator to stage our first pipeline:

1. we `read` from the input buffer (we shall see later how to configure where that input is coming from);
2. we pass every single bit of that input through our `scrambler()` component;
3. and we `write` every single bit of the output of our scrambler to the output buffer. (we shall see later on how to configure where that output should be going)

The fact that we pipe the output of the primitive `read` combinator to the input of the `scrambler()` block and its output to the input of the `write` block is clear from our use of `>>>` in-between these blocks. 

Finally, a rule: *every Blink program must be performing `read` from some input and `write` to some output*. The Blink type checker will reject programs with unspecified input or output.

Now, it is time to dive into the implementation of the scrambler.
```gcc
let comp scrambler() =
  var scrmbl_st: arr[7] bit := {'1,'1,'1,'1,'1,'1,'1}; 
  var tmp: bit;
  var y:bit;
  
  repeat
    seq {
      x <- take;
    
      do {
        tmp := (scrmbl_st[3] ^ scrmbl_st[0]);
        scrmbl_st[0:5] := scrmbl_st[1:6];
        scrmbl_st[6] := tmp;
        y := x ^ tmp;
      }; 

      emit y
    }
in ...
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
seq {
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
seq {
  c;
  repeat c;
}
```

which is also equivalent to:

```
seq {
  c;
  c;
  ...
}
```

### Going in more depth: structuring stream processors

Here is a summary of the constructs we have seen so far for building stream processors:

1. `take`: takes one element from the input stream.
2. `emit`: emits one element to the output stream.
3. `seq { c1; c2 }` sequences some stream processors, all of which read/write to the same stream, by
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
let f(x : int) =
  var y : int = 42;
  y := y + 1;
  return (x+y);
in

read >>> map f >>> write
```

We will go to more details about expression-level functions later
on. For the moment you may think of them as ordinary C functions.

For computations that are structured as sequences of `take`-ing one element, `do`-ing some computation without keeping local state and `emit`-ing the result on the output, it is recommended that they are structured as `map`s of
some function, because this fuses 3 dataflow processing blocks to just one so the execution overheads are smaller. 
That said, the Blink compiler will typically detect and optimize this pattern anyway. 
	

#### `while`: Running computations until some condition is met

Our `while` combinator allows you to run a stream processor while some condition is true.

```haskell
let comp f(x: int) =
  var y : int;
  seq { do { y := x; }    -- init
	  ; while (y > 0)
	     seq { w <- take
	         ; emit (w+1)
		     }
	  ; do { y := y-1 }   -- decrement
	  }
in

read >>> f(42) >>> write
```

#### Just `return` a value

We have seen that we can embed an imperative piece of code using the `do {...}` notation.
Sometimes we simply need to be able to `return` a value without executing some statements.
Here is an example:

```
let comp f(x : int) =
  var y : int := 0;
  seq { a <- seq { do { y := y + x; }
                 ; z <- take
                 ; emit (z+y);
                 ; return y
                 }
      ; emit a
      }
in ...
```

In sequence, we first perform another sequence of commands, which involve updating the value of `y`, then
`take`-ing one value from the input stream, emiting `z+y` on the output stream and `return`-ing the updated
value. That updated value becomes the return value of the whole sub-sequence

```
seq { do { y := y + x; }
    ; z <- take
    ; emit (z+y);
    ; return y
    }
```

that we bind to variable `a` and finally `emit` it on the output stream.


#### `if`: Conditionally run one of two processors

Here's an example of conditionals:

```
let comp f() =
   seq { x <- take
       ; if (x > 0) then
            emit (x+1)
         else return ()
       }
in
read >>> f() >>> write
```

In `seq`-uence, we first consume an element from the input stream with `x <- take`. Subsequently, if `x > 0`
then we emit an element in the output stream (`emit (x+1)`), or `return` control otherwise. The effect of this
computer is that it will keep on incrementing all positive values of the input stream and emitting those to the
output stream. However, once it meets the first non-positive one the whole computation will stop.

What will the following code do then?

```
read >>> repeat f() >>> write
```
It will run `f()` to completion, that is, incrementing all positive input values and emitting them. Once a non-positive value we will reinitialize `f()` and restart. The effect will be that the code above keeps only the positive values of the input stream and emits them, incremented, on the output stream.


#### `for`: loops

Blink provides a simple looping structure where you do not have to provide the increment value.

```
let comp f() =
  for n in [0:4]
    seq { x <- take; emit x }
in
let comp g(y : inr) =
  for n in [0,y]
    emit n -- n is bound in the body
in
read >>> seq { f() ; g(100) } >>> write
```
The syntax `for n in [0:4]` executes the processor in the body for `n = 0, n = 1, ... n = 4` times. The
syntaxi `for n in [0,y]` executes the processor in the body from `n = 0`, incrementing `n` in the end of the
loop, and as long as `n < y`. The syntactic forms `[0:4]` and `[0,y]` are _range_ specifications and will also
be useful when we describe array manipulation later on.

Notice finally that in every iteration the computer in the body may take or emit an arbitrary number of elements,
the `for`-loop makes sure that the inner computer will be re-initialized and iterated a specific number of times.



## Formal syntax

We give here rules for the formal syntax of Blink. Comments in Blink are given with `--` for single-line comments and `{- ... -}` for multi-line comments. Some notation for our informal BNF syntax below:

1. We denote with `?(<prod>)` an optional production `prod`.
2. We denote with `(<prod>)*` zero or more times the production `prod`.
3. We denote wth `null` the empty production (just whitespace).
4. We use capitals to denote several syntactic classes:
     * `VARNAME` : identifiers (that is, consisting of alphanumeric characters and "_")
	 * `FLDNAME` : identifiers for field names
	 * `FUNCNAME`: identifiers for expression or computation functions
	 * `TYPENAME`: identifiers for named struct types
	 * `INTEGER` : integer literals like 1,2,3
	 * `STRING`  : string literals like "foobar\n"
	 * `FLOAT`   : floating point literals like 3.14

### Expression language


#### Types

The language of types is given by `<basetype>` that describe expression types. `int` and `complex` default to `int32` and `complex32` unless type inference determines otherwise.

```
<basetype>
  ::= bit | bool | double 
    | int | int8 | int16 | int32
    | complex | complex16 | complex32
    | struct TYPENAME
    | arr <basetype>
    | arr[INTEGER] <basetype>
    | arr[length(VARNAME)] <basetype>
```
Arrays can have either:

1. A programmer-unspecified length, that type inference is going to try to infer.
2. They can have a `INTEGER` literal length. As a convenience, the parser actually does support also simple expressions that can statically be evaluated to integer literals such as `1024 + 48`.
3. Arrays can specify fo have the length of another array bound earler, with `arr[length(VARNAME)] <basetype>`.

Hence you may well have:
```
let f(x : arr[32] int) =
  var y : arr int;              -- length inferred
  var z : arr[length(x)] int;   -- same as length of x
  y := x;                       -- fixes length of y to 32
  z := y;
  ...
in ... 
```

#### Expressions

First of all some operator definitions. Unary operators are:

```
-- Unary operators
<unop> ::= -
         | not
	     | ~
	     | length
```

Binary operators:

```
-- Binary operators
<binop> ::= **       -- exponentiation
          | *        -- multiplication
	      | /        -- division
	      | %        -- modulo

	      | +        
	      | - 

          | <<       -- shift operators
	      | >>

          | <        -- comparisons
		  | <=
		  | >
		  | >=

          | &        -- bitwise and
		  | ^        -- bitwise xor
		  | |        -- bitwise or

          | &&       -- logical and
          | ||       -- logical or
```

The binary operators are presented in several groups, highest-priority
group first, and within the same group highest-priority operator
first. They are what one would expect in a C or Pascal-like language. 

The actual syntax of expression follows: 
```
<expr> ::= <expr> binop <expr>
         | unop <expr>
		 | (<expr>)
		 | <term>
```
And terms are:
```
<term> ::= <value>

		   -- structure initialization
		 | TYPENAME { FLDNAME1 = <expr>; ... FLDNAMEn = <expr> }

           -- dereferencing expressions
         | <dexpr>

           -- casts 
		 | <basetype>(<expr>)
		 
		   -- function calls
		 | FUNCNAME(<eargs>)

         | -- conditionals
		   if <expr> then <expr> else <expr>

         | -- let definitions
		   let <varbind> = <expr> in <expr>

-- expression arguments 
<eargs> ::= null | <expr>(,<expr>)*

<varbind> ::= VARNAME | ( VARNAME : <basetype>) 

<dexpr> ::=
           -- variable
		   VARNAME

		   -- field projection
         | <dexpr>.FLDNAME
		 
           -- subarray derefencing
		 | <dexpr>[INTEGER:INTEGER]

           -- array element dereferencing
		 | <dexpr>[INTEGER]

<value> ::= ()                -- unit
		  | true | false      -- boolean values
		  | '0 | '1           -- bit values
	      | STRING            -- e.g. "blink"
		  | INTEGER           -- e.g. 1,2,5,6 ...
		  | FLOAT             -- e.g. 3.14
		  | { <values> }      -- array values

<values> ::= null | <value>(,<value>)* 

```

Now, in addition to the expressions and terms, Blink provides support for "imperative" blocks of statements.

```
<stmts> ::= null | <stmt>(;<stmt>)*?;
<stmtblock> ::= <stmt> | { <stmts> }
<stmt> ::=
    -- assignment
    VARNAME := <expr>
	
    -- return
  | return <expr>

    -- function call
  | FUNCNAME(<eargs>)

    -- for loops
  | for VARNAME in <range> <stmtblock>
  
    -- while loops
  | while <expr> <stmtblock>

    -- conditional commands
  | if <expr> then <stmtblock> ?(else <stmtblock>)

    -- let bindings
  | let <varbind> = <expr> ?(in <stmt>)

    -- print to stdout
  | print <exprs>
  | println <exprs>
  
    -- exit current thread with an error message
  | error STRING

<exprs> ::= <expr>(,<expr>)*

<range> ::=

    -- (inclusive) interval range
    [INTEGER:INTEGER]

    -- start and length based range
  | [<expr>,<expr>]
  
```

A caveat: currently we dont support parsing of nested array values.


### Computation language

First of all, the computation types that we support are: 
```
<comptype> ::= 
      ST T <basetype> <basetype>
    | ST (C <basetype>) <basetype> <basetype>
```

```
<comp> ::=
     -- pipeline composition
     <comp> >>> <comp> 
   | <comp> |>>>| <comp>
   | (<comp>)

     -- unary operators
   | repeat ?<vectann> <comp>
   | until <expr> <comp>
   | while <expr> <comp>
   | for VARNAME in <range> <comp>

   | return <expr>
   | emit <expr>
   | emits <expr>
   | takes <expr>
   | filter <expr>
   | read ?<basetype>
   | write ?<basetype>
   | map ?<vectann> <expr>

   | FUNCNAME(<callargs>)
   | VARNAME

   | <compterm>

-- hints to the vectorizer 
<vectann>  ::= [INTEGER,INTEGER]

<callargs> ::= null | <callarg>(,<callarg>)*
<callarg>  ::= <expr> | <comp>

<compterm> ::=
    -- conditionals
    if <expr> then <comp> ?(else <comp>)

    -- struct definitions
  | <structdefn> in <comp>
  
    -- embedding a sequence of statements
  | do <stmtblock>

  | let <binding> in <comp>

  | ?seq { <commands> }

<structdefn> ::= TYPENAME { FLDNAME1 : <basetype>; ... ; FLDNAMEn : <basetype> } 

<command> ::=
  | <structdefn>
  | <binding>
  | <varbind> <- <comp>
  | <comp>

<commands> ::= null | <command>(;<command>)*?;

<binding> ::=
    -- let binding
  | let <varbind> = <expr>

    -- expression function definition
  | let FUNCNAME(<params>) = <decls> <stmts>
  
    -- external C function declaration 
  | let external FUNCNAME(<params>) : <basetype>

    -- computation binding
  | let comp VARNAME = <comp>

    -- computation function binding
  | let comp FUNCNAME(<comparams>) = <decls> <commands>

-- Expression function parameters
<params> ::= null | <param>(,<param>)*
<param>  ::= VARNAME : <basetype>

-- Computation function parameters
<compparams> ::= null | <compparam>(,<compparam>)*
<compparam>  ::= VARNAME : <comptype> | VARNAME : <basetype>
  
-- Variable declaration
<decls> ::= null | (<decl>;)*
<decl>  ::= var VARNAME : <basetype> [ := <expr> ]

```


### The main program

```
<program> ::= <decls> <comp>
```


   
   

		 


