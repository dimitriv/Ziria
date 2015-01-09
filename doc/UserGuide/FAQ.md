# Frequently Asked Questions

## Take/Emit-related Questions

### Question:
Why isn't `x <- take` an ordinary command (at the computation language level) like all others (e.g. like `x := 3`).

### Answer: 
I know it's a bit of a mind-shift but the thing to realize is that: 
```gcc
      x <- take 
```
is not a standalone syntactic entity in Ziria. It's not compatible with our compilation model, and not compatible with our type system. The general form of the language construct is:
```gcc
      seq { x <- c1; c2 } 
```
where the scope of "x" is over c2 and "c1" could be *any* Ziria computer - one of which is 'take' of course. Hence, there always has to be something (c2, in this case) following a "bind" the (x <- c1 part). It's called "bind" internally because it binds a new immutable variable. Why is that? If this is not the case then the compiler has to take some decisions about what is the *type* of a standalone
```gcc
x <- c 
```
to emit the necessary block in the pipeline graph. Possible options:
- Should it be `ST (C ()) a b`, which amounts to - in effect - `seq { x <- c; return () }` ??
- Should it be `ST t a b`, which amounts to - in effect - `seq { x <- c; return x }`  (which is also equivalent to just  c ) 
     
There are situations where you want the former, and situations where you want the latter, but it is not a good programming language design to force one choice on you. We had not managed to reach a conclusion even between us about if one is preferred over the other, so it seems that the Right Thing here is to disallow this ambiguity and let the programmer choose to do whatever fits them. 

Moreover, it does seem highly suspicious from a code portability point of view to have a standalone x <- c since 'x' is introduced as a free variable and *never* used. 

### Question:
Ok fine, but why don't you just give me an `x := take` possibility and maybe in general a `x := c` possibility. For instance don't introduce the 'x' but rather assign to an already declared 'x'.

### Answer:
Let me start with why `x := c` is a bit problematic. As you may or may not know our current story of  `{ x <- c1 ; c2 }` mimicks the "bind" construct of a monad. One of the nice properties of a monad, not because it's fun math but because it's one the optimizer is very heavily using, is the following:     
```gcc
seq { x <- return e ; c }  ===  let x = e in c.  
```
Notice that this is a "let" -- *not* a letref. Hence the compiler knows that this is an immutable variable and can potentially inline it (depending on the side-effects of e) in the continuation 'c'. However, consider what would happen if your code suddenly becomes:    
```gcc
{ x := return e; c }. 
```
We can only convert this to a "`var x := e in c`" or maybe just  `do { x := e } ; c` but "var"-bound bindings are *never* inlined because *who knows* how x is used imperatively inside c. So suddenly, by switching from `x <- c1` to `x := c1` you create a barrier for inlining. I could give you the option to use `x := c` very judiciously, perhaps just for 'take' (for efficiency reasons to avoid extra memcopies) but we'd really have to think of a design that would discourage programmers from using it.  You may say "This is all crap, I will not screw up and I will be very conscious about optimizations, just give me `x := take` option". But the optimizations are so aggressive that you may think that `x := take` is harmless but it could in fact be a blocker. For example:
```gcc
                emit v >>> seq {  x <- take ; transformer } 
```
in principle could be converted to:
```gcc
                let x = v in transformer
```
but 
```gcc
                emit v >>> seq { x := take; transformer } 
```
would not as the only semantically correct transformation is:
```gcc
                letref x := v in transformer }
```
and this is an inlining barrier. 

The only good argument I see is efficiency for assignments to *mutable* arrays. It's less efficient to do:       
```gcc
seq { x <- take;  do { y := x } } 
```
than just do `y := take`. This is correct, and I am going back and forth on this one and whether it's a problem of the optimizer or a shortcoming of the language construct. Why could this be considered as an optimizer shortcoming? Because here the compiler can perfectly and easily see the scope of "x" and make sure that he assigns directly to "y" in the generated code. On the other hand, we allow programmers to use "let" and "var" and explain to them very carefully the tradeoffs - if you use "var" you will get less inlining. Maybe it's the same analogy, and we should provide a x := c computation language command (and maybe even  `x[...range...] := c` command) and explain to users what is going on ... 


### Question:
The following code fails to compile:
```gcc 
  repeat seq {
    if (sym_cnt == 0) then {
      (s:arr[2048+160] complex16) <- takes 2048+160;
    } else {
      (s:arr[2048+144] complex16) <- takes 2048+144;
    }
```

With the message:
```gcc
"../demodulate.zir" (line 46, column 5):
unexpected symbol "}"
expecting command
last statement in a seq block should be a computation
```
 
How can I take a variable amount of samples according to the symbol number?


### Answer:
I hope the previous Q/A explains why we reject your code:
```gcc
  repeat seq {
    if (sym_cnt == 0) then {
      (s:arr[2048+160] complex16) <- takes 2048+160;
    } else {
      (s:arr[2048+144] complex16) <- takes 2048+144;
    }
```
Fails to compile with an unexpected "}". Now, your workaround is:
```gcc
  repeat seq {
    s <- takes 2048+144;
    if (sym_cnt == 0) then { 
      ignore <- takes 16;
      do { ignore[0] := ignore[0] } -- HACK: "last statement in a seq block should be a computation"
    }
```
Which is ugly - you should be able to just replace your HACK with sth like: 
```gcc
   repeat { 
      x <- takes 2048+144;
      if (sym_cnt == 0) then { takes 16; return ();}
   }
```
I know you rarely use return () directly, but it is extremely useful to make sure that a computation returns the ()-type. 

