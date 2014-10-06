# The Ziria type system

This document is work in progress and currently mostly outlines design
decisions rather than give the full specification of the type system.

## Decision decisions

### Length polymorphism

Functions in the expression language can be polymorphic in the length of arrays:

```
fun f(arr int x, arr[length(x)] int y) {
  ...
  return y;
}
```

after desugaring the type of `f` looks like

```
arr[_r] int -> arr[_r] -> arr[_r]
```

### Polymorphic computation primitives

The computation language contains a number of polymorphic primitives:

```
take   :: ST (C a) a b
emit   :: b -> ST (C ()) a b
return :: v -> ST (C v) a b 
map    :: (a -> b) -> ST T a b
```

However, this polymorphism is not first class, in the sense if the type of `f`
in

```
fun comp f() {
  take 
}
```

is not uniquely determined by the context the type checker will reject `f`.

### No other kinds of polymorphism

Length polymorphism is the _only_ source of polymorphism that is permitted in
user defined functions (both in the expression and in computation language). 

### No dependent types

We support polymorphism, but we do _not_ support dependent types. For instance,

```
fun g(int n) {
  return (<<something of type arr[n] int>>)
}
```

is _not_ permitted. If it was, what would be the type of

```
fun h2(int n, int m) {
  return g(n + m);
}
```

or, possibly worse, of

```
fun h1() {
  var x;
  // some imperative computation
  return g(x);
}
```

## Implementation consequences

Ziria works with an internal typed language in which all variable occurrences
have been marked with their type. Functions are not first class so they cannot
be bound to variables, but computations _are_ first class and can be passed
as arguments to functions.

This means that in the expression language we can always infer the type of a
subexpression. The _only_ source of polymorphism is length polymorphism in
functions, but since we do not support partial application we always have all
function arguments available and hence we can always find out the type.

In the computation language we have polymorphic primitives (emit, return, take,
etc) but they are _always_ used at a monomorphic type (remember that we do not
allow for user-defined polymorphism). So we annotate these primitives with the
`Ty`s that they are instantiated at, where necessary; for primitives such as
`emit` or `return` some of the type variables are ddetermined by the argument
of the primitive.


