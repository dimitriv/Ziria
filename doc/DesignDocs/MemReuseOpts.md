# Memory Reuse Optimizations Notes

Different low-level optimizations are implemented in the Ziria compiler in order to avoid unnecessary memory copying and its impact on performance. Special optimized cases mainly focus on reducing memory allocation and assignments when dealing with arrays. The following example shows one of the cases in which memory reuse is applied: local function array variables that are used as the return value of a function (i.e `oarr`) don't need to be allocated, and their contents should not be copied to the function caller. Instead, already allocated space in the caller is used to store the result value:

```
let perm(p: arr int,iarr: arr[length(p)] bit) =
  var oarr: arr[length(p)] bit;
  for i in [0,length(p)] {
    oarr[p[i]] := iarr[i]
  }
  return oarr
in
```

Other cases where no extra allocation is needed are those of $\keyw{let}$-bound variables that are assigned an immutable value. In expressions such as: 

```
let x = {1,2,3} in e
let x = y[...] in e
```

`x` can simply be a pointer to the allocated space for 
the value, thereby evading memory copying. 
Following this reasoning, `take` components can also be optimized when requesting arrays of elements from the input stream. The control value returned as the computation result does not need to be a copy of the input value, but a pointer to the already allocated space used to hold the input array.

Another special case to consider is when writing into arrays: whenever the right-side of the expression returns a variable bounded in the scope of the assignment (e.g a `let`-bound variable), the variable itself can be replaced by the left-side expression of the assignment. 
Examples below show how `oup` is replaced by `vect_ya` in the optimized code so that no extra allocation or memory copying is needed to store the contents of `vect_ya`:

```
let auto_map(vect_xa: arr[160] complex16) =
 letref vect_ya: arr[80] complex16
 in
 let _unused_1 = 
  for vect_j in [0, 20] {
   let y = vect_xa[vect_j*8:+8]
   in
   vect_ya[vect_j*4:+4] := 
    let _unused_2 = 
     permutatew(y[0:+4],tmp1);
     permutatew(y[4:+4],tmp2)                          
    in
    letref oup: arr[4] complex16
    in
    interleave_loww(tmp1,tmp2,oup);
    oup 
   }
  in
  vect_ya
in
map auto_map
```
```
let auto_map(vect_xa: arr[160] complex16) =
 letref vect_ya: arr[80] complex16
 in
 let _unused_1 = 
  for vect_j in [0, 20] {
   let y = vect_xa[vect_j*8:+8]
   in
   let _unused_2 = 
    permutatew(y[0:+4],tmp1);
    permutatew1313(y[4:+4],tmp2)
    in
    interleave_loww(tmp1,tmp2,vect_ya[vect_j]);
   }
  in
  vect_ya
in
map auto_map
```