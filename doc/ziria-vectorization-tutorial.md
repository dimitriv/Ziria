Here is an example:
 
fun comp rx() {
  var x : arr[4096] complex16
  repeat {
    x <- takes 4096;
    emits x;
  }
}
 
let comp main = read[complex16] >>> rx() >>> write
 
Takes means take multiple complex16 scalars and pack them into a vector. Similarly, emits means take a vector and emit multiple scalars. 
Furthermore, you have read[complex16] which forces the input to be a scalar. Write doesn’t have a type because it is automatically induced. 
This would have worked for the input in this case too (you could write just read).
 
How do you make it vector. Here is the same example as vector:
 
fun comp rx() {
  var x : arr[4096] complex16
  repeat {
    x <- take;
    emit x;
  }
}
 
let comp main = read[arr [4096] complex16] >>> rx() >>> write
 
Here take any variable (scalar or vector) and stores it in the same type. So the input of take and the storage have the same type (unlike with takes). 
Similarly for output. Further, you explicitly declare the read to read 4096 complex16 vectors. Again, you don’t need to specify the type of read (input) here, it should be automatically deduced. 
You only need to do it in programs where it is ambiguous (for example a program let comp main = read >>> write has to assign some type to the input but has no clue which one).
 
Finally, the array stuff is messy, why should you do it manually. Indeed, you shouldn’t, and this is what the vectorization is all about. 
If you compile the first program with --vectorize, it should do that for you and automatically convert scalar input to vector. 
The trick is to determine which size. In this case it is easy, but in a more general case it is difficult. This is why we have implemented the vectorizer. The details are in the ASPLOS paper, but in practice if you may want to check the output of the vectorizer to make sure it did what you expect it to do. In most cases it will do a good think, but some time you may have surprises. For example, in this case the vectorizer is not going to create 4096 vector but 128 or something, because it is the maximum, and probably fast enough. You can display the summarized vectorizer output using --ddump-vect and check what it did. However, you can always force it to anything you want in the way I explained.