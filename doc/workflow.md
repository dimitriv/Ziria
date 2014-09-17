Workflow
========

Branches
--------

The `release` branch is the main branch, which should be stable. That is, the
tests in tests/parser, tests/backend, and code/WiFi (see below) should all
pass, and should be regression free.

For now, can create topic branches for everything else (and somebody who has
the full setup can run the WiFi tests before merging).

Building
--------

Currently Cabal is not used to build wplc; the .cabal file in the root
directory is there only for dependency tracking. Instead, build by calling
'make'. (TODO: Change this.)

Running the test suite
----------------------

Generally the aim is to have a single Makefile target per test. 

### tests/parser

The default target (`all`) in this directory test _only_ whether it parses or
not. The programs in this directory are not intended to be run, so there are no
.outfiles, .infiles, or .ground files (see below). There are only .wpl (ziria
source) files, which

1. get converted to %.wpl.expanded by running cpp
2. then gets compiled to %.c by running wplc
3. finally get compiled to an executable %.out by running gcc (or other compiler)

By changing the Makefile to use 

```
PP   = ../../scripts/compile.sh
```

instead only step (2) is executed.

In addition, there is a `parsertests` target which uses `wplc` to dump a parse
tree (`%.wpl.expanded.ast.dump`) and compares it to a previously generated 
parse tree (`%.wpl.expanded.ast.dump.ground`). Currently these ground AST dumps
are not in the repo as they can get quite big. You can generate them locally
with

```
for i in *.wpl; do n=`basename $i .wpl`; make $n.ast.accept; done
```

### tests/backend

This contains programs that are meant to be run. It contains .wpl source files,
and for each .wpl file

* a .infile file with a test input stream
* a .ground file with the expected result output stream

By calling make in this directory the program is compiled and then run on the
example input stream, and the BlinkDiff tool is used to compare the result
to the .ground file. To generate or update a .ground file can use the `accept`
target:

```
make var-conflict.accept
```

This has to be done on a target by target basis.

In addition, the `parsertests` target is available here too.

### code/WiFi

This directory contains the code for the actual 802.11a/g implementation, which
is itself a very important test case. Running the associated tests (`runTests`)
requires WinDDK, SORA, and a Windows machine.

However, the `parsertests` target is available in code/WiFi/receiver/tests,
code/WiFi/transmitter/tests, and code/tests, and does not need a C compiler at
all.
