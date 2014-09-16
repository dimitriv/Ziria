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

The parser tests currently test _only_ whether it parses or not. (TODO: Verify
parse trees.) The programs in this directory are not intended to be run, so
there are no .outfiles, .infiles, or .ground files (see below). There are only
.wpl (ziria source) files, which

1. get converted to %.wpl.expanded by running cpp
2. then gets compiled to %.c by running wplc
3. finally get compiled to an executable %.out by running gcc (or other compiler)

By changing the Makefile to use 

```
PP   = ../../scripts/compile.sh
```

instead only step (2) is executed.

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

### code/WiFi

This directory contains the code for the actual 802.11a/g implementation, which
is itself a very important test case. Running the associated tests (`runTests`)
requires WinDDK, SORA, and a Windows machine.  (TODO: Split so that can run
the compiler separately, so that we can at least test that we get C output.)
