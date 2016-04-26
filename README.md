# Ziria [![Build Status][status]](https://travis-ci.org/dimitriv/Ziria)

Ziria is a programming language and compiler for low-level bitstream processing.
Check the [project website](http://research.microsoft.com/en-us/projects/ziria/).


### News

* Ziria now features a standard compliant LTE and LTE-U implementation. Check [Open Test Platform for LTE/LTE-U](https://github.com/Microsoft/OTP4LTE-U). 

* Ziria features standard compliant [802.11a/g PHY implementation](https://github.com/dimitriv/Ziria/tree/master/code/WiFi).

* Ziria works with [Sora](https://github.com/Microsoft/Sora/) and [BladeRF](https://github.com/Nuand/bladeRF).



### Some useful information:

* Top-level directory structure:

| Directory | Description                                            |
| ---------:|:------------------------------------------------------ |
|     src/  | Haskell source code                                    |
|   tools/  | A few useful tools                                     |
|     lib/  | Some simple libraries                                  |
|    csrc/  | C code and C libraries                                 |
|   tests/  | Testsuite                                              |
|    code/  | WiFi implementation code                               |
|     doc/  | Various documentation and design notes                 |
| scripts/  | Various scripts for building in different environments |

* Building the compiler: see [BUILD](https://github.com/dimitriv/Ziria/blob/master/BUILD)

[status]: https://travis-ci.org/dimitriv/Ziria.svg?branch=master

