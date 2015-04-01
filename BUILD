To build the Ziria (wplc) compiler (under Linux or Windows/Cygwin) you will
need:

* GHC 7.8.3 or greater
  On Windows it is best to install Haskell Platform for Windows (2014.2 or
  later), which also includes cabal.
  Also, make sure you use the same bit-widths for Haskell and Cygwin.
  (64-bit Cygwin is known to have issues with 32-bit Haskell)

* cabal 1.18 or greater
  This is included with the Haskell Platform 2014.2 or later.

  NOTE: Make sure yous Haskell Platform path
        (e.g. C:\Program Files\Haskell Platform\2014.2.0.0\mingw\bin)
        is before your Cygwin GCC path in Windows PATHs because cabal needs
        the Haskell Platform ld and not the Cygwin GCC one (otherwise horrible
        errors will occur)

In the root directory, run `cabal update` and then `make`.

The Makefile creates a cabal sandbox, installs dependencies from Hackage, and
finally. You will be left with a `wplc` binary.

For many examples outside of the main path one needs to set Ziria path:
set `ZIRIA_ROOT=/path/to/Ziria/wplc/compiler`

NOTE: If you update GHC on Windows, make sure you delete ghc and cabal dirs 
in `/Users/<username>/AppData/Roaming` to avoid stale state problems.

* Running the basic tests:

  Pre-requisites: You must have gcc installed in your Linux or Cygwin 
  environment. Go to tests/backend or tests/parser and type 'make'.

  Ziria compiler (wplc) produces C source code that needs to be
  further compiled with a C compiler. We currently support 3 C
  environments: gcc, WinDDK v7 and Visual Studio.  In order to use
  WinDDK, you need to set WINDDK_ROOT path.  In order to use VS, you
  need to set ZIRIA_VS to point to where vcvarsall.bat file is located
  (e.g. ZIRIA_VS="C:\Program Files (x86)\Microsoft Visual Studio
  12.0\VC" for VS2013). To use different C compiler with tests, run:

    COMPILER={gcc,winddk,vs} make



* Running the WiFi tests:

  To compile and run performance tests and the WiFi testsuite (in
  code/WiFi) you must have SORA installed. We currently support Sora
  2.0, although older versions can easily be adapted.  To build Sora,
  you need WinDDK version 7 (see Sora pages for more details).  Once
  Sora is build, you can use either WinDDK or Visual Studio
  environments. More documentation can be found in doc/



* Linking with Blade-RF

  Add path to blade-RF:
    BLADE_RF=C:\Program Files\bladeRF



* Ziria mode in Emacs

  First install haskell-mode for emacs from a standard repository.
  Then add the following to .emacs:

  (add-to-list 'load-path "<path-to-ziria>\\tools")
  (require 'blink-mode)
