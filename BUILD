To build the Ziria (wplc) compiler (under Linux or Windows/Cygwin) you will need:

* GHC 7.8 (including a cabal installation). 
  On Windows it is best to install Haskell Platform for Windows, which contains everything.
  Also, make sure you use the same bit-widths for Haskell and Cygwin
  (64-bit Cygwin is known to have issues with 32-bit Haskell)

* The cabal packages (install them with 'cabal install <package-name>' within cygwin shell)
   - text
   - parsec
   - mainland-pretty
   - language-c-quote 
   - dlist
   - unix-compat (on Windows you might need to install this one from cmd rather than cygwin shell)
   - hashable
   - pretty-show

  NOTE: Make sure yous Haskell Platform path is before your Cygwin GCC path in Windows PATHs
        because cabal needs Haskell Platform ld and not the Cygwin GCC one
	(otherwise horrible errors will occur)

  NOTE: parsec 3.1.6 contains a regression bug (https://github.com/aslatter/parsec/issues/9);
  use an older version for now.

Then go to src/ and type 'make'. You will be left with a 'wplc' binary.

For many examples outside of the main path one needs to set Ziria path:
set ZIRIA_ROOT=/path/to/Ziria/wplc/compiler

NOTE: If you update GHC on Windows, make sure you delete ghc and cabal dirs 
in /Users/<username>/AppData/Roaming to avoid stale state problems.


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



* Ziria mode in Emacs

  First install haskell-mode for emacs from a standard repository.
  Then add the following to .emacs:

  (add-to-list 'load-path "<path-to-ziria>\\tools")
  (require 'blink-mode)
