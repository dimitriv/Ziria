To build the Ziria (wplc) compiler (under Linux or Windows/Cygwin) you will need:

* GHC 7.6 (including a cabal installation)
* The cabal packages (install them with 'cabal install <package-name>' within cygwin shell)
   - text-1.1.1.3
   - parsec
   - mainland-pretty
   - language-c-quote 
   - dlist
   - unix-compat

Then go to src/ and type 'make'. You will be left with a 'wplc' binary.


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
