For Linux instructions, please refer to doc/Linux.md

To build the Ziria (wplc) compiler (under Windows/Cygwin) you will
need:

* GHC 7.8.3 (GHC 7.10 is currently NOT supported)
  On Windows it is best to install Haskell Platform for Windows (2014.2), 
  which also includes cabal.
  Also, make sure you use the same bit-widths for Haskell and Cygwin.
  (64-bit Cygwin is known to have issues with 32-bit Haskell)

* cabal 1.18 or greater
  This is included with the Haskell Platform 2014.2.
  

  NOTE: Order of paths may make a difference, consult this if
		an error occurs when running 'make'.
		Try changing your Haskell Platform path
        (e.g. C:\Program Files\Haskell Platform\2014.2.0.0\mingw\bin)
        to be before your Cygwin GCC path in Windows PATHs because cabal needs
        the Haskell Platform ld and not the Cygwin GCC one (otherwise horrible
        errors will occur) 
		Do this by going to 'Advanced system settings'->'Enviroment variables'
		and then edit the system variable called 'Path'.
		Then for example we would want to replace for example:
		C:\cygwin64\bin;C:\Program Files\Haskell Platform\2014.2.0.0\mingw\bin
		with 
		C:\Program Files\Haskell Platform\2014.2.0.0\mingw\bin;C:\cygwin64\bin 
		


In the root directory, run `cabal update` and then `make`. This may take some
time to compile. If an error occurs, try changing the paths as mentioned above,
or just try running it again.

The Makefile creates a cabal sandbox, installs dependencies from Hackage, and
finally. You will be left with a `wplc` binary.

For many examples outside of the main path one needs to set Ziria path:
I.e. go to 'Advanced system settings'->'Enviroment variables' and 
enter a new system variable with name ZIRIA_ROOT and value (e.g.)
"C:\cygwin\users\name\Ziria\wplc" or whatever your path to Ziria is.


NOTE: If you update GHC on Windows, make sure you delete ghc and cabal dirs 
in `/Users/<username>/AppData/Roaming` to avoid stale state problems.    

Go to Ziria/tests/backend and run 'make', this should compile with no errors.

* Running the basic tests:

  Pre-requisites: You must have gcc installed in your Linux or Cygwin 
  environment. Go to tests/backend or tests/parser and type 'make'.

  Ziria compiler (wplc) produces C source code that needs to be
  further compiled with a C compiler. We currently support 3 C
  environments: gcc, WinDDK v7 and Visual Studio.  In order to use
  WinDDK, you need to set WINDDK_ROOT path.  In order to use VS, you
  need to set ZIRIA_VS to point to where vcvarsall.bat file is located
  (e.g. create a new system variable with name "ZIRIA_VS" and value
  "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC" for VS2013).
  To use different C compiler with tests, run:

    COMPILER={gcc,winddk,vs} make

  Note that when setting the environment variable ZIRIA_VS and ZIRIA_ROOT, 
  the double quotes (for the path) are necessary. 
  Make sure to reboot Cygwin64 or related programs after set/reset system
  environment variables. Use $export in Cygwin command line to check if the 
  environment variables are successfully setted. 


* Running the WiFi tests:

  To compile and run performance tests and the WiFi testsuite (in
  code/WiFi) you must have SORA installed. We currently support Sora
  2.0, found at https://sora.codeplex.com/, although older versions
  can easily be adapted.  To build Sora, you need WinDDK version 7 
  (see Sora pages for more details).  Once Sora is built, you can
  use either WinDDK or Visual Studio environments. 
  More documentation and instructions about how to download and install
  Sora and WinDDK can be found in /doc.
  setting Sora Path: SORA_ROOT = C:\SoraSDK2.0

* Linking with Blade-RF
  Make sure BladeRF board is connected with PC throught USB3.0
  Also make sure the BladeRF is installed with libusb driver instead of
  CyUSB driver or any driver else 
  Add path to blade-RF:
    BLADE_RF=C:\Program Files\bladeRF
    Or
    BLADERF_PATH=C:\Program Files\bladeRF


* Ziria mode in Emacs

  First install haskell-mode for emacs from a standard repository.
  Then add the following to .emacs:

  (add-to-list 'load-path "<path-to-ziria>\\tools")
  (require 'blink-mode)
