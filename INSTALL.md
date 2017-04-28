# Installing Ziria

To build the Ziria (wplc) compiler you will need:

 * GHC 7.8, 7.10, or 8.0. We assume the use of 8.0 in the remainder of this document.
 * `cabal` >=1.18. This is included in the Haskell Platform.
 * `alex` and `happy`, both of which are also included with the Haskell Platform.
 * `gcc`
 * `make`

## Installing Windows prerequisites

On Windows it is best to install the **full** Haskell Platform for Windows
(8.0.1), which also includes cabal. We recommend building Ziria under msys2,
although it is also possible to use cygwin. Make sure that you consistently use
either the 32-bit or 64-bit versions of all tools (Haskell Platform, msys2, and
cygwin).

The (64-bit) tools we currently recommend for Windows can be found here:

 * [msys2](https://msys2.github.io/)
 * [cygwin](https://www.cygwin.com/)
 * [Haskell Platform 8.0.1](https://www.haskell.org/platform/)

You should install msys2 and cygwin **before** installing the Haskell Platform
so that the platform's binaries appear earliest in your `PATH`—this will help
avoid issues where GHC attempts to use the msys2 or cygwin tools, e.g., `ld`,
instead of the versions bundled in the platform.

When using msys2, you will need to make sure tools like `git` and `make` are installed. This can be done as follows:

    pacman -S git
    pacman -S make

By default, msys2's `PATH` does not include the full Windows `PATH`. This must be fixed in order to use GHC. You can do this as follows:

 1. Find the `MSYS2 MSYS` shortcut.

 2. Change the shortcut's target from `C:\msys64\msys2_shell.cmd -msys` to `C:\msys64\msys2_shell.cmd -msys -use-full-path`. Note the addition of the `-use-full-path` argument.

Running the tests also requires WinDDK and/or Visual Studio (we use 2013).

### Installing WiFi implementation prerequisites

Running the Ziria WiFi implementation requires the Sora SDK and WinDDK. Install
them as follows:

 1. Install the WinDDK, which can be found at
 [http://www.microsoft.com/en-us/download/details.aspx?id=11800](http://www.microsoft.com/en-us/download/details.aspx?id=11800).
 The installer should install version 7600.16385.1 of the WinDDK in `C:\WinDDK`.

 2. Install the Sora 2.0 SDK, which is available from [https://sora.codeplex.com/](https://sora.codeplex.com/).

 3. Set the environment variable `SORA_ROOT` (with capital letters) to the
 directory where Sora was installed, e.g., `C:\SoraSDK2.0`.

 4. Run "x64 Free Build Environment" (a CMD-type icon in your start menu). **Do
 not try to compile Sora from a standard `cmd.exe` shell or from `cygwin`**—it
 will fail! If you cannot find "x64 Free Build Environment" in the start menu,
 look in `C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Windows Driver
 Kits\WDK 7600.16385.1\Build Environments\Windows 7`

 5. Change to the `src` subdirectory of the Sora directory, e.g.,
`C:\SoraSDK2.0\src`.

 6. Run `set_dirs_x64.cmd` to set up the correct paths.

 7. Run `bcz` to build the Sora libraries. Once this is done, you should see the
 following libraries:

  * In `C:\SoraSDK2.0\lib\fre_win7_amd64\amd64` you should see `usora.lib`

  * In `C:\SoraSDK2.0\target\fre_win7_amd64\amd64` you should see:
  `dot11bbbu.lib`, `dot11bbau.lib`, `libbba_lutstu.lib`, `libbba_modu.lib`,
  and `sora_utility.lib`.

 8. Repeat steps 5 through 7 for the "x64 Checked Build Environment".

If you are not using the amd64 architecture, you can build using the appropriate
environments:

 * IA64:
  - IA-64 Free Build Environment
  - IA-64 Checked Build Environment

 * x86
  - x86 Free Build Environment
  - x86 Checked Build Environment

To test that everything is set up correctly, run `make test-WiFi-RX` in the
top-level Ziria directory after building Ziria (it may take some time to
complete). If these tests pass, both WinDDK and Sora are correctly set up.

### Linking with Blade-RF

Make sure the BladeRF board is connected via USB 3.0. Also make sure that
BladeRF is installed with the libusb driver instead of the CyUSB driver or any
other driver. You must also define one of the following environment variables:

    BLADE_RF=C:\Program Files\bladeRF

or

    BLADERF_PATH=C:\Program Files\bladeRF

## Installing Linux prerequisites

Ziria on Linux depends on GCC with developer headers as well as the components
mentioned above. Instructions for Ubuntu are provided below. If you do not use
Ubuntu and your distribution does not provide the exact versions of the packages
required, you may obtain the required packages using either the Nix package
manager, or using a Docker container with Ubuntu. Both Nix and Docker have been
tested in the past

### Ubuntu instructions

```bash
$ # Install essential build tools
$ sudo apt-get install binutils build-essential git

$ # Herbert's PPA on Ubuntu provides most of the required packages
$ sudo add-apt-repository ppa:hvr/ghc
$ sudo apt-get update

$ # This is the recommended version of GHC
$ sudo apt-get install ghc-8.0.1

$ # You could also install this in the sandbox if you wish
$ sudo apt-get install cabal-install-1.24

$ # The distribution version works
$ sudo apt-get install alex-3.1.7
$ sudo apt-get install happy-1.19.5

$ # Update cabal
$ /opt/cabal/1.24/bin/cabal update

$ # cd into whatever directory you wish, and grab the repo
$ git clone https://github.com/dimitriv/Ziria && cd Ziria

$ # Make using specific versions of GHC and cabal-install
$ GHC=/opt/ghc/8.0.1/bin/ghc CABAL=/opt/cabal/1.24/bin/cabal make

$ # Optionally grab the bladeRF library headers
$ sudo apt-get install libbladeRF-dev
$ # Setup the bladeRF board, and flash the device before use.
$ # You don't need to setup GNURadio with bladeRF support.
```

## Building Ziria

In the root Ziria directory, run `cabal update` and then `make`. It may take
some time to perform a full build. If an error occurs, check your `PATH` as
described in the FAQ.

The Makefile creates a cabal sandbox, installs dependencies from Hackage, and
finally compiles the Ziria compiler. You will be left with a `wplc` (or
`wplc.exe`) binary.

If everything compiler correctly, `make` in `Ziria/tests/backend` should
complete with no errors.

## Installing the Ziria emacs mode

First install `haskell-mode` for emacs from a standard repository. Then add the
following to your `.emacs`:

    (add-to-list 'load-path "<path-to-ziria>\\tools")
    (require 'blink-mode)

# Running tests

This section is geared towards Windows, but the process on Linux is similar. On
Linux only gcc is supported, so make sure you set the `COMPILER` environment
variable to `gcc` when running the tests.

## Running the basic tests

To run the tests, you must have gcc installed in your Linux or Cygwin
environment. Go to `tests/backend` or `tests/parser` and type `make`.

The Ziria compiler, `wplc`, produces C source code that needs to be further
compiled with a C compiler. On Windows, we currently support 3 C environments:
gcc, WinDDK v7, and Visual Studio. In order to use WinDDK, you need to set the
`WINDDK_ROOT` environment variable. In order to use VS, you need to set
`ZIRIA_VS` to point to the directory containing the `vcvarsall.bat` file, e.g.,
create a new environment variable `ZIRIA_VS` and give it the value `"C:\Program
Files (x86)\Microsoft Visual Studio 12.0\VC"` for VS 2013). To use a different C
compiler to run the tests, run:

    COMPILER={gcc,winddk,vs} make

Note that when setting the environment variable `ZIRIA_VS` and `ZIRIA_ROOT`, the
double quotes (for the path) are necessary. Make sure to reboot Cygwin64 or
related programs after set/reset system environment variables. Use $export in
Cygwin command line to check if the environment variables are successfully set.

## Running the WiFi tests

To compile and run performance tests and the WiFi testsuite (in `code/WiFi`) you
must have Sora installed, as described above.

# FAQ

Q: I get weird errors when I type `make`. How do I fix them?

A: The order of entries in the `PATH` environment variable may make a
difference. If an error occurs when running `make`, try changing your Haskell
Platform path, e.g., `C:\Program Files\Haskell Platform\8.0.1\bin`, to be before
your Cygwin and msys2 paths in the Windows `PATH` environment variable—cabal
needs the Haskell Platform `ld` and not the Cygwin version (otherwise horrible
errors will occur). Do this by going to 'Advanced system settings'->'Environment
variables' and then editing the system variable `PATH`. For example, replace
`C:\cygwin64\bin;C:\Program Files\Haskell Platform\8.0.1\bin` with `C:\Program
Files\Haskell Platform\8.0.1\bin;C:\cygwin64\bin`. In Windows 10, the
environment variable editor lists each component of `PATH` separately.

Q: How do I compile individual Ziria programs?

A: For the simplest examples, just run `wplc.exe`. Longer examples use the `g++`
preprocessor. One can use the `preprocesscompile.sh` batch file, e.g. in wifi,
as a shortcut. There are also `runOne` script files in many directories that
will compile wpl files using `wplc`, compile the generated C with `gcc`, and run
the code in one line. Also, `runTest` will typically run the same thing for many
files.

Q: How do I evaluate performance?

A: Performance evaluation is done using WinDDK (driver development kit)
environment used by SORA. The reason is that gcc and VSS are several orders of
magnitude slower in some cases. The simplest way to compile and run a wplc file
in this environment is using `runOnePerf` script (e.g. in `wifi/receiver`).
There is also `runTestPerf` that will run for multiple files (files are
specified in sourcesPerf).

Q: How does one debug the generated C code?

A: There is a VSS solution designed to help debug WPL-compiled C code. It is
located in `compiler\examples\Wrapper`, and it is called `Wrapper.sln`. There
are two projects in it, `CompilerVS2012` and `CompilerDDK`. `CompilerVS2012` is
for debugging using the VS compiler. It does not optimize the code and is easier
to debug. `CompilerDDK` uses the `WinDDK` environment to compile the code. It
can still be run and debugged from the VS environment, but it is more difficult
to debug because the code is heavily optimized. Before starting, make sure you
select the desired project and set it to be the startup project. Compile a WPL
file into `csrc/test.c`. Then use the usual VS shortcuts/command to
compile/run/debug.

Q. I updated GHC on Windows and now nothing works.

A. Make sure you delete the ghc and cabal directories in
`/Users/<USERNAME>/AppData/Roaming` to avoid problems with stale package state.
It is also likely that Ziria will not compiler under an untested new major
version of GHC.

Q. When I try to run any WiFi test, I get the error message "Initialization
error: couldn't start a thread!"

A. Sora threads are non-interruptible, core-pinned threads, so using them
requires extra resources. I have only encountered this error when trying to run
the WiFi tests under VMWare. After setting the number of CPUs to 1 and number of
cores per CPU to 4 in VMWare, I was able to run all WiFi tests. Note that
setting the number of cores per CPU to 2 did not work.
