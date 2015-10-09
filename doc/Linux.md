# Building Ziria on Linux

Ziria on Linux depends on the following:

* GHC 7.8.3 (not 7.8.4, and no higher version either)
* cabal-install (tested with 1.22, but please use the latest version)
* alex  (cabal should automatically fetch this)
* happy (cabal should automatically fetch this)
* bladeRF library headers (optional)
* GCC with developer headers (tested with 4.9, but any version should work)

You may obtain the required dependencies however you wish. Instructions for
Ubuntu are provided below. If you do not use Ubuntu and your distribution does
not provide the exact versions of the packages required, you may obtain the
required packages using either the Nix package manager, or using a Docker
container with Ubuntu. Both Nix and Docker have been tested in the past

## Ubuntu instructions

```bash
$ # Install essential build tools
$ sudo apt-get install binutils build-essential git

$ # Herbert's PPA on Ubuntu provides most of the required packages
$ sudo add-apt-repository ppa:hvr/ghc
$ sudo apt-get update

$ # Make sure to get this exact version of GHC
$ sudo apt-get install ghc-7.8.3

$ # Replace 1.22 with whatever may be the latest version.
$ # You could also install this in the sandbox if you wish
$ sudo apt-get install cabal-install-1.22

$ # The distribution version works
$ sudo apt-get install alex
$ sudo apt-get install happy

$ # Update cabal
$ /opt/cabal/1.22/bin/cabal update

$ # cd into whatever directory you wish, and grab the repo
$ git clone https://github.com/dimitriv/Ziria && cd Ziria

$ # Make using specific versions of GHC and cabal-install
$ GHC=/opt/ghc/7.8.3/bin/ghc CABAL=/opt/cabal/1.22/bin/cabal make

$ # Optionally grab the bladeRF library headers
$ sudo apt-get install libbladeRF-dev
$ # Setup the bladeRF board, and flash the device before use.
$ # You don't need to setup GNURadio with bladeRF support.
```

