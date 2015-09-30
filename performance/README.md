# Performance analysis

This directory contains the required tools for producing a performance
comparison chart. Currently this runs the WiFi-perf tests.

## Quick usage

If you are on Windows with GCC, VS, and WinDDK setup, and you want to compare
the performance of all three compilers on Windows, run the following from
the root of Ziria

```bash
$ make test-WiFi-perf-chart
```

If you want to cleanup the results, you can do so using the following command

```bash
$ make test-WiFi-perf-chart-clean
```

## Usage

First, make sure you are in the performance directory. To initialize, run

```bash
$ make install
```

Then, you can run the tests for the compiler of your choice. For example,
to run the tests for gcc and produce the benchmark results in
performance/perf/gcc.txt, execute the following:

```bash
$ make gcc
```

If you want to run tests for the same compiler multiple times, rename the
appropriate files in performance/perf before continuing.

You can do this for multiple compilers (gcc, vs, winddk). Once you have all
the required .txt files in performance/perf you can produce the chart using


```bash
$ make chart
```

This should produce the required chart in `output.svg`

If you plan on comparing test results between Windows and Linux, copy the
.txt files from one OS to the other (remember to rename appropriately) before
running `make chart`

If you want to cleanup the results, run

```bash
$ make clean
```

Although there would be no need to do so, if you want to clean up the cabal
sandbox created, you can do that as follows

```bash
$ make clean-sandbox
```

