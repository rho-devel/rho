Rho Benchmarks
==============

This directory contains scripts to benchmark Rho. The benchmarks are configured
to compile with Clang, and for the JIT benchmarks you will need the LLVM
development libraries.

The benchmark script `runbench.py` by default will benchmark rho with and
without JIT enabled, and CR (GnuR) with and without bytecode compilation.  The
CR benchmark is used as a baseline to compare rho performance, and can be
optionally skipped (see below).

The main benchmark suite used is the R benchmark repository, which is downloaded
with Git before benchmarks are run the first time. The benchmark repository is here:
https://github.com/rho-devel/benchmarks

The benchmark scripts and their usage are described in more detail below.

runbench.py
-----------

This is a python script that can be used to benchmark a single rho commit.

The benchmarked commit is checked out in the subdirectory `rho`.

Some example invocations:

    $ ./runbench.py 1234567          # Benchmark commit with hash 1234567.
    $ ./runbench.py 1234567 abcd123  # Benchmark multiple commits.


To build faster you can add `--no-clean` which skips running `git clean` before building each commit.

    $ ./runbench.py --no-clean 1234567 # Faster build for incremental benchmarks.


The benchmark script will build and benchmark with and without JIT enabled. You
can disable the JIT benchmark by adding `--no-jit`:

    $ ./runbench.py --skip-jit 1234567


It is also possible to skip the CR (GnuR) benchmark:

    $ ./runbench.py --skip-cr 1234567


Use a different rho repository (NB: must delete the `rho` subdirectory when switching repository):

    $ ./runbench.py --repository git@github:user/rhofork 1234567


report.R
--------

This script generates a PDF with graphs for each benchmark with data points for
each benchmarked compiler and version. The output file is `report.pdf`.

compare.R
---------

Generates a comparison PDF with bar graphs for each benchmarked RVM. The rho
versions to compare must be supplied on the command line, and at least two
version names are required.

incremental.R
-------------

This script fetches the latest commits in the parent repository and reads the
git log to see if any new commits have been added and not yet benchmarked.  The
script outputs commit hashes for any commits that are not logged in the file
`out/versions`.

This script can be used to run incremental periodic benchmarks. Example usage:

    $ Rscript incremental.R 4 && cat commits | xargs python runbench.py


