# Development on Rho has been suspended indefinitely.
## If you are interested in resuming development, please contact kmillar@google.com

<img src="doc/html/rho_logo.png?raw=true" alt="Rho logo" width="130" height="100"/>

[![Build Status](https://travis-ci.org/rho-devel/rho.svg?branch=master)](https://travis-ci.org/rho-devel/rho)
[![Coverage Status](https://img.shields.io/codecov/c/github/rho-devel/rho/master.svg)](https://codecov.io/github/rho-devel/rho?branch=master)

# Rho

The goal of the Rho project is to refactor the interpreter of the R language into a fully-compatible, efficient, VM for R using modern software engineering techniques.  Rho is being carried out independently of the main R development and maintenance effort.

Currently the rho codebase is based off R-devel.

## Build Requirements

Compiling rho requires a GCC or Clang compiler with C++ 11 support and fortran support.  In addition the following libraries must be installed:
   * boost >= 1.48.0
   * libcurl >= 7.28.0
   * zlib >= 1.2.5
   * libbzip2 >= 1.0.6
   * liblzma >= 5.0.3
   * pcre >= 8.10
   * libedit

Compilation of the LLVM JIT requires clang 3.4 or later and the matching
version of the LLVM library (except building on clang 3.7 is currently broken).

Rho has been tested to compile on both Linux and Mac OSX systems.

## Configuration and Compilation

To build with the LLVM JIT enabled:

    ./configure --enable-llvm-jit --enable-maintainer-mode
    make
    make check


For development builds, it is useful to set `CFLAGS` and `CXXFLAGS` to
`-Wall -DNO_CELLPOOLS -DCHECKED_SEXP_DOWNCAST -fsanitize=address -O1`
in order to find bugs more easily.

The configure call may complain about missing recommended packages. The
recommended packages can be obtained by running the script `tools/rsync-recommended`.

## Building on OSX

It is tricky to build rho with JIT support on OSX. The following steps show how
to build without JIT support.

First, you will need the XCode command line tools which provide clang and
clang++. Additional dependencies you need are:

* Fortran compiler
* XZ library (liblzma)
* PCRE library
* Boost C++ library

The above dependencies can be installed with e.g. Homebrew:

    brew install gcc # Provides gfortran.
    brew install xz
    brew install pcre
    brew install boost

After fetching the dependencies you should be able to build. If your Homebrew
folder is in your home directory you will need to add `~/homebrew/include` and
`~/homebrew/lib` as include/library directories:

    CC=clang CXX=clang++ \
        CFLAGS='-I/Users/me/homebrew/include -g -O2' \
        CXXFLAGS='-I/Users/me/homebrew/include -g -O2' \
        CPPFLAGS='-I/Users/me/homebrew/include' \
        LDFLAGS='-L/Users/me/homebrew/lib' \
        ./configure --with-x=no


## Notable Known Issues

* Currently rho doesn't support packages that contain native code that uses the `USE_RINTERNALS` macro.  This includes Rcpp, rJava, data.table, xts and all the packages that depend on them.
* Our testing on different platforms is currently very limited.  We currently test on Ubuntu trusty with gcc 4.9, gcc 5.3, gcc 6.1, clang 3.5, clang 3.6 and clang 3.8, so those should always work.  OSX and other linux distros are also supported, but aren't regularly tested.

## Rho Discussion Mailing List.

https://groups.google.com/forum/#!forum/rho-devel
