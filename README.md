<img src="share/logo/rho_logo.png?raw=true" alt="Rho logo" width="130" height="100"/>

[![Build Status](https://travis-ci.org/rho-devel/rho.svg?branch=master)](https://travis-ci.org/rho-devel/rho)

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
  ```
   configure --enable-llvm-jit --enable-maintainer-mode 
   make
   make check
   ```
For development builds, it is useful to define
`-Wall -DNO_CELLPOOLS -DCHECKED_SEXP_DOWNCAST -fsanitize=address -O1`
in order to find bugs more easily. (If using clang >= 3.6, either remove the `-fsanitize=address`, or define `ASAN_OPTIONS="detect_leaks=0"` to disable memory leak checking.)

## Notable Known Issues

* Currently rho doesn't support packages that contain native code that uses the `USE_RINTERNALS` macro.  This includes Rcpp, rJava, data.table, xts and all the packages that depend on them.
* Our testing on different platforms is currently very limited.  We currently test on Ubuntu trusty with gcc 4.9, gcc 5.0, clang-3.5, clang-3.6 and clang-3.8, so those should always work.  OSX and other linux distros are also supported, but aren't regularly tested.

## Notable Known Issues

* Currently rho doesn't support packages that contain native code that uses the `USE_RINTERNALS` macro.  This includes Rcpp, rJava, data.table, xts and all the packages that depend on them.
* Our testing on different platforms and compilers is currently very limited.  Building with clang 3.5 on ubuntu trusty should always work.  OSX, other linux distros and recent versions of clang and gcc are also supported, but aren't regularly tested.

## Rho Discussion Mailing List.

https://groups.google.com/forum/#!forum/rho-devel
