[![Build Status](https://travis-ci.org/cxxr-devel/cxxr.svg?branch=master)](https://travis-ci.org/cxxr-devel/cxxr)

# CXXR

The goal of the CXXR project is to refactor the interpreter of the R language into a fully-compatible, efficient, VM for R using modern software engineering techniques.  CXXR is being carried out independently of the main R development and maintenance effort.

Currently the CXXR codebase is based off R 3.0.2.

## Build Requirements

Compiling CXXR requires a GCC or Clang compiler with C++ 11 support and fortran support.  In addition, boost 1.48.0 or later must be installed.

Compilation of the LLVM JIT requires a Clang compiler and LLVM version 3.4 as well.  (GCC should also be supported in the future.)

CXXR has been tested to compile on both Linux and Mac OSX systems.

## Configuration and Compilation

To build a development build with the LLVM JIT enabled:
  ```
   configure --enable-llvm-jit --enable-maintainer-mode 
   make
   make check
   ```
It is useful to additionally define `-Wall -DNO_CELLPOOLS -DAGGRESSIVE_GC -fsanitize=address -O1` in order to find bugs more easily. 

For release builds, the flags `-Wall -O2 -DNDEBUG -DUNCHECKED_SEXP_DOWNCAST` should be defined.

Currently `make install` is disabled for CXXR.  It can be run directly from the build directory as `bin/R` however.  This will be re-enabled in the near future.

## CXXR Discussion Mailing List.

https://groups.google.com/forum/#!forum/cxxr-devel
