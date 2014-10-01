[![Build Status](https://travis-ci.org/cxxr-devel/cxxr.svg?branch=master)](https://travis-ci.org/cxxr-devel/cxxr)

# CXXR

The aim of the CXXR project is to refactor the interpreter of the R language, currently written for the most part in C, into C++ using modern software engineering techniques, whilst retaining full functionality and compatability. CXXR is being carried out independently of the main R development and maintenance effort.

Currently the CXXR codebase is based off R 3.0.2.

## Build Requirements

Compiling CXXR requires a GCC or Clang compiler with C++ 11 support and fortran support.  In addition, boost 1.48.0 or later must be installed.  Compilation of the LLVM JIT requires LLVM version 3.4 as well.

CXXR has been tested to compile on both Linux and Mac OSX systems.

## Configuration and Compilation

To build a development build with the LLVM JIT enabled:
  ```
   configure --enable-llvm-jit --enable-maintainer-mode 
   make
   make check
   ```
It is useful to additionally define `-Wall -DNO_CELLPOOLS -fsanitize=address -O1` in order to find bugs more easily. 

For release builds, the flags `-Wall -O2 -DNDEBUG -DUNCHECKED_SEXP_DOWNCAST` should be defined, and `src/include/CXXR/config.hpp` should be modified to undefine `CHECK_EXPOSURE` and `AGGRESSIVE_GC`.

Currently `make install` is disabled for CXXR.  It can be run directly from the build directory as `bin/R` however.  This will be re-enabled in the near future.

## CXXR Discussion Mailing List.

https://groups.google.com/forum/#!forum/cxxr-devel
