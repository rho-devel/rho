<img src="share/logo/rho_logo.png?raw=true" alt="Rho logo" width="130" height="100"/>

[![Build Status](https://travis-ci.org/rho-devel/rho.svg?branch=master)](https://travis-ci.org/rho-devel/rho)

# Rho

The goal of the Rho project is to refactor the interpreter of the R language into a fully-compatible, efficient, VM for R using modern software engineering techniques.  Rho is being carried out independently of the main R development and maintenance effort.

Currently the rho codebase is based off R-devel.

## Build Requirements

Compiling rho requires a GCC or Clang compiler with C++ 11 support and fortran support.  In addition, boost 1.48.0 or later must be installed.

Compilation of the LLVM JIT requires a Clang compiler and LLVM version 3.4 as well. On debian libedit has to be installed additionally.  (GCC should also be supported in the future.)

Rho has been tested to compile on both Linux and Mac OSX systems.

## Configuration and Compilation

To build a development build with the LLVM JIT enabled:
  ```
   configure --enable-llvm-jit --enable-maintainer-mode 
   make
   make check
   ```
It is useful to additionally define `-Wall -DNO_CELLPOOLS -fsanitize=address -O1` in order to find bugs more easily. 

For release builds, the flags `-Wall -O2 -DNDEBUG -DUNCHECKED_SEXP_DOWNCAST` should be defined.

Currently `make install` is disabled for rho.  It can be run directly from the build directory as `bin/R` however.  This will be re-enabled in the near future.

## Rho Discussion Mailing List.

https://groups.google.com/forum/#!forum/rho-devel
