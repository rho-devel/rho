#! /bin/sh

# Configure sparsehash for a C++11 compiler.
exec $(dirname $0)/configure "$@" \
     CXX="${CXX1X}" \
     CXXFLAGS="${CXX1XFLAGS}"
