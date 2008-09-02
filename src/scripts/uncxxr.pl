#!/usr/bin/env perl

# Where CR source files have been converted into C++ and otherwise
# adapted for use in CXXR, this script tries as far as possible to
# reverse the systematic changes.  It is used when upgrading to a new
# version of R to distinguish substantive from routine changes.

# Note that the output is for human eyes only: it is permissible for
# it occasionally to fail to produce syntactically correct C, or to
# get scopes wrong.

use strict;

while (<>) {
  s/[a-z]*_cast<([^>]*)>\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\($1\)$2/g;
  s/[a-z]*_cast<([^>]*)>/\($1\)/g;
  s/apse_bool_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(apse_bool_t\)$1/g;
  s/apse_bool_t\(/\(apse_bool_t\)\(/g;
  s/apse_size_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(apse_size_t\)$1/g;
  s/apse_size_t\(/\(apse_size_t\)\(/g;
  s/cDUPLICATE_ATTRIB/DUPLICATE_ATTRIB/g;
  s/(\W)char\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(char\)$2/g;
  s/(\W)char\(/$1\(char\)\(/g;
  s/connclass/class/g;
  s/connprivate/private/g;
  s/CXXRconst\s*//g;
  s/([^_\w\.])double\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(double\)$2/g;
  s/([^_\w\.])double\(/$1\(double\)\(/g;
  s/([^_\w\.])float\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(float\)$2/g;
  s/([^_\w\.])float\(/$1\(float\)\(/g;
  s/iconv_t\((\s*\*?[A-Za-z0-9_ ]+)\)/\(iconv_t\)$1/g;
  s/iconv_t\(/\(iconv_t\)\(/g;
  s/([^_\w\.])int\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(int\)$2/g;
  s/([^_\w\.])int\(/$1\(int\)\(/g;
  s/([^_\w\.])intptr_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(intptr_t\)$2/g;
  s/([^_\w\.])intptr_t\(/$1\(intptr_t\)\(/g;
  s/_lli_t\((\s*\*?[A-Za-z0-9_ ]+)\)/\(_lli_t\)$1/g;
  s/_lli_t\(/\(_lli_t\)\(/g;
  s/newbuf([^_\w\.])/new$1/g;
  s/newconn/new/g;
  s/newplot/new/g;
  s/R_size_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(R_size_t\)$1/g;
  s/R_size_t\(/\(R_size_t\)\(/g;
  s/Rbyte\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(Rbyte\)$1/g;
  s/Rbyte\(/\(Rbyte\)\(/g;
  s/Rconnection\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(Rconnection\)$1/g;
  s/Rconnection\(/\(Rconnection\)\(/g;
  s/SEXPTYPE\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(SEXPTYPE\)$1/g;
  s/SEXPTYPE\(/\(SEXPTYPE\)\(/g;
  s/short\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(short\)$1/g;
  s/short\(/\(short\)\(/g;
  s/([^_\w])size_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(size_t\)$2/g;
  s/([^_\w])size_t\(/$1\(size_t\)\(/g;
  s/thisconn/this/g;
  s/uInt\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(uInt\)$1/g;
  s/uInt\(/\(uInt\)\(/g;
  s/([^_\w\.])uintptr_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(uintptr_t\)$2/g;
  s/([^_\w\.])uintptr_t\(/$1\(uintptr_t\)\(/g;
  s/uLong\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(uLong\)$1/g;
  s/uLong\(/\(uLong\)\(/g;
  s/XVECTOR_ELT/VECTOR_ELT/g;
  s/z_off_t\((\s*\*?[A-Za-z0-9_ ]+)\)/\(z_off_t\)$1/g;
  s/z_off_t\(/\(z_off_t\)\(/g;
  print;
}
