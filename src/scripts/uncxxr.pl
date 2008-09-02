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
  s/(\W)char\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(char\)$2/g;
  s/(\W)char\(/$1\(char\)\(/g;
  s/connclass/class/g;
  s/connprivate/private/g;
  s/CXXRconst\s*//g;
  s/(\W)double\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(double\)$2/g;
  s/(\W)double\(/$1\(double\)\(/g;
  s/(\W)float\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(float\)$2/g;
  s/(\W)float\(/$1\(float\)\(/g;
  s/iconv_t\((\s*\*?[A-Za-z0-9_ ]+)\)/\(iconv_t\)$1/g;
  s/iconv_t\(/\(iconv_t\)\(/g;
  s/(\W)int\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(int\)$2/g;
  s/(\W)int\(/$1\(int\)\(/g;
  s/intptr_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(intptr_t\)$1/g;
  s/intptr_t\(/\(intptr_t\)\(/g;
  s/_lli_t\((\s*\*?[A-Za-z0-9_ ]+)\)/\(_lli_t\)$1/g;
  s/_lli_t\(/\(_lli_t\)\(/g;
  s/newbuf/new/g;
  s/newconn/new/g;
  s/R_size_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(R_size_t\)$1/g;
  s/R_size_t\(/\(R_size_t\)\(/g;
  s/short\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(short\)$1/g;
  s/short\(/\(short\)\(/g;
  s/([^_\w])size_t\((\s*[*-]?[A-Za-z0-9_ ]+)\)/$1\(size_t\)$2/g;
  s/([^_\w])size_t\(/$1\(size_t\)\(/g;
  s/thisconn/this/g;
  s/uInt\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(uInt\)$1/g;
  s/uInt\(/\(uInt\)\(/g;
  s/uLong\((\s*[*-]?[A-Za-z0-9_ ]+)\)/\(uLong\)$1/g;
  s/uLong\(/\(uLong\)\(/g;
  s/z_off_t\((\s*\*?[A-Za-z0-9_ ]+)\)/\(z_off_t\)$1/g;
  s/z_off_t\(/\(z_off_t\)\(/g;
  print;
}
