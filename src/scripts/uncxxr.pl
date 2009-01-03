#!/usr/bin/env perl

# Where CR source files have been converted into C++ and otherwise
# adapted for use in CXXR, this script tries as far as possible to
# reverse the systematic changes.  It is used when upgrading to a new
# version of R to distinguish substantive from routine changes.

# Note that the output is for human eyes only: it is permissible for
# it occasionally to fail to produce syntactically correct C, or to
# get scopes wrong.

use strict;

my $brack0 = "\\([^()]*\\)";
my $brack1 = "\\((?:[^()]*|$brack0)*\\)";
my $brack2 = "\\((?:[^()]*|$brack1)*\\)";

my $cs = "[^/%<=>\*+()-]*";
my $castscope = "\\s*[~!+&\*-]*(?:$cs(?:->$cs)*|\\[[^\\]]*\\]|$brack2)*";
#print $castscope;

my $constype1 = "ARITHOP_TYPE|apse_bool_t|apse_size_t|char|double|float";
my $constype2 = "HINSTANCE|iconv_t|int|intptr_t|_lli_t|long";
my $constype3 = "R_size_t|R_varloc_t|Rboolean|Rbyte|Rconnection|RELOP_TYPE";
my $constype4 = "Rprt_adj|Rrawconn|SEXPTYPE|short|size_t|time_t";
my $constype5 = "uInt|uIntuintptr_t|uintptr_t|uLong|wchar_t|wint_t|z_off_t";
my $constype = "(?:$constype1|$constype2|$constype3|$constype4|$constype5)";

while (<>) {
  # Convert C++ casts to C cast, discarding outer brackets if poss:
  s/[a-z]*_cast<([^>]*)>\(($castscope)\)([^.+-]|-[^>-]|\+[^+])/\($1\)$2$3/g;
  s/[a-z]*_cast<([^>]*)>/\($1\)/g;

  # Convert constructor expressions for types known to CR into casts:
  s/\b($constype)(\s*)\(($castscope)\)([^.+-]|-[^>-]|\+[^+])/\($1\)$2$3$4/g;
  s/\b($constype)(\s*)\(/\($1\)$2\(/g;

  # Reinstate C++ reserved words used as identifiers:
  s/connclass/class/g;
  s/connprivate/private/g;
  s/funstr/this/g;
  # s/newbuf([^_\w\.])/new$1/g;  # newbuf is used itself in saveload.c
  s/newconn/new/g;
  s/newplot/new/g;
  s/newv/new/g;
  s/thisconn/this/g;
  s/thiss/this/g;

  # Other changes:
  s/cDUPLICATE_ATTRIB/DUPLICATE_ATTRIB/g;
  s/\(char\*\)R_AllocStringBuffer/R_AllocStringBuffer/g;
  s/CXXRconst\s*//g;
  s/CXXRNOCAST//g;
  s/CXXRnot_hidden/attribute_hidden/g;
  s/CXXRunsigned\s*//g;
  s/XVECTOR_ELT/VECTOR_ELT/g;
  print;
}
