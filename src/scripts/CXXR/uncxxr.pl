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

my $constype1 = "ARITHOP_TYPE|apse_bool_t|apse_size_t|char|DL_FUNC|DllInfoInitCall|double|float";
my $constype2 = "HINSTANCE|hlen|iconv_t|int|int_least64_t|INTt|Int32|intptr_t|_lli_t|long|mode_t|N01type";
my $constype3 = "R_ExternalRoutine|R_ExternalRoutine2|R_len_t|R_size_t|R_varloc_t|R_xlen_t|Rboolean|Rbyte|Rconnection|RELOP_TYPE";
my $constype4 = "RNGtype|Rprt_adj|Rrawconn|SEXPTYPE|short|size_t|ssize_t|time_t";
my $constype5 = "uInt|uint_least64_t|uint64_t|uIntuintptr_t|uintptr_t|uLong";
my $constype6 = "wchar_t|wint_t|z_off_t";
my $constype = "(?:$constype1|$constype2|$constype3|$constype4|$constype5|$constype6)";

while (<>) {
  # Convert C++ casts to C cast, discarding outer brackets if poss:
  s/[a-z]*_cast<([^>]*)>\(($castscope)\)([^.+-]|-[^>-]|\+[^+])/\($1\)$2$3/g;
  s/[a-z]*_cast<([^>]*)>/\($1\)/g;

  # Convert constructor expressions for types known to CR into casts:
  s/\b($constype)(\s*)\(($castscope)\)([^.+-]|-[^>-]|\+[^+])/\($1\)$2$3$4/g;
  s/\b($constype)(\s*)\(/\($1\)$2\(/g;

  # Reinstate C++ reserved words used as identifiers:
  s/class_str/class/g;
  s/connclass/class/g;
  s/connprivate/private/g;
  s/devnum/this/g;
  s/ffalse/false/g;
  s/funstr/this/g;
  s/newbuff/new/g;
  s/newconn/new/g;
  s/newd\b/new/g;
  s/newf\b/new/g;
  s/newi\b/new/g;
  s/newplot/new/g;
  s/newv\b/new/g;
  s/protectct/protected/g;
  s/thisconn/this/g;
  s/thispath/this/g;
  s/thiss/this/g;

  # Bytecode related stuff:
  s/NSFROMEND\((\d+)\)/R_BCNodeStackTop[-$1]/g;

  # Other changes:
  s/cDUPLICATE_ATTRIB/DUPLICATE_ATTRIB/g;
  s/\(char\*\)R_AllocStringBuffer/R_AllocStringBuffer/g;
  s/CXXR_alloc/R_alloc/g;
  s/CXXRBUILTINFUNCTION:://g;
  s/CXXRCONSTRUCT\([^,]+, *((?:[^()]|$brack2)+)\)/$1/g;
  s/CXXRCONST\s*//g;
  s/CXXRCCAST\([^,]+, *((?:[^()]|$brack2)+)\)/$1/g;
  s/CXXRSCAST\([^,]+, *((?:[^()]|$brack2)+)\)/$1/g;
  s/CXXRFALSE/0/g;
  s/CXXRNOCAST//g;
  s/CXXRTRUE/1/g;
  s/CXXRUNSIGNED\s*//g;
  s/NODESTACKEND\[/R_BCNodeStackTop\[/g;
  s/Rf_//g;
  s/XVECTOR_ELT/VECTOR_ELT/g;
  s/std:://g;
  print;
}
