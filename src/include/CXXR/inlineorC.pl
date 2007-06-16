#!/usr/bin/env perl
#
# This script is a utility for processing header files intended for
# inclusion in both C and C++ programs.  If one or more filenames are
# specified on the command line, the script will perform in-place
# editing, backing up the original file by adding the suffix ".bak".
# If no arguments are given, the script acts as a filter.
#
# It will transform lines of the form
#
#  inline int square(int n)  { return n*n; }
#
# where the word "inline" is preceded by a single space, into the
# following construct:
#
# #ifndef __cplusplus
# int square(int n);
# #else
# inline int square(int n)  { return n*n; }
# #endif
#
# in which note that the word "inline" is no longer preceded by a
# space.
#
# Similarly it will transform constructs of the form:
#
#  inline int cube(int n)
# {
#     return n*n*n;
# }
#
# into the following:
#
# #ifndef __cplusplus
# int cube(int n);
# #else
# inline int cube(int n)
# {
#     return n*n*n;
# }
# #endif  // __cplusplus
#
# Here the body of the inlined function may be as long as required,
# and is considered to be terminated by a line whose first character
# is '}'.
#
# Remember that C-style linkage (extern "C" in C++) should be
# specified for all functions processed in this way.

use strict;
use English;

@ARGV == 0 or $INPLACE_EDIT = ".bak";
my $body = 0;
while (<>) {
  if (/^ inline\s+((.*\))\s*\{.*\})\s*$/) {
    my $decl = $1;
    my $sig = $2;
    print "#ifndef __cplusplus\n";
    print "$sig;\n";
    print "#else\n";
    print "inline $decl\n";
    print "#endif\n";
  } elsif (/^ inline\s+(.*\))\s*$/) {
    my $sig = $1;
    $body = 1;
    print "#ifndef __cplusplus\n";
    print "$sig;\n";
    print "#else\n";
    print "inline $sig\n";
  } elsif ($body && /^\}/) {
    print;
    print "#endif  // __cplusplus\n";
    $body = 0;
  } else {
    print;
  }
}
