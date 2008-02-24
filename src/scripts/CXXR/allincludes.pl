#!/usr/bin/env perl

# Creates a file allincludes.cpp that #includes all the header files
# below . (the current directory, which should be src/main) and ../include.

use strict;

@ARGV == 0 or die "Usage: allincludes.pl\n";
`pwd` =~ m|src/main$| or die "This program must be invoked within src/main.\n";
my @incs = sort `find . ../include \\( -name '*.h' -o -name '*.hpp' \\) -printf '%P\n'`;
print @incs;
my $fname = "allincludes.cpp";
open(OUT, "> $fname") or die "Cannot open $fname: $!\n";
print OUT <<EOF;
// This file is generated automatically be allincludes.pl : DO NOT EDIT.
// It #includes all the header files under src/main and src/include,
// and is particularly used to check that where a CXXR header file
// includes a function prototype that is also present in one of the
// CR header files, the prototypes are consistent with each other.  This
// simplifies the process of updating CXXR to a new release of CR.

EOF
# Put the CXXR headers first, to keep them out of the way of CRs macro
# definitions (esp. length)
foreach (grep(m|^CXXR/|, @incs)) {
  chomp;
  print OUT "#include \"$_\"\n";
}
foreach (grep(!m|^CXXR/|, @incs)) {
  chomp;
  print OUT "#include \"$_\"\n";
}


