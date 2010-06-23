#!/usr/bin/env perl

use strict;
use Expect;

my $exp;

sub onexit {
    $exp->send("quit\n");
    print "\n";
    exit;
}

sub await_gdb_prompt {
    my $i = $exp->expect(5, "-re", "^Program exited", "-re", "^\\(gdb\\)");
    if ($i == 1) {
	onexit;
    }
}

sub gdbcmd {
    $exp->send($_[0]);
    await_gdb_prompt;
}
	
@ARGV > 0 or die "Usage: samplestack.pl command args\n";
my $cmd = $ARGV[0];
shift;
$exp = Expect->spawn($cmd, "-d", "gdb") or die "Cannot spawn $cmd\n";
#$exp->debug(2);
await_gdb_prompt;
gdbcmd("set height 0\n");
gdbcmd("set width 0\n");
gdbcmd("b main\n");
gdbcmd(sprintf("run %s\n", join(" ", @ARGV)));
gdbcmd("del 1\n");
while (1) {
    $exp->send("c\n");  # Don't wait for prompt!
    select undef, undef, undef, 0.1;
    gdbcmd("\cC");
    gdbcmd("bt\n");
}
