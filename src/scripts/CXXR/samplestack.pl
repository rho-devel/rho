#!/usr/bin/env perl

use strict;
use Expect;

my $exp;

my @symbols = ("CXXR::BuiltInFunction::apply",
               "CXXR::Closure::apply",
               "CXXR::Evaluator::evaluate",
               "CXXR::Expression::evaluate",
               "CXXR::GCManager::gc",
               "CXXR::GCNode::gclite",
               "CXXR::Promise::evaluate",
               "CXXR::Symbol::evaluate",
               "Rf_applyClosure",
               "Rf_eval",
               "__kernel_vsyscall",
               "do_\\w*",
               "main",
               "run_Rmainloop",
               "setup_Rmainloop");

my $symrx = join("|", @symbols);
$symrx = "\\b(${symrx})\\b";

my %symhits;

my $samples = 0;

sub await_gdb_prompt {
    my $i = $exp->expect(5, "-re", "^\\(gdb\\)");
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
$exp->log_stdout(0);
await_gdb_prompt;
gdbcmd("set height 0\n");
gdbcmd("set width 0\n");
gdbcmd("b main\n");
gdbcmd(sprintf("run %s\n", join(" ", @ARGV)));
gdbcmd("del 1\n");
while (1) {
    $exp->send("c\n");  # Don't wait for prompt!
    select undef, undef, undef, 0.01;
    gdbcmd("\cC");
    if ($exp->before() =~ /^Program exited/m) {
        last;
    }
    gdbcmd("bt\n");
    ++$samples;
    $exp->before() =~ /$symrx/m;
    $symhits{$1} += 1;
}
$exp->send("quit\n");
print "\n";
print "Total samples = ${samples}\n\n";

my @sortedkeys = sort { $symhits{$b} <=> $symhits{$a} } keys %symhits;
foreach (@sortedkeys) {
    printf "%5d %s\n", $symhits{$_}, $_;
}
