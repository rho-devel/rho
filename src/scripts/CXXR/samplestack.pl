#!/usr/bin/env perl

use strict;
use Expect;

my $exp;

my $syscall = "__kernel_vsyscall";

my @symbols = ("CXXR::BuiltInFunction::apply",
               "CXXR::Closure::apply",
               "CXXR::Evaluator::evaluate",
               "CXXR::Expression::evaluate",
               "CXXR::GCManager::gc",
               "CXXR::GCNode::gclite",
               "CXXR::Promise::evaluate",
               "CXXR::Symbol::evaluate",
               "R_gc_internal",
               "Rf_applyClosure",
               "Rf_eval",
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
	
@ARGV > 1 or die "Usage: samplestack.pl sample_interval command args\n";
my $sampleinterval = $ARGV[0];
$sampleinterval > 0 or die "Sample interval must be positive\n";
shift;
my @fullcmd = @ARGV;
my $cmd = $ARGV[0];
shift;

system("date");
print "Working directory: ";
system("pwd"); 
printf "Command: %s\n", join(" ", @fullcmd);
print "Sampling interval = ${sampleinterval} secs\n\n";

$exp = Expect->spawn($cmd, "-d", "gdb") or die "Cannot spawn $cmd\n";
#$exp->debug(2);
$exp->log_stdout(0);
await_gdb_prompt;
gdbcmd("set height 0\n");
gdbcmd("set width 0\n");
# The foll. seemed to confuse gdb during the run:
#gdbcmd("set print address off\n");
gdbcmd("b main\n");
gdbcmd(sprintf("run %s\n", join(" ", @ARGV)));
gdbcmd("del 1\n");
while (1) {
    $exp->send("c\n");  # Don't wait for prompt!
    select undef, undef, undef, $sampleinterval;
    gdbcmd("\cC");
    if ($exp->before() =~ /^Program exited/m) {
        last;
    }
    ++$samples;
    # Don't bother with backtrace if we're in a system call:
    if ($exp->before() =~ /$syscall/mo) {
        $symhits{$syscall} += 1;
    } else {
        gdbcmd("bt\n");
        $exp->before() =~ /$symrx/mo;
        $symhits{$1} += 1;
	if ($1 eq "") {
            print "Peculiar stack at sample #${samples}:\n";
	    print $exp->before();
	    print "\n";
	}
#        if ($1 eq "do_return") {
#            print "do_return stack:\n";
#            print $exp->before();
#	    print "\n";
#        }
    }
}
$exp->send("quit\n");

print "Total samples = ${samples}\n";

my $syshits = $symhits{$syscall};
printf "Samples in user time = %d\n\n", $samples - $syshits;

my @sortedkeys = sort { $symhits{$b} <=> $symhits{$a} } keys %symhits;
foreach (@sortedkeys) {
    my $hits = $symhits{$_};
    printf "%5d ", $hits;
    if ($_ ne $syscall) {
        printf "(%3d%%u)", ($hits*100.0/($samples - $syshits))+0.5;
    } else {
        print "       ";
    }
    print " $_\n";
}
