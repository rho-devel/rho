#!/usr/bin/env perl

use strict;
use Expect;

my $exp;

my $syscall = "__kernel_vsyscall";

my @symbols = ("CXXR::BuiltInFunction::apply",
               "CXXR::BuiltInFunction::invoke",
               "CXXR::Closure::apply",
	       "CXXR::Closure::execute",
	       "CXXR::Closure::invoke",
               "CXXR::Environment::findBinding",
               "CXXR::GCManager::gc",
               "CXXR::GCNode::gclite",
               "CXXR::findFunction",
	       "R_findVar\\w*",
	       "R_JumpToContext",
               "R_gc_internal",
	       "R_possible_dispatch",
	       "R_restore_globals",
	       "R_run_onexits",
	       "Rf_DispatchGroup",
	       "Rf_DispatchOrEval",
               "Rf_applyClosure",
               "Rf_eval\\w*",
	       "Rf_findcontext",
               "Rf_findFun",
	       "Rf_findVar\\w*",
	       "Rf_usemethod",
	       "_Unwind_\\w*",
	       "__cxa_rethrow",
	       "__cxa_throw",
	       $syscall,
               "do_\\w*",
               "[\\w:]*evaluate\\b",
               "main",
               "run_Rmainloop",
               "setup_Rmainloop");

my $symrx = join("|", @symbols);
$symrx = "\\b(${symrx})\\b";

my $errorsrx = "^Initial frame selected|^Cannot access memory";

my %symhits;

my $samples = 0;

sub await_gdb_prompt {
    if (!defined($exp->expect(5, "-re", "^\\(gdb\\)"))) {
        print "Error awaiting gdb prompt: ";
	print $exp->error();
	print "\n--------------------\n";
	print $exp->before();
        print "\n--------------------\n";
	die "Aborting";
    }
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
my $gdbpid = $exp->pid();
print "gdb PID = $gdbpid\n";
gdbcmd("set debug-file-directory /usr/lib/debug\n");
gdbcmd("set height 0\n");
gdbcmd("set width 0\n");
#gdbcmd("set print address off\n");
gdbcmd("b main\n");
gdbcmd(sprintf("run %s\n", join(" ", @ARGV)));
gdbcmd("del 1\n");
`ps --no-headers --ppid=$gdbpid` =~ /^\s*(\d+).*R$/
  or die "Cannot get R PID: check command line";
my $Rpid = $1;
print "R PID = $Rpid\n\n";

while (1) {
    $exp->send("c\n");  # Don't wait for prompt!
    select undef, undef, undef, $sampleinterval;
    kill 'INT', $Rpid;
    await_gdb_prompt;
    if ($exp->before() =~ /^Program exited/m) {
        last;
    }
    if ($exp->before() !~ /SIGINT/) {
	printf "Sample #%d failed:\n--------------------\n", $samples + 1;
	print $exp->before();
	print "\n--------------------\nWrapping up.\n";
	last;
    }
    ++$samples;
    print STDERR "\r$samples ";
    # Don't bother with backtrace if we're in a system call:
    if ($exp->before() =~ /$syscall|$symrx/mo) {
        $symhits{$&} += 1;
    } else {
        do {
	    gdbcmd("up\n");
	} until ($exp->before() =~ /$errorsrx|$symrx/mo);
	if ($exp->before() =~ /$errorsrx/mo) {
            print "Peculiar stack at sample #${samples}:\n";
	    gdbcmd("bt\n");
	    print $exp->before();
	    print "\n";
	    gdbcmd("f 0\n");
	    $symhits{""} += 1;
	} else {
            $symhits{$&} += 1;
	}
#        if ($1 eq "do_return") {
#	    my $pre = "$`$&";
#	    my $post = $';
#	    $post =~ /.*?$/m;
#            print "do_return stack:\n$pre$&\n\n";
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
