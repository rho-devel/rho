#!/usr/bin/python

#  R : A Computer Language for Statistical Data Analysis
#  Copyright (C) 2016 and onwards the Rho Project Authors.
#
#  Rho is not part of the R project, and bugs and other issues should
#  not be reported via r-bugs or other R project channels; instead refer
#  to the Rho website.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, a copy is available at
#  https://www.R-project.org/Licenses/

# This script benchmarks a specific version of Rho and outputs the result in a
# file in the output directory.
#
# 1. Take Rho git ref (hash) on command line to select version.
# 2. Check out the selected Rho version into local rho directory.
# 3. Build the Rho version.
# 4. Run benchmarks and collect results into output directory.
#
# Output is generated into files with the naming scheme out/rho(-jit)?-GITREF.csv
# where GITREF is the Git reference. For each benchmark run, CR is also benchmarked
# to get a performance baseline.

import os
import subprocess
import argparse
import ConfigParser

from os.path import normpath

# This list contains benchmarks to run and the number of warmup/bench runs for each.
# Various numbers of runs are used for the benchmarks to ensure that
# short-running benchmarks run enough iterations to not be strongly affected by
# VM startup time.
benchmarks = [
        { 'name': 'benchmarks/scalar/crt/crt.R', 'warmup_rep': 10, 'bench_rep': 30 },
        { 'name': 'benchmarks/scalar/fib/fib.R', 'warmup_rep': 4, 'bench_rep': 12 },
        { 'name': 'benchmarks/scalar/fib/fib_rec.R', 'warmup_rep': 2, 'bench_rep': 5 },
        { 'name': 'benchmarks/scalar/gcd/gcd.R', 'warmup_rep': 2, 'bench_rep': 5 },
        { 'name': 'benchmarks/scalar/gcd/gcd_rec.R', 'warmup_rep': 2000, 'bench_rep': 5000 },
        { 'name': 'benchmarks/scalar/prime/prime.R', 'warmup_rep': 2, 'bench_rep': 3 },
        { 'name': 'benchmarks/scalar/ForLoopAdd/ForLoopAdd.R', 'warmup_rep': 2, 'bench_rep': 3 },
        { 'name': 'benchmarks/scalar/ForLoopAdd/ForLoopAdd.R', 'warmup_rep': 2, 'bench_rep': 3 },
        ]

def parse_args():
    parser = argparse.ArgumentParser(description='''Runs R benchmarks for a specific version of Rho.''')
    parser.add_argument('gitref', help='The Rho git reference to benchmark')
    parser.add_argument(
            '--repository', default='git@github.com:rho-devel/rho',
            help='The git repository to clone from')
    parser.add_argument(
            '--build_dir', default='rho',
            help='The directory to build Rho in. The Rho repository is cloned into this directory.')
    parser.add_argument('--result_dir', default='out',
            help='The output directory to store benchmark results in')
    args = parser.parse_args()
    return args

# Set up RVM parameters to run Gnu R.
def use_cr(jit):
    if jit:
        return { 'name': 'R-bytecode', 'id': 'cr-jit', 'jit': True }
    else:
        return { 'name': 'R', 'id': 'cr', 'jit': False }

def build_rho(args, jit, build=True):
    bench_dir = os.getcwd()
    rvm = { 'name': 'Rho', 'jit': jit }
    if jit:
        rvm['id'] = 'rho-jit'
    else:
        rvm['id'] = 'rho'
    try:
        os.chdir(args.build_dir)
        if not os.path.isdir('.git'):
            # Clone Rho into the local directory.
            exit_code = subprocess.call(['git', 'clone', args.repository, '.'])
        # Clean and switch to the selected revision.
        subprocess.call(['git', 'reset', '--hard', args.gitref])
        # Build with JIT enabled.
        subprocess.call(['git', 'clean', '-fd', 'HEAD'])
        subprocess.call(['git', 'clean', '-fX', 'HEAD'])
        if build:
            subprocess.call(['tools/rsync-recommended'])
            if jit:
                subprocess.call(['./configure', '--with-blas', '--with-lapack', '--enable-llvm-jit'])
            else:
                subprocess.call(['./configure', '--with-blas', '--with-lapack'])
            subprocess.call(['make', '-j2'])
    finally:
        os.chdir(bench_dir)
    # Create a new config file because the benchmark suite is not aware of our
    # Rho build.
    rvm['cfg_file'] = write_config(args, rvm)
    return rvm

def write_config(args, rvm):
    config = ConfigParser.RawConfigParser()
    config.add_section('GENERAL')
    config.set('GENERAL', 'WARMUP_REP', 2)
    config.set('GENERAL', 'BENCH_REP', 5)
    config.set('GENERAL', 'PERF_TMP', '_perf.tmp')
    config.set('GENERAL', 'PERF_REP', 1)
    config.set('GENERAL', 'PERF_CMD', 'perf stat -r %(PERF_REP)s -x, -o %(PERF_TMP)s --append')
    config.add_section('Rho')
    if rvm['jit']:
        config.set('Rho', 'ENV', 'R_COMPILE_PKGS=1 R_ENABLE_JIT=2')
        config.set('Rho', 'HARNESS_ARGS', 'TRUE')
    else:
        config.set('Rho', 'ENV', 'R_COMPILE_PKGS=0 R_ENABLE_JIT=0')
        config.set('Rho', 'HARNESS_ARGS', 'FALSE')
#HOME=
    config.set('Rho', 'HOME', os.path.realpath('%s/bin' % args.build_dir))
    config.set('Rho', 'CMD', 'Rscript')
    config.set('Rho', 'ARGS', '--vanilla')
    config.set('Rho', 'HARNESS', 'r_harness.R')
    cfg_file = normpath('%s/rho.cfg' % args.result_dir)
    with open(cfg_file, 'wb') as f:
        config.write(f)
    return cfg_file

def bench(args, rvm):
    for bm in benchmarks:
        bench_cmd = [
                'python', normpath('benchmarks/utility/rbench.py'),
                normpath(bm['name']),
                '--rvm', rvm['name'],
                '--warmup_rep', "%d" % bm['warmup_rep'],
                '--bench_rep', "%d" % bm['bench_rep'],
                '--timingfile',
                normpath('%s/%s-%s.csv' % (args.result_dir, rvm['id'], args.gitref))]
        if 'cfg_file' in rvm:
            bench_cmd = bench_cmd + ['--config', rvm['cfg_file']]
        subprocess.call(bench_cmd)

def main():
    args = parse_args()
    if not os.path.isdir('benchmarks'):
        # Clone the benchmark suite.
        subprocess.call(['git', 'clone', 'git@github.com:llbit/rbenchmarks.git', 'benchmarks'])
    if not os.path.isdir(args.build_dir):
        os.mkdir(args.build_dir)
    if not os.path.isdir(args.result_dir):
        os.mkdir(args.result_dir)
    # Build and benchmark Rho.
    bench(args, build_rho(args, jit=False))
    bench(args, build_rho(args, jit=True))
    # Also run CR to get a baseline for performance.
    bench(args, use_cr(jit=False))
    bench(args, use_cr(jit=True))

if __name__ == "__main__":
	main()
