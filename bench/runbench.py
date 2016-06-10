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
import re

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
        { 'name': 'benchmarks/shootout/nbody/nbody.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/shootout/fannkuch-redux/fannkuch-redux.R', 'warmup_rep': 0, 'bench_rep': 1 },
        # Skip spectral-norm.R because it takes too long for incremental benchmarking.
        #{ 'name': 'benchmarks/shootout/spectral-norm/spectral-norm.R', 'warmup_rep': 0, 'bench_rep': 1 },
        # Skip mandelbrot.R to avoid duplicate benchmark IDs (conflicts with riposte/mandelbrot.R).
        #{ 'name': 'benchmarks/shootout/mandelbrot/mandelbrot.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/shootout/pidigits/pidigits.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/black_scholes.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/cleaning.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/example.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/filter1d.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/histogram.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/kmeans.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/lr_test.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/lr.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/mandelbrot.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/pca.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/pca-blocked.R', 'warmup_rep': 0, 'bench_rep': 1 },
        # Skip qr.R because it fails to run (missing strip function).
        #{ 'name': 'benchmarks/riposte/qr.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/raysphere.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/sample_builtin.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/sample.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/smv_builtin.R', 'warmup_rep': 0, 'bench_rep': 1 },
        { 'name': 'benchmarks/riposte/smv.R', 'warmup_rep': 0, 'bench_rep': 1 },
        ]

def parse_args():
    parser = argparse.ArgumentParser(description='''Runs R benchmarks for a specific version of Rho.''')
    parser.add_argument('gitref', nargs='+', help='The Rho git reference to benchmark')
    parser.add_argument(
            '--repository', default='git@github.com:rho-devel/rho',
            help='The git repository to clone from.')
    parser.add_argument(
            '--build_dir', default='rho',
            help='The directory to build Rho in. The Rho repository is cloned into this directory.')
    parser.add_argument('--result_dir', default='out',
            help='The output directory to store benchmark results in.')
    parser.add_argument('--skip-cr', dest='skip_cr', action='store_true',
            help='Skips benchmarking CR as baseline.')
    parser.add_argument('--skip-jit', dest='skip_jit', action='store_true',
            help='Skips benchmarking the JIT variants.')
    parser.add_argument('--no-clean', dest='no_clean', action='store_true',
            help='The build directory is not cleaned before building each Rho revision. This speeds up the benchmarking process but may affect accuracy.')
    args = parser.parse_args()
    return args

# Set up RVM parameters to run Gnu R.
def use_cr(jit):
    if jit:
        return { 'name': 'R-bytecode', 'id': 'cr-jit' }
    else:
        return { 'name': 'R', 'id': 'cr' }

def build_rho(gitref, args, jit, build=True):
    bench_dir = os.getcwd()
    rvm = { 'name': 'Rho' }
    if jit:
        rvm['id'] = 'rho-jit'
    else:
        rvm['id'] = 'rho'
    try:
        os.chdir(args.build_dir)
        if not os.path.isdir('.git'):
            # Clone Rho into the local directory.
            exit_code = subprocess.call(['git', 'clone', args.repository, '.'])
        else:
            # Fetch latest commits.
            subprocess.call(['git', 'fetch', 'origin'])
        # Clean and switch to the selected revision.
        subprocess.call(['git', 'reset', '--hard', gitref])
        if build:
            if not args.no_clean:
                subprocess.call(['git', 'clean', '-fd'])
                subprocess.call(['git', 'clean', '-fX'])
            subprocess.call(['tools/rsync-recommended'])
            # Use Clang to build (needed for the LLVM JIT build).
            env = os.environ.copy()
            env['CC'] = 'clang'
            env['CXX'] = 'clang++'
            if jit:
                # Build with JIT enabled.
                # Requires llvm-config to be on PATH.
                subprocess.call(['./configure', '--with-blas', '--with-lapack', '--enable-llvm-jit'], env=env)
            else:
                subprocess.call(['./configure', '--with-blas', '--with-lapack'], env=env)
            subprocess.call(['make', '-j2'])
    finally:
        os.chdir(bench_dir)
    # Create a new config file because the benchmark suite is not aware of our
    # Rho build.
    rvm['cfg_file'] = write_config(args, rvm, jit)
    return rvm

# Returns the timestamp for a Git reference.
def get_timestamp(gitref, args):
    bench_dir = os.getcwd()
    try:
        os.chdir(args.build_dir)
        return subprocess.check_output(['git', 'show', '-s', '--format=%ci', gitref, '--'])
    finally:
        os.chdir(bench_dir)

# Write custom config file for running Rho in the benchmark suite.
def write_config(args, rvm, jit):
    config = ConfigParser.RawConfigParser()
    config.add_section('GENERAL')
    config.set('GENERAL', 'WARMUP_REP', 2)
    config.set('GENERAL', 'BENCH_REP', 5)
    config.set('GENERAL', 'PERF_TMP', '_perf.tmp')
    config.set('GENERAL', 'PERF_REP', 1)
    config.set('GENERAL', 'PERF_CMD', 'perf stat -r %(PERF_REP)s -x, -o %(PERF_TMP)s --append')
    config.add_section('Rho')
    if jit:
        config.set('Rho', 'ENV', 'R_COMPILE_PKGS=1 R_ENABLE_JIT=2')
    else:
        config.set('Rho', 'ENV', 'R_COMPILE_PKGS=0 R_ENABLE_JIT=0')
    config.set('Rho', 'HOME', os.path.realpath('%s/bin' % args.build_dir))
    config.set('Rho', 'CMD', 'Rscript')
    config.set('Rho', 'ARGS', '--vanilla')
    config.set('Rho', 'HARNESS', 'r_harness.R')
    config.set('Rho', 'HARNESS_ARGS', 'FALSE')
    cfg_file = normpath('%s/rho.cfg' % args.result_dir)
    with open(cfg_file, 'wb') as f:
        config.write(f)
    return cfg_file

def bench(gitref, args, rvm):
    print('Starting benchmark runs for commit %s with RVM %s.' % (gitref, rvm['name']))
    for bm in benchmarks:
        bench_cmd = [
                'python', normpath('benchmarks/utility/rbench.py'),
                normpath(bm['name']),
                '--rvm', rvm['name'],
                '--warmup_rep', "%d" % bm['warmup_rep'],
                '--bench_rep', "%d" % bm['bench_rep'],
                '--timingfile',
                normpath('%s/%s-%s.csv' % (args.result_dir, rvm['id'], gitref))]
        if 'cfg_file' in rvm:
            bench_cmd = bench_cmd + ['--config', rvm['cfg_file']]
        output = subprocess.check_output(bench_cmd)
        for line in output.splitlines():
            if line.startswith('[rbench]benchmarks'):
                print(line)
            elif line.startswith('nan,time'):
                print(line)
            elif re.match(r'\d+.*,time', line):
                print(line)

# Set up the benchmark suite. Generates input data for the benchmarks that need input data.
def setup_benchmarks():
    # Clone the benchmark suite.
    subprocess.call(['git', 'clone', 'git@github.com:rho-devel/benchmarks.git', 'benchmarks'])
    bench_dir = os.getcwd()
    try:
        # Generate benchmark input data.
        os.chdir(normpath('benchmarks/riposte'))
        subprocess.call(['curl', 'https://cran.r-project.org/src/contrib/clusterGeneration_1.3.4.tar.gz', '-o', 'clusterGeneration_1.3.4.tar.gz'])
        subprocess.call(['R', 'CMD', 'INSTALL', 'clusterGeneration_1.3.4.tar.gz'])
        os.mkdir('data')
        subprocess.call(['Rscript', 'gen_kmeans.R'])
        subprocess.call(['Rscript', 'gen_lr.R'])
        subprocess.call(['Rscript', 'gen_pca.R'])
    finally:
        os.chdir(bench_dir)


def main():
    args = parse_args()
    if not os.path.isdir('benchmarks'):
        setup_benchmarks()
    if not os.path.isdir(args.build_dir):
        os.mkdir(args.build_dir)
    if not os.path.isdir(args.result_dir):
        os.mkdir(args.result_dir)
    for gitref in args.gitref:
        # Build and benchmark Rho.
        bench(gitref, args, build_rho(gitref, args, jit=False))
        if not args.skip_jit:
            bench(gitref, args, build_rho(gitref, args, jit=True))
        # Also run CR to get a baseline for performance.
        if not args.skip_cr:
            bench(gitref, args, use_cr(jit=False))
            if not args.skip_jit:
                bench(gitref, args, use_cr(jit=True))
        # Update version list file to add newly benchmarked version:
        with open(os.path.join(args.result_dir, 'versions'), 'a') as f:
            print >>f, '%s, %s' % (gitref, get_timestamp(gitref, args))

if __name__ == "__main__":
	main()
