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
# Output is generated into files with the naming scheme
# out/rho(-jit)?-GITREF.csv # where GITREF is the Git reference.  For each
# benchmark run, CR (GnuR) is also benchmarked to get a performance baseline.

import benchmark
import os


# This list contains benchmarks to run and the number of warmup/bench runs for
# each.  Various numbers of runs are used for the benchmarks to ensure that
# short-running benchmarks run enough iterations to not be strongly affected by
# VM startup time.
benchmarks = [
    {'name': 'benchmarks/scalar/crt/crt.R', 'warmup_rep': 10, 'bench_rep': 30},
    {'name': 'benchmarks/scalar/fib/fib.R', 'warmup_rep': 4, 'bench_rep': 12},
    {'name': 'benchmarks/scalar/fib/fib_rec.R', 'warmup_rep': 2, 'bench_rep': 5},
    {'name': 'benchmarks/scalar/gcd/gcd.R', 'warmup_rep': 2, 'bench_rep': 5},
    {'name': 'benchmarks/scalar/gcd/gcd_rec.R', 'warmup_rep': 2000, 'bench_rep': 5000},
    {'name': 'benchmarks/scalar/prime/prime.R', 'warmup_rep': 2, 'bench_rep': 3},
    {'name': 'benchmarks/scalar/ForLoopAdd/ForLoopAdd.R', 'warmup_rep': 2, 'bench_rep': 3},
    {'name': 'benchmarks/shootout/nbody/nbody.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/shootout/fannkuch-redux/fannkuch-redux.R', 'warmup_rep': 0, 'bench_rep': 1},
    # Skip shootout/spectral-norm because it takes too long.
    # Skip shootout/mandelbrot to avoid duplicate benchmark IDs
    # (conflicts with riposte/mandelbrot.R).
    {'name': 'benchmarks/shootout/pidigits/pidigits.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/black_scholes.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/cleaning.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/example.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/filter1d.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/histogram.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/kmeans.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/lr_test.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/lr.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/mandelbrot.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/pca.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/pca-blocked.R', 'warmup_rep': 0, 'bench_rep': 1},
    # Skip riposte/qr.R because it fails to run (missing strip function).
    {'name': 'benchmarks/riposte/raysphere.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/sample_builtin.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/sample.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/smv_builtin.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'benchmarks/riposte/smv.R', 'warmup_rep': 0, 'bench_rep': 1},
    ]


def main():
  args = benchmark.parse_args()
  benchmark.setup_benchmarks(args)
  for gitref in args.gitref:
    # Build and benchmark Rho.
    benchmark.bench(
        benchmarks, gitref, args, benchmark.build_rho(gitref, args, jit=False))
    if not args.skip_jit:
      benchmark.bench(
          benchmarks, gitref, args, benchmark.build_rho(gitref, args, jit=True))
    # Also run CR to get a baseline for performance.
    if not args.skip_cr:
      benchmark.bench(benchmarks, gitref, args, benchmark.use_cr(jit=False))
      if not args.skip_jit:
        benchmark.bench(benchmarks, gitref, args, benchmark.use_cr(jit=True))
    # Update version list file to add newly benchmarked version:
    with open(os.path.join(args.result_dir, 'versions'), 'a') as f:
      print >>f, '%s, %s' % (gitref, benchmark.get_timestamp(gitref, args))


if __name__ == '__main__':
  main()
