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


# Allocator benchmarks are listed below:
benchmarks = [
    {'name': 'allocbench/small-lookup.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/large-lookup.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/huge-lookup.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/small.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/small-recursive.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/small-reuse.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/medium.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/medium-recursive.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/medium-reuse.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/large.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/large-recursive.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/large-reuse.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/huge.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/huge-recursive.R', 'warmup_rep': 0, 'bench_rep': 1},
    {'name': 'allocbench/huge-reuse.R', 'warmup_rep': 0, 'bench_rep': 1},
    ]


# The allocator benchmarks can not be run with any other Rho VM than Rho
# because the native code used in the benchmarks depends on the Rho interface.
# This could be changed by refactoring allocbench/allocator_test.cpp.
def main():
  args = benchmark.parse_args()
  benchmark.setup_benchmarks(args)
  for gitref in args.gitref:
    # Build and benchmark Rho.
    benchmark.benchmark_allocator(
        benchmarks, gitref, args, benchmark.build_rho(gitref, args, jit=False))
    if not args.skip_jit:
      benchmark.benchmark_allocator(
          benchmarks, gitref, args, benchmark.build_rho(gitref, args, jit=True))
    # Update version list file to add newly benchmarked version:
    with open(os.path.join(args.result_dir, 'versions'), 'a') as f:
      print >>f, '%s, %s' % (gitref, benchmark.get_timestamp(gitref, args))


if __name__ == '__main__':
  main()
