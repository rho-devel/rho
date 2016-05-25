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

# Simple script to decide which commits to bechmark nightly.  This script only
# writes the commit hashes for commits to benchmark to an output file. The
# output needs to be passed to runbench.py.  This script should not not run the
# runbench.py script because the R environment variables would be inherited by
# the process and affect benchmark running.
#
# Example usage of this script:
# $ Rscript incremental.R 4 && cat commits | xargs python runbench.py
#
# The number of commits to benchmark can be specified as the first command line
# argument, the default is 3 commits.

num_commit <- 3 # Limits number of commits to be benchmarked.
args <- commandArgs(trailingOnly=T)
if (length(args) > 0) {
	num_commit <- as.numeric(args[1])
}

system2('git', c('fetch', 'origin')) # Get latest commits.
revs <- system2('git', c('rev-list', 'origin/master', '--'), stdout=T)
revs <- head(revs, n=50) # Benchmark at most 50 commits back in time.
revs <- substring(revs, 1, 7) # Take first 7 characters of each commit hash.

commits <- NA
if (file.exists('out/versions')) {
	# Get previous benchmarked versions.
	commits <- read.csv('out/versions', header=F)$V1
}

out <- c()
for (rev in revs) {
	if (num_commit == 0) {
		break
	} else if (!(rev %in% commits)) {
		print(paste("Selected commit", rev))
		out <- c(out, rev)
		num_commit <- num_commit - 1
	}
}
write(file='commits', out)
