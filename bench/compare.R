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

# This script generates a benchmark report comparing two commits of rho.
# The commits to compare should be supplied as arguments.

library(methods)
library(ggplot2)
library(grid)
library(gridExtra)

args <- commandArgs(trailingOnly=T)
outdir <- 'out' # Benchmark output directory (data source).

if (length(args) < 2) {
    cat('Error: please supply at least two commits to generate comparison.\n')
    quit()
}

read.stats <- function(rvm, version, timestamp) {
    filename <- paste(outdir, '/', rvm, '-', version, '.csv', sep='')
    if (file.exists(filename)) {
        df <- read.csv(filename)
        df$rvm <- rvm
        df$version <- version
        df$timestamp <- strptime(timestamp, "%Y-%m-%d %H:%M:%S")
        df
    }
}

merge.stats <- function(prev, rvm, version, timestamp) {
    if (is.data.frame(prev)) {
        rbind(prev, read.stats(rvm, version, timestamp))
    } else {
        read.stats(rvm, version, timestamp)
    }
}

versions <- read.csv(paste(outdir, '/versions', sep=''), header=F)
colnames(versions) <- c('commit', 'timestamp')
versions <- subset(versions, commit %in% args) # Use only selected versions.
cr <- NA
cr.jit <- NA
rho <- NA
rho.jit <- NA
for (i in seq_len(nrow(versions))) {
    commit <- versions$commit[i]
    timestamp <- versions$timestamp[i]
    cr <- merge.stats(cr, 'cr', commit, timestamp)
    cr.jit <- merge.stats(cr.jit, 'cr-jit', commit, timestamp)
    rho <- merge.stats(rho, 'rho', commit, timestamp)
    rho.jit <- merge.stats(rho.jit, 'rho-jit', commit, timestamp)
}

cols <- c('benchmark', 'rvm', 'version', 'timestamp', 'time')
report <- rbind(cr[cols], cr.jit[cols], rho[cols], rho.jit[cols])

# Sort versions by timestamp.
report <- report[order(report$timestamp),]
report$version <- factor(report$version, unique(report$version))

# Convert millis to seconds.
report$time <- report$time / 1000

pdf('comparison.pdf')
for (vm in unique(report$rvm)) {
    report.subset <- subset(report, rvm == vm)
    print(ggplot(report.subset, aes(x=benchmark, y=time, group=version, fill=version)) +
        geom_bar(stat='identity', width=0.5, position=position_dodge(0.5)) +
        expand_limits(y=0) +
        labs(title=paste('Version Comparison for RVM', vm), y='time (seconds)') +
        theme(axis.text.x=element_text(angle=90, hjust=1)))
}
invisible(dev.off())
