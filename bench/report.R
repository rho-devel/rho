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

# This script generates benchmark reports.
# The output directory to use can be supplied as the first argument on the command line.

library(methods)
library(ggplot2)
library(grid)
library(gridExtra)

args <- commandArgs(trailingOnly=T)

outdir <- 'out'

if (length(args) > 0) {
    outdir <- args[1]
}

read.stats <- function(rvm, version, timestamp) {
    filename <- paste(outdir, '/', rvm, '-', version, '.csv', sep='')
    df <- read.csv(filename)
    df$rvm <- rvm
    df$version <- version
    df$timestamp <- strptime(timestamp, "%Y-%m-%d %H:%M:%S")
    df
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

pdf('report.pdf')
geom.mean = function(x) exp(sum(log(x)) / length(x))
report.gm <- aggregate(report$time, by=list(version=report$version, rvm=report$rvm), FUN=geom.mean)
ggplot(report.gm, aes(x=version, y=x, group=rvm, color=rvm)) +
    geom_line() +
    geom_point(aes(shape=rvm)) +
    expand_limits(y=0) +
    labs(title='Totals', y='time (seconds)')
for (bm in levels(report$benchmark)) {
    print(paste('Graph', bm))
    report.subset <- subset(report, benchmark == bm)
    print(ggplot(report.subset, aes(x=version, y=time, group=rvm, color=rvm)) +
        geom_line() +
        geom_point(aes(shape=rvm)) +
        expand_limits(y=0) +
        labs(title=bm, y='time (seconds)'))
}
invisible(dev.off())

pdf('tables.pdf')
for (bm in levels(report$benchmark)) {
    print(paste('Table', bm))
    report.subset <- subset(report[c('benchmark', 'rvm', 'version', 'time')], benchmark == bm)
    grid.table(reshape(report.subset, timevar='rvm', idvar=c('benchmark', 'version'), direction='wide'))
    grid.newpage()
}
invisible(dev.off())
