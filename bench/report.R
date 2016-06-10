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
    labs(title='All benchmarks [Type I & II]', y='Geometric mean of execution times') +
    theme(axis.text.x=element_text(angle=90, hjust=1))

shootout <- c('nbody.R', 'fannkuch-redux.R', 'pidigits.R')
report.shootout <- subset(report, benchmark %in% shootout)
shootout.gm <- aggregate(report.shootout$time, by=list(version=report.shootout$version, rvm=report.shootout$rvm), FUN=geom.mean)
ggplot(shootout.gm, aes(x=version, y=x, group=rvm, color=rvm)) +
    geom_line() +
    geom_point(aes(shape=rvm)) +
    expand_limits(y=0) +
    labs(title='Shootout [Type I]', y='Geometric mean of execution times') +
    theme(axis.text.x=element_text(angle=90, hjust=1))

scalar <- c('crt.R', 'fib.R', 'fib_rec.R', 'gcd.R', 'gcd_rec.R', 'prime.R', 'ForLoopAdd.R')
report.scalar <- subset(report, benchmark %in% scalar)
scalar.gm <- aggregate(report.scalar$time, by=list(version=report.scalar$version, rvm=report.scalar$rvm), FUN=geom.mean)
ggplot(scalar.gm, aes(x=version, y=x, group=rvm, color=rvm)) +
    geom_line() +
    geom_point(aes(shape=rvm)) +
    expand_limits(y=0) +
    labs(title='Scalar [Type I]', y='Geometric mean of execution times') +
    theme(axis.text.x=element_text(angle=90, hjust=1))

riposte <- c('black_scholes.R', 'cleaning.R', 'example.R', 'filter1d.R', 'histogram.R',
        'kmeans.R', 'lr_test.R', 'lr.R', 'mandelbrot.R', 'pca.R', 'pca-blocked.R',
        'raysphere.R', 'sample_builtin.R', 'sample.R', 'smv_builtin.R', 'smv.R')
report.riposte <- subset(report, benchmark %in% riposte)
riposte.gm <- aggregate(report.riposte$time, by=list(version=report.riposte$version, rvm=report.riposte$rvm), FUN=geom.mean)
ggplot(riposte.gm, aes(x=version, y=x, group=rvm, color=rvm)) +
    geom_line() +
    geom_point(aes(shape=rvm)) +
    expand_limits(y=0) +
    labs(title='Riposte [Type II]', y='Geometric mean of execution times') +
    theme(axis.text.x=element_text(angle=90, hjust=1))

for (bm in levels(report$benchmark)) {
    print(paste('Graph', bm))
    report.subset <- subset(report, benchmark == bm)
    print(ggplot(report.subset, aes(x=version, y=time, group=rvm, color=rvm)) +
        geom_line() +
        geom_point(aes(shape=rvm)) +
        expand_limits(y=0) +
        labs(title=bm, y='time (seconds)') +
        theme(axis.text.x=element_text(angle=90, hjust=1)))
}
invisible(dev.off())

#pdf('tables.pdf')
#for (bm in levels(report$benchmark)) {
#    print(paste('Table', bm))
#    report.subset <- subset(report[c('benchmark', 'rvm', 'version', 'time')], benchmark == bm)
#    grid.table(reshape(report.subset, timevar='rvm', idvar=c('benchmark', 'version'), direction='wide'))
#    grid.newpage()
#}
#invisible(dev.off())
