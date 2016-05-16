# Builds benchmark report.

library(methods)
library(ggplot2)

read.stats <- function(id, version, timestamp) {
    filename <- paste('out/', id, '-', version, '.csv', sep='')
    df <- read.csv(filename)
    df$id <- id
    df$version <- version
    df$timestamp <- strptime(timestamp, "%Y-%m-%d %H:%M:%S")
    df
}

merge.stats <- function(prev, id, version, timestamp) {
    if (is.data.frame(prev)) {
        rbind(prev, read.stats(id, version, timestamp))
    } else {
        read.stats(id, version, timestamp)
    }
}

versions <- read.csv('out/versions', header=F)
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

cols <- c('benchmark', 'id', 'version', 'timestamp', 'time')
report <- rbind(cr[cols], cr.jit[cols], rho[cols], rho.jit[cols])

# Sort versions by timestamp.
report <- report[order(report$timestamp),]
report$version <- factor(report$version, unique(report$version))

ggplot(report, aes(x=version, y=time)) +
    geom_bar(aes(fill=id), position='dodge', stat='identity') +
    labs(title='Totals')

single_report <- function(bm) {
    ggplot(subset(report, benchmark == bm), aes(x=version, y=time)) +
        geom_bar(aes(fill=id), position='dodge', stat='identity') +
        labs(title=bm)
}
single_report('crt.R')
single_report('fib.R')
single_report('fib_rec.R')
single_report('gcd.R')
single_report('gcd_rec.R')
single_report('prime.R')
single_report('ForLoopAdd.R')
