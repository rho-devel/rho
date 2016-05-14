# Builds benchmark report.

library(methods)
library(ggplot2)

read.stats <- function(id, version) {
    filename <- paste('out/', id, '-', version, '.csv', sep='')
    df <- read.csv(filename)
    df$id <- id
    df$version <- version
    df
}

merge.stats <- function(prev, id, version) {
    if (is.data.frame(prev)) {
        rbind(prev, read.stats(id, version))
    } else {
        read.stats(id, version)
    }
}

versions <- read.csv('out/versions', header=F)
cr <- NA
cr.jit <- NA
rho <- NA
rho.jit <- NA
for (v in versions$V1) {
    cr <- merge.stats(cr, 'cr', v)
    cr.jit <- merge.stats(cr.jit, 'cr-jit', v)
    rho <- merge.stats(rho, 'rho', v)
    rho.jit <- merge.stats(rho.jit, 'rho-jit', v)
}

cols <- c('benchmark', 'id', 'version', 'time')
report <- rbind(cr[cols], cr.jit[cols], rho[cols], rho.jit[cols])
ggplot(report, aes(x=version, y=time)) + geom_bar(aes(fill=id), position='dodge', stat='identity') + labs(title='Totals')

single_report <- function(bm) {
    ggplot(subset(report, benchmark == bm), aes(x=version, y=time)) + geom_bar(aes(fill=id), position='dodge', stat='identity') + labs(title=bm)
}
single_report('crt.R')
single_report('fib.R')
single_report('fib_rec.R')
single_report('gcd.R')
single_report('gcd_rec.R')
single_report('prime.R')
single_report('ForLoopAdd.R')
