# file MASS/R/hist.scott.R
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

hist.scott <- function(x, prob = TRUE, xlab = deparse(substitute(x)), ...)
   invisible(hist(x, nclass.scott(x), prob=prob, xlab=xlab, ...))
hist.FD <- function(x, prob = TRUE, xlab = deparse(substitute(x)), ...)
   invisible(hist(x, nclass.FD(x), prob=prob, xlab=xlab, ...))

frequency.polygon <- function(x, nclass = nclass.freq(x),
    xlab="", ylab="", ...)
{
    hst <- hist(x, nclass, probability=TRUE, plot=FALSE, ...)
    midpoints <- 0.5 * (hst$breaks[-length(hst$breaks)]
                        + hst$breaks[-1L])
    plot(midpoints, hst$counts, type="l", xlab=xlab, ylab=ylab)
}

nclass.freq <- function(x)
{
    h <- 2.15 * sqrt(var(x)) * length(x)^(-1/5)
    ceiling(diff(range(x))/h)
}

bandwidth.nrd <- function(x)
{
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L])/1.34
    4 * 1.06 * min(sqrt(var(x)), h) * length(x) ^ (-1/5)
}
