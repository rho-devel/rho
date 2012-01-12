# file MASS/R/eqscplot.R
# copyright (C) 1994-2005 W. N. Venables and B. D. Ripley
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
eqscplot <- function(x, y, ratio = 1, tol = 0.04, uin, ...)
{
  dots <- list(...); nmdots <- names(dots)
  Call <- match.call()
  Call$ratio <- Call$tol <- Call$uin <- NULL
  if(is.matrix(x)) {
    y <- x[, 2]
    x <- x[, 1]
    if(!is.null(dn <- colnames(x))) {
      xlab0 <- dn[1L]
      ylab0 <- dn[2L]
    } else {
      xlab0 <- ""
      ylab0 <- ""
    }
  } else if(is.list(x)) {
    y <- x$y
    x <- x$x
    xlab0 <- "x"; ylab0 <- "y"
  } else {
    xlab0 <- deparse(substitute(x))
    ylab0 <- deparse(substitute(y))
  }
  Call$x <- x; Call$y <- y
  Call$xlab <- if("xlab" %in% nmdots) dots$xlab else xlab0
  Call$ylab <- if("ylab" %in% nmdots) dots$ylab else ylab0
  xlim <- if("xlim" %in% nmdots) dots$xlim else range(x[is.finite(x)])
  ylim <- if("ylim" %in% nmdots) dots$ylim else range(y[is.finite(y)])
  midx <- 0.5 * (xlim[2L] + xlim[1L])
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2L] - xlim[1L])
  midy <- 0.5 * (ylim[2L] + ylim[1L])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2L] - ylim[1L])
  oldpin <- par("pin")
  xuin <- oxuin <- oldpin[1L]/abs(diff(xlim))
  yuin <- oyuin <- oldpin[2L]/abs(diff(ylim))
  if(missing(uin)) {
    if(yuin > xuin*ratio) yuin <- xuin*ratio
    else xuin <- yuin/ratio
  } else {
    if(length(uin) == 1L) uin <- uin * c(1, ratio)
    if(any(c(xuin, yuin) < uin)) stop("'uin' is too large to fit plot in")
    xuin <- uin[1L]; yuin <- uin[2L]
  }
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  Call$xlim <- xlim
  Call$ylim <- ylim
  Call$xaxs <- Call$yaxs <- "i"
  Call[[1L]] <- as.name("plot")
  #plot(x, y, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i",
  #     xlab = xlab, ylab = ylab, ...)
  eval.parent(Call)
}
