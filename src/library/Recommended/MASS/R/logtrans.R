# file MASS/R/logtrans.R
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
logtrans <- function(object, ...) UseMethod("logtrans")

logtrans.default<-
function(object, ..., alpha = seq(0.5, 6, by = 0.25) - min(y),
	plotit = TRUE, interp = (plotit && (m <
	100)), xlab = "alpha", ylab = "log Likelihood")
{
    if(is.null(object$y) || is.null(object$qr))
        stop(gettextf("%s does not have both 'qr' and 'y' components",
                      sQuote(deparse(substitute(object)))), domain = NA)
    y <- object$y
    n <- length(y)
    if(any(y + min(alpha) <= 0))
        stop("Response variable must be positive after additions")
    xqr <- object$qr
    xl <- loglik <- as.vector(alpha)
    m <- length(xl)
    for(i in 1L:m) {
        rs <- qr.resid(xqr, yt <- log(y + alpha[i]))
        loglik[i] <-  - n/2 * log(sum(rs^2)) - sum(yt)
    }
    if(interp) {
        sp <- spline(alpha, loglik, n = 100)
        xl <- sp$x
        loglik <- sp$y
        m <- length(xl)
    }
    if(plotit) {
        mx <- (1L:m)[loglik == max(loglik)][1L]
        Lmax <- loglik[mx]
        lim <- Lmax - qchisq(19/20, 1)/2
        dev.hold(); on.exit(dev.flush())
        plot(xl, loglik, xlab = xlab, ylab = ylab, type
             = "l", ylim = range(loglik, lim))
        plims <- par("usr")
        abline(h = lim, lty = 3)
        y0 <- plims[3L]
        scal <- (1/10 * (plims[4L] - y0))/par("pin")[2L]
        scx <- (1/10 * (plims[2L] - plims[1L]))/par("pin")[1L]
        text(xl[1L] + scx, lim + scal, " 95%")
        la <- xl[mx]
        if(mx > 1 && mx < m)
            segments(la, y0, la, Lmax, lty = 3)
        ind <- range((1L:m)[loglik > lim])
        if(loglik[1L] < lim) {
            i <- ind[1L]
            x <- xl[i - 1] + ((lim - loglik[i - 1]) *
                              (xl[i] - xl[i - 1]))/(loglik[i] - loglik[i - 1])
            segments(x, y0, x, lim, lty = 3)
        }
        if(loglik[m] < lim) {
            i <- ind[2L] + 1
            x <- xl[i - 1] + ((lim - loglik[i - 1]) *
                              (xl[i] - xl[i - 1]))/(loglik[i] - loglik[i - 1])
            segments(x, y0, x, lim, lty = 3)
        }
    }
    invisible(list(x = xl, y = loglik))
}

logtrans.formula <-
function(object, data, ...)
{
  object <- if(missing(data)) aov(object,y = TRUE, qr = TRUE)
  else aov(object, data = data, y = TRUE, qr = TRUE)
  invisible(NextMethod("logtrans"))
}

logtrans.lm <- function(object, ...)
{
    if(is.null(object$y) || is.null(object$qr))
        object <- update(object, y = TRUE, qr = TRUE)
    invisible(NextMethod("logtrans"))
}
