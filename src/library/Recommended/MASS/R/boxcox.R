# file MASS/R/boxcox.R
# copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
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
boxcox <- function(object, ...) UseMethod("boxcox")

boxcox.formula <-
function(object, lambda = seq(-2, 2, 1/10), plotit =  TRUE,
         interp = (plotit && (m < 100)), eps = 1/50,
         xlab = expression(lambda), ylab = "log-Likelihood", ...)
{
    m <- length(lambda)
    object <- lm(object, y = TRUE, qr = TRUE, ...)
    result <- NextMethod()
    if(plotit) invisible(result)
    else result
}

boxcox.lm <-
function(object, lambda = seq(-2, 2, 1/10), plotit = TRUE,
         interp = (plotit && (m < 100)), eps = 1/50,
         xlab = expression(lambda), ylab = "log-Likelihood", ...)
{
    m <- length(lambda)
    if(is.null(object$y) || is.null(object$qr))
        object <- update(object, y = TRUE, qr = TRUE, ...)
    result <- NextMethod()
    if(plotit) invisible(result)
    else result
}

boxcox.default <-
function(object, lambda = seq(-2, 2, 1/10), plotit = TRUE,
         interp = (plotit && (m < 100)), eps = 1/
         50, xlab = expression(lambda), ylab = "log-Likelihood", ...)
{
    if(is.null(y <- object$y) || is.null(xqr <- object$qr))
        stop(gettextf("%s does not have both 'qr' and 'y' components",
                      sQuote(deparse(substitute(object)))), domain = NA)
    if(any(y <= 0))
        stop("response variable must be positive")
    n <- length(y)
    ## scale y[]  {for accuracy in  y^la - 1 }:
    y <- y / exp(mean(log(y)))
    logy <- log(y) # now  ydot = exp(mean(log(y))) == 1
    xl <- loglik <- as.vector(lambda)
    m <- length(xl)
    for(i in 1L:m) {
        if(abs(la <- xl[i]) > eps)
            yt <- (y^la - 1)/la
        else yt <- logy * (1 + (la * logy)/2 *
                           (1 + (la * logy)/3 * (1 + (la * logy)/4)))
        loglik[i] <- - n/2 * log(sum(qr.resid(xqr, yt)^2))
    }
    if(interp) {
        sp <- spline(xl, loglik, n = 100)
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
        text(xl[1L] + scx, lim + scal, " 95%", xpd = TRUE)
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
    list(x = xl, y = loglik)
}
