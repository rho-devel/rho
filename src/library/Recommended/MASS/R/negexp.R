# file MASS/R/negexp.R
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

negexp.SSival <- function(mCall, data, LHS)
{
    x <- eval(mCall[["x"]], data)
    if(length(x) < 3L)
        stop("at least 3 distinct 'x' values are needed")
    y <- eval(LHS, data)
    mx <- mean(x)
    b <- as.vector(lsfit(cbind(x - mx,  - (x - mx)^2/2), y)$coef)
    rx <- range(x)
    xh <- mx + b[2L]/b[3L]
    if(prod(xh - rx) < 0)
        if(xh - rx[1L] > rx[2L] - xh)
            rx[2L] <- xh
        else rx[1L] <- xh
    x0 <- c(rx[1L], sum(rx)/2, rx[2L])
    dy <- diff(b[1L] + b[2L] * (x0 - mx) - (b[3L] * (x0 - mx)^2)/2)
    th <- (x0[2L] - x0[1L])/log(dy[1L]/dy[2L])
    b <- as.vector(lsfit(exp( - x/th), y)$coef)
    pars <- list(b[1L], b[2L], th)
    names(pars) <- mCall[c("b0", "b1", "th")]
    print(unlist(pars))
    pars
}
