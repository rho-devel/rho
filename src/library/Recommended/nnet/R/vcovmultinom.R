# file nnet/vcovmultinom.R
# copyright (c) 2003 B. D. Ripley
# Use of analytic Fisher information contributed by David Firth
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
multinomHess <- function(object, Z = model.matrix(object))
{
    probs <- fitted(object)
    coefs <- coef(object)
    if (is.vector(coefs)){ # ie there are only 2 response categories
        coefs <- t(as.matrix(coefs))
        probs <- cbind(1 - probs, probs)
    }
    coefdim <- dim(coefs)
    p <- coefdim[2L]
    k <- coefdim[1L]
    ncoefs <- k * p
    kpees <- rep(p, k)
    n <- dim(Z)[1L]
##  Now compute the observed (= expected, in this case) information,
##  e.g. as in T Amemiya "Advanced Econometrics" (1985) pp295-6.
##  Here i and j are as in Amemiya, and x, xbar are vectors
##  specific to (i,j) and to i respectively.
    info <- matrix(0, ncoefs, ncoefs)
    Names <- dimnames(coefs)
    if (is.null(Names[[1L]])) Names <- Names[[2L]]
    else Names <- as.vector(outer(Names[[2L]], Names[[1L]],
                             function(name2, name1)
                                 paste(name1, name2, sep = ":")))
    dimnames(info) <- list(Names, Names)
    x0 <- matrix(0, p, k+1)
    row.totals <- object$weights
    for (i in 1L:n){
        Zi <- Z[i, ]
        xbar <- Zi * rep(probs[i, -1, drop=FALSE], kpees)
        for (j in 1L:(k+1)){
            x <- x0
            x[, j] <- Zi
            x <- x[, -1, drop = FALSE]
            x <- x - xbar
            dim(x) <- c(1, ncoefs)
            info <- info + (row.totals[i] * probs[i, j] * crossprod(x))
        }
    }
    info
}
