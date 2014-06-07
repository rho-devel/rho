# file MASS/R/cov.trob.R
# copyright (C) 1994-2011 W. N. Venables and B. D. Ripley
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
cov.trob <- function(x, wt = rep(1, n), cor = FALSE, center = TRUE, nu = 5,
                     maxit = 25, tol = 0.01)
{
    test.values <- function(x)
    {
        if(any(is.na(x)) || any(is.infinite(x)))
            stop("missing or infinite values in 'x'")
    }
    scale.simp <- function(x, center, n, p) x - rep(center, rep(n, p))

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    dn <- colnames(x)
    test.values(x)
    if(!(miss.wt <- missing(wt))) {
        wt0 <- wt
        test.values(wt)
        if(length(wt) != n)
            stop("length of 'wt' must equal number of observations")
        if(any(wt < 0)) stop("negative weights not allowed")
        if(!sum(wt)) stop("no positive weights")
        x <- x[wt > 0, , drop = FALSE]; wt <- wt[wt > 0]; n <- nrow(x)
    }
    loc <- colSums(wt * x)/sum(wt)
    if(is.numeric(center)) {
        if(length(center) != p) stop("'center' is not the right length")
        loc <- center
    } else if(is.logical(center) && !center) loc <- rep(0, p)
    use.loc <- is.logical(center) && center
    w <- wt * (1 + p/nu)
    endit <- 0
    for(iter in 1L:maxit) {
        w0 <- w
        X <- scale.simp(x, loc, n, p)
        sX <- svd(sqrt(w/sum(w)) * X, nu = 0)
        wX <- X %*% sX$v %*% diag(1/sX$d,  , p)
        Q <- drop(wX^2 %*% rep(1, p))
        w <- (wt * (nu + p))/(nu + Q)
        #    print(summary(w))
        if(use.loc) loc <- colSums(w * x)/sum(w)
        if(all(abs(w - w0) < tol)) break
        endit <- iter
    }
    if(endit == maxit || abs(mean(w) - mean(wt)) > tol ||
       abs(mean(w * Q)/p - 1) > tol)
        warning("Probable convergence failure")
    cov <- crossprod(sqrt(w) * X)/sum(wt)
    if(length(dn)) {
        dimnames(cov) <- list(dn, dn)
        names(loc) <- dn
    }
    if(miss.wt)
        ans <- list(cov = cov, center = loc, n.obs = n)
    else ans <- list(cov = cov, center = loc, wt = wt0, n.obs = n)
    if(cor) {
        sd <- sqrt(diag(cov))
        cor <- (cov/sd)/rep(sd, rep.int(p, p))
        if(length(dn)) dimnames(cor) <- list(dn, dn)
        ans <- c(ans, list(cor = cor))
    }
    ans$call <- match.call()
    ans$iter <- endit
    ans
}
