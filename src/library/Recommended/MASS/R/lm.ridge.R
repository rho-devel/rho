# file MASS/R/lm.ridge.R
# copyright (C) 1994-2006 W. N. Venables and B. D. Ripley
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
lm.ridge <- function(formula, data, subset, na.action,
    lambda = 0, model = FALSE, x = FALSE, y = FALSE, contrasts = NULL, ...)
{
    m <- match.call(expand.dots = FALSE)
    m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    Y <- model.response(m)
    X <- model.matrix(Terms, m, contrasts)
    n <- nrow(X); p <- ncol(X)
    offset <- model.offset(m)
    if(!is.null(offset)) Y <- Y - offset
    if(Inter <- attr(Terms, "intercept"))
    {
        Xm <- colMeans(X[, -Inter])
        Ym <- mean(Y)
        p <- p - 1
        X <- X[, -Inter] - rep(Xm, rep(n, p))
        Y <- Y - Ym
    } else Ym <- Xm <- NA
    Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
    X <- X/rep(Xscale, rep(n, p))
    Xs <- svd(X)
    rhs <- t(Xs$u) %*% Y
    d <- Xs$d
    lscoef <-  Xs$v %*% (rhs/d)
    lsfit <- X %*% lscoef
    resid <- Y - lsfit
    s2 <- sum(resid^2)/(n - p - Inter)
    HKB <- (p-2)*s2/sum(lscoef^2)
    LW <- (p-2)*s2*n/sum(lsfit^2)
    k <- length(lambda)
    dx <- length(d)
    div <- d^2 + rep(lambda, rep(dx,k))
    a <- drop(d*rhs)/div
    dim(a) <- c(dx, k)
    coef <- Xs$v %*% a
    dimnames(coef) <- list(names(Xscale), format(lambda))
    GCV <- colSums((Y - X %*% coef)^2)/(n-colSums(matrix(d^2/div, dx)))^2
    res <- list(coef = drop(coef), scales = Xscale,
                Inter = Inter, lambda = lambda, ym = Ym, xm = Xm,
                GCV = GCV, kHKB = HKB, kLW = LW)
    class(res) <- "ridgelm"
    res
}

print.ridgelm <- function(x, ...)
{
    print(coef(x), ...)
    invisible(x)
}

select <- function(obj) UseMethod("select")

select.ridgelm <- function(obj)
{
    cat("modified HKB estimator is", format(obj$kHKB), "\n")
    cat("modified L-W estimator is", format(obj$kLW), "\n")
    GCV <- obj$GCV
    if(length(GCV)) {
        k <- seq_along(GCV)[GCV==min(GCV)]
        cat("smallest value of GCV  at",
            format(obj$lambda[k]), "\n")
    }
}

plot.ridgelm <- function(x, ...)
    matplot(x$lambda, t(x$coef), type = "l")

coef.ridgelm <- function(object, ...)
{
    scaledcoef <- t(as.matrix(object$coef / object$scales))
    if(object$Inter) {
        inter <- object$ym - scaledcoef %*% object$xm
        scaledcoef<- cbind(Intercept=inter, scaledcoef)
    }
    drop(scaledcoef)
}
