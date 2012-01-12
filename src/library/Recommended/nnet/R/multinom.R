# file nnet/multinom.R
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

multinom <-
    function(formula, data, weights, subset, na.action,
             contrasts = NULL, Hess = FALSE, summ = 0, censored = FALSE,
             model = FALSE, ...)
{
    class.ind <- function(cl)
    {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    summ2 <- function(X, Y)
    {
        X <- as.matrix(X)
        Y <- as.matrix(Y)
        n <- nrow(X)
        p <- ncol(X)
        q <- ncol(Y)
        Z <- t(cbind(X, Y))
        storage.mode(Z) <- "double"
        z <- .C(VR_summ2,
                as.integer(n),
                as.integer(p),
                as.integer(q),
                Z = Z,
                na = integer(1L))
        Za <- t(z$Z[, 1L:z$na, drop = FALSE])
        list(X = Za[, 1L:p, drop = FALSE], Y = Za[, p + 1L:q])
    }

    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$summ <- m$Hess <- m$contrasts <- m$censored <- m$model <- m$... <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    X <- model.matrix(Terms, m, contrasts)
    cons <- attr(X, "contrasts")
    Xr <- qr(X)$rank
    Y <- model.response(m)
    if(!is.matrix(Y)) Y <- as.factor(Y)
    w <- model.weights(m)
    if(length(w) == 0L)
        if(is.matrix(Y)) w <- rep(1, dim(Y)[1L])
        else w <- rep(1, length(Y))
    lev <- levels(Y)
    if(is.factor(Y)) {
        counts <- table(Y)
        if(any(counts == 0L)) {
            empty <- lev[counts == 0L]
            warning(sprintf(ngettext(length(empty),
                                     "group %s is empty",
                                     "groups %s are empty"),
                            paste(sQuote(empty), collapse=" ")), domain = NA)
            Y <- factor(Y, levels=lev[counts > 0L])
            lev <- lev[counts > 0L]
        }
        if(length(lev) < 2L)
            stop("need two or more classes to fit a multinom model")
        if(length(lev) == 2L) Y <- as.vector(unclass(Y)) - 1
        else Y <- class.ind(Y)
    }
    if(summ == 1) {
        Z <- cbind(X, Y)
        z1 <- cumprod(apply(Z, 2L, max)+1)
        Z1 <- apply(Z, 1L, function(x) sum(z1*x))
        oZ <- order(Z1)
        Z2 <- !duplicated(Z1[oZ])
        oX <- (seq_along(Z1)[oZ])[Z2]
        X <- X[oX, , drop=FALSE]
        Y <- if(is.matrix(Y)) Y[oX, , drop=FALSE] else Y[oX]
        w <- diff(c(0,cumsum(w))[c(Z2,TRUE)])
        print(dim(X))
    }
    if(summ == 2) {
        Z <- summ2(cbind(X, Y), w)
        X <- Z$X[, 1L:ncol(X)]
        Y <- Z$X[, ncol(X) + 1L:ncol(Y), drop = FALSE]
        w <- Z$Y
        print(dim(X))
    }
    if(summ == 3) {
        Z <- summ2(X, Y*w)
        X <- Z$X
        Y <- Z$Y[, 1L:ncol(Y), drop = FALSE]
        w <- rep(1, nrow(X))
        print(dim(X))
    }
    offset <- model.offset(m)
    r <- ncol(X)
    if(is.matrix(Y)) {
        # 3 or more response levels or direct matrix spec.
        p <- ncol(Y)
        sY <- Y %*% rep(1, p)
        if(any(sY == 0)) stop("some case has no observations")
        if(!censored) {
            Y <- Y / matrix(sY, nrow(Y), p)
            w <- w*sY
        }
        if(length(offset) > 1L) {
            if(ncol(offset) !=  p) stop("ncol(offset) is wrong")
            mask <- c(rep(FALSE, r+1L+p),
                      rep(c(FALSE, rep(TRUE, r), rep(FALSE, p)), p-1L) )
            X <- cbind(X, offset)
            Wts <- as.vector(rbind(matrix(0, r+1L, p), diag(p)))
            fit <- nnet.default(X, Y, w, Wts=Wts, mask=mask, size=0, skip=TRUE,
                                softmax=TRUE, censored=censored, rang=0, ...)
        } else {
            mask <- c(rep(FALSE, r+1L), rep(c(FALSE, rep(TRUE, r)), p-1L) )
            fit <- nnet.default(X, Y, w, mask=mask, size=0, skip=TRUE,
                                softmax=TRUE, censored=censored, rang=0, ...)
        }
    } else {
                                        # 2 response levels
        if(length(offset) <= 1L) {
            mask <- c(FALSE, rep(TRUE, r))
            fit <- nnet.default(X, Y, w, mask=mask, size=0, skip=TRUE,
                                entropy=TRUE, rang=0, ...)
        } else {
            mask <- c(FALSE, rep(TRUE, r), FALSE)
            Wts <- c(rep(0, r+1L), 1)
            X <- cbind(X, offset)
            fit <- nnet.default(X, Y, w, Wts=Wts, mask=mask, size=0, skip=TRUE,
                                entropy=TRUE, rang=0, ...)
        }
    }
    fit$formula <- as.vector(attr(Terms, "formula"))
    fit$terms <- Terms
    fit$call <- call
    fit$weights <- w
    fit$lev <- lev
    fit$deviance <- 2 * fit$value
    fit$rank <- Xr
    edf <- ifelse(length(lev) == 2L, 1, length(lev)-1)*Xr
    if(is.matrix(Y)) {
        edf <- (ncol(Y)-1)*Xr
        if(length(dn <- colnames(Y)) > 0) fit$lab <- dn
        else fit$lab <- 1L:ncol(Y)
    }
    fit$coefnames <- colnames(X)
    fit$vcoefnames <- fit$coefnames[1L:r] # remove offset cols
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    fit$edf <- edf
    fit$AIC <- fit$deviance + 2 * edf
    if(model) fit$model <- m
    class(fit) <- c("multinom", "nnet")
    if(Hess) fit$Hessian <- multinomHess(fit, X)
    fit
}

predict.multinom <- function(object, newdata, type=c("class","probs"), ...)
{
    if(!inherits(object, "multinom")) stop("not a \"multinom\" fit")
    type <- match.arg(type)
    if(missing(newdata)) Y <- fitted(object)
    else {
        newdata <- as.data.frame(newdata)
        rn <- row.names(newdata)
        Terms <- delete.response(object$terms)
        m <- model.frame(Terms, newdata, na.action = na.omit,
                         xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        keep <- match(row.names(m), rn)
        X <- model.matrix(Terms, m, contrasts = object$contrasts)
        Y1 <- predict.nnet(object, X)
        Y <- matrix(NA, nrow(newdata), ncol(Y1),
                    dimnames = list(rn, colnames(Y1)))
        Y[keep, ] <- Y1
    }
    switch(type, class={
        if(length(object$lev) > 2L)
            Y <- factor(max.col(Y), levels=seq_along(object$lev),
                        labels=object$lev)
        if(length(object$lev) == 2L)
            Y <- factor(1 + (Y > 0.5), levels=1L:2L, labels=object$lev)
        if(length(object$lev) == 0L)
            Y <- factor(max.col(Y), levels=seq_along(object$lab),
                        labels=object$lab)
    }, probs={})
    drop(Y)
}

print.multinom <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
        cat("Call:\n")
        dput(cl, control = NULL)
    }
    cat("\nCoefficients:\n")
    print(coef(x), ...)
    cat("\nResidual Deviance:", format(x$deviance), "\n")
    cat("AIC:", format(x$AIC), "\n")
    invisible(x)
}

coef.multinom <- function(object, ...)
{
    r <- length(object$vcoefnames)
    if(length(object$lev) == 2L) {
        coef <- object$wts[1L+(1L:r)]
        names(coef) <- object$vcoefnames
    } else {
        coef <- matrix(object$wts, nrow = object$n[3L], byrow=TRUE)[, 1L+(1L:r), drop=FALSE]
        if(length(object$lev)) dimnames(coef) <- list(object$lev, object$vcoefnames)
        if(length(object$lab)) dimnames(coef) <- list(object$lab, object$vcoefnames)
        coef <- coef[-1L, , drop=FALSE]
    }
    coef
}

drop1.multinom <- function(object, scope, sorted = FALSE, trace = FALSE, ...)
{
    if(!inherits(object, "multinom")) stop("not a \"multinom\" fit")
    if(missing(scope)) scope <- drop.scope(object)
    else {
        if(!is.character(scope))
            scope <- attr(terms(update.formula(object, scope)), "term.labels")
        if(!all(match(scope, attr(object$terms, "term.labels"),
                      nomatch = 0L)))
            stop("'scope' is not a subset of term labels")
    }
    ns <- length(scope)
    ans <- matrix(nrow = ns+1L, ncol = 2L,
                  dimnames = list(c("<none>", scope), c("Df", "AIC")))
    ans[1, ] <- c(object$edf, object$AIC)
    n0 <- length(object$residuals)
    i <- 2L
    for(tt in scope) {
        cat("trying -", tt,"\n")
        nobject <- update(object, paste("~ . -", tt), trace = trace,
                          evaluate = FALSE)
        nobject <- eval.parent(nobject)
        if(nobject$edf == object$edf) nobject$AIC <- NA
        ans[i, ] <- c(nobject$edf, nobject$AIC)
        if(length(nobject$residuals) != n0)
            stop("number of rows in use has changed: remove missing values?")
        i <- i+1L
    }
    if(sorted) ans <- ans[order(ans[, 2L]), ]
    as.data.frame(ans)
}

add1.multinom <- function(object, scope, sorted = FALSE, trace = FALSE, ...)
{
    if(!inherits(object, "multinom")) stop("not a \"multinom\" fit")
    if(!is.character(scope))
        scope <- add.scope(object, update.formula(object, scope,
                                                  evaluate = FALSE))
    if(!length(scope))
        stop("no terms in 'scope' for adding to object")
    ns <- length(scope)
    ans <- matrix(nrow = ns+1L, ncol = 2L,
                  dimnames = list(c("<none>",paste("+",scope,sep="")),
                  c("Df", "AIC")))
    ans[1L, ] <- c(object$edf, object$AIC)
    n0 <- length(object$residuals)
    i <- 2L
    for(tt in scope) {
        cat("trying +", tt,"\n")
        nobject <- update(object, as.formula(paste("~ . +", tt)), trace = trace,
                          evaluate = FALSE)
        nobject <- eval.parent(nobject)
        if(nobject$edf == object$edf) nobject$AIC <- NA
        ans[i, ] <- c(nobject$edf, nobject$AIC)
        if(length(nobject$residuals) != n0)
            stop("number of rows in use has changed: remove missing values?")
        i <- i+1L
    }
    if(sorted) ans <- ans[order(ans[, 2L]), ]
    as.data.frame(ans)
}

extractAIC.multinom <- function(fit, scale, k = 2, ...)
    c(fit$edf, fit$AIC + (k-2)*fit$edf)

vcov.multinom <- function(object, ...)
{
    ginv <- function(X, tol = sqrt(.Machine$double.eps))
    {
    #
    # simplified version of ginv in MASS
    #
        Xsvd <- svd(X)
        Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
        if(!any(Positive)) array(0, dim(X)[2L:1L])
        else Xsvd$v[, Positive] %*% ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive]))
    }

    if(is.null(Hess <- object$Hessian)) Hess <- multinomHess(object)
    structure(ginv(Hess), dimnames = dimnames(Hess))
}

summary.multinom <-
function(object, correlation = FALSE, digits = options()$digits,
         Wald.ratios = FALSE, ...)
{
    vc <- vcov(object)
    r <- length(object$vcoefnames)
    se <- sqrt(diag(vc))
    if(length(object$lev) == 2L) {
        coef <- object$wts[1L + (1L:r)]
        stderr <- se
        names(coef) <- names(stderr) <- object$vcoefnames
    } else {
        coef <- matrix(object$wts, nrow = object$n[3L],
                       byrow = TRUE)[-1L, 1L + (1L:r), drop = FALSE]
        stderr <- matrix(se, nrow = object$n[3L] - 1L, byrow = TRUE)
        if(length(l <- object$lab) || length(l <- object$lev))
            dimnames(coef) <- dimnames(stderr) <-
                list(l[-1L], object$vcoefnames)
    }
    object$is.binomial <- (length(object$lev) == 2L)
    object$digits <- digits
    object$coefficients <- coef
    object$standard.errors <- stderr
    if(Wald.ratios) object$Wald.ratios <- coef/stderr
    if(correlation) object$correlation <- vc/outer(se, se)
    class(object) <- "summary.multinom"
    object
}

print.summary.multinom <- function(x, digits = x$digits, ...)
{
    if(!is.null(cl <- x$call)) {
        cat("Call:\n")
        dput(cl, control = NULL)
    }
    cat("\nCoefficients:\n")
    if(x$is.binomial) {
        print(cbind(Values = x$coefficients,
                    "Std. Err." = x$standard.errors,
                    "Value/SE" = x$Wald.ratios),
              digits = digits)
    } else {
        print(x$coefficients, digits = digits)
        cat("\nStd. Errors:\n")
        print(x$standard.errors, digits = digits)
        if(!is.null(x$Wald.ratios)) {
            cat("\nValue/SE (Wald statistics):\n")
            print(x$coefficients/x$standard.errors, digits = digits)
        }
    }
    cat("\nResidual Deviance:", format(x$deviance), "\n")
    cat("AIC:", format(x$AIC), "\n")
    if(!is.null(correl <- x$correlation)) {
        p <- dim(correl)[2L]
        if(p > 1) {
            cat("\nCorrelation of Coefficients:\n")
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits))
            correl[!ll] <- ""
            print(correl[-1L, -p], quote = FALSE, ...)
        }
    }
    invisible(x)
}

anova.multinom <- function(object, ..., test = c("Chisq", "none"))
{
    test <- match.arg(test)
    dots <- list(...)
    if(length(dots) == 0)
        stop('anova is not implemented for a single "multinom" object')
    mlist <- list(object, ...)
    nt <- length(mlist)
    dflis <- sapply(mlist, function(x) x$edf)
    s <- order(dflis)
    ## careful, might use na.exclude here
    dflis <- nrow(object$residuals) * (ncol(object$residuals)-1) - dflis
    mlist <- mlist[s]
    if(any(!sapply(mlist, inherits, "multinom")))
        stop('not all objects are of class "multinom"')
    ns <- sapply(mlist, function(x) length(x$residuals))
    if(any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")
    rsp <- unique(sapply(mlist, function(x) paste(formula(x)[2L])))
    mds <- sapply(mlist, function(x) paste(formula(x)[3L]))
    dfs <- dflis[s]
    lls <- sapply(mlist, function(x) deviance(x))
    tss <- c("", paste(1L:(nt - 1), 2L:nt, sep = " vs "))
    df <- c(NA, -diff(dfs))
    x2 <- c(NA, -diff(lls))
    pr <- c(NA, 1 - pchisq(x2[-1L], df[-1L]))
    out <- data.frame(Model = mds, Resid.df = dfs,
                      Deviance = lls, Test = tss, Df = df, LRtest = x2,
                      Prob = pr)
    names(out) <- c("Model", "Resid. df", "Resid. Dev", "Test",
                    "   Df", "LR stat.", "Pr(Chi)")
    if(test == "none") out <- out[, 1L:6L]
    class(out) <- c("Anova", "data.frame")
    attr(out, "heading") <-
        c("Likelihood ratio tests of Multinomial Models\n",
          paste("Response:", rsp))
    out
}


model.frame.multinom <- function(formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
    if(length(nargs) || is.null(formula$model)) {
        oc <- formula$call
        oc[[1L]] <- as.name("model.frame")
        m <- match(names(oc)[-1L], c("formula", "data", "na.action", "subset"))
        oc <- oc[c(TRUE, !is.na(m))]
        oc[names(nargs)] <- nargs
        if (is.null(env <- environment(formula$terms))) env <- parent.frame()
        eval(oc, env)
    } else formula$model
}

confint.multinom <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    ## matrix case covers e.g. multinom.
    pnames <- if(is.matrix(cf)) colnames(cf) else names(cf)
    if (missing(parm))
        parm <- seq_along(pnames)
    else if (is.character(parm))
        parm <- match(parm, pnames, nomatch = 0L)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(round(100*a, 1), "%")
    fac <- qnorm(a)
    if(is.matrix(cf)) {
        ses <- matrix(sqrt(diag(vcov(object))), ncol=ncol(cf),
                      byrow=TRUE)[, parm, drop = FALSE]
        cf <- cf[, parm, drop = FALSE]
        ci <- array(NA, dim = c(dim(cf), 2L),
                    dimnames = c(dimnames(cf), list(pct)))
        ci[,,1L] <- cf + ses*fac[1L]
        ci[,,2L] <- cf + ses*fac[2L]
        aperm(ci, c(2L,3L,1L))
    } else {
        ci <- array(NA, dim = c(length(parm), 2L),
                    dimnames = list(pnames[parm], pct))
        ses <- sqrt(diag(vcov(object)))[parm]
        ci[] <- cf[parm] + ses %o% fac
        ci
    }
}

logLik.multinom <- function(object, ...)
    structure(-0.5 * object$deviance, df = object$edf, class = "logLik")
