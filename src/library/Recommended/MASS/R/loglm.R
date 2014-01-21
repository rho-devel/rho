# file MASS/R/loglm.R
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
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
denumerate <- function(x) UseMethod("denumerate")

renumerate <- function(x) UseMethod("renumerate")

denumerate.formula <- function(x)
{
    if(length(x) == 1L) {
        if(mode(x) == "numeric" ||
           (mode(x) == "name" &&
            any(substring(as.character(x), 1L, 1L) == as.character(1L:9))))
            x <- as.name(paste(".v", x, sep = ""))
    } else {
        x[[2L]] <- Recall(x[[2L]])
        if(length(x) == 3L && x[[1L]] != as.name("^"))
            x[[3L]] <- Recall(x[[3L]])
    }
    x
}

renumerate.formula <- function(x)
{
    if(length(x) == 1L) {
        if(mode(x) == "name"
	   ## these are always one-byte chars
           && nchar(xx <- as.character(x), "b") > 2
           && substring(xx, 1L, 2L) == ".v")
            x <- as.name(substring(xx, 3L))
    } else {
        x[[2L]] <- Recall(x[[2L]])
        if(length(x) == 3L && x[[1L]] != as.name("^"))
           x[[3L]] <- Recall(x[[3L]])
    }
    x
}

loglm <-
  function(formula, data, subset, na.action, ...)
{
    .call <- match.call()
    if(missing(data) || inherits(data, "data.frame")) {
        m <- match.call(expand.dots = FALSE)
        m$... <- NULL
        m[[1L]] <- quote(stats::model.frame)
        data <- eval.parent(m)
        .formula <- as.formula(attr(data, "terms"))
    } else {
        trms <- attr(data, "terms") <- terms(formula <- denumerate(formula))
        .formula <- renumerate(as.formula(trms))
    }
    loglm1(formula, data, ..., .call=.call, .formula=.formula)
}

loglm1 <- function(formula, data, ...) UseMethod("loglm1", data)

loglm1.xtabs <-
function(formula, data, ...)
{
    attr(data, "marginals") <- attr(data, "call") <- class(data) <- NULL
    NextMethod("loglm1")
}

loglm1.data.frame <-
function(formula, data, ...)
{
    trms <- attr(data, "terms")
    if(is.null(trms)) stop("'data' has no 'terms' attribute")
    if(attr(trms, "response") == 0) stop("formula specifies no response")
    resp <- match(as.character(attr(trms, "variables"))[1+attr(trms, "response")],
                  names(data))
    off <- attr(trms, "offset")
    fac <- data.frame(lapply(data[, -c(resp, off)], as.factor))
    rsp <- data[, resp]
    tab <- table(fac)
    if(max(tab) > 1L) {
#
# an extra factor needed for repeated frequencies
#
        i <- do.call("order", rev(fac))
        fac <- fac[i,  ]
        rsp <- rsp[i]
        fac$.Within. <-
            factor(unlist(sapply(tab,
                                 function(x) if(x > 0) seq(x) else NULL)))
    }
    dn <- lapply(fac, levels)
    dm <- sapply(dn, length)
    offset <- model.offset(data)
    if (is.null(offset)) offset <- 0
    offset <- rep(offset, length.out = nrow(data))
    data <- structure(array(-1, dm, dn), terms = trms)
    data[do.call("cbind", lapply(fac, as.numeric))] <- rsp
    st <- array(as.numeric(data >= 0), dm, dn)
    st[do.call("cbind", lapply(fac, as.numeric))] <- exp(offset)
    data[data < 0] <- 0
    loglm1.default(formula, data, ..., start = st)
}

loglm1.default <-
function(formula, data, start = rep(1, length(data)), fitted = FALSE,
	keep.frequencies = fitted, param = TRUE, eps =
	1/10, iter = 40, print = FALSE, ...)
{
    trms <- attr(data, "terms")
    if(is.null(trms)) stop("'data' has no 'terms' attribute")
    factors <- attr(trms, "factors") > 0
    if((r <- attr(trms, "response")))
        factors <- factors[-r,  , drop = FALSE]
    nt <- ncol(factors)
    fo <- order(colSums(factors))
    factors <- factors[, fo, drop = FALSE]
    ff <- crossprod(factors)
    keep <- rep(TRUE, nt)
    j <- 0L
    while((j <- j + 1L) < nt) keep[j] <- ff[j, j] > max(ff[j, (j + 1L):nt])
    factors <- factors[, keep, drop = FALSE]
    ldim <- length(dim(data))
    nnames <- paste(".v", 1L:ldim, sep = "")
    which <- structure(1L:ldim, names = nnames)
    if(!is.null(anames <- names(dimnames(data))))
        which <- c(which, structure(which, names = anames))
    margins <- apply(factors, 2L, function(x, which, nam)
                     as.vector(which[nam[x]]), which, rownames(factors))
    if(is.matrix(margins))
        margins <- as.list(data.frame(margins))
    else margins <- structure(as.list(margins), names = names(margins))
    Fit <- loglin(data, margins, start = start, fit = fitted,
                  param = param, eps = eps, iter = iter, print = print)
    dots <- list(...)
    if(".formula" %in% names(dots)) {
        Fit$call <- dots$.call
        Fit$formula <- dots$.formula
    }
    class(Fit) <- "loglm"
    if(keep.frequencies) Fit$frequencies <- structure(data, terms = NULL)
    if(fitted) {
        names(Fit)[match("fit", names(Fit))] <- "fitted"
        attr(Fit$fitted, "terms") <- NULL
    }
    Fit$deviance <- Fit$lrt
    Fit$nobs <- length(data)
    Fit$df <- Fit$df - sum(start == 0)
    Fit$terms <- trms # for stepAIC
    Fit
}


anova.loglm <- function(object, ..., test = c("Chisq", "chisq", "LR"))
{
    test <- match.arg(test)
    margs <- function(...) nargs()
    if(!(k <- margs(...))) return(object)
    objs <- list(object, ...)
    dfs <- sapply(objs, "[[", "df")
    o <- order( - dfs)
    objs <- objs[o]
    dfs <- c(dfs[o], 0)
    forms <- lapply(objs, function(x) x$call$formula)
    dev <- c(sapply(objs, "[[", "lrt"), 0)
    M <- array(0, c(k + 2L, 5L),
               list(c(paste("Model", 1L:(k + 1L)), "Saturated"),
                    c("Deviance", "df", "Delta(Dev)", "Delta(df)", "P(> Delta(Dev)")))
    M[, 1L] <- dev
    M[, 2L] <- dfs
    M[-1L, 3L] <- dev[1L:(k + 1L)] - dev[2L:(k + 2L)]
    M[-1L, 4L] <- dfs[1L:(k + 1L)] - dfs[2L:(k + 2L)]
    M[-1L, 5L] <- 1 - pchisq(M[-1L, 3L], M[-1L, 4L])
    res <- structure(M, formulae = forms)
    class(res) <- "anova.loglm"
    res
}

print.anova.loglm <- function(x, ...)
{
    rjustify <- function(str) {
        m <- max(n <- nchar(str, "c"))
        blanks <- format(c("", str[n == m][1L]))[1L]
        paste(substring(blanks, 0L, m - n), str, sep = "")
    }
    y <- x
    y[, 5L] <- round(y[, 5L], 5L)
    R <- array("", dim(x), dimnames(x))
    for(j in 1L:5L) {
        colj <- rjustify(c(colnames(x)[j], format(y[, j])))
        R[, j] <- colj[-1L]
        colnames(R)[j] <- colj[1L]
    }
    R[1L, 3L:5L] <- ""
    forms <- attr(x, "formulae")
    cat("LR tests for hierarchical log-linear models\n\n")
    for(i in seq_along(forms))
        cat(paste("Model ", i, ":\n", sep = ""),
            deparse(forms[[i]], width.cutoff = 500L), "\n")
    cat("\n")
    print(R, quote = FALSE)
    invisible(x)
}

print.loglm <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    ts.array <- rbind(c(x$lrt, x$df,
                        if(x$df > 0L) 1 - pchisq(x$lrt, x$df) else 1),
                      c(x$pearson, x$df,
                        if(x$df > 0L) 1 - pchisq(x$pearson, x$df)
                        else 1))
    dimnames(ts.array) <- list(c("Likelihood Ratio",
                                 "Pearson"), c("X^2", "df", "P(> X^2)"))
    cat("\nStatistics:\n")
    print(ts.array)
    invisible(x)
}

summary.loglm <- function(object, fitted = FALSE, ...)
{
    ts.array <- rbind(c(object$lrt, object$df,
                        if(object$df > 0L) 1 - pchisq(object$lrt, object$df)
                        else 1), c(object$pearson, object$df,
                                   if(object$df > 0L)
                                   1 - pchisq(object$pearson, object$df)
                                   else 1))
    dimnames(ts.array) <- list(c("Likelihood Ratio", "Pearson"),
                               c("X^2", "df", "P(> X^2)"))
    if(fitted) {
        if(is.null(object$fitted) || is.null(object$freqencies)) {
            cat("Re-fitting to find fitted values\n")
            object <- update(object, fitted = TRUE, keep.frequencies = TRUE)
        }
        fit <- format(round(object$fit, 1L))
        OE <- array(paste(format(object$freq), " (", fit, ")", sep = ""),
                    dim(fit), dimnames(object$freq))
    }  else OE <- NULL
    structure(list(formula = formula(object), tests = ts.array, oe = OE),
              class = "summary.loglm")
}

print.summary.loglm <- function(x, ...)
{
    cat("Formula:\n")
    print(formula(x))
    cat("\nStatistics:\n")
    print(x$tests)
    if(!is.null(x$oe)) {
        cat("\nObserved (Expected):\n")
        print(x$oe, quote = FALSE)
    }
    invisible(x)
}

update.loglm <- function (object, formula, ...)
{
    if (is.null(call <- object$call))
        stop("'object' has no 'call' component.  Updating not possible")
    if (fix <- !missing(formula)) {
        object$formula <- denumerate(object$formula)
        formula <- denumerate(as.formula(formula))
        call$formula <- renumerate(update.formula(formula(object), formula))
    }
    extras <- match.call(expand.dots = FALSE)$...
    if (length(extras) > 0L) {
        existing <- !is.na(match(names(extras), names(call)))
        ## do these individually to allow NULL to remove entries.
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    result <- eval.parent(call)
    result
}

fitted.loglm <- function(object, ...)
{
    if(!is.null(object$fit))
        return(unclass(object$fit))
    cat("Re-fitting to get fitted values\n")
    unclass(update(object, fitted = TRUE, keep.frequencies = FALSE)$fitted)
}

residuals.loglm <-
    function(object, type = c("deviance", "pearson", "response"), ...)
{
    type <- match.arg(type)
    if(is.null(object$fit) || is.null(object$freq)) {
        cat("Re-fitting to get frequencies and fitted values\n")
        object <- update(object, fitted = TRUE, keep.frequencies = TRUE)
    }
    y <- object$freq
    mu <- object$fit
    res <- y - mu
    nz <- mu > 0
    y <- y[nz]
    mu <- mu[nz]
    res[nz] <-
        switch(type,
               deviance = sign(y - mu) *
                 sqrt(2*abs(y*log((y + (y == 0))/mu) - (y - mu))),
               pearson = (y - mu)/sqrt(mu),
               response = y - mu)
    res
}

coef.loglm <- function(object, ...)
{
    if(!is.null(cf <- object$param)) return(cf)
    cat("Re-fitting to calculate missing coefficients\n")
    update(object, param = TRUE)$param
}

nobs.loglm  <- function(object, ...) object[["nobs"]]

