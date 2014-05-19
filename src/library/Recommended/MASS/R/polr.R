# file MASS/R/polr.R
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
# Use of transformed intercepts contributed by David Firth
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
polr <- function(formula, data, weights, start, ..., subset,
                 na.action, contrasts = NULL, Hess = FALSE, model = TRUE,
                 method = c("logistic", "probit", "cloglog", "cauchit"))
{
    m <- match.call(expand.dots = FALSE)
    method <- match.arg(method)
    if(is.matrix(eval.parent(m$data))) m$data <- as.data.frame(data)
    m$start <- m$Hess <- m$method <- m$model <- m$... <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    n <- nrow(x)
    pc <- ncol(x)
    cons <- attr(x, "contrasts") # will get dropped by subsetting
    if(xint > 0L) {
        x <- x[, -xint, drop = FALSE]
        pc <- pc - 1L
    } else warning("an intercept is needed and assumed")
    wt <- model.weights(m)
    if(!length(wt)) wt <- rep(1, n)
    offset <- model.offset(m)
    if(length(offset) <= 1L) offset <- rep(0, n)
    y <- model.response(m)
    if(!is.factor(y)) stop("response must be a factor")
    lev <- levels(y); llev <- length(lev)
    if(llev <= 2L) stop("response must have 3 or more levels")
    y <- unclass(y)
    q <- llev - 1L
    Y <- matrix(0, n, q)
    if(missing(start)) {
        # try logistic/probit regression on 'middle' cut
        q1 <- llev %/% 2L
        y1 <- (y > q1)
        X <- cbind(Intercept = rep(1, n), x)
        fit <-
            switch(method,
                   "logistic"= glm.fit(X, y1, wt, family = binomial(), offset = offset),
                   "probit" = glm.fit(X, y1, wt, family = binomial("probit"), offset = offset),
                   ## this is deliberate, a better starting point
                   "cloglog" = glm.fit(X, y1, wt, family = binomial("probit"), offset = offset),
                   "cauchit" = glm.fit(X, y1, wt, family = binomial("cauchit"), offset = offset))
        if(!fit$converged)
            stop("attempt to find suitable starting values failed")
        coefs <- fit$coefficients
        if(any(is.na(coefs))) {
            warning("design appears to be rank-deficient, so dropping some coefs")
            keep <- names(coefs)[!is.na(coefs)]
            coefs <- coefs[keep]
            x <- x[, keep[-1L], drop = FALSE]
            pc <- ncol(x)
        }
        logit <- function(p) log(p/(1 - p))
        spacing <- logit((1L:q)/(q+1L)) # just a guess
        if(method != "logistic") spacing <- spacing/1.7
        gammas <- -coefs[1L] + spacing - spacing[q1]
        start <- c(coefs[-1L], gammas)
    } else if(length(start) != pc + q)
	stop("'start' is not of the correct length")

    ans <- polr.fit(x, y, wt, start, offset, method, hessian = Hess, ...)
    beta <- ans$coefficients
    zeta <- ans$zeta
    deviance <- ans$deviance
    res <- ans$res
    niter <- c(f.evals = res$counts[1L], g.evals = res$counts[2L])

    eta <- if(pc) offset + drop(x %*% beta) else offset + rep(0, n)
    pfun <- switch(method, logistic = plogis, probit = pnorm,
                   cloglog = pgumbel, cauchit = pcauchy)
    cumpr <- matrix(pfun(matrix(zeta, n, q, byrow=TRUE) - eta), , q)
    fitted <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
    dimnames(fitted) <- list(row.names(m), lev)
    fit <- list(coefficients = beta, zeta = zeta, deviance = deviance,
                fitted.values = fitted, lev = lev, terms = Terms,
                df.residual = sum(wt) - pc - q, edf = pc + q, n = sum(wt),
                nobs = sum(wt), call = match.call(), method = method,
		convergence = res$convergence, niter = niter, lp = eta)
    if(Hess) {
        dn <- c(names(beta), names(zeta))
        H <- res$hessian
        dimnames(H) <- list(dn, dn)
        fit$Hessian <- H
    }
    if(model) fit$model <- m
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    class(fit) <- "polr"
    fit
}

print.polr <- function(x, ...)
{
    if(!is.null(cl <- x$call)) {
        cat("Call:\n")
        dput(cl, control=NULL)
    }
    if(length(coef(x))) {
        cat("\nCoefficients:\n")
        print(coef(x), ...)
    } else {
        cat("\nNo coefficients\n")
    }
    cat("\nIntercepts:\n")
    print(x$zeta, ...)
    cat("\nResidual Deviance:", format(x$deviance, nsmall=2L), "\n")
    cat("AIC:", format(x$deviance + 2*x$edf, nsmall=2L), "\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("(", mess, ")\n", sep="")
    if(x$convergence > 0)
        cat("Warning: did not converge as iteration limit reached\n")
    invisible(x)
}

vcov.polr <- function(object, ...)
{
    jacobian <- function(theta) { ## dgamma by dtheta matrix
        k <- length(theta)
        etheta <- exp(theta)
        mat <- matrix(0 , k, k)
        mat[, 1L] <- rep(1, k)
        for (i in 2L:k) mat[i:k, i] <- etheta[i]
        mat
    }

    if(is.null(object$Hessian)) {
        message("\nRe-fitting to get Hessian\n")
	utils::flush.console()
        object <- update(object, Hess = TRUE,
                         start = c(object$coefficients, object$zeta))
    }
    vc <- ginv(object$Hessian)
    pc <- length(coef(object))
    gamma <- object$zeta
    z.ind <- pc + seq_along(gamma)
    theta <- c(gamma[1L], log(diff(gamma)))
    J <- jacobian(theta)
    A <- diag(pc + length(gamma))
    A[z.ind, z.ind] <- J
    V <- A %*% vc %*% t(A)
    structure(V,  dimnames = dimnames(object$Hessian))
}

summary.polr <- function(object, digits = max(3, .Options$digits - 3),
                         correlation = FALSE, ...)
{
    cc <- c(coef(object), object$zeta)
    pc <- length(coef(object))
    q <- length(object$zeta)
    coef <- matrix(0, pc+q, 3L, dimnames=list(names(cc),
                               c("Value", "Std. Error", "t value")))
    coef[, 1L] <- cc
    vc <- vcov(object)
    coef[, 2L] <- sd <- sqrt(diag(vc))
    coef[, 3L] <- coef[, 1L]/coef[, 2L]
    object$coefficients <- coef
    object$pc <- pc
    object$digits <- digits
    if(correlation)
        object$correlation <- (vc/sd)/rep(sd, rep(pc+q, pc+q))
    class(object) <- "summary.polr"
    object
}

print.summary.polr <- function(x, digits = x$digits, ...)
{
    if(!is.null(cl <- x$call)) {
        cat("Call:\n")
        dput(cl, control=NULL)
    }
    coef <- format(round(x$coefficients, digits=digits))
    pc <- x$pc
    if(pc > 0) {
        cat("\nCoefficients:\n")
        print(x$coefficients[seq_len(pc), , drop=FALSE], quote = FALSE,
              digits = digits, ...)
    } else {
        cat("\nNo coefficients\n")
    }
    cat("\nIntercepts:\n")
    print(coef[(pc+1L):nrow(coef), , drop=FALSE], quote = FALSE,
          digits = digits, ...)
    cat("\nResidual Deviance:", format(x$deviance, nsmall=2L), "\n")
    cat("AIC:", format(x$deviance + 2*x$edf, nsmall=2L), "\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("(", mess, ")\n", sep="")
    if(!is.null(correl <- x$correlation)) {
        cat("\nCorrelation of Coefficients:\n")
        ll <- lower.tri(correl)
        correl[ll] <- format(round(correl[ll], digits))
        correl[!ll] <- ""
        print(correl[-1L, -ncol(correl)], quote = FALSE, ...)
    }
    invisible(x)
}

predict.polr <- function(object, newdata, type=c("class","probs"), ...)
{
    if(!inherits(object, "polr")) stop("not a \"polr\" object")
    type <- match.arg(type)
    if(missing(newdata)) Y <- object$fitted
    else {
        newdata <- as.data.frame(newdata)
        Terms <- delete.response(object$terms)
        m <- model.frame(Terms, newdata, na.action = function(x) x,
                         xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts = object$contrasts)
        xint <- match("(Intercept)", colnames(X), nomatch=0L)
        if(xint > 0L) X <- X[, -xint, drop=FALSE]
        n <- nrow(X)
        q <- length(object$zeta)
        eta <- drop(X %*% object$coefficients)
        pfun <- switch(object$method, logistic = plogis, probit = pnorm,
                       cloglog = pgumbel, cauchit = pcauchy)
        cumpr <- matrix(pfun(matrix(object$zeta, n, q, byrow=TRUE) - eta), , q)
        Y <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
        dimnames(Y) <- list(rownames(X), object$lev)
    }
    if(missing(newdata) && !is.null(object$na.action))
        Y <- napredict(object$na.action, Y)
    if(type == "class")
        factor(max.col(Y), levels=seq_along(object$lev), labels=object$lev)
    else drop(Y)
}

extractAIC.polr <- function(fit, scale = 0, k = 2, ...)
{
    edf <- fit$edf
    c(edf, deviance(fit) + k * edf)
}

model.frame.polr <- function(formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
    if(length(nargs) || is.null(formula$model)) {
        m <- formula$call
        m$start <- m$Hess <- m$... <- NULL
        m[[1L]] <- quote(stats::model.frame)
        m[names(nargs)] <- nargs
        if (is.null(env <- environment(formula$terms))) env <- parent.frame()
        data <- eval(m, env)
        if(!is.null(mw <- m$weights)) {
            nm <- names(data)
            nm[match("(weights)", nm)] <- as.character(mw)
            names(data) <- nm
        }
        data
    } else formula$model
}

pgumbel <- function(q, loc = 0, scale = 1, lower.tail = TRUE)
{
    q <- (q - loc)/scale
    p <- exp(-exp(-q))
    if (!lower.tail) 1 - p else p
}

dgumbel <- function (x, loc = 0, scale = 1, log = FALSE)
{
    x <- (x - loc)/scale
    d <- log(1/scale) - x - exp(-x)
    if (!log) exp(d) else d
}

anova.polr <- function (object, ..., test = c("Chisq", "none"))
{
    test <- match.arg(test)
    dots <- list(...)
    if (!length(dots))
        stop('anova is not implemented for a single "polr" object')
    mlist <- list(object, ...)
    nt <- length(mlist)
    dflis <- sapply(mlist, function(x) x$df.residual)
    s <- order(dflis, decreasing = TRUE)
    mlist <- mlist[s]
    if (any(!sapply(mlist, inherits, "polr")))
        stop('not all objects are of class "polr"')
    ns <- sapply(mlist, function(x) length(x$fitted.values))
    if(any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")
    rsp <- unique(sapply(mlist, function(x) paste(formula(x)[2L])))
    mds <- sapply(mlist, function(x) paste(formula(x)[3L]))
    dfs <- dflis[s]
    lls <- sapply(mlist, function(x) deviance(x))
    tss <- c("", paste(seq_len(nt - 1L), 2L:nt, sep = " vs "))
    df <- c(NA_integer_, -diff(dfs))
    x2 <- c(NA_real_, -diff(lls))
    pr <- c(NA_real_, 1 - pchisq(x2[-1L], df[-1L]))
    out <- data.frame(Model = mds, Resid.df = dfs, Deviance = lls,
                      Test = tss, Df = df, LRtest = x2, Prob = pr)
    names(out) <- c("Model", "Resid. df", "Resid. Dev", "Test",
                    "   Df", "LR stat.", "Pr(Chi)")
    if (test == "none") out <- out[, -7L]
    class(out) <- c("Anova", "data.frame")
    attr(out, "heading") <-
        c("Likelihood ratio tests of ordinal regression models\n",
          paste("Response:", rsp))
    out
}

polr.fit <- function(x, y, wt, start, offset, method, ...)
{
    fmin <- function(beta) {
        theta <- beta[pc + ind_q]
        gamm <- c(-Inf , cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
        eta <- offset
        if (pc) eta <- eta + drop(x %*% beta[ind_pc])
        pr <- pfun(pmin(100, gamm[y + 1] - eta)) -
            pfun(pmax(-100, gamm[y] - eta))
        if (all(pr > 0)) -sum(wt * log(pr)) else Inf
    }

    gmin <- function(beta)
    {
        jacobian <- function(theta) { ## dgamma by dtheta matrix
            k <- length(theta)
            etheta <- exp(theta)
            mat <- matrix(0 , k, k)
            mat[, 1L] <- rep(1, k)
            for (i in 2L:k) mat[i:k, i] <- etheta[i]
            mat
        }
        theta <- beta[pc + ind_q]
        gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
        eta <- offset
        if(pc) eta <- eta + drop(x %*% beta[ind_pc])
        z1 <- pmin(100, gamm[y+1L] - eta)
        z2 <- pmax(-100, gamm[y] - eta)
        pr <- pfun(z1) - pfun(z2)
        p1 <- dfun(z1); p2 <- dfun(z2)
        g1 <- if(pc) t(x) %*% (wt*(p1 - p2)/pr) else numeric()
        xx <- .polrY1*p1 - .polrY2*p2
        g2 <- - t(xx) %*% (wt/pr)
        g2 <- t(g2) %*% jacobian(theta)
        if(all(pr > 0)) c(g1, g2) else rep(NA_real_, pc+q)
    }

    pfun <- switch(method, logistic = plogis, probit = pnorm,
                   cloglog = pgumbel, cauchit = pcauchy)
    dfun <- switch(method, logistic = dlogis, probit = dnorm,
                   cloglog = dgumbel, cauchit = dcauchy)
    n <- nrow(x)
    pc <- ncol(x)
    ind_pc <- seq_len(pc)
    lev <- levels(y)
    if(length(lev) <= 2L) stop("response must have 3 or more levels")
    y <- unclass(y)
    q <- length(lev) - 1L
    ind_q <- seq_len(q)
    Y <- matrix(0, n, q)
    .polrY1 <- col(Y) == y; .polrY2 <- col(Y) == (y - 1L)
    # pc could be 0.
    s0 <- if(pc) c(start[seq_len(pc+1L)], log(diff(start[-seq_len(pc)])))
    else c(start[1L], log(diff(start)))
    res <- optim(s0, fmin, gmin, method="BFGS", ...)
    beta <- res$par[seq_len(pc)]
    theta <- res$par[pc + ind_q]
    zeta <- cumsum(c(theta[1L], exp(theta[-1L])))
    deviance <- 2 * res$value
    names(zeta) <- paste(lev[-length(lev)], lev[-1L], sep="|")
    if(pc) names(beta) <- colnames(x)
    list(coefficients = beta, zeta = zeta, deviance = deviance, res = res)
}

profile.polr <- function(fitted, which = 1L:p, alpha = 0.01,
                         maxsteps = 10, del = zmax/5, trace = FALSE, ...)
{
    Pnames <- names(B0 <- coefficients(fitted))
    pv0 <- t(as.matrix(B0))
    p <- length(Pnames)
    if(is.character(which)) which <- match(which, Pnames)
    summ <- summary(fitted)
    std.err <- summ$coefficients[, "Std. Error"]
    mf <- model.frame(fitted)
    n <- length(Y <- model.response(mf))
    O <- model.offset(mf)
    if(!length(O)) O <- rep(0, n)
    W <- model.weights(mf)
    if(length(W) == 0L) W <- rep(1, n)
    OriginalDeviance <- deviance(fitted)
    X <- model.matrix(fitted)[, -1L, drop=FALSE] # drop intercept
    zmax <- sqrt(qchisq(1 - alpha, 1))
    profName <- "z"
    prof <- vector("list", length=length(which))
    names(prof) <- Pnames[which]
    start <- c(fitted$coefficients, fitted$zeta)
    for(i in which) {
        zi <- 0
        pvi <- pv0
        Xi <- X[,  - i, drop = FALSE]
        pi <- Pnames[i]
        for(sgn in c(-1, 1)) {
            if(trace) {
                message("\nParameter:", pi, c("down", "up")[(sgn + 1)/2 + 1])
                utils::flush.console()
            }
            step <- 0
            z <- 0
            ## LP is the linear predictor including offset.
            ## LP <- X %*% fitted$coef + O
            while((step <- step + 1) < maxsteps && abs(z) < zmax) {
                bi <- B0[i] + sgn * step * del * std.err[i]
                o <- O + X[, i] * bi
                fm <- polr.fit(x = Xi, y = Y, wt = W, start = start[-i],
                               offset = o, method = fitted$method)
                ri <- pv0
                ri[, names(coef(fm))] <- coef(fm)
                ri[, pi] <- bi
                pvi <- rbind(pvi, ri)
                zz <- fm$deviance - OriginalDeviance
                if(zz > - 1e-3) zz <- max(zz, 0)
                else stop("profiling has found a better solution, so original fit had not converged")
                z <- sgn * sqrt(zz)
                zi <- c(zi, z)
            }
        }
        si <- order(zi)
        prof[[pi]] <- structure(data.frame(zi[si]), names = profName)
        prof[[pi]]$par.vals <- pvi[si, , drop = FALSE]
    }
    val <- structure(prof, original.fit = fitted, summary = summ)
    class(val) <- c("profile.polr", "profile")
    val
}

confint.polr <- function(object, parm, level = 0.95, trace = FALSE, ...)
{
    pnames <- names(coef(object))
    if(missing(parm)) parm <- seq_along(pnames)
    else if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
    message("Waiting for profiling to be done...")
    utils::flush.console()
    object <- profile(object, which = parm, alpha = (1. - level)/4.,
                      trace = trace)
    confint(object, parm=parm, level=level, trace=trace, ...)
}

confint.profile.polr <-
  function(object, parm = seq_along(pnames), level = 0.95, ...)
{
    of <- attr(object, "original.fit")
    pnames <- names(coef(of))
    if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
    a <- (1-level)/2
    a <- c(a, 1-a)
    pct <- paste(round(100*a, 1L), "%")
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(pnames[parm], pct))
    cutoff <- qnorm(a)
    for(pm in parm) {
        pro <- object[[ pnames[pm] ]]
        if(length(pnames) > 1L)
            sp <- spline(x = pro[, "par.vals"][, pm], y = pro[, 1])
        else sp <- spline(x = pro[, "par.vals"], y = pro[, 1])
        ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
}

logLik.polr <- function(object, ...)
    structure(-0.5 * object$deviance, df = object$edf,
              nobs = object[["nobs"]], class = "logLik")

nobs.polr <- function(object, ...) object[["nobs"]]

simulate.polr <- function(object, nsim = 1, seed = NULL, ...)
{
    if(!is.null(object$model) && any(model.weights(object$model) != 1))
        stop("weighted fits are not supported")

    rgumbel <- function(n, loc = 0, scale = 1) loc - scale*log(rexp(n))

    ## start the same way as simulate.lm
    if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        runif(1)                     # initialize the RNG if necessary
    if(is.null(seed))
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
	set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    rfun <- switch(object$method, logistic = rlogis, probit = rnorm,
                   cloglog = rgumbel, cauchit = rcauchy)
    eta <- object$lp
    n <- length(eta)
    res <- cut(rfun(n*nsim, eta),
               c(-Inf, object$zeta, Inf),
               labels = colnames(fitted(object)),
               ordered_result = TRUE)
    val <- split(res, rep(seq_len(nsim), each=n))
    names(val) <- paste("sim", seq_len(nsim), sep="_")
    val <- as.data.frame(val)
    if (!is.null(nm <- rownames(fitted(object)))) row.names(val) <- nm
    attr(val, "seed") <- RNGstate
    val
}
