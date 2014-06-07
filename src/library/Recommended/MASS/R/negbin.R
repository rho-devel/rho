# file MASS/R/negbin.R
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
#
anova.negbin <- function(object, ..., test = "Chisq")
{
  dots <- list(...)
  if(length(dots) == 0L) {
    warning("tests made without re-estimating 'theta'")
    object$call[[1L]] <- quote(stats::glm)
    if(is.null(object$link)) object$link <- as.name("log")
    object$call$family <-
        call("negative.binomial", theta = object$ theta, link = object$link)
    NextMethod(object, test = test)
  } else {
    if(test != "Chisq")
      warning("only Chi-squared LR tests are implemented")
    mlist <- list(object, ...)
    nt <- length(mlist)
    dflis <- sapply(mlist, function(x) x$df.residual)
    s <- sort.list(-dflis)
    mlist <- mlist[s]
    if(any(!sapply(mlist, inherits, "negbin")))
      stop("not all objects are of class \"negbin\"")
    rsp <- unique(sapply(mlist, function(x) paste(formula(x)[2L])))
    mds <- sapply(mlist, function(x) paste(formula(x)[3L]))
    ths <- sapply(mlist, function(x) x$theta)
    dfs <- dflis[s]
    lls <- sapply(mlist, function(x) x$twologlik)
    tss <- c("", paste(1L:(nt - 1L), 2:nt, sep = " vs "))
    df <- c(NA,  - diff(dfs))
    x2 <- c(NA, diff(lls))
    pr <- c(NA, 1 - pchisq(x2[-1L], df[-1L]))
    out <- data.frame(Model = mds, theta = ths, Resid.df = dfs,
                      "2 x log-lik." = lls, Test = tss, df = df, LRtest = x2,
                      Prob = pr)
    names(out) <- c("Model", "theta", "Resid. df",
                    "   2 x log-lik.", "Test", "   df", "LR stat.", "Pr(Chi)")
    class(out) <- c("Anova", "data.frame")
    attr(out, "heading") <-
      c("Likelihood ratio tests of Negative Binomial Models\n",
        paste("Response:", rsp))
    out
  }
}

print.Anova <- function(x, ...)
{
    heading <- attr(x, "heading")
    if(!is.null(heading)) cat(heading, sep = "\n")
    attr(x, "heading") <- NULL
    res <- format.data.frame(x, ...)
    nas <- is.na(x) # format loses this
    res[] <- sapply(seq_len(ncol(res)), function(i){
        x <- as.character(res[[i]])
        x[nas[, i]] <- ""
        x
    })
    print.data.frame(res)
    invisible(x)
}

family.negbin <- function(object, ...) object$family

glm.convert <- function(object)
{
    object$call[[1L]] <- quote(stats::glm)
    if(is.null(object$link))
        object$link <- as.name("log")
    object$call$family <- call("negative.binomial", theta = object$theta,
                               link = object$link)
    object$call$init.theta <- object$call$link <- NULL
    class(object) <- c("glm", "lm")
    object
}

glm.nb <- function(formula, data, weights,
		   subset, na.action, start = NULL, etastart, mustart,
		   control = glm.control(...), method = "glm.fit",
		   model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ...,
		   init.theta, link = log)
{
    loglik <- function(n, th, mu, y, w)
        sum(w*(lgamma(th + y) - lgamma(th) - lgamma(y + 1) + th * log(th) +
               y * log(mu + (y == 0)) - (th + y) * log(th + mu)))

    link <- substitute(link)
    fam0 <- if(missing(init.theta))
        do.call("poisson", list(link = link))
    else
        do.call("negative.binomial", list(theta = init.theta, link = link))

    dots <- list(...)
    mf <- Call <- match.call()
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "etastart", "mustart", "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    Terms <- attr(mf, "terms")
    if(method == "model.frame") return(mf)
    Y <- model.response(mf, "numeric")
    ## null model support
    X <- if (!is.empty.model(Terms)) model.matrix(Terms, mf, contrasts) else matrix(,NROW(Y),0)
    w <- model.weights(mf)
    if(!length(w)) w <- rep(1, nrow(mf))
    else if(any(w < 0)) stop("negative weights not allowed")
    offset <- model.offset(mf)
    ## these allow starting values to be expressed in terms of other vars.
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")
    n <- length(Y)
    if(!missing(method)) {
        if(!exists(method, mode = "function"))
            stop(gettextf("unimplemented method: %s", sQuote(method)),
                 domain = NA)
        glm.fitter <- get(method)
    } else {
        method <- "glm.fit"
        glm.fitter <- stats::glm.fit
    }
    if(control$trace > 1) message("Initial fit:")
    fit <- glm.fitter(x = X, y = Y, w = w, start = start,
                      etastart = etastart, mustart = mustart,
                      offset = offset, family = fam0,
                      control = list(maxit=control$maxit,
                      epsilon = control$epsilon,
                      trace = control$trace > 1),
                      intercept = attr(Terms, "intercept") > 0)
    class(fit) <- c("glm", "lm")
    mu <- fit$fitted.values
    th <- as.vector(theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace =
                             control$trace> 2)) # drop attributes
    if(control$trace > 1)
        message(gettextf("Initial value for 'theta': %f", signif(th)),
                domain = NA)
    fam <- do.call("negative.binomial", list(theta = th, link = link))
    iter <- 0
    d1 <- sqrt(2 * max(1, fit$df.residual))
    d2 <- del <- 1
    g <- fam$linkfun
    Lm <- loglik(n, th, mu, Y, w)
    Lm0 <- Lm + 2 * d1
    while((iter <- iter + 1) <= control$maxit &&
          (abs(Lm0 - Lm)/d1 + abs(del)/d2) > control$epsilon) {
        eta <- g(mu)
        fit <- glm.fitter(x = X, y = Y, w = w, etastart =
                          eta, offset = offset, family = fam,
                          control = list(maxit=control$maxit,
                          epsilon = control$epsilon,
                          trace = control$trace > 1),
                          intercept = attr(Terms, "intercept") > 0)
        t0 <- th
        th <- theta.ml(Y, mu, sum(w), w, limit=control$maxit,
                       trace = control$trace > 2)
        fam <- do.call("negative.binomial", list(theta = th, link = link))
        mu <- fit$fitted.values
        del <- t0 - th
        Lm0 <- Lm
        Lm <- loglik(n, th, mu, Y, w)
        if(control$trace) {
            Ls <- loglik(n, th, Y, Y, w)
            Dev <- 2 * (Ls - Lm)
            message(sprintf("Theta(%d) = %f, 2(Ls - Lm) = %f",
                            iter, signif(th), signif(Dev)), domain = NA)
        }
    }
    if(!is.null(attr(th, "warn"))) fit$th.warn <- attr(th, "warn")
    if(iter > control$maxit) {
        warning("alternation limit reached")
        fit$th.warn <- gettext("alternation limit reached")
    }

  # If an offset and intercept are present, iterations are needed to
  # compute the Null deviance; these are done here, unless the model
  # is NULL, in which case the computations have been done already
  #
    if(length(offset) && attr(Terms, "intercept")) {
        null.deviance <-
            if(length(Terms))
                glm.fitter(X[, "(Intercept)", drop = FALSE], Y, w,
                           offset = offset, family = fam,
                           control = list(maxit=control$maxit,
                           epsilon = control$epsilon,
                           trace = control$trace > 1),
                           intercept = TRUE
                           )$deviance
           else fit$deviance
        fit$null.deviance <- null.deviance
    }
    class(fit) <- c("negbin", "glm", "lm")
    fit$terms <- Terms
    fit$formula <- as.vector(attr(Terms, "formula"))
    ## make result somewhat reproducible
    Call$init.theta <- signif(as.vector(th), 10)
    Call$link <- link
    fit$call <- Call
    if(model) fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if(x) fit$x <- X
    if(!y) fit$y <- NULL
    fit$theta <- as.vector(th)
    fit$SE.theta <- attr(th, "SE")
    fit$twologlik <- as.vector(2 * Lm)
    fit$aic <- -fit$twologlik + 2*fit$rank + 2
    fit$contrasts <- attr(X, "contrasts")
    fit$xlevels <- .getXlevels(Terms, mf)
    fit$method <- method
    fit$control <- control
    fit$offset <- offset
    fit
}

negative.binomial <-
    function(theta = stop("'theta' must be specified"), link = "log")
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) linktemp <- deparse(linktemp)
    if (linktemp %in% c("log", "identity", "sqrt"))
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    } else {
        ## what else shall we allow?  At least objects of class link-glm.
        if(inherits(link, "link-glm")) {
            stats <- link
            if(!is.null(stats$name)) linktemp <- stats$name
        } else
            stop(gettextf("\"%s\" link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"", linktemp))
    }
    .Theta <- theta ## avoid codetools warnings
    env <- new.env(parent=.GlobalEnv)
    assign(".Theta", theta, envir=env)
    variance <- function(mu)
        mu + mu^2/.Theta
    validmu <- function(mu)
        all(mu > 0)
    dev.resids <- function(y, mu, wt)
        2 * wt * (y * log(pmax(1, y)/mu) - (y + .Theta) *
                  log((y + .Theta)/ (mu + .Theta)))
    aic <- function(y, n, mu, wt, dev) {
        term <- (y + .Theta) * log(mu + .Theta) - y * log(mu) +
            lgamma(y + 1) - .Theta * log(.Theta) + lgamma(.Theta) - lgamma(.Theta+y)
        2 * sum(term * wt)
    }
    initialize <- expression({
        if (any(y < 0))
            stop("negative values not allowed for the negative binomial family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)/6
    })
    simfun <- function(object, nsim) {
        ftd <- fitted(object)
        val <- rnegbin(nsim * length(ftd), ftd, .Theta)
    }
    environment(variance) <- environment(validmu) <-
        environment(dev.resids) <- environment(aic) <-
            environment(simfun) <- env
    famname <- paste("Negative Binomial(", format(round(theta, 4)), ")",
                     sep = "")
    structure(list(family = famname, link = linktemp, linkfun = stats$linkfun,
                   linkinv = stats$linkinv, variance = variance,
                   dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta,
                   initialize = initialize, validmu = validmu,
                   valideta = stats$valideta, simulate = simfun),
              class = "family")
}

rnegbin <- function(n, mu = n, theta = stop("'theta' must be specified"))
{
    k <- if(length(n) > 1L) length(n) else n
    rpois(k, (mu * rgamma(k, theta))/theta)
}

summary.negbin <- function(object, dispersion = 1, correlation = FALSE, ...)
{
    if(is.null(dispersion)) dispersion <- 1
    summ <- c(summary.glm(object, dispersion = dispersion,
                          correlation = correlation),
              object[c("theta", "SE.theta", "twologlik", "th.warn")])
    class(summ) <- c("summary.negbin", "summary.glm")
    summ
}

print.summary.negbin <- function(x, ...)
{
    NextMethod()
    dp <- max(2 - floor(log10(x$SE.theta)), 0)
    cat("\n              Theta: ", format(round(x$theta, dp), nsmall=dp),
        "\n          Std. Err.: ", format(round(x$SE.theta, dp), nsmall=dp),
        "\n")
    if(!is.null(x$th.warn))
        cat("Warning while fitting theta:", x$th.warn,"\n")
    cat("\n 2 x log-likelihood: ", format(round(x$twologlik, 3), nsmall=dp), "\n")
    invisible(x)
}

theta.md <-
    function(y, mu, dfr, weights, limit = 20, eps = .Machine$double.eps^0.25)
{
    if(inherits(y, "lm")) {
        mu <- y$fitted.values
        dfr <- y$df.residual
        y <- if(is.null(y$y)) mu + residuals(y) else y$y
    }
    if(missing(weights)) weights <- rep(1, length(y))
    n <- sum(weights)
    t0 <- n/sum(weights*(y/mu - 1)^2)
    a <- 2 * sum(weights*y * log(pmax(1, y)/mu)) - dfr
    it <- 0
    del <- 1
    while((it <- it + 1) < limit && abs(del) > eps) {
        t0 <- abs(t0)
        tmp <- log((y + t0)/(mu + t0))
        top <- a - 2 * sum(weights*(y + t0) * tmp)
        bot <- 2 * sum(weights*((y - mu)/(mu + t0) - tmp))
        del <- top/bot
        t0 <- t0 - del
    }
    if(t0 < 0) {
        t0 <- 0
        warning("estimate truncated at zero")
        attr(t0, "warn") <- gettext("estimate truncated at zero")
    }
    t0
}

theta.ml <-
    function(y, mu, n = sum(weights), weights, limit = 10,
             eps = .Machine$double.eps^0.25,
             trace = FALSE)
{
    score <- function(n, th, mu, y, w)
        sum(w*(digamma(th + y) - digamma(th) + log(th) +
               1 - log(th + mu) - (y + th)/(mu + th)))
    info <- function(n, th, mu, y, w)
        sum(w*( - trigamma(th + y) + trigamma(th) - 1/th +
               2/(mu + th) - (y + th)/(mu + th)^2))
    if(inherits(y, "lm")) {
        mu <- y$fitted.values
        y <- if(is.null(y$y)) mu + residuals(y) else y$y
    }
    if(missing(weights)) weights <- rep(1, length(y))
    t0 <- n/sum(weights*(y/mu - 1)^2)
    it <- 0
    del <- 1
    if(trace) message(sprintf("theta.ml: iter %d 'theta = %f'",
                              it, signif(t0)), domain = NA)
    while((it <- it + 1) < limit && abs(del) > eps) {
        t0 <- abs(t0)
        del <- score(n, t0, mu, y, weights)/(i <- info(n, t0, mu, y, weights))
        t0 <- t0 + del
        if(trace) message("theta.ml: iter", it," theta =", signif(t0))
    }
    if(t0 < 0) {
        t0 <- 0
        warning("estimate truncated at zero")
        attr(t0, "warn") <- gettext("estimate truncated at zero")
    }
    if(it == limit) {
        warning("iteration limit reached")
        attr(t0, "warn") <- gettext("iteration limit reached")
    }
    attr(t0, "SE") <- sqrt(1/i)
    t0
}

theta.mm <- function(y, mu, dfr, weights, limit = 10,
                     eps = .Machine$double.eps^0.25)
{
    if(inherits(y, "lm")) {
        mu <- y$fitted.values
        dfr <- y$df.residual
        y <- if(is.null(y$y)) mu + residuals(y) else y$y
    }
    if(missing(weights)) weights <- rep(1, length(y))
    n <- sum(weights)
    t0 <- n/sum(weights*(y/mu - 1)^2)
    it <- 0
    del <- 1
    while((it <- it + 1) < limit && abs(del) > eps) {
        t0 <- abs(t0)
        del <- (sum(weights*((y - mu)^2/(mu + mu^2/t0))) - dfr)/
            sum(weights*(y - mu)^2/(mu + t0)^2)
        t0 <- t0 - del
    }
    if(t0 < 0) {
        t0 <- 0
        warning("estimate truncated at zero")
        attr(t0, "warn") <- gettext("estimate truncated at zero")
    }
    t0
}

logLik.negbin <- function(object, ...)
{
    if (nargs() > 1L) warning("extra arguments discarded")
    p <- object$rank + 1L # for theta
    val <- object$twologlik/2
    attr(val, "df") <- p
    attr(val, "nobs") <- sum(!is.na(object$residuals)) # as for glm
    class(val) <- "logLik"
    val
}

vcov.negbin <- function(object, ...)
    with(summary.negbin(object, ...), dispersion * cov.unscaled)

## based on simulate.lm
simulate.negbin <- function(object, nsim = 1, seed = NULL, ...)
{
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
    ftd <- fitted(object)
    nm <- names(ftd)
    n <- length(ftd)
    val <- rnegbin(n * nsim, ftd, object$theta)
    dim(val) <- c(n, nsim)
    val <- as.data.frame(val)
    names(val) <- paste("sim", seq_len(nsim), sep="_")
    if (!is.null(nm)) row.names(val) <- nm
    attr(val, "seed") <- RNGstate
    val
}
