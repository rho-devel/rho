# file nnet/nnet.R
# copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
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
nnet <- function(x, ...) UseMethod("nnet")

nnet.formula <- function(formula, data, weights, ...,
                         subset, na.action, contrasts=NULL)
{
    class.ind <- function(cl)
    {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval.parent(m$data)))
        m$data <- as.data.frame(data)
    m$... <- m$contrasts <- NULL
    m[[1L]] <- as.name("model.frame")
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    cons <- attr(x, "contrast")
    xint <- match("(Intercept)", colnames(x), nomatch=0L)
    if(xint > 0L) x <- x[, -xint, drop=FALSE] # Bias term is used for intercepts
    w <- model.weights(m)
    if(length(w) == 0L) w <- rep(1, nrow(x))
    y <- model.response(m)
    if(is.factor(y)) {
        lev <- levels(y)
        counts <- table(y)
        if(any(counts == 0L)) {
            empty <- lev[counts == 0L]
            warning(sprintf(ngettext(length(empty),
                                     "group %s is empty",
                                     "groups %s are empty"),
                            paste(sQuote(empty), collapse=" ")), domain = NA)
            y <- factor(y, levels=lev[counts > 0L])
        }
        if(length(lev) == 2L) {
            y <- as.vector(unclass(y)) - 1
            res <- nnet.default(x, y, w, entropy=TRUE, ...)
            res$lev <- lev
        } else {
            y <- class.ind(y)
            res <- nnet.default(x, y, w, softmax=TRUE, ...)
            res$lev <- lev
        }
    } else res <- nnet.default(x, y, w, ...)
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$call <- match.call()
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("nnet.formula", "nnet")
    res
}

nnet.default <-
function(x, y, weights, size, Wts, mask=rep(TRUE, length(wts)),
	 linout=FALSE, entropy=FALSE, softmax=FALSE, censored=FALSE, skip=FALSE,
	 rang=0.7, decay=0, maxit=100, Hess=FALSE, trace=TRUE,
         MaxNWts=1000, abstol=1.0e-4, reltol=1.0e-8, ...)
{
    net <- NULL
    x <- as.matrix(x)
    y <- as.matrix(y)
    if(any(is.na(x))) stop("missing values in 'x'")
    if(any(is.na(y))) stop("missing values in 'y'")
    if(dim(x)[1L] != dim(y)[1L]) stop("nrows of 'x' and 'y' must match")
    if(linout && entropy) stop("entropy fit only for logistic units")
    if(softmax) {
        linout <- TRUE
        entropy <- FALSE
    }
    if(censored) {
        linout <- TRUE
        entropy <- FALSE
        softmax <- TRUE
    }
    net$n <- c(dim(x)[2L], size, dim(y)[2L])
    net$nunits <- as.integer(1L + sum(net$n))
    net$nconn <- rep(0, net$nunits+1L)
    net$conn <- numeric(0L)
    net <- norm.net(net)
    if(skip) net <- add.net(net, seq(1L,net$n[1L]),
                            seq(1L+net$n[1L]+net$n[2L], net$nunits-1L))
    if((nwts <- length(net$conn))==0) stop("no weights to fit")
    if(nwts > MaxNWts)
        stop(gettextf("too many (%d) weights", nwts), domain=NA)
    nsunits <- net$nunits
    if(linout) nsunits <- net$nunits - net$n[3L]
    net$nsunits <- nsunits
    net$decay <- decay
    net$entropy <- entropy
    if(softmax && NCOL(y) < 2L)
        stop("'softmax = TRUE' requires at least two response categories")
    net$softmax <- softmax
    net$censored <- censored
    if(missing(Wts))
        if(rang > 0) wts <- runif(nwts, -rang, rang)
        else wts <- rep(0, nwts)
    else wts <- Wts
    if(length(wts) != nwts) stop("weights vector of incorrect length")
    if(length(mask) != length(wts)) stop("incorrect length of 'mask'")
    if(trace) {
        cat("# weights: ", length(wts)) #
        nw <- sum(mask != 0)
        if(nw < length(wts)) cat(" (", nw, " variable)\n",sep="")
        else cat("\n")
        flush.console()
    }
    if(length(decay) == 1L) decay <- rep(decay, length(wts))
    .C(VR_set_net,
       as.integer(net$n),
       as.integer(net$nconn),
       as.integer(net$conn),
       as.double(decay),
       as.integer(nsunits),
       as.integer(entropy),
       as.integer(softmax),
       as.integer(censored)
       )
    ntr <- dim(x)[1L]
    nout <- dim(y)[2L]
    if(missing(weights)) weights <- rep(1, ntr)
    if(length(weights) != ntr || any(weights < 0))
        stop("invalid weights vector")
    Z <- as.double(cbind(x,y))
    storage.mode(weights) <- "double"
    tmp <- .C(VR_dovm,
              as.integer(ntr), Z, weights,
              as.integer(length(wts)),
              wts=as.double(wts),
              val=double(1),
              as.integer(maxit),
              as.logical(trace),
              as.integer(mask),
              as.double(abstol), as.double(reltol),
              ifail = integer(1L)
              )
    net$value <- tmp$val
    net$wts <- tmp$wts
    net$convergence <- tmp$ifail
    tmp <- matrix(.C(VR_nntest,
                     as.integer(ntr), Z, tclass = double(ntr*nout),
                     as.double(net$wts))$tclass,  ntr, nout)
    dimnames(tmp) <- list(rownames(x), colnames(y))
    net$fitted.values <- tmp
    tmp <- y - tmp
    dimnames(tmp) <- list(rownames(x), colnames(y))
    net$residuals <- tmp
    .C(VR_unset_net)
    if(entropy) net$lev <- c("0","1")
    if(softmax) net$lev <- colnames(y)
    net$call <- match.call()
    if(Hess) net$Hessian <- nnetHess(net, x, y, weights)
    class(net) <- "nnet"
    net
}


predict.nnet <- function(object, newdata, type=c("raw","class"), ...)
{
    if(!inherits(object, "nnet")) stop("object not of class \"nnet\"")
    type <- match.arg(type)
    if(missing(newdata)) z <- fitted(object)
    else {
        if(inherits(object, "nnet.formula")) { #
            ## formula fit
            newdata <- as.data.frame(newdata)
            rn <- row.names(newdata)
            ## work hard to predict NA for rows with missing data
            Terms <- delete.response(object$terms)
            m <- model.frame(Terms, newdata, na.action = na.omit,
                             xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses")))
                .checkMFClasses(cl, m)
            keep <- match(row.names(m), rn)
            x <- model.matrix(Terms, m, contrasts = object$contrasts)
            xint <- match("(Intercept)", colnames(x), nomatch=0L)
            if(xint > 0L) x <- x[, -xint, drop=FALSE] # Bias term is used for intercepts
        } else {
            ## matrix ...  fit
            if(is.null(dim(newdata)))
                dim(newdata) <- c(1L, length(newdata)) # a row vector
            x <- as.matrix(newdata)     # to cope with dataframes
            if(any(is.na(x))) stop("missing values in 'x'")
            keep <- 1L:nrow(x)
            rn <- rownames(x)
        }
        ntr <- nrow(x)
        nout <- object$n[3L]
        .C(VR_set_net,
           as.integer(object$n), as.integer(object$nconn),
           as.integer(object$conn), rep(0.0, length(object$wts)),
           as.integer(object$nsunits), as.integer(0L),
           as.integer(object$softmax), as.integer(object$censored))
        z <- matrix(NA, nrow(newdata), nout,
                    dimnames = list(rn, dimnames(object$fitted.values)[[2L]]))
        z[keep, ] <- matrix(.C(VR_nntest,
                               as.integer(ntr),
                               as.double(x),
                               tclass = double(ntr*nout),
                               as.double(object$wts)
                               )$tclass, ntr, nout)
        .C(VR_unset_net)
    }
    switch(type, raw = z,
           class = {
               if(is.null(object$lev)) stop("inappropriate fit for class")
               if(ncol(z) > 1L) object$lev[max.col(z)]
               else object$lev[1L + (z > 0.5)]
           })
}

eval.nn <- function(wts)
{
    z <- .C(VR_dfunc,
            as.double(wts), df = double(length(wts)), fp = as.double(1))
    fp <- z$fp
    attr(fp, "gradient") <- z$df
    fp
}

add.net <- function(net, from, to)
{
    nconn <- net$nconn
    conn <- net$conn
    for(i in to){
        ns <- nconn[i+2L]
        cadd <- from
        if(nconn[i+1] == ns) cadd <- c(0,from)
        con <- NULL
        if(ns > 1L) con <- conn[1L:ns]
        con <- c(con, cadd)
        if(length(conn) > ns) con <- c(con, conn[(ns+1L):length(conn)])
        for(j in (i+1L):net$nunits) nconn[j+1L] <- nconn[j+1L]+length(cadd)
        conn <- con
    }
    net$nconn <- nconn
    net$conn <- con
    net
}

norm.net <- function(net)
{
    n <- net$n; n0 <- n[1L]; n1 <- n0+n[2L]; n2 <- n1+n[3L];
    if(n[2L] <= 0) return(net)
    net <- add.net(net, 1L:n0,(n0+1L):n1)
    add.net(net, (n0+1L):n1, (n1+1L):n2)
}

which.is.max <- function(x)
{
    y <- seq_along(x)[x == max(x)]
    if(length(y) > 1L) sample(y, 1L) else y
}

nnetHess <- function(net, x, y, weights)
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if(dim(x)[1L] != dim(y)[1L]) stop("dims of 'x' and 'y' must match")
    nw <- length(net$wts)
    decay <- net$decay
    if(length(decay) == 1) decay <- rep(decay, nw)
    .C(VR_set_net,
       as.integer(net$n),
       as.integer(net$nconn),
       as.integer(net$conn),
       as.double(decay),
       as.integer(net$nsunits),
       as.integer(net$entropy),
       as.integer(net$softmax),
       as.integer(net$censored)
       )
    ntr <- dim(x)[1L]
    if(missing(weights)) weights <- rep(1, ntr)
    if(length(weights) != ntr || any(weights < 0))
        stop("invalid weights vector")
    Z <- as.double(cbind(x,y))
    storage.mode(weights) <- "double"
    z <- matrix(.C(VR_nnHessian, as.integer(ntr), Z, weights,
                   as.double(net$wts), H = double(nw*nw))$H,
                nw, nw)
    .C(VR_unset_net)
    z
}

class.ind <- function(cl)
{
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)) )
    x[(1L:n) + n*(unclass(cl)-1L)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}

print.nnet <- function(x, ...)
{
    if(!inherits(x, "nnet")) stop("not a legitimate neural net fit")
    cat("a ",x$n[1L],"-",x$n[2L],"-",x$n[3L]," network", sep="")
    cat(" with", length(x$wts),"weights\n")
    if(length(x$coefnames))  cat("inputs:", x$coefnames, "\noutput(s):",
                                 deparse(formula(x)[[2L]], backtick=TRUE), "\n")
    cat("options were -")
    tconn <- diff(x$nconn)
    if(tconn[length(tconn)] > x$n[2L]+1L) cat(" skip-layer connections ")
    if(x$nunits > x$nsunits && !x$softmax) cat(" linear output units ")
    if(x$entropy) cat(" entropy fitting ")
    if(x$softmax) cat(" softmax modelling ")
    if(x$decay[1L] > 0) cat(" decay=", x$decay[1L], sep="")
    cat("\n")
    invisible(x)
}

coef.nnet <- function(object, ...)
{
    wts <- object$wts
    wm <- c("b", paste("i", seq_len(object$n[1L]), sep=""))
    if(object$n[2L] > 0L)
        wm <- c(wm, paste("h", seq_len(object$n[2L]), sep=""))
    if(object$n[3L] > 1L)  wm <- c(wm,
                                  paste("o", seq_len(object$n[3L]), sep=""))
    else wm <- c(wm, "o")
    names(wts) <- apply(cbind(wm[1+object$conn],
                              wm[1L+rep(1L:object$nunits - 1L,
                                        diff(object$nconn))]),
                        1L, function(x)  paste(x, collapse = "->"))
    wts
}

summary.nnet <- function(object, ...)
{
    class(object) <- c("summary.nnet", class(object))
    object
}


print.summary.nnet <- function(x, ...)
{
    cat("a ",x$n[1L],"-",x$n[2L],"-",x$n[3L]," network", sep="")
    cat(" with", length(x$wts),"weights\n")
    cat("options were -")
    tconn <- diff(x$nconn)
    if(tconn[length(tconn)] > x$n[2L]+1L) cat(" skip-layer connections ")
    if(x$nunits > x$nsunits && !x$softmax) cat(" linear output units ")
    if(x$entropy) cat(" entropy fitting ")
    if(x$softmax) cat(" softmax modelling ")
    if(x$decay[1L] > 0) cat(" decay=", x$decay[1L], sep="")
    cat("\n")
    wts <- format(round(coef.nnet(x),2))
    lapply(split(wts, rep(1L:x$nunits, tconn)),
           function(x) print(x, quote=FALSE))
    invisible(x)
}

# residuals.nnet <- function(object, ...) object$residuals
