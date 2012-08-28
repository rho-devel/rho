#### Tools for Package Testing  --- in Matrix, sourced by ./test-tools.R
#### -------------------------
## to be used as, e.g.,
## source(system.file("test-tools-1.R", package="Matrix"), keep.source=FALSE)

### ------- Part I --  unrelated to "Matrix" classes ---------------

if(!exists("paste0", .BaseNamespaceEnv)) # have in R >= 2.15.0
    paste0 <- function(...) paste(..., sep = '')

identical3 <- function(x,y,z)	  identical(x,y) && identical (y,z)
identical4 <- function(a,b,c,d)   identical(a,b) && identical3(b,c,d)
identical5 <- function(a,b,c,d,e) identical(a,b) && identical4(b,c,d,e)

## Make sure errors are signaled
assertError <- function(expr) {
    d.expr <- deparse(substitute(expr))
    t.res <- tryCatch(expr, error = function(e) e)
    if(!inherits(t.res, "error"))
	stop(d.expr, "\n\t did not give an error", call. = FALSE)
    invisible(t.res)
}
assertWarningAtLeast <- function(expr) {
    d.expr <- deparse(substitute(expr))
    t.res <- tryCatch(expr, error = function(e)e, warning = function(w)w)
    if(!inherits(t.res, "error") && !inherits(t.res, "warning"))
	stop(d.expr, "\n\t did not give an error or warning", call. = FALSE)
    invisible(t.res)
}
assertWarning <- function(expr) {
    d.expr <- deparse(substitute(expr))
    t.res <- tryCatch(expr, warning = function(w)w)
    if(!inherits(t.res, "warning"))
	stop(d.expr, "\n\t did not give a warning", call. = FALSE)
    invisible(t.res)
}

isValid <- function(x, class) validObject(x, test=TRUE) && is(x, class)

is.all.equal3 <- function(x,y,z, tol = .Machine$double.eps^0.5)
    isTRUE(all.equal(x,y, tol=tol)) && isTRUE(all.equal(y,z, tol=tol))

is.all.equal4 <- function(x,y,z,u, tol = .Machine$double.eps^0.5)
    is.all.equal3(x,y,z, tol=tol) && isTRUE(all.equal(z,u, tol=tol))

## A version of all.equal() for the slots
all.slot.equal <- function(x,y, ...) {
    slts <- slotNames(x)
    for(sl in slts) {
        aeq <- all.equal(slot(x,sl), slot(y,sl), ...)
        if(!identical(TRUE, aeq))
            return(paste("slot '",sl,"': ", aeq, sep=''))
    }
    TRUE
}

## all.equal() for list-coercible objects -- apart from *some* components
all.equal.X <- function(x,y, except, ...)
{
    .trunc <- function(x) {
	ll <- as.list(x)
	ll[ - match(except, names(ll), nomatch = 0L)]
    }
    all.equal(.trunc(x), .trunc(y), ...)
}
## e.g. in lme4:
##  all.equal.X(env(m1), env(m2), except = c("call", "frame"))

## The relative error typically returned by all.equal:
relErr <- function(target, current) { ## make this work for 'Matrix'
    ## ==> no mean() ..
    n <- length(current)
    if(length(target) < n)
        target <- rep(target, length.out = n)
    sum(abs(target - current)) / sum(abs(target))
}

##' Compute the signed relative error between target and current vector -- vectorized
##' @title Relative Error (:= 0 when absolute error == 0)
##' @param target
##' @param current {maybe scalar; need  length(current) a multiple of length(target)}
##' @return *vector* of the same length as target and current
##' @author Martin Maechler
relErrV <- function(target, current) {
    n <- length(target <- as.vector(target))
    ## assert( <length current> is multiple of <length target>) :
    if(length(current) %% n)
	stop("length(current) must be a multiple of length(target)")
    RE <- current
    RE[] <- 0
    fr <- current/target
    neq <- is.na(current) | (current != target)
    RE[neq] <- 1 - fr[neq]
    RE
}

## is.R22 <- (paste(R.version$major, R.version$minor, sep=".") >= "2.2")

pkgRversion <- function(pkgname)
    sub("^R ([0-9.]+).*", "\\1", packageDescription(pkgname)[["Built"]])

showSys.time <- function(expr) {
    ## prepend 'Time' for R CMD Rdiff
    st <- system.time(expr)
    writeLines(paste("Time", capture.output(print(st))))
    invisible(st)
}
showProc.time <- local({ ## function + 'pct' variable
    pct <- proc.time()
    function(final="\n") { ## CPU elapsed __since last called__
	ot <- pct ; pct <<- proc.time()
	## 'Time ..' *not* to be translated:  tools::Rdiff() skips its lines!
	cat('Time elapsed: ', (pct - ot)[1:3], final)
    }
})

##' @title turn an S4 object (with slots) into a list with corresponding components
##' @param obj an R object with a formal class (aka "S4")
##' @return a list with named components where \code{obj} had slots
##' @author Martin Maechler
S4_2list <- function(obj) {
   sn <- slotNames(obj)
   structure(lapply(sn, slot, object = obj), .Names = sn)
}


### ------- Part II  -- related to matrices, but *not* "Matrix" -----------

add.simpleDimnames <- function(m) {
    stopifnot(length(d <- dim(m)) == 2)
    dimnames(m) <- list(if(d[1]) paste0("r", seq_len(d[1])),
                        if(d[2]) paste0("c", seq_len(d[2])))
    m
}

as.mat <- function(m) {
    ## as(., "matrix")	but with no extraneous empty dimnames
    m <- as(m, "matrix")
    if(identical(dimnames(m), list(NULL,NULL)))
	dimnames(m) <- NULL
    m
}

assert.EQ.mat <- function(M, m, tol = if(show) 0 else 1e-15, show=FALSE) {
    ## Purpose: check equality of  'Matrix' M with  'matrix' m
    ## ----------------------------------------------------------------------
    ## Arguments: M: is(., "Matrix") typically {but just needs working as(., "matrix")}
    ##            m: is(., "matrix")
    ##            show: if TRUE, return (and hence typically print) all.equal(...)
    validObject(M)
    MM <- as.mat(M)                     # as(M, "matrix")
    if(is.logical(MM) && is.numeric(m))
	storage.mode(MM) <- "integer"
    attr(MM, "dimnames") <- attr(m, "dimnames") <- NULL
    if(show) all.equal(MM, m, tol = tol)
    else if(!isTRUE(r <- all.equal(MM, m, tol = tol)))
	stop("all.equal() |->  ", r)
}
## a short cut
assert.EQ.Mat <- function(M, M2, tol = if(show) 0 else 1e-15, show=FALSE)
    assert.EQ.mat(M, as.mat(M2), tol=tol, show=show)


chk.matrix <- function(M) {
    ## check object; including coercion to "matrix" :
    cl <- class(M)
    cat("class ", dQuote(cl), " [",nrow(M)," x ",ncol(M),"]; slots (",
	paste(slotNames(M), collapse=","), ")\n", sep='')
    stopifnot(validObject(M),
	      dim(M) == c(nrow(M), ncol(M)),
	      identical(dim(m <- as(M, "matrix")), dim(M))
	      )
}

isOrthogonal <- function(x, tol = 1e-15) {
    all.equal(diag(as(zapsmall(crossprod(x)), "diagonalMatrix")),
              rep(1, ncol(x)), tol = tol)
}
