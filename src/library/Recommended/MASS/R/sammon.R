# file MASS/R/sammon.R
# copyright (C) 1994-2005 W. N. Venables and B. D. Ripley
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
sammon <- function(d, y = cmdscale(d, k), k = 2, niter = 100, trace = TRUE,
                   magic = 0.2, tol = 1e-4)
{
    call <- match.call()
    if(any(is.infinite(d))) stop("Infs not allowed in 'd'")
    if(any(is.na(d)) && missing(y))
        stop("an initial configuration must be supplied if there are NAs in 'd'")
    if(!is.matrix(y)) stop("'y' must be a matrix")

    if(is.null(n <- attr(d, "Size"))) {
        x <- as.matrix(d)
        if((n <- nrow(x)) != ncol(x))
            stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    } else {
        x <- matrix(0, n, n)
        x[row(x) > col(x)] <- d
        x <- x + t(x)
        rn <- attr(d, "Labels")
    }
    n <- as.integer(n)
    if(is.na(n)) stop("invalid size")
    ab <- x[row(x) < col(x)] <= 0
    if (any(ab, na.rm = TRUE)) {
        ab <- !is.na(ab) & ab
        aa <- cbind(as.vector(row(x)), as.vector(col(x)))[row(x) < col(x),]
        aa <- aa[ab, , drop=FALSE]
        stop(gettextf("zero or negative distance between objects %d and %d",
                      aa[1,1], aa[1,2]), domain = NA)
    }
    nas <- is.na(x)
    diag(nas) <- FALSE  # diag never used
    if(any(rowSums(!nas) < 2)) stop("not enough non-missing data")

    if(any(dim(y) != c(n, k)) ) stop("invalid initial configuration")
    if(any(!is.finite(y))) stop("initial configuration must be complete")
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    z <- .C(VR_sammon,
            x = x,
            n,
            as.integer(k),
            y = y,
            as.integer(niter),
            e = double(1),
            as.integer(trace),
            as.double(magic),
            as.double(tol),
            NAOK = TRUE)
    points <- z$y
    dimnames(points) <- list(rn, NULL)
    list(points=points, stress=z$e, call=call)
}
