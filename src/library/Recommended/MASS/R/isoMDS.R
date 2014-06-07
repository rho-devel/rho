# file MASS/R/R/isoMDS.R
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
isoMDS <- function(d, y = cmdscale(d, k), k = 2, maxit = 50, trace = TRUE,
                   tol = 1e-3, p = 2)
{
    if(any(!is.finite(d)) && missing(y))
        stop("an initial configuration must be supplied with NA/Infs in 'd'")
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

    dis <- x[row(x) > col(x)]
    ord <- order(dis)
    nd <- sum(!is.na(ord))

    on.exit(.C(VR_mds_unload))
    .C(VR_mds_init_data,
       as.integer(nd),
       as.integer(k),
       n,
       as.integer(ord - 1),
       as.integer(order(ord) - 1),
       as.double(y),
       as.double(p))
    tmp <- .C(VR_mds_dovm,
              val = double(1), as.integer(maxit), as.integer(trace),
              y = as.double(y), as.double(tol))
    points <- matrix(tmp$y,,k)
    dimnames(points) <- list(rn, NULL)
    list(points = points, stress = tmp$val)
}

Shepard <- function(d, x, p = 2)
{
#
# Given a dissimilarity d and configuration x, compute Shepard plot
#
  n <- as.integer(nrow(x))
  if (is.na(n)) stop("invalid row(x)")
  k <- ncol(x)
  y <- dist(x, method="minkowski", p = p)
  ord <- order(d)
  y <- y[ord]
  nd <- length(ord)
  if (is.na(nd)) stop("invalid length(d)")
  Z <- .C(VR_mds_fn,
	  as.double(y),
	  yf=as.double(y),
	  as.integer(nd),
	  ssq = double(1),
	  as.integer(order(ord)-1),
	  as.double(x),
	  as.integer(n),
	  as.integer(k),
	  g=double(n*k),
	  as.integer(1L),
          as.double(2.0))
  list(x = d[ord], y = y, yf = Z$yf)
}

