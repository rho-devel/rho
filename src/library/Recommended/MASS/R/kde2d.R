# file MASS/R/kde2d.R
# copyright (C) 1994-2012 W. N. Venables and B. D. Ripley
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
kde2d <- function(x, y, h, n = 25, lims = c(range(x), range(y)) )
{
    nx <- length(x)
    if(length(y) != nx)
        stop("data vectors must be the same length")
    if(any(!is.finite(x)) || any(!is.finite(y)))
	stop("missing or infinite values in the data are not allowed")
    if(any(!is.finite(lims)))
	stop("only finite values are allowed in 'lims'")
    n <- rep(n, length.out = 2L)
    gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
    gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
    h <- if (missing(h)) c(bandwidth.nrd(x), bandwidth.nrd(y))
    else rep(h, length.out = 2L)
    if (any(h <= 0))
        stop("bandwidths must be strictly positive")
    h <- h/4                            # for S's bandwidth scale
    ax <- outer(gx, x, "-" )/h[1L]
    ay <- outer(gy, y, "-" )/h[2L]
    z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), , nx))/ (nx * h[1L] * h[2L])
    list(x = gx, y = gy, z = z)
}
