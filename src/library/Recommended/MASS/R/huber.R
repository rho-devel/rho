# file MASS/R/huber.R
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
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
huber <- function(y, k=1.5, tol = 1.0e-6)
{
    y <- y[!is.na(y)]
    n <- length(y)
    mu <- median(y)
    s <- mad(y)
    if(s == 0) stop("cannot estimate scale: MAD is zero for this sample")
    repeat{
        yy <- pmin(pmax(mu-k*s,y),mu+k*s)
        mu1 <- sum(yy)/n
        if(abs(mu-mu1) < tol*s) break
        mu <- mu1
    }
    list(mu=mu,s=s)
}
