# file MASS/R/hubers.R
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
hubers <- function(y, k = 1.5, mu, s, initmu = median(y), tol = 1.0e-6)
{
    mmu <- missing(mu); ms <- missing(s)
    y <- y[!is.na(y)]
    n <- length(y)
    if(mmu) {
        mu0 <- initmu
        n1 <- n - 1
    } else {
        mu0 <- mu
        mu1 <- mu
        n1 <- n
    }
    if(ms) { # middle 50% of data is constant
	s0 <- mad(y)
	if(s0 == 0.0) return(list(mu=mu0, s = 0))
    } else {s0 <- s;s1 <- s}
    th <- 2*pnorm(k)-1
    beta <- th + k^2*(1-th) - 2*k*dnorm(k)
    for(i in 1:30) { # avoid infinite loop
        yy <- pmin(pmax(mu0-k*s0,y), mu0+k*s0)
        if(mmu) mu1 <- sum(yy)/n
        if(ms) {
            ss <- sum((yy-mu1)^2)/n1
            s1 <- sqrt(ss/beta)
        }
        if((abs(mu0-mu1) < tol*s0) && abs(s0-s1) < tol*s0) break
        mu0 <- mu1; s0 <- s1
    }
    list(mu=mu0, s=s0)
}
