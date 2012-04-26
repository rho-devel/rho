# file MASS/R/gamma.shape.R
# copyright (C) 1994-2009 W. N. Venables and B. D. Ripley
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
gamma.shape <- function(object, ...) UseMethod("gamma.shape")

gamma.shape.glm <- function(object, it.lim = 10,
                            eps.max = .Machine$double.eps^0.25,
			    verbose = FALSE, ...)
{
    if(is.null(object$y)) object <- update(object, y = TRUE)
    y <- object$y
    A <- object$prior.weights
    if(is.null(A)) A <- rep(1, length(y))
    u <- object$fitted.values
    Dbar <- object$deviance/object$df.residual
    alpha <- (6 + 2*Dbar)/(Dbar*(6 + Dbar))
    if(verbose) {
	message("Initial estimate: ", format(alpha))
	utils::flush.console()
    }
    fixed <-  -y/u - log(u) + log(A) + 1 + log(y + (y == 0))
    eps <- 1
    itr <- 0
    while(abs(eps) > eps.max && (itr <- itr + 1) <= it.lim) {
        sc <- sum(A * (fixed + log(alpha) - digamma(A * alpha)))
        inf <- sum(A * (A * trigamma(A * alpha) - 1/alpha))
        alpha <- alpha + (eps <- sc/inf)
        if(verbose) {
	    message("Iter. ", itr, " Alpha: ", format(alpha))
	    utils::flush.console()
	}
    }
    if(itr > it.lim) warning("iteration limit reached")
    res <- list(alpha = alpha, SE = sqrt(1/inf))
    class(res) <- "gamma.shape"
    res
}

gamma.dispersion <- function(object, ...)
    1/gamma.shape(object, ...)[[1L]]

print.gamma.shape <- function(x, ...)
{
    y <- x
    x <- array(unlist(x), dim = 2L:1L,
               dimnames = list(c("Alpha:", "SE:"), ""))
    NextMethod("print")
    invisible(y)
}
