# file MASS/R/neg.bin.R
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
neg.bin <- function(theta = stop("'theta' must be given"))
{
    ## use this to avoid capturing the MASS namespace
    .Theta <- theta ## avoid codetools warnings
    env <- new.env(parent=.GlobalEnv)
    assign(".Theta", theta, envir=env)
    stats <- make.link("log")
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
            stop("negative values not allowed for the Negative Binomal family")
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
    structure(list(family = "Negative Binomial",
                   link = "log",
                   linkfun = stats$linkfun,
                   linkinv = stats$linkinv,
                   variance = variance,
                   dev.resids = dev.resids,
                   aic = aic,
                   mu.eta = stats$mu.eta,
                   initialize = initialize,
                   validmu = validmu,
                   valideta = stats$valideta,
                   simulate = simfun),
              class = "family")
}
