# file MASS/R/confint.R
# copyright (C) 1994-2006 W. N. Venables and B. D. Ripley
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

confint.glm <- function(object, parm, level = 0.95, trace = FALSE, ...)
{
    pnames <- names(coef(object))
    if(missing(parm)) parm <- seq_along(pnames)
    else if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
    message("Waiting for profiling to be done...")
    utils::flush.console()
    object <- profile(object, which = parm, alpha = (1. - level)/4.,
                      trace = trace)
    confint(object, parm=parm, level=level, trace=trace, ...)
}

confint.profile.glm <-
  function(object, parm = seq_along(pnames), level = 0.95, ...)
{
    of <- attr(object, "original.fit")
    pnames <- names(coef(of))
    if(is.character(parm))  parm <- match(parm, pnames, nomatch = 0L)
    a <- (1-level)/2
    a <- c(a, 1-a)
    pct <- paste(round(100*a, 1), "%")
    ci <- array(NA, dim = c(length(parm), 2L),
                dimnames = list(pnames[parm], pct))
    cutoff <- qnorm(a)
    for(pm in parm) {
        pro <- object[[ pnames[pm] ]]
        ## skip aliased params
        if(is.null(pro)) next
        if(length(pnames) > 1L)
            sp <- spline(x = pro[, "par.vals"][, pm], y = pro[, 1])
        else sp <- spline(x = pro[, "par.vals"], y = pro[, 1])
        ci[pnames[pm], ] <- approx(sp$y, sp$x, xout = cutoff)$y
    }
    drop(ci)
}

confint.nls <-
  function(object, parm, level = 0.95, ...)
{
  pnames <- names(coef(object))
  if(missing(parm)) parm <- seq_along(pnames)
  if(is.numeric(parm))  parm <- pnames[parm]
  message("Waiting for profiling to be done...")
  utils::flush.console()
  object <- profile(object, which = parm, alphamax = (1. - level)/4.)
  confint(object, parm=parm, level=level, ...)
}

confint.profile.nls <-
  function(object, parm = seq_along(pnames), level = 0.95, ...)
{
  pnames <- names(object) # non-linear pars only
  ncoefs <- length(coef(attr(object, "original.fit")))
  of <- attr(object, "original.fit")
  if(is.numeric(parm))  parm <- pnames[parm]
  ## drop any plinear paramaters
  parm <- parm[parm %in% pnames]
  n <- length(fitted(of)) - length(of$m$getPars())
  a <- (1-level)/2
  a <- c(a, 1-a)
  pct <- paste(round(100*a, 1), "%", sep = "")
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  cutoff <- qt(a, n)
  for(pm in parm) {
    pro <- object[[pm]]
    sp <- if(ncoefs > 1) spline(x = pro[, "par.vals"][, pm], y = pro$tau)
    else spline(x = pro[, "par.vals"], y = pro$tau)
    ci[pm, ] <- approx(sp$y, sp$x, xout = cutoff)$y
  }
  drop(ci)
}

