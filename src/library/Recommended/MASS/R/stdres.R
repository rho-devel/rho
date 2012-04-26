# file MASS/R/stdres.R
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
lmwork <- function(object)
{
    resid <- object$residuals
    hat <- lm.influence(object, do.coef = FALSE)$hat
    hat <- hat[hat > 0]
    ok <- !(is.na(resid))
    n.miss <- sum(!ok)
    switch(ifelse(n.miss > 2, 2, n.miss),
           warning("1 missing observation deleted"),
           warning(n.miss, " missing observations deleted")
           )
    resid <- resid[ok]
    n <- length(resid)
    p <- object$rank
    rdf <- object$df.residual
    if(is.null(rdf))
        rdf <- n - p
    if(!is.null(object$weights)) {
        wt <- object$weights[ok]
        resid <- resid * wt^0.5
        excl <- wt == 0
        if(any(excl)){
            warning(sum(excl), " rows with zero weights not counted")
            resid <- resid[!excl]
            if(is.null(object$df.resid))
                rdf <- rdf - sum(excl)
        }
    }
    stdres <- studres <- resid
    if(n > p) {
        stddev <- sqrt(sum(resid^2)/rdf)
        sr <- resid/(sqrt(1 - hat) * stddev)
        stdres <- sr
        studres <- sr/sqrt((n-p-sr^2)/(n-p-1))
        if(!is.null(object$na.action)) {
            stdres <- naresid(object$na.action, stdres)
            studres <- naresid(object$na.action, studres)
        }
    }
    else stddev <- stdres[] <- studres[]<- NA
    list(stddev=stddev, stdres=stdres, studres=studres)
}
stdres <- function(object) lmwork(object)$stdres
studres <- function(object) lmwork(object)$studres
