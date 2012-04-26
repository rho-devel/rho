## Contributed by Mary Lindstrom <lindstro@biostat.wisc.edu>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

getVarCov <- function(obj, ...) UseMethod("getVarCov")

getVarCov.lme <-
    function(obj,
             individuals,
             type= c("random.effects","conditional","marginal"), ...)
{
    type  <-  match.arg(type)
    if(any("nlme" == class(obj)))
        stop("Not implemented for nlme objects")
    if(length(obj$group) > 1)
        stop("Not implemented for multiple levels of nesting")
    sigma <- obj$sigma
    D <- as.matrix(obj$modelStruct$reStruct[[1]]) * sigma^2
    if (type=="random.effects")
    {
        result  <-  D
    }
    else
    {
        result <- list()
        groups  <-  obj$groups[[1]]
        ugroups  <-  unique(groups)
        if (missing(individuals)) individuals  <-  as.matrix(ugroups)[1,]
        if (is.numeric(individuals))
            individuals  <-  ugroups[individuals]
        for (individ in individuals)
        {
            indx <- (1:length(ugroups))[individ==ugroups]
            if (!length(indx))
                stop(paste("individual",individ,"was not used in the fit."))
            if (is.na(indx))
                stop(paste("individual",individ,"was not used in the fit."))
            ind <- groups == individ
            if(!is.null(obj$modelStruct$corStruct)) {
                V <- corMatrix(obj$modelStruct$corStruct)[[as.character(individ)]]
            }
            else V <- diag(sum(ind))
            if(!is.null(obj$modelStruct$varStruct))
                sds <- 1/varWeights(obj$modelStruct$varStruct)[ind]
            else
                sds <- rep(1, sum(ind))
            sds <- obj$sigma * sds
            cond.var <- t(V * sds) * sds
            dimnames(cond.var)  <-  list(1:nrow(cond.var),1:ncol(cond.var))
            if (type=="conditional")
                result[[as.character(individ)]] <- cond.var
            else
            {
                Z <- model.matrix(obj$modelStruct$reStruc,
                                  getData(obj))[ind, , drop = FALSE]
                result[[as.character(individ)]] <-
                    cond.var + Z %*% D %*% t(Z)
            }
        }
    }
    class(result)  <-  c(type,"VarCov")
    attr(result,"group.levels")  <-  names(obj$groups)
    result
}

getVarCov.gls <-
    function(obj, individual = 1, ...)
{
    S <- corMatrix(obj$modelStruct$corStruct)[[individual]]
    if (!is.null( obj$modelStruct$varStruct))
    {
        ind  <-  obj$groups==individual
        vw  <-  1/varWeights(obj$modelStruct$varStruct)[ind]
    }
    else vw  <-  rep(1,nrow(S))
    vars  <-  (obj$sigma * vw)^2
    result  <-  t(S * sqrt(vars))*sqrt(vars)
    class(result)  <-  c("marginal","VarCov")
    attr(result,"group.levels")  <-  names(obj$groups)
    result
}

print.VarCov <-
    function(x, corr = FALSE, stdevs = TRUE, digits = 5, ...)
{
    pvc  <-  function(x, type, corr, stdevs, digits) {
        cat(c("Random effects","Conditional",
              "Marginal")[match(type,
                                c("random.effects","conditional",
                                  "marginal"))], " ", sep = "")
        x  <-  as.matrix(x)
        class(x)  <-  NULL
        attr(x,"group.levels")  <-  NULL
        if (corr)
        {
            cat("correlation matrix\n")
            sds <- sqrt(diag(x))
            print(signif(t(x/sds)/sds,digits))
        }
        else
        {
            cat("variance covariance matrix\n")
            print(signif(x,digits))
            if(stdevs)
                sds <- sqrt(diag(x))
        }
        if (stdevs) cat("  Standard Deviations:",signif(sds,digits),"\n")
    }
    if (!is.list(x))
        pvc(x,class(x)[1],corr,stdevs,digits)
    else
    {
        for (nm in names(x))
        {
            cat(attr(x,"group.levels"),nm,"\n")
            pvc(x[[nm]],class(x)[1],corr,stdevs,digits)
        }
    }
    invisible(x)
}

