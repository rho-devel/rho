###              Classes of correlation structures
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
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

##*## Generics that should be implemented for any corStruct class

corFactor <-
  ## extractor for transpose inverse square root factor of corr matrix
  function(object, ...) UseMethod("corFactor")

corMatrix <-
  ## extractor for correlation matrix or the transpose inverse
  ## square root matrix
  function(object, ...) UseMethod("corMatrix")

###*# Constructor
### There is no constructor function for this class (i.e. no function
### called corStruct) because the class is virtual.

###*# Methods for local generics

corFactor.corStruct <-
  function(object, ...)
{
  if (!is.null(aux <- attr(object, "factor"))) {
    return(aux)
  }
  corD <- Dim(object)
  val <- .C(corStruct_factList,
	    as.double(unlist(corMatrix(object))),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
            logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corStruct <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (corr) {
    ## Do not know how to calculate the correlation matrix
    stop(paste("Don't know how to calculate correlation matrix of",
	       class(object)[1],"object"))
  } else {
    ## transpose inverse square root
    if (data.class(covariate) == "list") {
      if (is.null(names(covariate))) {
	names(covariate) <- 1:length(covariate)
      }
      corD <- Dim(object, rep(names(covariate),
			      unlist(lapply(covariate, length))))
    } else {
      corD <- Dim(object, rep(1, length(covariate)))
    }
    val <- .C(corStruct_factList,
	      as.double(unlist(corMatrix(object, covariate))),
	      as.integer(unlist(corD)),
	      factor = double(corD[["sumLenSq"]]),
	      logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
    if (corD[["M"]] > 1) {
      val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
      val <- lapply(val, function(el) {
        nel <- round(sqrt(length(el)))
        array(el, c(nel, nel))
      })
      names(val) <- names(corD[["len"]])
      val <- as.list(val)
    } else {
      val <- array(val, c(corD[["N"]], corD[["N"]]))
    }
    attr(val, "logDet") <- lD
    val
  }
}

###*# Methods for standard generics

as.matrix.corStruct <-
  function(x, ...) corMatrix(x)

coef.corStruct <-
  ## Accessor for constrained or unconstrained parameters of
  ## corStruct objects
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) {
    if (is.null(isFix <- attr(object, "fixed"))) {
      stop("corStruct object must have a \"fixed\" attribute.")
    }
    if (isFix) {
      numeric(0)
    } else {
      as.vector(object)
    }
  } else {
    stop(paste("Don't know how to obtain parameters of",
	       class(object)[1], "object"))
  }
}

"coef<-.corStruct" <-
  function(object, ..., value)
{
  ## Assignment of the unconstrained parameter of corStruct objects
  value <- as.numeric(value)
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  object[] <- value
  ## updating the factor list and logDet, by forcing a recalculation
  attr(object, "factor") <- NULL
  attr(object, "factor") <- corFactor(object)
  attr(object, "logDet") <- NULL
  attr(object, "logDet") <- logDet(object)
  object
}

Dim.corStruct <-
  function(object, groups, ...)
{
  if (missing(groups)) return(attr(object, "Dim"))
  ugrp <- unique(groups)
  groups <- factor(groups, levels = ugrp)
  len <- table(groups)
  list(N = length(groups),
       M = length(len),
       maxLen = max(len),
       sumLenSq = sum(len^2),
       len = len,
       start = match(ugrp, groups) - 1)
}

formula.corStruct <-
  ## Accessor for the covariate formula
  function(x, ...) eval(attr(x, "formula"))

getCovariate.corStruct <-
  function(object, form = formula(object), data)
{
  if (!missing(form)) {
    form <- formula(object)
    warning("Cannot change \"form\".")
  }
  if (is.null(covar <- attr(object, "covariate"))) { # need to calculate it
    if (missing(data)) {
      stop("Need data to calculate covariate of corStruct object")
    }
    covForm <- getCovariateFormula(form)
    if (!is.null(getGroupsFormula(form))) {
      grps <- getGroups(object, data = data)
    } else {
      grps <- NULL
    }
    if (length(all.vars(covForm)) > 0) { # primary covariate present
      if (is.null(grps)) {
        covar <- getCovariate(data, covForm)
      } else {
        if (all(all.vars(covForm) == sapply(splitFormula(covForm, "+"),
                          function(el) deparse(el[[2]])))) {
          covar <- split(getCovariate(data, covForm), grps)
        } else {
          covar <- lapply(split(data, grps), getCovariate, covForm)
        }
      }
    } else {
      if (is.null(grps)) {
        covar <- 1:nrow(data)
      } else {
	covar <- lapply(split(grps, grps), function(x) 1:length(x))
      }
    }
    if (!is.null(grps)) {
      covar <- as.list(covar)
    }
  }
  covar
}

getGroups.corStruct <-
  function(object, form = formula(object), level, data, sep)
{
  if (is.null(val <- attr(object, "groups"))) { # need to calculate
    if (!missing(data)) {
      if ((grpLev <- length(getGroupsFormula(form, asList = TRUE))) > 0) {
        ## use innermost grouping level
        val <- getGroups(data, form, level = grpLev)
        factor(val, levels = unique(as.character(val)))
      } else {
        rep(1, dim(data)[1])
      }
    } else {
      NULL
    }
  } else {
    val
  }
}

Initialize.corStruct <-
  ## Initializes some attributes of corStruct objects
  function(object, data, ...)
{
  form <- formula(object)
  ## obtaining the groups information, if any
  if (!is.null(getGroupsFormula(form))) {
    attr(object, "groups") <- getGroups(object, form, data = data)
    attr(object, "Dim") <- Dim(object, attr(object, "groups"))
  } else {                              # no groups
    attr(object, "Dim") <- Dim(object, as.factor(rep(1, nrow(data))))
  }
  ## obtaining the covariate(s)
  attr(object, "covariate") <- getCovariate(object, data = data)
  object
}

logDet.corStruct <-
  function(object, covariate = getCovariate(object), ...)
{
  if (!is.null(aux <- attr(object, "logDet"))) {
    return(aux)
  }
  if (is.null(aux <- attr(object, "factor"))) {
    ## getting the transpose sqrt factor
    aux <- corMatrix(object, covariate = covariate, corr = FALSE)
  }
  if (is.null(aux1 <- attr(aux, "logDet"))) {
    ## checking for logDet attribute; if not present, get corr matrix
    aux <- corMatrix(object, covariate)
    if (data.class(aux) == "list") {    # by group
      sum(log(abs(unlist(lapply(aux, function(el) svd(el)$d)))))/2
    } else {
      sum(log(abs(svd(aux)$d)))/2
    }
  } else {
    -aux1
  }
}

## NB, no "nobs"
logLik.corStruct <-
  function(object, data, ...) -logDet(object)

needUpdate.corStruct <-
  function(object) FALSE

print.corStruct <-
  function(x, ...)
{
  if (length(aux <- coef(x, unconstrained = FALSE)) > 0) {
    cat("Correlation structure of class", class(x)[1], "representing\n")
    print(aux, ...)
  } else {
    cat("Uninitialized correlation structure of class", class(x)[1], "\n")
  }
  invisible(x)
}

print.summary.corStruct <-
  function(x, ...)
{
  class(x) <- attr(x, "oClass")
  cat(paste("Correlation Structure: ", attr(x, "structName"), "\n", sep = ""))
  cat(paste(" Formula:", deparse(formula(x)),"\n"))
  cat(" Parameter estimate(s):\n")
  print(coef(x, unconstrained = FALSE))
  invisible(x)
}


recalc.corStruct <-
  function(object, conLin, ...)
{
  conLin[["Xy"]][] <-
    .C(corStruct_recalc,
       Xy = as.double(conLin[["Xy"]]),
       as.integer(unlist(Dim(object))),
       as.integer(ncol(conLin[["Xy"]])),
       as.double(unlist(corFactor(object))))[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + logLik(object)
  conLin
}

summary.corStruct <-
  function(object, structName = class(object)[1], ...)
{
  attr(object, "structName") <- structName
  attr(object, "oClass") <- class(object)
  class(object) <- "summary.corStruct"
  object
}

update.corStruct <-
  function(object, data, ...)
{
  object
}

##*## Classes that substitute for (i.e. inherit from) corStruct

###*# corSymm - general, unstructured correlation

####* Constructor

corSymm <-
  ## Constructor for the corSymm class
  function(value = numeric(0), form = ~ 1, fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "fixed") <- fixed
  class(value) <- c("corSymm", "corStruct")
  value
}

###*# Methods for local generics

corFactor.corSymm <-
  function(object, ...)
{
  corD <- Dim(object)
  val <- .C(symm_factList,
	    as.double(as.vector(object)),
	    as.integer(unlist(attr(object, "covariate"))),
	    as.integer(attr(object, "maxCov")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corSymm <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate, length))))
  } else {
    corD <- Dim(object, rep(1, length(covariate)))
  }
  if (corr) {
    val <- .C(symm_matList,
	      as.double(as.vector(object)),
	      as.integer(unlist(covariate)),
	      as.integer(attr(object, "maxCov")),
	      as.integer(unlist(corD)),
	      mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(symm_factList,
              as.double(as.vector(object)),
              as.integer(unlist(covariate)),
              as.integer(attr(object, "maxCov")),
              as.integer(unlist(corD)),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for standard generics

coef.corSymm <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) {
    if (attr(object, "fixed")) {
      return(numeric(0))
    } else {
      return(as.vector(object))
    }
  }
  mC <- attr(object, "maxCov")
  .C(symm_fullCorr, as.double(object),
     as.integer(mC), corr = double(round(mC * (mC - 1) / 2)))[["corr"]]
}

"coef<-.corSymm" <-
  function(object, ..., value)
{
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  object[] <- value
  corD <- attr(object, "Dim")
  ## updating the factor list and logDet
  aux <- .C(symm_factList,
	    as.double(as.vector(object)),
	    as.integer(unlist(getCovariate(object))),
	    as.integer(attr(object, "maxCov")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Initialize.corSymm <-
  function(object, data, ...)
{
  if (!is.null(attr(object, "maxCov"))) {# initialized - nothing to do
    return(object)
  }
  object <- NextMethod()

  covar <- attr(object, "covariate")
  if (data.class(covar) != "list") {
    covar <- list(covar)
  }
  if (any(unlist(lapply(covar, duplicated)))) {
    stop(paste("Covariate must have unique values",
	       "within groups for corSymm objects"))
  }
  covar <- unlist(covar) - 1
  maxCov <- max(uCov <- unique(covar)) + 1
  if (length(uCov) != maxCov) {
    stop(paste("Unique values of the covariate  for \"corSymm\"",
	       "objects must be a sequence of consecutive integers"))
  }
  if (Dim(object)[["M"]] > 1) {
    attr(object, "covariate") <- split(covar, getGroups(object))
  } else {
    attr(object, "covariate") <- covar
  }
  attr(object, "maxCov") <- maxCov
  natPar <- as.vector(object)
  if (length(natPar) > 0) {
    ## parameters assumed in constrained form
    if (length(natPar) != round(maxCov * (maxCov - 1) / 2)) {
      stop("Initial value for corSymm parameters of wrong dimension")
    }
    if (max(abs(natPar)) >= 1) {
      stop("Initial values for corSymm must be between -1 and 1")
    }
    natMat <- diag(maxCov)/2
    natMat[lower.tri(natMat)] <- natPar
    natMat <- (t(natMat) + natMat)
    ## checking if positive-definite
    if (any(eigen(natMat)$values <= 0)) {
      stop(paste("Initial values for corSymm do not define",
                 "a positive-definite correlation structure"))
    }
    natMat <- chol(natMat)
    uncPar <- numeric(0)
    for(i in 2:maxCov) {
      aux <- acos(natMat[1:(i-1),i]/sqrt(cumsum(natMat[i:1,i]^2)[i:2]))
      uncPar <- c(uncPar, log(aux/(pi - aux)))
    }
    coef(object) <- uncPar
  } else {				# initializing the parameters
    oldAttr <- attributes(object)
    object <- double(round(maxCov * (maxCov - 1) / 2))
    attributes(object) <- oldAttr
    attr(object, "factor") <- corFactor(object)
    attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  }
  object
}

print.corSymm <-
  function(x, ...)
{
  if (length(as.vector(x)) > 0 &&
      !is.null(mC <- attr(x, "maxCov"))) {
    aux <- coef.corSymm(x, unconstrained = FALSE)
    val <- diag(mC)
    dimnames(val) <- list(1:mC, 1:mC)
    val[lower.tri(val)] <- aux
    class(val) <- "correlation"
    cat("Correlation structure of class corSymm representing\n")
    print(val, ...)
  }
  else cat("Unitialized correlation structure of class corSymm\n")
  invisible(x)
}

print.summary.corSymm <-
  function(x, ...)
{
  if (length(as.vector(x)) > 0 &&
      !is.null(mC <- attr(x, "maxCov"))) {
    cat("Correlation Structure: General\n")
    cat(paste(" Formula:", deparse(formula(x)),"\n"))
    cat(" Parameter estimate(s):\n")
    val <- diag(mC)
    dimnames(val) <- list(1:mC, 1:mC)
    val[lower.tri(val)] <- coef.corSymm(x, unconstrained = FALSE)
    class(val) <- "correlation"
    print(val, ...)
  } else cat("Unitialized correlation structure of class corSymm\n")
  invisible(x)
}

recalc.corSymm <-
  function(object, conLin, ...)
{
  val <-
    .C(symm_recalc,
       Xy = as.double(conLin[["Xy"]]),
       as.integer(unlist(Dim(object))),
       as.integer(ncol(conLin[["Xy"]])),
       as.double(as.vector(object)),
       as.integer(unlist(getCovariate(object))),
       as.integer(attr(object, "maxCov")),
       logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

summary.corSymm <-
  function(object, structName = "General correlation", ...)
{
  attr(object, "structName") <- structName
  class(object) <- "summary.corSymm"
  object
}

###*# corNatural - general correlation in natural parametrization

####* Constructor

corNatural <-
  ## Constructor for the corSymm class
  function(value = numeric(0), form = ~ 1, fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "fixed") <- fixed
  class(value) <- c("corNatural", "corStruct")
  value
}

###*# Methods for local generics

corFactor.corNatural <-
  function(object, ...)
{
  corD <- Dim(object)
  val <- .C(nat_factList,
	    as.double(as.vector(object)),
	    as.integer(unlist(attr(object, "covariate"))),
	    as.integer(attr(object, "maxCov")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
            logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corNatural <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate, length))))
  } else {
    corD <- Dim(object, rep(1, length(covariate)))
  }
  if (corr) {
    val <- .C(nat_matList,
	      as.double(as.vector(object)),
	      as.integer(unlist(covariate)),
	      as.integer(attr(object, "maxCov")),
	      as.integer(unlist(corD)),
              mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(nat_factList,
              as.double(as.vector(object)),
              as.integer(unlist(covariate)),
              as.integer(attr(object, "maxCov")),
              as.integer(unlist(corD)),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for standard generics

coef.corNatural <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) {
    if (attr(object, "fixed")) {
      return(numeric(0))
    } else {
      return(as.vector(object))
    }
  }
  mC <- attr(object, "maxCov")
  val <- .C(nat_fullCorr, as.double(object),
            as.integer(mC), corr = double(round(mC * (mC - 1) / 2)))[["corr"]]
  names(val) <- outer(1:mC, 1:mC,
                      function(x,y) {
                        paste("cor(",y,",",x,")",sep="")
                      })[lower.tri(diag(mC))]
  val
}

"coef<-.corNatural" <-
  function(object, ..., value)
{
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  object[] <- value
  corD <- attr(object, "Dim")
  ## updating the factor list and logDet
  aux <- .C(nat_factList,
	    as.double(as.vector(object)),
	    as.integer(unlist(getCovariate(object))),
	    as.integer(attr(object, "maxCov")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Initialize.corNatural <-
  function(object, data, ...)
{
  if (!is.null(attr(object, "maxCov"))) {# initialized - nothing to do
    return(object)
  }
  object <- NextMethod()

  covar <- attr(object, "covariate")
  if (data.class(covar) != "list") {
    covar <- list(covar)
  }
  if (any(unlist(lapply(covar, duplicated)))) {
    stop(paste("Covariate must have unique values",
	       "within groups for corNatural objects"))
  }
  covar <- unlist(covar) - 1
  maxCov <- max(uCov <- unique(covar)) + 1
  if (length(uCov) != maxCov) {
    stop(paste("Unique values of the covariate  for corNatural",
	       "objects must be a sequence of consecutive integers"))
  }
  if (Dim(object)[["M"]] > 1) {
    attr(object, "covariate") <- split(covar, getGroups(object))
  } else {
    attr(object, "covariate") <- covar
  }
  attr(object, "maxCov") <- maxCov
  natPar <- as.vector(object)
  if (length(natPar) > 0) {
    ## parameters assumed in constrained form
    if (length(natPar) != round(maxCov * (maxCov - 1) / 2)) {
      stop("Initial value for corNatural parameters of wrong dimension")
    }
    if (max(abs(natPar)) >= 1) {
      stop("Initial values for corNatural must be between -1 and 1")
    }
    natMat <- diag(maxCov)/2
    natMat[lower.tri(natMat)] <- natPar
    natMat <- (t(natMat) + natMat)
    ## checking if positive-definite
    if (any(eigen(natMat)$values <= 0)) {
      stop(paste("Initial values for corNatural do not define",
                 "a positive-definite correlation structure"))
    }
    coef(object) <- log((natPar + 1)/(1 - natPar))
  } else {				# initializing the parameters
    oldAttr <- attributes(object)
    object <- double(round(maxCov * (maxCov - 1) / 2))
    attributes(object) <- oldAttr
    attr(object, "factor") <- corFactor(object)
    attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  }
  object
}

print.corNatural <-
  function(x, ...)
{
  if (length(as.vector(x)) > 0 &&
      !is.null(mC <- attr(x, "maxCov"))) {
    aux <- coef(x, FALSE)
    val <- diag(mC)
    dimnames(val) <- list(1:mC, 1:mC)
    val[lower.tri(val)] <- aux
    class(val) <- "correlation"
    cat("Correlation structure of class corNatural representing\n")
    print(val, ...)
  }
  else cat("Unitialized correlation structure of class corNatural\n")
  invisible(x)
}

print.summary.corNatural <-
  function(x, ...)
{
  if (length(as.vector(x)) > 0 &&
      !is.null(mC <- attr(x, "maxCov"))) {
    cat("Correlation Structure: General\n")
    cat(paste(" Formula:", deparse(formula(x)),"\n"))
    cat(" Parameter estimate(s):\n")
    aux <- coef(x, FALSE)
    val <- diag(mC)
    dimnames(val) <- list(1:mC, 1:mC)
    val[lower.tri(val)] <- aux
    class(val) <- "correlation"
    print(val, ...)
  } else cat("Unitialized correlation structure of class corNatural\n")
  invisible(x)
}

recalc.corNatural <-
  function(object, conLin, ...)
{
  val <-
    .C(nat_recalc,
       Xy = as.double(conLin[["Xy"]]),
       as.integer(unlist(Dim(object))),
       as.integer(ncol(conLin[["Xy"]])),
       as.double(as.vector(object)),
       as.integer(unlist(getCovariate(object))),
       as.integer(attr(object, "maxCov")),
       logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

summary.corNatural <-
  function(object,
           structName = "General correlation, with natural parametrization",
           ...)
{
  attr(object, "structName") <- structName
  class(object) <- "summary.corNatural"
  object
}

###*# corIdent - independent structure

####* Constructor

corIdent <-
  ## Constructor for the corIdent class
  function(form = NULL)
{
  value <- numeric(0)
  attr(value, "formula") <- form
  attr(value, "fixed") <- TRUE
  class(value) <- c("corIdent", "corStruct")
  value
}

###*# Methods for local generics

corMatrix.corIdent <-
  function(object, covariate = getCovariate(object), corr, ...)
{
  if (data.class(covariate) == "list") {# by group
    as.list(lapply(covariate, function(el, object) corMatrix(object, el)))
  } else {
    diag(length(covariate))
  }
}

###*# Methods for standard generics

coef.corIdent <-
  function(object, unconstrained = TRUE, ...) numeric(0)

"coef<-.corIdent" <-
  function(object, ..., value) object

Initialize.corIdent <-
  function(object, data, ...)
{
  attr(object, "logDet") <- 0
  object
}

logDet.corIdent <-
  function(object, covariate, ...) 0

recalc.corIdent <-
  function(object, conLin, ...)
{
  conLin
}

summary.corIdent <-
  function(object, structName = "Independent", ...)
{
  summary.corStruct(object, structName)
}

###*# corAR1 - autoregressive of order one structure

####* Constructor

corAR1 <-
  ## Constructor for the corAR1 class
  function(value = 0, form = ~ 1, fixed = FALSE)
{
  if (abs(value) >= 1) {
    stop("Parameter in AR(1) structure must be between -1 and 1")
  }
  value <- log((1 + value)/( 1 - value))
  attr(value, "formula") <- form
  attr(value, "fixed") <- fixed
  class(value) <- c("corAR1", "corStruct")
  value
}

###*# Methods for local generics

corFactor.corAR1 <-
  function(object, ...)
{
  corD <- Dim(object)
  val <- .C(AR1_factList,
	    as.double(as.vector(object)),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corAR1 <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate, length))))
  } else {
    corD <- Dim(object, rep(1, length(covariate)))
  }
  if (corr) {
    val <- .C(AR1_matList,
	      as.double(as.vector(object)),
	      as.integer(unlist(corD)),
	      mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(AR1_factList,
              as.double(as.vector(object)),
              as.integer(unlist(corD)),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for standard generics

coef.corAR1 <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) {
    if (attr(object, "fixed")) {
      return(numeric(0))
    } else {
      return(as.vector(object))
    }
  }
  aux <- exp(as.vector(object))
  aux <- c((aux - 1)/(aux + 1))
  names(aux) <- "Phi"
  aux
}

"coef<-.corAR1" <-
  function(object, ..., value)
{
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  object[] <- value
  corD <- attr(object, "Dim")
  ## updating the factor list and logDet
  aux <- .C(AR1_factList,
	    as.double(as.vector(object)),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Initialize.corAR1 <-
  ## Initializes corAR1 objects
  function(object, data, ...)
{
  object <- NextMethod()
  covar <- attr(object, "covariate")
  if (data.class(covar) != "list") {
    covar <- list(covar)
  }
  if (any(unlist(lapply(covar, duplicated)))) {
    stop(paste("Covariate must have unique values",
	       "within groups for corAR1 objects"))
  }
  if (any(unlist(lapply(covar, diff)) != 1)) {
    ## Cannot use formulas for inverse of square root matrix
    ## will convert to class ARMA(1,0)
    attr(object, "p") <- 1
    attr(object, "q") <- 0
    class(object) <- c("corARMA", "corStruct")
    Initialize(object, data)
  } else {
    ## obtaining the factor list and logDet
    attr(object, "factor") <- corFactor(object)
    attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
    object
  }
}

recalc.corAR1 <-
  function(object, conLin, ...)
{
  val <-
    .C(AR1_recalc,
       Xy = as.double(conLin[["Xy"]]),
       as.integer(unlist(Dim(object))),
       as.integer(ncol(conLin[["Xy"]])),
       as.double(as.vector(object)),
       logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

summary.corAR1 <-
  function(object, structName = "AR(1)", ...)
{
  summary.corStruct(object, structName)
}

####*# corCAR1 - continuous time autoregressive of order one structure

#####* Constructor

corCAR1 <-
  ## Constructor for the corCAR1 class
  function(value = 0.2, form = ~ 1, fixed = FALSE)
{
  if (value <= 0 | value >= 1) {
    stop("Parameter in CAR(1) structure must be between 0 and 1")
  }
  value <- log(value / (1 - value))
  attr(value, "formula") <- form
  attr(value, "fixed") <- fixed
  class(value) <- c("corCAR1", "corStruct")
  value
}


###*# Methods for local generics

corFactor.corCAR1 <-
  function(object, ...)
{
  corD <- Dim(object)
  val <- .C(CAR1_factList,
	    as.double(as.vector(object)),
	    as.double(unlist(attr(object, "covariate"))),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corCAR1 <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate, length))))
  } else {
    corD <- Dim(object, rep(1, length(covariate)))
  }
  if (corr) {
    val <- .C(CAR1_matList,
	      as.double(as.vector(object)),
	      as.double(unlist(covariate)),
	      as.integer(unlist(corD)),
	      mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(CAR1_factList,
              as.double(as.vector(object)),
              as.double(unlist(covariate)),
              as.integer(unlist(corD)),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for standard generics

coef.corCAR1 <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) {
    if (attr(object, "fixed")) {
      return(numeric(0))
    } else {
      return(as.vector(object))
    }
  }
  aux <- c(exp(as.vector(object)))
  aux <- aux/(1+aux)
  names(aux) <- "Phi"
  aux
}

"coef<-.corCAR1" <-
  function(object, ..., value)
{
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  object[] <- value
  corD <- attr(object, "Dim")
  ## updating the factor list and logDet
  aux <- .C(CAR1_factList,
	    as.double(as.vector(object)),
	    as.double(unlist(getCovariate(object))),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Initialize.corCAR1 <-
  ## Initializes corCAR1 objects
  function(object, data, ...)
{
  object <- NextMethod()
  covar <- attr(object, "covariate")
  if (data.class(covar) != "list") {
    covar <- list(covar)
  }

  if (any(unlist(lapply(covar, duplicated)))) {
    stop(paste("Covariate must have unique values",
	       "within groups for corCAR1 objects"))
  }
  attr(object, "factor") <- corFactor(object)
  attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  object
}

recalc.corCAR1 <-
  function(object, conLin, ...)
{
  val <-
    .C(CAR1_recalc,
     Xy = as.double(conLin[["Xy"]]),
     as.integer(unlist(Dim(object))),
     as.integer(ncol(conLin[["Xy"]])),
     as.double(as.vector(object)),
     as.double(unlist(getCovariate(object))),
     logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

summary.corCAR1 <-
  function(object, structName = "Continuous AR(1)", ...)
{
  summary.corStruct(object, structName)
}

###*# corARMA - autoregressive-moving average structures

####* Constructor

corARMA <-
  ## Constructor for the corARMA class
  function(value = double(p + q), form = ~ 1, p = 0, q = 0, fixed = FALSE)
{
  if (!(p >= 0 && (p == round(p)))) {
    stop("Autoregressive order must be a non-negative integer")
  }
  if (!(q >= 0 && (q == round(q)))) {
    stop("Moving average order must be a non-negative integer")
  }
  if (0 == (p + q)) {
    return(corIdent())
  }
  if (length(value) != p + q) {
    stop("Initial value for parameter of wrong length")
  }
  if (max(abs(value)) >= 1) {
    stop("Parameters in ARMA structure must be < 1 in absolute value")
  }
  ## unconstrained parameters
  value <- .C(ARMA_unconstCoef,
	      as.integer(p),
	      as.integer(q),
	      pars = as.double(value))$pars
  attributes(value) <- list(formula = form, p = p, q = q, fixed = fixed)
  class(value) <- c("corARMA", "corStruct")
  value
}


###*# Methods for local generics

corFactor.corARMA <-
  function(object, ...)
{
    maxLag <- attr(object, "maxLag")
    if(is.null(maxLag)) stop("'object' has not been Initialize()d")
  corD <- Dim(object)
  val <- .C(ARMA_factList,
	    as.double(as.vector(object)),
	    as.integer(attr(object, "p")),
	    as.integer(attr(object, "q")),
	    as.integer(unlist(attr(object, "covariate"))),
	    as.integer(attr(object, "maxLag")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}


corMatrix.corARMA <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate, length))))
  } else {
    corD <- Dim(object, rep(1, length(covariate)))
  }
  p <- attr(object, "p")
  q <- attr(object, "q")
  maxLag <- attr(object, "maxLag")
  if(is.null(maxLag)) stop("'object' has not been Initialize()d")
  if (corr) {
    val <- .C(ARMA_matList,
	      as.double(as.vector(object)),
	      as.integer(p),
	      as.integer(q),
	      as.integer(unlist(covariate)),
	      as.integer(maxLag),
	      as.integer(unlist(corD)),
	      mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(ARMA_factList,
              as.double(as.vector(object)),
              as.integer(attr(object, "p")),
              as.integer(attr(object, "q")),
              as.integer(unlist(covariate)),
              as.integer(attr(object, "maxLag")),
              as.integer(unlist(corD)),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for standard generics

coef.corARMA <-
  function(object, unconstrained = TRUE, ...)
{
  if (attr(object, "fixed") && unconstrained) {
    return(numeric(0))
  }
  val <-  as.vector(object)
  if (!unconstrained) {
    p <- attr(object, "p")
    q <- attr(object, "q")
    nams <- NULL
    if (p > 0) {
      nams <- paste(rep("Phi", p), 1:p, sep="")
    }
    if (q > 0) {
      nams <- c(nams, paste(rep("Theta", q), 1:q, sep=""))
    }
    val <- c(.C(ARMA_constCoef, as.integer(attr(object,"p")),
		as.integer(attr(object,"q")),
		pars = as.double(val))$pars)
    names(val) <- nams
  }
  val
}

"coef<-.corARMA" <-
  function(object, ..., value)
{
  maxLag <- attr(object, "maxLag")
  if(is.null(maxLag)) stop("'object' has not been Initialize()d")
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  p <- attr(object, "p")
  q <- attr(object, "q")
  object[] <- value
  ## updating the factor list and logDet
  corD <- Dim(object)
  aux <- .C(ARMA_factList,
	    as.double(as.vector(object)),
	    as.integer(p),
	    as.integer(q),
	    as.integer(unlist(getCovariate(object))),
	    as.integer(attr(object, "maxLag")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Initialize.corARMA <-
  function(object, data, ...)
{
  ## Initializes corARMA objects
  object <- NextMethod()
  covar <- attr(object, "covariate")
  if (data.class(covar) != "list") {
    covar <- list(covar)
  }
  if (any(unlist(lapply(covar, duplicated)))) {
    stop(paste("Covariate must have unique values",
	       "within groups for corARMA objects"))
  }
  if ((attr(object, "p") == 1) && (attr(object, "q") == 0) &&
     all(unlist(lapply(covar, diff)) == 1)) {
    ## Use AR1 methods instead
    class(object) <- c("corAR1", "corStruct")
    Initialize(object, data)
  } else {
    attr(object, "maxLag") <-
      max(unlist(lapply(covar, function(el) max(abs(outer(el,el,"-"))))))
    attr(object, "factor") <- corFactor(object)
    attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
    object
  }
}

recalc.corARMA <-
  function(object, conLin, ...)
{
    maxLag <- attr(object, "maxLag")
    if(is.null(maxLag)) stop("'object' has not been Initialize()d")
  val <-
    .C(ARMA_recalc,
     Xy = as.double(conLin[["Xy"]]),
     as.integer(unlist(Dim(object))),
     as.integer(ncol(conLin[["Xy"]])),
     as.double(as.vector(object)),
     as.integer(attr(object, "p")),
     as.integer(attr(object, "q")),
     as.integer(unlist(getCovariate(object))),
     as.integer(attr(object, "maxLag")),
     logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

summary.corARMA <-
  function(object, structName = paste("ARMA(",attr(object,"p"),",",
		     attr(object,"q"), ")", sep = ""), ...)
{
  summary.corStruct(object, structName)
}

###*# corCompSymm - Compound symmetry structure structure

####* Constructor

corCompSymm <-
  ## Constructor for the corCompSymm class
  function(value = 0, form = ~ 1, fixed = FALSE)
{
  if (abs(value) >= 1) {
    stop(paste("Parameter in \"corCompSymm\" structure",
	       "must be < 1 in absolute value"))
  }
  attr(value, "formula") <- form
  attr(value, "fixed") <- fixed
  class(value) <- c("corCompSymm", "corStruct")
  value
}

###*# Methods for local generics

corFactor.compSymm <-
  function(object, ...)
{
  corD <- Dim(object)
  val <- .C(compSymm_factList,
	    as.double(as.vector(object)),
	    as.double(attr(object, "inf")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corCompSymm <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate, length))))
  } else {
    corD <- Dim(object, rep(1, length(covariate)))
  }
  if (corr) {
    val <- .C(compSymm_matList,
	      as.double(as.vector(object)),
	      as.double(attr(object, "inf")),
	      as.integer(unlist(corD)),
	      mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(compSymm_factList,
              as.double(as.vector(object)),
              as.double(attr(object, "inf")),
              as.integer(unlist(corD)),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for local generics

coef.corCompSymm <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) {
    if (attr(object, "fixed")) {
      return(numeric(0))
    } else {
      return(as.vector(object))
    }
  }
  val <- exp(as.vector(object))
  val <- c((val + attr(object, "inf"))/(val + 1))
  names(val) <- "Rho"
  val
}

"coef<-.corCompSymm" <-
  function(object, ..., value)
{
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter of a corStruct object")
  }
  object[] <- value
  corD <- attr(object, "Dim")
  ## updating the factor list and logDet
  aux <- .C(compSymm_factList,
	    as.double(as.vector(object)),
	    as.double(attr(object, "inf")),
	    as.integer(unlist(corD)),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Initialize.corCompSymm <-
  ## Initializes corCompSymm objects
  function(object, data, ...)
{
  if (!is.null(attr(object, "inf"))) {   # initialized - nothing to do
    return(object)
  }
  object <- NextMethod()
  natPar <- as.vector(object)
  corD <- Dim(object)
  if (natPar <= (attr(object, "inf") <- -1/(corD[["maxLen"]] - 1))) {
    stop(paste("Initial value in corCompSymm must be > than",
               attr(object, "inf")))
  }
  object[] <- log((natPar - attr(object, "inf"))/(1 - natPar))
  attr(object, "factor") <- corFactor(object)
  attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  object
}

recalc.corCompSymm <-
  function(object, conLin, ...)
{
  val <-
    .C(compSymm_recalc,
       Xy = as.double(conLin[["Xy"]]),
       as.integer(unlist(Dim(object))),
       as.integer(ncol(conLin[["Xy"]])),
       as.double(as.vector(object)),
       as.double(attr(object, "inf")),
       logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

summary.corCompSymm <-
  function(object, structName = "Compound symmetry", ...)
{
  summary.corStruct(object, structName)
}

####*# corHF - Huyn-Feldt structure

#corHF <-
#  ## Constructor for the corHuynFeldt class
#  function(value = numeric(0), form = ~ 1)
#{
#  attr(value, "formula") <- form
#  class(value) <- c("corHF", "corStruct")
#  value
#}

####*# Methods for local generics

#corFactor.corHF <-
#  function(object)
#{
#  corD <- Dim(object)
#  val <- .C("HF_factList",
#	    as.double(as.vector(object)),
#	    as.integer(attr(object, "maxCov")),
#	    as.integer(unlist(getCovariate(object))),
#	    as.integer(unlist(corD)),
#	    factor = double(corD[["sumLenSq"]]),
#	    logDet = double(1))[c("factor", "logDet")]
#  lD <- val[["logDet"]]
#  val <- val[["factor"]]
#  attr(val, "logDet") <- lD
#  val
#}

#corMatrix.corHF <-
#  function(object, covariate = getCovariate(object), corr = TRUE)
#{
#  if (data.class(covariate) == "list") {
#    if (is.null(names(covariate))) {
#      names(covariate) <- 1:length(covariate)
#    }
#    corD <- Dim(object, rep(names(covariate),
#			    unlist(lapply(covariate, length))))
#  } else {
#    corD <- Dim(object, rep(1, length(covariate)))
#  }
#  if (corr) {
#    val <- .C("HF_matList",
#	      as.double(as.vector(object)),
#	      as.integer(attr(object, "maxCov")),
#	      as.integer(unlist(covariate)),
#	      as.integer(unlist(corD)),
#	      mat = double(corD[["sumLenSq"]]))[["mat"]]
#    lD <- NULL
#  } else {
#    val <- .C("HF_factList",
#              as.double(as.vector(object)),
#              as.integer(attr(object, "maxCov")),
#              as.integer(unlist(covariate)),
#              as.integer(unlist(corD)),
#              factor = double(corD[["sumLenSq"]]),
#              logDet = double(1))[c("factor", "logDet")]
#    lD <- val[["logDet"]]
#    val <- val[["factor"]]
#  }
#  if (corD[["M"]] > 1) {
#    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
#    val <- lapply(val, function(el) {
#      nel <- round(sqrt(length(el)))
#      array(el, c(nel, nel))
#    })
#    names(val) <- names(corD[["len"]])
#  } else {
#    val <- array(val, c(corD[["N"]], corD[["N"]]))
#  }
#  attr(val, "logDet") <- lD
#  val
#}

####*# Methods for standard generics

#coef.corHF <-
#  function(object, unconstrained = TRUE)
#{
#  aux <- as.vector(object)
#  if (!unconstrained) {
#    aux <- 2 * (exp(aux) + attr(object, "inf")) + 1
#  }
#  aux
#}

#"coef<-.corHF" <-
#  function(object, value)
#{
#  if (length(value) != length(object)) {
#    stop("Cannot change the length of the parameter of a corStruct object")
#  }
#  object[] <- value
#  corD <- attr(object, "Dim")
#  ## updating the factor list and logDet
#  aux <- .C("HF_factList",
#	    as.double(as.vector(object)),
#	    as.integer(attr(object, "maxCov")),
#	    as.integer(unlist(getCovariate(object))),
#	    as.integer(unlist(corD)),
#	    factor = double(corD[["sumLenSq"]]),
#	    logDet = double(1))[c("factor", "logDet")]
#  attr(object, "factor") <- aux[["factor"]]
#  attr(object, "logDet") <- -aux[["logDet"]]
#  object
#}

#initialize.corHF <-
#  function(object, data, ...)
#{
#  if (!is.null(attr(object, "inf"))) {   # initialized - nothing to do
#    return(object)
#  }
#  object <- NextMethod()
#  covar <- attr(object, "covariate")
#  if (data.class(covar) == "list") {
#    attr(object, "covariate") <- covar <-
#      lapply(covar, function(el) el - 1)
#  } else {
#    attr(object, "covariate") <- covar <- covar - 1
#    covar <- list(covar)
#  }
#  if (any(unlist(lapply(covar, duplicated)))) {
#    stop(paste("Covariate must have unique values",
#               "within groups for corHF objects"))
#  }
#  maxCov <- max(uCov <- unique(unlist(covar))) + 1
#  if (length(uCov) != maxCov) {
#    stop(paste("Unique values of the covariate  for \"corHF\"",
#               "objects must be a sequence of consecutive integers"))
#  }
#  attr(object, "maxCov") <- maxCov
#  attr(object, "inf") <- -1/(2*maxCov)
#  natPar <- as.vector(object)
#  if (length(natPar) > 0) {
#    if (length(aux) != attr(object, "maxCov"))
#      stop("Initial value for Huyn-Feldt parameters of wrong dimension")
#    ## verifying if initial values satisfy constraints
#    if (any(natPar <= attr(object, "inf"))) {
#      stop(paste("Initial values for \"corHF\" parameters",
#		 "must be > than", attr(object, "inf")))
#    }
#    object[] <- log(natPar - attr(object, "inf"))
#  } else {				# initializing the parameters
#    oldAttr <- attributes(object)
#    object <- log(rep(-attr(object, "inf"), att(object, "maxCov")))
#    attributes(object) <- oldAttr
#  }
#  attr(object, "factor") <- corFactor(object)
#  attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
#  object
#}

#print.corHF <-
#  function(x, ...)
#{
#  if (length(as.vector(x)) > 0 && !is.null(attr(object, "maxCov")))
#    NextMethod()
#  else cat("Unitialized correlation structure of class corHF\n")
#}

#recalc.corHF <-
#  function(object, conLin)
#{
#  val <-
#    .C("HF_recalc",
#       Xy = as.double(conLin[["Xy"]]),
#       as.integer(unlist(Dim(object))),
#       as.integer(ncol(conLin[["Xy"]])),
#       as.double(as.vector(object)),
#       as.integer(unlist(getCovariate(object))),
#       as.integer(attr(object, "maxCov")),
#       logLik = double(1))[c("Xy", "logLik")]
#  conLin[["Xy"]][] <- val[["Xy"]]
#  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
#  conLin
#}

#summary.corHF <-
#  function(object, structName = "Huyn-Feldt")
#{
#  summary.corStruct(object, structName)
#}

###*# corSpatial - a virtual class of spatial correlation structures

###*# Constructor

corSpatial <-
  ## Constructor for the corSpatial class
  function(value = numeric(0), form = ~ 1, nugget = FALSE,
	   type = c("spherical", "exponential", "gaussian", "linear",
             "rational"),
	   metric = c("euclidean", "maximum", "manhattan"), fixed = FALSE)
{
  type <- match.arg(type)
  spClass <- switch(type,
		    spherical = "corSpher",
		    exponential = "corExp",
		    gaussian = "corGaus",
		    linear = "corLin",
                    rational = "corRatio")
  attr(value, "formula") <- form
  attr(value, "nugget") <- nugget
  attr(value, "metric") <- match.arg(metric)
  attr(value, "fixed") <- fixed
  class(value) <- c(spClass, "corSpatial", "corStruct")
  value
}

###*# Methods for local generics

corFactor.corSpatial <-
  function(object, ...)
{
  corD <- Dim(object)
  val <- .C(spatial_factList,
	    as.double(as.vector(object)),
	    as.integer(attr(object, "nugget")),
	    as.double(unlist(getCovariate(object))),
	    as.integer(unlist(corD)),
	    as.double(attr(object, "minD")),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  lD <- val[["logDet"]]
  val <- val[["factor"]]
  attr(val, "logDet") <- lD
  val
}

corMatrix.corSpatial <-
  function(object, covariate = getCovariate(object), corr = TRUE, ...)
{
  if (data.class(covariate) == "list") {
    if (is.null(names(covariate))) {
      names(covariate) <- 1:length(covariate)
    }
    corD <- Dim(object, rep(names(covariate),
			    unlist(lapply(covariate,
		  function(el) round((1 + sqrt(1 + 8 * length(el)))/2)))))
  } else {
    corD <- Dim(object, rep(1, round((1 + sqrt(1 + 8* length(covariate)))/2)))
  }
  if (corr) {
    val <- .C(spatial_matList,
	      as.double(as.vector(object)),
	      as.integer(attr(object, "nugget")),
	      as.double(unlist(covariate)),
	      as.integer(unlist(corD)),
	      as.double(attr(object, "minD")),
	      mat = double(corD[["sumLenSq"]]))[["mat"]]
    lD <- NULL
  } else {
    val <- .C(spatial_factList,
              as.double(as.vector(object)),
              as.integer(attr(object, "nugget")),
              as.double(unlist(getCovariate(object))),
              as.integer(unlist(corD)),
              as.double(attr(object, "minD")),
              factor = double(corD[["sumLenSq"]]),
              logDet = double(1))[c("factor", "logDet")]
    lD <- val[["logDet"]]
    val <- val[["factor"]]
  }
  if (corD[["M"]] > 1) {
    val <- split(val, rep(1:corD[["M"]], (corD[["len"]])^2))
    val <- lapply(val, function(el) {
      nel <- round(sqrt(length(el)))
      array(el, c(nel, nel))
    })
    names(val) <- names(corD[["len"]])
    val <- as.list(val)
  } else {
    val <- array(val, c(corD[["N"]], corD[["N"]]))
  }
  attr(val, "logDet") <- lD
  val
}

###*# Methods for standard generics

coef.corSpatial <-
  function(object, unconstrained = TRUE, ...)
{
  if (attr(object, "fixed") && unconstrained) {
    return(numeric(0))
  }
  val <- as.vector(object)
  if (length(val) == 0) {               # uninitialized
    return(val)
  }
  if (!unconstrained) {
    val <- exp(val)
    if (attr(object, "nugget")) val[2] <- val[2]/(1+val[2])
  }
  if (attr(object, "nugget")) names(val) <- c("range", "nugget")
  else names(val) <- "range"
  val
}

"coef<-.corSpatial" <-
  function(object, ..., value)
{
  if (length(value) != length(object)) {
    stop("Cannot change the length of the parameter after initialization")
  }
  object[] <- value
  corD <- attr(object, "Dim")
  ## updating the factor list and logDet
  aux <- .C(spatial_factList,
	    as.double(as.vector(object)),
	    as.integer(attr(object, "nugget")),
	    as.double(unlist(getCovariate(object))),
	    as.integer(unlist(corD)),
	    as.double(attr(object, "minD")),
	    factor = double(corD[["sumLenSq"]]),
	    logDet = double(1))[c("factor", "logDet")]
  attr(object, "factor") <- aux[["factor"]]
  attr(object, "logDet") <- -aux[["logDet"]]
  object
}

Dim.corSpatial <-
  function(object, groups, ...)
{
  if (missing(groups)) return(attr(object, "Dim"))
  val <- Dim.corStruct(object, groups)
  val[["start"]] <-
    c(0, cumsum(val[["len"]] * (val[["len"]] - 1)/2)[-val[["M"]]])
  ## will use third component of Dim list for spClass
  names(val)[3] <- "spClass"
  val[[3]] <-
    match(class(object)[1], c("corSpher", "corExp", "corGaus", "corLin",
                              "corRatio"), 0)
  val
}

getCovariate.corSpatial <-
  function(object, form = formula(object), data)
{
  if (is.null(covar <- attr(object, "covariate"))) { # need to calculate it
    if (missing(data)) {
      stop("Need data to calculate covariate")
    }
    covForm <- getCovariateFormula(form)
    if (length(all.vars(covForm)) > 0) { # covariate present
      if (attr(terms(covForm), "intercept") == 1) {
	covForm <-
          eval(parse(text = paste("~", deparse(covForm[[2]]),"-1",sep="")))
      }
      covar <-
          as.data.frame(unclass(model.matrix(covForm,
                                             model.frame(covForm, data,
                                                         drop.unused.levels = TRUE))))
    } else {
      covar <- NULL
    }

    if (!is.null(getGroupsFormula(form))) { # by groups
      grps <- getGroups(object, data = data)
      if (is.null(covar)) {
	covar <- lapply(split(grps, grps),
                        function(x) as.vector(dist(1:length(x))))
      } else {
	covar <- lapply(split(covar, grps),
			function(el, metric) {
                          el <- as.matrix(el)
                          if (nrow(el) > 1) {
                            as.vector(dist(el, metric))
                          } else {
                            numeric(0)
                          }
			}, metric = attr(object, "metric"))
      }
      covar <- covar[sapply(covar, length) > 0]  # no 1-obs groups
    } else {				# no groups
      if (is.null(covar)) {
	covar <- as.vector(dist(1:nrow(data)))
      } else {
	covar <- as.vector(dist(as.matrix(covar),
                                method = attr(object, "metric")))
      }
    }
    if (any(unlist(covar) == 0)) {
      stop("Cannot have zero distances in \"corSpatial\"")
    }
  }
  covar
}

Initialize.corSpatial <-
  function(object, data, ...)
{
  if (!is.null(attr(object, "minD"))) { #already initialized
    return(object)
  }
  object <- Initialize.corStruct(object, data)
  nug <- attr(object, "nugget")

  val <- as.vector(object)
  if (length(val) > 0) {		# initialized
    if (val[1] <= 0) {
      stop("Range must be > 0 in \"corSpatial\" initial value")
    }
    if (nug) {				# with nugget effect
      if (length(val) == 1) {		# assuming nugget effect not given
	val <- c(val, 0.1)		# setting it to 0.1
      } else {
	if (length(val) != 2) {
	  stop("Initial value for corSpatial parameters of wrong dimension")
	}
      }
      if ((val[2] <= 0) || (val[2] >= 1)) {
	stop("Initial value of nugget ratio must be between 0 and 1")
      }
    } else {				# only range parameter
      if (length(val) != 1) {
	stop("Initial value for corSpatial parameters of wrong dimension")
      }
    }
  } else {
    val <- min(unlist(attr(object, "covariate"))) * 0.9
    if (nug) val <- c(val, 0.1)
  }
  val[1] <- log(val[1])
  if (nug) val[2] <- log(val[2]/(1 - val[2]))
  oldAttr <- attributes(object)
  object <- val
  attributes(object) <- oldAttr
  attr(object, "minD") <- min(unlist(attr(object, "covariate")))
  attr(object, "factor") <- corFactor(object)
  attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  object
}

recalc.corSpatial <-
  function(object, conLin, ...)
{
  val <-
    .C(spatial_recalc,
       Xy = as.double(conLin[["Xy"]]),
       as.integer(unlist(Dim(object))),
       as.integer(ncol(conLin[["Xy"]])),
       as.double(as.vector(object)),
       as.double(unlist(getCovariate(object))),
       as.double(attr(object, "minD")),
       as.integer(attr(object, "nugget")),
       logLik = double(1))[c("Xy", "logLik")]
  conLin[["Xy"]][] <- val[["Xy"]]
  conLin[["logLik"]] <- conLin[["logLik"]] + val[["logLik"]]
  conLin
}

Variogram.corSpatial <-
  function(object, distance = NULL, sig2 = 1, length.out = 50, FUN, ...)
{
  if (is.null(distance)) {
    rangeDist <- range(unlist(getCovariate(object)))
    distance <- seq(rangeDist[1], rangeDist[2], length = length.out)
  }
  params <- coef(object, unconstrained = FALSE)
  if (length(params) == 1) {            # no nugget effect
    rang <- params
    nugg <- 0
  } else {                              # nugget effect
    rang <- params[1]
    nugg <- params[2]
  }
  val <- data.frame(variog = sig2 * (nugg + (1 - nugg) * FUN(distance, rang)),
                    dist = distance)
  class(val) <- c("Variogram", "data.frame")
  val
}

###*# corExp - exponential spatial correlation structure

corExp <-
  ## Constructor for the corExp class
  function(value = numeric(0), form = ~ 1, nugget = FALSE,
	   metric = c("euclidean", "maximum", "manhattan"), fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "nugget") <- nugget
  attr(value, "metric") <- match.arg(metric)
  attr(value, "fixed") <- fixed
  class(value) <- c("corExp", "corSpatial", "corStruct")
  value
}

###*# Methods for standard generics

summary.corExp <-
  function(object, structName = "Exponential spatial correlation", ...)
{
  summary.corStruct(object, structName)
}

Variogram.corExp <-
  function(object, distance = NULL, sig2 = 1, length.out = 50, ...)
{
  Variogram.corSpatial(object, distance, sig2, length.out,
                       function(x, y) { 1 - exp(-x/y) })
}

###*# corGaus - Gaussian spatial correlation structure

corGaus <-
  ## Constructor for the corGaus class
  function(value = numeric(0), form = ~ 1, nugget = FALSE,
	   metric = c("euclidean", "maximum", "manhattan"), fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "nugget") <- nugget
  attr(value, "metric") <- match.arg(metric)
  attr(value, "fixed") <- fixed
  class(value) <- c("corGaus", "corSpatial", "corStruct")
  value
}

###*# Methods for standard generics

summary.corGaus <-
  function(object, structName = "Gaussian spatial correlation", ...)
{
  summary.corStruct(object, structName)
}

Variogram.corGaus <-
  function(object, distance = NULL, sig2 = 1, length.out = 50, ...)
{
  Variogram.corSpatial(object, distance, sig2, length.out,
                       function(x, y){ 1 - exp(-(x/y)^2) })
}

###*# corLin - Linear spatial correlation structure

corLin <-
  ## Constructor for the corLin class
  function(value = numeric(0), form = ~ 1, nugget = FALSE,
	   metric = c("euclidean", "maximum", "manhattan"), fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "nugget") <- nugget
  attr(value, "metric") <- match.arg(metric)
  attr(value, "fixed") <- fixed
  class(value) <- c("corLin", "corSpatial", "corStruct")
  value
}

###*# Methods for standard generics

coef.corLin <-
  function(object, unconstrained = TRUE, ...)
{
  val <- NextMethod()
  if (!unconstrained) val[1] <- val[1] + attr(object, "minD")
  val
}

Initialize.corLin <-
  function(object, data, ...)
{
  if (!is.null(attr(object, "minD"))) { #already initialized
    return(object)
  }
  object <- Initialize.corStruct(object, data)
  nug <- attr(object, "nugget")

  minD <- min(unlist(attr(object, "covariate")))
  val <- as.vector(object)
  if (length(val) > 0) {		# initialized
    if (val[1] <= 0) {
      stop("Range must be > 0 in \"corLin\" initial value")
    }
    if (val[1] <= minD) {
      warning(paste("Initial value for range less than minimum distance.",
		    "Setting it to 1.1 * min(distance)"))
      val[1] <- 1.1 * minD
    }
    if (nug) {				# with nugget effect
      if (length(val) == 1) {		# assuming nugget effect not given
	val <- c(val, 0.1)		# setting it to 0.1
      } else {
	if (length(val) != 2) {
	  stop("Initial value for corSpher parameters of wrong dimension")
	}
      }
      if ((val[2] <= 0) || (val[2] >= 1)) {
	stop("Initial value of nugget ratio must be between 0 and 1")
      }
    } else {				# only range parameter
      if (length(val) != 1) {
	stop("Initial value for corLin parameters of wrong dimension")
      }
    }
  } else {
    val <- minD * 1.1
    if (nug) val <- c(val, 0.1)
  }
  val[1] <- log(val[1] - minD)
  if (nug) val[2] <- log(val[2]/(1 - val[2]))
  oldAttr <- attributes(object)
  object <- val
  attributes(object) <- oldAttr
  attr(object, "minD") <- minD
  attr(object, "factor") <- corFactor(object)
  attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  object
}

summary.corLin <-
  function(object, structName = "Linear spatial correlation", ...)
{
  summary.corStruct(object, structName)
}

Variogram.corLin <-
  function(object, distance = NULL, sig2 = 1, length.out = 50, ...)
{
  Variogram.corSpatial(object, distance, sig2, length.out,
                       function(x, y) { pmin(x/y, 1) })
}

###*# corRatio - rational quadratic spatial correlation structure

corRatio <-
  ## Constructor for the corRational class
  function(value = numeric(0), form = ~ 1, nugget = FALSE,
	   metric = c("euclidean", "maximum", "manhattan"), fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "nugget") <- nugget
  attr(value, "metric") <- match.arg(metric)
  attr(value, "fixed") <- fixed
  class(value) <- c("corRatio", "corSpatial", "corStruct")
  value
}

###*# Methods for standard generics

summary.corRatio <-
  function(object, structName = "Rational quadratic spatial correlation", ...)
{
  summary.corStruct(object, structName)
}

Variogram.corRatio <-
  function(object, distance = NULL, sig2 = 1, length.out = 50, ...)
{
  Variogram.corSpatial(object, distance, sig2, length.out,
                       function(x, y) {
                         x <- (x/y)^2
                         x/(1+x)
                       })
}

###*# corSpher - spherical spatial correlation structure

corSpher <-
  ## Constructor for the corSpher class
  function(value = numeric(0), form = ~ 1, nugget = FALSE,
	   metric = c("euclidean", "maximum", "manhattan"), fixed = FALSE)
{
  attr(value, "formula") <- form
  attr(value, "nugget") <- nugget
  attr(value, "metric") <- match.arg(metric)
  attr(value, "fixed") <- fixed
  class(value) <- c("corSpher", "corSpatial", "corStruct")
  value
}

###*# Methods for standard generics

coef.corSpher <-
  function(object, unconstrained = TRUE, ...)
{
  val <- NextMethod()
  if (!unconstrained) val[1] <- val[1] + attr(object, "minD")
  val
}

Initialize.corSpher <-
  function(object, data, ...)
{
  if (!is.null(attr(object, "minD"))) { #already initialized
    return(object)
  }
  object <- Initialize.corStruct(object, data)
  nug <- attr(object, "nugget")

  minD <- min(unlist(attr(object, "covariate")))
  val <- as.vector(object)
  if (length(val) > 0) {		# initialized
    if (val[1] <= 0) {
      stop("Range must be > 0 in \"corSpher\" initial value")
    }
    if (val[1] <= minD) {
      warning(paste("Initial value for range less than minimum distance.",
		    "Setting it to 1.1 * min(distance)"))
      val[1] <- 1.1 * minD
    }
    if (nug) {				# with nugget effect
      if (length(val) == 1) {		# assuming nugget effect not given
	val <- c(val, 0.1)		# setting it to 0.1
      } else {
	if (length(val) != 2) {
	  stop("Initial value for corSpher parameters of wrong dimension")
	}
      }
      if ((val[2] <= 0) || (val[2] >= 1)) {
	stop("Initial value of nugget ratio must be between 0 and 1")
      }
    } else {				# only range parameter
      if (length(val) != 1) {
	stop("Initial value for corSpher parameters of wrong dimension")
      }
    }
  } else {
    val <- minD * 1.1
    if (nug) val <- c(val, 0.1)
  }
  val[1] <- log(val[1] - minD)
  if (nug) val[2] <- log(val[2]/(1 - val[2]))
  oldAttr <- attributes(object)
  object <- val
  attributes(object) <- oldAttr
  attr(object, "minD") <- minD
  attr(object, "factor") <- corFactor(object)
  attr(object, "logDet") <- -attr(attr(object, "factor"), "logDet")
  object
}

summary.corSpher <-
  function(object, structName = "Spherical spatial correlation", ...)
{
  summary.corStruct(object, structName)
}

Variogram.corSpher <-
  function(object, distance = NULL, sig2 = 1, length.out = 50, ...)
{
  Variogram.corSpatial(object, distance, sig2, length.out,
                       function(x, y) {
                         x <- pmin(x/y, 1)
                         1.5 * x - 0.5 * x^3
                       })
}

####*# corWave - Wave spatial correlation structure

#corWave <-
#  ## Constructor for the corWave class
#  function(value = numeric(0), form = ~ 1, nugget = FALSE,
#	   metric = c("euclidean", "maximum", "manhattan"))
#{
#  attr(value, "formula") <- form
#  attr(value, "nugget") <- nugget
#  attr(value, "metric") <- match.arg(metric)
#  class(value) <- c("corWave", "corSpatial", "corStruct")
#  value
#}

####*# Methods for standard generics

#summary.corWave <-
#  function(object, structName = "Wave spatial correlation")
#{
#  summary.corStruct(object, structName)
#}


##*## Beginning of epilogue
### This file is automatically placed in Outline minor mode.
### The file is structured as follows:
### Chapters:     ^L #
### Sections:    ##*##
### Subsections: ###*###
### Components:  non-comment lines flushed left
###              Random code beginning with a ####* comment

### Local variables:
### mode: S
### mode: outline-minor
### outline-regexp: "\^L\\|\\`#\\|##\\*\\|###\\*\\|[a-zA-Z]\\|\\\"[a-zA-Z]\\|####\\*"
### End:


