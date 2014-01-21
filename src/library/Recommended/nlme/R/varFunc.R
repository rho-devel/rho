###              Classes of variance functions
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

##*## Generics that should be implemented for any varFunc class

varWeights <-
  ## Calculates the weights of the variance function
  function(object) UseMethod("varWeights")

##*## varFunc - a virtual class of variance functions

###*# Constructor

varFunc <-
  ## Can take as argument either a varFunc object, in which case it does
  ## nothing, a formula or a character string , in which case it
  ## calls varFixed
  function(object)
{
  if(is.null(object)) return(object)	# NULL object - no varFunc structure
  if (inherits(object, "varFunc")) {
    ## constructing from another varFunc object
    return(object)
  }
  if (inherits(object, "formula") || is.character(object)) {
    ## constructing from a formula of the form ~ x
    return(varFixed(asOneSidedFormula(object)))
  }

  stop("can only construct \"varFunc\" object from another \"varFunc\" object, a formula, or a character string")
}


###*# Methods for local generics

varWeights.varFunc <-
  function(object) attr(object, "weights")

###*# Methods for standard generics

coef.varFunc <-
  function(object, unconstrained = TRUE, allCoef = FALSE, ...)
{
  ### checking if initialized
  wPar <- attr(object, "whichFix")
  if (is.null(wPar) ||
      (length(object) != (length(wPar) - sum(wPar)))) {
    stop("cannot extract parameters of uninitialized object")
  }
  if (unconstrained) {
    if (allCoef) {
      val <- double(length(wPar))
      if (any(wPar)) {
        val[wPar] <- attr(object, "fixed")
      }
      if (any(!wPar)) {
        val[!wPar] <- as.vector(object)
      }
    } else {
      val <- as.vector(object)
    }
    val
  } else {
      stop(gettextf("do not know how to get coefficients for %s object",
                    dQuote(class(object)[1])), domain = NA)
  }
}

"covariate<-.varFunc" <-
  function(object, value)
{
  value <- as.numeric(value)
  if (!is.null(aux <- getCovariate(object))) {
    if (length(aux) != length(value)) {
      stop("cannot change the length of covariate in \"varFunc\" object")
    }
  }
  attr(object, "covariate") <- value
  object
}

formula.varFunc <-
  function(x, ...) eval(attr(x, "formula"))

getCovariate.varFunc <-
  function(object, form, data) attr(object, "covariate")

getGroups.varFunc <-
  function(object, form, level, data, sep) attr(object, "groups")

Initialize.varFunc <-
  function(object, data, ...)
{
  if (is.null(varWeights(object))) {
    attr(object, "weights") <- rep(1, dim(data)[1])
  }
  if (is.null(logLik(object))) {
    attr(object, "logLik") <- 0
  }
  object
}

## NB no nobs
logLik.varFunc <-
  function(object, data, ...)
{
    if (is.null(ll <- attr(object, "logLik"))) return(NULL)
    attr(ll, "df") <- length(object)
    class(ll) <- "logLik"
    ll
}

print.summary.varFunc <-
  function(x, header = TRUE, ...)
{
  ox <- x
  class(x) <- attr(x, "oClass")
  if (length(aux <- coef(x, uncons = FALSE, allCoef = TRUE)) > 0) {
    if (header) cat("Variance function:\n")
    cat(paste(" Structure: ", attr(x, "structName"), "\n", sep = ""))
    cat(paste(" Formula:", deparse(formula(x)),"\n"))
    cat(" Parameter estimates:\n")
    print(aux)
  } else {
    if (inherits(x, "varIdent")) {
      ## varIdent with no parameters - nothing to print
      return()
    }
    cat("Variance function structure of class", class(x)[1],
	"with no parameters, or uninitialized\n")
  }
  invisible(ox)
}

print.varFunc <-
  function(x, ...)
{
  if (length(aux <- coef(x, uncons = FALSE, allCoef = TRUE)) > 0) {
    cat("Variance function structure of class", class(x)[1],
	"representing\n")
    print(aux, ...)
  } else {
    cat("Variance function structure of class", class(x)[1],
	"with no parameters, or uninitialized\n")
  }
  invisible(x)
}

recalc.varFunc <-
  function(object, conLin, ...)
{
  conLin$Xy[] <- conLin$Xy * varWeights(object)
  conLin$logLik <- conLin$logLik + logLik(object)
  conLin
}

summary.varFunc <-
  function(object, structName = class(object)[1], ...)
{
  attr(object, "structName") <- structName
  attr(object, "oClass") <- class(object)
  class(object) <- "summary.varFunc"
  object
}

update.varFunc <-
  function(object, data, ...)
{
  if (needUpdate(object)) {
    covariate(object) <-
      eval(getCovariateFormula(object)[[2]], data)
  }
  object
}

##*## Classes that substitute for (i.e. inherit from) varFunc

###*# varFixed - fixed weights

####* Constructor

varFixed <-
  function(value = ~ 1)
{
  if (!inherits(value, "formula")) {
    stop("'value' must be a one sided formula")
  }
  form <- asOneSidedFormula(value)
  if (length(all.vars(getCovariateFormula(form))) == 0) {
    stop("'form' must have a covariate")
  }
  if (!is.null(getGroupsFormula(form))) {
    form <- getCovariateFormula(form)
    warning("ignoring 'group' in \"varFixed\" formula")
  }
  value <- numeric(0)
  attr(value, "formula") <- form
  class(value) <- c("varFixed", "varFunc")
  value
}

###*# Methods for standard generics

coef.varFixed <-
  function(object, ...) numeric(0)

"coef<-.varFixed" <-
  function(object, ..., value) object

Initialize.varFixed <-
  function(object, data, ...)
{
  form <- formula(object)
  if (any(is.na(match(all.vars(form), names(data))))) {
    ## cannot evaluate covariate on data
    stop("all variables used in 'formula' must be in 'data'")
  }
  attr(object, "needUpdate") <- FALSE
  attr(object, "covariate") <- getCovariate(data, form)
  attr(object, "logLik") <-
    sum(log(attr(object, "weights") <- 1/sqrt(abs(attr(object,"covariate")))))
  object
}

print.summary.varFixed <-
  function(x, header = TRUE, ...)
{
  cat("Variance function:\n")
  cat(" Structure: fixed weights\n")
  cat(paste(" Formula:", deparse(formula(x)),"\n"))
  invisible(x)
}

summary.varFixed <-
  function(object, structName, ...)
{
  class(object) <- "summary.varFixed"
  object
}

###*# varFIdent - equal variances per stratum structure

####* Constructor

varIdent <-
  function(value = numeric(0), form = ~ 1, fixed = NULL)
{
  if (is.null(getGroupsFormula(form))) { # constant value
    value <- numeric(0)
    attr(value, "fixed") <- NULL	# nothing to estimate
  } else {
    if ((lv <- length(value)) > 0) {		# initialized
      if (is.null(grpNames <- names(value)) && (lv > 1)) {
	stop("initial values must have group names in 'varIdent'")
      }
      value <- unlist(value)		# may be a list with names
      if (any(value <= 0)) {
	stop("initial values for 'varIdent' must be > 0")
      }
      value <- log(value)               # unconstrained form
    } else grpNames <- NULL
    attr(value, "groupNames") <- grpNames
    if (!is.null(fixed)) {
       fix <- attr(value, "fixed") <- log(unlist(fixed))
      if (is.null(fixNames <- names(fix))) {
	stop("fixed parameters must have names in 'varIdent'")
      }
      if (!is.null(attr(value, "groupNames"))) {
	attr(value, "groupNames") <- c(attr(value, "groupNames"), fixNames)
      }
    }
  }
  attr(value, "formula") <- asOneSidedFormula(form)
  class(value) <- c("varIdent", "varFunc")
  value
}

###*# Methods for standard generics

coef.varIdent <-
  function(object, unconstrained = TRUE, allCoef = FALSE, ...)
{
  if (!is.null(getGroupsFormula(object)) &&
      !is.null( wPar <- attr(object, "whichFix"))) {
    ## different groups variances
    if (unconstrained && !allCoef) {
      return(as.vector(object))
    }
    val <- double(length(wPar))
    if (any(wPar)) {
      val[wPar] <- attr(object, "fixed")
    }
    if (any(!wPar)) {
      val[!wPar] <- as.vector(object)
    }
    if (!unconstrained) {
      val <- c(1, exp(val))
      names(val) <- attr(object, "groupNames")
      if (!allCoef) {
	val <- val[c(FALSE, !attr(object, "whichFix"))]
      }
    }
    val
  } else {
    numeric(0)
  }
}

"coef<-.varIdent" <-
  function(object, ..., value)
{
  if (!(is.null(grps <- getGroups(object)) ||
       all(attr(object, "whichFix")))) {
    ## different group variances & varying parameters
    value <- as.numeric(value)
    nGroups <- length(attr(object, "groupNames"))
#    if (nGroups == 0) {
#      stop("Cannot assign parameters of uninitialized varIdent object")
#    }
    ## fix from PR#9831
    nFixed  <- sum(as.numeric(attr(object,"whichFix")))
    if (length(value) != nGroups - nFixed - 1) {
      stop("cannot change the length of the \"varIdent\" parameter after initialization")
    }
    object[] <- value
    natPar <- coef(object, FALSE, allCoef = TRUE)
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <- 1/natPar[grps]))
  }
  object
}

Initialize.varIdent <-
  function(object, data, ...)
{
  if (!is.null(form <- formula(object)) &&
      !is.null(grpForm <- getGroupsFormula(form))) {
    if (length(coef(object)) > 0) { # initialized - nothing to do
      return(object)
    }
    strat <- attr(object, "groups") <-
      as.character(getGroups(data, form,
                             level = length(splitFormula(grpForm, sep = "*")),
                             sep = "*"))
    if (length((uStrat <- unique(strat))) == 1) {
      ## equal variances structure
      return(Initialize(varIdent(), data))
    }
    if (!is.null(fix <- attr(object, "fixed"))) {
      fixNames <- names(fix)
      if (any(is.na(match(fixNames, uStrat)))) {
	stop("fixed parameter names in 'varIdent' must be a subset of group names")
      }
      uStratVar <- uStrat[is.na(match(uStrat, fixNames))] # varying strata
      uStrat <- c(uStratVar, fixNames)
    } else {				# nothing fixed
      uStratVar <- uStrat
    }
    if ((nStratVar <- length(uStratVar)) == 0) {
      stop("cannot fix variances in all groups")
    }
    if (nStratVar > 1) {
      if (length(object) <= 1) {
	## repeat for all groups
	oldAttr <- attributes(object)
	if (length(object) > 0) {		# initialized
	  object <- rep(as.vector(object), nStratVar - 1)
	} else {			# uninitialized
	  object <- rep(0, nStratVar - 1)
	}
	attributes(object) <- oldAttr
	attr(object, "groupNames") <- uStrat
      } else {
	if (length(as.vector(object)) != (len <- (nStratVar - 1))) {
            stop(gettextf("initial value for \"varIdent\" should be of length %d",
                          len), domain = NA)
	}
	if (!is.null(stN <- attr(object, "groupNames"))) {
	  missStrat <- uStrat[is.na(match(uStrat, stN))]
	  if (length(missStrat) != 1) {
	    stop("names of starting value for \"varIdent\" object must contain all but one of the stratum levels")
	  }
	  stN <-  c(missStrat, stN)
	  if ((length(stN) != length(uStrat)) ||
	      any(sort(stN) != sort(uStrat))) {
	    stop("nonexistent group names for initial values in 'varIdent'")
	  }
	  attr(object, "groupNames") <- stN
	} else {
	  attr(object, "groupNames") <- uStrat
	}
      }
    } else {				# fixed for all but one strata
      oldAttr <- attributes(object)
      object <- numeric(0)
      attributes(object) <- oldAttr
      attr(object, "groupNames") <- uStrat
    }
    attr(object, "whichFix") <-
      !is.na(match(attr(object, "groupNames")[-1], names(fix)))
    if (all(attr(object, "whichFix"))) {
      if (all(attr(object, "fixed") == 0)) {
	## equal variances structure
	return(Initialize(varIdent(), data))
      } else {
	oldAttr <- attributes(object)
	object <- numeric(0)
	attributes(object) <- oldAttr
      }
    }
    ## initializing weights and logDet
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <-
              1/coef(object, FALSE, allCoef = TRUE)[strat]))
    object
  } else {				# no strata
    attr(object, "whichFix") <- TRUE
    NextMethod()
  }
}

needUpdate.varIdent <-
  function(object) FALSE

recalc.varIdent <-
  function(object, conLin, ...)
{
  if (is.null(formula(object))) conLin else NextMethod()
}

summary.varIdent <-
  function(object,
	   structName = if (is.null(formula(object))) "Constant variance"
	                else "Different standard deviations per stratum",
           ...)
{
    summary.varFunc(object, structName)
}


###*# varPower - power of variance covariate variance structure

####* Constructor

varPower <-
  function(value = numeric(0), form = ~ fitted(.), fixed = NULL)
{
  value <- unlist(value)		# may be given as a list
  fixed <- attr(value, "fixed") <- unlist(fixed)
  attr(value, "formula") <- form <- asOneSidedFormula(form)
  if (length(all.vars(getCovariateFormula(form))) == 0) {
    stop("'form' must have a covariate")
  }
  if (!is.null(getGroupsFormula(form))) {
    if (is.null(grpNames <- names(value)) && (length(value) > 1)) {
      stop("initial values must have group names in 'varPower'")
    }
    if (!is.null(fixed)) {
      if (is.null(names(fixed))) {
	stop("fixed parameters must have group names in 'varPower'")
      }
    }
    attr(value, "groupNames") <- c(grpNames, names(fixed))
  } else {                              # single parameter
    attr(value, "whichFix") <- !is.null(fixed)
  }
  class(value) <- c("varPower", "varFunc")
  value
}

###*# Methods for standard generics

coef.varPower <-
  function(object, unconstrained = TRUE, allCoef = FALSE, ...)
{
  if (((length(object) == 0) &&
       (!allCoef || is.null(attr(object, "fixed")))) ||
      is.null(wPar <- attr(object, "whichFix"))) {
    ## uninitialized
    return(numeric(0))
  }
  val <- double(length(wPar))
  if (any(wPar)) {
    val[wPar] <- attr(object, "fixed")
  }
  if (any(!wPar)) {
    val[!wPar] <- as.vector(object)
  }
  if (!is.null(getGroupsFormula(object))) {
    ##different values per group
    names(val) <- attr(object, "groupNames")
  } else {
    names(val) <- "power"
  }
  if (!allCoef) {
    val <- val[!wPar]
  }
  val
}

"coef<-.varPower" <-
  function(object, ..., value)
{
  if ((len <- length(object)) > 0) {		# varying parameters
    value <- as.numeric(value)
    if (length(value) != len) {
      stop("cannot change the length of the \"varStruct\" parameter after initialization")
    }
    object[] <- value
    aux <- coef(object, FALSE, allCoef = TRUE)
    if (!is.null(grps <- getGroups(object))) {
      aux <- aux[grps]
    }
    covariateObj <- getCovariate(object)
    if(is.null(covariateObj)) covariateObj <- NA
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <- abs(covariateObj)^(-aux)))
  } else {
    stop("cannot change coefficients before initialization or when all parameters are fixed")
  }
  object
}

Initialize.varPower <-
  function(object, data, ...)
{
  form <- formula(object)
  if (all(!is.na(match(all.vars(getCovariateFormula(form)), names(data))))) {
    ## can evaluate covariate on data
    attr(object, "needUpdate") <- FALSE
    attr(object, "covariate") <- getCovariate(data, form)
  } else {
    attr(object, "needUpdate") <- TRUE
  }
  if (!is.null(grpForm <- getGroupsFormula(form))) {
    strat <- as.character(getGroups(data, form,
                            level = length(splitFormula(grpForm, sep = "*")),
                            sep = "*"))
    uStrat <- unique(strat)
    if (length(uStrat) > 1) {		# multi-groups
      attr(object, "groups") <- strat
      if (!is.null(attr(object, "fixed"))) {
	fixNames <- names(attr(object, "fixed"))
	if (is.null(fixNames)) {
	  stop("fixed parameters must have group names")
	}
	if (any(is.na(match(fixNames, uStrat)))) {
	  stop("mismatch between group names and fixed values names")
	}
      } else {
	fixNames <- NULL
      }
      uStratVar <- uStrat[is.na(match(uStrat, fixNames))]
      nStratVar <- length(uStratVar)
      attr(object, "whichFix") <- !is.na(match(uStrat, fixNames))
      if (nStratVar > 0) {
	if (length(object) <= 1) {
	  ## repeat for all groups
	  names(object) <- NULL
	  oldAttr <- attributes(object)
	  if (length(object) > 0) {
	    object <- rep(as.vector(object), nStratVar)
	  } else {
	    object <- rep(0, nStratVar)
	  }
	  attributes(object) <- oldAttr
	  attr(object, "groupNames") <- uStrat
	  names(object) <- uStratVar
	} else {
	  if (length(as.vector(object)) != nStratVar) {
              stop(gettextf("initial value for \"varPower\" should be of length %d",
                            nStratVar), domain = NA)
	  }
	  stN <- attr(object, "groupNames") # must have names
	  if (length(stN) != length(uStrat) ||
	      any(sort(stN) != sort(uStrat))) {
	    stop("nonexistent group names for initial values in \"varPower\"")
	  }
	}
      } else {				# all parameters are fixed
	if (all(attr(object, "fixed") == 0)) {
	  ## equal variances structure
	  return(Initialize(varIdent(), data))
	} else {
	  oldAttr <- attributes(object)
	  object <- numeric(0)
	  attributes(object) <- oldAttr
	  attr(object, "groupNames") <- uStrat
	}
      }
    } else {                            # single stratum
      attr(object, "formula") <- getCovariateFormula(formula(object))
      attr(object, "whichFix") <- !is.null(attr(object, "fixed"))
    }
  }
  if (is.null(getGroupsFormula(object))) {
    ## single stratum
    if (attr(object, "whichFix")) {
      if (attr(object, "fixed") == 0) {
        ## equal variances structure
        return(Initialize(varIdent(), data))
      } else {				# fixed power
        oldAttr <- attributes(object)
        object <- numeric(0)
        attributes(object) <- oldAttr
      }
    } else {
      len <- length(as.vector(object))
      if (len == 0) {			# uninitialized
        oldAttr <- attributes(object)
        object <- 0
        attributes(object) <- oldAttr
      } else if (len > 1) {
        stop("initial value for \"varPower\" should be of length 1")
      }
    }
  }
  if (!is.null(covar <- getCovariate(object))) {
    natPar <- coef(object, allCoef = TRUE)
    if (!is.null(grps <- getGroups(object))) {
      natPar <- natPar[grps]
    }
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <- abs(covar^(-natPar))))
    object
  } else {
    NextMethod()
  }
}

summary.varPower <-
  function(object, structName = "Power of variance covariate", ...)
{
  if (!is.null(getGroupsFormula(object))) {
    structName <- paste(structName, " different strata", sep = ",")
  }
  summary.varFunc(object, structName)
}

update.varPower <-
  function(object, data, ...)
{
  val <- NextMethod()
  if (length(val) == 0) {		# chance to update weights
    aux <- coef(val, allCoef = TRUE)
    if (!is.null(grps <- getGroups(val))) {
      aux <- aux[grps]
    }
    attr(val, "logLik") <-
      sum(log(attr(val, "weights") <- abs(getCovariate(val))^(-aux)))
  }
  val
}

###*# varExp - exponential of variance covariate variance structure

####* Constructor

varExp <-
  function(value = numeric(0), form = ~ fitted(.), fixed = NULL)
{
  value <- unlist(value)		# may be given as a list
  fixed <- attr(value, "fixed") <- unlist(fixed)
  attr(value, "formula") <- form <- asOneSidedFormula(form)
  if (length(all.vars(getCovariateFormula(form))) == 0) {
    stop("'form' must have a covariate")
  }
  if (!is.null(getGroupsFormula(form))) {
    if (is.null(grpNames <- names(value)) && (length(value) > 1)) {
      stop("initial values must have group names in 'varExp'")
    }
    if (!is.null(fixed)) {
      if (is.null(names(fixed))) {
	stop("fixed parameters must have group names in 'varExp'")
      }
    }
    attr(value, "groupNames") <- c(grpNames, names(fixed))
  } else {
    attr(value, "whichFix") <- !is.null(fixed)
  }
  class(value) <- c("varExp", "varFunc")
  value
}

###*# Methods for standard generics

coef.varExp <-
  function(object, unconstrained = TRUE, allCoef = FALSE, ...)
{
  if (((length(object) == 0) &&
       (!allCoef || is.null(attr(object, "fixed")))) ||
      is.null( wPar <- attr(object, "whichFix"))) {
    return(numeric(0))
  }
  val <- double(length(wPar))
  if (any(wPar)) {
    val[wPar] <- attr(object, "fixed")
  }
  if (any(!wPar)) {
    val[!wPar] <- as.vector(object)
  }
  if (!is.null(getGroupsFormula(object))) {
    ##different values per group
    names(val) <- attr(object, "groupNames")
  } else {
    names(val) <- "expon"
  }
  if (!allCoef) {
    val <- val[!wPar]
  }
  val
}

"coef<-.varExp" <-
  function(object, ..., value)
{
  if (length(object) > 0) {		# varying parameters
    value <- as.numeric(value)
    if (length(value) != length(object)) {
      stop("cannot change the length of the \"varExp\" parameter after initialization")
    }
    object[] <- value
    aux <- coef(object, FALSE, allCoef = TRUE)
    if (!is.null(grps <- getGroups(object))) {
      aux <- aux[grps]
    }
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <- exp(-aux * getCovariate(object))))
  } else {
    stop("cannot change coefficients before initialization or when all parameters are fixed")
  }
  object
}

Initialize.varExp <-
  function(object, data, ...)
{
  form <- formula(object)
  if (all(!is.na(match(all.vars(getCovariateFormula(form)), names(data))))) {
    ## can evaluate covariate on data
    attr(object, "needUpdate") <- FALSE
    attr(object, "covariate") <- getCovariate(data, form)
  } else {
    attr(object, "needUpdate") <- TRUE
  }
  if (!is.null(grpForm <- getGroupsFormula(form))) {
    strat <- as.character(getGroups(data, form,
                            level = length(splitFormula(grpForm, sep = "*")),
                            sep = "*"))
    uStrat <- unique(strat)
    if (length(uStrat) > 1) {		# multi-groups
      attr(object, "groups") <- strat
      if (!is.null(attr(object, "fixed"))) {
	fixNames <- names(attr(object, "fixed"))
	if (is.null(fixNames)) {
	  stop("fixed parameters must have group names")
	}
	if (any(is.na(match(fixNames, uStrat)))) {
	  stop("mismatch between group names and fixed values names")
	}
      } else {
	fixNames <- NULL
      }
      uStratVar <- uStrat[is.na(match(uStrat, fixNames))]
      nStratVar <- length(uStratVar)
      attr(object, "whichFix") <- !is.na(match(uStrat, fixNames))
      if (nStratVar > 0) {
	if (length(object) <= 1) {
	  ## repeat for all groups
	  names(object) <- NULL
	  oldAttr <- attributes(object)
	  if (length(object) > 0) {
	    object <- rep(as.vector(object), nStratVar)
	  } else {
	    object <- rep(0, nStratVar)
	  }
	  attributes(object) <- oldAttr
	  attr(object, "groupNames") <- uStrat
	  names(object) <- uStratVar
	} else {
	  if (length(as.vector(object)) != nStratVar) {
              stop(gettextf("initial value for \"varExp\" should be of length %d",
                            nStratVar), domain = NA)
	  }
	  stN <- attr(object, "groupNames") #must have names
	  if ((length(stN) != length(uStrat)) ||
	      any(sort(stN) != sort(uStrat))) {
	    stop("nonexistent group names for initial values in \"varExp\"")
	  }
	}
      } else {
	if (all(attr(object, "fixed") == 0)) {
	  ## equal variances structure
	  return(Initialize(varIdent(), data))
	} else {
	  oldAttr <- attributes(object)
	  object <- numeric(0)
	  attributes(object) <- oldAttr
	  attr(object, "groupNames") <- uStrat
	}
      }
    } else {                            # single stratum
      attr(object, "formula") <- getCovariateFormula(formula(object))
      attr(object, "whichFix") <- !is.null(attr(object, "fixed"))
    }
  }
  if (is.null(getGroupsFormula(object))) {
    ## single stratum
    if (attr(object, "whichFix")) {
      if (!attr(object, "fixed")) {
        ## equal variances structure
        return(Initialize(varIdent(), data))
      } else {
        oldAttr <- attributes(object)
        object <- numeric(0)
        attributes(object) <- oldAttr
      }
    } else {
      len <- length(as.vector(object))
      if (len == 0) {			# uninitialized
        oldAttr <- attributes(object)
        object <- 0
        attributes(object) <- oldAttr
      } else if (len > 1) {
        stop("initial value for \"varExp\" should be of length 1")
      }
    }
  }
  if (!is.null(covar <- getCovariate(object))) {
    natPar <- coef(object, allCoef = TRUE)
    if (!is.null(grps <- getGroups(object))) {
      natPar <- natPar[grps]
    }
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <- exp(-natPar * covar)))
    object
  } else {
    NextMethod()
  }
}


summary.varExp <-
  function(object, structName = "Exponential of variance covariate", ...)
{
  if (!is.null(getGroupsFormula(object))) {
    structName <- paste(structName, " different strata", sep = ",")
  }
  summary.varFunc(object, structName)
}

update.varExp <-
  function(object, data, ...)
{
  val <- NextMethod()
  if (length(val) == 0) {		# chance to update weights
    aux <- coef(val, allCoef = TRUE)
    if (!is.null(grps <- getGroups(val))) {
      aux <- aux[grps]
    }
    attr(val, "logLik") <-
      sum(log(attr(val, "weights") <- exp(-aux * getCovariate(val))))
  }
  val
}

###*# varConstPower - Constant plus power of covariance function
###*#               variance structure

####* Constructor

varConstPower <-
  ## Constructor for the varConstPower class
  function(const = numeric(0), power = numeric(0),
	   form = ~ fitted(.), fixed = NULL)
{
  CPconstr <- function(val, form, nam) {
    if ((lv <- length(val)) == 0) return(val)
    if (lv > 2) {
      stop(gettextf("%s can have at most two components", nam), domain = NA)
    }
    if (is.null(nv <- names(val))) {
      names(val) <- c("const", "power")[1:lv]
    } else {
      if (any(is.na(match(nv, c("const", "power"))))) {
        stop(gettextf("%s can only have names \"const\" and \"power\"", nam),
             domain = NA)
      }
    }
    nv <- names(val)
    if (data.class(val) == "list") {
      val <- lapply(val, unlist)
      grpNames <- unique(unlist(lapply(val, names)))
    } else {				# must be a vector or a scalar
      if (!is.numeric(val)) {
        stop(gettextf("%s can only be a list or numeric", nam), domain = NA)
     }
      val <- as.list(val)
      names(val) <- nv
      grpNames <- NULL
    }
    if (!is.null(getGroupsFormula(form))) {
      if (any(unlist(lapply(val, function(el) {
	(length(el) > 1) && is.null(names(el))
      })))) {
        stop(gettextf("%s must have group names in 'varConstPower'", nam),
             domain = NA)
      }
      attr(val, "groupNames") <- grpNames
    }
    if (length(val$const) > 0) {
      if (any(val$const <= 0)) {
	stop("constant in \"varConstPower\" structure must be > 0")
      }
      val$const <- log(val$const)
    }
    list(const = val$const, power = val$power)
  }
  value <- list(const = const, power = power)
  form <- asOneSidedFormula(form)
  if (length(all.vars(getCovariateFormula(form))) == 0) {
    stop("'form' must have a covariate")
  }
  ## initial value may be given as a vector or list. If groups are
  ## present and different initial values are given for each group, then
  ## it must be a list with components "const" and/or "power"
  value <- CPconstr(value, form, "Value")
  fixed <- CPconstr(fixed, form, "Fixed")
  attr(value, "formula") <- form
  attr(value, "groupNames") <-
    unique(c(attr(value, "groupNames"),
	   attr(attr(value[["const"]], "fixed"), "groupNames"),
	   attr(attr(value[["power"]], "fixed"), "groupNames")))
  for (i in names(fixed)) {
    attr(value[[i]], "fixed") <- c(fixed[[i]])
  }
  if (is.null(getGroupsFormula(form))) {   # no groups
    whichFix <- array(FALSE, c(2,1), list(c("const", "power"), NULL))
    whichFix[,1] <- unlist(lapply(value,
                                  function(el) !is.null(attr(el, "fixed"))))
    attr(value, "whichFix") <- whichFix
  }
  class(value) <- c("varConstPower", "varFunc")
  value
}

###*# Methods for standard generics

coef.varConstPower <-
    function (object, unconstrained = TRUE, allCoef = FALSE, ...)
{
    wPar <- attr(object, "whichFix")
    nonInit <- !unlist(lapply(object, length))
    nonInit <- is.null(wPar) || (any(nonInit) && !all(c(wPar[nonInit,
                                                             ])))
    if (nonInit || (!allCoef && (length(unlist(object)) == 0))) {
        return(numeric(0))
    }
    val <- array(0, dim(wPar), dimnames(wPar))
    for (i in names(object)) {
        if (any(wPar[i, ])) {
            val[i, wPar[i, ]] <- attr(object[[i]], "fixed")
        }
        if (any(!wPar[i, ])) {
            val[i, !wPar[i, ]] <- c(object[[i]])
        }
    }
    if (!unconstrained) {
        val[1, ] <- exp(val[1, ])
    }
    if (!allCoef) {
        val <- list(const = if (!all(wPar[1, ])) val[1, !wPar[1,
                    ]] else NULL, power = if (!all(wPar[2, ])) val[2,
                                  !wPar[2, ]] else NULL)
        val <- lapply(val, function(el) if(length(el) == 1) as.vector(el)
        else el)
        val <- unlist(val[!unlist(lapply(val, is.null))])
    }
    else {
        val <- val[, 1:ncol(val)]
    }
    val
}

"coef<-.varConstPower" <-
  function(object, ..., value)
{
  if (length(unlist(object)) > 0) {	# varying parameters
    value <- as.numeric(value)
    if (length(value) != length(unlist(object))) {
      stop("cannot change the length of the parameter after initialization")
    }
    start <- 0
    for(i in names(object)) {
      if (aux <- length(object[[i]])) {
	object[[i]][] <- value[start + (1:aux)]
	start <- start + aux
      }
    }
    natPar <- as.matrix(coef(object, FALSE, allCoef = TRUE))
    if (!is.null(grps <- getGroups(object))) {
      natPar <- natPar[, grps]
    }
    attr(object, "logLik") <-
      sum(log(attr(object, "weights") <-
	      1/(natPar[1,] + abs(getCovariate(object))^natPar[2,])))
  } else {
    stop("cannot change coefficients before initialization or when all parameters are fixed")
  }
  object
}

Initialize.varConstPower <-
  function(object, data, ...)
{
  form <- formula(object)
  if (all(!is.na(match(all.vars(getCovariateFormula(form)), names(data))))) {
    ## can evaluate covariate on data
    attr(object, "needUpdate") <- FALSE
    attr(object, "covariate") <- getCovariate(data, form)
  } else {
    attr(object, "needUpdate") <- TRUE
  }
  dfltCoef <- c(const = log(0.1), power = 0)
  if (!is.null(grpForm <- getGroupsFormula(form))) {
    strat <- as.character(getGroups(data, form,
                            level = length(splitFormula(grpForm, sep = "*")),
                            sep = "*"))
    uStrat <- unique(strat)
    whichFix <- array(FALSE, c(2, length(uStrat)),
		      list(c("const", "power"), uStrat))
    if (length(uStrat) > 1) {		# multi-groups
      attr(object, "groups") <- strat
      for(i in names(object)) {
	if (!is.null(attr(object[[i]], "fixed"))) {
	  fixNames <- names(attr(object[[i]], "fixed"))
	  if (is.null(fixNames)) {
	    stop("fixed parameters must have group names")
	  }
	  if (any(is.na(match(fixNames, uStrat)))) {
	    stop("mismatch between group names and fixed values names")
	  }
	} else {
	  fixNames <- NULL
	}
	uStratVar <- uStrat[is.na(match(uStrat, fixNames))]
	nStratVar <- length(uStratVar)
	whichFix[i,] <- !is.na(match(uStrat, fixNames))
	if (nStratVar > 0) {
	  if (length(object[[i]]) <= 1) {
	    ## repeat for all groups
	    names(object[[i]]) <- NULL
	    oldAttr <- attributes(object[[i]])
	    if (length(object[[i]]) > 0) {
	      object[[i]] <- rep(as.vector(object[[i]]), nStratVar)
	    } else {
	      object[[i]] <- rep(dfltCoef[i], nStratVar)
	    }
	    attributes(object[[i]]) <- oldAttr
	    names(object[[i]]) <- uStratVar
	  } else {
	    if (length(as.vector(object[[i]])) != nStratVar) {
                stop(gettext("initial value should be of length %d",
                             nStratVar), domain = NA)
	    }
	    stN <- names(object[[i]]) # must have names
	    if ((length(stN) != length(uStratVar)) ||
		any(sort(stN) != sort(uStratVar))) {
	      stop("nonexistent group names for initial values")
	    }
	  }
	}
      }
      if (all(whichFix) &&
	  all(attr(object[["const"]], "fixed") == 0) &&
	  all(attr(object[["power"]], "fixed") == 0)) {
	## equal variances structure
	return(Initialize(varIdent(), data))
      }
      for(i in names(object)) {
	if (all(whichFix[i,])) {
	  oldAttr <- attributes(object[[i]])
	  object[[i]] <- numeric(0)
	  attributes(object[[i]]) <- oldAttr
	}
      }
      attr(object, "whichFix") <- whichFix
      attr(object, "groupNames") <- uStrat
      return(NextMethod())
    }
  }
  ## single stratum
  whichFix <- attr(object, "whichFix")
  if (all(whichFix) &&
      !any(unlist(lapply(object, function(el) attr(el, "fixed"))))) {
    ## equal variances structure
    return(Initialize(varIdent(), data))
  }
  for(i in names(object)) {
    if (all(whichFix[i,])) {
      oldAttr <- attributes(object[[i]])
      object[[i]] <- numeric(0)
      attributes(object[[i]]) <- oldAttr
    } else {
      if (length(object[[i]]) == 0) {
	object[[i]] <- dfltCoef[i]
      }
    }
  }
  aux <- 2 - sum(whichFix[,1])
  if (length(as.vector(unlist(object))) != aux) {
      stop(gettext("initial value should be of length %d", aux), domain = NA)
  }
  NextMethod()
}

summary.varConstPower <-
  function(object, structName = "Constant plus power of variance covariate",
           ...)
{
  if (!is.null(getGroupsFormula(object))) {
    structName <- paste(structName, " different strata", sep = ",")
  }
  summary.varFunc(object, structName)
}

update.varConstPower <-
  function(object, data, ...)
{
  val <- NextMethod()
  if (length(unlist(val)) == 0) {	# chance to update weights
    aux <- as.matrix(coef(val, FALSE, allCoef = TRUE))
    if (!is.null(grps <- getGroups(val))) {
      aux <- aux[, grps]
    }
    attr(val, "logLik") <-
      sum(log(attr(val, "weights") <-
	      1/(aux[1,] + abs(getCovariate(val))^aux[2,])))
  }
  val
}

###*# varFComb - combination of variance function structures

####* Constructor

varComb <-
  ## constructor for the varComb class
  function(...)
{
  val <- list(...)
  if (!all(unlist(lapply(val, inherits, "varFunc")))) {
    stop("all arguments to 'varComb' must be of class \"varFunc\".")
  }
  if (is.null(names(val))) {
    names(val) <- LETTERS[1:length(val)]
  }
  class(val) <- c("varComb", "varFunc")
  val
}

####* Methods for local generics


varWeights.varComb <-
  function(object)
{
  apply(as.data.frame(lapply(object, varWeights)), 1, prod)
}

###*# Methods for standard generics

coef.varComb <-
  function(object, unconstrained = TRUE, allCoef = FALSE, ...)
{
  unlist(lapply(object, coef, unconstrained, allCoef))
}

"coef<-.varComb" <-
  function(object, ..., value)
{
  plen <- attr(object, "plen")
  if ((len <- sum(plen)) > 0) {		# varying parameters
    if (length(value) != len) {
      stop("cannot change parameter length of initialized \"varComb\" object")
    }
    start <- 0
    for (i in seq_along(object)) {
      if (plen[i] > 0) {
	coef(object[[i]]) <- value[start + (1:plen[i])]
	start <- start + plen[i]
      }
    }
  }
  object
}

formula.varComb <-
  function(x, ...) lapply(x, formula)

Initialize.varComb <-
  function(object, data, ...)
{
  val <- lapply(object, Initialize, data)
  attr(val, "plen") <- unlist(lapply(val, function(el) length(coef(el))))
  class(val) <- c("varComb", "varFunc")
  val
}

logLik.varComb <-
  function(object, ...)
{
    lls <- lapply(object, logLik)
    val <- sum(unlist(lls))
    attr(val, "df") <- sum(unlist(lapply(lls, attr, "df")))
    class(val) <- "logLik"
    val
}

needUpdate.varComb <-
  function(object) any(unlist(lapply(object, needUpdate)))

print.varComb <-
  function(x, ...)
{
  cat("Combination of:\n")
  lapply(x, print)
  invisible(x)
}

print.summary.varComb <-
  function(x, ...)
{
  cat(attr(x, "structName"),"\n")
  lapply(x, print, FALSE)
  invisible(x)
}

summary.varComb <-
  function(object, structName = "Combination of variance functions:", ...)
{
  object[] <- lapply(object, summary)
  attr(object, "structName") <- structName
  class(object) <- c("summary.varComb", class(object))
  object
}

update.varComb <-
  function(object, data, ...)
{
  object[] <- lapply(object, update, data)
  object
}
