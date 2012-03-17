###       Functions that are used in several parts of the nlme library
###                 but do not belong to any specific part
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

allCoef <-
  ## Combines different coefficient vectors into one vector, keeping track
  ## of which coefficients came from which object
  function(..., extract = coef)
{
  dots <- list(...)
  theta <- lapply(dots, extract)
  len <- unlist(lapply(theta, length))
  num <- seq_along(len)
  if (sum(len) > 0) {
    which <- outer(rep(num, len), num, "==")
  } else {
    which <- array(FALSE, c(1, length(len)))
  }
  cnames <- unlist(as.list(sys.call()[-1]))
  dimnames(which) <- list(NULL, cnames[cnames != substitute(extract)])
  theta <- unlist(theta)
  attr(theta, "map") <- which
  theta
}

allVarsRec <-
  ## Recursive version of all.vars
  function(object)
{
  if (is.list(object)) {
    unlist(lapply(object, allVarsRec))
  } else {
    all.vars(object)
  }
}

asOneFormula <-
  ## Constructs a linear formula with all the variables used in a
  ## list of formulas, except for the names in omit
  function(..., omit = c(".", "pi"))
{
  names <- unique(allVarsRec((list(...))))
  names <- names[is.na(match(names, omit))]
  if (length(names)) {
    eval(parse(text = paste("~", paste(names, collapse = "+")))[[1]])
  } else NULL
}

compareFits <-
  ## compares coeffificients from different fitted objects
  function(object1, object2, which = 1:ncol(object1))
{
  dn1 <- dimnames(object1)
  dn2 <- dimnames(object2)
  aux <- rep(NA, length(dn1[[1]]))
  if (any(aux1 <- is.na(match(dn2[[2]], dn1[[2]])))) {
    object1[,dn2[[2]][aux1]] <- aux
  }
  if (any(aux1 <- is.na(match(dn1[[2]], dn2[[2]])))) {
    object2[,dn1[[2]][aux1]] <- aux
  }
  dn1 <- dimnames(object1)
  c1 <- deparse(substitute(object1))
  c2 <- deparse(substitute(object2))
  if (any(sort(dn1[[1]]) != sort(dn2[[1]]))) {
    stop("Objects must have coefficients with same row names")
  }
  ## putting object2 in same order
  object2 <- object2[dn1[[1]], dn1[[2]], drop = FALSE]
  object1 <- object1[, which, drop = FALSE]
  object2 <- object2[, which, drop = FALSE]
  dn1 <- dimnames(object1)
  dm1 <- dim(object1)
  out <- array(0, c(dm1[1], 2, dm1[2]), list(dn1[[1]], c(c1,c2), dn1[[2]]))
  for(i in dn1[[2]]) {
    out[,,i] <- cbind(object1[[i]], object2[[i]])
  }
  class(out) <- "compareFits"
  out
}

fdHess <- function(pars, fun, ..., .relStep = (.Machine$double.eps)^(1/3),
                   minAbsPar = 0)
  ## Use a Koschal design to establish a second order model for the response
{
  pars <- as.numeric(pars)
  npar <- length(pars)
  incr <- ifelse( abs(pars) <= minAbsPar, minAbsPar * .relStep,
                 abs(pars) * .relStep )
  baseInd <- diag(npar)
  frac <- c(1, incr, incr^2)
  cols <- list(0, baseInd, -baseInd)
  for ( i in seq_along(pars)[ -npar ] ) {
    cols <- c( cols, list( baseInd[ , i ] + baseInd[ , -(1:i) ] ) )
    frac <- c( frac, incr[ i ] * incr[ -(1:i) ] )
  }
  indMat <- do.call( "cbind", cols)
  shifted <- pars + incr * indMat
  indMat <- t(indMat)
  Xcols <- list(1, indMat, indMat^2)
  for ( i in seq_along(pars)[ - npar ] ) {
    Xcols <- c( Xcols, list( indMat[ , i ] * indMat[ , -(1:i) ] ) )
  }
  coefs <- solve( do.call( "cbind", Xcols ) , apply(shifted, 2, fun, ...) )/frac
  Hess <- diag( coefs[ 1 + npar + seq_along(pars) ], ncol = npar )
  Hess[ row( Hess ) > col ( Hess ) ] <- coefs[ -(1:(1 + 2 * npar)) ]
  list( mean = coefs[ 1 ], gradient = coefs[ 1 + seq_along(pars) ],
       Hessian = ( Hess + t(Hess) ) )
}

gapply <-
  ## Apply a function to the subframes of a data.frame
  ## If "apply" were generic, this would be the method for groupedData
  function(object, which, FUN, form = formula(object), level,
           groups = getGroups(object, form, level), ...)
{
  if (!inherits(object, "data.frame")) {
    stop("Object must inherit from data.frame")
  }
  ## Apply a function to the subframes of a groupedData object
  if (missing(groups)) {                # formula and level are required
    if (!inherits(form, "formula")) {
      stop("\"Form\" must be a formula")
    }
    if (is.null(grpForm <- getGroupsFormula(form, asList = TRUE))) {
      ## will use right hand side of form as groups formula
      grpForm <- splitFormula(asOneSidedFormula(form[[length(form)]]))
    }
    if (missing(level)) level <- length(grpForm)
    else if (length(level) != 1) {
      stop("Only one level allowed in gapply")
    }
    groups <- groups                    # forcing evaluation
  }
  if (!missing(which)) {
    switch(mode(which),
           character = {
             wchNot <- is.na(match(which, names(object)))
             if (any(wchNot)) {
               stop(paste(paste(which[wchNot], collapse = ","),
                          "not matched"))
             }
           },
           numeric = {
             if (any(is.na(match(which, 1:ncol(object))))) {
               stop("Which must be between 1 and", ncol(object))
             }
           },
           stop("Which can only be character or integer.")
           )
    object <- object[, which, drop = FALSE]
  }
  val <- lapply(X = split(object, groups), FUN = FUN, ...)
  if (is.atomic(val[[1]]) && length(val[[1]]) == 1) {
    val <- unlist(val)
  }
  val
}

getCovariateFormula <-
  function(object)
{
  ## Return the primary covariate formula as a one sided formula
  form <- formula(object)
  if (!(inherits(form, "formula"))) {
    stop("formula(object) must return a formula")
  }
  form <- form[[length(form)]]
  if (length(form) == 3 && form[[1]] == as.name("|")){ # conditional expression
    form <- form[[2]]
  }
  eval(substitute(~form))
}

getResponseFormula <-
  function(object)
{
  ## Return the response formula as a one sided formula
  form <- formula(object)
  if (!(inherits(form, "formula") && (length(form) == 3))) {
    stop("\"Form\" must be a two sided formula")
  }
  eval(parse(text = paste("~", deparse(form[[2]]))))
}

gsummary <-
  ## Summarize an object according to the levels of a grouping factor
  ##
  function(object, FUN = function(x) mean(x, na.rm = TRUE),
           omitGroupingFactor = FALSE,
	   form = formula(object), level,
	   groups = getGroups(object, form , level),
	   invariantsOnly = FALSE, ...)
{
  if (!inherits(object, "data.frame")) {
    stop("Object must inherit from data.frame")
  }
  if (missing(groups)) {                # formula and level are required
    if (!inherits(form, "formula")) {
      stop("\"Form\" must be a formula")
    }
    if (is.null(grpForm <- getGroupsFormula(form, asList = TRUE))) {
      ## will use right hand side of form as groups formula
      grpForm <- splitFormula(asOneSidedFormula(form[[length(form)]]))
    }
    if (missing(level)) level <- length(grpForm)
    else if (length(level) != 1) {
      stop("Only one level allowed in gsummary")
    }
  }
  gunique <- unique(groups)
  firstInGroup <- match(gunique, groups)
  asFirst <- firstInGroup[match(groups, gunique)]
  value <- as.data.frame(object[firstInGroup, , drop = FALSE])
  row.names(value) <- as.character(gunique)
  value <- value[as.character(sort(gunique)), , drop = FALSE]
  varying <- unlist(lapply(object,
			   function(column, frst) {
			     aux <- as.character(column)
			     any(!identical(aux, aux[frst]))
			   },
			   frst = asFirst))
  if (any(varying) && (!invariantsOnly)) { # varying wanted
    Mode <- function(x) {
      aux <- table(x)
      names(aux)[match(max(aux), aux)]
    }
    if (data.class(FUN) == "function") {	# single function given
      FUN <- list(numeric = FUN, ordered = Mode, factor = Mode)
    } else {
      if (!(is.list(FUN) &&
	   all(sapply(FUN, data.class) == "function"))) {
	stop("FUN can only be a function or a list of functions")
      }
      auxFUN <- list(numeric = mean, ordered = Mode, factor = Mode)
      aux <- names(auxFUN)[is.na(match(names(auxFUN), names(FUN)))]
      if (length(aux) > 0) FUN[aux] <- auxFUN[aux]
    }
    for(nm in names(object)[varying]) {
      ## dClass <- data.class(object[[nm]])
      ## The problem here is that dclass may find an irrelevant class,
      ## e.g. Hmisc's "labelled"
      dClass <- if(is.ordered(object[[nm]])) "ordered" else
	        if(is.factor(object[[nm]])) "factor" else mode(object[[nm]])
      if (dClass == "numeric") {
	value[[nm]] <- as.vector(tapply(object[[nm]], groups, FUN[["numeric"]],...))
      } else {
	value[[nm]] <-
	  as.vector(tapply(as.character(object[[nm]]), groups, FUN[[dClass]]))
        if (inherits(object[,nm], "ordered")) {
          value[[nm]] <- ordered(value[,nm], levels = levels(object[,nm]))[drop = TRUE]
        } else {
          value[[nm]] <- factor(value[,nm], levels = levels(object[,nm]))[drop = TRUE]
        }
      }
    }
  } else {				# invariants only
    value <- value[, !varying, drop = FALSE]
  }
  if (omitGroupingFactor) {
    if (is.null(form)) {
      stop("Cannot omit grouping factor without \"form\"")
    }
    grpForm <- getGroupsFormula(form, asList = TRUE)
    if (missing(level)) level <- length(grpForm)
    grpNames <- names(grpForm)[level]
    whichKeep <- is.na(match(names(value), grpNames))
    if (any(whichKeep)) {
      value <- value[ , whichKeep, drop = FALSE]
    } else {
      return(NULL);
    }
  }
  value
}

pooledSD <-
  function(object)
{
  if (!inherits(object, "lmList")) {
    stop("Object must inherit from class \"lmList\"")
  }
  aux <- apply(sapply(object,
		      function(el) {
			if (is.null(el)) {
			  c(0,0)
			} else {
			  aux <- resid(el)
			  c(sum(aux^2), length(aux) - length(coef(el)))
			}
		      }), 1, sum)
  if (aux[2] == 0) {
    stop("No degrees of freedom for estimating std. dev.")
  }
  val <- sqrt(aux[1]/aux[2])
  attr(val, "df") <- aux[2]
  val
}

splitFormula <-
  ## split, on the nm call, the rhs of a formula into a list of subformulas
  function(form, sep = "/")
{
  if (inherits(form, "formula") ||
      mode(form) == "call" && form[[1]] == as.name("~"))
    return(splitFormula(form[[length(form)]], sep = sep))
  if (mode(form) == "call" && form[[1]] == as.name(sep))
    return(do.call("c", lapply(as.list(form[-1]), splitFormula, sep = sep)))
  if (mode(form) == "(") return(splitFormula(form[[2]], sep = sep))
  if (length(form) < 1) return(NULL)
  list(asOneSidedFormula(form))
}

##*## phenoModel - one-compartment open model with intravenous
##*##   administration and first-order elimination for the Phenobarbital data

phenoModel <-
  function(Subject, time, dose, lCl, lV)
{
  .C(nlme_one_comp_first,
     as.integer(length(time)),
     resp = as.double(dose),
     as.double(cbind(Subject, time, dose, exp(lV), exp(lCl))),
     NAOK = TRUE)$resp
}

##*## quinModel - one-compartment open model with first order
##*##   absorption for the Quinidine data

quinModel <-
  function(Subject, time, conc, dose, interval, lV, lKa, lCl)
{
  .C(nlme_one_comp_open,
     as.integer(length(time)),
     resp = as.double(dose),
     as.double(cbind(Subject, time, conc, dose, interval,
                     exp(lV), exp(lKa), exp(lCl - lV))),
     NAOK = TRUE)$resp
}

LDEsysMat <-
    function(pars, incidence)
{
    tt <- incidence[, "To"]
    ff <- incidence[, "From"]
    pp <- pars[incidence[, "Par"]]
    n <- max(ff, tt)
    val <- array(double(n * n), c(n, n))
    diag(val) <- - tapply(pp, ff, sum)
    val[incidence[tt > 0, c("To", "From"), drop = FALSE]] <- pp[tt > 0]
    val
}

## Local Variables:
## mode:S
## End:

