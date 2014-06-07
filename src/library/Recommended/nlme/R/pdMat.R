###              Classes of positive-definite matrices
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

pdConstruct <-
  ## a virtual constructor for these objects
  function(object, value, form, nam, data, ...) UseMethod("pdConstruct")

pdFactor <-
  function(object) UseMethod("pdFactor")

pdMatrix <-
  ## extractor for the pd, correlation, or square-root factor matrix
  function(object, factor = FALSE) UseMethod("pdMatrix")

##*## pdMat - a virtual class of positive definite matrices

###*#  constructor for the virtual class

pdMat <-
  function(value = numeric(0), form = NULL, nam = NULL,
	   data = sys.frame(sys.parent()), pdClass = "pdSymm")
{
  if (inherits(value, "pdMat")) {	# nothing to construct
    pdClass <- class(value)
  }
  object <- numeric(0)
  class(object) <- unique(c(pdClass, "pdMat"))
  pdConstruct(object, value, form, nam, data)
}

###*# Methods for local generics

corMatrix.pdMat <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot access the matrix of uninitialized objects")
  }
  Var <- pdMatrix(object)
  if (length(unlist(dimnames(Var))) == 0) {
    aux <- paste("V", 1:(Dim(Var)[2]), sep = "")
    dimnames(Var) <- list(aux, aux)
  }
  dd <- dim(Var)
  dn <- dimnames(Var)
  stdDev <- sqrt(diag(Var))
  names(stdDev) <- colnames(Var)
  value <- array(t(Var/stdDev)/stdDev, dd, dn)
  attr(value, "stdDev") <- stdDev
  value
}

pdConstruct.pdMat <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.frame(sys.parent()), ...)
{
  if (inherits(value, "pdMat")) {	# constructing from another pdMat
    if (length(form) == 0) {
      form <- formula(value)
    }
    if (length(nam) == 0) {
      nam <- Names(value)
    }
    if (isInitialized(value)) {
      return(pdConstruct(object, as.matrix(value), form, nam, data))
    } else {
      return(pdConstruct(object, form = form, nam = nam, data = data))
    }
  }
  if (length(value) > 0) {
    if (inherits(value, "formula") || data.class(value) == "call") {
      ## constructing from a formula
      if (!is.null(form)) {
	warning("ignoring argument 'form'")
      }
      form <- formula(value)
      if (length(form) == 3) {          #two-sided case - nlme
        form <- list(form)
      }
    } else if (is.character(value)) {	# constructing from character array
      if (length(nam) > 0) {
	warning("ignoring argument 'nam'")
      }
      nam <- value
    } else if (is.matrix(value)) {	# constructing from a pd matrix
      vdim <- dim(value)
      if (length(vdim) != 2 || diff(vdim) != 0) {
        stop("'value' must be a square matrix")
      }
      if (length(unlist(vnam <- dimnames(value))) > 0) {
        vnam <- unique(unlist(vnam))
        if (length(vnam) != vdim[1]) {
          stop("dimnames of 'value' must match or be NULL")
        }
        dimnames(value) <- list(vnam, vnam)
        if (length(nam) > 0) {          # check consistency
	  if (any(is.na(match(nam, vnam))) || any(is.na(match(vnam, nam)))) {
	    stop("names of 'value' are not consistent with 'nam' argument")
	  }
	  value <- value[nam, nam, drop = FALSE]
	} else {
	  nam <- vnam
	}
      }
      form <- form                      # avoid problems with lazy evaluation
      nam <- nam
      object <- chol((value + t(value))/2) # ensure it is positive-definite
      attr(object, "dimnames") <- NULL
      attr(object, "rank") <- NULL
    } else if (is.numeric(value)) {	# constructing from the parameter
      value <- as.numeric(value)
      attributes(value) <- attributes(object)
      object <- value
    } else if (data.class(value) == "list") {
      ## constructing from a list of two-sided formulae - nlme case
      if (!is.null(form)) {
	warning("ignoring argument 'form'")
      }
      form <- value
    } else {
        stop(gettextf("%s is not a valid object for \"pdMat\"",
                      sQuote(deparse(object))), domain = NA)
    }
  }

  if (!is.null(form)) {
    if (inherits(form, "formula") && length(form) == 3) {#two-sided case - nlme
      form <- list(form)
    }
    if (is.list(form)) {   # list of formulae
      if (any(!unlist(lapply(form,
                             function(el) {
                               inherits(el, "formula") && length(el) == 3
                             })))) {
        stop("all elements of 'form' list must be two-sided formulas")
      }
      val <- list()
      for(i in seq_along(form)) {
        if (is.name(form[[i]][[2]])) {
          val <- c(val, list(form[[i]]))
        } else {
          val <- c(val, eval(parse(text = paste("list(",
            paste(paste(all.vars(form[[i]][[2]]), deparse(form[[i]][[3]]),
                        sep = "~"), collapse=","),")"))))
        }
      }
      form <- val
      class(form) <- "listForm"
      namesForm <- Names(form, data)
    } else {
      if (inherits(form, "formula")) {
        namesForm <- Names(asOneSidedFormula(form), data)
##        namesForm1 <- NULL
      } else {
        stop("'form' can only be a formula or a list of formulae")
      }
    }
    if (length(namesForm) > 0) {
      if (length(nam) == 0) {             # getting names from formula
        nam <- namesForm
      } else {				# checking consistency with names
        if (any(noMatch <- is.na(match(nam, namesForm)))) {
          err <- TRUE
          namCopy <- nam
          indNoMatch <- (1:length(nam))[noMatch]
          if (any(wch1 <- (nchar(nam, "c") > 12))) {
            ## possibly names with .(Intercept) in value
            wch1 <- substring(nam, nchar(nam, "c")-10) == "(Intercept)"
            if (any(wch1)) {
              namCopy[indNoMatch[wch1]] <-
                substring(nam[wch1], 1, nchar(nam[wch1], "c") - 12)
              noMatch[wch1] <- FALSE
              indNoMatch <- indNoMatch[!wch1]  # possibly not matched
            }
          }
          if (sum(noMatch) > 0) {
            ## still no matches - try adding .(Intercept)
            namCopy[indNoMatch] <-
              paste(namCopy[indNoMatch], "(Intercept)", sep = ".")
          }
          ## try matching modified value
          if (!any(is.na(match(namCopy, namesForm)))) {
            err <- FALSE
          }
          if (err) stop("'form' not consistent with 'nam'")
        }
      }
    }
  }

  if (is.matrix(object)) {	# initialized as matrix, check consistency
    if (length(nam) > 0 && (length(nam) != dim(object)[2])) {
      stop("length of 'nam' not consistent with dimensions of initial value")
    }
  }
  attr(object, "formula") <- form
  attr(object, "Dimnames") <- list(nam, nam)
  object
}

pdFactor.pdMat <-
  function(object)
{
  c(qr.R(qr(pdMatrix(object))))
}

pdMatrix.pdMat <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot access the matrix of uninitialized objects")
  }
  if (factor) {
    stop("no default method for extracting the square root of a \"pdMat\" object")
  } else {
    crossprod(pdMatrix(object, factor = TRUE))
  }
}

###*# Methods for standard generics

as.matrix.pdMat <-
  function(x, ...) pdMatrix(x)

coef.pdMat <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained || !isInitialized(object)) {
    as.vector(object)
  } else {
    stop("do not know how to obtain constrained coefficients")
  }
}

"coef<-.pdMat" <-
  function(object, ..., value)
{
  value <- as.numeric(value)
  if (isInitialized(object)) {
    if (length(value) != length(object)) {
      stop("cannot change the length of the parameter after initialization")
    }
  } else {
    return(pdConstruct(object, value))
  }
  class(value) <- class(object)
  attributes(value) <- attributes(object)
  value
}

Dim.pdMat <-
  function(object, ...)
{
  if ((val <- length(Names(object))) > 0) {
    return(c(val, val))
  } else if (isInitialized(object)) {
    return(dim(as.matrix(object)))
  }
  stop("cannot access the number of columns of uninitialized objects without names")
}

formula.pdMat <-
  function(x, asList, ...) eval(attr(x, "formula"))

isInitialized.pdMat <-
  function(object)
{
  length(object) > 0
}

logDet.pdMat <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot extract the log of the determinant from an uninitialized object")
  }
  sum(log(svd(pdMatrix(object, factor = TRUE))$d))
}

"matrix<-.pdMat" <-
  function(object, value)
{
  value <- as.matrix(value)
  ## check for consistency of dimensions when object is initialized
  if (isInitialized(object) && any(dim(value) != Dim(object))) {
    stop("cannot change dimensions on an initialized \"pdMat\" object")
  }
  pdConstruct(object, value)
}

Names.pdMat <-
  function(object, ...)
{
  as.character(attr(object, "Dimnames")[[2]])
}

"Names<-.pdMat" <-
  function(object, ..., value)
{
  if (is.null(value)) {
    attr(object, "Dimnames") <- NULL
    return(object)
  } else {
    value <- as.character(value)
    if (length(dn <- Names(object)) == 0) {
      if (isInitialized(object)) {	# object is initialized without names
	if (length(value) != (aux <- Dim(object)[2])) {
            stop(gettextf("Length of names should be %d", aux), domain = NA)
	}
      }
      attr(object, "Dimnames") <- list(value, value)
      return(object)
    }
    if (length(dn) != length(value)) {
        stop(gettextf("Length of names should be %d", length(dn)), domain = NA)
    }
    err <- FALSE
    if (any(noMatch <- is.na(match(value, dn)))) {
      err <- TRUE
      ## checking nlme case
      valueCopy <- value
      indNoMatch <- (1:length(value))[noMatch]
      nam1 <- value[noMatch]            # no matching names
      if (any(wch1 <- (nchar(nam1, "c") > 12))) {
        ## possibly names with .(Intercept) in value
        wch1 <- substring(nam1, nchar(nam1, "c")-10) == "(Intercept)"
        if (any(wch1)) {
          valueCopy[indNoMatch[wch1]] <-
            substring(nam1[wch1], 1, nchar(nam1[wch1], "c") - 12)
          noMatch[wch1] <- FALSE
          indNoMatch <- indNoMatch[!wch1]  # possibly not matched
        }
      }
      if (sum(noMatch) > 0) {
        ## still no matches - try adding .(Intercept)
        valueCopy[indNoMatch] <-
          paste(valueCopy[indNoMatch], "(Intercept)", sep = ".")
      }
      ## try matching modified value
      indMatch <- match(valueCopy, dn)
      if (!any(is.na(indMatch))) {      # all match
        attr(object, "Dimnames") <- list(value, value)
        if ((length(indMatch)) > 1 && any(diff(indMatch) != 1) &&
            isInitialized(object)) { # permutation
          auxMat <- as.matrix(object)[indMatch, indMatch, drop = FALSE]
          dimnames(auxMat) <- list(value, value)
          return(pdConstruct(object, auxMat))
        }
        return(object)
      }
    }
    if (err) {
      stop("names being assigned do not correspond to a permutation of previous names")
    }
    indMatch <- match(value, dn)
    if ((length(indMatch) == 1) || all(diff(indMatch) == 1)) {
      return(object)
    }
    ## must be a permutation of names
    attr(object, "Dimnames") <- list(value, value)
    if (isInitialized(object)) {
      auxMat <- as.matrix(object)[indMatch, indMatch, drop = FALSE]
      dimnames(auxMat) <- list(value, value)
      return(pdConstruct(object, auxMat))
    }
    object
  }
}

plot.pdMat <-
  function(x, nseg = 50, levels = 1, center = rep(0, length(stdDev)),
	   additional, ...)
{
  corr <- corMatrix(x)
  stdDev <- attr(corr, "stdDev")
  attr(corr, "stdDev") <- NULL
  assign(".corr", corr)
  assign(".angles", seq(-pi, pi, length = nseg + 1))
  assign(".cosines", cos(.angles))
  nlev <- length(levels)
  dataMat <- array(aperm(outer(rbind(-stdDev, stdDev), levels), c(1, 3, 2)),
		   dim = c(nlev * 2, length(stdDev)),
		   dimnames = list(NULL, names(stdDev)))
  groups <- rep(1:nlev, rep(2, nlev))
  dataMat <- t(t(dataMat) + center)
  if (!missing(additional)) {
    additional <- as.matrix(additional)
    dataMat <- rbind(dataMat, additional)
    groups <- c(groups, rep(0, nrow(additional)))
  }
  splom(~ dataMat, panel = function(x, y, subscripts, groups, ...) {
    groups <- groups[subscripts]	# should be a no-op but
    if (any(g0 <- groups == 0)) {	# plot as points
      panel.xyplot(x[g0], y[g0], ..., type = "p")
    }
    g1 <- groups == 1			# plot the center points
    panel.xyplot(mean(x[g1]), mean(y[g1]), ..., type = "p", pch = 3)
    p <- ncol(.corr)
    laggedCos <- cos(.angles + acos(.corr[round(mean(x[g1])*p + 0.5),
					  round(mean(y[g1])*p + 0.5)]))
    xylist <- lapply(split(data.frame(x = x[!g0], y = y[!g0]), groups[!g0]),
		     function(el, lagged) {
		       if (nrow(el) != 2) {
			 stop("x-y data to splom got botched somehow")
		       }
		       sumDif <- array(c(1,1,1,-1)/2, c(2,2)) %*% as.matrix(el)
		       list(x = sumDif[1,1] + .cosines * sumDif[2,1],
			    y = sumDif[1,2] + lagged * sumDif[2,2])
		     }, lagged = laggedCos)
    gg <- rep(seq_along(xylist), rep(length(.angles), length(xylist)))
    panel.superpose(unlist(lapply(xylist, "[[", "x")),
		    unlist(lapply(xylist, "[[", "y")),
		    subscripts = seq_along(gg), groups = gg, ..., type = "l")
  }, subscripts = TRUE, groups = groups)
}

print.pdMat <-
  function(x, ...)
{
  if (isInitialized(x)) {
    cat("Positive definite matrix structure of class", class(x)[1], "representing\n")
    print(as.matrix(x), ...)
  } else {
    cat("Uninitialized positive definite matrix structure of class ", class(x)[1],
	".\n", sep = "")
  }
  invisible(x)
}

print.summary.pdMat <-
  function(x, sigma = 1, rdig = 3, Level = NULL, resid = FALSE, ...)
  ## resid = TRUE causes an extra row to be added
{
  if (!is.list(x)) {
    if (!(is.null(form <- attr(x, "formula")))) {
      cat(paste(" Formula: "))
      if (inherits(form, "formula")) {
        cat(deparse(form))
        if (!is.null(Level)) { cat( paste( " |", Level ) ) }
      } else {
        if (length(form) == 1) {
          cat(deparse(form[[1]]))
          if (!is.null(Level)) { cat( paste( " |", Level ) ) }
        } else {
          cat(deparse(lapply(form,
                             function(el) as.name(deparse(el)))))
          cat("\n Level:", Level)
        }
      }
      cat( "\n" )
    }
    if (ncol(x) == 1) {
      if (resid) {
        print(array(sigma * c(attr(x, "stdDev"), 1), c(1, 2),
                    list("StdDev:",
                         c(names(attr(x, "stdDev")), "Residual"))), ... )
      } else {
        print(array(sigma * attr(x, "stdDev"), c(1,1),
                    list("StdDev:", names(attr(x, "stdDev")))), ... )
      }
    } else {
      cat(paste(" Structure: ", attr(x, "structName"), "\n", sep = ""))
      if (attr(x, "noCorrelation") | (1 >= (p <- dim(x)[2]))) {
        if (resid) {
          print(array(sigma * c(attr(x, "stdDev"), 1), c(1, p + 1),
                      list("StdDev:",
                           c(names(attr(x, "stdDev")), "Residual"))), ...)
        } else {
          print(array(sigma * attr(x, "stdDev"), c(1, p),
                      list("StdDev:", names(attr(x, "stdDev")))), ...)
        }
      } else {                          # we essentially do print.correlation here
        ll <- lower.tri(x)
        stdDev <- attr(x, "stdDev")
        x[ll] <- format(round(x[ll], digits = rdig), ...)
        x[!ll] <- ""
        xx <- array("", dim(x),
                    list(names(attr(x, "stdDev")),
                         c("StdDev", "Corr", rep("", p - 2))))
        xx[, 1] <- format(sigma * attr(x, "stdDev"))
        xx[-1, -1] <- x[ -1, -p ]
        if (!is.null(colnames(x))) {
          xx[1, -1] <- abbreviate(colnames(x)[ -p ], minlength = rdig + 3)
        }
        if (resid) {
          x <- array("", dim(xx) + c(1, 0),
                     list(c(rownames(xx), "Residual"), colnames(xx)))
          x[ 1:p, ] <- xx
          x[ , 1 ] <- format(sigma * c(stdDev, 1))
          xx <- x
        }
        print( xx, ..., quote = FALSE )
      }
    }
  } else {				# composite structure
    cat(paste(" Composite Structure: ", attr(x, "structName"), "\n", sep =""))
    elName <- attr(x, "elementName")
    compNames <- names(x)
    for (i in seq_along(x)) {
      cat(paste("\n ", elName, " ", i, ": ", compNames[i], "\n", sep = ""))
      print.summary.pdMat(x[[i]], sigma = sigma, Level = Level,
                          resid = resid && (i == length(x)), ...)
    }
  }
  invisible(x)
}

solve.pdMat <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot get the inverse of an uninitialized object")
  }
  matrix(a) <- solve(as.matrix(a))
  a
}

summary.pdMat <-
  function(object, structName = class(object)[1], noCorrelation = FALSE, ...)
{
  if (isInitialized(object)) {
    value <- corMatrix(object)
    attr(value, "structName") <- structName
    attr(value, "noCorrelation") <- noCorrelation
    attr(value, "formula") <- formula(object)
    class(value) <- "summary.pdMat"
    value
  } else {
    object
  }
}

"[.pdMat" <-
  function(x, i, j, drop = TRUE)
{
  xx <- x
  x <- as.matrix(x)
  if (missing(i)) li <- 0
  else li <- length(i)
  if (missing(j)) lj <- 0
  else lj <- length(j)

  if ((li + lj == 0) ||
      (li == lj) && ((mode(i) == mode(j)) && all(i == j))) {
    drop <- FALSE			# even for a 1 by 1 submatrix,
					# you want it to be a matrix
    pdConstruct(xx, NextMethod())
  } else {
    NextMethod()
  }
}

"[<-.pdMat" <-
  function(x, i, j, value)
{
  xx <- x
  x <- as.matrix(x)
  pdConstruct(xx, NextMethod())
}

##*## Classes that substitute for (i.e. inherit from) pdMat

###*# pdSymm - a class of general pd matrices

####* Constructor

pdSymm <-
  ## Constructor for the pdSymm class
  function(value = numeric(0), form = NULL, nam = NULL, data = parent.frame())
{
  object <- numeric(0)
  class(object) <- c("pdSymm", "pdMat")
  pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

pdConstruct.pdSymm <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.frame(sys.parent()), ...)
{
  val <- NextMethod()
  if (length(val) == 0) {               # uninitialized object
    class(val) <- c("pdSymm", "pdMat")
    return(val)
  }

  if (is.matrix(val)) {
    vald <- svd(val, nu = 0)
    object <- vald$v %*% (log(vald$d) * t(vald$v))
    value <- object[row(object) <= col(object)]
    attributes(value) <- attributes(val)[names(attributes(val)) !=  "dim"]
    class(value) <- c("pdSymm", "pdMat")
    return(value)
  }
  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
      stop(gettextf("an object of length %d does not match the required parameter size",
                    length(val)), domain = NA)
  }
  class(val) <- c("pdSymm", "pdMat")
  val
}

pdFactor.pdSymm <-
  function(object)
{
  Ncol <- round((-1 + sqrt(1 + 8 * length(object))) / 2)
  .C(matrixLog_pd,
     Factor = double(Ncol * Ncol),
     as.integer(Ncol),
     as.double(object))$Factor
}

pdMatrix.pdSymm <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot extract matrix from an uninitialized object")
  }
  if (factor) {
    Ncol <- Dim(object)[2]
    value <- array(pdFactor(object), c(Ncol, Ncol), attr(object, "Dimnames"))
    attr(value, "logDet") <- sum(log(abs(svd(value)$d)))
    value
  } else {
    NextMethod()
  }
}

####* Methods for standard generics

coef.pdSymm <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained || !isInitialized(object)) NextMethod()
  else {				# upper triangular elements
    val <- as.matrix(object)
    aN <- Names(object)
    aN1 <- paste("cov(", aN, sep ="")
    aN2 <- paste(aN, ")", sep ="")
    aNmat <- t(outer(aN1, aN2, paste, sep = ","))
    aNmat[row(aNmat) == col(aNmat)] <- paste("var(",aN,")",sep="")
    val <- val[row(val) <= col(val)]
    names(val) <- aNmat[row(aNmat) <= col(aNmat)]
    val
  }
}

Dim.pdSymm <-
  function(object, ...)
{
  if (isInitialized(object)) {
    val <- round((sqrt(8*length(object) + 1) - 1)/2)
    c(val, val)
  } else {
    NextMethod()
  }
}

logDet.pdSymm <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot extract the log of the determinant from an uninitialized object")
  }
  attr(pdMatrix(object, factor = TRUE), "logDet")
}

solve.pdSymm <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot extract the inverse from an uninitialized object")
  }
  coef(a) <- -coef(a, TRUE)
  a
}

summary.pdSymm <-
  function(object,
	   structName = "General positive-definite", ...)
{
  summary.pdMat(object, structName)
}

### No need to implement other methods as the methods for pdMat
### are sufficient.

###*# pdLogChol - a general positive definite structure parameterized
###   by the non-zero elements of the Cholesky factor with the diagonal
###   elements given in the logarithm scale.

####* Constructor

pdLogChol <-
  ## Constructor for the pdLogChol class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.parent())
{
  object <- numeric(0)
  class(object) <- c("pdLogChol", "pdMat")
  pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

pdConstruct.pdLogChol <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.parent(), ...)
{
  val <- pdConstruct.pdMat(object, value, form, nam, data)
  if (length(val) == 0) {               # uninitialized object
    class(val) <- c("pdLogChol", "pdSymm", "pdMat")
    return(val)
  }
  if (is.matrix(val)) {
    value <- c(log(diag(val)), val[row(val) < col(val)])
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    class(value) <- c("pdLogChol", "pdSymm", "pdMat")
    return(value)
  }
  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
      stop(gettextf("an object of length %d does not match a Cholesky factor",
                    length(val)), domain = NA)
  }
  class(val) <- c("pdLogChol", "pdSymm", "pdMat")
  val
}

pdFactor.pdLogChol <-
  function(object)
{
  Ncol <- round((-1 + sqrt(1 + 8 * length(object))) / 2)
  .C(logChol_pd,
     Factor = double(Ncol * Ncol),
     as.integer(Ncol),
     as.double(object))$Factor
}

####* Methods for standard generics

solve.pdLogChol <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot get the inverse of an uninitialized object")
  }
  Ncol <- (-1 + sqrt(1 + 8 * length(a))) / 2
#  val <- array(.Fortran("dbksl",
# 			as.double(pdFactor(a)),
# 			as.integer(Ncol),
# 			as.integer(Ncol),
# 			val = as.double(diag(Ncol)),
# 			as.integer(Ncol),
# 			integer(1))[["val"]], c(Ncol, Ncol))
#  val <- qr(t(val))$qr
  val <- qr(t(solve(pdMatrix(a, factor = TRUE))))$qr
  val <- sign(diag(val)) * val
  coef(a) <- c(log(diag(val)), val[c(row(val) < col(val))])
  a
}

summary.pdLogChol <-
  function(object, structName =
           "General positive-definite, Log-Cholesky parametrization",
           ...)
{
  summary.pdMat(object, structName)
}

### No need to implement other methods as the methods for pdMat
### are sufficient.

####*# pdChol - a general positive definite structure parameterized by
####   the non-zero elements of the Cholesky factor.

#####* Constructor

#pdChol <-
#  ## Constructor for the pdChol class
#  function(value = numeric(0), form = NULL, nam = NULL, data = sys.parent())
#{
#  object <- numeric(0)
#  class(object) <- c("pdChol", "pdMat")
#  pdConstruct(object, value, form, nam, data)
#}

#####* Methods for local generics

#pdConstruct.pdChol <-
#  function(object, value = numeric(0), form = formula(object),
#	   nam = Names(object), data = sys.parent())
#{
#  val <- pdConstruct.pdMat(object, value, form, nam, data)
#  if (length(val) == 0) {               # uninitialized object
#    class(val) <- c("pdChol", "pdSymm", "pdMat")
#    return(val)
#  }
#  if (is.matrix(val)) {
#    value <- val[row(val) <= col(val)]
#    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
#    class(value) <- c("pdChol", "pdSymm", "pdMat")
#    return(value)
#  }
#  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
#  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
#    stop(paste("An object of length", length(val),
#	       "does not match a Cholesky factor"))
#  }
#  class(val) <- c("pdChol", "pdSymm", "pdMat")
#  val
#}

#pdFactor.pdChol <-
#  function(object)
#{
#  round(Ncol <- (-1 + sqrt(1 + 8 * length(object))) / 2)
#  .C("Chol_pd",
#     Factor = double(Ncol * Ncol),
#     as.integer(Ncol),
#     as.double(object))$Factor
#}

#####* Methods for standard generics

#solve.pdChol <-
#  function(a, b)
#{
#  if (!isInitialized(a)) {
#    stop("cannot get the inverse of an uninitialized object")
#  }
#  Ncol <- (-1 + sqrt(1 + 8 * length(a))) / 2
#  val <- array(.Fortran("dbksl",
#			as.double(pdFactor(a)),
#			as.integer(Ncol),
#			as.integer(Ncol),
#			val = as.double(diag(Ncol)),
#			as.integer(Ncol),
#			integer(1))[["val"]], c(Ncol, Ncol))
#  coef(a) <-  qr(t(val))$qr[c(row(val) <= col(val))]
#  a
#}

#summary.pdChol <-
#  function(object,
#           structName = "General positive-definite, Cholesky parametrization")
#{
#  summary.pdMat(object, structName)
#}

#### No need to implement other methods as the methods for pdMat
#### are sufficient.

####*# pdSpher - a general positive definite structure parameterized
####   by the non-zero elements of the Cholesky factor with each column
####   represented in spherical coordinates

#####* Constructor

#pdSpher <-
#  ## Constructor for the pdSpher class
#  function(value = numeric(0), form = NULL, nam = NULL, data = sys.parent())
#{
#  object <- numeric(0)
#  class(object) <- c("pdSpher", "pdMat")
#  pdConstruct(object, value, form, nam, data)
#}

#####* Methods for local generics

#pdConstruct.pdSpher <-
#  function(object, value = numeric(0), form = formula(object),
#	   nam = Names(object), data = sys.parent())
#{
#  val <- pdConstruct.pdMat(object, value, form, nam, data)
#  if (length(val) == 0) {			# uninitiliazed object
#    class(val) <- c("pdSpher", "pdSymm", "pdMat")
#    return(val)
#  }
#  if (is.matrix(val)) {
#    Ncol <- dim(val)[2]
#    value <- log(apply(val, FUN = function(x){sqrt(sum(x^2))},2))
#    for(i in (1:Ncol)[-1]) {
#      aux <- acos(val[1:(i-1),i]/sqrt(cumsum(val[i:1,i]^2)[i:2]))
#      value <- c(value, log(aux/(pi - aux)))
#    }
#    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
#    class(value) <- c("pdSpher", "pdSymm", "pdMat")
#    return(value)
#  }
#  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
#  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
#    stop(paste("An object of length", length(val),
#	       "does not match a Cholesky factor"))
#  }
#  class(val) <- c("pdSpher", "pdSymm", "pdMat")
#  val
#}

#pdFactor.pdSpher <-
#  function(object)
#{
#  round(Ncol <- (-1 + sqrt(1 + 8 * length(object))) / 2)
#  .C("spher_pd",
#     Factor = double(Ncol * Ncol),
#     as.integer(Ncol),
#     as.double(object))$Factor
#}

#####* Methods for standar generics

#summary.pdSpher <-
#  function(object,
#           structName = "General positive-definite, Spherical parametrization")
#{
#  summary.pdMat(object, structName)
#}

####*# pdMatrixLog - a general positive definite structure parameterized
####   by the matrix logarithm.

#####* Constructor

#pdMatrixLog <-
#  ## Constructor for the pdMatrixLog class
#  function(value = numeric(0), form = NULL, nam = NULL, data = sys.parent())
#{
#  object <- numeric(0)
#  class(object) <- c("pdMatrixLog", "pdMat")
#  pdConstruct(object, value, form, nam, data)
#}

#####* Methods for local generics

#pdConstruct.pdMatrixLog <-
#  function(object, value = numeric(0), form = formula(object),
#	   nam = Names(object), data = sys.parent())
#{
#  val <- pdConstruct.pdMat(object, value, form, nam, data)
#  if (length(val) == 0) {               # uninitialized object
#    class(val) <- c("pdMatrixLog", "pdSymm", "pdMat")
#    return(val)
#  }

#  if (is.matrix(val)) {
#    object <- eigen(crossprod(val), symmetric = TRUE)
#    object <- object$vectors %*% (log(object$values) * t(object$vectors))
#    value <- object[row(object) <= col(object)]
#    attributes(value) <- attributes(val)[names(attributes(val)) !=  "dim"]
#    class(value) <- c("pdMatrixLog", "pdSymm", "pdMat")
#    return(value)
#  }
#  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
#  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
#    stop(paste("An object of length", length(val),
#	       "does not match the required parameter size"))
#  }
#  class(val) <- c("pdMatrixLog", "pdSymm", "pdMat")
#  val
#}

#pdFactor.pdMatrixLog <-
#  function(object)
#{
#  round(Ncol <- (-1 + sqrt(1 + 8 * length(object))) / 2)
#  .C("matrixLog_pd",
#     Factor = double(Ncol * Ncol),
#     as.integer(Ncol),
#     as.double(object))$Factor
#}

#####* Methods for standard generics

#solve.pdMatrixLog <-
#  function(a, b)
#{
#  if (!isInitialized(a)) {
#    stop("cannot extract the inverse from an uninitialized object")
#  }
#  coef(a) <- -coef(a, TRUE)
#  a
#}

#summary.pdMatrixLog <-
#  function(object,
#	   structName = "General positive-definite")
#{
#  summary.pdMat(object, structName)
#}

#### No need to implement other methods as the methods for pdMat
#### are sufficient.


####*# pdGivens - a general positive definite structure parameterized
####   by the eigenvalues and eigenvectors (as Givens rotations)

#####* Constructor

#pdGivens <-
#  ## Constructor for the pdGivens class
#  function(value = numeric(0), form = NULL, nam = NULL, data = sys.parent())
#{
#  object <- numeric(0)
#  class(object) <- c("pdGivens", "pdMat")
#  pdConstruct(object, value, form, nam, data)
#}

#####* Methods for local generics

#pdConstruct.pdGivens <-
#  function(object, value = numeric(0), form = formula(object),
#	   nam = Names(object), data = sys.parent())
#{
#  val <- pdConstruct.pdMat(object, value, form, nam, data)
#  if (length(val) == 0) {               # uninitiliazed object
#    class(val) <- c("pdGivens", "pdSymm", "pdMat")
#    return(val)
#  }
#  if (is.matrix(val)) {
#    q <- dim(val)[1]
#    aux <-  eigen(crossprod(val), symmetric = TRUE)
#    Q <- aux$vectors
#    values <- aux$values
#    angles <- array(0,q*(q-1)/2)
#    k <- 0
#    for(i in 1:(q-1)) {
#      for(j in ((i+1):q)) {
#	k <- k + 1
#	p <- sqrt(Q[i,i]^2 + Q[j,i]^2)
#	if (p == 0) {
#	  angles[k] <- 0
#	} else {
#	  aux0 <- Q[i,i]/p
#	  aux1 <- Q[j,i]/p
#	  if (aux1 < 0) {
#	    aux0 <- -aux0
#	    aux1 <- -aux1
#	  }
#	  aux <- Q[i,]
#	  angles[k] <- log(acos(aux0)/(pi - acos(aux0)))
#	  Q[i,] <- Q[i,] * aux0 + Q[j,] * aux1
#	  Q[j,] <- Q[j,] * aux0 - aux * aux1
#	}
#      }
#    }
#    value <- c(log(c(values[q], diff(values[q:1]))), angles)
#    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
#    class(value) <- c("pdGivens", "pdSymm", "pdMat")
#    return(value)
#  }
#  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
#  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
#    stop(paste("An object of length", length(val),
#	       "does not match the required parameter size"))
#  }
#  class(val) <- c("pdGivens", "pdSymm", "pdMat")
#  val
#}

#pdFactor.pdGivens <-
#  function(object)
#{
#  round(Ncol <- (-1 + sqrt(1 + 8 * length(object))) / 2)
#  .C("Givens_pd",
#     Factor = double(Ncol * Ncol),
#     as.integer(Ncol),
#     as.double(object))$Factor
#}

#####* Methods for standard generics

#summary.pdGivens <-
#  function(object,
#	   structName = "General positive-definite, Givens parametrization")
#{
#  summary.pdMat(object, structName)
#}

#### No need to implement other methods as the methods for pdMat
#### are sufficient.

#pdConstruct.pdSymm <- pdConstruct.pdMatrixLog    #default parametrization

####*# pdNatural - a general positive definite structure parameterized
####   by the log of the square root of the diagonal elements and the
####   generalized logit of the correlations. This is NOT an unrestricted
####   parametrization

####* Constructor

pdNatural <-
  ## Constructor for the pdNatural class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
{
  object <- numeric(0)
  class(object) <- c("pdNatural", "pdMat")
  pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

pdConstruct.pdNatural <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.frame(sys.parent()), ...)
{
  val <- pdConstruct.pdMat(object, value, form, nam, data)
  if (length(val) == 0) {               # uninitiliazed object
    class(val) <- c("pdNatural", "pdMat")
    return(val)
  }
  if (is.matrix(val)) {
    q <- ncol(val)
    if (q > 1) {
      aux <- crossprod(val)
      stdDev <- sqrt(diag(aux))
      aux <- t(aux/stdDev)/stdDev
      aux <- aux[row(aux) > col(aux)]
      value <- c(log(stdDev), log((aux + 1)/(1 - aux)))
    } else {
      value <- log(val)
    }
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    class(value) <- c("pdNatural", "pdMat")
    return(value)
  }
  Ncol <- round((sqrt(8*length(val) + 1) - 1)/2)
  if (length(val) != round((Ncol * (Ncol + 1))/2)) {
      stop(gettextf("an object of length %d does not match the required parameter size",
                    length(val)), domain = NA)
  }
  class(val) <- c("pdNatural", "pdMat")
  val
}

pdFactor.pdNatural <-
  function(object)
{
  Ncol <- round((-1 + sqrt(1 + 8 * length(object))) / 2)
  .C(natural_pd,
     Factor = double(Ncol * Ncol),
     as.integer(Ncol),
     as.double(object))$Factor
}

pdMatrix.pdNatural <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot extract matrix from an uninitialized object")
  }
  if (factor) {
    Ncol <- Dim(object)[2]
    value <- array(pdFactor(object), c(Ncol, Ncol), attr(object, "Dimnames"))
    attr(value, "logDet") <- sum(log(diag(value)))
    value
  } else {
    NextMethod()
  }
}

####* Methods for standard generics

coef.pdNatural <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained || !isInitialized(object)) NextMethod()
  else {				# standard deviations and correlations
    Ncol <- round((-1 + sqrt(1 + 8 * length(object))) / 2)
    val <- exp(as.vector(object))
    aux <- val[-(1:Ncol)]
    val[-(1:Ncol)] <- (aux - 1) / (aux + 1)
    aN <- Names(object)
    aNmat <- t(outer(aN, aN, paste, sep = ","))
    names(val) <- c(paste("sd(",aN,")", sep = ""),
		    if (Ncol > 1) {
		      paste("cor(", aNmat[row(aNmat) > col(aNmat)],")",sep="")
		    })
    val
  }
}

Dim.pdNatural <-
  function(object, ...)
{
  if (isInitialized(object)) {
    val <- round((sqrt(8*length(object) + 1) - 1)/2)
    c(val, val)
  } else {
    NextMethod()
  }
}

logDet.pdNatural <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot extract the log of the determinant from an uninitialized object")
  }
  attr(pdMatrix(object, factor = TRUE), "logDet")
}


solve.pdNatural <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot get the inverse of an uninitialized object")
  }
  Ncol <- round((-1 + sqrt(1 + 8 * length(a))) / 2)
  if (Ncol > 1) {
#     val <- array(.Fortran("dbksl",
# 			  as.double(pdFactor(a)),
# 			  as.integer(Ncol),
# 			  as.integer(Ncol),
# 			  val = as.double(diag(Ncol)),
# 			  as.integer(Ncol),
# 			  integer(1))[["val"]], c(Ncol, Ncol))
    val <- solve(pdMatrix(a, factor = TRUE))
    val <- val %*% t(val)
    stdDev <- sqrt(diag(val))
    val <- t(val/stdDev)/stdDev
    val <- val[row(val) > col(val)]
    coef(a) <- c(log(stdDev), log((val + 1)/(1 - val)))
  } else {
    coef(a) <- -coef(a)
  }
  a
}

summary.pdNatural <-
  function(object,
	   structName = "General positive-definite, Natural parametrization",
           ...)
{
  summary.pdMat(object, structName)
}

### No need to implement other methods as the methods for pdMat
### are sufficient.

###*# pdDiag - diagonal structure parameterized by the logarithm of
###   the square root of the diagonal terms.

####* Constructor

pdDiag <-
  ## Constructor for the pdDiag class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
{
  object <- numeric(0)
  class(object) <- c("pdDiag", "pdMat")
  pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

corMatrix.pdDiag <-
  function(object, ...)
{
  val <- diag(length(as.vector(object)))
  attr(val, "stdDev") <- exp(as.vector(object))
  len <- length(as.vector(object))
  if (length(nm <- Names(object)) == 0) {
    nm <- paste("V", 1:len, sep = "")
    dimnames(val) <- list(nm, nm)
  }
  names(attr(val, "stdDev")) <- nm
  val
}

pdConstruct.pdDiag <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.frame(sys.parent()), ...)
{
  val <- NextMethod()
  if (length(val) == 0) {               # uninitiliazed object
    return(val)
  }
  if (is.matrix(val)) {			# initialize from a positive definite
#    if (any(value[row(val) != col(val)])) {
#      warning("Initializing matrix is not diagonal")
#    }
    value <- log(diag(crossprod(val)))/2
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    class(value) <- c("pdDiag", "pdMat")
    return(value)
  }
  if ((aux <- length(Names(val))) > 0) {
    if (aux && (aux != length(val))) {
        stop(gettextf("an object of length %d does not match the required parameter size",
                      length(val)), domain = NA)
    }
  }
  val
}

pdFactor.pdDiag <-
  function(object)
{
  diag(exp(as.vector(object)), length(object))
}

pdMatrix.pdDiag <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized object")
  }
  len <- length(as.vector(object))
  if (factor) {
    value <- diag(exp(as.vector(object)), len)
    attr(value, "logDet") <- sum(as.vector(object))
  } else {
    value <- diag(exp(2 * as.vector(object)), len)
  }
  dimnames(value) <- attr(object, "Dimnames")
  value
}

####* Methods for standard generics

coef.pdDiag <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) NextMethod()
  else {
    val <- exp(as.vector(object))
    names(val) <- paste("sd(",Names(object),")", sep ="")
    val
  }
}

Dim.pdDiag <-
  function(object, ...)
{
  if (isInitialized(object)) {
    val <- length(object)
    c(val, val)
  } else {
    NextMethod()
  }
}

logDet.pdDiag <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot extract the log of the determinant from an uninitialized object")
  }
  sum(as.vector(object))
}

solve.pdDiag <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot extract the inverse from an uninitialized object")
  }
  coef(a) <- -coef(a, TRUE)
  a
}

summary.pdDiag <-
  function(object, structName = "Diagonal", ...)
{
  summary.pdMat(object, structName, noCorrelation = TRUE)
}

### No need to implement other methods as the "pdMat" methods suffice.

###*# pdIdent: multiple of the identity matrix - the parameter is
###   the log of the multiple.

####* Constructor

pdIdent <-
  ## Constructor for the pdIdent class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
{
  object <- numeric(0)
  class(object) <- c("pdIdent", "pdMat")
  pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

corMatrix.pdIdent <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized \"pdIdent\" object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop("cannot extract the matrix with uninitialized dimensions")
  }
  val <- diag(nrow = Ncol)
  attr(val, "stdDev") <- rep(exp(as.vector(object)), Ncol)
  if (length(nm <- Names(object)) == 0) {
    nm <- paste("V", 1:Ncol, sep = "")
  }
  dimnames(val) <- list(nm, nm)
  names(attr(val, "stdDev")) <- nm
  val
}

pdConstruct.pdIdent <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.frame(sys.parent()), ...)
{
  val <- NextMethod()
  if (length(val) == 0) {			# uninitialized object
    if ((ncol <- length(Names(val))) > 0) {
      attr(val, "ncol") <- ncol
    }
    return(val)
  }
  if (is.matrix(val)) {
#    if (any(val[row(val) != col(val)])) {
#      warning("Initializing pdIdent object from non-diagonal matrix")
#    }
#    if (any(diag(val) != val[1,1])) {
#      warning("Diagonal of initializing matrix is not constant")
#    }
    value <- log(mean(diag(crossprod(val))))/2
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    attr(value, "ncol") <- dim(val)[2]
    class(value) <- c("pdIdent", "pdMat")
    return(value)
  }
  if (length(val) > 1) {
      stop(gettextf("an object of length %d does not match the required parameter size",
                    length(val)), domain = NA)
  }
  if (((aux <- length(Names(val))) == 0) && is.null(formula(val))) {
    stop("must give names when initializing \"pdIdent\" from parameter without a formula")
  } else {
    attr(val, "ncol") <- aux
  }
  val
}

pdFactor.pdIdent <-
  function(object)
{
  exp(as.vector(object)) * diag(attr(object, "ncol"))
}


pdMatrix.pdIdent <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized \"pdIdent\" object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop("cannot extract the matrix with uninitialized dimensions")
  }
  value <- diag(Ncol)
  if (factor) {
    value <- exp(as.vector(object)) * value
    attr(value, "logDet") <- Ncol * as.vector(object)
  } else {
    value <- exp(2 * as.vector(object)) * value
  }
  dimnames(value) <- attr(object, "Dimnames")
  value
}

####* Methods for standard generics

coef.pdIdent <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) NextMethod()
  else structure(exp(as.vector(object)),
           names = c(paste("sd(", deparse(formula(object)[[2]]),")",sep = "")))
}

Dim.pdIdent <-
  function(object, ...)
{
  if (!is.null(val <- attr(object, "ncol"))) {
    c(val, val)
  } else {
    stop("cannot extract the dimensions")
  }
}

logDet.pdIdent <-
  function(object, ...)
{
  attr(object, "ncol") * as.vector(object)
}

solve.pdIdent <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot extract the inverse from an uninitialized object")
  }
  coef(a) <- -coef(a, TRUE)
  a
}

summary.pdIdent <-
  function(object, structName = "Multiple of an Identity", ...)
{
  summary.pdMat(object, structName, noCorrelation = TRUE)
}

###*# pdCompSymm: Compound symmetry structure

####* Constructor

pdCompSymm <-
  ## Constructor for the pdCompSymm class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
{
  object <- numeric(0)
  class(object) <- c("pdCompSymm", "pdMat")
  pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

corMatrix.pdCompSymm <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized \"pdCompSymm\" object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop("cannot extract the matrix with uninitialized dimensions")
  }
  obj <- as.vector(object)
  aux <- exp(obj[2])
  aux <- c(exp(2 * obj[1]), (aux - 1/(Ncol - 1))/(aux + 1))
  value <- array(aux[2], c(Ncol, Ncol))
  value[row(value) == col(value)] <- 1
  attr(value, "stdDev") <- rep(exp(obj[1]), Ncol)
  if (length(nm <- Names(object)) == 0) {
    nm <- paste("V", 1:Ncol, sep = "")
    dimnames(value) <- list(nm, nm)
  }
  names(attr(value, "stdDev")) <- nm
  value
}

pdConstruct.pdCompSymm <-
  function(object, value = numeric(0), form = formula(object),
	   nam = Names(object), data = sys.frame(sys.parent()), ...)
{
  val <- NextMethod()
  if (length(val) == 0) {                # uninitialized object
    if ((nc <- length(Names(val))) > 0) {
      attr(val, "ncol") <- nc
    }
    return(val)
  }
  if (is.matrix(val)) {
    value <- crossprod(val)
#    if (length(unique(value[row(value) != col(value)])) > 1) {
#      warning("Initializing pdCompSymm object from non-compound symmetry matrix")
#    }
#    if (any(diag(value) != value[1,1])) {
#      warning("Diagonal of initializing matrix is not constant")
#    }
    nc <- dim(value)[2]
    aux <- 1/sqrt(diag(value))
    aux <- aux * t(value * aux)
    if ((aux <- mean(aux[row(aux) != col(aux)])) <= -1/(nc - 1)) {
      aux <- -1/nc
      warning("initializing \"pdCompSymm\" object is not positive definite")
    }
    value <- c(log(mean(diag(value)))/2, log((aux + 1/(nc - 1))/(1 - aux)))
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    attr(value, "ncol") <- nc
    class(value) <- c("pdCompSymm", "pdMat")
    return(value)
  }
  if (length(val) != 2) {
      stop(gettextf("an object of length %d does not match the required parameter size",
                    length(val)), domain = NA)
  }
  if (((aux <- length(Names(val))) == 0) && is.null(formula(val))) {
    stop("must give names when initializing \"pdCompSymm\" from parameter without a formula")
  } else {
    attr(val, "ncol") <- aux
  }
  val
}

pdFactor.pdCompSymm <-
  function(object)
{
  Ncol <- attr(object, "ncol")
  .C(compSymm_pd,
     Factor = double(Ncol * Ncol),
     as.integer(Ncol),
     as.double(object))$Factor
}

pdMatrix.pdCompSymm <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot extract the matrix from an uninitialized \"pdCompSymm\" object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop("cannot extract the matrix with uninitialized dimensions")
  }

  obj <- as.vector(object)
  aux <- exp(obj[2])
  aux <- c(exp(2 * obj[1]), (aux - 1/(Ncol - 1))/(aux + 1))
  if (factor) {
    value <- array(pdFactor(object), c(Ncol, Ncol))
    attr(value, "logDet") <-  Ncol * obj[1] +
      ((Ncol - 1) * log(1 - aux[2]) + log(1 + (Ncol - 1) * aux[2]))/2
  } else {
    value <- array(aux[2], c(Ncol, Ncol))
    value[row(value) == col(value)] <- 1
    value <- aux[1] * value
  }
  dimnames(value) <- attr(object, "Dimnames")
  value
}

####* Methods for standard generics

coef.pdCompSymm <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained || !isInitialized(object)) NextMethod()
  else {
    if (is.null(Ncol <- attr(object, "ncol"))) {
      stop("cannot obtain constrained coefficients with uninitialized dimensions")
    }
    val <- as.vector(object)
    aux <- exp(val[2])
    val <- c(exp(val[1]), (aux - 1 / (Ncol - 1)) / (aux + 1))
    names(val) <- c("std. dev", "corr.")
    val
  }
}

Dim.pdCompSymm <-
  function(object, ...)
{
  if (!is.null(val <- attr(object, "ncol"))) {
    c(val, val)
  } else {
    stop("cannot extract the dimensions")
  }
}

logDet.pdCompSymm <-
  function(object, ...)
{
  attr(pdMatrix(object, factor = TRUE), "logDet")
}

summary.pdCompSymm <-
  function(object, structName = "Compound Symmetry", ...)
{
  summary.pdMat(object, structName)
}

####*# pdBlocked: A blocked variance structure

#####* Constructor

pdBlocked <-
  ## Constructor for the pdBlocked class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()),
	   pdClass = "pdSymm")
{
  object <- numeric(0)
  class(object) <- c("pdBlocked", "pdMat")
  pdConstruct(object, value, form, nam, data, pdClass)
}

####* Methods for local generics

corMatrix.pdBlocked <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("cannot access the matrix of uninitialized objects")
  }
  if (length(Names(object)) == 0) {
    stop("cannot access the matrix of object without names")
  }
  namesList <- Names(object, TRUE)
  Ncol <- Dim(object)[2]
  value <- array(0, c(Ncol, Ncol), attr(object, "Dimnames"))
  stdDev <- double(Ncol)
  names(stdDev) <- colnames(value)
  for (i in seq_along(object)) {
    aux <- corMatrix(object[[i]])
    value[namesList[[i]], namesList[[i]]] <- as.vector(aux)
    stdDev[namesList[[i]]] <- attr(aux, "stdDev")
  }
  attr(value, "stdDev") <- stdDev
  value
}


pdConstruct.pdBlocked <-
  function(object, value = numeric(0), form = formula(object, TRUE),
	   nam = Names(object, TRUE), data = sys.frame(sys.parent()),
	   pdClass = "pdSymm", ...)
{
  if (inherits(value, "pdMat")) {	# constructing from another pdMat
    if (inherits(value, "pdBlocked")) {
      if (length(form) == 0) form <- formula(value, TRUE)
      if (length(nam) == 0) nam <- Names(value, TRUE)
      if (missing(pdClass)) pdClass <- unlist(lapply(value, data.class))
    }
    if (isInitialized(value)) {
      return(pdConstruct(object, as.matrix(value), form, nam, data, pdClass))
    } else {
      return(pdConstruct(object, form = form, nam = nam, data = data,
                         pdClass = pdClass))
    }
  }
  ## checking validity and consistency of form, nam, and pdClass
  if (!is.null(form)) {
    if (data.class(form) != "list") {
      stop("'form' must be a list")
    }
    nF <- length(form)
  } else {
    nF <- 0
  }

  if (!is.null(nam)) {
    if (data.class(nam) != "list") {
      stop("'nam' must be a list")
    }
    nN <- length(nam)
    if ((nF > 0) && (nN != nF)) {
      stop("'form' and 'nam' have incompatible lengths")
    }
  } else {
    nN <- 0
  }

  if (!missing(pdClass)) {
    if (!is.character(pdClass)) {
      stop("'pdClass' must be a character vector")
    }
    nP <- length(pdClass)
    if ((nP > 1)) {
      if ((nF > 0) && (nF != nP)) {
	stop("'form' and 'pdClass' have incompatible lengths")
      }
      if ((nN > 0) && (nN != nP)) {
	stop("'nam' and 'pdClass' have incompatible lengths")
      }
    }
  } else {
    nP <- 1
  }

  nB <- max(c(nF, nN, nP))

  oVal <- value
  if (length(value) == 0 || is.matrix(value) || is.numeric(value)) {
    if (nB == 1) {
      stop("LNone of the arguments specify more than one block")
    }
    ## will first do a null initialization when value is a matrix or numeric
    value <- lapply(vector("list", nB), function(el) numeric(0))
  } else {
    if (data.class(value) != "list") {
      stop("'object' must be a list when not missing, not a matrix, and not numeric")
    }
    nO <- length(value)
    if ((nB > 1) && (nB != nO)) {
      stop("arguments imply different number of blocks")
    }
    nB <- nO
  }
  if (nP == 1) {
    pdClass <- rep(pdClass, nB)
  }

  object <- vector("list", nB)
  namInterc <- rep(FALSE, nB)
  namCoef <- vector("list", nB)
  for(i in 1:nB) {
    if (is.null(nm <- nam[[i]])) {
      if (is.null(frm <- form[[i]])) {
        if (inherits(value[[i]], "formula")) {
          nm <- Names(getCovariateFormula(value[[i]]))
          if ((length(nm) == 1) && (nm == "(Intercept)") &&
              length(value[[i]]) == 3) {
            ## nlme case with single intercept terms
            nm <-  sapply(splitFormula(getResponseFormula(value[[i]])[[2]],
                                       sep = "+"),
                          function(el) deparse(el[[2]]))
          }
          if (length(value[[i]]) == 3) { # nlme case
            namCoef[[i]] <-
              sapply(splitFormula(getResponseFormula(value[[i]])[[2]],
                                  sep = "+"),
                     function(el) deparse(el[[2]]))
          }
        }
      } else {
        if (inherits(frm, "formula")) {
          nm <- Names(getCovariateFormula(frm))
          if ((length(nm) == 1) && (nm == "(Intercept)") &&
              length(frm) == 3) {
            ## nlme case with single intercept terms
            nm <-  sapply(splitFormula(getResponseFormula(frm)[[2]],
                                       sep = "+"),
                          function(el) deparse(el[[2]]))
          }
          if (length(value[[i]]) == 3) { # nlme case
            namCoef[[i]] <-
              sapply(splitFormula(getResponseFormula(value[[i]])[[2]],
                                  sep = "+"),
                     function(el) deparse(el[[2]]))
          }
        } else {                        # listForm
          nm <- unique(unlist(lapply(frm,
                                     function(el) {
                                       Names(getCovariateFormula(el))
                                     })))
          if ((length(nm) == 1) && (nm == "(Intercept)") &&
              length(frm[[1]]) == 3) {
            ## nlme case with single intercept terms
            nm <-  sapply(frm, function(el) {
              sapply(splitFormula(getResponseFormula(el)[[2]],
                                  sep = "+"), function(el1) deparse(el1[[2]]))
            })
          }
          namCoef[[i]] <- sapply(frm, function(el) {
            sapply(splitFormula(getResponseFormula(el)[[2]],
                                  sep = "+"), function(el1) deparse(el1[[2]]))
          })
        }
      }
    }
    if (!is.null(nm)) {
      namInterc[i] <- (length(nm) == 1) && (nm == "(Intercept)")
    }
    object[[i]] <- pdMat(value[[i]], form[[i]], nam[[i]], data, pdClass[i])
  }
  if (!all(unlist(lapply(object, inherits, "pdMat")))) {
    stop("all elements in the argument must generate \"pdMat\" objects")
  }
  namesList <- lapply(object, Names)
  lNam <- unlist(lapply(namesList, length))
#  namInterc <- unlist(lapply(namesList,
#                             function(el) {
#                               (length(el) == 1) && (el == "(Intercept)")
#                             }))
  if (!is.null(namCoef[[1]])) {         # nlme case
    namCoef <- unlist(namCoef)
    duplCoef <- unique(namCoef[duplicated(namCoef)])
    if (length(duplCoef) > 0) {
      for(i in 1:nB) {
        wchDupl <- !is.na(match(namesList[[i]], duplCoef))
        if (any(wchDupl)) {
          namesList[[i]][wchDupl] <-
            paste(namesList[[i]][wchDupl], "(Intercept)", sep = ".")
          Names(object[[i]]) <- namesList[[i]]
        }
      }
    }
  }
  if (sum(namInterc) > 1 && (length(unique(lNam[namInterc])) == 1)) {
    stop("cannot have duplicated column names in a \"pdMat\" object")
  }
  if ((sum(namInterc) == length(lNam)) ||
      !any(lNam[!namInterc])) {			# no names
    class(object) <- c("pdBlocked", "pdMat")
    if (is.null(formula(object))) {
      stop("must have formula when no names are given")
    }
    if (length(oVal) && (is.matrix(oVal) || is.numeric(oVal))) {
      stop("must give names when initializing from matrix or parameter")
    }
    return(object)
  } else {
    if (!all(lNam)) {
      stop("all elements must have names when any has names")
    }
    attr(object, "namesList") <- namesList
    allNames <- unlist(namesList)
    if (any(duplicated(allNames))) {
      stop("cannot have duplicated column names in a \"pdMat\" object")
    }
    plen <- unlist(lapply(object, function(el)
			  {
			    if (isInitialized(el)) {
			      length(coef(el, TRUE))
			    } else {
			      matrix(el) <- diag(length(Names(el)))
			      length(coef(el, TRUE))
			    }
			  }))
    if (!all(plen)) {
      stop("all elements must have a non-zero size")
    }
    attr(object, "plen") <- plen
    attr(object, "Dimnames") <- list(allNames, allNames)
    class(object) <- c("pdBlocked", "pdMat")

    if (length(oVal) > 0) {
      if (is.matrix(oVal)) {		# initializing from matrix
	matrix(object) <- oVal
      } else if (is.numeric(oVal)){		# initializing from a vector
	coef(object) <- oVal
      }
    }
    return(object)
  }
}

pdMatrix.pdBlocked <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("cannot access the matrix of uninitialized objects")
  }
  if (length(Names(object)) == 0) {
    stop("cannot access the matrix of object without names")
  }
  namesList <- Names(object, TRUE)
  Ncol <- Dim(object)[2]
  value <- array(0, c(Ncol, Ncol), attr(object, "Dimnames"))
  if (factor) {
    lD <- 0
  }
  for (i in seq_along(object)) {
    aux <- pdMatrix(object[[i]], factor)
    value[namesList[[i]], namesList[[i]]] <- as.vector(aux)
    if (factor) lD <- lD + attr(aux, "logDet")
  }
  if (factor) attr(value, "logDet") <- lD
  value
}

####* Methods for standard generics

coef.pdBlocked <-
  function(object, unconstrained = TRUE, ...)
{
  unlist(lapply(object, coef, unconstrained))
}

"coef<-.pdBlocked" <-
  function(object, ..., value)
{
  if (is.null(plen <- attr(object, "plen"))) {
    stop("cannot change the parameter when length of parameters is undefined")
  }
  if (length(value) != sum(plen)) {
    stop("cannot change parameter length of initialized \"pdMat\" object")
  }
  ends <- cumsum(plen)
  starts <- 1 + c(0, ends[-length(ends)])
  for (i in seq_along(object)) {
    coef(object[[i]]) <- value[(starts[i]):(ends[i])]
  }
  object
}

formula.pdBlocked <-
  function(x, asList = TRUE, ...)
{
  val <- lapply(x, formula)
  isNULL <- unlist(lapply(val, is.null))
  if (all(isNULL)) return(NULL)
  if (any(isNULL)) {
    stop("all elements must have formulas when any has a formula")
  }
  if (asList) return(val)
  isTwoSided <- unlist(lapply(val,
                              function(el) {
                                inherits(el, "listForm")
                              }))
  if (all(isTwoSided)) {
    ## list of two-sided formulas
    val <- do.call("c", val)
#    for(i in seq(along = object)) {
#      val <- if (inherits(object[[i]], "formula")) list(object[[i]])
#               else object[[i]]
#    }
    class(val) <- "listForm"
    return(val)
  }
  if (any(isTwoSided)) {
    stop("all elements of formula must be list of two-sided formulae or two-sided formulae")
  }
  val <- lapply(val, terms)
  aux <- paste(unlist(lapply(val, function(el) attr(el, "term.labels"))),
	       collapse = "+")
  if (!any(unlist(lapply(val, function(el) attr(el, "intercept"))))) {
    ## no intercept
    aux <- paste(aux, " - 1")
  }
  eval(parse(text = paste("~", aux)))
}

isInitialized.pdBlocked <-
  function(object)
{
  all(unlist(lapply(object, isInitialized)))
}

logDet.pdBlocked <-
  function(object, ...)
{
  sum(unlist(lapply(object, logDet)))
}

"matrix<-.pdBlocked" <-
  function(object, value)
{
  value <- as.matrix(value)
  namesList <- Names(object, TRUE)
  Ncol <- Dim(object)[2]
  dims <- dim(value)
  if (!((dims[1] == dims[2]) && (dims[1] == Ncol))) {
    stop("cannot change the number of columns on an initialized object")
  }
  if (is.null(vNames <- rownames(value))) {
    vNames <- unlist(namesList)
    dimnames(value) <- list(vNames, vNames)
  } else {
    if (!(all(match(unlist(namesList), vNames, nomatch = 0)))) {
      stop("names of object and value must match")
    }
    attr(object, "Dimnames") <- list(vNames, vNames)
  }
  for (i in seq_along(object)) {
    matrix(object[[i]]) <- value[namesList[[i]], namesList[[i]]]
  }
  object
}

Names.pdBlocked <-
  function(object, asList = FALSE, ...)
{
  if (asList) attr(object, "namesList")
  else attr(object, "Dimnames")[[2]]
}

"Names<-.pdBlocked" <-
  function(object, ..., value)
{
  if (!is.null(Names(object))) NextMethod()
  else {
    ## cannot do anything before initialization of names
    object
  }
}

pdFactor.pdBlocked <-
  function(object)
{
  pdMatrix(object, factor = TRUE)
}

solve.pdBlocked <-
  function(a, b, ...)
{
  if (!isInitialized(a)) {
    stop("cannot get the inverse of an uninitialized object")
  }
  coef(a) <- unlist(lapply(a, function(el) coef(solve(el), TRUE)))
  a
}

summary.pdBlocked <-
  function(object, structName = "Blocked", ...)
{
  value <- lapply(object, summary)
  names(value) <- unlist(lapply(object, function(el) paste(Names(el),
							   collapse = ", ")))
  attr(value, "structName") <- structName
  attr(value, "elementName") <- "Block"
  class(value) <- "summary.pdMat"
  value
}

"[.pdBlocked" <-
  function(x, i, j, drop = TRUE)
{
  xx <- x
  x <- as.matrix(x)
  mCall <- match.call()
  mCall[[1]] <- get("[")
  mCall[["x"]] <- x
  mCall[["drop"]] <- drop
  if (length(i) == length(j) && mode(i) == mode(j) && all(i == j)) {
    mCall[["drop"]] <- FALSE		# even for a 1 by 1 submatrix,
					# you want it to be a matrix
    val <- eval(mCall)
    vNames <- colnames(val)
    auxNames <- lapply(Names(xx, TRUE),
		       function(el, vN) {
			 aux <- match(vN, el)
			 if (any(aux1 <- !is.na(aux))) {
			   el[aux[aux1]]
			 }
		       }, vN = vNames)
    auxWhich <- !unlist(lapply(auxNames, is.null))
    if (sum(auxWhich) == 1) {
      return(pdConstruct(as.list(xx)[auxWhich][[1]], val))
    }
    auxNames <- auxNames[auxWhich]
    auxClass <- unlist(lapply(xx, function(el) class(el)[1]))[auxWhich]
    return(pdConstruct(xx, val, nam = auxNames, form = NULL,
		       pdClass = auxClass))
  } else {
    eval(mCall)
  }
}

### Local variables:
### mode: S
### End:


