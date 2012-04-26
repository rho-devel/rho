###      Methods for the class of random-effects structures.
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

##*## Generics that should be implemented for any reStruct class

###*# Constructor

reStruct <-
  function(object, pdClass = "pdLogChol", REML = FALSE, data = sys.frame(sys.parent()))
{
  ## object can be:
  ## 1) a named list of formulas or pdMats with grouping factors as names
  ##    (assume same order of nesting as order of names)
  ## 2) a formula of the form ~ x | g or ~ x | g1/g2/../gn
  ## 3) a list of formulas like ~x | g
  ## 4) a formula like ~x, a pdMat object, or a list of such
  ##    formulas or objects . In this case, the data used to
  ##    initialize the reStruct will be required to inherit from class
  ##    "groupedData"
  ## 5) another reStruct object
  ## parametrization specifies the pdMat constructor to be used for all
  ## formulas used in object

  if (inherits(object, "reStruct")) {	# little to do, return object
    if (!missing(REML)) attr(object, "settings")[1] <- as.integer(REML)
    object[] <- lapply(object,
		       function(el, data) {
			 pdMat(el, data = data)
		       }, data = data)
    return(object)
  }
  plen <- NULL
  if (inherits(object, "formula")) {	# given as a formula
    if (is.null(grpForm <- getGroupsFormula(object, asList = TRUE))) {
      object <- list( object )
    } else {
      if (length(object) == 3) {        # nlme type of formula
        object <-
          eval(parse(text = paste(deparse(getResponseFormula(object)[[2]]),
                       deparse(getCovariateFormula(object)[[2]], width.cutoff=500),
                     sep = "~")))
      } else {
        object <- getCovariateFormula(object)
      }
      object <- rep( list(object), length( grpForm ) )
      names( object ) <- names( grpForm )
    }
  } else if (inherits(object, "pdMat")) { # single group, as pdMat
    if (is.null(formula(object))) {
      stop("pdMat element must have a formula")
    }
    object <- list(object)
  } else {
    if (data.class(object) != "list") {
      stop("Object must be a list or a formula")
    }
    ## checking if nlme-type list - unnamed list of 2-sided formulas
    if (is.null(names(object)) &&
        all(unlist(lapply(object, function(el) {
          inherits(el, "formula") && length(el) == 3})))) {
      object <- list(object)
    } else {
      ## checking if elements are valid
      object <- lapply(object,
                       function(el) {
                         if (inherits(el, "pdMat")) {
                           if (is.null(formula(el))) {
                             stop("pdMat elements must have a formula")
                           }
                           return(el)
                         }
                         if (inherits(el, "formula")) {
                           grpForm <- getGroupsFormula(el)
                           if (!is.null(grpForm)) {
                             el <- getCovariateFormula(el)
                             attr(el, "grpName") <- deparse(grpForm[[2]])
                           }
                           return(el)
                         } else {
                           if (data.class(el) == "list" &&
                               all(unlist(lapply(el, function(el1) {
                                 inherits(el1, "formula") && length(el1) == 3
                               })))) { return(el) }
                           else {
                 stop("Elements in object must be formulas or pdMat objects")
                           }
                         }
		     })
    }
    if (is.null(namObj <- names(object))) {
      namObj <- rep("", length(object))
    }
    aux <- unlist(lapply(object,
			 function(el) {
			   if (inherits(el, "formula") &&
			       !is.null(attr(el, "grpName"))) {
			     attr(el, "grpName")
			   } else ""
			 }))
    auxNam <- namObj == ""
    if (any(auxNam)) {
      namObj[auxNam] <- aux[auxNam]
    }
    names(object) <- namObj
  }

  ## converting elements in object to pdMat objects
  object <- lapply(object,
		   function(el, pdClass, data) {
#                     if (data.class(el) == "pdSymm")
#                       warning("class pdSymm may cause problems if using analytic gradients")
		     pdMat(el, pdClass = pdClass, data = data)
		   }, pdClass = pdClass, data = data)

  object <- rev(object)			# inner to outer groups
  if (all(unlist(lapply(object, isInitialized)))) {
    plen <- unlist(lapply(object, function(el) length(coef(el))))
  }
  pC <- unlist(lapply(object, data.class))
  pC <- match(pC, c("pdSymm", "pdDiag", "pdIdent", "pdCompSymm",
                    "pdLogChol"), 0) - 1
#  if (any(pC == -1)) {                 # multiple nesting
#    pC <- -1
#  }
  ## at this point, always require asDelta = TRUE and gradHess = 0
  attr(object, "settings") <- c(as.integer(REML), 1, 0, pC)
  attr(object, "plen") <- plen
  class(object) <- "reStruct"
  object
}

###*# Methods for pdMat generics

corMatrix.reStruct <-
  function(object, ...)
{
  if (!isInitialized(object)) {
    stop("Cannot access the matrix of uninitialized objects")
  }
  as.list(rev(lapply(object, corMatrix)))
}

pdFactor.reStruct <-
  function(object)
{
  unlist(lapply(object, pdFactor))
}

pdMatrix.reStruct <-
  function(object, factor = FALSE)
{
  if (!isInitialized(object)) {
    stop("Cannot access the matrix of uninitialized objects")
  }
  as.list(rev(lapply(object, pdMatrix, factor)))
}

###*# Methods for standard generics

as.matrix.reStruct <-
  function(x, ...) pdMatrix(x)

coef.reStruct <-
  function(object, unconstrained = TRUE, ...)
{
  unlist(lapply(object, coef, unconstrained))
}

"coef<-.reStruct" <-
  function(object, ..., value)
{
  if (is.null(plen <- attr(object, "plen"))) {
    stop(paste("Cannot change the parameter when",
	       "length of parameters is undefined"))
  }
  if (length(value) != sum(plen)) {
    stop("Cannot change parameter length of initialized objects")
  }
  ends <- cumsum(plen)
  starts <- 1 + c(0, ends[-length(ends)])
  for (i in seq_along(object)) {
    coef(object[[i]]) <- value[(starts[i]):(ends[i])]
  }
  object
}

formula.reStruct <-
  function(x, asList = FALSE, ...)
{
  as.list(lapply(x, formula, asList))
}

getGroupsFormula.reStruct <-
  function(object, asList = FALSE, sep)
{
  if (is.null(val <- rev(formula(object)))) {
    stop("Can not extract groups formula without a formula")
  }
  if (is.null(nVal <- names(val))) return(NULL)
  if (asList) {
    for(i in nVal) {
      val[[i]] <- eval(parse(text = paste("~",i)))
    }
  } else {
    val <- eval(parse(text = paste("~",paste(nVal, collapse = "/"))))
  }
  val
}

isInitialized.reStruct <-
  function(object) all(unlist(lapply(object, isInitialized)))

Initialize.reStruct <-
  function(object, data, conLin, control = list(niterEM = 20), ...)
{
  ## initialize reStruct object, possibly getting initial estimates
  seqO <- seq_along(object)
  ## check if names are defined
  lNams <- unlist(lapply(object, function(el) length(Names(el)))) == 0
  if (any(lNams)) {			# need to resolve formula names
    aux <- seqO[lNams]
    object[aux] <- lapply(object[aux],
			  function(el, data) {
			    pdConstruct(el, el, data = data)
			  }, data = data)
  }
  ## obtaining the parameters mapping
  plen <- unlist(lapply(object, function(el)
			{
			  if (isInitialized(el)) {
			    length(coef(el))
			  } else {
			    matrix(el) <- diag(length(Names(el)))
			    length(coef(el))
			  }
			}))
  if (!all(plen > 0)) {
    stop("All elements of a reStruct object must have a non-zero size")
  }
  attr(object, "plen") <- plen

  ## checking initialization
  isIni <- unlist(lapply(object, isInitialized))
  if (!all(isIni)) {			# needs initialization
    dims <- conLin$dims
    Q <- dims$Q
    qvec <- dims$qvec[1:Q]
    auxInit <-
      lapply(split(0.375^2 * apply((conLin$Xy[, 1:sum(qvec), drop = FALSE])^2,
	     2, sum)/ rep(dims$ngrps[1:Q], qvec), rep(1:Q, qvec)),
	     function(x) diag(x, length(x)))
  }
  for(i in seqO) {
    if (isIni[i]) {
      object[[i]] <- solve(object[[i]])	#working with precisions
    } else {
      matrix(object[[i]]) <- auxInit[[i]]
    }
    NULL
  }
  MEEM(object, conLin, control$niterEM) # refine initial estimates with EM
}

logDet.reStruct <-
  function(object, ...)
{
  unlist(lapply(object, logDet))
}

logLik.reStruct <-
  function(object, conLin, ...)
{
  if(any(!is.finite(conLin$Xy))) return(-Inf)
  .C(mixed_loglik,
     as.double(conLin$Xy),
     as.integer(unlist(conLin$dims)),
     as.double(pdFactor(object)),
     as.integer(attr(object, "settings")),
     loglik = double(1),
     double(1))$loglik
}

"matrix<-.reStruct" <-
  function(object, value)
{
  if (data.class(value) != "list") value <- list(value)
  if (length(value) != length(object)) {
    stop("Cannot change the length of object")
  }
  value <- rev(value)                   # same order as object
  for(i in seq_along(object)) {
    matrix(object[[i]]) <- value[[i]]
  }
  object
}

model.matrix.reStruct <-
  function(object, data, contrast = NULL, ...)
{
  if (is.null(form <- formula(object, asList = TRUE))) {
    stop("Cannot extract model matrix without formula")
  }
  form1 <- asOneFormula(form)
  if (length(form1) > 0) {
    data <- model.frame(form1, data = data)
  } else {
    data <- data.frame("(Intercept)" = rep(1, nrow(data)))
  }
  any2list <- function( object, data, contrast ) {
    form2list <- function(form, data, contrast) {
      if (length(asOneFormula( form )) == 0) {# the ~ 1 case
        return(list("(Intercept)" = rep(1, dim(data)[1])))
      }
      as.data.frame(unclass(model.matrix(form,
                                         model.frame(form, data),
                                         contrast)))
    }
    if (inherits( object, "formula" )) {
      return( form2list( object, data, contrast ) )
    }
    if (is.list( object ) ) {
      return( unlist(lapply(object, form2list, data = data, contrast = contrast),
                     recursive = FALSE ) )
    }
    return( NULL)
  }
  value <- as.list(lapply(form, any2list,
                          data = data, contrast = contrast))
  ## save the contrasts currently in effect for later predictions
  contr <- as.list(lapply( as.data.frame(data), function(x)
                  if( inherits( x, "factor" ) &&
                     length(levels(x)) > 1) contrasts(x) else NULL ))
  contr[names(contrast)] <- contrast

  ncols <- as.vector(unlist(lapply(value, length)))
  nams <- if (length(value) == 1) {
    names(value[[1]])
  } else {
    paste(rep(names(value), ncols), unlist(lapply(value, names)), sep = ".")
  }
  val <- matrix(unlist(value), nrow = nrow(data),
                dimnames = list(row.names(data), nams))
  attr(val, "ncols") <- ncols
  attr(val, "nams") <- as.list(lapply(value, names))
  attr(val, "contr") <- contr
  val
}

Names.reStruct <-
    function(object, ...)
{
    as.list(lapply(object, Names))
}

"Names<-.reStruct" <-
  function(object, ..., value)
{
  if (length(object) != length(value)) {
    stop("Incompatible lengths for object names")
  }
  for(i in seq_along(object)) {
    Names(object[[i]]) <- value[[i]]
  }
  object
}

needUpdate.reStruct <-
  function(object) FALSE

print.reStruct <-
  function(x, sigma = 1, reEstimates, verbose = FALSE, ...)
{
  ox <- x
  if (isInitialized(x)) {
    nobj <- length(x)
    if (is.null(namx <- names(x))) names(x) <- nobj:1
    aux <- t(array(rep(names(x), nobj), c(nobj, nobj)))
    aux[lower.tri(aux)] <- ""
    x[] <- rev(x)
    names(x) <-
      rev(apply(aux, 1, function(x) paste(x[x != ""], collapse = " %in% ")))
    cat("Random effects:\n")
    for(i in seq_along(x)) {
      print(summary(x[[i]]), sigma, Level = names(x)[i],
            resid = (i == length(x)), ...)
      if (verbose) {
	cat("Random effects estimates:\n")
	print(reEstimates[[i]])
      }
      cat("\n")
    }
  } else {
    cat("Uninitialized random effects structure\n")
  }
  invisible(ox)
}

recalc.reStruct <-
  function(object, conLin, ...)
{
  conLin[["logLik"]] <- conLin[["logLik"]] + logLik(object, conLin)
  conLin
}

solve.reStruct <-
  function(a, b, ...)
{
  a[] <- lapply(a, solve)
  a
}

summary.reStruct <- function(object, ...) object

update.reStruct <-
  function(object, data, ...)
{
  object
}

"[.reStruct" <-
  function(x, ...)
{
  val <- NextMethod()
  if (length(val)) class(val) <- "reStruct"
  val
}

### Local variables:
### mode: S
### End:


