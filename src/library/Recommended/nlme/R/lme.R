###            Fit a general linear mixed effects model
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

lme <-
  ## fits general linear mixed effects model by maximum likelihood, or
  ## residual maximum likelihood using Newton-Raphson algorithm.
  function(fixed,
	   data = sys.frame(sys.parent()),
	   random,
	   correlation = NULL,
	   weights = NULL,
	   subset,
	   method = c("REML", "ML"),
	   na.action = na.fail,
	   control = list(),
           contrasts = NULL, keep.data = TRUE)
  UseMethod("lme")

lme.groupedData <-
  function(fixed,
	   data = sys.frame(sys.parent()),
	   random,
	   correlation = NULL,
	   weights = NULL,
	   subset,
	   method = c("REML", "ML"),
	   na.action = na.fail,
	   control = list(),
           contrasts = NULL, keep.data = TRUE)
{
  args <- as.list(match.call())[-1]
  names(args)[1] <- "data"
  form <- getResponseFormula(fixed)
  form[[3]] <- getCovariateFormula(fixed)[[2]]
  do.call("lme", c(list(fixed = form), args))
}

lme.lmList <-
  function(fixed,
	   data = sys.frame(sys.parent()),
	   random,
	   correlation = NULL,
	   weights = NULL,
	   subset,
	   method = c("REML", "ML"),
	   na.action = na.fail,
	   control = list(),
           contrasts = NULL, keep.data = TRUE)
{
  if (length(grpForm <- getGroupsFormula(fixed, asList = TRUE)) > 1) {
    stop("can only fit \"lmList\" objects with single grouping variable")
  }
  this.call <- as.list(match.call())[-1]
  ## warn "data" is passed to this function
  if (!is.na(match("data", names(this.call)))) {
    warning("'lme.lmList' will redefine 'data'")
  }
  ## add object, data, and groups from the call that created object
  last.call <- as.list(attr(fixed, "call"))[-1]
  whichLast <- match(c("object", "data", "na.action"), names(last.call))
  whichLast <- whichLast[!is.na(whichLast)]
  last.call <- last.call[whichLast]
  names(last.call)[match(names(last.call), "object")] <- "fixed"
  this.call[names(last.call)] <- last.call
  this.call$fixed <-
    as.vector(eval(parse(text=paste(deparse(getResponseFormula(fixed)[[2]]),
                   c_deparse(getCovariateFormula(fixed)[[2]]), sep="~"))))
  if (missing(random)) {
    random <- eval(as.call(this.call[["fixed"]][-2]))
  }
  random <- reStruct(random, data = NULL)
  mData <- this.call[["data"]]
  if (is.null(mData)) {			# will try to construct
    allV <- all.vars(formula(random))
    if (length(allV) > 0) {
      alist <- lapply(as.list(allV), as.name)
      names(alist) <- allV
      alist <- c(as.list(as.name("data.frame")), alist)
      mode(alist) <- "call"
      mData <- eval(alist, sys.parent(1))
    }
  } else {
    if (mode(mData) == "name" || mode(mData) == "call") {
      mData <- eval(mData)
    }
  }

  reSt <- reStruct(random, data = mData) # getting random effects names
  names(reSt) <- names(grpForm)
  if (length(reSt) > 1) {
    stop("can only fit \"lmList\" objects with single grouping variable")
  }
  rNames <- Names(reSt[[1]])
  if (all(match(rNames, names(cf <- na.omit(coef(fixed))), 0))) {
    if (isInitialized(reSt)) {
      warning("initial value for \"reStruct\" overwritten in 'lme.lmList'")
    }
    madRes <- mad(resid(fixed), na.rm = TRUE)
    madRan <- unlist(lapply(cf, mad, na.rm = TRUE)[rNames])
    names(madRan) <- rNames
    matrix(reSt) <- diag((madRan/madRes)^2, ncol = length(rNames))
  }
  this.call[["random"]] <- reSt
  val <- do.call("lme.formula", this.call)
  val$origCall <- match.call()
  val
}

lme.formula <-
  function(fixed,
	   data = sys.frame(sys.parent()),
	   random = pdSymm( eval( as.call( fixed[ -2 ] ) ) ),
	   correlation = NULL,
	   weights = NULL,
	   subset,
	   method = c("REML", "ML"),
	   na.action = na.fail,
	   control = list(),
           contrasts = NULL,
           keep.data = TRUE)
{
  Call <- match.call()
  miss.data <- missing(data) || !is.data.frame(data)

  ## control parameters
  controlvals <- lmeControl()
  if (!missing(control)) {
    if(!is.null(control$nlmStepMax) && control$nlmStepMax < 0) {
      warning("negative control$nlmStepMax - using default value")
      control$nlmStepMax <- NULL
    }
    controlvals[names(control)] <- control
  }

  ##
  ## checking arguments
  ##
  if (!inherits(fixed, "formula") || length(fixed) != 3) {
    stop("\nfixed-effects model must be a formula of the form \"resp ~ pred\"")
  }
  method <- match.arg(method)
  REML <- method == "REML"
  reSt <- reStruct(random, REML = REML, data = NULL)
  groups <- getGroupsFormula(reSt)
  if (is.null(groups)) {
    if (inherits(data, "groupedData")) {
      groups <- getGroupsFormula(data)
      namGrp <- rev(names(getGroupsFormula(data, asList = TRUE)))
      Q <- length(namGrp)
      if (length(reSt) != Q) { # may need to repeat reSt
	if (length(reSt) != 1) {
	  stop("incompatible lengths for 'random' and grouping factors")
	}
        randL <- vector("list", Q)
        names(randL) <- rev(namGrp)
        for(i in 1:Q) randL[[i]] <- random
        randL <- as.list(randL)
	reSt <- reStruct(randL, REML = REML, data = NULL)
      } else {
	names(reSt) <- namGrp
      }
    } else {
      ## will assume single group
      groups <- ~ 1
      names(reSt) <- "1"
    }
  }
  ## check if corStruct is present and assign groups to its formula,
  ## if necessary
  if (!is.null(correlation)) {
    if(!is.null(corGrpsForm <- getGroupsFormula(correlation, asList = TRUE))) {
      corGrpsForm <- unlist(lapply(corGrpsForm,
                                   function(el) deparse(el[[2]])))
      corQ <- length(corGrpsForm)
      lmeGrpsForm <- unlist(lapply(splitFormula(groups),
                        function(el) deparse(el[[2]])))
      lmeQ <- length(lmeGrpsForm)
      if (corQ <= lmeQ) {
        if (any(corGrpsForm != lmeGrpsForm[1:corQ])) {
          stop("incompatible formulas for groups in 'random' and 'correlation'")
        }
        if (corQ < lmeQ) {
          warning("cannot use smaller level of grouping for 'correlation' than for 'random'. Replacing the former with the latter.")
          attr(correlation, "formula") <-
            eval(parse(text = paste("~",
                    c_deparse(getCovariateFormula(formula(correlation))[[2]]),
                         "|", deparse(groups[[2]]))))
        }
      } else {
        if (any(lmeGrpsForm != corGrpsForm[1:lmeQ])) {
          stop("incompatible formulas for groups in 'random' and 'correlation'")
        }
      }
    } else {
      ## using the same grouping as in random
      attr(correlation, "formula") <-
        eval(parse(text = paste("~",
		     c_deparse(getCovariateFormula(formula(correlation))[[2]]),
		     "|", deparse(groups[[2]]))))
      corQ <- lmeQ <- 1
    }
    } else {
    corQ <- lmeQ <- 1
  }
  ## create an lme structure containing the random effects model and plug-ins
  lmeSt <- lmeStruct(reStruct = reSt, corStruct = correlation,
		     varStruct = varFunc(weights))

  ## extract a data frame with enough information to evaluate
  ## fixed, groups, reStruct, corStruct, and varStruct
  mfArgs <- list(formula = asOneFormula(formula(lmeSt), fixed, groups),
		 data = data, na.action = na.action)
  if (!missing(subset)) {
    mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2]]
  }
  mfArgs$drop.unused.levels <- TRUE
  dataMix <- do.call("model.frame", mfArgs)
  origOrder <- row.names(dataMix)	# preserve the original order
  for(i in names(contrasts))            # handle contrasts statement
      contrasts(dataMix[[i]]) = contrasts[[i]]
  ## sort the model.frame by groups and get the matrices and parameters
  ## used in the estimation procedures
  grps <- getGroups(dataMix, groups)
  ## ordering data by groups
  if (inherits(grps, "factor")) {	# single level
    ord <- order(grps)	#"order" treats a single named argument peculiarly
    grps <- data.frame(grps)
    row.names(grps) <- origOrder
    names(grps) <- as.character(deparse((groups[[2]])))
  } else {
    ord <- do.call("order", grps)
    ## making group levels unique
    for(i in 2:ncol(grps)) {
      grps[, i] <-
        as.factor(paste(as.character(grps[, i-1]), as.character(grps[,i]),
                        sep = "/"))
      NULL
    }
  }
  if (corQ > lmeQ) {
    ## may have to reorder by the correlation groups
    ord <- do.call("order", getGroups(dataMix,
                                 getGroupsFormula(correlation)))
  }
  grps <- grps[ord, , drop = FALSE]
  dataMix <- dataMix[ord, ,drop = FALSE]
  revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order

  ## obtaining basic model matrices
  N <- nrow(grps)
  Z <- model.matrix(reSt, dataMix)
  ncols <- attr(Z, "ncols")
  Names(lmeSt$reStruct) <- attr(Z, "nams")
  ## keeping the contrasts for later use in predict
  contr <- attr(Z, "contr")
  X <- model.frame(fixed, dataMix)
  Terms <- attr(X, "terms")
  auxContr <- lapply(X, function(el)
		     if (inherits(el, "factor") &&
                         length(levels(el)) > 1) contrasts(el))
  contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
  contr <- contr[!unlist(lapply(contr, is.null))]
  X <- model.matrix(fixed, data=X)
  y <- eval(fixed[[2]], dataMix)
  ncols <- c(ncols, dim(X)[2], 1)
  Q <- ncol(grps)
  ## creating the condensed linear model
  attr(lmeSt, "conLin") <-
    list(Xy = array(c(Z, X, y), c(N, sum(ncols)),
	     list(row.names(dataMix), c(colnames(Z), colnames(X),
					deparse(fixed[[2]])))),
	 dims = MEdims(grps, ncols), logLik = 0)
  ## checking if enough observations per group to estimate ranef
  tmpDims <- attr(lmeSt, "conLin")$dims
  if (max(tmpDims$ZXlen[[1]]) < tmpDims$qvec[1]) {
      warning(gettextf("fewer observations than random effects in all level %s groups",
                       Q), domain = NA)
  }
  ## degrees of freedom for testing fixed effects
  fixDF <- getFixDF(X, grps, attr(lmeSt, "conLin")$dims$ngrps,
                    terms = Terms)
  ## initialization
  lmeSt <- Initialize(lmeSt, dataMix, grps, control = controlvals)
  parMap <- attr(lmeSt, "pmap")
  ## Checking possibility of single decomposition
  if (length(lmeSt) == 1)  {	# reStruct only, can do one decomposition
    ## need to save conLin for calculating fitted values and residuals
    oldConLin <- attr(lmeSt, "conLin")
    decomp <- TRUE
    attr(lmeSt, "conLin") <- MEdecomp(attr(lmeSt, "conLin"))
  } else decomp <- FALSE
  ##
  ## getting the linear mixed effects fit object,
  ## possibly iterating for variance functions
  ##
  numIter <- 0
  repeat {
    oldPars <- coef(lmeSt)
    optRes <- if (controlvals$opt == "nlminb") {
        control <- list(iter.max = controlvals$msMaxIter,
                        eval.max = controlvals$msMaxEval,
                        trace = controlvals$msVerbose)
        keep <- c("abs.tol", "rel.tol", "x.tol", "xf.tol", "step.min",
                  "step.max", "sing.tol", "scale.init", "diff.g")
        control <- c(control, controlvals[names(controlvals) %in% keep])
        nlminb(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, lmePars),
               control = control)
    } else {
        reltol <- controlvals$reltol
        if(is.null(reltol))  reltol <- 100*.Machine$double.eps
        control <- list(trace = controlvals$msVerbose,
                        maxit = controlvals$msMaxIter,
                        reltol = if(numIter == 0) controlvals$msTol else reltol)
        keep <- c("fnscale", "parscale", "ndeps", "abstol", "alpha", "beta",
                  "gamma", "REPORT", "type", "lmm", "factr", "pgtol",
                  "temp", "tmax")
        control <- c(control, controlvals[names(controlvals) %in% keep])
        optim(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, lmePars),
              control = control, method = controlvals$optimMethod)
    }
    numIter0 <- NULL
    coef(lmeSt) <- optRes$par
    attr(lmeSt, "lmeFit") <- MEestimate(lmeSt, grps)
    ## checking if any updating is needed
    if (!needUpdate(lmeSt)) {
	if (optRes$convergence) {
	    msg <- paste(controlvals$opt, " problem, convergence error code = ",
			 optRes$convergence, "\n  message = ", optRes$message,
			 sep='')
	    if(!controlvals$returnObject)
		stop(msg, domain = NA)
	    else
		warning(msg, domain = NA)
	}
	break
    }

    ## updating the fit information
    numIter <- numIter + 1
    lmeSt <- update(lmeSt, dataMix)
    ## calculating the convergence criterion
    aConv <- coef(lmeSt)
    conv <- abs((oldPars - aConv)/ifelse(aConv == 0, 1, aConv))
    aConv <- NULL
    for(i in names(lmeSt)) {
	if (any(parMap[,i])) {
	    aConv <- c(aConv, max(conv[parMap[,i]]))
	    names(aConv)[length(aConv)] <- i
	}
    }
    if (max(aConv) <= controlvals$tolerance) {
	break
    }
    if (numIter > controlvals$maxIter) {
	msg <- paste("maximum number of iterations",
		     "(lmeControl(maxIter)) reached without convergence")
	if (controlvals$returnObject) {
	    warning(msg, domain = NA)
	    break
	} else
	    stop(msg, domain = NA)
    }

  } ## end{repeat}

  ## wrapping up
  lmeFit <- attr(lmeSt, "lmeFit")
  names(lmeFit$beta) <- namBeta <- colnames(X)
  attr(fixDF, "varFixFact") <- varFix <- lmeFit$sigma * lmeFit$varFix
  varFix <- crossprod(varFix)
  dimnames(varFix) <- list(namBeta, namBeta)
  ##
  ## fitted.values and residuals (in original order)
  ##
  Fitted <- fitted(lmeSt, level = 0:Q,
		   conLin = if (decomp) oldConLin else attr(lmeSt, "conLin"))[
		   revOrder, , drop = FALSE]
  Resid <- y[revOrder] - Fitted
  rownames(Resid) <- rownames(Fitted) <- origOrder
  attr(Resid, "std") <- lmeFit$sigma/(varWeights(lmeSt)[revOrder])
  ## putting groups back in original order
  grps <- grps[revOrder, , drop = FALSE]
  ## making random effects estimates consistently ordered
#  for(i in names(lmeSt$reStruct)) {
#    lmeFit$b[[i]] <- lmeFit$b[[i]][unique(as.character(grps[, i])),, drop = F]
#    NULL
#  }
  ## inverting back reStruct
  lmeSt$reStruct <- solve(lmeSt$reStruct)
  ## saving part of dims
  dims <- attr(lmeSt, "conLin")$dims[c("N", "Q", "qvec", "ngrps", "ncol")]
  ## getting the approximate var-cov of the parameters
  if (controlvals$apVar) {
    apVar <- lmeApVar(lmeSt, lmeFit$sigma,
		      .relStep = controlvals[[".relStep"]],
                      minAbsPar = controlvals[["minAbsParApVar"]],
		      natural = controlvals[["natural"]])
  } else {
    apVar <- "Approximate variance-covariance matrix not available"
  }
  ## getting rid of condensed linear model and fit
  attr(lmeSt, "conLin") <- NULL
  attr(lmeSt, "lmeFit") <- NULL
  ##
  ## creating the  lme object
  ##
  estOut <- list(modelStruct = lmeSt,
		 dims = dims,
		 contrasts = contr,
		 coefficients = list(
		     fixed = lmeFit$beta,
		     random = lmeFit$b),
		 varFix = varFix,
		 sigma = lmeFit$sigma,
		 apVar = apVar,
		 logLik = lmeFit$logLik,
		 numIter = if (needUpdate(lmeSt)) numIter
		   else numIter0,
		 groups = grps,
		 call = Call,
                 terms = Terms,
		 method = method,
		 fitted = Fitted,
		 residuals = Resid,
                 fixDF = fixDF,
                 na.action = attr(dataMix, "na.action"))
  if (keep.data && !miss.data) estOut$data <- data
  if (inherits(data, "groupedData")) {
    ## saving labels and units for plots
    attr(estOut, "units") <- attr(data, "units")
    attr(estOut, "labels") <- attr(data, "labels")
  }
  class(estOut) <- "lme"
  estOut
}

### Auxiliary functions used internally in lme and its methods

getFixDF <-
  function(X, grps, ngrps, assign = attr(X, "assign"), terms)
{
  ## calculates degrees of freedom for fixed effects Wald tests
  if (!is.list(assign)) {               # in R
    namTerms <- attr(terms, "term.labels")
    if (attr(terms, "intercept") > 0) {
      namTerms <- c("(Intercept)", namTerms)
    }
    namTerms <- factor(assign, labels = namTerms)
    assign <- split(order(assign), namTerms)
  }
  ## function to check if a vector is (nearly) a multiple of (1,1,...,1)
  const <- function(x, tolerance = sqrt(.Machine$double.eps)) {
      if (length(x) < 1) return(NA)
      x <- as.numeric(x)
      if (x[1] == 0.) return(all(abs(x) < tolerance))
      all(abs((x/x[1] - 1.)) < tolerance)
  }
  N <- nrow(X)
  p <- ncol(X)
  Q <- ncol(grps)
  Qp1 <- Q + 1
  namX <- colnames(X)
  ngrps <- rev(ngrps)[-(1:2)]
  stratNam <- c(names(ngrps), "Residual")
  dfX <- dfTerms <- c(ngrps, N) - c(0, ngrps)
  names(dfX) <- names(dfTerms) <- stratNam
  valX <- double(p)
  names(valX) <- namX
  namTerms <- names(assign)
  valTerms <- double(length(assign))
  names(valTerms) <- namTerms
  if (any(notIntX <- !apply(X, 2, const))) {
      ## percentage of groups for which columns of X are inner
      innP <- array(c(rep(1, p),
                      .C(inner_perc_table,
                         as.double(X),
                         as.integer(unlist(grps)),
                         as.integer(p),
                         as.integer(Q),
                         as.integer(N),
                         val = double(p * Q))[["val"]]), c(p, Qp1),
                    list(namX, stratNam))
    ## strata in which columns of X are estimated
    ## ignoring fractional inner percentages for now
    stratX <- stratNam[apply(innP, 1, function(el, index) max(index[el > 0]),
                             index = 1:Qp1)]
    ## strata in which terms are estimated
    notIntTerms <- unlist(lapply(assign,
                                 function(el, notIntX) {
                                   any(notIntX[el])
                                 }, notIntX = notIntX))
    stratTerms <- stratNam[unlist(lapply(assign,
                          function(el, stratX, stratNam) {
                            max(match(stratX[el], stratNam))
                          },
                       stratX = stratX, stratNam = stratNam))][notIntTerms]
    stratX <- stratX[notIntX]
    xDF <- table(stratX)
    dfX[names(xDF)] <- dfX[names(xDF)] - xDF
    if (!all(notIntX)) {                # correcting df for intercept
      dfX[1] <- dfX[1] - 1
    } else {
      dfX[-1] <- dfX[-1] + 1
    }
    valX[notIntX] <- dfX[stratX]
    ## number of parameters in each term
    pTerms <- unlist(lapply(assign, length))[notIntTerms]
    tDF <- tapply(pTerms, stratTerms, sum)
    dfTerms[names(tDF)] <- dfTerms[names(tDF)] - tDF
    if (!all(notIntTerms)) {
      dfTerms[1] <- dfTerms[1] - 1
    } else {
      dfTerms[-1] <- dfTerms[-1] + 1
    }
    valTerms[notIntTerms] <- dfTerms[stratTerms]
  } else {
    notIntTerms <- unlist(lapply(assign,
                                 function(el, notIntX) {
                                   any(notIntX[el])
                                 }, notIntX = notIntX))
  }
  if (!all(notIntX)) {  #intercept included
    valX[!notIntX] <- max(dfX)
    if (!all(notIntTerms)) {
      valTerms[!notIntTerms] <- max(dfTerms)
    }
  }
  val <- list(X = valX, terms = valTerms)
  attr(val, "assign") <- assign
  val
}

lmeApVar <-
  function(lmeSt, sigma, conLin = attr(lmeSt, "conLin"),
           .relStep = (.Machine$double.eps)^(1/3), minAbsPar = 0,
           natural = TRUE)
{
  ## calculate approximate variance-covariance matrix of all parameters
  ## except the fixed effects. By default, uses natural parametrization for
  ## for pdSymm matrices
  fullLmeLogLik <-
    function(Pars, object, conLin, dims, N, settings) {
      ## logLik as a function of sigma and coef(lmeSt)
      npar <- length(Pars)
      sigma <- exp(Pars[npar])              # within-group std. dev.
      Pars <- Pars[-npar]
      coef(object) <- Pars
      if ((lO <- length(object)) > 1) {
	for(i in lO:2) {
	  conLin <- recalc(object[[i]], conLin)
	  NULL
	}
      }
      val <- .C(mixed_loglik,
		as.double(conLin$Xy),
		as.integer(unlist(dims)),
		as.double(sigma * unlist(pdFactor(solve(object$reStruct)))),
		as.integer(settings),
		logLik = double(1),
		lRSS = double(1))[c("logLik", "lRSS")]
      aux <- (exp(val[["lRSS"]])/sigma)^2
      conLin[["logLik"]] + val[["logLik"]] + (N * log(aux) - aux)/2
    }
  dims <- conLin$dims
  sett <- attr(lmeSt, "settings")
  N <- dims$N - sett[1] * dims$ncol[dims$Q + 1]
  sett[2:3] <- c(1, 0)			# asDelta = TRUE and no grad/Hess
  conLin[["logLik"]] <- 0               # making sure
  sig2 <- sigma * sigma
  reSt <- lmeSt[["reStruct"]]
  for(i in seq_along(reSt)) {
    matrix(reSt[[i]]) <- as.double(sig2) * pdMatrix(reSt[[i]])
    if (inherits(reSt[[i]], "pdSymm") && natural) {
      reSt[[i]] <- pdNatural(reSt[[i]])
    }
    if (inherits(reSt[[i]], "pdBlocked") && natural) {
      for(j in seq_along(reSt[[i]])) {
        if (inherits(reSt[[i]][[j]], "pdSymm")) {
          reSt[[i]][[j]] <- pdNatural(reSt[[i]][[j]])
        }
      }
    }
  }
  lmeSt[["reStruct"]] <- reSt
  cSt <- lmeSt[["corStruct"]]
  if (!is.null(cSt) && inherits(cSt, "corSymm") && natural) {
    cStNatPar <- coef(cSt, unconstrained = FALSE)
    class(cSt) <- c("corNatural", "corStruct")
    coef(cSt) <- log((cStNatPar + 1)/(1 - cStNatPar))
    lmeSt[["corStruct"]] <- cSt
  }
  Pars <- c(coef(lmeSt), lSigma = log(sigma))
  val <- fdHess(Pars, fullLmeLogLik, lmeSt, conLin, dims, N, sett,
		.relStep = .relStep, minAbsPar = minAbsPar)[["Hessian"]]
  if (all(eigen(val)$values < 0)) {
    ## negative definite - OK
    val <- solve(-val)
    nP <- names(Pars)
    dimnames(val) <- list(nP, nP)
    attr(val, "Pars") <- Pars
    attr(val, "natural") <- natural
    val
  } else {
    ## problem - solution is not a maximum
    "Non-positive definite approximate variance-covariance"
  }
}

MEdecomp <-
 function(conLin)
  ## decompose a condensed linear model.  Returns another condensed
  ## linear model
{
  dims <- conLin$dims
  if (dims[["StrRows"]] >= dims[["ZXrows"]]) {
    ## no pint in doing the decomposition
    return(conLin)
  }
  dc <- array(.C(mixed_decomp,
		 as.double(conLin$Xy),
		 as.integer(unlist(dims)))[[1]],
	      c(dims$StrRows, dims$ZXcols))
  dims$ZXrows <- dims$StrRows
  dims$ZXoff <- dims$DecOff
  dims$ZXlen <- dims$DecLen
  conLin[c("Xy", "dims")] <- list(Xy = dc, dims = dims)
  conLin
}

MEEM <-
  function(object, conLin, niter = 0)
  ## perform niter iterations of the EM algorithm for conLin
  ## assumes that object is in precision form
{
  if (niter > 0) {
    dd <- conLin$dims
    pdCl <- attr(object, "settings")[-(1:3)]
    pdCl[pdCl == -1] <- 0
    precvec <- unlist(pdFactor(object))
    zz <- .C(mixed_EM,
	     as.double(conLin$Xy),
	     as.integer(unlist(dd)),
	     precvec = as.double(precvec),
	     as.integer(niter),
	     as.integer(pdCl),
	     as.integer(attr(object, "settings")[1]),
	     double(1),
	     double(length(precvec)),
	     double(1))[["precvec"]]
    Prec <- vector("list", length(object))
    names(Prec) <- names(object)
    for (i in seq_along(object)) {
      len <- dd$qvec[i]^2
      matrix(object[[i]]) <-
        crossprod(matrix(zz[1:len + dd$DmOff[i]], ncol = dd$qvec[i]))
    }
  }
  object
}

MEestimate <-
  function(object, groups, conLin = attr(object, "conLin"))
{
  dd <- conLin$dims
  nc <- dd$ncol
  REML <- attr(object$reStruct, "settings")[1]
  Q <- dd$Q
  rConLin <- recalc(object, conLin)
  zz <- .C(mixed_estimate,
	   as.double(rConLin$Xy),
	   as.integer(unlist(dd)),
	   as.double(unlist(pdFactor(object$reStruct))),
	   as.integer(REML),
	   double(1),
	   estimates = double(dd$StrRows * dd$ZXcols),
	   as.logical(FALSE))[["estimates"]]
  estimates <- array(zz, c(dd$StrRows, dd$ZXcols))
  resp <- estimates[ , dd$ZXcols]
  reSt <- object$reStruct
  nam <- names(reSt)
  val <- vector(mode = "list", length = Q)
  names(val) <- nam
  start <- dd$StrRows * c(0, cumsum(nc))
  for (i in seq_along(reSt)) {
    val[[i]] <-
      matrix(resp[as.vector(outer(1:(nc[i]), dd$SToff[[i]] - start[i], "+"))],
	     ncol = nc[i], byrow = TRUE,
	     dimnames = list(unique(as.character(groups[, nam[i]])),
		 Names(reSt[[i]])))
    NULL
  }
  p <- nc[Q + 1]
  N <- dd$N - REML * p
  dimE <- dim(estimates)
  list(logLik = N * (log(N) - (1 + log(2 * pi)))/2 + rConLin$logLik,
       b = rev(val),
       beta = resp[dimE[1] - (p:1)],
       sigma = abs(resp[dimE[1]])/sqrt(N),
       varFix = t(solve(estimates[dimE[1]-(p:1), dimE[2]-(p:1), drop = FALSE])))
}

MEdims <-
  function(groups, ncols)
{
  ## define constants used in matrix decompositions and log-lik calculations
  ## first need some local functions
  lengths <-
    ## returns the group lengths from a vector of last rows in the group
    function(lstrow) diff(c(0, lstrow))
  offsets <-
    ## converts total number of columns(N), columns per level(ncols), and
    ## a list of group lengths to offsets in C arrays
    function(N, ncols, lstrow, triangle = FALSE)
  {
    pop <- function(x) x[-length(x)]
    cstart <- c(0, cumsum(N * ncols))
    for (i in seq_along(lstrow)) {
      lstrow[[i]] <- cstart[i] +
        if (triangle) {
          lstrow[[i]] - ncols[i]        # storage offsets style
        } else {
          pop(c(0, lstrow[[i]]))        # decomposition style
        }
    }
    lstrow
  }
  Q <- ncol(groups)                     # number of levels
  N <- nrow(groups)                     # number of observations
  ## 'isLast' indicates if the row is the last row in the group at that level.
  ## this version propagates changes from outer groups to inner groups
#  isLast <- (array(unlist(lapply(c(rev(as.list(groups)),
#                                 list(X = rep(0, N), y = rep(0, N))),
#                                function(x) c(0 != diff(codes(x)), TRUE))),
#                  c(N, Q+2), list(NULL, c(rev(names(groups)), "X", "y")))
#             %*% (row(diag(Q+2)) >= col(diag(Q+2)))) != 0
  ## this version does not propagate changes from outer to inner.
  isLast <- array(FALSE, dim(groups) + c(0, 2),
                  list(NULL, c(rev(names(groups)), "X", "y")))
  for(i in 1:Q) {
    isLast[, Q + 1 - i] <- c(0 != diff(as.integer(groups[[i]])), TRUE)
  }
  isLast[N,  ] <- TRUE
  lastRow <- apply(isLast, 2, function(x) seq_along(x)[x])
  if(!is.list(lastRow)) {
    nm <- names(lastRow)
    lastRow <- as.list(lastRow)
    names(lastRow) <- nm
  }

  isLast <- t(isLast)
  strSizes <- cumsum(ncols * isLast) * isLast # required storage sizes
  lastStr <- apply(t(strSizes), 2, function(x) x[x != 0])
  if(!is.list(lastStr)) {
    nm <- names(lastStr)
    lastStr <- as.list(lastStr)
    names(lastStr) <- nm
  }
  strRows <- max(lastStr[[length(lastStr)]])
  lastBlock <- vector("list", Q)
  names(lastBlock) <- rownames(strSizes)[1:Q]
  for(i in 1:Q) lastBlock[[i]] <- c(strSizes[i, -N], strRows)
  maxStr <- do.call("pmax", lastBlock)
  for(i in 1:Q) lastBlock[[i]] <- maxStr[as.logical(lastBlock[[i]])]
  lastBlock <- c(lastBlock, list(X = strRows, y = strRows))
  list(N = N,                   # total number of rows in data
       ZXrows = N,              # no. of rows in array
       ZXcols = sum(ncols),     # no. of columns in array
       Q = Q,                   # no. of levels of random effects
       StrRows = strRows,       # no. of rows required for storage
       qvec = ncols * c(rep(1, Q), 0, 0), # lengths of random effects
                                        # no. of groups at each level
       ngrps = c(unlist(lapply(lastRow, length), N, N)),
                                        # offsets into DmHalf array by level
       DmOff = (c(0, cumsum(ncols^2)))[1:(Q+2)],
       ncol = ncols,            # no. of columns decomposed per level
                                        # no. of columns rotated per level
       nrot = (rev(c(0, cumsum(rev(ncols)))))[-1],
       ZXoff = offsets(N, ncols, lastRow), # offsets into ZXy
       ZXlen = lapply(lastRow, lengths), # lengths of ZXy groups
                                        # storage array offsets
       SToff = offsets(strRows, ncols, lastStr, triangle = TRUE),
                                        # decomposition offsets
       DecOff = offsets(strRows, ncols, lastBlock),
                                        # decomposition lengths
       DecLen = lapply(lastBlock, lengths)
       )
}

### Methods for standard generics

ACF.lme <-
  function(object, maxLag,
           resType = c("pearson", "response", "normalized"), ...)
{
  resType <- match.arg(resType)
  res <- resid(object, type = resType, asList = TRUE)
  if(missing(maxLag)) {
    maxLag <- min(c(maxL <- max(sapply(res, length)) - 1,
                    as.integer(10 * log10(maxL + 1))))
  }
  val <- lapply(res,
                function(el, maxLag) {
                  N <- maxLag + 1
                  tt <- double(N)
                  nn <- integer(N)
                  N <- min(c(N, n <- length(el)))
                  nn[1:N] <- n + 1 - 1:N
                  ## el <- el - mean(el)
                  for(i in 1:N) {
                    tt[i] <- sum(el[1:(n-i+1)] * el[i:n])
                  }
                  array(c(tt,nn), c(length(tt), 2))
                }, maxLag = maxLag)
  val0 <- apply(sapply(val, function(x) x[,2]), 1, sum)
  val1 <- apply(sapply(val, function(x) x[,1]), 1, sum)/val0
  val2 <- val1/val1[1]
  z <- data.frame(lag = 0:maxLag, ACF = val2)
  attr(z, "n.used") <- val0
  class(z) <- c("ACF", "data.frame")
  z
}


anova.lme <-
  function(object, ..., test = TRUE, type = c("sequential", "marginal"),
           adjustSigma = TRUE, Terms, L, verbose = FALSE)

{
  ## returns the likelihood ratio statistics, the AIC, and the BIC
  Lmiss <- missing(L)
  dots <- list(...)
  if ((rt <- (length(dots) + 1)) == 1) {    ## just one object
    if (!inherits(object,"lme")) {
      stop("object must inherit from class \"lme\" ")
    }
    vFix <- attr(object$fixDF, "varFixFact")
    if (adjustSigma && object$method == "ML")
      ## using REML-like estimate of sigma under ML
      vFix <- sqrt(object$dims$N/(object$dims$N - ncol(vFix))) * vFix
    c0 <- solve(t(vFix), fixef(object))
    assign <- attr(object$fixDF, "assign")
    nTerms <- length(assign)
    if (missing(Terms) && Lmiss) {
      ## returns the F.table (Wald) for the fixed effects
      type <- match.arg(type)
      Fval <- Pval <- double(nTerms)
      nDF <- integer(nTerms)
      dDF <- object$fixDF$terms
      for(i in 1:nTerms) {
        nDF[i] <- length(assign[[i]])
        if (type == "sequential") {       # type I SS
          c0i <- c0[assign[[i]]]
        } else {
          c0i <- c(qr.qty(qr(vFix[, assign[[i]], drop = FALSE]), c0))[1:nDF[i]]
        }
        Fval[i] <- sum(c0i^2)/nDF[i]
        Pval[i] <- 1 - pf(Fval[i], nDF[i], dDF[i])
      }
      ##
      ## fixed effects F-values, df, and p-values
      ##
      aod <- data.frame(nDF, dDF, Fval, Pval)
      dimnames(aod) <-
        list(names(assign),c("numDF","denDF","F-value", "p-value"))
      attr(aod,"rt") <- rt
    } else {
      nX <- length(unlist(assign))
      if (Lmiss) {                 # terms is given
        if (is.numeric(Terms) && all(Terms == as.integer(Terms))) {
          if (min(Terms) < 1 || max(Terms) > nTerms) {
              stop(gettextf("'Terms' must be between 1 and %d", nTerms),
                   domain = NA)
          }
        } else {
          if (is.character(Terms)) {
            if (any(noMatch <- is.na(match(Terms, names(assign))))) {
                stop(sprintf(ngettext(sum(noMatch),
                                      "term %s not matched",
                                      "terms %s not matched"),
                             paste(Terms[noMatch], collapse = ", ")),
                     domain = NA)
            }
          } else {
            stop("terms can only be integers or characters")
          }
        }
        dDF <- unique(object$fixDF$terms[Terms])
        if (length(dDF) > 1) {
          stop("terms must all have the same denominator DF")
        }
        lab <-
          paste("F-test for:",paste(names(assign[Terms]),collapse=", "),"\n")
        L <- diag(nX)[unlist(assign[Terms]),,drop=FALSE]
      } else {
        L <- as.matrix(L)
        if (ncol(L) == 1) L <- t(L)     # single linear combination
        nrowL <- nrow(L)
        ncolL <- ncol(L)
        if (ncol(L) > nX) {
          stop(sprintf(ngettext(nX,
                                "'L' must have at most %d column",
                                "'L' must have at most %d columns"),
                       nX), domain = NA)
       }
        dmsL1 <- rownames(L)
        L0 <- array(0, c(nrowL, nX), list(NULL, names(object$fixDF$X)))
        if (is.null(dmsL2 <- colnames(L))) {
          ## assume same order as effects
          L0[, 1:ncolL] <- L
        } else {
          if (any(noMatch <- is.na(match(dmsL2, colnames(L0))))) {
              stop(sprintf(ngettext(sum(noMatch),
                                    "effect %s not matched",
                                    "effects %s not matched"),
                           paste(dmsL2[noMatch],collapse=", ")),
                   domain = NA)
          }
          L0[, dmsL2] <- L
        }
        L <- L0[noZeroRowL <- as.logical((L0 != 0) %*% rep(1, nX)), , drop = FALSE]
        nrowL <- nrow(L)
        if (is.null(dmsL1)) {
          dmsL1 <- 1:nrowL
        } else {
          dmsL1 <- dmsL1[noZeroRowL]
        }
        rownames(L) <- dmsL1
        dDF <-
         unique(object$fixDF$X[noZeroColL <-
                               as.logical(c(rep(1,nrowL) %*% (L != 0)))])
        if (length(dDF) > 1) {
          stop("L may only involve fixed effects with the same denominator DF")
        }
        lab <- "F-test for linear combination(s)\n"
      }
      nDF <- sum(svd(L)$d > 0)
      c0 <- c(qr.qty(qr(vFix %*% t(L)), c0))[1:nDF]
      Fval <- sum(c0^2)/nDF
      Pval <- 1 - pf(Fval, nDF, dDF)
      aod <- data.frame(nDF, dDF, Fval, Pval)
      names(aod) <- c("numDF", "denDF", "F-value", "p-value")
      attr(aod, "rt") <- rt
      attr(aod, "label") <- lab
      if (!Lmiss) {
        if (nrow(L) > 1) attr(aod, "L") <- L[, noZeroColL, drop = FALSE]
        else attr(aod, "L") <- L[, noZeroColL]
      }
    }
  }

  ##
  ## Otherwise construct the likelihood ratio and information table
  ## objects in ... may inherit from gls, gnls, lm, lmList, lme,
  ## nlme, nlsList, and nls
  ##
  else {
    ancall <- sys.call()
    ancall$verbose <- ancall$test <- NULL
    object <- list(object, ...)
    termsClass <- unlist(lapply(object, data.class))
    if(!all(match(termsClass, c("gls", "gnls", "lm", "lmList", "lme","nlme","nlsList","nls"), 0))) {
      stop("objects must inherit from classes \"gls\", \"gnls\",\"lm\",\"lmList\", \"lme\",\"nlme\",\"nlsList\", or \"nls\"")
    }
    resp <- unlist(lapply(object,
		  function(el) deparse(getResponseFormula(el)[[2]])))
    ## checking if responses are the same
    subs <- as.logical(match(resp, resp[1], FALSE))
    if (!all(subs))
      warning("some fitted objects deleted because response differs from the first model")
    if (sum(subs) == 1)
      stop("first model has a different response from the rest")
    object <- object[subs]
    rt <- length(object)
    termsModel <- lapply(object, function(el) formula(el)[-2])
    estMeth <- unlist(lapply(object,
			     function(el) {
			       val <- el[["method"]]
			       if (is.null(val)) val <- NA
			       val
			     }))
    ## checking consistency of estimation methods
    if(length(uEst <- unique(estMeth[!is.na(estMeth)])) > 1) {
      stop("all fitted objects must have the same estimation method")
    }
    estMeth[is.na(estMeth)] <- uEst
    ## checking if all models have same fixed effects when estMeth = "REML"
    REML <- uEst == "REML"
    if(REML) {
      aux <- unlist(lapply(termsModel,
                           function(el) {
                             aux <- terms(el)
                             val <- paste(sort(attr(aux, "term.labels")),
                                          collapse = "&")
                             if (attr(aux, "intercept") == 1) {
                               val <- paste(val, "(Intercept)", sep = "&")
                             }
                             val
                           }))
      if(length(unique(aux)) > 1) {
        warning("fitted objects with different fixed effects.", " ",
                "REML comparisons are not meaningful.")
      }
    }
    termsCall <-
      lapply(object, function(el) {
        if (is.null(val <- el$call)) {
          if (is.null(val <- attr(el, "call"))) {
            stop("objects must have a \"call\" component or attribute")
          }
        }
        val
      })
    termsCall <- unlist(lapply(termsCall,
			       function(el) paste(deparse(el), collapse ="")))

    aux <- lapply(object, logLik, REML)
    if (length(unique(unlist(lapply(aux, function(el) attr(el, "nall")))))>1){
      stop("all fitted objects must use the same number of observations")
    }
    dfModel <- unlist(lapply(aux, function(el) attr(el, "df")))
    logLik <- unlist(lapply(aux, function(el) c(el)))
    AIC <- unlist(lapply(aux, AIC))
    BIC <- unlist(lapply(aux, BIC))
    aod <- data.frame(call = termsCall,
		      Model = (1:rt),
		      df = dfModel,
		      AIC = AIC,
		      BIC = BIC,
		      logLik = logLik,
		      check.names = FALSE)
    if (test) {
      ddf <-  diff(dfModel)
      if (sum(abs(ddf)) > 0) {
	effects <- rep("", rt)
	for(i in 2:rt) {
	  if (ddf[i-1] != 0) {
	    effects[i] <- paste(i - 1, i, sep = " vs ")
	  }
	}
	pval <- rep(NA, rt - 1)
	ldf <- as.logical(ddf)
	lratio <- 2 * abs(diff(logLik))
	lratio[!ldf] <- NA
	pval[ldf] <- 1 - pchisq(lratio[ldf],abs(ddf[ldf]))
	aod <- data.frame(aod,
			  Test = effects,
			  "L.Ratio" = c(NA, lratio),
			  "p-value" = c(NA, pval),
			  check.names = FALSE)
      }
    }
    row.names(aod) <- unlist(lapply(as.list(ancall[-1]), deparse))
    attr(aod, "rt") <- rt
    attr(aod, "verbose") <- verbose
  }
  class(aod) <- c("anova.lme", "data.frame")
  aod
}

augPred.lme <-
  function(object, primary = NULL, minimum = min(primary),
	   maximum = max(primary), length.out = 51, level = Q, ...)
{
  data <- eval(object$call$data)
  if (!inherits(data, "data.frame")) {
       stop(gettextf("data in %s call must evaluate to a data frame",
                      sQuote(substitute(object))), domain = NA)
  }
  if(is.null(primary)) {
    if (!inherits(data, "groupedData")) {
        stop(gettextf("%s without \"primary\" can only be used with fits of \"groupedData\" objects",
                      sys.call()[[1]]), domain = NA)
    }
    primary <- getCovariate(data)
    prName <- deparse(getCovariateFormula(data)[[2]])
  } else{
    primary <- asOneSidedFormula(primary)[[2]]
    prName <- deparse(primary)
    primary <- eval(primary, data)
  }
  newprimary <- seq(from = minimum, to = maximum, length.out = length.out)

  Q <- object$dims$Q                    # number of levels
  if (is.null(level)) level <- Q
  nL <- length(level)                   # number of requested levels
  maxLev <- max(c(level, 1))
  groups <- getGroups(object, level = maxLev)
  if (!is.ordered(groups)) {
    groups <- ordered(groups, levels = unique(as.character(groups)))
  }
  grName <- ".groups"
  ugroups <- unique(groups)
  value <- data.frame(rep(rep(newprimary, length(ugroups)), nL),
		      rep(rep(ugroups, rep(length(newprimary),
                                           length(ugroups))), nL))
  names(value) <- c(prName, grName)
  ## recovering other variables in data that may be needed for predictions
  ## varying variables will be replaced by their means
  summData <- gsummary(data, groups = groups)
  if (any(toAdd <- is.na(match(names(summData), names(value))))) {
    summData <- summData[, toAdd, drop = FALSE]
  }
  value[, names(summData)] <- summData[value[, 2], ]
  pred <- predict(object, value[1:(nrow(value)/nL), , drop = FALSE], level = level)

  if (nL > 1) {                         # multiple levels
    pred <- pred[, ncol(pred) - (nL - 1):0] # eliminating groups
    predNames <- rep(names(pred), rep(nrow(pred), nL))
    pred <- c(unlist(pred))
  } else {
    predNames <- rep("predicted", nrow(value))
  }
  newvals <- cbind(value[, 1:2], pred)
  names(newvals)[3] <- respName <-
    deparse(getResponseFormula(object)[[2]])
  orig <- data.frame(primary, groups, getResponse(object))
  names(orig) <- names(newvals)
  value <- rbind(orig, newvals)
  attributes(value[, 2]) <- attributes(groups)
  value[, ".type"] <- ordered(c(rep("original", nrow(data)), predNames),
                              levels = c(unique(predNames), "original"))
  labs <- list(x = prName, y = respName)
  unts <- list(x = "", y = "")
  if(inherits(data, "groupedData")) {
    labs[names(attr(data, "labels"))] <- attr(data, "labels")
    unts[names(attr(data, "units"))] <- attr(data, "units")
    attr(value, "units") <- attr(data, "units")
  }
  attr(value, "labels") <- labs
  attr(value, "units") <- unts
  attr(value, "formula") <-
      eval(parse(text = paste(respName, "~", prName, "|", grName)))
  class(value) <- c("augPred", class(value))
  value
}

coef.lme <-
  function(object, augFrame = FALSE, level = Q, data, which = 1:ncol(data),
	   FUN = mean, omitGroupingFactor = TRUE, subset = NULL, ...)
{
  Q <- object$dims$Q
  if (length(level) > 1) {
    stop("only single level allowed")
  }
  fixed <- fixef(object)
  p <- length(fixed)
  value <- ranef(object, level = 1:level)
  grps <- object[["groups"]]
  if (Q > 1) {
    grpNames <- t(array(rep(rev(names(grps)), Q), c(Q, Q)))
    grpNames[lower.tri(grpNames)] <- ""
    grpNames <-
      rev(apply(grpNames, 1,
                function(x) paste(x[x != ""], collapse = " %in% ")))[level]
  } else {
    grpNames <- names(grps)
  }
  grps <- grps[, 1:level, drop = FALSE]
  grps <- gsummary(grps, groups = grps[, level])
  if (level == 1) value <- list(value)
  effNams <- unlist(lapply(value, names))
  grps <- grps[row.names(value[[level]]), , drop = FALSE]
  M <- nrow(grps)
  effNams <- unique(c(names(fixed), effNams))
  effs <- array(0, c(M, length(effNams)),
		list(row.names(grps), effNams))

  effs[, names(fixed)] <- array(rep(fixed, rep(M, p)),	c(M, p))
  for (i in 1:level) {
    nami <- names(value[[i]])
    effs[, nami] <- as.matrix(effs[, nami] + value[[i]][as.character(grps[, i]), ])
  }

  if (augFrame) {			# can only do that for last level
    if (missing(data)) {
      data <- getData(object)
    }
    data <- as.data.frame(data)
    data <- data[, which, drop = FALSE]
    value <- ranef(object, TRUE, level, data, FUN = FUN,
			    omitGroupingFactor = omitGroupingFactor,
                            subset = subset)
    whichKeep <- is.na(match(names(value), effNams))
    if (any(whichKeep)) {
      effs <- cbind(effs, value[, whichKeep, drop = FALSE])
    }
  }
  effs <- as.data.frame(effs)
  attr(effs, "level") <- level
  attr(effs, "label") <- "Coefficients"
  attr(effs, "effectNames") <- effNams
  attr(effs, "standardized") <- FALSE
  attr(effs, "grpNames") <- grpNames
  class(effs) <- unique(c("coef.lme", "ranef.lme", class(effs)))
  effs
}

fitted.lme <-
  function(object, level = Q, asList = FALSE, ...)
{
  Q <- object$dims$Q
  val <- object[["fitted"]]
  if (is.character(level)) {		# levels must be given consistently
    nlevel <- match(level, names(val))
    if (any(aux <- is.na(nlevel))) {
        stop(sprintf(ngettext(sum(aux),
                              "nonexistent level %s",
                              "nonexistent levels %s"),
                     level[aux]), domain = NA)
    }
    level <- nlevel
  } else {				# assuming integers
    level <- 1 + level
  }
  val2 <- napredict(object$na.action, val[, level])
  if (length(level) == 1) {
    grp.nm <- row.names(object[["groups"]])
    grps <- as.character(object[["groups"]][, max(c(1, level - 1))])
    if (asList) {
      val <- as.list(split(val, ordered(grps, levels = unique(grps))))
    } else {
      grp.nm <- row.names(object[["groups"]])
      val <- val2
      names(val) <- grps[match(names(val), grp.nm)]
    }
    lab <- "Fitted values"
    if (!is.null(aux <- attr(object, "units")$y)) {
      lab <- paste(lab, aux)
    }
    attr(val, "label") <- lab
    val
  } else val2
}

formula.lme <- function(x, ...) eval(x$call$fixed)

fixef.lme <-
  function(object, ...) object$coefficients$fixed

getGroups.lme <-
  function(object, form, level = Q, data, sep)
{
  Q <- object$dims$Q
  val <- object[["groups"]][, level]
  if (length(level) == 1) {		# single group
    attr(val, "label") <- names(object[["groups"]])[level]
  }
  val
}

getGroupsFormula.lme <-
  function(object, asList = FALSE, sep)
{
  getGroupsFormula(object$modelStruct$reStruct, asList)
}

getResponse.lme <-
  function(object, form)
{
  val <- resid(object) + fitted(object)
  if (is.null(lab <- attr(object, "labels")$y)) {
    lab <- deparse(getResponseFormula(object)[[2]])
  }
  if (!is.null(aux <- attr(object, "units")$y)) {
    lab <- paste(lab, aux)
  }
  attr(val, "label") <- lab
  val
}

intervals.lme <-
  function(object, level = 0.95, which = c("all", "var-cov", "fixed"), ...)
{
  which <- match.arg(which)
  val <- list()
  if (which != "var-cov") {		# fixed effects included
    est <- fixef(object)
    len <- -qt((1-level)/2, object$fixDF$X) * sqrt(diag(object$varFix))
    vfix <- array(c(est - len, est, est + len), c(length(est), 3),
                  list(names(est), c("lower", "est.", "upper")))
    attr(vfix, "label") <- "Fixed effects:"
    val <- list(fixed = vfix)
  }
  if (which != "fixed") {		# variance-covariance included
    if (is.character(aV <- object$apVar)) {
        stop(gettextf("cannot get confidence intervals on var-cov components: %s",
                      aV), domain = NA)
    }
    est <- attr(aV, "Pars")
    nP <- length(est)
    len <- -qnorm((1-level)/2) * sqrt(diag(aV))
    origInt <-                          # intervals in unconstrained parameters
      array(c(est - len, est, est + len),
	    c(nP, 3), list(names(est), c("lower", "est.", "upper")))

    lmeSt <- object$modelStruct
    if (!all(whichKeep <- apply(attr(lmeSt, "pmap"), 2, any))) {
      ## need to deleted components with fixed coefficients
      aux <- lmeSt[whichKeep]
      class(aux) <- class(lmeSt)
      attr(aux, "settings") <- attr(lmeSt, "settings")
      attr(aux, "pmap") <- attr(lmeSt, "pmap")[, whichKeep, drop = FALSE]
      lmeSt <- aux
    }
    cSt <- lmeSt[["corStruct"]]
    if (!is.null(cSt) && inherits(cSt, "corSymm") && attr(aV, "natural")) {
      ## converting to corNatural
      class(cSt) <- c("corNatural", "corStruct")
      lmeSt[["corStruct"]] <- cSt
    }
    pmap <- attr(lmeSt, "pmap")
    namL <- names(lmeSt)
    natInt <- vector("list", length(namL) + 1) # list of intervals in natural pars
    names(natInt) <- c(namL, "sigma")
    natInt <- as.list(natInt)
    ## intervals for sigma are stored separately and dropped from origInt
    vsig <- exp(origInt[nP,])
    attr(vsig, "label") <- "Within-group standard error:"
    natInt[["sigma"]] <- vsig
    origInt <- origInt[-nP,, drop = FALSE]

    if (attr(aV, "natural")) {          # convert any pdSymm's to pdNatural's
      for(i in seq_along(lmeSt$reStruct)) {
	if (inherits(lmeSt$reStruct[[i]], "pdSymm")) {
	  lmeSt$reStruct[[i]] <- pdNatural(lmeSt$reStruct[[i]])
	} else {
          if (inherits(lmeSt$reStruct[[i]], "pdBlocked")) {
            for(j in seq_along(lmeSt$reStruct[[i]])) {
              if (inherits(lmeSt$reStruct[[i]][[j]], "pdSymm")) {
                lmeSt$reStruct[[i]][[j]] <- pdNatural(lmeSt$reStruct[[i]][[j]])
              }
            }
          }
        }
      }
    }
    rownames(origInt) <-           # re-express names if necessary
      ## namP <-
          names(coef(lmeSt, unconstrained = FALSE))
    for(i in 1:3) {                     # re-express intervals in constrained pars
      coef(lmeSt) <- origInt[,i]
      origInt[,i] <- coef(lmeSt, unconstrained = FALSE)
    }
    for(i in namL) {
      natInt[[i]] <- origInt[ pmap[ , i ], , drop = FALSE ]
      switch(i,
             reStruct = {
               plen <- attr( lmeSt$reStruct, "plen" )
               natInt[[i]] <-
               natInt[[i]] <-
                 rev(as.matrix( split( as.data.frame( natInt[[i]] ),
                                      rep( seq_along(plen), plen ))))
               names(natInt[[i]]) <- rev(names(plen))
               for (j in names(plen)) {
                 dimnames(natInt[[i]][[j]])[[1]] <-
                   names( coef( lmeSt[[i]][[j]], unconstrained = FALSE ) )
               }
             },
             corStruct =,
             varStruct = {
               dimnames(natInt[[i]])[[1]] <-
                 names(coef(lmeSt[[i]], unconstrained = FALSE))
             }
             )
      attr(natInt[[i]], "label") <-
	switch(i,
	       reStruct = "Random Effects:",
	       corStruct = "Correlation structure:",
	       varStruct = "Variance function:",
	       paste(i,":",sep=""))
    }
    val <- c(val, natInt)
  }
  attr(val, "level") <- level
  class(val) <- "intervals.lme"
  val
}

logLik.lme <-
  function(object, REML, ...)
{
  p <- object$dims$ncol[object$dims$Q + 1]
  N <- object$dims$N
##  Np <- N - p
  estM <- object$method
  if (missing(REML)) REML <- estM == "REML"
  val <- object[["logLik"]]
  if (REML && (estM == "ML")) {			# have to correct logLik
    val <- val + (p * (log(2 * pi) + 1) + (N - p) * log(1 - p/N) +
		  sum(log(abs(svd(object$varFix)$d)))) / 2
  }
  if (!REML && (estM == "REML")) {	# have to correct logLik
    val <- val - (p * (log(2*pi) + 1) + N * log(1 - p/N) +
		  sum(log(abs(svd(object$varFix)$d)))) / 2
  }
  attr(val, "nall") <- N
  attr(val, "nobs") <- N - REML * p
  attr(val, "df") <- p + length(coef(object[["modelStruct"]])) + 1
  class(val) <- "logLik"
  val
}

nobs.lme <- function(object, ...) object$dims$N

pairs.lme <-
  function(x, form = ~coef(.), label, id = NULL, idLabels = NULL,
	   grid = FALSE, ...)
{
  object <- x
  ## scatter plot matrix plots, generally based on coef or ranef
  if (!inherits(form, "formula")) {
    stop("'form' must be a formula")
  }
  if (length(form) != 2) {
    stop("'form' must be a one-sided formula")
  }
  ## constructing data
  allV <- all.vars(asOneFormula(form, id, idLabels))
  allV <- allV[is.na(match(allV,c("T","F","TRUE","FALSE")))]
  if (length(allV) > 0) {
    data <- getData(object)
    if (is.null(data)) {		# try to construct data
      alist <- lapply(as.list(allV), as.name)
      names(alist) <- allV
      alist <- c(as.list(as.name("data.frame")), alist)
      mode(alist) <- "call"
      data <- eval(alist, sys.parent(1))
    } else {
      if (any(naV <- is.na(match(allV, names(data))))) {
        stop(sprintf(ngettext(sum(naV),
                              "%s not found in data",
                              "%s not found in data"),
                     allV[naV]), domain = NA)
      }
    }
  } else data <- NULL

  ## argument list
  dots <- list(...)
  if (length(dots) > 0) args <- dots
  else args <- list()

  ## covariate - must be present as a data.frame
  covF <- getCovariateFormula(form)
  .x <- eval(covF[[2]], list(. = object)) # only function of "."
  if (!inherits(.x, "data.frame")) {
    stop("covariate must be a data frame")
  }
  level <- attr(.x, "level")
  if (!is.null(effNams <- attr(.x, "effectNames"))) {
    .x <- .x[, effNams, drop = FALSE]
  }
  ## eliminating constant effects
  isFixed <- unlist(lapply(.x, function(el) length(unique(el)) == 1))
  .x <- .x[, !isFixed, drop = FALSE]
  nc <- ncol(.x)
  if (nc == 1) {
    stop("cannot do pairs of just one variable")
  }
  if (!missing(label)) {
    names(.x) <- label
  }
  if (nc == 2) {
    ## will use xyplot
    argForm <- .y ~ .x
    argData <- .x
    names(argData) <- c(".x", ".y")
    if (is.null(args$xlab)) {
      args$xlab <- names(.x)[1]
    }
    if (is.null(args$ylab)) {
      args$ylab <- names(.x)[2]
    }
  } else {				# splom
    argForm <- ~ .x
    argData <- list(.x = .x)
  }

  auxData <- list()
  ## groups - need not be present
  grpsF <- getGroupsFormula(form)
  if (!is.null(grpsF)) {
    gr <- splitFormula(grpsF, sep = "*")
    for(i in 1:length(gr)) {
      auxGr <- all.vars(gr[[i]])
      for(j in auxGr) {
        auxData[[j]] <- eval(as.name(j), data)
      }
    }
    if (length(argForm) == 2)
      argForm <- eval(parse(text = paste("~ .x |", deparse(grpsF[[2]]))))
    else argForm <- eval(parse(text = paste(".y ~ .x |", deparse(grpsF[[2]]))))
  }
  ## id and idLabels - need not be present
  if (!is.null(id)) {			# identify points in plot
    N <- object$dims$N
    id <-
      switch(mode(id),
	     numeric = {
	       if ((id <= 0) || (id >= 1)) {
		 stop("'id' must be between 0 and 1")
	       }
	       if (is.null(level)) {
	 stop("covariate must have a level attribute when groups are present")
       }
	       aux <- t(as.matrix(ranef(object, level = level)))
	       aux <- as.logical(apply(
	(solve(t(pdMatrix(object$modelStruct$reStruct, factor = TRUE)[[level]]),
		 aux)/object$sigma)^2, 2, sum) > qchisq(1 - id, dim(aux)[1]))
	       aux
	     },
	     call = eval(asOneSidedFormula(id)[[2]], data),
	     stop("'id' can only be a formula or numeric")
	     )
    if (length(id) == N) {
      ## id as a formula evaluated in data
      if (is.null(level)) {
	stop("covariate must have a level attribute when 'id' is a formula")
      }
      auxData[[".id"]] <- id
    }

    if (is.null(idLabels)) {
      idLabels <- row.names(.x)
    } else {
      if (mode(idLabels) == "call") {
	idLabels <-
	  as.character(eval(asOneSidedFormula(idLabels)[[2]], data))
      } else if (is.vector(idLabels)) {
	if (length(idLabels <- unlist(idLabels)) != N) {
	  stop("'idLabels' of incorrect length")
	}
	idLabels <- as.character(idLabels)
      } else {
	stop("'idLabels' can only be a formula or a vector")
      }
    }
    if (length(idLabels) == N) {
      ## idLabels as a formula evaluated in data
      if (is.null(level)) {
      stop("covariate must have a level attribute when 'idLabels' is a formula")
      }
      auxData[[".Lid"]] <- idLabels
    }
  }

  if (length(auxData)) {		# need collapsing
    auxData <- gsummary(as.data.frame(auxData),
			groups = getGroups(object, level = level))
    auxData <- auxData[row.names(.x), , drop = FALSE]

    if (!is.null(auxData[[".id"]])) {
      id <- auxData[[".id"]]
    }

    if (!is.null(auxData[[".Lid"]])) {
      idLabels <- auxData[[".Lid"]]
    }
    wchDat <- is.na(match(names(auxData), c(".id", ".idLabels")))
    if (any(wchDat)) {
      argData <- c(argData, as.list(auxData[, wchDat, drop = FALSE]))
    }
  }

  if (!is.null(id)) assign("id", as.logical(as.character(id)))# , where = 1)
  assign("idLabels", as.character(idLabels))#, where = 1)
  #assign("grid", grid, where = 1)
  ## adding to args list
  args <- c(list(argForm, data = argData), args)
  if (is.null(args$strip)) {
    args$strip <- function(...) strip.default(..., style = 1)
  }
  if (is.null(args$cex)) args$cex <- par("cex")
  if (is.null(args$adj)) args$adj <- par("adj")

  ## defining the type of plot
  if (length(argForm) == 3) {		# xyplot
    plotFun <- "xyplot"
    if (is.null(args$panel)) {
      args <- c(args,
                panel = list(function(x, y, subscripts, ...)
		  {
                    x <- as.numeric(x)
                    y <- as.numeric(y)
                    dots <- list(...)
		    if (grid) panel.grid()
		    panel.xyplot(x, y, ...)
                    if (any(ids <- id[subscripts])){
                        ltext(x[ids], y[ids], idLabels[subscripts][ids],
                              cex = dots$cex, adj = dots$adj)
                    }
		  }))
    }
  } else {				# splom
      plotFun <- "splom"
      if (is.null(args$panel)) {
          args <- c(args,
                    panel = list(function(x, y, subscripts, ...)
                {
                    x <- as.numeric(x)
                    y <- as.numeric(y)
                    dots <- list(...)
		    if (grid) panel.grid()
		    panel.xyplot(x, y, ...)
                    if (any(ids <- id[subscripts])){
                        ltext(x[ids], y[ids], idLabels[subscripts][ids],
                              cex = dots$cex, adj = dots$adj)
                    }
                }))
      }
  }
  do.call(plotFun, as.list(args))
}

plot.ranef.lme <-
  function(x, form = NULL, omitFixed = TRUE, level = Q,
           grid = TRUE, control, ...)
{
  object <- x
  plotControl <-
    function(drawLine = TRUE, span.loess = 2/3, degree.loess = 1,
             cex.axis = 0.8, srt.axis = 0, mgp.axis = c(2, 0.5, 0))
    {
      list(drawLine = drawLine,
           span.loess = span.loess,
           degree.loess = degree.loess,
           cex.axis = cex.axis,
           srt.axis = srt.axis,
           mgp.axis = mgp.axis)
    }

  pControl <- plotControl()
  if (!missing(control)) {
    pControl[names(control)] <- control
  }
  if (!inherits(object, "data.frame")) {
    ## must be a list of data frames
    Q <- length(object)
    if (length(level) > 1) {
      stop("only single level allowed")
    }
    oAttr <- attributes(object)[c("label", "standardized", "namsEff")]
    object <- object[[level]]
    oAttr$namsEff <- oAttr$namsEff[level]
    attributes(object)[c("label", "standardized", "namsEff")] <- oAttr
  }
  if (omitFixed) {			# eliminating constant effects
    isFixed <- unlist(lapply(object, function(el) length(unique(el)) == 1))
    if (any(isFixed)) {
      oattr <- attributes(object)
      oattr <- oattr[names(oattr) != "names"]
      object <- object[, !isFixed, drop = FALSE]
      oattr$effectNames <- oattr$effectNames[!is.na(match(oattr$effectNames,
							  names(object)))]
      attributes(object)[names(oattr)] <- oattr
    }
  }

  eNames <- attr(object, "effectNames")
  if (is.null(form) || (inherits(form, "formula") && length(form) == 2)) {
    eLen <- length(eNames)
    argData <- data.frame(.pars = as.vector(unlist(object[, eNames])),
                          .enames = ordered(rep(eNames, rep(nrow(object), eLen)),
                          level = eNames), check.names = FALSE)
    for(i in names(object)[is.na(match(names(object), eNames))]) {
      argData[[i]] <- rep(object[[i]], eLen)
    }
    argForm <- .groups ~ .pars | .enames
    argData[[".groups"]] <- rep(row.names(object), eLen)
    if (inherits(form, "formula")) {
      onames <- all.vars(form)
      if (any(whichNA <- is.na(match(onames, names(argData))))) {
          stop(sprintf(ngettext(sum(whichNA),
                                "%s not available for plotting",
                                "%s not available for plotting"),
                       onames[whichNA], collapse = ", "), domain = NA)
      }
      argData[[".groups"]] <-
        as.character(argData[[as.character(onames[1])]])
      if (length(onames) > 1) {
        for(i in onames[-1]) {
          argData[[".groups"]] <-
            paste(as.character(argData[[".groups"]]),
                  as.character(argData[[i]]))
        }
      }
    }
    argData[[".groups"]] <- ordered(argData[[".groups"]],
                                    levels = unique(argData[[".groups"]]))
    args <- list(argForm, data = argData, ...)
    if (is.null(args$xlab)) {
      args$xlab <- attr(object, "label")
    }
    if (is.null(args$ylab)) {
      if (is.null(form)) {
        args$ylab <- attr(object, "grpNames")
      } else {
        args$ylab <- deparse(form[[2]])
      }
    }
    if (is.null(args$scales)) {
      if (!is.null(attr(object, "standardized")) &&
          !attr(object, "standardized")) {
        args$scales <- list(x = list(relation = "free"))
      }
    }
    if (is.null(args$strip)) {
      args$strip <- function(...) strip.default(..., style = 1)
    }
    do.call("dotplot", as.list(args))
  } else {
    if (!inherits(form, "formula")) {
      stop("'form' must be a formula when not NULL")
    }
    reName <- form[[2]]
    if (length(reName) != 1 &&
        substring(deparse(reName),
                  nchar(deparse(reName), "c") - 10) != "(Intercept)") {
      stop("only single effects allowed in left side of 'form'")
    }
    reName <- deparse(reName)
    if (is.na(match(reName, eNames))) {
      stop(gettextf("%s is not a valid effect name", sQuote(reName)),
           domain = NA)
    }
    vNames <- all.vars(form[[3]])       # variable names
    if (any(!is.na(match(vNames, eNames)))) {
      stop("no effects allowed in right side of formula")
    }
    if (any(whichNA <- is.na(match(vNames, names(object))))) {
        stop(sprintf(ngettext(sum(whichNA),
                              "%s not available for plotting",
                              "%s not available for plotting"),
                     onames[whichNA], collapse = ", "), domain = NA)
    }
    nV <- length(vNames)                # number of variables
    nG <- nrow(object)                  # number of groups
    reVal <- numeric(0)
    vNam <- character(0)
    vVal <- numeric(0)
    vType <- character(nV)
    names(vType) <- vNames
    vLevs <- vector("list", nV)
    names(vLevs) <- vNames
    aux <- object[, reName]
    for(i in 1:nV) {
      obj <- object[, vNames[i]]
      if (inherits(obj, "factor") ||
          is.character(obj)) {
        obj <- as.factor(obj)
        vLevs[[i]] <- levels(obj)
        vType[i] <- "factor"
        reVal <- c(reVal, c(NA, NA, aux))
        vVal <- c(vVal, c(0.5, length(levels(obj)) + 0.5, as.integer(obj)))
        vNam <- c(vNam, rep(vNames[i], nG + 2))
      } else {                          # numeric
        vType[i] <- "numeric"
        reVal <- c(reVal, aux)
        vVal <- c(vVal, obj)
        vNam <- c(vNam, rep(vNames[i], nG))
      }
    }
    argData <- data.frame(y = reVal, x = vVal,
                          g = ordered(vNam, levels = vNames))
    assign(".vNam", vNam)#, where = 1)
    assign(".vType", vType)#, where = 1)
    assign(".vLevs", vLevs)#, where = 1)
    assign(".grid", grid)#, where = 1)
    assign(".drawLine", pControl$drawLine)#, where = 1)
    assign(".span", pControl$span.loess)#, where = 1)
    assign(".degree", pControl$degree.loess)#, where = 1)
    #assign("panel.bwplot2", panel.bwplot2, where = 1)
    assign(".cex", pControl$cex.axis)#, where = 1)
    assign(".srt", pControl$srt.axis)#, where = 1)
    assign(".mgp", pControl$mgp.axis)#, where = 1)
    dots <- list(...)
    ylab <- dots$ylab
    if (is.null(ylab)) {
      ylab <- reName
    }
    strip <- dots$strip
    if (is.null(strip)) {
      strip <- strip.default
    }

    ## this is a hack to make this work, it's probably possible to
    ## implement the whole thing much more succintly -- ds

    ## The idea here is that the limits component of scales$x is going
    ## to be a list -- and character vectors have special meaning as
    ## limits, controlling both limits and the tick mark
    ## positions/labels

    condvar <- eval(expression(g), argData)
    xscales.lim <- as.list(levels(condvar))
    subsc <- seq_along(condvar)

    for (i in seq_along(xscales.lim)) {
        subscripts <- subsc[condvar == xscales.lim[[i]]]
        vN <- .vNam[subscripts][1]
        if (.vType[vN] == "numeric") {
            xscales.lim[[i]] <- range(argData$x[subscripts])
        }
        else
            xscales.lim[[i]] <- .vLevs[vN][[1]]
    }

    xyplot(y ~ x | g, data = argData, subscripts = TRUE,
           scales = list(x = list(relation = "free", limits = xscales.lim)),
           panel = function(x, y, subscripts, ...) {
             vN <- .vNam[subscripts][1]
             if (.grid) panel.grid()
             if (.vType[vN] == "numeric") {
               panel.xyplot(x, y, ...)
               if (.drawLine) {
                 panel.loess(x, y, span = .span, degree = .degree)
               }
             } else {
                 panel.bwplot(x, y, horizontal = FALSE)
                 if (.drawLine) {
                     plot.line <- trellis.par.get("plot.line")
                     panel.linejoin(x, y, fun = median, horizontal = FALSE,
                                    col.line = plot.line$col,
                                    lwd = plot.line$lwd,
                                    lty = plot.line$lty)
                 }
             }
           }, xlab = "", ylab = ylab, strip = strip, ...)
  }
}

predict.lme <-
  function(object, newdata, level = Q, asList = FALSE,
           na.action = na.fail, ...)
{
  ##
  ## method for predict() designed for objects inheriting from class lme
  ##
  Q <- object$dims$Q
  if (missing(newdata)) {		# will return fitted values
    val <- fitted(object, level, asList)
    if (length(level) == 1) return(val)
    return(data.frame(object[["groups"]][,level[level != 0], drop = FALSE],
		      predict = val))
  }
  maxQ <- max(level)			# maximum level for predictions
  mCall <- object$call
  fixed <- eval(eval(mCall$fixed)[-2])
  Terms <- object$terms
  newdata <- as.data.frame(newdata)

  if (maxQ > 0) {			# predictions with random effects
    reSt <- object$modelStruct$reStruct[Q - (maxQ - 1):0]
    lmeSt <- lmeStruct(reStruct = reSt)
    groups <- getGroupsFormula(reSt)
    if (any(is.na(match(all.vars(groups), names(newdata))))) {
      ## groups cannot be evaluated in newdata
      stop("cannot evaluate groups for desired levels on 'newdata'")
    }
  } else {
    reSt <- NULL
  }

  mfArgs <- list(formula = asOneFormula(formula(reSt), fixed),
		 data = newdata, na.action = na.action,
                 drop.unused.levels = TRUE)
  dataMix <- do.call("model.frame", mfArgs)
  origOrder <- row.names(dataMix)	# preserve the original order
  whichRows <- match(origOrder, row.names(newdata))

  if (maxQ > 0) {
    ## sort the model.frame by groups and get the matrices and parameters
    ## used in the estimation procedures
    grps <- getGroups(newdata,
	      eval(parse(text = paste("~1", deparse(groups[[2]]), sep = "|"))))
    ## ordering data by groups
    if (inherits(grps, "factor")) {	# single level
      grps <- grps[whichRows]
      oGrps <- data.frame(grps)
      ## checking if there are missing groups
      if (any(naGrps <- is.na(grps))) {
	grps[naGrps] <- levels(grps)[1]	# input with existing level
      }
      ord <- order(grps)     #"order" treats a single named argument peculiarly
      grps <- data.frame(grps)
      row.names(grps) <- origOrder
      names(grps) <- names(oGrps) <- as.character(deparse((groups[[2]])))
    } else {
      grps <- oGrps <-
	do.call("data.frame", lapply(grps[whichRows, ], function(x) x[drop = TRUE]))
      ## checking for missing groups
      if (any(naGrps <- is.na(grps))) {
	## need to input missing groups
	for(i in names(grps)) {
	  grps[naGrps[, i], i] <- levels(grps[,i])[1]
	}
	naGrps <- t(apply(naGrps, 1, cumsum)) # propagating NAs
      }
      ord <- do.call("order", grps)
      ## making group levels unique
      grps[, 1] <- grps[, 1][drop = TRUE]
      for(i in 2:ncol(grps)) {
	grps[, i] <-
          as.factor(paste(as.character(grps[, i-1]), as.character(grps[,i]),
                          sep = "/"))
	NULL
      }
    }
    naGrps <- cbind(FALSE, naGrps)[ord, , drop = FALSE]
    grps <- grps[ord, , drop = FALSE]
    dataMix <- dataMix[ord, ,drop = FALSE]
  }
  ## making sure factor levels are the same as in contrasts
  contr <- object$contrasts
  for(i in names(dataMix)) {
    if (inherits(dataMix[,i], "factor") && !is.null(contr[[i]])) {
      levs <- levels(dataMix[,i])
      levsC <- dimnames(contr[[i]])[[1]]
      if (any(wch <- is.na(match(levs, levsC)))) {
          stop(sprintf(ngettext(sum(wch),
                                "level %s not allowed for %s",
                                "levels %s not allowed for %s"),
                       paste(levs[wch], collapse = ",")),
               domain = NA)
      }
#      if (length(levs) < length(levsC)) {
#        if (inherits(dataMix[,i], "ordered")) {
#          dataMix[,i] <- ordered(as.character(dataMix[,i]), levels = levsC)
#        } else {
#          dataMix[,i] <- factor(as.character(dataMix[,i]), levels = levsC)
#        }
#      }
      attr(dataMix[,i], "contrasts") <- contr[[i]][levs, , drop = FALSE]
    }
  }
  if (maxQ > 0) {
    revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order
#    Z <- model.matrix(reSt, dataMix, contr)
    Z <- model.matrix(reSt, dataMix)
    ncols <- attr(Z, "ncols")
    Names(lmeSt$reStruct) <- attr(Z, "nams")
  }
  N <- nrow(dataMix)
  if (length(all.vars(fixed)) > 0) {
#    X <- model.matrix(fixed, model.frame(fixed, dataMix), contr)
    X <- model.matrix(fixed, model.frame(delete.response(Terms), dataMix))
  } else {
    X <- array(1, c(N, 1), list(row.names(dataMix), "(Intercept)"))
  }
  if (maxQ == 0) {
    ## only population predictions
    val <- c(X %*% fixef(object))
    attr(val, "label") <- "Predicted values"
    return(val)
  }

  ncols <- c(ncols, dim(X)[2], 1)
  ## creating the condensed linear model
  attr(lmeSt, "conLin") <-
    list(Xy = array(c(Z, X, double(N)), c(N, sum(ncols)),
	     list(row.names(dataMix), c(colnames(Z), colnames(X),
					"resp"))),
	 dims = MEdims(grps, ncols))
  ## Getting the appropriate BLUPs of the random effects
  re <- object$coefficients$random[1:maxQ]
  for(i in names(re)) {
    ugrps <- unique(as.character(grps[, i]))
    val <- array(NA, c(length(ugrps), ncol(re[[i]])),
		 list(ugrps, dimnames(re[[i]])[[2]]))
    mGrps <- match(ugrps, dimnames(re[[i]])[[1]])
    mGrps <- mGrps[!is.na(mGrps)]
    re[[i]] <- re[[i]][mGrps, , drop = FALSE]
    val[dimnames(re[[i]])[[1]], ] <- re[[i]]
    re[[i]] <- val
  }

  attr(lmeSt, "lmeFit") <- list(beta = fixef(object), b = re)
  val <- fitted(lmeSt, level = 0:maxQ)
  val[as.logical(naGrps)] <- NA			# setting missing groups to NA
  ## putting back in original order and extracting levels
  val <- val[revOrder, level + 1]		# predictions
  if (maxQ > 1) {                      # making groups unique
    for(i in 2:maxQ) {
      oGrps[, i] <-
        as.factor(paste(as.character(oGrps[,i-1]), as.character(oGrps[,i]),
                        sep = "/"))
    }
  }
  if (length(level) == 1) {
    grps <- as.character(oGrps[, level])
    if (asList) {
      val <- split(val, ordered(grps, levels = unique(grps)))
    } else {
      names(val) <- grps
    }
    attr(val, "label") <- "Predicted values"
    if (!is.null(aux <- attr(object, "units")$y))
      attr(val, "label") <- paste(attr(val, "label"), aux)
    val
  } else {
    data.frame(oGrps, predict = data.frame(val))
  }
}

print.anova.lme <-
  function(x, verbose = attr(x, "verbose"), ...)
{
  ox <- x
  if ((rt <- attr(x,"rt")) == 1) {
    if (!is.null(lab <- attr(x, "label"))) {
      cat(lab)
      if (!is.null(L <- attr(x, "L"))) {
        print(zapsmall(L))
      }
    }
    pval <- format(round(x[, "p-value"],4))
    pval[as.double(pval) == 0] <- "<.0001"
    x[, "F-value"] <- format(zapsmall(x[, "F-value"]))
    x[, "p-value"] <- pval
    print(as.data.frame(x))
  } else {
    if (verbose) {
      cat("Call:\n")
      objNams <- row.names(x)
      for(i in 1:rt) {
	cat(" ",objNams[i],":\n", sep ="")
	cat(" ",as.character(x[i,"call"]),"\n")
      }
      cat("\n")
    }
    x <- as.data.frame(x[,-1])
    for(i in names(x)) {
      xx <- x[[i]]
      if (i == "p-value") {
        xx <- round(xx, 4)
        xna <- is.na(xx)
        xx[!xna] <- format(xx[!xna])
        xx[as.double(xx) == 0] <- "<.0001"
        xx[xna] <- ""
      } else {
        if (match(i, c("AIC", "BIC", "logLik", "L.Ratio"), 0)) {
          xna <- is.na(xx)
          xx <- zapsmall(xx)
          xx[xna] <- 0
          xx <- format(xx)
          xx[xna] <- ""
        }
      }
      x[[i]] <- xx
    }
    print(as.data.frame(x))
  }
  invisible(ox)
}

print.intervals.lme <-
  function(x, ...)
{
  cat(paste("Approximate ", attr(x, "level") * 100,
	    "% confidence intervals\n", sep = ""))
  for(i in names(x)) {
    aux <- x[[i]]
    cat("\n ",attr(aux, "label"), "\n", sep = "")
    if (i == "reStruct") {
      for(j in names(aux)) {
	cat("  Level:", j, "\n")
	print(as.matrix(aux[[j]]), ...)
      }
    } else {
      if (i == "sigma") print(c(aux), ...)
      else print(as.matrix(aux), ...)
    }
  }
  invisible(x)
}

print.lme <-
  function(x, ...)
{
  dd <- x$dims
  if (inherits(x, "nlme")) {	# nlme object
    cat( "Nonlinear mixed-effects model fit by " )
    cat( ifelse( x$method == "REML", "REML\n", "maximum likelihood\n") )
    cat("  Model:", deparse(x$call$model),"\n")
  } else {				# lme objects
    cat( "Linear mixed-effects model fit by " )
    cat( ifelse( x$method == "REML", "REML\n", "maximum likelihood\n") )
  }
  cat("  Data:", deparse( x$call$data ), "\n")
  if (!is.null(x$call$subset)) {
    cat("  Subset:", deparse(asOneSidedFormula(x$call$subset)[[2]]),"\n")
  }
  cat("  Log-", ifelse(x$method == "REML", "restricted-", ""),
             "likelihood: ", format(x$logLik), "\n", sep = "")
  fixF <- x$call$fixed
  if (inherits(fixF, "formula") || is.call(fixF) || is.name(fixF)) {
    cat("  Fixed:", deparse(x$call$fixed), "\n")
  } else {
    cat("  Fixed:", deparse(lapply(fixF, function(el)
                                   as.name(deparse(el)))), "\n")
  }
  print(fixef(x))
  cat("\n")
  print(summary(x$modelStruct), sigma = x$sigma)
  cat("Number of Observations:", dd[["N"]])
  cat("\nNumber of Groups: ")
  Ngrps <- dd$ngrps[1:dd$Q]
  if ((lNgrps <- length(Ngrps)) == 1) {	# single nesting
    cat(Ngrps,"\n")
  } else {				# multiple nesting
    sNgrps <- 1:lNgrps
    aux <- rep(names(Ngrps), sNgrps)
    aux <- split(aux, array(rep(sNgrps, lNgrps),
			    c(lNgrps, lNgrps))[!lower.tri(diag(lNgrps))])
    names(Ngrps) <- unlist(lapply(aux, paste, collapse = " %in% "))
    cat("\n")
    print(rev(Ngrps))
  }
  invisible(x)
}

print.ranef.lme <-
  function(x, ...)
{
  if (!inherits(x[[1]], "data.frame")) {
    print.data.frame(x, ...)
  } else {                              # list
    for(i in seq_along(x)) {
      cat("Level:", attr(x, "grpNames")[i],"\n")
      print.data.frame(x[[i]])
      if (i < length(x)) cat("\n")
    }
  }
  invisible(x)
}

print.summary.lme <-
  function(x, verbose = FALSE, ...)
{
  dd <- x$dims
  verbose <- verbose || attr(x, "verbose")
  if (inherits(x, "nlme")) {	# nlme object
    cat( "Nonlinear mixed-effects model fit by " )
    cat( ifelse( x$method == "REML", "REML\n", "maximum likelihood\n") )
    cat("  Model:", deparse(x$call$model),"\n")
  } else {				# lme objects
    cat( "Linear mixed-effects model fit by " )
    cat( ifelse( x$method == "REML", "REML\n", "maximum likelihood\n") )
  }
  method <- x$method
  cat(" Data:", deparse( x$call$data ), "\n")
  if (!is.null(x$call$subset)) {
    cat("  Subset:", deparse(asOneSidedFormula(x$call$subset)[[2]]),"\n")
  }
  print( data.frame( AIC = x$AIC, BIC = x$BIC, logLik = c(x$logLik),
                    row.names = " ") )
  if (verbose) { cat("Convergence at iteration:",x$numIter,"\n") }
  cat("\n")
  print(summary(x$modelStruct), sigma = x$sigma,
	reEstimates = x$coef$random, verbose = verbose)
  cat("Fixed effects: ")
  fixF <- x$call$fixed
  if (inherits(fixF, "formula") || is.call(fixF)) {
    cat(deparse(x$call$fixed), "\n")
  } else {
    cat(deparse(lapply(fixF, function(el) as.name(deparse(el)))),
        "\n")
  }
  ## fixed effects t-table and correlations
  xtTab <- as.data.frame(x$tTable)
  wchPval <- match("p-value", names(xtTab))
  for(i in names(xtTab)[-wchPval]) {
    xtTab[, i] <- format(zapsmall(xtTab[, i]))
  }
  xtTab[,wchPval] <- format(round(xtTab[,wchPval], 4))
  if (any(wchLv <- (as.double(levels(xtTab[, wchPval])) == 0))) {
    levels(xtTab[, wchPval])[wchLv] <- "<.0001"
  }
  row.names(xtTab) <- dimnames(x$tTable)[[1]]
  print(xtTab)
  if (nrow(x$tTable) > 1) {
    corr <- x$corFixed
    class(corr) <- "correlation"
    print(corr,
	  title = " Correlation:",
	  ...)
  }
  cat("\nStandardized Within-Group Residuals:\n")
  print(x$residuals)
  cat("\nNumber of Observations:",x$dims[["N"]])
  cat("\nNumber of Groups: ")
  Ngrps <- dd$ngrps[1:dd$Q]
  if ((lNgrps <- length(Ngrps)) == 1) {	# single nesting
    cat(Ngrps,"\n")
  } else {				# multiple nesting
    sNgrps <- 1:lNgrps
    aux <- rep(names(Ngrps), sNgrps)
    aux <- split(aux, array(rep(sNgrps, lNgrps),
			    c(lNgrps, lNgrps))[!lower.tri(diag(lNgrps))])
    names(Ngrps) <- unlist(lapply(aux, paste, collapse = " %in% "))
    cat("\n")
    print(rev(Ngrps))
  }
  invisible(x)
}

qqnorm.lme <-
  function(y, form = ~ resid(., type = "p"), abline = NULL,
           id = NULL, idLabels = NULL, grid = FALSE, ...)
  ## normal probability plots for residuals and random effects
{
  object <- y
  if (!inherits(form, "formula")) {
    stop("'form' must be a formula")
  }
  ## constructing data
  allV <- all.vars(asOneFormula(form, id, idLabels))
  allV <- allV[is.na(match(allV,c("T","F","TRUE","FALSE")))]
  if (length(allV) > 0) {
    data <- getData(object)
    if (is.null(data)) {		# try to construct data
      alist <- lapply(as.list(allV), as.name)
      names(alist) <- allV
      alist <- c(as.list(as.name("data.frame")), alist)
      mode(alist) <- "call"
      data <- eval(alist, sys.parent(1))
    } else {
      if (any(naV <- is.na(match(allV, names(data))))) {
        stop(sprintf(ngettext(sum(naV),
                              "%s not found in data",
                              "%s not found in data"),
                     allV[naV]), domain = NA)
      }
    }
  } else data <- NULL
  ## argument list
  dots <- list(...)
  if (length(dots) > 0) args <- dots
  else args <- list()
  ## appending object to data
  data <- as.list(c(as.list(data), . = list(object)))

  ## covariate - must always be present
  covF <- getCovariateFormula(form)
  .x <- eval(covF[[2]], data)
  labs <- attr(.x, "label")
  if (inherits(.x, "ranef.lme")) {      # random effects
    type <- "reff"
  } else {
    if (!is.null(labs) && ((labs == "Standardized residuals") ||
                           (labs == "Normalized residuals") ||
                           (substring(labs, 1, 9) == "Residuals"))) {
      type <- "res"                     # residuals
    } else {
      stop("only residuals and random effects allowed")
    }
  }
  if (is.null(args$xlab)) args$xlab <- labs
  if (is.null(args$ylab)) args$ylab <- "Quantiles of standard normal"
  if(type == "res") {			# residuals
    fData <- qqnorm(.x, plot.it = FALSE)
    data[[".y"]] <- fData$x
    data[[".x"]] <- fData$y
    dform <- ".y ~ .x"
    if (!is.null(grp <- getGroupsFormula(form))) {
      dform <- paste(dform, deparse(grp[[2]]), sep = "|")
    }
    if (!is.null(id)) {			# identify points in plot
      id <-
        switch(mode(id),
               numeric = {
                 if ((id <= 0) || (id >= 1)) {
                   stop("'Id' must be between 0 and 1")
                 }
                 if (labs == "Normalized residuals") {
                   as.logical(abs(resid(object, type="normalized"))
                              > -qnorm(id / 2))
                 } else {
                   as.logical(abs(resid(object, type="pearson"))
                              > -qnorm(id / 2))
                 }
               },
               call = eval(asOneSidedFormula(id)[[2]], data),
               stop("'id' can only be a formula or numeric")
               )
      if (is.null(idLabels)) {
        idLabels <- getGroups(object)
        if (length(idLabels) == 0) idLabels <- 1:object$dims$N
        idLabels <- as.character(idLabels)
      } else {
        if (mode(idLabels) == "call") {
          idLabels <-
            as.character(eval(asOneSidedFormula(idLabels)[[2]], data))
        } else if (is.vector(idLabels)) {
          if (length(idLabels <- unlist(idLabels)) != length(id)) {
            stop("'idLabels' of incorrect length")
          }
          idLabels <- as.character(idLabels)
        } else {
          stop("'idLabels' can only be a formula or a vector")
        }
      }
    }
  } else {				# random.effects
    level <- attr(.x, "level")
    std <- attr(.x, "standardized")
    if (!is.null(effNams <- attr(.x, "effectNames"))) {
      .x <- .x[, effNams, drop = FALSE]
    }
    nc <- ncol(.x)
    nr <- nrow(.x)
    fData <- lapply(as.data.frame(.x), qqnorm, plot.it = FALSE)
    fData <- data.frame(.x = unlist(lapply(fData, function(x) x[["y"]])),
			.y = unlist(lapply(fData, function(x) x[["x"]])),
			.g = ordered(rep(names(fData),rep(nr, nc)),
                        levels = names(fData)), check.names = FALSE)
    dform <- ".y ~ .x | .g"
    if (!is.null(grp <- getGroupsFormula(form))) {
      dform <- paste(dform, deparse(grp[[2]]), sep = "*")
      auxData <- data[is.na(match(names(data), "."))]
    } else {
      auxData <- list()
    }
    ## id and idLabels - need not be present
    if (!is.null(id)) {			# identify points in plot
      N <- object$dims$N
      id <-
        switch(mode(id),
               numeric = {
                 if ((id <= 0) || (id >= 1)) {
                   stop("'id' must be between 0 and 1")
                 }
                 aux <- ranef(object, level = level, standard = TRUE)
                 as.logical(abs(c(unlist(aux))) > -qnorm(id / 2))
               },
               call = eval(asOneSidedFormula(id)[[2]], data),
               stop("'id' can only be a formula or numeric")
               )
      if (length(id) == N) {
        ## id as a formula evaluated in data
        auxData[[".id"]] <- id
      }

      if (is.null(idLabels)) {
        idLabels <- rep(row.names(.x), nc)
      } else {
        if (mode(idLabels) == "call") {
          idLabels <-
            as.character(eval(asOneSidedFormula(idLabels)[[2]], data))
        } else if (is.vector(idLabels)) {
          if (length(idLabels <- unlist(idLabels)) != N) {
            stop("'idLabels' of incorrect length")
          }
          idLabels <- as.character(idLabels)
        } else {
          stop("'idLabels' can only be a formula or a vector")
        }
      }
      if (length(idLabels) == N) {
        ## idLabels as a formula evaluated in data
        auxData[[".Lid"]] <- idLabels
      }
    }

    if (length(auxData)) {		# need collapsing
      auxData <- gsummary(as.data.frame(auxData),
                          groups = getGroups(object, level = level))
      auxData <- auxData[row.names(.x), , drop = FALSE]

      if (!is.null(auxData[[".id"]])) {
        id <- rep(auxData[[".id"]], nc)
      }

      if (!is.null(auxData[[".Lid"]])) {
        idLabels <- rep(auxData[[".Lid"]], nc)
      }
      data <- cbind(fData, do.call("rbind", rep(list(auxData), nc)))
    } else {
      data <- fData
    }
  }
  assign("id", if (is.null(id)) NULL else as.logical(as.character(id)))#,
  #   where = 1)
  assign("idLabels", as.character(idLabels))#, where = 1)
  #assign("grid", grid, where = 1)
  assign("abl", abline)#, where = 1)
  if (is.null(args$strip)) {
    args$strip <- function(...) strip.default(..., style = 1)
  }
  if (is.null(args$cex)) args$cex <- par("cex")
  if (is.null(args$adj)) args$adj <- par("adj")

  args <- c(list(eval(parse(text = dform)),
                 data = substitute(data)), args)
  if (is.null(args$panel)) {
    args <- c(list(panel = function(x, y, subscripts, ...){
      x <- as.numeric(x)
      y <- as.numeric(y)
      dots <- list(...)
      if (grid) panel.grid()
      panel.xyplot(x, y, ...)
      if (any(ids <- id[subscripts])){
          ltext(x[ids], y[ids], idLabels[subscripts][ids],
                cex = dots$cex, adj = dots$adj)
      }
      if (!is.null(abl)) { if (length(abl) == 2) panel.abline(a = abl, ...) else panel.abline(h = abl, ...) }
    }), args)
  }
  if(type == "reff" && !std) {
    args[["scales"]] <- list(x = list(relation = "free"))
  }
  do.call("xyplot", as.list(args))
}

ranef.lme <-
  ##  Extracts the random effects from an lme object.
  ##  If aug.frame is true, the returned data frame is augmented with a
  ##  values from the original data object, if available.  The variables
  ##  in the original data are collapsed over the cluster variable by the
  ##  function fun.
function(object, augFrame = FALSE, level = 1:Q, data, which = 1:ncol(data),
	 FUN = mean, standard = FALSE , omitGroupingFactor = TRUE,
         subset = NULL, ...)
{
  Q <- object$dims$Q
  effects <- object$coefficients$random
  if (Q > 1) {
    grpNames <- t(array(rep(rev(names(effects)), Q), c(Q, Q)))
    grpNames[lower.tri(grpNames)] <- ""
    grpNames <-
      rev(apply(grpNames, 1, function(x) paste(x[x != ""], collapse = " %in% ")))
  } else {
    grpNames <- names(effects)
  }
  effects <- effects[level]
  grpNames <- grpNames[level]
  if (standard) {
    for (i in names(effects)) {
      effects[[i]] <-
	t(t(effects[[i]]) / (object$sigma *
		     sqrt(diag(as.matrix(object$modelStruct$reStruct[[i]])))))
    }
  }
  effects <- lapply(effects, as.data.frame)
  if (augFrame) {
    if (length(level) > 1) {
      stop("augmentation of random effects only available for single level")
    }
    effects <- effects[[1]]
    effectNames <- names(effects)
    if (missing(data)) {
      data <- getData(object)
    }
    data <- as.data.frame(data)
    if (is.null(subset)) {              # nlme case
      subset <- eval(object$call[["naPattern"]])
    } else {
      subset <- asOneSidedFormula(as.list(match.call())[["subset"]])
    }
    if (!is.null(subset)) {
      subset <- eval(subset[[2]], data)
      data <- data[subset,  ,drop=FALSE]
    }
    data <- data[, which, drop = FALSE]
    ## eliminating columns with same names as effects
    data <- data[, is.na(match(names(data), effectNames)), drop = FALSE]
    grps <- as.character(object[["groups"]][, level])
    data <- gsummary(data, FUN = FUN, groups = grps)
    if (omitGroupingFactor) {
      data <-
	data[, is.na(match(names(data), names(object$modelStruct$reStruct))),
	      drop = FALSE]
    }
    if (length(data) > 0) {
      effects <- cbind(effects, data[row.names(effects),, drop = FALSE])
    }
    attr(effects, "effectNames") <- effectNames
  } else {
    effects <- lapply(effects,
                      function(el) {
                        attr(el, "effectNames") <- names(el)
                        el
                      })
    if (length(level) == 1) effects <- effects[[1]]
  }
  attr(effects, "label") <-
    if (standard) {
      "Standardized random effects"
    } else {
      "Random effects"
    }
  attr(effects, "level") <- max(level)
  attr(effects, "standardized") <- standard
  attr(effects, "grpNames") <- grpNames
  class(effects) <- c("ranef.lme", class(effects))
  effects
}

residuals.lme <-
  function(object, level = Q, type = c("response", "pearson", "normalized"),
           asList = FALSE, ...)

{
  type <- match.arg(type)
  Q <- object$dims$Q
  val <- object[["residuals"]]
  if (is.character(level)) {		# levels must be given consistently
    nlevel <- match(level, names(val))
    if (any(aux <- is.na(nlevel))) {
        stop(sprintf(ngettext(sum(aux),
                              "nonexistent level %s",
                              "nonexistent levels %s"),
                     level[aux]), domain = NA)
    }
    level <- nlevel
  } else {				# assuming integers
    level <- 1 + level
  }
  if (type != "response") {		# standardize
    ## have to standardize properly for when corStruct neq NULL
    val <- val[, level]/attr(val, "std")
  } else {
    val <- val[, level]
  }
  if (type == "normalized") {
    if (!is.null(cSt <- object$modelStruct$corStruct)) {
      ## normalize according to inv-trans factor
      val <- recalc(cSt, list(Xy = as.matrix(val)))$Xy[, 1:length(level)]
    } else {                            # will just standardized
      type <- "pearson"
    }
  }
  if (length(level) == 1) {
    grps <- as.character(object[["groups"]][, max(c(1, level - 1))])
    if (asList) {
      val <- as.list(split(val, ordered(grps, levels = unique(grps))))
    } else {
      grp.nm <- row.names(object[["groups"]])
      val <-naresid(object$na.action, val)
      names(val) <- grps[match(names(val), grp.nm)]
    }
    attr(val, "label") <-
      switch(type,
             response = {
               lab <- "Residuals"
               if (!is.null(aux <- attr(object, "units")$y)) {
                 lab <- paste(lab, aux)
               }
               lab
             },
             pearson = "Standardized residuals",
             normalized = "Normalized residuals"
             )
    val
  } else naresid(object$na.action, val)
}

summary.lme <- function(object, adjustSigma = TRUE, verbose = FALSE, ...)
{
  ##  variance-covariance estimates for fixed effects
  fixed <- fixef(object)
  stdFixed <- sqrt(diag(as.matrix(object$varFix)))
  object$corFixed <- array(t(object$varFix/stdFixed)/stdFixed,
                           dim(object$varFix), list(names(fixed),names(fixed)))
  if (adjustSigma && object$method == "ML")
    stdFixed <- stdFixed *
	sqrt(object$dims$N/(object$dims$N - length(stdFixed)))
  ## fixed effects coefficients, std. deviations and t-ratios
  ##
  tTable <- data.frame(fixed, stdFixed, object$fixDF[["X"]],
                       fixed/stdFixed, fixed)
  dimnames(tTable)<-
    list(names(fixed),c("Value", "Std.Error", "DF", "t-value", "p-value"))
  tTable[, "p-value"] <- 2 * pt(-abs(tTable[,"t-value"]), tTable[,"DF"])
  object$tTable <- as.matrix(tTable)
  ##
  ## residuals
  ##
  resd <- resid(object, type = "pearson")
  if (length(resd) > 5) {
    resd <- quantile(resd, na.rm = TRUE) # might have NAs from na.exclude
    names(resd) <- c("Min","Q1","Med","Q3","Max")
  }
  object$residuals <- resd
  ##
  ## generating the final object
  ##
  aux <- logLik(object)
  object$BIC <- BIC(aux)
  object$AIC <- AIC(aux)
  attr(object, "oClass") <- class(object)
  attr(object, "verbose") <- verbose
  class(object) <- c("summary.lme", class(object))

  object
}

# based on R's update.default
update.lme <-
    function (object, fixed., ..., evaluate = TRUE)
{
    call <- object$call
    if (is.null(call))
	stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(fixed.))
	call$fixed <- update.formula(formula(object), fixed.)
    if(length(extras) > 0) {
	existing <- !is.na(match(names(extras), names(call)))
	## do these individually to allow NULL to remove entries.
	for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
	if(any(!existing)) {
	    call <- c(as.list(call), extras[!existing])
	    call <- as.call(call)
	}
    }
    if(evaluate) eval(call, parent.frame())
    else call
}

#update.lme <-
#  function(object, fixed, data, random, correlation, weights, subset,
#           method, na.action, control, contrasts, ...)
#{
#  thisCall <- as.list(match.call())[-(1:2)]
#  if (is.null(nextCall <- object$origCall) ||
#      !is.null(thisCall$fixed) ||
#      is.null(thisCall$random)) {
#    nextCall <- object$call
#  }
#  nextCall <- as.list(nextCall)[-1]
#  if (is.null(thisCall$random)  && is.null(thisCall$subset)) {
#    ## no changes in ranef model and no subsetting
#    thisCall$random <- object$modelStruct$reStruct
#  }
#  if (is.na(match("correlation", names(thisCall))) &&
#      !is.null(thCor <- object$modelStruct$corStruct)) {
#    thisCall$correlation <- thCor
#  }
#  if (is.na(match("weights", names(thisCall))) &&
#      !is.null(thWgt <- object$modelStruct$varStruct)) {
#    thisCall$weights <- thWgt
#  }
#    argNams <- unique( c(names(nextCall), names(thisCall)) )
#    args <- vector("list", length(argNams))
#    names(args) <- argNams
#    args[ names(nextCall) ] <- nextCall
#    nextCall <- args
#  if (!is.null(thisCall$fixed)) {
#    thisCall$fixed <- update(as.formula(nextCall$fixed), fixed)
#  }
#  nextCall[names(thisCall)] <- thisCall
#  do.call("lme", nextCall)
#}

Variogram.lme <-
  function(object, distance, form = ~1,
           resType = c("pearson", "response", "normalized"),
           data, na.action = na.fail, maxDist, length.out = 50,
           collapse = c("quantiles", "fixed", "none"), nint = 20, breaks,
           robust = FALSE, metric = c("euclidean", "maximum", "manhattan"),
           ...)
{
  resType <- match.arg(resType)
  Q <- object$dims$Q
  grps <- getGroups(object, level = Q)
  ## checking if object has a corSpatial element
  csT <- object$modelStruct$corStruct
  wchRows <- NULL
  if (missing(distance)) {
    if (missing(form) && inherits(csT, "corSpatial")) {
      distance <- getCovariate(csT)
    } else {
      metric <- match.arg(metric)
      if (missing(data)) {
        data <- getData(object)
      }
      if (is.null(data)) {			# will try to construct
        allV <- all.vars(form)
        if (length(allV) > 0) {
          alist <- lapply(as.list(allV), as.name)
          names(alist) <- allV
          alist <- c(as.list(as.name("data.frame")), alist)
          mode(alist) <- "call"
          data <- eval(alist, sys.parent(1))
        }
      }
      covForm <- getCovariateFormula(form)
      if (length(all.vars(covForm)) > 0) {
        if (attr(terms(covForm), "intercept") == 1) {
          covForm <-
            eval(parse(text = paste("~", c_deparse(covForm[[2]]),"-1",sep="")))
        }
        covar <- model.frame(covForm, data, na.action = na.action)
        ## making sure grps is consistent
        wchRows <- !is.na(match(row.names(data), row.names(covar)))
        grps <- grps[wchRows, drop = TRUE]
        covar <- as.data.frame(unclass(model.matrix(covForm, covar)))
      } else {
        covar <-
          data.frame(dist = unlist(tapply(rep(1, nrow(data)), grps, cumsum)))
      }
      covar <- split(covar, grps)
      ## getting rid of 1-observation groups
      covar <- covar[sapply(covar, function(el) nrow(as.matrix(el))) > 1]
      distance <- lapply(covar,
                         function(el, metric) dist(as.matrix(el), metric),
                         metric = metric)
    }
  }
  res <- resid(object, type = resType)
  if (!is.null(wchRows)) {
    res <- res[wchRows]
  }
  res <- split(res, grps)
  res <- res[sapply(res, length) > 1] # no 1-observation groups
  levGrps <- levels(grps)
  val <- vector("list", length(levGrps))
  names(val) <- levGrps
  for(i in levGrps) {
    val[[i]] <- Variogram(res[[i]], distance[[i]])
  }
  val <- do.call("rbind", val)
  if (!missing(maxDist)) {
    val <- val[val$dist <= maxDist, ]
  }
  collapse <- match.arg(collapse)
  if (collapse != "none") {             # will collapse values
    dst <- val$dist
    udist <- sort(unique(dst))
    ludist <- length(udist)
    if (!missing(breaks)) {
      if (min(breaks) > udist[1]) {
        breaks <- c(udist[1], breaks)
      }
      if (max(breaks) < udist[2]) {
        breaks <- c(breaks, udist[2])
      }
      if (!missing(nint) && nint != (length(breaks) - 1)) {
        stop("'nint' is not consistent with 'breaks'")
      }
      nint <- length(breaks) - 1
    }
    if (nint < ludist) {
      if (missing(breaks)) {
        if (collapse == "quantiles") {    # break into equal groups
          breaks <- unique(quantile(dst, seq(0, 1, 1/nint)))
        } else {                          # fixed length intervals
          breaks <- seq(udist[1], udist[length(udist)], length = nint + 1)
        }
      }
      cutDist <- cut(dst, breaks)
    } else {
      cutDist <- dst
    }
    val <- lapply(split(val, cutDist),
                  function(el, robust) {
                    nh <- nrow(el)
                    vrg <- el$variog
                    if (robust) {
                      vrg <- ((mean(vrg^0.25))^4)/(0.457+0.494/nh)
                    } else {
                      vrg <- mean(vrg)
                    }
                    dst <- median(el$dist)
                    data.frame(variog = vrg, dist = dst)
                  }, robust = robust)
    val <- do.call("rbind", val)
    val$n.pairs <- as.vector(table(na.omit(cutDist)))
    val <- na.omit(val)                 # getting rid of NAs
  }
  row.names(val) <- 1:nrow(val)
  if (inherits(csT, "corSpatial") && resType != "normalized") {
    ## will keep model variogram
    if (resType == "pearson") {
      sig2 <- 1
    } else {
      sig2 <- object$sigma^2
    }
    attr(val, "modelVariog") <-
      Variogram(csT, sig2 = sig2, length.out = length.out)
  }
  attr(val, "collapse") <- collapse != "none"
  class(val) <- c("Variogram", "data.frame")
  val
}

###*### lmeStruct - a model structure for lme fits

lmeStruct <-
  ## constructor for lmeStruct objects
  function(reStruct, corStruct = NULL, varStruct = NULL)
{

  val <- list(reStruct = reStruct, corStruct = corStruct,
              varStruct = varStruct)
  val <- val[!sapply(val, is.null)]	# removing NULL components
  attr(val, "settings") <- attr(val$reStruct, "settings")
  class(val) <- c("lmeStruct", "modelStruct")
  val
}

##*## lmeStruct methods for standard generics

fitted.lmeStruct <-
  function(object, level = Q, conLin = attr(object, "conLin"),
           lmeFit = attr(object, "lmeFit"), ...)
{
  if (is.null(conLin)) {
    stop("no condensed linear model")
  }
  if (is.null(lmeFit)) {
    stop("no fitted \"lme\" object")
  }
  dd <- conLin$dims
  Q <- dd$Q
  Qp1 <- Q + 1
  nc <- dd$ncol
  fit <- array(0, c(dd$N, Qp1),
       list(dimnames(conLin$Xy)[[1]], c("fixed", rev(names(object$reStruct)))))
  ZXstart <- rev(cumsum(c(1, nc[1:Q])))
  ZXend <- rev(cumsum(nc[1:Qp1]))
  ZXlen <- dd$ZXlen[Q:1]
  ZXngrps <- dd$ngrps[Q:1]
  ZXb <- lmeFit$b
  nc <- nc[Q:1]

  fit[, "fixed"] <-			# population fitted values
    conLin$Xy[, ZXstart[1]: ZXend[1], drop = FALSE] %*% lmeFit$beta

  for(i in 1:Q) {
    j <- i + 1
    fit[, j] <- fit[, i] +
      (conLin$Xy[, ZXstart[j]:ZXend[j], drop = FALSE] *
       ZXb[[i]][rep(1:ZXngrps[i], ZXlen[[i]]),,drop = FALSE]) %*% rep(1, nc[i])
  }
  ## this is documented to return a vector for one level, matrix for more.
  ## So it should be a matrix if there is only one row, but not if
  ## there is only one columns.
  if(length(level) > 1) fit[, level + 1, drop = FALSE] else fit[, level+1]
}

Initialize.lmeStruct <-
  function(object, data, groups, conLin = attr(object, "conLin"),
	   control= list(niterEM = 20, gradHess = TRUE), ...)
{
  object[] <- lapply(object, Initialize, data, conLin, control)
  theta <- lapply(object, coef)
  len <- unlist(lapply(theta, length))
  num <- seq_along(len)
  if (sum(len) > 0) {
    pmap <- outer(rep(num, len), num, "==")
  } else {
    pmap <- array(FALSE, c(1, length(len)))
  }
  dimnames(pmap) <- list(NULL, names(object))
  attr(object, "pmap") <- pmap
  if (length(object) == 1  &&           # only reStruct
      all(attr(object, "settings")[-(1:3)] >= 0) && # known pdMat class
      control[["gradHess"]]) {
    ## can use numerical derivatives
    attr(object, "settings")[2:3] <- c(0, 1)
    class(object) <- c("lmeStructInt", class(object))
  }
  if (needUpdate(object)) {
    attr(object, "lmeFit") <- MEestimate(object, groups)
    update(object, data)
  } else {
    object
  }
}

logLik.lmeStruct <-
  function(object, Pars, conLin = attr(object, "conLin"), ...)
{
  coef(object) <- Pars			# updating parameter values
  recalc(object, conLin)[["logLik"]]	# updating conLin
}

logLik.lmeStructInt <-
  function(object, Pars, conLin = attr(object, "conLin"), ...)
{
  ## logLik for objects with reStruct parameters only, with
  ## internally defined class
  q <- length(Pars)
  aux <- .C(mixed_loglik,
	    as.double(conLin[["Xy"]]),
	    as.integer(unlist(conLin$dims)),
	    as.double(Pars),
	    as.integer(attr(object, "settings")),
	    val = double(1 + q * (q + 1)),
	    double(1))[["val"]]
  val <- aux[1]
  attr(val, "gradient") <- -aux[1 + (1:q)]
  attr(val, "hessian") <- -array(aux[-(1:(q+1))], c(q, q))
  val
}

residuals.lmeStruct <-
  function(object, level = Q, conLin = attr(object, "conLin"),
           lmeFit = attr(object, "lmeFit"), ...)
{
  Q <- conLin$dims$Q
  conLin$Xy[, conLin$dims$ZXcols] - fitted(object, level, conLin, lmeFit)
}

varWeights.lmeStruct <-
  function(object)
{
  if (is.null(object$varStruct)) rep(1, attr(object, "conLin")$dims$N)
  else varWeights(object$varStruct)
}

## Auxiliary control functions

lmeScale <- function(start)
#
# function used to set the scale inside ms(), for lme() and nlme()
# calls
#
{
  scale <- abs(start)
  nonzero <- scale > 0
  if (any(nonzero)) {
    scale[nonzero] <- 1/scale[nonzero]
    scale[!nonzero] <- median(scale[nonzero])
  }
  else {
    scale <- rep(1, length(scale))
  }
  scale
}

lmeControl <-
  ## Control parameters for lme
  function(maxIter = 50, msMaxIter = 50, tolerance = 1e-6, niterEM = 25,
           msMaxEval = 200,
	   msTol = 1e-7, msScale = lmeScale, msVerbose = FALSE,
           returnObject = FALSE, gradHess = TRUE, apVar = TRUE,
	   .relStep = (.Machine$double.eps)^(1/3), minAbsParApVar = 0.05,
           nlmStepMax = 100.0, opt = c("nlminb", "optim"),
	   optimMethod = "BFGS", natural = TRUE,
           ...)
{
  list(maxIter = maxIter, msMaxIter = msMaxIter, tolerance = tolerance,
       niterEM = niterEM, msMaxEval = msMaxEval, msTol = msTol, msScale = msScale,
       msVerbose = msVerbose, returnObject = returnObject,
       gradHess = gradHess , apVar = apVar, .relStep = .relStep,
       nlmStepMax = nlmStepMax, opt = match.arg(opt),
       optimMethod = optimMethod,
       minAbsParApVar = minAbsParApVar, natural = natural, ...)
}

## Local Variables:
## mode:S
## End:

