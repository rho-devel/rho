###  Fit a general nonlinear regression model with correlated and/or
###  heteroscedastic errors
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

gnls <-
  function(model,
	   data = sys.frame(sys.parent()),
	   params,
	   start,
           correlation = NULL,
           weights = NULL,
	   subset,
	   na.action = na.fail,
	   naPattern,
	   control = list(),
	   verbose= FALSE)
{
  finiteDiffGrad <-
	 function(model, data, pars)
	 {
	   dframe <- data.frame(data, pars)
	   base <- eval(model, dframe)
	   nm <- colnames(pars)
	   grad <- array(base, c(length(base), length(nm)), list(NULL, nm))
	   ssize <- sqrt(.Machine$double.eps)
	   for (i in nm) {
	     diff <- pp <- pars[ , i]
	     diff[pp == 0] <- ssize
	     diff[pp != 0] <- pp[pp != 0] * ssize
	     dframe[[i]] <- pp + diff
	     grad[ , i] <- (base - eval(model, dframe))/diff
	     dframe[[i]] <- pp
	   }
	   grad
         }

  ## keeping the call
  Call <- match.call()
  ## assigning a new name to the "object" argument
  form <- model

  ## control parameters
  controlvals <- gnlsControl()
  if (!missing(control)) {
    if(!is.null(control$nlmStepMax) && control$nlmStepMax < 0) {
      warning("Negative control$nlmStepMax - using default value")
      control$nlmStepMax <- NULL
    }
    controlvals[names(control)] <- control
  }
  ##
  ## checking arguments
  ##
  if (!inherits(form, "formula"))
    stop("\"object\" must be a formula")
  if (length(form)!=3)
    stop("object formula must be of the form \"resp ~ pred\"")

  ##
  ## checking if self-starting formula is given
  ##
  if (missing(start)) {
    if (!is.null(attr(eval(form[[3]][[1]]), "initial"))) {
      nlsCall <- Call[c("","model","data")]
      nlsCall[[1]] <- as.name("nls")
      names(nlsCall)[2] <- "formula"
      ## checking if "data" is not equal to sys.frame(sys.parent())
      if (is.null(dim(data))) {
        stop(paste("\"data\" must be given explicitly to use \"nls\"",
                   "to get initial estimates"))
      }
      start <- coef(eval(nlsCall))
    } else {
      stop("No initial values for model parameters")
    }
  } else {
    start <- unlist(start)
  }

  gnlsModel <- call("-", form[[2]], form[[3]])
  ##
  ## save writing list(...) when only one element
  ##

  if (missing(params)) {
    if (is.null(pNams <- names(start))) {
      stop("Starting estimates must have names when \"params\" is missing")
    }
    params <- eval(parse(text = paste(paste(pNams, collapse = "+"), "1",
                           sep = "~")))
  }
  if (!is.list(params)) {
    params <- list(params)
  }
  val <- NULL
  for(i in seq_along(params)) {
    if (is.name(params[[i]][[2]])) {
      val <- c(val, list(params[[i]]))
    } else {
      ## multiple parameters on left hand side
      val <- c(val, eval(parse(text = paste("list(",
           paste(paste(all.vars(params[[i]][[2]]), deparse(params[[i]][[3]]),
                       sep = "~"), collapse=","),")"))))
    }
  }
  params <- as.list(val)
  pnames <- character(length(params))
  for (i in seq_along(params)) {
    this <- eval(params[[i]])
    if (!inherits(this, "formula"))
      stop ("params must be a formula or list of formulae")
    if (length(this) != 3)
      stop ("formulae in params must be of the form \"parameter ~ expr\".")
    if (!is.name(this[[2]]))
      stop ("formulae in params must be of the form \"parameter ~ expr\".")
    pnames[i] <- as.character(this[[2]])
  }
  names(params) <- pnames

  ##
  ##  If data is a pframe, copy the parameters in the frame to frame 1
  ##  Doesn't exist in R
##  if (inherits(data, "pframe")) {
##    pp <- parameters(data)
##    for (i in names(pp)) {
##      assign(i, pp[[i]])
##    }
##    attr(data,"parameters") <- NULL
##    class(data) <- "data.frame"
##  }

  ## check if correlation is present and has groups
  if (!is.null(correlation)) {
    groups <- getGroupsFormula(correlation)
  } else groups <- NULL
#  if (!is.null(correlation)) {
#    groups <- getGroupsFormula(correlation, asList = TRUE)
#    if (!is.null(groups)) {
#      if (length(groups) > 1) {
#	stop("Only single level of grouping allowed")
#      }
#      groups <- groups[[1]]
#    } else {
#      if (inherits(data, "groupedData")) { # will use as groups
#	groups <- getGroupsFormula(data, asList = TRUE)
#	if (length(groups) > 1) {	# ignore it
#	  groups <- NULL
#	} else {
#          groups <- groups[[1]]
#          attr(correlation, "formula") <-
#            eval(parse(text = paste("~",
#                         deparse(getCovariateFormula(formula(correlation))[[2]]),
#			 "|", deparse(groups[[2]]))))
#        }
#      }
#    }
#  } else groups <- NULL

  ## create an gnls structure containing the correlation and weights
  gnlsSt <- gnlsStruct(corStruct = correlation, varStruct = varFunc(weights))

  ## extract a data frame with enough information to evaluate
  ## form, params, random, groups, correlation, and weights
  mfArgs <- list(formula = asOneFormula(formula(gnlsSt), form, params,
                   groups, omit = c(pnames, "pi")),
		 data = data, na.action = na.action)
  if (!missing(subset)) {
    mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2]]
  }
  mfArgs$drop.unused.levels <- TRUE
  dataMod <- do.call("model.frame", mfArgs)

  origOrder <- row.names(dataMod)	# preserve the original order
  ##
  ## Evaluating the groups expression, if needed
  ##
  if (!is.null(groups)) {
    ## sort the model.frame by groups and get the matrices and parameters
    ## used in the estimation procedures
    ## always use innermost level of grouping
    groups <- eval(parse(text = paste("~1", deparse(groups[[2]]), sep = "|")))
    grps <- getGroups(dataMod, groups,
                      level = length(getGroupsFormula(groups, asList = TRUE)))
    ## ordering data by groups
    ord <- order(grps)
    grps <- grps[ord]
    dataMod <- dataMod[ord, ,drop = FALSE]
##    revOrder <- match(origOrder, row.names(dataMod)) # putting in orig. order
  } else grps <- NULL

  N <- dim(dataMod)[1]			# number of observations
  ##
  ## evaluating the naPattern expression, if any
  ##
  if (missing(naPattern)) naPat <- rep(TRUE, N)
  else naPat <- as.logical(eval(asOneSidedFormula(naPattern)[[2]], dataMod))
  origOrderShrunk <- origOrder[naPat]

  dataModShrunk <- dataMod[naPat, , drop=FALSE]
  yShrunk <- eval(form[[2]], dataModShrunk)
  if (!is.null(groups)) {
##    ordShrunk <- ord[naPat]
    grpShrunk <- grps[naPat]
    revOrderShrunk <- match(origOrderShrunk, row.names(dataModShrunk))
  } else {
    grpShrunk <- NULL
  }

  ##
  ## defining list with parameter information
  ##
  contr <- list()
  plist <- vector("list", length(pnames))
  names(plist) <- pnames
  for (nm in pnames) {
    plist[[nm]] <- TRUE
    if (deparse(params[[nm]][[3]]) != "1") {
      plist[[nm]] <-
        model.matrix(asOneSidedFormula(params[[nm]][[3]]),
                 model.frame(asOneSidedFormula(params[[nm]][[3]]), dataModShrunk))
      auxContr <- attr(plist[[nm]], "contrasts")
      contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
    }
  }
  ##
  ## Params effects names
  ##
  pn <- character(0)
  currPos <- 0
  parAssign <- list()
  for(nm in pnames) {
    if (is.logical(p <- plist[[nm]])) {
      currPos <- currPos + 1
      currVal <- list(currPos)
      pn <- c(pn, nm)
      names(currVal) <- nm
      parAssign <- c(parAssign, currVal)
    } else {
      currVal <- attr(p, "assign")
      fTerms <- terms(asOneSidedFormula(params[[nm]][[3]]), data=data)
      namTerms <- attr(fTerms, "term.labels")
      if (attr(fTerms, "intercept") > 0) {
        namTerms <- c("(Intercept)", namTerms)
      }
      namTerms <- factor(currVal, labels = namTerms)
      currVal <- split(order(currVal), namTerms)
      names(currVal) <- paste(nm, names(currVal), sep = ".")
      parAssign <- c(parAssign, lapply(currVal,
                                       function(el, currPos) {
                                         el + currPos
                                       }, currPos = currPos))
      currPos <- currPos + length(unlist(currVal))
      pn <- c(pn, paste(nm, colnames(p), sep = "."))
    }
  }
  pLen <- length(pn)
  if (length(start) != pLen)
    stop ("starting values for parameters are not of the correct length")
  spar <- start
  names(spar) <- pn
  NReal <- sum(naPat)
  ##
  ## Creating the params map
  ##
  pmap <- list()
  n1 <- 1
  for(nm in pnames) {
    if (is.logical(p <- plist[[nm]])) {
      pmap[[nm]] <- n1
      n1 <- n1 + 1
    } else {
      pmap[[nm]] <- n1:(n1+ncol(p) - 1)
      n1 <- n1 + ncol(p)
    }
  }

  ##
  ## defining the nlFrame
  ##
  nlEnv <- new.env()
  nlList <-
                            list(model = gnlsModel,
			    data = dataMod,
			    plist = plist,
			    beta = as.vector(spar),
			    X = array(0, c(NReal, pLen), list(NULL, pn)),
			    pmap = pmap,
                            N = NReal,
                            naPat = naPat,
			    .parameters = c("beta"),
                            finiteDiffGrad = finiteDiffGrad)

  lapply(names(nlList), function(x, y, env) assign(x, y[[x]], envir = env),
         nlList, env = nlEnv)

  modelExpression <- ~{
    pars <- getParsGnls(plist, pmap, beta, N)
    res <- eval(model, data.frame(data, pars))
    if (!length(grad <- attr(res, "gradient"))) {
      grad <- finiteDiffGrad(model, data, pars)[naPat, , drop = FALSE]
    } else {
      grad <- grad[naPat, , drop = FALSE]
    }
    res <- res[naPat]
    for (nm in names(plist)) {
      gradnm <- grad[, nm]
      if (is.logical(p <- plist[[nm]])) {
        X[, pmap[[nm]]] <- gradnm
      } else {
        X[, pmap[[nm]]] <- gradnm * p
      }
    }
    result <- c(X, res)
    result[is.na(result)] <- 0
    result
  }

  modelResid <- ~eval(model, data.frame(data,
      getParsGnls(plist, pmap, beta, N)))[naPat]
  w <- eval(modelResid[[2]], envir = nlEnv)
  ## creating the condensed linear model
  Dims <- list(p = pLen, N = NReal, REML = FALSE)
  attr(gnlsSt, "conLin") <-
    list(Xy = array(w, c(NReal, 1),
           list(row.names(dataModShrunk), deparse(form[[2]]))), dims = Dims,
         logLik = 0)

  ## additional attributes of gnlsSt
  attr(gnlsSt, "resp") <- yShrunk
  attr(gnlsSt, "model") <- modelResid
  attr(gnlsSt, "local") <- nlEnv
  attr(gnlsSt, "NReal") <- NReal
  ## initialization
  gnlsSt <- Initialize(gnlsSt, dataModShrunk)
  parMap <- attr(gnlsSt, "pmap")

  numIter <- 0				# number of iterations
  nlsSettings <- c(controlvals$nlsMaxIter, controlvals$minScale,
                    controlvals$nlsTol, 0, 0, 0)
  nlModel <- nonlinModel(modelExpression, nlEnv)
  repeat {
  ## alternating algorithm
    numIter <- numIter + 1
    ## GLS step
    if (needUpdate(gnlsSt)) {             # updating varying weights
      gnlsSt <- update(gnlsSt, dataModShrunk)
    }
    if (length(oldPars <- coef(gnlsSt)) > 0) {
        if (controlvals$opt == "nlminb") {
            optRes <- nlminb(c(coef(gnlsSt)),
                             function(gnlsPars) -logLik(gnlsSt, gnlsPars),
                             control = list(trace = controlvals$msVerbose,
                             iter.max = controlvals$msMaxIter))
            convIter <- optRes$iterations
        } else {
            optRes <- optim(c(coef(gnlsSt)),
                            function(gnlsPars) -logLik(gnlsSt, gnlsPars),
                            method = controlvals$optimMethod,
                            control = list(trace = controlvals$msVerbose,
                            maxit = controlvals$msMaxIter,
                            reltol = if(numIter == 0) controlvals$msTol
                            else 100*.Machine$double.eps))
            convIter <- optRes$count[2]
        }
        aConv <- coef(gnlsSt) <- optRes$par
        if (verbose) {
            cat("\n**Iteration", numIter)
            cat("\n")
            cat("GLS step: Objective:", format(optRes$value))
            print(gnlsSt)
        }
    } else {
        aConv <- oldPars <- NULL
    }

    ## NLS step
    if (is.null(correlation)) {
      cF <- 1.0
      cD <- 1
    } else {
      cF <- corFactor(gnlsSt$corStruct)
      cD <- Dim(gnlsSt$corStruct)
    }
    if (is.null(weights)) {
      vW <- 1.0
    } else {
      vW <- varWeights(gnlsSt$varStruct)
    }
    work <- .C(fit_gnls,
	       thetaNLS = as.double(spar),
	       as.integer(unlist(Dims)),
	       as.double(cF),
	       as.double(vW),
               as.integer(unlist(cD)),
	       settings = as.double(nlsSettings),
	       additional = double(NReal),
	       as.integer(!is.null(correlation)),
	       as.integer(!is.null(weights)),
               nlModel,
	       NAOK = TRUE)
    if (work$settings[4] == 1) {
##      convResult <- 2
      if (controlvals$returnObject) {
        warning("Step halving factor reduced below minimum in NLS step")
      } else {
        stop("Step halving factor reduced below minimum in NLS step")
      }
      break
    }
    oldPars <- c(spar, oldPars)
    spar[] <- work$thetaNLS
    if (length(coef(gnlsSt)) == 0 && work$set[5] < controlvals$nlsMaxIter) {
      break
    }
    attr(gnlsSt, "conLin")$Xy[] <- work$additional
    attr(gnlsSt, "conLin")$logLik <- 0
    if (verbose) {
      cat("\nNLS step: RSS = ", format(work$set[6]), "\n model parameters:")
      for (i in 1:pLen) cat(format(signif(spar[i]))," ")
      cat("\n iterations:",work$set[5],"\n")
    }
    aConv <- c(spar, aConv)

    conv <- abs((oldPars - aConv)/
                ifelse(abs(aConv) < controlvals$tolerance, 1, aConv))
    aConv <- c(max(conv[1:pLen]))
    names(aConv) <- "params"
    if (length(conv) > pLen) {
      conv <- conv[-(1:pLen)]
      for(i in names(gnlsSt)) {
        if (any(parMap[,i])) {
          aConv <- c(aConv, max(conv[parMap[,i]]))
          names(aConv)[length(aConv)] <- i
        }
      }
    }

    if (verbose) {
      cat("\nConvergence:\n")
      print(aConv)
    }

    if ((max(aConv) <= controlvals$tolerance) ||
        (aConv["params"] <= controlvals$tolerance && convIter == 1)) {
##      convResult <- 0
      break
    }
    if (numIter >= controlvals$maxIter) {
##      convResult <- 1
      if (controlvals$returnObject) {
	warning("Maximum number of iterations reached without convergence")
	break
      } else {
	stop("Maximum number of iterations reached without convergence")
      }
    }
  }

  ## wraping up
  ww <- eval(modelExpression[[2]], envir = nlEnv)
  auxRes <- ww[NReal * pLen + (1:NReal)]
  attr(gnlsSt, "conLin")$Xy <- array(ww, c(NReal, pLen + 1))
  attr(gnlsSt, "conLin") <- recalc(gnlsSt)
  sigma <-
    sqrt(sum((attr(gnlsSt,"conLin")$Xy[,pLen+1])^2)/(NReal - pLen))
  varBeta <- qr(attr(gnlsSt, "conLin")$Xy[ , 1:pLen, drop = FALSE])
  if (varBeta$rank < pLen) {
    stop(paste("Approx. covariance matrix for parameter estimates",
               "not of full rank"))
  }
  lsig <- log(sigma) + 0.5 * log(1 - pLen/NReal)
  attr(parAssign, "varBetaFact") <- varBeta <-
    sigma * t(backsolve(qr.R(varBeta), diag(pLen)))
  varBeta <- crossprod(varBeta)
  dimnames(varBeta) <- list(pn, pn)
  loglik <- - NReal * (1 + log(2 * pi) + 2 * lsig) / 2 +
    attr(gnlsSt, "conLin")$logLik
  ##
  ## fitted.values and residuals (in original order)
  ##
  Resid <- resid(gnlsSt)
  Fitted <- yShrunk - Resid
  attr(Resid, "std") <- sigma/(varWeights(gnlsSt))
  if (!is.null(groups)) {
    attr(Resid, "std") <- attr(Resid, "std")[revOrderShrunk]
    Resid[] <- Resid[revOrderShrunk]
    Fitted[] <- Fitted[revOrderShrunk]
    grpShrunk[] <- grpShrunk[revOrderShrunk]
  }
  names(Resid) <- names(Fitted) <- origOrderShrunk
  ## getting the approximate var-cov of the parameters
  ## first making Xy into single column array again
  attr(gnlsSt, "conLin")$Xy <- array(auxRes, c(NReal, 1))
  if (controlvals$apVar) {
    apVar <- gnlsApVar(gnlsSt, lsig, .relStep = controlvals[[".relStep"]],
                       minAbsPar = controlvals[["minAbsParApVar"]])
  } else {
    apVar <- "Approximate variance-covariance matrix not available"
  }
  ## getting rid of condensed linear model and fit
  oClass <- class(gnlsSt)
  attributes(gnlsSt) <-
    attributes(gnlsSt)[!is.na(match(names(attributes(gnlsSt)),
                                    c("names","pmap")))]
  class(gnlsSt) <- oClass
  ##
  ## creating the  gnls object
  ##
  estOut <- list(modelStruct = gnlsSt,
		 dims = Dims,
                 contrasts = contr,
		 coefficients = spar,
		 varBeta = varBeta,
		 sigma = sigma,
		 apVar = apVar,
		 logLik = loglik,
		 numIter = numIter,
		 groups = grpShrunk,
		 call = Call,
                 method = "ML",
		 fitted = Fitted,
		 residuals = Resid,
		 plist = plist,
                 pmap = pmap,
                 parAssign = parAssign,
                 na.action = attr(dataMod, "na.action"))
  if (inherits(data, "groupedData")) {
    ## saving labels and units for plots
    attr(estOut, "units") <- attr(data, "units")
    attr(estOut, "labels") <- attr(data, "labels")
  }
  class(estOut) <- c("gnls", "gls")
  estOut
}

### Auxiliary functions used internally in gls and its methods

gnlsApVar <-
  function(gnlsSt, lsigma, conLin = attr(gnlsSt, "conLin"),
           .relStep = (.Machine$double.eps)^(1/3), minAbsPar = 0,
           natural = TRUE)
{
  ## calculate approximate variance-covariance matrix of all parameters
  ## except the coefficients
  fullGnlsLogLik <-
    function(Pars, object, conLin, N) {
      ## logLik as a function of sigma and coef(glsSt)
      npar <- length(Pars)
      lsigma <- Pars[npar]              # within-group std. dev.
      Pars <- Pars[-npar]
      coef(object) <- Pars
      conLin <- recalc(object, conLin)
      conLin[["logLik"]] - N * lsigma - sum(conLin$Xy^2)/(2*exp(2*lsigma))
    }
  if (length(gnlsCoef <- coef(gnlsSt)) > 0) {
    cSt <- gnlsSt[["corStruct"]]
    if (!is.null(cSt) && inherits(cSt, "corSymm") && natural) {
      cStNatPar <- coef(cSt, unconstrained = FALSE)
      class(cSt) <- c("corNatural", "corStruct")
      coef(cSt) <- log((cStNatPar + 1)/(1 - cStNatPar))
      gnlsSt[["corStruct"]] <- cSt
      gnlsCoef <- coef(gnlsSt)
    }
    dims <- conLin$dims
    N <- dims$N
    conLin[["logLik"]] <- 0               # making sure
    Pars <- c(gnlsCoef, lSigma = lsigma)
    val <- fdHess(Pars, fullGnlsLogLik, gnlsSt, conLin, N,
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
  } else {
    NULL
  }
}

###
### function used to calculate the parameters from
### the params and random effects
###

getParsGnls <-
  function(plist, pmap, beta, N)
{
  pars <- array(0, c(N, length(plist)), list(NULL, names(plist)))
  for (nm in names(plist)) {
    if (is.logical(p <- plist[[nm]])) {
      pars[, nm] <- beta[pmap[[nm]]]
    } else {
      pars[, nm] <- p %*% beta[pmap[[nm]]]
    }
  }
  pars
}

###
###  Methods for standard generics
###

coef.gnls <- function(object, ...) object$coefficients

formula.gnls <- function(x, ...) eval(x$call[["model"]])

getData.gnls <-
  function(object)
{
  mCall <- object$call
  data <- eval(mCall$data)
  if (is.null(data)) return(data)
  naPat <- eval(mCall$naPattern)
  if (!is.null(naPat)) {
    data <- data[eval(naPat[[2]], data), , drop = FALSE]
  }
  naAct <- eval(mCall$na.action)
  if (!is.null(naAct)) {
    data <- naAct(data)
  }
  subset <- mCall$subset
  if (!is.null(subset)) {
    subset <- eval(asOneSidedFormula(subset)[[2]], data)
    data <- data[subset, ]
  }
  return(data)
}


logLik.gnls <-
  function(object, REML = FALSE, ...)
{
  if (REML) {
    stop("Cannot calculate REML log-likelihood for gnls objects")
  }
  p <- object$dims$p
  N <- object$dims$N
  val <- object[["logLik"]]
  attr(val, "nobs") <- attr(val, "nall") <- N
  attr(val, "df") <- p + length(coef(object[["modelStruct"]])) + 1
  class(val) <- "logLik"
  val
}

nobs.gnls <- function(object, ...) object$dims$N

predict.gnls <-
  function(object, newdata, na.action = na.fail, naPattern = NULL, ...)
{
  ##
  ## method for predict() designed for objects inheriting from class gnls
  ##
  if (missing(newdata)) {		# will return fitted values
    return(fitted(object))
  }
  newdata <- data.frame(newdata, check.names = FALSE)
  mCall <- object$call

  mfArgs <- list(formula = asOneFormula(formula(object),
                   mCall$params, naPattern,
                   omit = c(names(object$plist), "pi",
                     deparse(getResponseFormula(object)[[2]]))),
                 data = newdata, na.action = na.action)
  mfArgs$drop.unused.levels <- TRUE
  dataMod <- do.call("model.frame", mfArgs)

  ## making sure factor levels are the same as in contrasts
  contr <- object$contrasts
  for(i in names(dataMod)) {
    if (inherits(dataMod[,i], "factor") &&
        !is.null(contr[[i]]) && is.matrix(contr[[i]]) ) {
      levs <- levels(dataMod[,i])
      levsC <- dimnames(contr[[i]])[[1]]
      if (any(wch <- is.na(match(levs, levsC)))) {
        stop(paste("Levels", paste(levs[wch], collapse = ","),
                   "not allowed for", i))
      }
      attr(dataMod[,i], "contrasts") <- contr[[i]][levs, , drop = FALSE]
#      if (length(levs) < length(levsC)) {
#        if (inherits(dataMod[,i], "ordered")) {
#          dataMod[,i] <- ordered(as.character(dataMod[,i]), levels = levsC)
#        } else {
#          dataMod[,i] <- factor(as.character(dataMod[,i]), levels = levsC)
#        }
#      }
    }
  }
  N <- nrow(dataMod)
  ##
  ## evaluating the naPattern expression, if any
  ##
  if (is.null(naPattern)) naPat <- rep(TRUE, N)
  else naPat <- as.logical(eval(asOneSidedFormula(naPattern)[[2]], dataMod))

  ##
  ## Getting  the plist for the new data frame
  ##
  ##
  plist <- object$plist
  pnames <- names(plist)
  if (is.null(params <- eval(object$call$params))) {
    params <- eval(parse(text = paste(paste(pnames, collapse = "+"), "1",
                           sep = "~")))
  }
  if (!is.list(params)) {
    params <- list(params)
  }
  val <- NULL
  for(i in seq_along(params)) {
    if (is.name(params[[i]][[2]])) {
      val <- c(val, list(params[[i]]))
    } else {
      ## multiple parameters on left hand side
      val <- c(val, eval(parse(text = paste("list(",
           paste(paste(all.vars(params[[i]][[2]]), deparse(params[[i]][[3]]),
                       sep = "~"), collapse=","),")"))))
    }
  }
  params <- val
  names(params) <- pnames
  prs <- coef(object)
  pn <- names(prs)
  for(nm in pnames) {
    if (!is.logical(plist[[nm]])) {
      plist[[nm]] <- model.matrix(asOneSidedFormula(params[[nm]][[3]]),
                model.frame(asOneSidedFormula(params[[nm]][[3]]), dataMod))
    }
  }
  modForm <- getCovariateFormula(object)[[2]]
  val <- eval(modForm, data.frame(dataMod,
              getParsGnls(plist, object$pmap, prs, N)))[naPat]
  names(val) <- row.names(newdata)
  lab <- "Predicted values"
  if (!is.null(aux <- attr(object, "units")$y)) {
    lab <- paste(lab, aux)
  }
  attr(val, "label") <- lab
  val
}

#based on R's update.default
update.gnls <-
    function (object, model., ..., evaluate = TRUE)
{
    call <- object$call
    if (is.null(call))
	stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(model.))
	call$model <- update.formula(formula(object), model.)
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
#update.gnls <-
#  function(object, model, data = sys.frame(sys.parent()), params, start ,
#           correlation = NULL, weights = NULL, subset,
#           na.action = na.fail, naPattern, control = list(),
#	   verbose = FALSE, ...)
#{
#  thisCall <- as.list(match.call())[-(1:2)]
#  nextCall <- as.list(object$call)[-1]
#  if (!is.null(thisCall$model)) {
#    thisCall$model <- update(formula(object), model)
#  } else {                              # same model
#    if (is.null(thisCall$start)) {
#      thisCall$start <- coef(object)
#    }
#  }
#  if (is.na(match("correlation", names(thisCall))) &&
#      !is.null(thCor <- object$modelStruct$corStruct)) {
#    thisCall$correlation <- thCor
#  }
#  if (is.na(match("weights", names(thisCall))) &&
#      !is.null(thWgt <- object$modelStruct$varStruct)) {
#    thisCall$weights <- thWgt
#  }
#  nextCall[names(thisCall)] <- thisCall
#  do.call("gnls", nextCall)
#}

###*### gnlsStruct - a model structure for gnls fits

gnlsStruct <-
  ## constructor for gnlsStruct objects
  function(corStruct = NULL, varStruct = NULL)
{

  val <- list(corStruct = corStruct, varStruct = varStruct)
  val <- val[!sapply(val, is.null)]	# removing NULL components
#  attr(val, "settings") <- attr(val$reStruct, "settings")
#  attr(val, "resp") <- resp
#  attr(val, "model") <- model
#  attr(val, "local") <- local
#  attr(val, "N") <- N
#  attr(val, "naPat") <- naPat
  class(val) <- c("gnlsStruct", "glsStruct", "modelStruct")
  val
}

##*## gnlsStruct methods for standard generics

fitted.gnlsStruct <-
  function(object, ...)
{
  attr(object, "resp") - resid(object)
}

Initialize.gnlsStruct <-
  function(object, data, ...)
{
  if (length(object)) {
    object[] <- lapply(object, Initialize, data)
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
    if (needUpdate(object)) {
      object <- update(object, data)
    }
  }
  object
}

logLik.gnlsStruct <-
  function(object, Pars, conLin = attr(object, "conLin"), ...)
{
  coef(object) <- Pars			# updating parameter values
  conLin <- recalc(object, conLin)	# updating conLin
  conLin[["logLik"]] - conLin$dims$N * log(sum(conLin$Xy^2)) / 2
}


residuals.gnlsStruct <-
  function(object, ...)
{
  c(eval(attr(object, "model")[[2]], envir = attr(object, "local")))
}

gnlsControl <-
  ## Set control values for iterations within gnls
  function(maxIter = 50, nlsMaxIter = 7, msMaxIter = 50,
	   minScale = 0.001, tolerance = 1e-6, nlsTol = 0.001,
           msTol = 1e-7, msScale = lmeScale,
           returnObject = FALSE, msVerbose = FALSE,
           apVar = TRUE, .relStep = (.Machine$double.eps)^(1/3),
           nlmStepMax = 100.0,
	   opt = c("nlminb", "optim"),  optimMethod = "BFGS",
           minAbsParApVar = 0.05)
{
  list(maxIter = maxIter, nlsMaxIter = nlsMaxIter, msMaxIter = msMaxIter,
       minScale = minScale, tolerance = tolerance, nlsTol = nlsTol,
       msTol = msTol, msScale = msScale, returnObject = returnObject,
       msVerbose = msVerbose, apVar = apVar,
       nlmStepMax = nlmStepMax, opt = match.arg(opt), optimMethod = optimMethod,
       .relStep = .relStep, minAbsParApVar = minAbsParApVar)
}
