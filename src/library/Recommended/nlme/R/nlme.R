###            Fit a general nonlinear mixed effects model
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

nlme <-
  function(model,
	   data = sys.frame(sys.parent()),
	   fixed,
	   random = fixed,
           groups,
	   start,
           correlation = NULL,
           weights = NULL,
	   subset,
	   method = c("ML", "REML"),
	   na.action = na.fail,
	   naPattern,
	   control = list(),
	   verbose= FALSE)
{
  UseMethod("nlme")
}

nlme.nlsList <-
  function(model,
	   data = sys.frame(sys.parent()),
	   fixed,
	   random = fixed,
           groups,
	   start,
           correlation = NULL,
           weights = NULL,
	   subset,
	   method = c("ML", "REML"),
	   na.action = na.fail,
	   naPattern,
	   control = list(),
	   verbose= FALSE)
{
  ## control parameters
  controlvals <- nlmeControl()
  controlvals[names(control)] <- control

  thisCall <- as.list(match.call())[-1]
  ## checking the use of arguments defined within the function
  if (any(!is.na(match(names(thisCall),
		       c("fixed", "data", "start"))))) {
    warning(paste("nlme.nlsList will redefine \"fixed\"",
		  "\"data\", and \"start\""))
  }
  method <- match.arg(method)
  REML <- method == "REML"
  ## add model, data, and optionally groups from the call that created model
  last.call <- as.list(attr(model, "call"))[-1]
  last.call$control <- NULL
  last.call$pool <- NULL
  thisCall[names(last.call)] <- last.call
  thisModel <- last.call[["model"]]
  thisCall[["model"]] <-
    eval(parse(text=paste(deparse(getResponseFormula(thisModel)[[2]]),
                 c_deparse(getCovariateFormula(thisModel)[[2]]),sep="~")))
  ## create "fixed" and "start"
  cf <- na.omit(coef(model))
  start <- list(fixed = unlist(lapply(cf, median, na.rm = TRUE)))
  pnames <- names(start$fixed) <- names(cf)
  thisCall[["fixed"]] <- lapply(as.list(pnames), function(el)
                                 eval(parse(text = paste(el, 1, sep = "~"))))
  if (missing(random)) {
    random <- thisCall[["fixed"]]
  }
  reSt <- reStruct(random, data = NULL)
  if (missing(groups)) {
    thisCall[["groups"]] <- groups <- getGroupsFormula(model)
  }
  if (length(reSt) > 1 || length(groups[[2]]) > 1) {
    stop("Can only fit nlsList objects with single grouping variable")
  }
  ranForm <- formula(reSt)[[1]]
  if (!is.list(ranForm)) {
    ranForm <- list(ranForm)
  }
  mData <- thisCall[["data"]]
  if (is.null(mData)) {			# will try to construct
    allV <- unique(unlist(lapply(ranForm, function(el) all.vars(el[[3]]))))
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
  reSt <- reStruct(random, REML = REML, data = mData)
  names(reSt) <- deparse(groups[[2]])
  ## convert list of "name" objects to "character" vector
  rnames <- sapply(lapply(ranForm, "[[", 2), deparse)
  ## if the random effects are a subset of the coefficients,
  ## construct initial estimates for their var-cov matrix
  if (all(match(rnames, pnames, 0))) {
    madRes <- mad(resid(model), na.rm = TRUE)
    madRan <- unlist(lapply(cf, mad, na.rm = TRUE))
    madRan <- madRan[rnames]
    if (isInitialized(reSt)) {
      warning("Initial value for reStruct overwritten in nlme.nlsList")
    }
    matrix(reSt) <- diag((madRan/madRes)^2, ncol = length(rnames))
  }
  thisCall[["start"]] <- start
  thisCall[["random"]] <- reSt
  val <- do.call("nlme.formula", thisCall)
  val$origCall <- match.call()
  val
}


nlme.formula <-
  function(model,
	   data = sys.frame(sys.parent()),
	   fixed,
	   random,
           groups,
	   start,
           correlation = NULL,
           weights = NULL,
	   subset,
	   method = c("ML", "REML"),
	   na.action = na.fail,
	   naPattern,
	   control = list(),
	   verbose= FALSE)
{
  ## This is the method that actually does the fitting
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

  ## control parameters
  controlvals <- nlmeControl()
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
  if (!inherits(model, "formula"))
    stop("\"model\" must be a formula")
  if (length(model)!=3)
    stop("model formula must be of the form \"resp ~ pred\"")

  method <- match.arg(method)
  REML <- method == "REML"
  if (missing(random)) {
    random <- fixed
  }
  reSt <- reStruct(random, REML = REML, data = NULL)
  if (missing(groups)) {
    groups <- getGroupsFormula(reSt)
  }
  if (is.null(groups)) {
    if (inherits(data, "groupedData")) {
      groups <- getGroupsFormula(data)
      namGrp <- rev(names(getGroupsFormula(data, asList = TRUE)))
      Q <- length(namGrp)
      if (length(reSt) != Q) { # may need to repeat reSt
	if (length(reSt) != 1) {
	  stop("Incompatible lengths for \"random\" and grouping factors")
	}
        randL <- vector("list", Q)
        names(randL) <- rev(namGrp)
        for(i in 1:Q) randL[[i]] <- random
        reSt <- reStruct(randL, REML = REML, data = NULL)
      }
    } else {
      ## will assume single group
      groups <- ~ 1
      names(reSt) <- namGrp <- "1"

    }
  } else {
    namGrp <- rev(names(getGroupsFormula(eval(parse(text =
                                   paste("~1", deparse(groups[[2]]),sep="|"))),
                        asList = TRUE)))
  }
  names(reSt) <- namGrp
  ##
  ## checking if self-starting formula is given
  ##
  if (missing(start) && !is.null(attr(eval(model[[3]][[1]]), "initial"))) {
    nlmeCall <- Call
    nlsLCall <- nlmeCall[c("","model","data","groups")]
    nlsLCall[[1]] <- as.name("nlsList")
    names(nlsLCall)[2] <- "model"
    for(i in c("data", "groups", "start")) {
      nlmeCall[[i]] <- NULL
    }
    nlmeCall[[1]] <- as.name("nlme.nlsList")
    ## checking if "data" is not equal to sys.frame(sys.parent())
    if (is.null(dim(data))) {
      stop("\"data\" must be given explicitly to use \"nlsList()\"")
    }
    nlsLObj <- eval(nlsLCall)
    nlmeCall[["model"]] <- as.name("nlsLObj")
    nlmeCall <- as.call(nlmeCall)
    val <- eval(nlmeCall)
    val$origCall <- NULL
    return(val)
  }
  if (is.numeric(start)) {               # assume it is the fixed effects
    start <- list(fixed = start)
  }
  nlmeModel <- call("-", model[[2]], model[[3]])
  ##
  ## save writing list(...) when only one element
  ##

  if (!is.list(fixed)) {
    fixed <- list(fixed)
  }
  val <- NULL
  for(i in seq_along(fixed)) {
    if (is.name(fixed[[i]][[2]])) {
      val <- c(val, list(fixed[[i]]))
    } else {
      ## multiple parameters on left hand side
      val <- c(val, eval(parse(text = paste("list(",
           paste(paste(all.vars(fixed[[i]][[2]]), deparse(fixed[[i]][[3]]),
                       sep = "~"), collapse=","),")"))))
    }
  }
  fixed <- as.list(val)
  fnames <- character(length(fixed))
  for (i in seq_along(fixed)) {
    this <- eval(fixed[[i]])
    if (!inherits(this, "formula"))
      stop ("fixed must be a formula or list of formulae")
    if (length(this) != 3)
      stop ("formulae in fixed must be of the form \"parameter ~ expr\".")
    if (!is.name(this[[2]]))
      stop ("formulae in fixed must be of the form \"parameter ~ expr\".")
    fnames[i] <- as.character(this[[2]])
  }
  names(fixed) <- fnames

  ranForm <- formula(reSt)              # random effects formula(s)
  Q <- length(ranForm)                  # number of groups
  names(ranForm) <- namGrp
  rnames <- vector("list", Q)
  names(rnames) <- namGrp
  for(i in 1:Q) {
    rnames[[i]] <- character(length(ranForm[[i]]))
    for (j in seq_along(ranForm[[i]])) {
      this <- eval(ranForm[[i]][[j]])
      if (!inherits(this, "formula"))
        stop ("random formula must be a formula or list of formulae")
      if (length(this) != 3)
        stop ("formulae in random must be of the form \"parameter ~ expr\".")
      if (!is.name(this[[2]]))
        stop ("formulae in random must be of the form \"parameter ~ expr\".")
      rnames[[i]][j] <- deparse(this[[2]])
    }
    names(ranForm[[i]]) <- rnames[[i]]
  }
  ## all parameter names
  pnames <- unique(c(fnames, unlist(rnames)))
  ##
  ##  If data is a pframe, copy the parameters in the frame to frame 1
  ## Doesn't exist in R
##  if (inherits(data, "pframe")) {
##    pp <- parameters(data)
##    for (i in names(pp)) {
##      assign(i, pp[[i]])
##    }
##    attr(data,"parameters") <- NULL
##    class(data) <- "data.frame"
##  }

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
          stop(paste("Incompatible formulas for groups in \"random\"",
                     "and \"correlation\""))
        }
        if (corQ < lmeQ) {
          warning(paste("Cannot use smaller level of grouping for",
                        "\"correlation\" than for \"random\". Replacing",
                        "the former with the latter."))
          attr(correlation, "formula") <-
            eval(parse(text = paste("~",
                    c_deparse(getCovariateFormula(formula(correlation))[[2]]),
                         "|", deparse(groups[[2]]))))
        }
      } else {
        if (any(lmeGrpsForm != corGrpsForm[1:lmeQ])) {
          stop(paste("Incompatible formulas for groups in \"random\"",
                     "and \"correlation\""))
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
  ## create an nlme structure containing the random effects model and plug-ins
  nlmeSt <- nlmeStruct(reStruct = reSt, corStruct = correlation,
                       varStruct = varFunc(weights))

  ## extract a data frame with enough information to evaluate
  ## form, fixed, random, groups, correlation, and weights
  mfArgs <- list(formula = asOneFormula(formula(nlmeSt), model, fixed,
                   groups, omit = c(pnames, "pi")),
		 data = data, na.action = na.action)
  if (!missing(subset)) {
    mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2]]
  }
  mfArgs$drop.unused.levels <- TRUE
  dataMix <- do.call("model.frame", mfArgs)

  origOrder <- row.names(dataMix)	# preserve the original order
  ##
  ## Evaluating the groups expression
  ##
  grps <- getGroups(dataMix,
	     eval(parse(text = paste("~1", deparse(groups[[2]]), sep = "|"))))
  N <- dim(dataMix)[1]			# number of observations
  ##
  ## evaluating the naPattern expression, if any
  ##
  if (missing(naPattern)) naPat <- rep(TRUE, N)
  else naPat <- as.logical(eval(asOneSidedFormula(naPattern)[[2]], dataMix))
  origOrderShrunk <- origOrder[naPat]

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
##  revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order
  naPat <- naPat[ord]			# ordered naPat
  dataMixShrunk <- dataMix[naPat, , drop=FALSE]
##  ordShrunk <- ord[naPat]
  grpShrunk <- grps[naPat,, drop = FALSE]
  revOrderShrunk <- match(origOrderShrunk, row.names(dataMixShrunk))
  yShrunk <- eval(model[[2]], dataMixShrunk)

  ##
  ## defining list with parameter information
  ##
  contr <- list()
  plist <- vector("list", length(pnames))
  names(plist) <- pnames
  for (nm in pnames) {
    this <- list(fixed = !is.null(fixed[[nm]]),
                 random = as.list(lapply(ranForm, function(el, nm)
                   !is.null(el[[nm]]), nm = nm)))
    if (this[["fixed"]]) {
## Peter Dalgaard claims the next line should be this[["fixed"]][[3]][[1]] != "1"
## but the current version seems to work ok.
      if (fixed[[nm]][[3]] != "1") {
	this[["fixed"]] <-
          model.matrix(asOneSidedFormula(fixed[[nm]][[3]]),
                  model.frame(asOneSidedFormula(fixed[[nm]][[3]]), dataMix))
        auxContr <- attr(this[["fixed"]], "contrasts")
        contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
      }
    }
    if (any(unlist(this[["random"]]))) {
      for(i in 1:Q) {
        wch <- (1:length(rnames[[i]]))[!is.na(match(rnames[[i]], nm))]
        if (length(wch) == 1) {           # only one formula for nm at level i
          if (ranForm[[i]][[nm]][[3]] != "1") {
            this[["random"]][[i]] <-
              model.matrix(asOneSidedFormula(ranForm[[i]][[nm]][[3]]),
                        model.frame(asOneSidedFormula(ranForm[[i]][[nm]][[3]]),
                                    dataMix))
            auxContr <- attr(this[["random"]][[i]], "contrasts")
            contr <-
              c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
          }
        } else if (length(wch) > 0) {    # multiple formulas
          this[["random"]][[i]] <-
            as.list(lapply(ranForm[[i]][wch], function(el, data) {
              if (el[[3]] == "1") TRUE
              else model.matrix(asOneSidedFormula(el[[3]]),
                                model.frame(asOneSidedFormula(el[[3]]), data))
            }, data = dataMix))
          for(j in seq_along(this[["random"]][[i]])) {
            if (is.matrix(this[["random"]][[i]][[j]])) {
              auxContr <- attr(this[["random"]][[i]][[j]], "contrasts")
              contr <-
                c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
            }
          }
        }
      }
    }
    plist[[nm]] <- this
  }
  ## Ensure that all elements of are matrices
  contrMat <- function(nm, contr, data)
  {
    ## nm is a term in a formula, and can be a call
    x <- eval(parse(text=nm), data)
    levs <- levels(x)
    val <- do.call(contr[[nm]], list(n = length(levs)))
    rownames(val) <- levs
    val
  }
  nms <- names(contr)[sapply(contr, is.character)]
  contr[nms] <- lapply(nms, contrMat, contr = contr, data = dataMix)

  if (is.null(sfix <- start$fixed))
    stop ("start must have a component called \"fixed\"")
  ##
  ## Fixed effects names
  ##
  fn <- character(0)
  currPos <- 0
  fixAssign <- list()
  for(nm in fnames) {
    if (is.logical(f <- plist[[nm]]$fixed)) {
      currPos <- currPos + 1
      currVal <- list(currPos)
      if (all(unlist(lapply(plist[[nm]]$random, is.logical)))) {
        fn <- c(fn, nm)
        names(currVal) <- nm
      } else {
        aux <- paste(nm, "(Intercept)", sep=".")
        fn <- c(fn, aux)
        names(currVal) <- aux
      }
      fixAssign <- c(fixAssign, currVal)
    } else {
      currVal <- attr(f, "assign")
      fTerms <- terms(asOneSidedFormula(fixed[[nm]][[3]]), data=data)
      namTerms <- attr(fTerms, "term.labels")
      if (attr(fTerms, "intercept") > 0) {
        namTerms <- c("(Intercept)", namTerms)
      }
      namTerms <- factor(currVal, labels = namTerms)
      currVal <- split(order(currVal), namTerms)
      names(currVal) <- paste(nm, names(currVal), sep = ".")
      fixAssign <- c(fixAssign, lapply(currVal,
                                 function(el, currPos) {
                                   el + currPos
                                 }, currPos = currPos))
      currPos <- currPos + length(unlist(currVal))
      fn <- c(fn, paste(nm, colnames(f), sep = "."))
    }
  }
  fLen <- length(fn)
  if (length(sfix) != fLen)
    stop ("starting values for the fixed component are not the correct length")
  names(sfix) <- fn
  ##
  ## Random effects names
  ##
  rn <- wchRnames <- vector("list", Q)
  names(rn) <- names(wchRnames) <- namGrp
  for(i in 1:Q) {
    rn[[i]] <- character(0)
    uRnames <- unique(rnames[[i]])
    wchRnames[[i]] <- integer(length(uRnames))
    names(wchRnames[[i]]) <- uRnames
    for(j in seq_along(rnames[[i]])) {
      nm <- rnames[[i]][j]
      wchRnames[[i]][nm] <- wchRnames[[i]][nm] + 1
      r <- plist[[nm]]$random[[i]]
      if (data.class(r) == "list") r <- r[[wchRnames[[i]][nm]]]
      if (is.logical(r)) {
        if (r) {
          if (is.logical(plist[[nm]]$fixed)) {
            rn[[i]] <- c(rn[[i]], nm)
          } else {
            rn[[i]] <- c(rn[[i]], paste(nm,"(Intercept)",sep="."))
          }
        }
      } else {
        rn[[i]] <- c(rn[[i]], paste(nm, colnames(r), sep = "."))
      }
    }
  }
  Names(nlmeSt$reStruct) <- rn
  rNam <- unlist(rn)                    # unlisted names of random effects
  rlength<- unlist(lapply(rn, length))  # number of random effects per stratum
  rLen <- sum(rlength)                  # total number of random effects
  pLen <- rLen + fLen                   # total number of parameters
  ncols <- c(rlength, fLen, 1)
  Dims <- MEdims(grpShrunk, ncols)
  if (max(Dims$ZXlen[[1]]) < Dims$qvec[1]) {
    warning(paste("Fewer observations than random effects in all level",
                  Q,"groups"))
  }
  sran <- vector("list", Q)
  names(sran) <- namGrp
  if (!is.null(sran0 <- start$random)) {
    if (inherits(sran0, "data.frame")) {
      sran0 <- list(as.matrix(sran0))
    } else {
      if (!is.list(sran0)) {
        if (!is.matrix(sran0)) {
          stop("Starting values for random effects should be a list, or a matrix")
        }
        sran0 <- list(as.matrix(sran0))
      }
    }
    if (is.null(namSran <- names(sran0))) {
      if (length(sran) != Q) {
        stop(paste("List with starting values for random effects must have names",
                   "or be of length", Q))
      }
      names(sran0) <- rev(namGrp)        # assume given in outer-inner order
    } else {
      if (any(noMatch <- is.na(match(namSran, namGrp)))) {
        stop(paste("Group names not matched in starting values",
                   "for random effects:", paste(namSran[noMatch], collapse=", ")))
      }
    }
  }
  for(i in 1:Q) {
    if (is.null(sran[[i]] <- sran0[[namGrp[i]]])) {
      sran[[i]] <- array(0, c(rlength[i], Dims$ngrps[i]),
                  list(rn[[i]], unique(as.character(grps[, Q-i+1]))))
    } else {
      if (!is.matrix(sran[[i]]))
        stop (paste("starting values for the random components should be",
                    "a list of matrices"))
      dimsran <- dim(sran[[i]])
      if (dimsran[1] != Dims$ngrps[i]) {
        stop (paste("number of rows in starting values for random component",
                  "at level", namGrp[i], "should be", Dims$ngrps[i]))
      }
      if (dimsran[2] != rlength[i]) {
        stop (paste("number of columns in starting values for",
                    "random component at level", namGrp[i],
                    "should be", rlength[i]))
      }
      dnamesran <- dimnames(sran[[i]])
      if (is.null(dnamesran[[1]])) {
        stop("starting values for random effects must include group levels")
      } else {
        levGrps <- unique(as.character(grps[, Q-i+1]))
        if(!all(sort(dnamesran[[1]]) == sort(levGrps))) {
          stop (paste("groups levels mismatch in random and starting values",
                      "for random at level", namGrp[i]))
        }
        sran[[i]] <- sran[[i]][levGrps, , drop = FALSE]
      }

      if (!is.null(dnamesran[[2]])) {
        if(!all(sort(dnamesran[[2]]) == sort(rn[[i]]))) {
          ## first try to resolve it
          for(j in 1:rlength[i]) {
            if (is.na(match(dnamesran[[2]][j], rn[[i]]))) {
              if (!is.na(mDn <- match(paste(dnamesran[[2]][j],
                                       "(Intercept)", sep="."), rn[[i]]))) {
                dnamesran[[2]][j] <- rn[[i]][mDn]
              } else {
                if (!is.na(mDn <- match(dnamesran[[2]][j],
                           paste(rn[[i]], "(Intercept)", sep = ".")))) {
                  dnamesran[[2]][j] <- rn[[i]][mDn]
                } else {
                  stop (paste("names mismatch in random and starting values",
                              "for random at level", namGrp[i]))
                }
              }
            }
          }
          dimnames(sran[[i]]) <- dnamesran
        }
        sran[[i]] <- sran[[i]][, rn[[i]], drop = FALSE]
      } else {
        dimnames(sran[[i]])[[2]] <- rn[[i]]
      }
      sran[[i]] <- t(sran[[i]])
    }
  }
  names(sran) <- namGrp
  nPars <- length(unlist(sran)) + fLen  # total number of PNLS parameters
  ##
  ##   defining values of constants used in calculations
  ##
  NReal <- sum(naPat)
  ##
  ## Creating the fixed and random effects maps
  ##
  fmap <- list()
  n1 <- 1
  for(nm in fnames) {
    if (is.logical(f <- plist[[nm]]$fixed)) {
      fmap[[nm]] <- n1
      n1 <- n1 + 1
    } else {
      fmap[[nm]] <- n1:(n1+ncol(f) - 1)
      n1 <- n1 + ncol(f)
    }
  }
  rmap <- rmapRel <- vector("list", Q)
  names(rmap) <- names(rmapRel) <- namGrp
  n1 <- 1
  startRan <- 0
  for(i in 1:Q) {
    wchRnames[[i]][] <- 0
    rmap[[i]] <- rmapRel[[i]] <- list()
    for(nm in rnames[[i]]) {
      wchRnames[[i]][nm] <- wchRnames[[i]][nm] + 1
      r <- plist[[nm]]$random[[i]]
      if (data.class(r) == "list") {
        r <- r[[wchRnames[[i]][nm]]]
      }
      if (is.logical(r)) {
        val <- n1
        n1 <- n1 + 1
      } else {
        val <- n1:(n1+ncol(r) - 1)
        n1 <- n1 + ncol(r)
      }
      if (is.null(rmap[[i]][[nm]])) {
        rmap[[i]][[nm]] <- val
        rmapRel[[i]][[nm]] <- val - startRan
      } else {
        rmap[[i]][[nm]] <- c(rmap[[i]][[nm]], list(val))
        rmapRel[[i]][[nm]] <- c(rmapRel[[i]][[nm]], list(val - startRan))
      }
    }
    startRan <- startRan + ncols[i]
  }

  ##
  ## defining the nlFrame
  ##
  grpsRev <- rev(lapply(grps, as.character))
  bmap <- c(0, cumsum(unlist(lapply(sran, function(el) length(as.vector(el))))))
  nlEnv <- new.env()
  nlList <-
                            list(model = nlmeModel,
			    data = dataMix,
			    groups = grpsRev,
			    plist = plist,
			    beta = as.vector(sfix),
                            bvec = unlist(sran),
			    b = sran,
			    X = array(0, c(N, fLen),
			      list(NULL, fn)),
			    Z = array(0, c(N, rLen),
                              list(NULL, rNam)),
			    fmap = fmap,
			    rmap = rmap,
                            rmapRel = rmapRel,
                            bmap = bmap,
                            level = Q,
                            N = N,
                            Q = Q,
                            naPat = naPat,
			    .parameters = c("bvec", "beta"),
                            finiteDiffGrad = finiteDiffGrad)

  lapply(names(nlList), function(x, y, env) assign(x, y[[x]], envir = env),
         nlList, env = nlEnv)

  modelExpression <- ~{
    pars <- getParsNlme(plist, fmap, rmapRel, bmap, groups, beta, bvec, b, level, N)
    res <- eval(model, data.frame(data, pars))
    if (!length(grad <- attr(res, "gradient"))) {
      grad <- finiteDiffGrad(model, data, pars)
    }
    for (nm in names(plist)) {
      gradnm <- grad[, nm]
      if (is.logical(f <- plist[[nm]]$fixed)) {
        if (f) {
          X[, fmap[[nm]]] <- gradnm
        }
      } else {
        X[, fmap[[nm]]] <- gradnm * f
      }
      for(i in 1:Q) {
        if (is.logical(r <- plist[[nm]]$random[[i]])) {
          if (r) {
            Z[, rmap[[i]][[nm]]] <- gradnm
          }
        } else {
          if (data.class(rmap[[i]][[nm]]) != "list") {
            Z[, rmap[[i]][[nm]]] <- gradnm * r
          } else {
            for(j in seq_along(rmap[[i]][[nm]])) {
              if (is.logical(rr <- r[[j]])) {
                Z[, rmap[[i]][[nm]][[j]]] <- gradnm
              } else {
                Z[, rmap[[i]][[nm]][[j]]] <- gradnm * rr
              }
            }
          }
        }
      }
    }
    result <- c(Z[naPat, ], X[naPat, ], res[naPat])
    result[is.na(result)] <- 0
    result
  }

  modelResid <- ~eval(model, data.frame(data,
      getParsNlme(plist, fmap, rmapRel, bmap, groups, beta, bvec, b, level, N)))[naPat]
  ww <- eval(modelExpression[[2]], envir = nlEnv)
  w <- ww[NReal * pLen + (1:NReal)]
  ZX <- array(ww[1:(NReal*pLen)], c(NReal, pLen),
              list(row.names(dataMixShrunk), c(rNam, fn)))
  w <- w + as.vector(ZX[, rLen + (1:fLen), drop = FALSE] %*% sfix)
  if (!is.null(start$random)) {
    startRan <- 0
    for(i in 1:Q) {
      w <- w + as.vector((ZX[, startRan + 1:ncols[i], drop = FALSE] *
                  t(sran[[i]])[as.character(grpShrunk[, Q-i+1]),,drop = FALSE]) %*%
                         rep(1, ncols[i]))
      startRan <- startRan + ncols[i]
    }
  }
  ## creating the condensed linear model
  attr(nlmeSt, "conLin") <-
    list(Xy = array(c(ZX, w), c(NReal, sum(ncols)),
	     list(row.names(dataMixShrunk), c(colnames(ZX),
					deparse(model[[2]])))),
	 dims = Dims, logLik = 0)

  ## additional attributes of nlmeSt
  attr(nlmeSt, "resp") <- yShrunk
  attr(nlmeSt, "model") <- modelResid
  attr(nlmeSt, "local") <- nlEnv
  attr(nlmeSt, "NReal") <- NReal
  ## initialization
  nlmeSt <- Initialize(nlmeSt, dataMixShrunk, grpShrunk,
                       control = controlvals)
  parMap <- attr(nlmeSt, "pmap")

  if (length(coef(nlmeSt)) == length(coef(nlmeSt$reStruct)) &&
      !needUpdate(nlmeSt))  {	# can do one decomposition
    ## need to save conLin for calculating updating between steps
    oldConLin <- attr(nlmeSt, "conLin")
    decomp <- TRUE
  } else decomp <- FALSE

  numIter <- 0				# number of iterations
  pnlsSettings <- c(controlvals$pnlsMaxIter, controlvals$minScale,
                    controlvals$pnlsTol, 0, 0, 0)
  nlModel <- nonlinModel(modelExpression, nlEnv)
  repeat {                              ## alternating algorithm
    numIter <- numIter + 1
    ## LME step
    if (needUpdate(nlmeSt)) {             # updating varying weights
      nlmeSt <- update(nlmeSt, dataMixShrunk)
    }
    if (decomp) {
      attr(nlmeSt, "conLin") <- MEdecomp(oldConLin)
    }
    oldPars <- coef(nlmeSt)
    if (controlvals$opt == "nlminb") {
        optRes <- nlminb(c(coef(nlmeSt)),
                         function(nlmePars) -logLik(nlmeSt, nlmePars),
                         control = list(trace = controlvals$msVerbose,
                         iter.max = controlvals$msMaxIter))
        aConv <- coef(nlmeSt) <- optRes$par
        convIter <- optRes$iterations
    } else {
        aNlm <- nlm(f = function(nlmePars) -logLik(nlmeSt, nlmePars),
                    p = c(coef(nlmeSt)), hessian = TRUE,
                    print.level = controlvals$msVerbose,
                    gradtol = if(numIter == 1) controlvals$msTol
                    else 100*.Machine$double.eps,
                    iterlim = if(numIter < 10) 10 else controlvals$msMaxIter,
                    check.analyticals = FALSE)
        aConv <- coef(nlmeSt) <- aNlm$estimate
        convIter <- aNlm$iterations
    }
    nlmeFit <- attr(nlmeSt, "lmeFit") <- MEestimate(nlmeSt, grpShrunk)
    if (verbose) {
      cat("\n**Iteration", numIter)
      cat("\n")
      cat("LME step: Loglik:", format(nlmeFit$logLik),
          ", nlm iterations:", convIter, "\n")
      print(nlmeSt)
    }

    ## PNLS step
    if (is.null(correlation)) {
      cF <- 1.0
      cD <- 1
    } else {
      cF <- corFactor(nlmeSt$corStruct)
      cD <- Dim(nlmeSt$corStruct)
    }
    if (is.null(weights)) {
      vW <- 1.0
    } else {
      vW <- varWeights(nlmeSt$varStruct)
    }
    work <- .C(fit_nlme,
	       thetaPNLS = as.double(c(as.vector(unlist(sran)), sfix)),
               pdFactor = as.double(pdFactor(nlmeSt$reStruct)),
               as.integer(unlist(rev(grpShrunk))),
	       as.integer(unlist(Dims)),
               as.integer(attr(nlmeSt$reStruct, "settings"))[-(1:3)],
	       as.double(cF),
	       as.double(vW),
               as.integer(unlist(cD)),
	       settings = as.double(pnlsSettings),
	       additional = double(NReal * (pLen + 1)),
	       as.integer(!is.null(correlation)),
	       as.integer(!is.null(weights)),
               nlModel,
	       NAOK = TRUE)
    if (work$settings[4] == 1) {
##      convResult <- 2
      if (controlvals$returnObject) {
        warning("Step halving factor reduced below minimum in PNLS step")
      } else {
        stop("Step halving factor reduced below minimum in PNLS step")
      }
    }
    # dim(work$pdFactor) <- dim(pdMatrix(nlmeSt$reStruct[[1]]))
    # matrix(nlmeSt$reStruct[[1]]) <- crossprod(work$pdFactor)
    # fix from Setzer.Woodrow@epamail.epa.gov for nested grouping factors
    pdFacStart <- 1
    for (i in seq_along(nlmeSt$reStruct)) {
      tmppdFactor <- work$pdFactor[pdFacStart:
                                   (pdFacStart -1 +
                                    prod(dim(pdMatrix(nlmeSt$reStruct[[i]]))))]
      dim(tmppdFactor) <- dim(pdMatrix(nlmeSt$reStruct[[i]]))
      matrix(nlmeSt$reStruct[[i]]) <- crossprod(tmppdFactor)
      pdFacStart <- pdFacStart + prod(dim(pdMatrix(nlmeSt$reStruct[[i]])))
    }
    oldPars <- c(sfix, oldPars)
    for(i in 1:Q) sran[[i]][] <- work$thetaPNLS[(bmap[i]+1):bmap[i+1]]
    sfix[] <- work$thetaPNLS[nPars + 1 - (fLen:1)]
    if (verbose) {
      cat("\nPNLS step: RSS = ", format(work$set[6]), "\n fixed effects:")
      for (i in 1:fLen) cat(format(signif(sfix[i]))," ")
      cat("\n iterations:",work$set[5],"\n")
    }
    aConv <- coef(nlmeSt) # added by SDR 04/19/2002
    aConv <- c(sfix, aConv)
    w[] <- work$additional[(NReal * pLen) + 1:NReal]
    ZX[] <- work$additional[1:(NReal * pLen)]
    w <- w + as.vector(ZX[, rLen + (1:fLen), drop = FALSE] %*% sfix)
    startRan <- 0
    for(i in 1:Q) {
      w <- w + as.vector((ZX[, startRan + 1:ncols[i], drop = FALSE] *
                 t(sran[[i]])[as.character(grpShrunk[, Q-i+1]),,drop = FALSE]) %*%
                         rep(1, ncols[i]))
      startRan <- startRan + ncols[i]
    }
    if (decomp) {
      oldConLin$Xy[] <- c(ZX, w)
      oldConLin$logLik <- 0
    } else {
      attr(nlmeSt, "conLin")$Xy[] <- c(ZX, w)
      attr(nlmeSt, "conLin")$logLik <- 0
    }

    conv <- abs((oldPars - aConv)/
                ifelse(abs(aConv) < controlvals$tolerance, 1, aConv))
    aConv <- c(max(conv[1:fLen]))
    names(aConv) <- "fixed"
    conv <- conv[-(1:fLen)]
    for(i in names(nlmeSt)) {
      if (any(parMap[,i])) {
	aConv <- c(aConv, max(conv[parMap[,i]]))
	names(aConv)[length(aConv)] <- i
      }
    }

    if (verbose) {
      cat("\nConvergence:\n")
      print(aConv)
    }

    if ((max(aConv) <= controlvals$tolerance) ||
        (aConv["fixed"] <= controlvals$tolerance && convIter == 1)) {
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

  ## wrapping up
  if (decomp) {
    nlmeFit <- MEestimate(nlmeSt, grpShrunk, oldConLin)
  } else {
    nlmeFit <- MEestimate(nlmeSt, grpShrunk)
  }
  ## degrees of freedom for fixed effects tests
  fixDF <- getFixDF(ZX[, rLen + (1:fLen), drop = FALSE],
                    grpShrunk, attr(nlmeSt, "conLin")$dims$ngrps, fixAssign)

  attr(fixDF, "varFixFact") <- nlmeFit$sigma * nlmeFit$varFix
  varFix <- crossprod(nlmeFit$sigma * nlmeFit$varFix)
  dimnames(varFix) <- list(fn, fn)
  ##
  ## fitted.values and residuals (in original order)
  ##
  if (decomp) {
    Resid <- resid(nlmeSt, level = 0:Q, oldConLin)[revOrderShrunk, ]
  } else {
    Resid <- resid(nlmeSt, level = 0:Q)[revOrderShrunk, ]
  }
  Fitted <- yShrunk[revOrderShrunk] - Resid
  rownames(Resid) <- rownames(Fitted) <- origOrderShrunk
  grpShrunk <- grpShrunk[revOrderShrunk, , drop = FALSE]
  attr(Resid, "std") <- nlmeFit$sigma/(varWeights(nlmeSt)[revOrderShrunk])
  ## inverting back reStruct
  nlmeSt$reStruct <- solve(nlmeSt$reStruct)
  ## saving part of dims
  dims <- attr(nlmeSt, "conLin")$dims[c("N", "Q", "qvec", "ngrps", "ncol")]
  ## getting the approximate var-cov of the parameters
  if (controlvals$apVar) {
    apVar <- lmeApVar(nlmeSt, nlmeFit$sigma,
		      .relStep = controlvals[[".relStep"]],
                      minAbsPar = controlvals[["minAbsParApVar"]],
     		      natural = controlvals[["natural"]])
  } else {
    apVar <- "Approximate variance-covariance matrix not available"
  }
  ## putting sran in the right format
  sran <- lapply(sran, t)
  ## getting rid of condensed linear model, fit, and other attributes
  oClass <- class(nlmeSt)
  attributes(nlmeSt) <- attributes(nlmeSt)[c("names", "class", "pmap")]
  class(nlmeSt) <- oClass
  ##
  ## creating the  nlme object
  ##
  estOut <- list(modelStruct = nlmeSt,
		 dims = dims,
                 contrasts = contr,
		 coefficients = list(fixed = sfix, random = rev(sran)),
		 varFix = varFix,
		 sigma = nlmeFit$sigma,
		 apVar = apVar,
		 logLik = nlmeFit$logLik,
		 numIter = numIter,
		 groups = grpShrunk,
		 call = Call,
		 method = method,
		 fitted = Fitted,
		 residuals = Resid,
		 plist = plist,
                 map = list(fmap=fmap,rmap=rmap,rmapRel=rmapRel,bmap=bmap),
                 fixDF = fixDF)
  if (inherits(data, "groupedData")) {
    ## saving labels and units for plots
    attr(estOut, "units") <- attr(data, "units")
    attr(estOut, "labels") <- attr(data, "labels")
  }
  class(estOut) <- c("nlme","lme")
  estOut
}

###
### function used to calculate the parameters from
### the fixed and random effects
###

getParsNlme <-
  function(plist, fmap, rmapRel, bmap, groups, beta, bvec, b, level, N)
{
  pars <- array(0, c(N, length(plist)), list(NULL, names(plist)))
  for (nm in names(plist)) {
    if (is.logical(f <- plist[[nm]]$fixed)) {
      if (f) {
        pars[, nm] <- beta[fmap[[nm]]]
      }
    } else {
      pars[, nm] <- f %*% beta[fmap[[nm]]]
    }
    if (level > 0) {
      Q <- length(groups)
      for(i in (Q - level + 1):Q) {
        b[[i]][] <- bvec[(bmap[i] + 1):bmap[i+1]]
        if (is.logical(r <- plist[[nm]]$random[[i]])) {
          if (r) {
            pars[, nm] <- pars[, nm] + b[[i]][rmapRel[[i]][[nm]], groups[[i]]]
          }
        } else {
          if (data.class(r) != "list") {
            pars[,nm] <- pars[,nm] +
              (r * t(b[[i]])[groups[[i]], rmapRel[[i]][[nm]], drop = FALSE]) %*%
                rep(1, ncol(r))
          } else {
            for(j in seq_along(rmapRel[[i]][[nm]])) {
              if (is.logical(rr <- r[[j]])) {
                pars[, nm] <- pars[, nm] +
                  b[[i]][rmapRel[[i]][[nm]][[j]], groups[[i]]]
              } else {
                pars[,nm] <- pars[,nm] +
                  (rr * t(b[[i]])[groups[[i]], rmapRel[[i]][[nm]][[j]],
                                  drop = FALSE]) %*% rep(1, ncol(rr))
              }
            }
          }
        }
      }
    }
  }
  pars
}

###
###  Methods for standard generics
###

formula.nlme <- function(x, ...) eval(x$call[["model"]])

predict.nlme <-
  function(object, newdata, level = Q, asList = FALSE, na.action = na.fail,
	   naPattern = NULL, ...)
{
  ##
  ## method for predict() designed for objects inheriting from class nlme
  ##
  Q <- object$dims$Q
  if (missing(newdata)) {		# will return fitted values
    val <- fitted(object, level, asList)
    if (length(level) == 1) return(val)
    return(data.frame(object[["groups"]][,level[level != 0], drop = FALSE],
		      predict = val))
  }
  maxQ <- max(level)
  nlev <- length(level)
  newdata <- as.data.frame(newdata)
  mCall <- object$call
  if (maxQ > 0) {			# predictions with random effects
    whichQ <- Q - (maxQ-1):0
    reSt <- object$modelStruct$reStruct[whichQ]
    nlmeSt <- nlmeStruct(reStruct = reSt)
    groups <- getGroupsFormula(reSt)
    if (any(is.na(match(all.vars(groups), names(newdata))))) {
      ## groups cannot be evaluated in newdata
      stop("Cannot evaluate groups for desired levels on \"newdata\"")
    }
  } else {
    reSt <- NULL
  }

  mfArgs <- list(formula = asOneFormula(formula(object),
                   mCall$fixed, formula(reSt), naPattern,
                   omit = c(names(object$plist), "pi",
                     deparse(getResponseFormula(object)[[2]]))),
                 data = newdata, na.action = na.action)
  mfArgs$drop.unused.levels <- TRUE
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
      grps <- grps[whichRows, drop = TRUE]
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
    if (match(0, level, nomatch = 0)) {
      naGrps <- cbind(FALSE, naGrps)
    }
    naGrps <- as.matrix(naGrps)[ord, , drop = FALSE]
    grps <- grps[ord, , drop = FALSE]
    dataMix <- dataMix[ord, ,drop = FALSE]
    revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order
  }
  ## making sure factor levels are the same as in contrasts
  contr <- object$contrasts
  for(i in names(dataMix)) {
    if (inherits(dataMix[,i], "factor") && !is.null(contr[[i]])) {
      levs <- levels(dataMix[,i])
      levsC <- dimnames(contr[[i]])[[1]]
      if (any(wch <- is.na(match(levs, levsC)))) {
        stop(paste("Levels", paste(levs[wch], collapse = ","),
                   "not allowed for", i))
      }
      attr(dataMix[,i], "contrasts") <- contr[[i]][levs, , drop = FALSE]
#      if (length(levs) < length(levsC)) {
#        if (inherits(dataMix[,i], "ordered")) {
#          dataMix[,i] <- ordered(as.character(dataMix[,i]), levels = levsC)
#        } else {
#          dataMix[,i] <- factor(as.character(dataMix[,i]), levels = levsC)
#        }
#      }
    }
  }

  N <- nrow(dataMix)
  ##
  ## evaluating the naPattern expression, if any
  ##
  if (is.null(naPattern)) naPat <- rep(TRUE, N)
  else naPat <- as.logical(eval(asOneSidedFormula(naPattern)[[2]], dataMix))

  ##
  ## Getting  the plist for the new data frame
  ##
  ##
  plist <- object$plist
  fixed <- eval(object$call$fixed)
  if (!is.list(fixed)) {
    fixed <- list(fixed)
  }
  val <- NULL
  for(i in seq_along(fixed)) {
    if (is.name(fixed[[i]][[2]])) {
      val <- c(val, list(fixed[[i]]))
    } else {
      ## multiple parameters on left hand side
      val <- c(val, eval(parse(text = paste("list(",
           paste(paste(all.vars(fixed[[i]][[2]]), deparse(fixed[[i]][[3]]),
                       sep = "~"), collapse=","),")"))))
    }
  }
  fixed <- val
  fnames <- unlist(lapply(fixed, function(el) deparse(el[[2]])))
  names(fixed) <- fnames
  fix <- fixef(object)
  fn <- names(fix)
  for(nm in fnames) {
    if (!is.logical(plist[[nm]]$fixed)) {
      plist[[nm]]$fixed <- model.matrix(asOneSidedFormula(fixed[[nm]][[3]]),
                 model.frame(asOneSidedFormula(fixed[[nm]][[3]]), dataMix))
    }
  }

  if (maxQ > 0) {
    grpsRev <- lapply(rev(grps), as.character)
    ranForm <- formula(reSt)[whichQ]
    namGrp <- names(ranForm)
    rnames <- lapply(ranForm, function(el)
                     unlist(lapply(el, function(el1) deparse(el1[[2]]))))
    for(i in 1:length(ranForm)) {
      names(ranForm[[i]]) <- rnames[[i]]
    }
    ran <- ranef(object)
    if(is.data.frame(ran)) {
      ran <- list(ran)
    } else {
      ran <- rev(ran)
    }
    rn <- lapply(ran[whichQ], names)
    ran <- lapply(ran, t)
    ranVec <- unlist(ran)
    for(nm in names(plist)) {
      for(i in namGrp) {
        if (!is.logical(plist[[nm]]$random[[i]])) {
          wch <- (1:length(rnames[[i]]))[!is.na(match(rnames[[i]], nm))]
          if (length(wch) == 1) {         # only one formula for nm
            plist[[nm]]$random[[i]] <-
              model.matrix(asOneSidedFormula(ranForm[[i]][[nm]][[3]]),
                 model.frame(asOneSidedFormula(ranForm[[i]][[nm]][[3]]),
                             dataMix))
          } else {                        # multiple formulae
            plist[[nm]]$random[[i]] <- lapply(ranForm[[i]][wch],
                           function(el, data) {
                             if (el[[3]] == "1") {
                               TRUE
                             } else {
                               val <- model.matrix(asOneSidedFormula(el[[3]]),
                                       model.frame(asOneSidedFormula(el[[3]]),
                                                   data))
                               val

                             }
                           }, data = dataMix)
          }
        }
      }
    }
  } else {
    namGrp <- ""
    grpsRev <- ranVec <- ran <- NULL
  }
  val <- vector("list", nlev)
  names(val) <- c("fixed", rev(namGrp))[level + 1]
  modForm <- getCovariateFormula(object)[[2]]
  for(i in 1:nlev) {
    val[[i]] <- eval(modForm, data.frame(dataMix,
                  getParsNlme(plist, object$map$fmap, object$map$rmapRel,
                          object$map$bmap, grpsRev, fix, ranVec, ran,
                          level[i], N)))[naPat]
  }
  val <- as.data.frame(val)

  if (maxQ > 0) {
    val <- val[revOrder, , drop = FALSE]
    if (any(naGrps)) {
      val[naGrps] <- NA
    }
  }
  ## putting back in original order
  if (maxQ > 1) {                      # making groups unique
    for(i in 2:maxQ) {
      oGrps[, i] <- paste(as.character(oGrps[,i-1]), as.character(oGrps[,i]),
                          sep = "/")
    }
  }
  if (length(level) == 1) {
    val <- val[,1]
    if (level > 0) {
      grps <- as.character(oGrps[, level])
      if (asList) {
        val <- split(val, ordered(grps, levels = unique(grps)))
      } else {
        names(val) <- grps
      }
    }
    lab <- "Predicted values"
    if (!is.null(aux <- attr(object, "units")$y)) {
      lab <- paste(lab, aux)
    }
    attr(val, "label") <- lab
  } else {
    val <- data.frame(oGrps, predict = val)
  }
  val
}

# based on R's update.default
update.nlme <-
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

#update.nlme <-
#  function(object, model, data, fixed, random, groups, start, correlation,
#           weights, subset, method, na.action, naPattern, control,
#           verbose, ...)
#{
#  thisCall <- as.list(match.call())[-(1:2)]
#  if (!is.null(thisCall$start) && is.numeric(start)) {
#    thisCall$start <- list(fixed = start)
#  }
#  if (!is.null(nextCall <- object$origCall) &&
#      (is.null(thisCall$fixed) && !is.null(thisCall$random))) {
#    nextCall <- as.list(nextCall)[-1]
#  } else {
#    nextCall <- as.list(object$call)[-1]
#    if (is.null(thisCall$fixed)) {        # no changes in fixef model
#      if (is.null(thisCall$start)) {
#        thisCall$start <- list(fixed = fixef(object))
#      } else {
#        if (is.null(thisCall$start$fixed)) {
#          thisCall$start$fixed <- fixef(object)
#        }
#      }
#    }
#    if (!is.null(thisCall$start$random)) {  # making start random NULL
#      thisCall$start$random <- NULL
#    }
#    if (is.null(thisCall$random) && is.null(thisCall$subset)) {
#      ## no changes in ranef model and no subsetting
#      thisCall$random <- object$modelStruct$reStruct
#    }
#  }
#  if (!is.null(thisCall$model)) {
#    thisCall$model <- update(formula(object), model)
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
#  do.call("nlme", nextCall)
#}

###*### nlmeStruct - a model structure for nlme fits

nlmeStruct <-
  ## constructor for nlmeStruct objects
  function(reStruct, corStruct = NULL, varStruct = NULL)#, resp = NULL,
           #model = NULL, local = NULL, N = NULL, naPat = NULL)
{

  val <- list(reStruct = reStruct, corStruct = corStruct,
              varStruct = varStruct)
  val <- val[!sapply(val, is.null)]	# removing NULL components
  attr(val, "settings") <- attr(val$reStruct, "settings")
#  attr(val, "resp") <- resp
#  attr(val, "model") <- model
#  attr(val, "local") <- local
#  attr(val, "N") <- N
#  attr(val, "naPat") <- naPat
  class(val) <- c("nlmeStruct", "lmeStruct", "modelStruct")
  val
}

##*## nlmeStruct methods for standard generics

fitted.nlmeStruct <-
  function(object, level = Q,  conLin = attr(object, "conLin"), ...)
{
  Q <- attr(object, "conLin")$dims[["Q"]]
  attr(object, "resp") - resid(object, level, conLin)
}


residuals.nlmeStruct <-
  function(object, level = Q, conLin = attr(object, "conLin"), ...)
{
  Q <- conLin$dims[["Q"]]
  loc <- attr(object, "local")
  oLev <- get("level", envir = loc)
  on.exit(assign("level", oLev, envir = loc))
  dn <- c("fixed", rev(names(object$reStruct)))[level + 1]
  val <- array(0, c(attr(object, "NReal"), length(level)),
       list(dimnames(conLin$Xy)[[1]], dn))
  for(i in 1:length(level)) {
    assign("level", level[i], envir = loc, immediate = TRUE)
    val[, i] <- c(eval(attr(object, "model")[[2]], envir = loc))
  }
  val
}

nlmeControl <-
  ## Set control values for iterations within nlme
  function(maxIter = 50, pnlsMaxIter = 7, msMaxIter = 50,
	   minScale = 0.001, tolerance = 1e-5, niterEM = 25,
           pnlsTol = 0.001, msTol = 0.000001, msScale = lmeScale,
           returnObject = FALSE, msVerbose = FALSE, gradHess = TRUE,
           apVar = TRUE, .relStep = (.Machine$double.eps)^(1/3),
           nlmStepMax = 100.0, minAbsParApVar = 0.05,
	   opt = c("nlminb", "nlm"), natural = TRUE)
{
  list(maxIter = maxIter, pnlsMaxIter = pnlsMaxIter, msMaxIter = msMaxIter,
       minScale = minScale, tolerance = tolerance, niterEM = niterEM,
       pnlsTol = pnlsTol, msTol = msTol, msScale = msScale,
       returnObject = returnObject, msVerbose = msVerbose,
       gradHess = gradHess, apVar = apVar, .relStep = .relStep,
       nlmStepMax = nlmStepMax, minAbsParApVar = minAbsParApVar,
       opt = match.arg(opt), natural = natural)
}

nonlinModel <- function( modelExpression, env,
                        paramNames = get(".parameters", envir = env)) {
  modelExpression <- modelExpression[[2]]
  thisEnv <- environment()
  offset <- 0
  ind <- vector("list", length(paramNames))
  names(ind) <- paramNames
  for( i in paramNames ) {
    ind[[ i ]] <- offset + seq_along(get(i, envir = env))
    offset <- offset + length( get(i, envir = env) )
  }
  modelValue <- eval(modelExpression, env)
  on.exit(remove(i, offset, paramNames))
  function (newPars) {
    if(!missing(newPars)) {
      for(i in names(ind))
	assign( i, unname(newPars[ ind[[i]] ]), envir = env)
      assign("modelValue", eval(modelExpression, env), envir = thisEnv)
    }
    modelValue
  }
}
