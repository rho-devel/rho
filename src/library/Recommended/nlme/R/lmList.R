###                  Create a list of lm objects
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

lmList <-
  ## a list of lm objects from a formula or a groupedData object
  function(object, data, level, subset, na.action = na.fail, pool = TRUE)
  UseMethod("lmList")

lmList.groupedData <-
  function(object, data, level, subset, na.action = na.fail, pool = TRUE)
{
  ### object will provide the formula, the data, and the groups
  form <- formula(object)
  args <- as.list(match.call())[-1]
  args[["object"]] <- as.vector(eval(parse(text = paste(deparse(form[[2]]),
                                             "~", deparse(form[[3]][[2]])))))
  if (!missing(data)) {
    args[["data"]] <- substitute(object)
  } else {
    args <- as.list(c(args, list(data = substitute(object))))
  }
  do.call("lmList.formula", args)
}

lmList.formula <-
  function(object, data, level, subset, na.action = na.fail, pool = TRUE)
{
  Call <- match.call()
  if (!missing(subset)) {
    data <-
      data[eval(asOneSidedFormula(Call[["subset"]])[[2]], data),, drop = FALSE]
  }
  if (!inherits(data, "data.frame")) data <- as.data.frame(data)
  data <- na.action(data)
  if (is.null(grpForm <- getGroupsFormula(object))) {
    if (inherits(data, "groupedData")) {
      if (missing(level))
        level <- length(getGroupsFormula(data, asList = TRUE))
      else if (length(level) > 1) {
	stop("Multiple levels not allowed")
      }
      groups <- getGroups(data, level = level)[drop = TRUE]
      grpForm <- getGroupsFormula(data)
      Call$object <-
        as.vector(eval(parse(text=paste(deparse(Call$object),
                               deparse(grpForm[[2]]), sep = "|"))))

    } else {
      stop ("data must be a groupedData object if groups argument is missing")
    }
  } else {
    if (missing(level))
      level <- length(getGroupsFormula(object, asList = TRUE))
    else if (length(level) > 1) {
      stop("Multiple levels not allowed")
    }
    groups <- getGroups(data, form = grpForm, level = level)[drop = TRUE]
    object <- eval(parse(text=paste(deparse(getResponseFormula(object)[[2]]),
                       c_deparse(getCovariateFormula(object)[[2]]), sep = "~")))
  }
  val <- lapply(split(data, groups),
		function(dat, form, na.action)
		{
                  lm(formula = form, data = dat, na.action = na.action)
		},
		form = object, na.action = na.action)
  if (inherits(data, "groupedData")) {
    ## saving labels and units for plots
    attr(val, "units") <- attr(data, "units")
    attr(val, "labels") <- attr(data, "labels")
  }

  attr(val, "dims") <- list(N = nrow(data), M = length(val))
  attr(val,"call") <- Call
  attr(val, "groupsForm") <- grpForm
  attr(val,"groups") <- ordered(groups, levels = names(val))
  attr(val, "origOrder") <- match(unique(as.character(groups)), names(val))
  attr(val, "level") <- level
  attr(val, "pool") <- pool
  class(val) <- "lmList"
  val
}

###*# Methods for standard generics

augPred.lmList <-
  function(object, primary = NULL, minimum = min(primary),
	   maximum = max(primary), length.out = 51, ...)
{
  data <- eval(attr(object, "call")[["data"]])
  if (!inherits(data, "data.frame")) {
    stop(paste("Data in", substitute(object),
               "call must evaluate to a data frame"))
  }
  if(is.null(primary)) {
    if (!inherits(data, "groupedData")) {
      stop(paste(sys.call()[[1]],
      "without \"primary\" can only be used with fits of groupedData objects"))
    }
    primary <- getCovariate(data)
    prName <- deparse(getCovariateFormula(data)[[2]])
  } else{
    primary <- asOneSidedFormula(primary)[[2]]
    prName <- deparse(primary)
    primary <- eval(primary, data)
  }
  newprimary <- seq(from = minimum, to = maximum, length.out = length.out)
  groups <- getGroups(object)
  grName <- deparse(getGroupsFormula(object)[[2]])
  ugroups <- unique(groups)
  value <- data.frame(rep(newprimary, length(ugroups)),
		      rep(ugroups, rep(length(newprimary), length(ugroups))))
  names(value) <- c(prName, grName)
  ## recovering other variables in data that may be needed for predictions
  ## varying variables will be replaced by their means
  summData <- gsummary(data, groups = groups)
  if (any(toAdd <- is.na(match(names(summData), names(value))))) {
    summData <- summData[, toAdd, drop = FALSE]
  }
  value[, names(summData)] <- summData[value[, 2], ]
  pred <- c(predict(object, value, asList = FALSE))
  newvals <- cbind(value[, 1:2], pred)
  names(newvals)[3] <- respName <-
    deparse(getResponseFormula(object)[[2]])
  orig <- data.frame(primary, groups, getResponse(object))
  names(orig) <- names(newvals)
  value <- rbind(orig, newvals)
  attributes(value[, 2]) <- attributes(groups)
  value[, ".type"] <- ordered(c(rep("original", nrow(data)),
				rep("predicted", nrow(newvals))),
			      levels = c("predicted", "original"))
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

coef.lmList <-
  ## Extract the coefficients and form a  data.frame if possible
  function(object, augFrame = FALSE, data = NULL,
           which = NULL, FUN = mean, omitGroupingFactor = TRUE, ...)
{
  coefs <- lapply(object, coef)
  non.null <- !unlist(lapply(coefs, is.null))
  if (sum(non.null) > 0) {
    template <- coefs[non.null][[1]]
    if (is.numeric(template)) {
      co <- matrix(template,
		      ncol = length(template),
		      nrow = length(coefs),
		      byrow = TRUE,
		      dimnames = list(names(object), names(template)))
      for (i in names(object)) {
	co[i,] <- if (is.null(coefs[[i]])) { NA } else coefs[[i]]
      }
      coefs <- as.data.frame(co)
      effectNames <- names(coefs)
      if(augFrame) {
        if (is.null(data)) {
          data <- getData(object)
        }
        data <- as.data.frame(data)
        if (is.null(which)) {
          which <- 1:ncol(data)
        }
	data <- data[, which, drop = FALSE]
	## eliminating columns with same names as effects
	data <- data[, is.na(match(names(data), effectNames)), drop = FALSE]
        data <- gsummary(data, FUN = FUN, groups = getGroups(object))
	if (omitGroupingFactor) {
	  data <- data[, is.na(match(names(data),
                   names(getGroupsFormula(object, asList = TRUE)))),
                       drop = FALSE]
	}
	if (length(data) > 0) {
	  coefs <- cbind(coefs, data[row.names(coefs),,drop = FALSE])
	}
      }
      attr(coefs, "level") <- attr(object, "level")
      attr(coefs, "label") <- "Coefficients"
      attr(coefs, "effectNames") <- effectNames
      attr(coefs, "standardized") <- FALSE
      attr(coefs, "grpNames") <- deparse(getGroupsFormula(object)[[2]])
      class(coefs) <- c("coef.lmList", "ranef.lmList", class(coefs))
    }
  }
  coefs
}

fitted.lmList <-
  function(object, subset = NULL, asList = FALSE, ...)
{
  if(!is.null(subset)) {
    if(is.character(subset)) {
      if (any(is.na(match(subset, names(object))))) {
        stop("Non-existent groups requested in \"subset\".")
      }
    } else {
      if (is.integer(subset)) {
        if (any(is.na(match(subset, 1:length(object))))) {
          stop("Non-existent groups requested in \"subset\".")
        }
      } else {
        stop("Subset can only be character or integer")
      }
    }
    oclass <- class(object)
    oatt <- attr(object, "call")
    object <- object[subset]
    attr(object, "call") <- oatt
    class(object) <- oclass
  }
  val <- lapply(object, fitted)
  if(!asList) {				#convert to array
    ngrps <- table(getGroups(object))[names(object)]
    if(any(aux <- sapply(object, is.null))) {
      for(i in names(ngrps[aux])) {
	val[[i]] <- rep(NA, ngrps[i])
      }
    }
    val <- val[attr(object, "origOrder")] # putting in original order
    namVal <- names(val)
    val <- unlist(val)
    names(val) <- rep(namVal, ngrps)
  }
  lab <- "Fitted values"
  if (!is.null(aux <- attr(object, "units")$y)) {
    lab <- paste(lab, aux)
  }
  attr(val, "label") <- lab
  val
}

fixef.lmList <-
  function(object, ...)
{
  coeff <- coef(object)
  if(is.matrix(coeff) || is.data.frame(coeff)) {
    return(apply(coeff, 2, mean, na.rm = TRUE))
  }
  NULL
}

formula.lmList <-
  function(x, ...) eval(attr(x, "call")[["object"]])

getData.lmList <-
  function(object)
{
  mCall <- attr(object, "call")
  data <- eval(mCall$data)
  if (is.null(data)) return(data)
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

getGroups.lmList <-  function(object, form, level, data, sep)
  attr(object, "groups")

getGroupsFormula.lmList <-
  function(object, asList = FALSE, sep)
{
  val <- attr(object, "groupsForm")
  getGroupsFormula(eval(parse(text=paste("~1",c_deparse(val[[2]]),sep="|"))),
		   asList = asList)
}

getResponse.lmList <-
  function(object, form)
{
  fitted(object) + resid(object)
}

intervals.lmList <-
  function(object, level = 0.95, pool = attr(object, "pool"), ...)
{
  smry <- summary(object, pool = pool)
  coeff <- coef(smry)
  out <- coeff[ , 1:3 , ]
  dn <- dimnames(out)
  dimnames(out) <-
    if(is.null(dn))
      list(NULL, c("lower", "est.", "upper"))
    else {
      dn[[2]] <- c("lower", "est.", "upper")
      dn
    }
  mult <- sqrt(qf(level, 1, smry$df.residual))
  out[ , "est.", ] <- coeff[ , "Estimate",  ]
  out[ , "lower", ] <- out[ , "est.", ] - mult * coeff[ , "Std. Error", ]
  out[ , "upper", ] <- out[ , "est.", ] + mult * coeff[ , "Std. Error", ]
  attr(out, "groupsName") <- deparse(attr(object, "groupsForm")[[2]])
  class(out) <- "intervals.lmList"
  out
}

logLik.lmList <-
  function(object, REML = FALSE, pool = attr(object, "pool"), ...)
{
  if(any(unlist(lapply(object, is.null)))) {
    stop("Log-likelihood not available with NULL fits.")
  }
  if(pool) {
    aux <- apply(sapply(object, function(el) {
                   res <- resid(el)
		   p <- el$rank
		   n <- length(res)
		   if (is.null(w <- el$weights)) w <- rep(1, n)
		   else {
		     excl <- w == 0
		     if (any(excl)) {
		       res <- res[!excl]
		       n <- length(res)
		       w <- w[!excl]
		     }
		   }
		   c(n, sum(w * res^2), p, sum(log(w)),
		     sum(log(abs(diag(el$qr$qr)[1:p]))))
		 }), 1, sum)
    N <- aux[1] - REML * aux[3]
    val <- (aux[4] - N * (log(2 * pi) + 1 - log(N) + log(aux[2])))/2 -
      REML * aux[5]
    attr(val, "nall") <- aux[1]
    attr(val, "nobs") <- N
    attr(val, "df") <- aux[3] + 1
  } else {
    aux <- lapply(object, logLik, REML)
    val <- sum(unlist(aux))
    attr(val, "nobs") <- sum(sapply(aux, function(x) attr(x, "nobs")))
    attr(val, "df") <- sum(sapply(aux, function(x) attr(x, "df")))
  }
  class(val) <- "logLik"
  val
}

pairs.lmList <-
  function(x, form = ~ coef(.), label, id = NULL, idLabels = NULL,
	   grid = FALSE, ...)
{
  object <- x
  ## scatter plot matrix plots, generally based on coef or random.effects
  if (!inherits(form, "formula")) {
    stop("\"Form\" must be a formula")
  }
  if (length(form) != 2) {
    stop("\"Form\" must be a one-sided formula")
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
	stop(paste(allV[naV], "not found in data"))
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
    stop("Covariate must be a data frame")
  }
  if (!is.null(effNams <- attr(.x, "effectNames"))) {
    .x <- .x[, effNams, drop = FALSE]
  }
  ## eliminating constant effects
  isFixed <- unlist(lapply(.x, function(el) length(unique(el)) == 1))
  .x <- .x[, !isFixed, drop = FALSE]
  nc <- ncol(.x)
  if (nc == 1) {
    stop("Cannot do pairs of just one variable")
  }
  if (!missing(label)) {
    names(.x) <- labels
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
      auxData[[deparse(gr[[i]][[2]])]] <- eval(gr[[i]][[2]], data)
    }
    if (length(argForm) == 2)
      argForm <- eval(parse(text = paste("~ .x |", deparse(grpsF[[2]]))))
    else argForm <- eval(parse(text = paste(".y ~ .x |", deparse(grpsF[[2]]))))
  }

  ## id and idLabels - need not be present
  if (!is.null(id)) {			# identify points in plot
    N <- attr(object, "dims")$N
    id <-
      switch(mode(id),
	     numeric = {
	       if ((id <= 0) || (id >= 1)) {
		 stop("Id must be between 0 and 1")
	       }
	       aux <- as.matrix(na.omit(ranef(object)))
	       auxV <- t(chol(var(aux)))
	       aux <- as.logical(apply((solve(auxV, t(aux)))^2, 2, sum) >
				 qchisq(1 - id, dim(aux)[2]))
	       aux
	     },
	     call = eval(asOneSidedFormula(id)[[2]], data),
	     stop("\"Id\" can only be a formula or numeric.")
	     )
    if (length(id) == N) {
      ## id as a formula evaluated in data
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
	  stop("\"IdLabels\" of incorrect length")
	}
	idLabels <- as.character(idLabels)
      } else {
	stop("\"IdLabels\" can only be a formula or a vector")
      }
    }
    if (length(idLabels) == N) {
      ## idLabels as a formula evaluated in data
      auxData[[".Lid"]] <- idLabels
    }
  }

  if (length(auxData)) {		# need collapsing
    auxData <- gsummary(data.frame(auxData),
			groups = getGroups(object))
    auxData <- auxData[row.names(.x), , drop = FALSE]
    if (!is.null(auxData[[".g"]])) {
      argData[[".g"]] <- auxData[[".g"]]
    }

    if (!is.null(auxData[[".id"]])) {
      id <- auxData[[".id"]]
    }

    if (!is.null(auxData[[".Lid"]])) {
      idLabels <- auxData[[".Lid"]]
    }
    wchDat <- is.na(match(names(auxData), c(".id", ".idLabels")))
    if (any(wchDat)) {
      argData <- cbind(argData, auxData[, wchDat, drop = FALSE])

    }
  }

  assign("id", as.logical(as.character(id)) )
  assign("idLabels", as.character(idLabels))
  assign("grid", grid)

  ## adding to args list
  args <- c(list(argForm, data = argData), args)
##   if (is.null(args$strip)) {
##     args$strip <- function(...) strip.default(..., style = 1)
##   }
  if (is.null(args$cex)) args$cex <- par("cex")
  if (is.null(args$adj)) args$adj <- par("adj")

  ## defining the type of plot
  if (length(argForm) == 3) {		# xyplot
    plotFun <- "xyplot"
    args <- c(args,
	      panel = list(function(x, y, subscripts, ...)
		  {
                    x <- as.numeric(x)
                    y <- as.numeric(y)
                    dots <- list(...)
		    if (grid) panel.grid()
		    panel.xyplot(x, y)
                    if (any(ids <- id[subscripts])) {
                        ltext(x[ids], y[ids], idLabels[subscripts][ids],
                             cex = dots$cex, adj = dots$adj)
		    }
		  }))
  } else {				# splom
    plotFun <- "splom"
    args <- c(args,
	      panel = list(function(x, y, subscripts, ...)
		  {
                    x <- as.numeric(x)
                    y <- as.numeric(y)
                    dots <- list(...)
		    if (grid) panel.grid()
		    panel.xyplot(x, y)
                    if (any(ids <- id[subscripts])) {
                        ltext(x[ids], y[ids], idLabels[subscripts][ids],
                              cex = dots$cex, adj = dots$adj)
		    }
		  }))
  }
  do.call(plotFun, args)
}

plot.intervals.lmList <-
  function(x, ...)
{
  object <- x
  dims <- dim(object)
  dn <- dimnames(object)
  ## changed definition of what to ordered to preserve order of parameters
  tt <- data.frame(group = ordered(rep(dn[[1]], dims[2] * dims[3]),
                   levels = dn[[1]]),
		   intervals = as.vector(object),
		   what = ordered(rep(dn[[3]],
                   rep(dims[1] * dims[2], dims[3])), levels = dn[[3]]))
  strip <- list(...)[["strip"]]
  if ( is.null( strip ) ) {
    strip <- function(...) strip.default(..., style = 1)
  }
  xlab <- list(...)[["xlab"]]
  if ( is.null( xlab ) ) {
    xlab <- ""
  }

  ylab <- list(...)[["ylab"]]
  if ( is.null( ylab ) ) {
    ylab <- attr(object, "groupsName")
  }
  dotplot(group ~ intervals | what,
	  data = tt,
	  scales = list(x="free"),
	  strip = strip,
	  xlab = xlab, ylab = ylab,
	  panel = function(x, y, pch = dot.symbol$pch,
	      col = dot.symbol$col, cex = dot.symbol$cex,
	      font = dot.symbol$font, ...)
	  {
            x <- as.numeric(x)
            y <- as.numeric(y)
	    ok <- !is.na(x) & !is.na(y)
	    yy <- y[ok]
	    xx <- x[ok]
	    dot.symbol <- trellis.par.get("dot.symbol")
	    dot.line <- trellis.par.get("dot.line")
	    panel.abline(h = yy, lwd = dot.line$lwd, lty = dot.line$lty, col =
                         dot.line$col)
	    lpoints(xx, yy, pch = "|", col = col, cex = cex, font = font, ...)
	    lower <- tapply(xx, yy, min)
	    upper <- tapply(xx, yy, max)
	    nams <- as.numeric(names(lower))
	    lsegments(lower, nams, upper, nams, col = 1, lty = 1, lwd =
                      if (dot.line$lwd) {
                        dot.line$lwd
                      } else {
                        2
                      })
	  }, ...)
}

plot.ranef.lmList <-
  function(x, form = NULL, grid = TRUE, control, ...)
{
  fArgs <- as.list(match.call())[-1]
  do.call("plot.ranef.lme", fArgs)
}

plot.lmList <-
  function(x, form = resid(., type = "pool") ~ fitted(.), abline,
	   id = NULL, idLabels = NULL,  grid, ...)
  ## Diagnostic plots based on residuals and/or fitted values
{
  object <- x
  if (!inherits(form, "formula")) {
    stop("\"Form\" must be a formula")
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
	stop(paste(allV[naV], "not found in data"))
      }
    }
  } else data <- NULL

  if (inherits(data, "groupedData")) {	# save labels and units, if present
    ff <- formula(data)
    rF <- deparse(getResponseFormula(ff)[[2]])
    cF <- c_deparse(getCovariateFormula(ff)[[2]])
    lbs <- attr(data, "labels")
    unts <- attr(data, "units")
    if (!is.null(lbs$x)) cL <- paste(lbs$x, unts$x) else cF <- NULL
    if (!is.null(lbs$y)) rL <- paste(lbs$y, unts$y) else rF <- NULL
  } else {
    rF <- rC <- NULL
  }

  ## argument list
  dots <- list(...)
  if (length(dots) > 0) args <- dots
  else args <- list()
  ## appending object to data
  data <- as.list(c(as.list(data), . = list(object)))

  ## covariate - must always be present
  covF <- getCovariateFormula(form)
  .x <- eval(covF[[2]], data)
  if (!is.numeric(.x)) {
    stop("Covariate must be numeric")
  }
  argForm <- ~ .x
  argData <- as.data.frame(.x)
  if (is.null(xlab <- attr(.x, "label"))) {
    xlab <- c_deparse(covF[[2]])
    if (!is.null(cF) && (xlab == cF)) xlab <- cL
    else if (!is.null(rF) && (xlab == rF)) xlab <- rL
  }
  if (is.null(args$xlab)) args$xlab <- xlab

  ## response - need not be present
  respF <- getResponseFormula(form)
  if (!is.null(respF)) {
    .y <- eval(respF[[2]], data)
    if (is.null(ylab <- attr(.y, "label"))) {
      ylab <- deparse(respF[[2]])
      if (!is.null(cF) && (ylab == cF)) ylab <- cL
      else if (!is.null(rF) && (ylab == rF)) ylab <- rL
    }
    argForm <- .y ~ .x
    argData[, ".y"] <- .y
    if (is.null(args$ylab)) args$ylab <- ylab
  }

  ## groups - need not be present
  grpsF <- getGroupsFormula(form)
  if (!is.null(grpsF)) {
    gr <- splitFormula(grpsF, sep = "*")
    for(i in 1:length(gr)) {
      argData[[deparse(gr[[i]][[2]])]] <- eval(gr[[i]][[2]], data)
    }
    if (length(argForm) == 2)
      argForm <- eval(parse(text = paste("~ .x |", deparse(grpsF[[2]]))))
    else argForm <- eval(parse(text = paste(".y ~ .x |", deparse(grpsF[[2]]))))
  }
  ## adding to args list
  args <- c(list(argForm, data = argData), args)
##   if (is.null(args$strip)) {
##     args$strip <- function(...) strip.default(..., style = 1)
##   }
  if (is.null(args$cex)) args$cex <- par("cex")
  if (is.null(args$adj)) args$adj <- par("adj")

  if (!is.null(id)) {			# identify points in plot
    id <-
      switch(mode(id),
	     numeric = {
	       if ((id <= 0) || (id >= 1)) {
		 stop("Id must be between 0 and 1")
	       }
	       as.logical(abs(resid(object, type = "pooled")) > -qnorm(id / 2))
	     },
	     call = eval(asOneSidedFormula(id)[[2]], data),
	     stop("\"Id\" can only be a formula or numeric.")
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
	  stop("\"IdLabels\" of incorrect length")
	}
	idLabels <- as.character(idLabels)
      } else {
	stop("\"IdLabels\" can only be a formula or a vector")
      }
    }
  }

  ## defining abline, if needed
  if (missing(abline)) {
    if (missing(form)) {		# r ~ f
      abline <- c(0, 0)
    } else {
      abline <- NULL
    }
  }

  assign("id", id )
  assign("idLabels", idLabels)
  assign("abl", abline)

  ## defining the type of plot
  if (length(argForm) == 3) {
    if (is.numeric(.y)) {		# xyplot
      plotFun <- "xyplot"
      args <- c(args,
		panel = list(function(x, y, subscripts, ...)
		    {
                      x <- as.numeric(x)
                      y <- as.numeric(y)
                      dots <- list(...)
		      if (grid) panel.grid()
		      panel.xyplot(x, y)
                      if (any(ids <- id[subscripts])) {
                          ltext(x[ids], y[ids], idLabels[subscripts][ids],
                                cex = dots$cex, adj = dots$adj)
                      }
		      if (!is.null(abl)) {
			if (length(abl) == 2) panel.abline(a = abl, ...) else panel.abline(h = abl, ...)
		      }
		    }))
    } else {				# assume factor or character
      plotFun <- "bwplot"
      args <- c(args,
		panel = list(function(x, y, ...)
		    {
		      if (grid) panel.grid()
		      panel.bwplot(x, y)
		      if (!is.null(abl)) {
			panel.abline(v = abl[1], ...)
		      }
		    }))
    }
  } else {
    plotFun <- "histogram"
    args <- c(args,
	      panel = list(function(x, y, ...)
		  {
		    if (grid) panel.grid()
		    panel.histogram(x, y)
		    if (!is.null(abl)) {
		      panel.abline(v = abl[1], ...)
		    }
		  }))
  }
  ## defining grid
  if (missing(grid)) {
    if (plotFun == "xyplot") grid <- TRUE
    else grid <- FALSE
  }
  assign("grid", grid)

  do.call(plotFun, args)
}

predict.lmList <-
  function(object, newdata, subset = NULL, pool = attr(object, "pool"),
	   asList = FALSE, se.fit = FALSE, ...)
{
  if(missing(newdata)) {
    if (!se.fit) return(fitted(object, subset, asList))
    myData <- getData(object)
    grps <- getGroups(object)
    myData <- split(myData, grps)
    newdata <- NULL
    sameData <- FALSE
  } else {
    newdata <- as.data.frame(newdata)
    sameData <- TRUE
    ## checking if same newdata for all groups
    formGrps <- getGroupsFormula(object)
    if(all(match(all.vars(formGrps), names(newdata), 0))) {
      ## newdata contains groups definition
      grps <- getGroups(newdata, getGroupsFormula(object, asList = TRUE),
			level = attr(object, "level"))
      grps <- grps[drop = TRUE]
      subset <- as.character(unique(grps))
      if(any(is.na(match(subset, names(object))))) {
	stop("Non-existent group in \"newdata\".")
      }
      myData <- split(newdata, grps)
      newdata <- NULL
      sameData <- FALSE
    }
  }
  if(!is.null(subset)) {
    if(any(is.na(match(subset, names(object)))))
      stop("Non-existent group requested in \"subset\".")
    oclass <- class(object)
    ## fix for PR#13788
    oatt <- attributes(object)[c("call", "groupsForm", "pool")]
    object <- object[subset]
    attributes(object) <- c(attributes(object), oatt)
    class(object) <- oclass
    if(is.null(newdata))
      myData <- myData[subset]
  }
  nmGrps <- names(object)
  noNull <- !sapply(object, is.null)
  val <- vector("list", length(nmGrps))
  names(val) <- nmGrps
  if(!sameData) {
    if(!se.fit) {
      for(i in nmGrps[noNull]) {
        val[[i]] <- predict(object[[i]], myData[[i]])
      }
    } else {
      if(pool) {
	poolSD <- pooledSD(object)
      }
      for(i in nmGrps[noNull]) {
	aux <- predict(object[[i]], myData[[i]], se.fit = TRUE)
	if(pool) {
	  val[[i]] <- data.frame(fit = aux$fit,
				 se.fit = aux$se.fit*poolSD/aux$res)
	} else {
	  val[[i]] <- data.frame(fit = aux$fit, se.fit = aux$se.fit)
	}
      }
    }
  } else {
    if(pool) {
      poolSD <- pooledSD(object)
      val[noNull] <-
	lapply(object[noNull],
	       function(el, newdata, se.fit, poolSD) {
		 aux <- predict(el, newdata, se.fit = se.fit)
		 if(se.fit) {
		   data.frame(fit = aux$fit,
			      se.fit = aux$se.fit*poolSD/aux$res)
		 } else {
		   aux
		 }
	       }, newdata = newdata, se.fit = se.fit, poolSD = poolSD)
    } else {
      val[noNull] <-
	lapply(object[noNull],
	       function(el, newdata, se.fit) {
		 aux <- predict(el, newdata, se.fit = se.fit)
		 if(se.fit) {
		   data.frame(fit = aux$fit, se.fit = aux$se.fit)
		 } else {
		   aux
		 }
	       }, newdata = newdata, se.fit = se.fit)
    }
  }
  if(!asList) {				#convert to array
    if(is.null(newdata)) {
      ngrps <- table(grps)[names(object)]
    } else {
      ngrps <- rep(dim(newdata)[1], length(object))
      names(ngrps) <- names(object)
    }
    if(any(aux <- sapply(object, is.null))) {
      for(i in names(ngrps[aux])) {
	aux1 <- rep(NA, ngrps[i])
	if(se.fit) {
	  val[[i]] <- data.frame(fit = aux1, se.fit = aux1)
	} else {
	  val[[i]] <- aux1
	}
      }
    }
    if(se.fit) {
      val <- do.call("rbind", val)
      val[, as.character(getGroupsFormula(object)[[2]])] <-
	rep(names(ngrps), ngrps)
      val <- val[, c(3,1,2)]
      row.names(val) <- 1:nrow(val)
    } else {
      val <- unlist(val)
      names(val) <- rep(names(ngrps), ngrps)
      attr(val, "label") <- "Predicted values"
      if (!is.null(aux <- attr(object, "units")$y)) {
        attr(val, "label") <- paste(attr(val, "label"), aux)
      }
    }
  }
  val
}

print.intervals.lmList <-
  function(x, ...)
{
  ox <- x
  x <- unclass(x)
  attr(x, "groupsName") <- NULL
  print(x, ...)
  invisible(ox)
}

print.lmList <-
  function(x, pool = attr(x, "pool"), ...)
{
  mCall <- attr(x, "call")
  cat("Call:\n")
  form <- formula(x)
  cat("  Model:", deparse(getResponseFormula(form)[[2]]),
      "~", c_deparse(getCovariateFormula(form)[[2]]), "|",
      deparse(getGroupsFormula(x)[[2]]), "\n")
  if (!is.null(mCall$level)) {
    cat(" Level:", mCall$level, "\n")
  }
  cat("   Data:", deparse(mCall$data),"\n\n")
  cat("Coefficients:\n")
  invisible(print(coef(x)))
  if(pool) {
    cat("\n")
    poolSD <- pooledSD(x)
    dfRes <- attr(poolSD, "df")
    RSE <- c(poolSD)
    cat("Degrees of freedom: ", length(unlist(lapply(x, fitted))),
	" total; ", dfRes, " residual\n", sep = "")
    cat("Residual standard error:", format(RSE))
    cat("\n")
  }
  invisible(x)
}

print.summary.lmList <-
  function(x, ...)
{
  cat("Call:\n")
  form <- formula(x)
  cat("  Model:", deparse(getResponseFormula(form)[[2]]),
      "~", c_deparse(getCovariateFormula(form)[[2]]), "|",
      deparse(attr(x, "groupsForm")[[2]]), "\n")
  if (!is.null(x$call$level)) {
    cat(" Level:", x$call$level, "\n")
  }
  cat("   Data:", deparse(x$call$data),"\n\n")
  cat("Coefficients:\n")
  for(i in dimnames(coef(x))[[3]]) {
    cat("  ",i,"\n")
    print(coef(x)[,,i])
  }
  if(x$pool) {
    cat("\n")
    cat("Residual standard error:", format(x$RSE), "on",
	x$df.residual, "degrees of freedom\n")
  }
  cat("\n")
  invisible(x)
}

qqnorm.lmList <-
  function(y, form = ~ resid(., type = "pooled"), abline = NULL,
           id = NULL, idLabels = NULL, grid = FALSE, resType = "pool", ...)
  ## normal probability plots for residuals and random effects
{
  object <- y
  if (!inherits(form, "formula")) {
    stop("\"Form\" must be a formula")
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
	stop(paste(allV[naV], "not found in data"))
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
  if (inherits(.x, "ranef.lmList")) {      # random effects
    type <- "reff"
  } else {
    if (!is.null(labs) && ((labs == "Standardized residuals") ||
                           (substring(labs, 1, 9) == "Residuals"))) {
      type <- "res"                     # residuals
    } else {
      stop("Only residuals and random effects allowed")
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
                   stop("Id must be between 0 and 1")
                 }
                 as.logical(abs(resid(object, type=resType))
                            > -qnorm(id / 2))
               },
               call = eval(asOneSidedFormula(id)[[2]], data),
               stop("\"Id\" can only be a formula or numeric.")
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
            stop("\"IdLabels\" of incorrect length")
          }
          idLabels <- as.character(idLabels)
        } else {
          stop("\"IdLabels\" can only be a formula or a vector")
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
                          levels = names(fData)))
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
                   stop("Id must be between 0 and 1")
                 }
                 aux <- ranef(object, standard = TRUE)
                 as.logical(abs(c(unlist(aux))) > -qnorm(id / 2))
               },
               call = eval(asOneSidedFormula(id)[[2]], data),
               stop("\"Id\" can only be a formula or numeric.")
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
            stop("\"IdLabels\" of incorrect length")
          }
          idLabels <- as.character(idLabels)
        } else {
          stop("\"IdLabels\" can only be a formula or a vector")
        }
      }
      if (length(idLabels) == N) {
        ## idLabels as a formula evaluated in data
        auxData[[".Lid"]] <- idLabels
      }
    }

    if (length(auxData)) {		# need collapsing
      auxData <- gsummary(data.frame(auxData),
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
  assign("id", if (is.null(id)) NULL else as.logical(as.character(id)))
  assign("idLabels", as.character(idLabels))
  assign("grid", grid)
  assign("abl", abline)
  if (is.null(args$strip)) {
    args$strip <- function(...) strip.default(..., style = 1)
  }
  if (is.null(args$cex)) args$cex <- par("cex")
  if (is.null(args$adj)) args$adj <- par("adj")

  args <- c(list(eval(parse(text = dform)),
                 data = substitute(data),
                 panel = function(x, y, subscripts, ...){
                   x <- as.numeric(x)
                   y <- as.numeric(y)
                   dots <- list(...)
                   if (grid) panel.grid()
                   panel.xyplot(x, y, ...)
                   if (any(ids <- id[subscripts])) {
                       ltext(x[ids], y[ids], idLabels[subscripts][ids],
                             cex = dots$cex, adj = dots$adj)
                   }
                   if (!is.null(abl)) panel.abline(abl, ...)
                 }), args)
  if(type == "reff" && !std) {
    args[["scales"]] <- list(x = list(relation = "free"))
  }
  do.call("xyplot", args)
}

ranef.lmList <-
  ##  Extracts the random effects from an lmList object.
  ##  If aug.frame is true, the returned data frame is augmented with a
  ##  values from the original data object, if available.  The variables
  ##  in the original data are collapsed over the cluster variable by the
  ##  function fun.
  function(object, augFrame = FALSE, data = NULL,
           which = NULL, FUN = mean, standard = FALSE,
           omitGroupingFactor = TRUE, ...)
{
  val <- coef(object, augFrame, data, which, FUN, omitGroupingFactor)
  effNames <- attr(val, "effectNames")
  effs <- val[, effNames, drop = FALSE]
  effs <-
    as.data.frame(lapply(effs, function(el) el - mean(el, na.rm = TRUE)))

  if(standard) {
    stdEff <- unlist(lapply(effs, function(el) sqrt(var(el[!is.na(el)]))))
    effs <- as.data.frame(as.matrix(effs) %*% diag(1/stdEff))
    attr(val, "label") <- "Standardized random effects"
  } else {
    attr(val, "label") <- "Random effects"
  }
  val[, effNames] <- effs
  attr(val, "standardized") <- standard
  class(val) <- unique(c("ranef.lmList", class(val)[-1]))
  val
}

residuals.lmList <-
  function(object, type = c("response", "pearson", "pooled.pearson"),
	   subset = NULL, asList = FALSE, ...)
{
  type <- match.arg(type)
  if(type == "pooled.pearson") {
    poolSD <- pooledSD(object)
  }
  if(!is.null(subset)) {
    if(is.character(subset)) {
      if (any(is.na(match(subset, names(object))))) {
        stop("Non-existent groups requested in \"subset\".")
      }
    } else {
      if (is.integer(subset)) {
        if (any(is.na(match(subset, 1:length(object))))) {
          stop("Non-existent groups requested in \"subset\".")
        }
      } else {
        stop("Subset can only be character or integer")
      }
    }
    oclass <- class(object)
    oatt <- attr(object, "call")
    object <- object[subset]
    attr(object, "call") <- oatt
    class(object) <- oclass
  }
  val <-
    switch(type,
	   pooled.pearson = {
	     lapply(object, function(el, pSD) {
	       if(!is.null(el)) resid(el)/pSD
	       else NULL
	     }, pSD = poolSD)
	   },
	   pearson = lapply(object, function(el) {
	     if(!is.null(el)) {
	       aux <- resid(el)
	       aux/sqrt(sum(aux^2)/(length(aux) - length(coef(el))))
	     } else {
	       NULL
	     }
	   }),
	   response = lapply(object, function(el) {
	     if(!is.null(el)) resid(el)
	     else NULL
	   })
	   )
  if(!asList) {				#convert to array
    ngrps <- table(getGroups(object))[names(object)]
    if(any(aux <- sapply(object, is.null))) {
      for(i in names(ngrps[aux])) {
	val[[i]] <- rep(NA, ngrps[i])
      }
    }
    val <- val[attr(object, "origOrder")] # putting in original order
    namVal <- names(val)
    val <- unlist(val)
    names(val) <- rep(namVal, ngrps)
  }
  if (type == "response") {
    lab <- "Residuals"
    if (!is.null(aux <- attr(object, "units")$y)) {
      lab <- paste(lab, aux)
    }
  } else lab <- "Standardized residuals"
  attr(val, "label") <- lab
  val
}

summary.lmList <-
  function(object, pool = attr(object, "pool"), ...)
{
  to.3d.array <-
    ## Convert the list to a 3d array watching for null elements
    function(lst, template)
      {
	if (!is.matrix(template)) {
	  return(lst)
	}
	val <- aperm(array(unlist(lapply(lst, function(el, template)
 					 if(is.null(el)) { template }
					 else { el }, template = template)),
			   c(dim(template), length(lst)),
			   c(dimnames(template), list(names(lst)))),
		     c(3, 2, 1))
	val[unlist(lapply(lst, is.null)), , ] <- NA
	val
      }
  to.2d.array <-
    ## Convert the list to a 2d array watching for null elements
    function(lst, template)
      {
	if(is.null(template)) {
	  return(lst)
	}
	template <- as.vector(template)
	val <- t(array(unlist(lapply(lst, function(el, template)
				     if(is.null(el)) { template }
				     else { el }, template = template)),
		       c(length(template), length(lst)),
		       list(names(template), names(lst))))
	val[unlist(lapply(lst, is.null)), ] <- NA
	val
      }
  ## Create a summary by applying summary to each component of the list
  sum.lst <- lapply(object, function(el) if(is.null(el)) {NULL}
                                         else {summary(el)})
  nonNull <- !unlist(lapply(sum.lst, is.null))
  if(!any(nonNull)) {
    return(NULL)
  }
  template <- sum.lst[[match(TRUE, nonNull)]]
  val <- list()
  for (i in names(template)) {
    val[[i]] <- lapply(sum.lst, "[[", i)
    class(val[[i]]) <- "listof"
  }
  ## re-arrange the matrices into 3d arrays
  for(i in c("parameters", "cov.unscaled", "correlation", "coefficients")) {
    if(length(val[[i]])) {
      val[[i]] <- to.3d.array(val[[i]], template[[i]])
    }
  }
  ## re-arrange the vectors into 2d arrays
  for(i in c("df", "fstatistic")) {
    val[[i]] <- to.2d.array(val[[i]], template[[i]])
  }
  ## re-arrange the scalars into vectors
  for(i in c("sigma", "r.squared")) {
    ##    val[[i]] <- unlist(val[[i]]) - this deletes NULL components
    val[[i]] <- c(to.2d.array(val[[i]], template[[i]]))
  }
  ## select those attributes that do not vary with groups
  for(i in c("terms", "formula")) {
    val[[i]] <- template[[i]]
  }
  val[["call"]] <- attr(object, "call")
  if(inherits(object, "nlsList")) {
    names(val[["call"]]["model"]) <- "object"
  }
  val[["pool"]] <- pool
  if(pool) {
    poolSD <- pooledSD(object)
    dfRes <- attr(poolSD, "df")
    RSE <- c(poolSD)
    corRSE <- RSE/val$sigma
    if(inherits(object, "nlsList")) {
      pname <- "parameters"
    } else {
      pname <- "coefficients"
    }
    val[[pname]][,2,] <- val[[pname]][,2,] * corRSE
    val[[pname]][,3,] <- val[[pname]][,3,] / corRSE
    if(!inherits(object, "nlsList")) {
      val[[pname]][,4,] <- 2*(1-pt(abs(val[[pname]][,3,]), dfRes))
    }
    val[["df.residual"]] <- dfRes
    val[["RSE"]] <- RSE
  }
  attr(val, "groupsForm") <- attr(object, "groupsForm")
  class(val) <- "summary.lmList"
  val
}

# based on R's update.default
update.lmList <-
    function (object, formula., ..., evaluate = TRUE)
{
    call <- attr(object, "call")
    if (is.null(call))
	stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.))
	call$object <- update.formula(formula(object), formula.)
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

#update.lmList <-
#  function(object, formula, data, level, subset, na.action, pool, ...)
#{
#  thisCall <- as.list(match.call())[-(1:2)]
#  if (!missing(formula)) {
#    names(thisCall)[match(names(thisCall), "formula")] <- "object"
#  }
#  nextCall <- attr(object, "call")
#  nextCall[names(thisCall)] <- thisCall
#  if (!is.null(thisCall$object)) {
#    nextCall$object <- update(as.formula(nextCall$object), nextCall$object)
#  }
#  nextCall[[1]] <- as.name("lmList")
#  eval(nextCall, envir = parent.frame(1))
#}

### Local variables:
### mode: S
### End:
