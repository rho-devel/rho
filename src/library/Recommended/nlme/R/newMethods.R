###      Methods for generics from newGenerics.q for some standard classes
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>

##*## Methods for some of the generics in newGenerics.q for standard classes

Dim.default <- function(object, ...) dim(object)

getCovariate.data.frame <-
  function(object, form = formula(object), data)
{
  ## Return the primary covariate
  if (!(inherits(form, "formula"))) {
    stop("'form' must be a formula")
  }
  aux <- getCovariateFormula(form)
  if (length(all.vars(aux)) > 0) {
    eval(aux[[2]], object)
  } else {
    rep(1, dim(object)[1])
  }
}

getData.nls <-
  function(object)
{
  mCall <- object$call
  ## avoid partial matches here.
  data <- eval(if("data" %in% names(object)) object$data else mCall$data)
  if (is.null(data)) return(data)
  naAct <- object[["na.action"]]
  if (!is.null(naAct)) {
      ## guessing here: known values (omit, exclude) work.
      data <- if (inherits(naAct, "omit")) data[-naAct, ]
      else if (inherits(naAct, "exclude")) data
      else eval(mCall$na.action)(data)
  }
  subset <- mCall$subset
  if (!is.null(subset)) {
    subset <- eval(asOneSidedFormula(subset)[[2]], data)
    data <- data[subset, ]
  }
  data
}

getGroups.data.frame <-
  ## Return the groups associated with object according to form for level
  function(object, form = formula(object), level, data, sep = "/")
{
  if (!missing(data)) {
    stop( "data argument to \"data.frame\" method for 'getGroups' does not make sense" )
  }
  if (inherits(form, "formula")) {
    grpForm <- getGroupsFormula(form, asList = TRUE, sep = sep)
    if (is.null(grpForm)) {
      ## will use right hand side of form as the group formula
      grpForm <- splitFormula(asOneSidedFormula(form[[length(form)]]),
                              sep = sep)
      names(grpForm) <-
        unlist( lapply( grpForm, function(el) deparse( el[[ length(el) ]] ) ) )
    }
    if (any(unlist(lapply(grpForm,
#                          function(el) length(el[[length(el)]]))) != 1)) {
                          function(el) length(all.vars(el)))) != 1)) {
      stop("invalid formula for groups")
    }
    form <- grpForm
  } else if (data.class(form) == "list") {
    if (!all(unlist(lapply(form, function(el) inherits(el, "formula"))))) {
      stop("'form' must have all components as formulas")
    }
  } else {
    stop("'form' can only be a formula, or a list of formulas")
  }
  vlist <- lapply(form,
                  function(x, dat, N) {
                    val <- eval(x[[length(x)]], dat)
                    if (length(val) == 1) {             # repeat groups
                      return(as.factor(rep(val, N)))
                    } else {
                      return(as.factor(val)[drop = TRUE])
                    }
                  }, dat = object, N = nrow(object))
  if (length(vlist) == 1) return(vlist[[1]]) # ignore level - only one choice
  ## make the list into a data frame with appropriate names
  value <- do.call("data.frame", vlist)
  if (missing(level)) return(value)
  if (is.character(level)) {
    nlevel <- match(level, names(vlist))
    if (any(aux <- is.na(nlevel))) {
        stop(gettextf("level of %s does not match formula ",
                      level[aux], sQuote(deparse(form))), domain = NA)
    }
  } else {
    nlevel <- as.numeric(level)
    if (any(aux <- is.na(match(nlevel, 1:ncol(value))))) {
        stop(gettextf("level of %s does not match formula ",
                      level[aux], sQuote(deparse(form))), domain = NA)
    }
  }
  if (length(nlevel) > 1)  return(value[, nlevel]) # multicolumn selection
  if (nlevel == 1)         return(value[, 1])     # no need to do more work
  value <- value[, 1:nlevel]
  val <- as.factor(do.call("paste", c(lapply(as.list(value),
					     as.character), sep = sep)))
  if (inherits(value[, 1], "ordered")) {
    value <- value[do.call("order", value),]
    aux <- unique(do.call("paste", c(lapply(as.list(value),
					    as.character), sep = sep)))
    return(ordered(val, aux))
  } else {
    return(ordered(val, unique(as.character(val))))
  }
}

getResponse.data.frame <-
  function(object, form = formula(object))
{
  ## Return the response, the evaluation of the left hand side of a formula
  ## on object
  if (!(inherits(form, "formula") && (length(form) == 3))) {
    stop("'form' must be a two-sided formula")
  }
  eval(form[[2]], object)
}

getGroupsFormula.default <-
  ## Return the formula(s) for the groups associated with object.
  ## The result is a one-sided formula unless asList is TRUE in which case
  ## it is a list of formulas, one for each level.
  function(object, asList = FALSE, sep = "/")
{
  form <- formula(object)
  if (!inherits(form, "formula")){
    stop("'form' argument must be a formula")
  }
  form <- form[[length(form)]]
  if (!((length(form) == 3) && (form[[1]] == as.name("|")))) {
    ## no conditioning expression
    return(NULL)
  }
  ## val <- list( asOneSidedFormula( form[[ 3 ]] ) )
  val <- splitFormula(asOneSidedFormula(form[[3]]), sep = sep)
  names(val) <- unlist(lapply(val, function(el) deparse(el[[2]])))
#  if (!missing(level)) {
#    if (length(level) == 1) {
#      return(val[[level]])
#    } else {
#      val <- val[level]
#    }
#  }
  if (asList) as.list(val)
  else as.formula(eval(parse(text = paste("~",  paste(names(val),
                               collapse = sep)))))
}

Names.formula <-
  function(object, data = list(), exclude = c("pi", "."), ...)
{
  if (!is.list(data)) { return(NULL) }  # no data to evaluate variable names
  allV <- all.vars(object)
  allV <- allV[is.na(match(allV, exclude))]

  if (length(allV) == 0) {
    if (attr(terms(object), "intercept")) { return("(Intercept)") }
    return(NULL)
  }

  if (any(is.na(match(allV, names(data))))) { return(NULL) }
  dimnames(model.matrix(object, model.frame(object, data)))[[2]]
}

Names.listForm <-
  function(object, data = list(), exclude = c("pi", "."), ...)
{
  pnames <- as.character(unlist(lapply(object, "[[", 2)))
  nams <- lapply(object, function(el, data, exclude) {
    Names(getCovariateFormula(el), data, exclude)
    }, data = data, exclude = exclude)
  if (is.null(nams[[1]])) return(NULL)
  val <- c()
  for(i in seq_along(object)) {
    if ((length(nams[[i]]) == 1) && (nams[[i]] == "(Intercept)")) {
      val <- c(val, pnames[i])
    } else {
      val <- c(val, paste(pnames[i], nams[[i]], sep = "."))
    }
  }
  val
}

needUpdate.default <-
  function(object)
{
  val <- attr(object, "needUpdate")
  if (is.null(val) || !val) FALSE
  else TRUE
}

##--- needs Trellis/Lattice :
pairs.compareFits <-
  function(x, subset, key = TRUE, ...)
{
  object <- x
  if(!missing(subset)) {
    object <- object[subset,,]
  }
  dims <- dim(object)
  if(dims[3] == 1) {
    stop("at least two coefficients are needed")
  }
  dn <- dimnames(object)
  coefs <- array(c(object), c(dims[1]*dims[2], dims[3]),
		 list(rep(dn[[1]], dims[2]), dn[[3]]))
  if(dims[3] > 2) {			# splom
    tt <- list(coefs = coefs,
	       grp = ordered(rep(dn[[2]], rep(dims[1], dims[2])),
		   levels  = dn[[2]]))
    args <- list(~ coefs,
                 data = tt,
                 groups = tt$grp,
                 panel = function(x, y, subscripts, groups, ...)
             {
                 x <- as.numeric(x)
                 y <- as.numeric(y)
                 panel.superpose(x, y, subscripts, groups)
                 aux <- groups[subscripts]
                 aux <- aux == unique(aux)[1]
                 lsegments(x[aux], y[aux], x[!aux], y[!aux],
                           lty = 2, lwd = 0.5)
             })
} else {
    tt <- list(x = coefs[,1], y = coefs[,2],
	       grp = ordered(rep(dn[[2]], rep(dims[1], dims[2])),
               levels = dn[[2]]))
    args <- list(y ~ x,
                 data = tt,
                 groups = tt$grp,
                 panel = function(x, y, subscripts, groups, ...)
             {
                 x <- as.numeric(x)
                 y <- as.numeric(y)
                 panel.grid()
                 panel.superpose(x, y, subscripts, groups)
                 aux <- groups[subscripts]
                 aux <- aux == unique(aux)[1]
                 lsegments(x[aux], y[aux], x[!aux], y[!aux],
                           lty = 2, lwd = 0.5)
             }, xlab = dn[[3]][1], ylab = dn[[3]][2])
}
  dots <- list(...)
  args[names(dots)] <- dots
  if(is.logical(key)) {
      if(key && length(unique(tt$grp)) > 1) {
          args[["key"]] <-
              list(points = Rows(trellis.par.get("superpose.symbol"), 1:2),
                   text = list(levels = levels(tt$grp)), columns = 2)
      }
  } else {
      args[["key"]] <- key
  }
  if(dims[3] > 2) do.call("splom", args) else do.call("xyplot", args)
}

##--- needs Trellis/Lattice :
plot.nls <-
  function(x, form = resid(., type = "pearson") ~ fitted(.), abline,
	   id = NULL, idLabels = NULL, idResType = c("pearson", "normalized"),
           grid, ...)
  ## Diagnostic plots based on residuals and/or fitted values
{
  object <- x
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
      alist <- c(list(as.name("data.frame")), alist)
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
  if (inherits(data, "groupedData")) {	# save labels and units, if present
    ff <- formula(data)
    rF <- deparse(getResponseFormula(ff)[[2]])
    cF <- c_deparse(getCovariateFormula(ff)[[2]])
    lbs <- attr(data, "labels")
    unts <- attr(data, "units")
    if (!is.null(lbs$x)) cL <- paste(lbs$x, unts$x) else cF <- NULL
    if (!is.null(lbs$y)) rL <- paste(lbs$y, unts$y) else rF <- NULL
  } else {
    rF <- cF <- NULL
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
    stop("covariate must be numeric")
  }
  argForm <- ~ .x
  argData <- data.frame(.x = .x, check.names = FALSE)
  if (is.null(xlab <- attr(.x, "label"))) {
    xlab <- deparse(covF[[2]])
    if (!is.null(cF) && (xlab == cF)) xlab <- cL  #### BUG!!!!
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
      auxGr <- all.vars(gr[[i]])
      for(j in auxGr) {
        argData[[j]] <- eval(as.name(j), data)
      }
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
    idResType <- match.arg(idResType)
    id <-
      switch(mode(id),
	     numeric = {
	       if ((id <= 0) || (id >= 1)) {
		 stop("'id' must be between 0 and 1")
	       }
	       as.logical(abs(resid(object, type = idResType)) >
                          -qnorm(id / 2))
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

  ## defining abline, if needed
  if (missing(abline)) {
    if (missing(form)) {		# r ~ f
      abline <- c(0, 0)
    } else {
      abline <- NULL
    }
  }

  #assign("id", id , where = 1)
  #assign("idLabels", idLabels, where = 1)
  #assign("abl", abline, where = 1)
  assign("abl", abline)

  ## defining the type of plot
  if (length(argForm) == 3) {
    if (is.numeric(.y)) {		# xyplot
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
		      if (!is.null(abl)) {
			if (length(abl) == 2) panel.abline(a = abl, ...) else panel.abline(h = abl, ...)
		      }
		    }))
      }
    } else {				# assume factor or character
      plotFun <- "bwplot"
      if (is.null(args$panel)) {
        args <- c(args,
                  panel = list(function(x, y, ...)
		    {
		      if (grid) panel.grid()
		      panel.bwplot(x, y, ...)
		      if (!is.null(abl)) {
			panel.abline(v = abl[1], ...)
		      }
		    }))
      }
    }
  } else {
    plotFun <- "histogram"
    if (is.null(args$panel)) {
      args <- c(args,
                panel = list(function(x, ...)
		  {
		    if (grid) panel.grid()
		    panel.histogram(x, ...)
		    if (!is.null(abl)) {
		      panel.abline(v = abl[1], ...)
		    }
		  }))
    }
  }

  ## defining grid
  if (missing(grid)) {
    if (plotFun == "xyplot") grid <- TRUE
    else grid <- FALSE
  }
  # assign("grid", grid, where = 1)
  do.call(plotFun, as.list(args))
}

#pruneLevels.factor <- function(object) object[drop = TRUE]

##*## Plot method for ACF objects
plot.ACF <-
  function(x, alpha = 0, xlab = "Lag", ylab = "Autocorrelation",
           grid = FALSE, ...)
{
  object <- x
  ylim <- range(object$ACF)
  if (alpha) {
    assign("stdv",  qnorm(1-alpha/2)/sqrt(attr(object,"n.used")))
    stMax <- max(stdv)
    ylim <- c(min(c(-stMax, ylim[1])), max(c(ylim[2], stMax)))
  }
  assign("alpha", as.logical(alpha))
  assign("grid", grid)
  xyplot(ACF ~ lag, object, ylim = ylim,
         panel = function(x, y, ...) {
           x <- as.numeric(x)
           y <- as.numeric(y)
           if (grid) panel.grid()
           panel.xyplot(x, y, type = "h")
           panel.abline(0, 0)
           if (alpha) {
             llines(x, stdv, lty = 2)
             llines(x, -stdv, lty = 2)
           }
         }, xlab = xlab, ylab = ylab, ...)
}

plot.augPred <-
  function(x, key = TRUE, grid = FALSE, ...)
{
  labels <- list(xlab = paste(attr(x, "labels")$x, attr(x, "units")$x),
		 ylab = paste(attr(x, "labels")$y, attr(x, "units")$y))
  labels <- labels[unlist(lapply(labels, function(el) length(el) > 0))]
  args <- c(list(attr(x, "formula"),
		 groups = as.name(".type"),
		 data = x,
		 strip = function(...) strip.default(..., style = 1),
		 panel = if (length(levels(x[[".type"]])) == 2) {
                   ## single prediction level
                   function(x, y, subscripts, groups, ...) {
                     if (grid) panel.grid()
                     orig <- groups[subscripts] == "original"
                     panel.xyplot(x[orig], y[orig], ...)
                     panel.xyplot(x[!orig], y[!orig], ..., type = "l")
                   }
                 } else {             # multiple prediction levels
                   function(x, y, subscripts, groups, ...) {
                     if (grid) panel.grid()
                     orig <- groups[subscripts] == "original"
                     panel.xyplot(x[orig], y[orig], ...)
                     panel.superpose(x[!orig], y[!orig], subscripts[!orig],
                                     groups, ..., type = "l")
                   }
                 }), labels)
  ## perhaps include key argument allowing logical values
  dots <- list(...)
  args[names(dots)] <- dots
  if (is.logical(key) && key) {
    levs <- levels(x[[".type"]])
    if ((lLev <- length(levs)) > 2) {	# more than one levels
      lLev <- lLev - 1
      levs <- levs[1:lLev]
      aux <- !is.na(match(substring(levs, 1, 8), "predict."))
      if (sum(aux) > 0) {
	levs[aux] <- substring(levs[aux], 9)
      }
      args[["key"]] <-
	list(lines = c(Rows(trellis.par.get("superpose.line"), 1:lLev),
		       list(size = rep(3, lLev))),
	     text = list(levels = levs), columns = min(6, lLev))
    }
  } else {
    args[["key"]] <- key
  }
  assign("grid", grid)
  do.call("xyplot", args)
}

plot.compareFits <-
  function(x, subset, key = TRUE, mark = NULL, ...)
{
  object <- x
  if(!missing(subset)) {
    object <- object[subset,,]
  }
  dims <- dim(object)
  dn <- dimnames(object)
  assign("mark", rep(mark, rep(dims[1] * dims[2], dims[3])))
  tt <- data.frame(group = ordered(rep(dn[[1]], dims[2] * dims[3]),
		       levels = dn[[1]]),
		   coefs = as.vector(object),
		   what = ordered(rep(dn[[3]],
		       rep(dims[1] * dims[2], dims[3])), levels = dn[[3]]),
		   grp = ordered(rep(rep(dn[[2]], rep(dims[1], dims[2])),
		       dims[3]), levels = dn[[2]]))
  args <- list(group ~ coefs | what,
	       data = tt,
	       scales = list(x=list(relation="free")),
	       strip = function(...) strip.default(..., style = 1),
	       xlab = "",
	       groups = tt$grp,
	       panel = function(x, y, subscripts, groups, ...)
	       {
                 x <- as.numeric(x)
                 y <- as.numeric(y)
		 dot.line <- trellis.par.get("dot.line")
		 panel.abline(h = y, lwd = dot.line$lwd,
			      lty = dot.line$lty, col = dot.line$col)
		 if(!is.null(mark)) {
		   panel.abline(v = mark[subscripts][1], lty = 2)
		 }
		 panel.superpose(x, y, subscripts, groups)
	       })
  dots <- list(...)
  args[names(dots)] <- dots
  if(is.logical(key)) {
    if(key && length(unique(tt$grp)) > 1) {
      args[["key"]] <-
	list(points = Rows(trellis.par.get("superpose.symbol"), 1:2),
	     text = list(levels = levels(tt$grp)), columns = 2)
    }
  } else {
    args[["key"]] <- key
  }
  do.call("dotplot", args)
}

plot.Variogram <-
  function(x, smooth, showModel, sigma = NULL, span = 0.6,
           xlab = "Distance", ylab = "Semivariogram", type = "p", ylim,
           grid = FALSE, ...)
{
  object <- x
  trlLin <- trellis.par.get("superpose.line")
  coll <- attr(object, "collapse")
  modVrg <- attr(object, "modelVariog")
  lineT <- 1
  if (!is.na(match(type, c("l","o","b")))) {
    lineT <- lineT + 1
  }
  if (missing(showModel)) {
    showModel <- !is.null(modVrg)
  }
  if (showModel) {
    if (is.null(modVrg)) {
      stop("no model variogram available with 'showModel = TRUE'")
    }
    assign("ltyM", trlLin$lty[lineT])
    assign("colM", trlLin$col[lineT])
    assign("modVrg", modVrg)
    lineT <- lineT + 1
  }
  if (missing(smooth)) {
    smooth <- !showModel
  }
  if (smooth) {
    assign("ltyS", trlLin$lty[lineT])
    assign("colS", trlLin$col[lineT])
  }
  assign("smooth", smooth)
  assign("showModel", showModel)
  assign("span", span)
  assign("type", type)
  assign("sigma", sigma)
  assign("grid", grid)
  if (missing(ylim)) {
    ylim <- c(0, max(object$variog))
  }
  xyplot(variog ~ dist, object, ylim = ylim,
         panel = function(x, y, ...) {
           if (grid) panel.grid()
           panel.xyplot(x, y, type = type, ...)
           if (showModel) {
             panel.xyplot(modVrg$dist, modVrg$variog, type = "l",
                          col = colM, lty = ltyM, ...)
           }
           if (smooth) {
             panel.loess(x, y, span = span, col = colS, lty = ltyS, ...)
           }
           if (!is.null(sigma)) {
             panel.abline(c(sigma, 0), lty = 2)
           }
         }, xlab = xlab, ylab = ylab, ...)
}

print.compareFits <-
  function(x, ...)
{			# Will need to be changed for S4!
  print(unclass(x), ...)
  invisible(x)
}

print.correlation <-
  ## Print only the lower triangle of a correlation matrix
  function(x, title = " Correlation:", rdig = 3, ...)
{
  p <- dim(x)[2]
  if (p > 1) {
    cat(title, "\n")
    ll <- lower.tri(x)
    x[ll] <- format(round(x[ll], digits = rdig))
    x[!ll] <- ""
    if (!is.null(colnames(x))) {
      colnames(x) <- abbreviate(colnames(x), minlength = rdig + 3)
    }
    print(x[-1,  - p, drop = FALSE], ..., quote = FALSE)
  }
  invisible(x)
}

##if(R.version$major <= 1 && R.version$minor < 3) ## not in R 1.3 and later
##--- needs Trellis/Lattice :
qqnorm.nls <-
  function(y, form = ~ resid(., type = "p"), abline = NULL,
           id = NULL, idLabels = NULL, grid = FALSE, ...)
  ## normal probability plots for residuals
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
  if (is.null(labs) || ((labs != "Standardized residuals") &&
                        (labs != "Normalized residuals") &&
                        (substring(labs, 1, 9) != "Residuals"))) {
    stop("only residuals allowed")
  }
  if (is.null(args$xlab)) args$xlab <- labs
  if (is.null(args$ylab)) args$ylab <- "Quantiles of standard normal"
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
                 stop("'id' must be between 0 and 1")
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
#  assign("id", if (is.null(id)) NULL else as.logical(as.character(id)),
#         frame = 1)
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
                 data = substitute(data)),
                 args)
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
  do.call("xyplot", args)
}

Variogram.default <-
  function(object, distance, ...)
{
  ld <- length(distance)
  lo <- length(object)
  if (ld != round(lo*(lo-1)/2)) {
    stop("'distance' and 'object' have incompatible lengths")
  }
  val <- outer(object, object, function(x,y) ((x - y)^2)/2)
  val <- val[lower.tri(val)]
  val <- data.frame(variog = val, dist = as.numeric(distance))
  class(val) <- c("Variogram", "data.frame")
  val
}

## local function for complete deparsing
c_deparse <- function(...) paste(deparse(..., width.cutoff=500), collapse="")

## Local Variables:
## mode:S
## End:
