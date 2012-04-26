###           groupedData - data frame with a grouping structure
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

groupedData <-
  ## Constructor for the groupedData class.  Takes a formula and a frame
  ## The formula must be of the form "response ~ primary | groups",
  ## "respose ~ primary ~ groups1/groups2/.../groups_k",
  ## or "response ~ (primary1 | groups1) / ... / (primary|groups_k)"
  ## where groups_i evaluates to a factor in frame.
  function(formula, data = sys.parent(1), order.groups = TRUE,
	   FUN = function(x) max(x, na.rm = TRUE), outer = NULL,
           inner = NULL, labels = NULL, units = NULL)
{
  if (!(inherits(formula, "formula") && length(formula) == 3)) {
    stop("first argument to groupedData must be a two-sided formula")
  }
  if (is.null(grpForm <- getGroupsFormula(formula, asList = TRUE))) {
    stop("Right hand side of first argument must be a conditional expression")
  }
  mCall <- match.call()
  mCall[[1]] <- as.name(ifelse(length(grpForm) == 1, "nfGroupedData",
                               "nmGroupedData"))
  eval(mCall, envir = parent.frame())
}

nfGroupedData <-
  ## Constructor for the nfGroupedData class.  Takes a formula and a frame
  ## The formula must be of the form "response ~ primary | groups"
  ## where groups evaluates to a factor in frame.
  function(formula, data = sys.parent(1), order.groups = TRUE,
	   FUN = function(x) max(x, na.rm = TRUE), outer = NULL,
           inner = NULL, labels = NULL, units = NULL)
{
  if (!(inherits(formula, "formula") && length(formula) == 3)) {
    stop("first argument to nfGroupedData must be a two-sided formula")
  }
  grpForm <- getGroupsFormula(formula, asList = TRUE)
  if (is.null(grpForm)) {
    stop("Right hand side of first argument must be a conditional expression")
  }
  if (length(grpForm) > 1) {
    stop("Only one level of grouping allowed")
  }
  ## create a data frame in which formula, inner, and outer can be evaluated
  if (missing(data)) {
    vnames <- all.vars(asOneFormula(formula, inner, outer))
    alist <- lapply(as.list(vnames), as.name)
    names(alist) <- vnames
    data <- do.call('data.frame', alist)
  } else {
    if (!inherits(data, "data.frame")) {
      stop("second argument to groupedData must inherit from data.frame")
    }
  }
  ## Although response and primary are not always used, they are
  ## evaluated here to verify that they can be evaluated.
  response <- getResponse(data, formula)
  primary <- getCovariate(data, formula)
  groupName <- names(grpForm)
  groups <- getGroups(data, formula)
  data[[groupName]] <- groups

  if (order.groups) {
    if (!inherits(groups, "ordered")) {
      if (is.null(outer)) {
        data[[groupName]] <-
          ordered(groups,
                  levels = names(sort(tapply(response, groups, FUN))))
      } else {
        ## split the data according to the 'outer' factors and
        ## obtain the order within each group
        outer <- asOneSidedFormula(outer)
        ## paste together all variables in outer with a character
        ## unlikely to be in a name
        combined <-
          do.call("paste", c(data[, all.vars(outer), drop = FALSE], sep='\007'))
        levs <-
          as.vector(unlist(lapply(split(data.frame(response = response,
                                                   groups = groups),
                                        combined),
                                  function(obj, func) {
                                    names(sort(tapply(obj$response,
                                                      obj$groups, func)))
                                  }, func = FUN)))
        data[[groupName]] <- ordered(groups, levels = levs)
      }
    }
  }
  attr(data, "formula") <- formula
  attr(data, "labels") <- labels
  attr(data, "units") <- units
  attr(data, "outer") <- outer
  attr(data, "inner") <- inner
  attr( data, "FUN" ) <- FUN
  attr( data, "order.groups" ) <- order.groups
  dClass <-  unique(c("nfGroupedData", "groupedData", class(data)))
  if ((length(all.vars(getCovariateFormula(formula))) == 0) ||
      (data.class(primary) != "numeric")) {
    ## primary covariate is a factor or a "1"
    class(data) <- unique(c("nffGroupedData", dClass))
  } else {
    ## primary covariate is numeric
    class(data) <- unique(c("nfnGroupedData", dClass))
  }
  data
}

nmGroupedData <-
  ## Constructor for the nmGroupedData class.  Takes a formula and a frame
  ## The formula must be of the form
  ## "respose ~ primary | groups1/groups2/.../groups_k",
  ## where groups_i evaluates to a factor in frame.
  function(formula, data = sys.parent(1), order.groups = TRUE,
	   FUN = function(x) max(x, na.rm = TRUE), outer = NULL,
           inner = NULL, labels = NULL, units = NULL)
{
  checkForList <- function(object, nams, expand = FALSE) {
    if (is.null(object)) return(object)
    if (is.list(object)) {
      if (is.null(names(object))) {
        names(object) <- nams[1:length(object)]
      }
      return(object)
    }
    if (expand) {
      object <- rep(list(object), length(nams))
      names(object) <- nams
      return(object)
    }
    object <- list(object)
    names(object) <- nams[length(nams)]
    object
  }
  if (!(inherits(formula, "formula") && length(formula) == 3)) {
    stop("first argument to nmGroupedData must be a two-sided formula")
  }
  grpForm <- getGroupsFormula(formula, asList = TRUE)
  if (is.null(grpForm)) {
    stop("Right hand side of first argument must be a conditional expression")
  }
  if (length(grpForm) == 1) {           # single group
    mCall <- match.call()[-1]
    do.call("nfGroupedData", mCall)
  }

  grpNames <- names(grpForm)
  names(grpNames) <- grpNames
  ## ckecking if arguments are lists
  order.groups <- checkForList(order.groups, grpNames, TRUE)
  outer <- checkForList(outer, grpNames)
  inner <- checkForList(inner, grpNames)

  ## create a data frame in which formula, outer, and inner can be evaluated
  if (missing(data)) {
    vnames <- all.vars(asOneFormula(formula, outer, inner))
    alist <- lapply(as.list(vnames), as.name)
    names(alist) <- vnames
    data <- do.call('data.frame', alist)
  } else {
    if (!inherits(data, "data.frame")) {
      stop("second argument to groupedData must inherit from data.frame")
    }
  }
  ## Although response and primary are not always used, they are
  ## evaluated here to verify that they can be evaluated.
  response <- getResponse(data, formula)
  primary <- getCovariate(data, formula)
  groups <- getGroups(data, formula)

  attr(data, "formula") <- formula
  attr(data, "formulaList") <- grpForm
  attr(data, "labels") <- labels
  attr(data, "units") <- units
  attr(data, "inner") <- inner
  attr(data, "outer") <- outer
  attr(data, "order.groups") <- order.groups
  attr(data, "FUN") <- FUN
  class(data) <- unique(c("nmGroupedData", "groupedData", class(data)))
  data
}

###*# Methods for standard generics

as.data.frame.groupedData <-
  function(x, row.names = NULL, optional = FALSE, ...)
{
  attributes(x) <- attributes(x)[c("names", "row.names")]
  class(x) <- "data.frame"
  NextMethod()
}

collapse.groupedData <-
  function(object, collapseLevel = Q, displayLevel = collapseLevel,
           outer = NULL, inner = NULL, preserve = NULL, FUN = mean,
           subset = NULL, ...)
{
  form <- formula(object)
  grpForm <- getGroupsFormula(form, asList = TRUE)
  grpNames <- names(grpForm)
  names(grpNames) <- grpNames
  Q <- length(grpForm)                  # number of levels
  if (Q == 1) {                         # no collapsing
    if (!missing(subset)) {
      warning("Subset ignored with single grouping factor")
    }
    return(object)
  }
  groups <- getGroups(object, form, level = 1:Q)
  if (!is.null(subset)) {
    ## choosing some levels of grouping factors
    if (!is.list(subset)) {
      stop("\"Subset\" must be a list")
    }
    if (!any(is.na(match(names(subset), 1:Q)))) {
      ## subset names given as integers
      names(subset) <- grpNames[names(subset)]
    }
    if (any(is.na(match(names(subset), grpNames)))) {
      stop("Undefined group declared in \"subset\"")
    }
    auxSubset <- rep(TRUE, dim(object)[1])
    for(i in names(subset)) {
      auxSubset <- auxSubset & as.logical(match(groups[[i]], subset[[i]], 0))
    }
    object <- object[auxSubset, , drop = FALSE]
    groups <- groups[auxSubset, , drop = FALSE]
    groups[] <- lapply(groups, function(x) x[drop = TRUE])
  }
  if (length(displayLevel) != 1) {
    stop("Only one display level allowed")
  }
  if (is.null(grpForm[[displayLevel]])) {
    stop(paste("Undefined display level",displayLevel,"for",
	       substitute(object)))
  }
  attribs <- attributes(object)
  ord <- attribs[["order.groups"]][[displayLevel]]
  if (is.logical(outer)) {
    outer <- attribs[["outer"]][[displayLevel]]
  }
  if (is.logical(inner)) {
    inner <- attribs[["inner"]][[displayLevel]]
  }
  form[[3]][[3]] <- grpForm[[displayLevel]][[2]]
  args <- list(formula = form,
	       order.groups = ord,
	       FUN = attribs[["FUN"]],
	       outer = outer,
	       inner = inner,
	       labels = attribs[["labels"]],
	       units = attribs[["units"]])
  dlevel <- if (is.character(displayLevel)) { # as the level name
              match(displayLevel, grpNames)
	    } else {                    # as the level number
	      displayLevel
	    }
  if (dlevel < Q) {			# may need to collapse object
    if (is.null(grpForm[[collapseLevel]])) {
      stop(paste("Undefined collapsing level", collapseLevel,
		 "for", substitute(object)))
    }
    clevel <- if (is.character(collapseLevel)) {
      match(collapseLevel, grpNames)
    } else {
      collapseLevel
    }
    if (clevel < dlevel) {
      clevel <- dlevel
      warning(paste("Collapsing level cannot be smaller than display level;",
		    "setting it to the display level"))
    }
    if ((dlevel < clevel) || (clevel < Q)) {
      collapseGroups <-
        do.call("paste", c(lapply(groups[, 1:clevel, drop = FALSE ],
                                  as.character), sep = "\007"))
      if (dlevel < clevel) {            # may need innerGroups
	object[[".collapseGroups"]] <- as.factor(collapseGroups)
      }
      if (!is.null(preserve)) {
        if (!(inherits(preserve, "formula") && length(preserve) == 2)) {
          stop("\"Preserve\" must be a two-sided formula")
        }
        collapseGroups <- paste(collapseGroups, eval(preserve[[2]], object),
                                sep = "\007")
      }
      collapseGroups <- paste(collapseGroups, getCovariate(object),
                              sep = "\007")
      collapseGroups <- ordered(collapseGroups,
                                levels = unique(as.character(collapseGroups)))
      if (length(levels(collapseGroups)) < dim(object)[1]) {
        ## collapsing the object
        object <- gsummary(object, groups = collapseGroups, FUN = FUN)
        row.names(object) <- 1:dim(object)[1]
        ## need to recalculate groups --- fix from JCP
        groups <- getGroups(object, grpForm, level = 1:Q)
      }
    }
  }
  object <- as.data.frame(object)
  if (dlevel == 1) {			# no outer groups
    args[["data"]] <- object
    value <- do.call("nfGroupedData", args)
  } else {
    ## need to establish an appropriate ordering
    namesDgrp <- names(groups)
    for(i in 2:Q) {
      groups[, i] <- paste(as.character(groups[, i - 1]),
                           as.character(groups[, i]), sep = "/")
      namesDgrp[i] <- paste(namesDgrp[i-1], namesDgrp[i], sep = "/")
    }
    displayGroups <- groups[, dlevel]
    isOrd <- unlist(lapply(groups, is.ordered))[1:dlevel]
    ordOrig <- unlist(attribs[["order.groups"]][1:dlevel]) & !isOrd
    if (any(ordOrig)) {
      groups[ordOrig] <- lapply(groups[ordOrig], function(el, y, func) {
	ordered(el, levels = names(sort(tapply(y, el, func))))
      }, y = getResponse(object, form), func = attribs[["FUN"]])
    }
    if (!is.null(outer)) {
      outFact <- do.call("paste", c(lapply(object[, all.vars(outer)],
					 as.character), sep = "\007"))
      groups <- c(list(outFact), groups)
    }
    displayGroups <- ordered(displayGroups,
      levels = unique(as.character(displayGroups[do.call("order", groups)])))
    form[[3]][[3]] <- as.name(".groups")
    object[[".groups"]] <- displayGroups
    args[["formula"]] <- form
    args[["data"]] <- object
    value <- do.call("nfGroupedData", args)
  }
  if (match(".collapseGroups", names(object), 0)) {
    groups <- eval(form[[3]][[3]], value)
    rnams <- unlist(split(1:nrow(value), groups))
    cGroups <- unlist(lapply(split(value[[".collapseGroups"]], groups),
                             function(el) as.integer(el[drop = TRUE])))
    value[[".collapseGroups"]] <- cGroups[order(rnams)]
    attr(value, "innerGroups") <- ~.collapseGroups
  }
  if (dlevel > 1 && !is.na(match(".groups", names(value)))) {
    attr(value[,".groups"], "label") <- namesDgrp[dlevel]
  }
  value
}

formula.groupedData <-
  function(x, ...) eval(attr(x, "formula"))

plot.nfnGroupedData <-
  function(x, outer = NULL, inner = NULL, innerGroups = NULL,
           xlab = paste(attr(x, "labels")$x, attr(x, "units")$x),
           ylab = paste(attr(x, "labels")$y, attr(x, "units")$y),
           strip = function(...) strip.default(..., style = 1),
           aspect = "xy",
           panel = function(x, y, ...) {
             if (grid) panel.grid()
             panel.xyplot(x, y, ...)
             y.avg <- tapply(y, x, mean) # lines through average y
             y.avg <- y.avg[!is.na(y.avg)]
             if (length(y.avg) > 0) {
               xvals <- as.numeric(names(y.avg))
               ord <- order(xvals)
               panel.xyplot(xvals[ord], y.avg[ord], type = "l")
             }
           }, key = TRUE, grid = TRUE, ...)
{
  labels <- list(xlab = xlab, ylab =  ylab)
  labels <- labels[unlist(lapply(labels, length)) > 0]
  args <- c(list(attr(x, "formula"), data = x, strip = strip,
		 aspect = aspect, panel = panel), labels)
  if (length(outer) > 0) {
    if (is.logical(outer) && outer) {	# get the default outer formula
      outer <- attr(x, "outer")
    }
    args[[1]][[3]][[3]] <- asOneSidedFormula(outer)[[2]]
    if (length(innerGroups) == 0) {
      innerGroups <- getGroupsFormula(x)
    }
  }
  if ((length(innerGroups) > 0) && (length(inner) == 0)) {
    inner <- innerGroups
    innerGroups <- NULL
  }
  if (length(inner) > 0) {
    if (is.logical(inner) && inner) {	# get the default inner formula
      inner <- attr(x, "inner")
    }
    args[["subscripts"]] <- TRUE
    trll.set <- trellis.par.get("superpose.line")[c("lty", "col")]
    if (length(innerGroups) == 0) {
      args[["groups"]] <- asOneSidedFormula(inner)[[2]]
      if (missing(inner)) {
        Inner <- NULL
        trll.lty <- trll.set[["lty"]][1]
        trll.col <- trll.set[["col"]][1]
        assign("trll.lty", trll.lty)
        assign("trll.col", trll.col)
        args[["panel"]] <- function(x, y, subscripts, groups, ...)
          {
            panel.grid()
            panel.xyplot(x, y, ...)
            panel.superpose(x, y, subscripts, groups, type = "l",
                            col = trll.col, lty = trll.lty)
          }
      } else {
        Inner <- as.factor(eval(asOneSidedFormula(inner)[[2]], x))
        levInn <- levels(Inner)
        args[["panel"]] <- function(x, y, subscripts, groups, ...)
          {
            panel.grid()
            panel.xyplot(x, y, ...)
            panel.superpose(x, y, subscripts, groups, type = "l")
          }
      }
    } else {				#inner and innerGroups
      args[["groups"]] <- asOneSidedFormula(innerGroups)[[2]]
      Inner <- as.factor(eval(asOneSidedFormula(inner)[[2]], x))
      levInn <- levels(Inner)
      Inner <- (as.integer(Inner) - 1) %% length(trll.set[["lty"]]) + 1
      trll.lty <- trll.set[["lty"]][Inner]
      trll.col <- trll.set[["col"]][Inner]
      assign("trll.lty", trll.lty)
      assign("trll.col", trll.col)
      args[["panel"]] <- function(x, y, subscripts, groups, ...)
	{
	  panel.grid()
	  panel.xyplot(x, y, ...)
          aux <- match(unique(groups), groups)
          panel.superpose(x, y, subscripts, groups, type = "l",
			  col = trll.col[aux],
			  lty = trll.lty[aux])
	}
    }
  } else {
    Inner <- NULL
  }
  if(is.logical(key)) {
    if(key && (!is.null(Inner) && (lInn <- length(levInn)) > 1)) {
      ## lInn <- min(c(lInn, length(trll.set[["lty"]])))
      args[["key"]] <-
	list(lines = Rows(trellis.par.get("superpose.line"), 1:lInn),
	     text = list(levels = levInn), columns = min(6, lInn))
    }
  } else {
    args[["key"]] <- key
  }
  dots <- list(...)
  args[names(dots)] <- dots
  assign("grid", grid)
  do.call("xyplot", args)
}

plot.nffGroupedData <-
  function(x, outer = NULL, inner = NULL, innerGroups = NULL,
           xlab = paste(attr(x, "labels")$y, attr(x, "units")$y),
           ylab = groupLabel,
           strip = function(...) strip.default(..., style = 1),
           panel = function(x, y) {
             dot.line <- trellis.par.get("dot.line")
             panel.abline(h = y, lwd = dot.line$lwd,
                          lty = dot.line$lty, col = dot.line$col)
             panel.dotplot(x, y)
           }, key = length(inner) > 0, grid, ...)
{
  groupExpr <- c_deparse(getGroupsFormula(x)[[2]])
  if (is.null(groupLabel <- attr(x[, groupExpr], "label"))) {
    groupLabel <- groupExpr
  }
  labels <- list(xlab = xlab, ylab = ylab)
  labels <- labels[unlist(lapply(labels, length)) > 0]
  if (length(outer) > 0) {
    if (is.logical(outer) && outer) {	# get the default outer formula
      form <- formula(paste(groupExpr,
                            "~", deparse(getResponseFormula(x)[[2]]),"|",
			     c_deparse(attr(x, "outer")[[2]])))
    } else {
      form <-  formula(paste(groupExpr,
			    "~", deparse(getResponseFormula(x)[[2]]),"|",
			     c_deparse(outer[[2]])))
    }
  } else {
    form <- formula(paste(groupExpr, "~",
                          deparse(getResponseFormula(x)[[2]])))
  }
  args <- c(list(form, data = x, strip = strip, panel = panel),
            labels)
  if ((length(innerGroups) > 0) && (length(inner) == 0)) {
    inner <- innerGroups
    innerGroups <- NULL
  }
  if (length(inner) == 0) {
    covForm <- getCovariateFormula(x)
    if (length(all.vars(covForm)) > 0) {# non-trivial covariate
      inner <- covForm
    }
  }
  if (length(inner) > 0) {
    if (is.logical(inner) && inner) {	# get the default inner formula
      inner <- attr(x, "inner")
    }
    args[["subscripts"]] <- TRUE
    args[["groups"]] <- asOneSidedFormula(inner)[[2]]
    args[["panel"]] <- function(x, y, subscripts, groups)
      {
	dot.line <- trellis.par.get("dot.line")
	panel.abline(h = y, lwd = dot.line$lwd,
		     lty = dot.line$lty, col = dot.line$col)
	panel.superpose(x, y, subscripts, groups)
      }
  }
  if(is.logical(key) && key && (length(inner) > 0)) {
    Inner <- eval(inner[[2]], x)
    levInn <- levels(as.factor(Inner))
    lInn <- length(levInn)
    ## lInn <- min(c(lInn, length(trellis.par.get("superpose.symbol")$pch)))
    args[["key"]] <-
      list(points = Rows(trellis.par.get("superpose.symbol"), 1:lInn),
	     text = list(levels = levInn), columns = min(6, lInn))
  }
  dots <- list(...)
  args[names(dots)] <- dots
  do.call("dotplot", args)
}

plot.nmGroupedData <-
  function(x, collapseLevel = Q, displayLevel = collapseLevel,
	   outer = NULL, inner = NULL, preserve = NULL, FUN = mean,
           subset = NULL, key = TRUE, grid = TRUE, ...)
{
  args <- list(outer = outer, inner = inner, key = key, grid = grid, ...)
  Q <- length(getGroupsFormula(x, asList = TRUE))
  if (is.null(preserve) && (collapseLevel < Q) && (!is.null(inner))) {
    if (is.logical(inner)) {
      preserve <- attr(x, "inner")[[displayLevel]]
    } else {
      preserve <- inner
    }
  }
  x <- collapse(x, collapseLevel, displayLevel, outer, inner,
		preserve, FUN, subset)
  args[["innerGroups"]] <- attr(x, "innerGroups")
  args[["x"]] <- x
  do.call("plot", args)
}

print.groupedData <- function(x, ...)
{
  cat("Grouped Data: ")
  if(identical(emptyenv(), environment(frm <- attr(x, "formula"))))
      environment(frm) <- globalenv()# for printing, as that will be suppressed
  print(frm)
  print.data.frame(x, ...)
}

update.groupedData <-
  function(object, formula, data, order.groups, FUN, outer, inner,
           labels, units, ...)

{
  args <- as.list( attributes( object ) )
  args <- args[is.na(match(names(args),
                       c("names", "row.names", "class", "formulaList")))]
  thisCall <- as.list(match.call())[-(1:2)]
  args[names(thisCall)] <- thisCall
  if (is.null(args[["data"]])) args[["data"]] <- as.data.frame(object)
  do.call("groupedData", args)
}

"[.groupedData" <-
  function(x, i, j, drop = if (missing(i)) TRUE else length(cols) == 1)
{
  oAttr <- attributes(x)
  x <- as.data.frame(x)
  data <- NextMethod()
  if (!inherits(data, "data.frame")) return(data)
  allV <- all.vars(asOneFormula(oAttr[["formula"]], oAttr[["inner"]],
                                oAttr[["outer"]]))
  ## check if any columns used in formulas were deleted
  if( any( is.na( match( allV, names(data) ) ) ) ) { # return data frame
    cols <- ncol(data)
    return( data[, seq(length=ncol(data)), drop = drop] )
  }
  args <- as.list(oAttr)
  args <- args[ is.na( match( names( args ), c( "names", "row.names" ) ) ) ]
  if (nrow(x) == nrow(data)) {		# only columns deleted
    attributes(data) <- c( attributes( data ), args )
    return( data )
  }
  ## pruning the levels of factors
  whichFact <- unlist(lapply(data, is.factor))
  data[whichFact] <- lapply(data[whichFact], function(x) x[drop = TRUE])
  args <- c(args[!is.na(match(names( args ), c("formula", "order.groups",
            "FUN", "outer", "inner", "labels", "units")))], list(data = data))
  do.call("groupedData", args)
}

isBalanced.groupedData <-
  function(object, countOnly = FALSE, level)
{
  if (missing(level)) {
    level <- length(getGroupsFormula(object, asList = TRUE))
  }
  if ( countOnly ) {
    return( length( unique( table( getGroups(object, level = level) ) ) ) == 1 )
  }
  length(unique(table(getCovariate(object),
                      getGroups(object, level = level)))) == 1
}

asTable.groupedData <-
  function(object)
{
  if (length(getGroupsFormula(object, asList = TRUE)) > 1) {
    stop("asTable cannot be used with multilevel grouped data")
  }
  tab <- table( getGroups(object), getCovariate(object) )
  if (1 != length(unique(tab)))
    stop("asTable can only be used with balanced groupedData objects")
  tab[] <- getResponse(object)[order(getCovariate(object),getGroups(object))]
  tab
}

balancedGrouped <-
  function(form, data, labels = NULL, units = NULL)
{
  form <- as.formula( form )
  data <- t( as.matrix( data ) )
  dn <- dimnames( data )
  if ( all( !is.na( as.numeric( dn[[1]] ) ) ) ) {
    dn[[1]] <- as.numeric( dn[[1]] )
  }
  names(dn) <- c( as.character(getCovariateFormula(form)[[2]]),
                  as.character(getGroupsFormula(form)[[2]]) )
  frm <- do.call("expand.grid", dn)
  frm[[ as.character(getResponseFormula(form)[[2]]) ]] <- as.vector( data )
  do.call("groupedData", list(form, data = frm, labels = labels, units = units ))
}

### Local variables:
### mode: S
### End:


