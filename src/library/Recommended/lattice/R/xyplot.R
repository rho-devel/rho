

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA




prepanel.default.xyplot <-
    function(x, y, type, subscripts, groups = NULL, ...)
{
    ## Note: shingles satisfy is.numeric()
    if (any(!is.na(x)) && any(!is.na(y)))
    {
        ord <- order(as.numeric(x))
        if (!is.null(groups))
        {
            gg <- groups[subscripts]
            dx <- unlist(lapply(split(as.numeric(x)[ord], gg[ord]), diff))
            dy <- unlist(lapply(split(as.numeric(y)[ord], gg[ord]), diff))
            ## ok <- !is.na(gg)

            ## One may argue that points with is.na(gg) should be
            ## excluded from the data rectangle since they are not
            ## plotted.  For now I'm going to take the other view,
            ## namely that the points are there, they just happen to
            ## be invisible because the value of the variable defining
            ## their graphical parameters is unknown.
        }
        else
        {
            dx <- diff(as.numeric(x[ord]))
            dy <- diff(as.numeric(y[ord]))
            ## ok <- TRUE
        }
        list(xlim = scale.limits(x), ylim = scale.limits(y), dx = dx, dy = dy,
             xat = if (is.factor(x)) sort(unique(as.numeric(x))) else NULL,
             yat = if (is.factor(y)) sort(unique(as.numeric(y))) else NULL)
    }
    else prepanel.null()
}




panel.xyplot <-
    function(x, y, type = "p",
             groups = NULL,
             pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch,
             col,
             col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
             col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
             font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font,
             fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily,
             fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface,
             lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
             cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
             fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill,
             lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
             horizontal = FALSE,
             ...,
             grid = FALSE, abline = NULL,
             jitter.x = FALSE, jitter.y = FALSE,
             factor = 0.5, amount = NULL,
             identifier = "xyplot")
{
    if (all(is.na(x) | is.na(y))) return()
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
        if (missing(col.symbol)) col.symbol <- col
    }
    if (missing(grid) && ("g" %in% type)) grid <- TRUE ## FIXME: what if list?
    if (!identical(grid, FALSE))
    {
        if (!is.list(grid))
            grid <- switch(as.character(grid),
                           "TRUE" = list(h = -1, v = -1, x = x, y = y),
                           "h" = list(h = -1, v = 0, y = y),
                           "v" = list(h = 0, v = -1, x = x),
                           list(h = 0, v = 0))
        do.call(panel.grid, grid)
    }
    if (!is.null(abline))
    {
        if (is.numeric(abline)) abline <- as.list(abline)
        do.call(panel.abline, abline)
    }
    if (!is.null(groups))
        panel.superpose(x, y,
                        type = type,
                        groups = groups,
                        pch = pch,
                        col.line = col.line,
                        col.symbol = col.symbol,
                        font = font,
                        fontfamily = fontfamily,
                        fontface = fontface,
                        lty = lty,
                        cex = cex,
                        fill = fill,
                        lwd = lwd,
                        horizontal = horizontal,
                        panel.groups = panel.xyplot,
                        jitter.x = jitter.x,
                        jitter.y = jitter.y,
                        factor = factor,
                        amount = amount,
                        grid = FALSE, ## grid=TRUE/type="g" already handled
                        ...)
    else
    {
        x <- as.numeric(x)
        y <- as.numeric(y)
        id <- identifier
        if ("o" %in% type || "b" %in% type) type <- c(type, "p", "l")
        if ("p" %in% type)
            panel.points(x = if (jitter.x) jitter(x, factor = factor, amount = amount) else x,
                         y = if (jitter.y) jitter(y, factor = factor, amount = amount) else y,
                         cex = cex,
                         fill = fill,
                         font = font,
                         fontfamily = fontfamily,
                         fontface = fontface,
                         col = col.symbol,
                         pch = pch, ...,
                         identifier = id)
        if ("l" %in% type)
            panel.lines(x = x, y = y, lty = lty, col = col.line, lwd = lwd,
                        ..., identifier = id)
        if ("h" %in% type)
        {
            if (horizontal)
                panel.lines(x = x, y = y, type = "H",
                            lty = lty, col = col.line, lwd = lwd,
                            ..., identifier = id)
            else
                panel.lines(x = x, y = y, type = "h",
                            lty = lty, col = col.line, lwd = lwd,
                            ..., identifier = id)
        }

        ## FIXME: should this be delegated to llines with type='s'?
        if ("s" %in% type)
        {
            ord <- if (horizontal) sort.list(y) else sort.list(x)
            n <- length(x)
            xx <- numeric(2*n-1)
            yy <- numeric(2*n-1)

            xx[2*1:n-1] <- x[ord]
            yy[2*1:n-1] <- y[ord]
            xx[2*1:(n-1)] <- x[ord][-1]
            yy[2*1:(n-1)] <- y[ord][-n]
            panel.lines(x = xx, y = yy,
                        lty = lty, col = col.line, lwd = lwd, ...,
                        identifier = id)
        }
        if ("S" %in% type)
        {
            ord <- if (horizontal) sort.list(y) else sort.list(x)
            n <- length(x)
            xx <- numeric(2*n-1)
            yy <- numeric(2*n-1)

            xx[2*1:n-1] <- x[ord]
            yy[2*1:n-1] <- y[ord]
            xx[2*1:(n-1)] <- x[ord][-n]
            yy[2*1:(n-1)] <- y[ord][-1]
            panel.lines(x = xx, y = yy,
                        lty = lty, col = col.line, lwd = lwd,
                        ..., identifier = id)
        }
        if ("r" %in% type) panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, ...)
        if ("smooth" %in% type)
            panel.loess(x, y, horizontal = horizontal,
                        col = col.line, lty = lty, lwd = lwd, ...)
        if ("spline" %in% type)
            panel.spline(x, y, horizontal = horizontal,
                        col = col.line, lty = lty, lwd = lwd, ...)
        if ("a" %in% type)
            panel.linejoin(x, y, 
                           horizontal = horizontal,
                           lwd = lwd,
                           lty = lty,
                           col.line = col.line,
                           ...)
    }
}



xyplot <- function(x, data, ...) UseMethod("xyplot")


## xyplot.default <- 
##     function(x, data = NULL,
##              formula = try(stats::formula(x), silent = TRUE),
##              ...)
## {
##     ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(xyplot)
##     ccall <- match.call()
##     if (!is.null(ccall$data)) 
##         warning("explicit 'data' specification ignored")
##     if (!inherits(formula, "formula")) formula = y ~ x
##     ccall$data <- x
##     ccall$x <- formula
##     ccall$formula <- NULL
##     ccall[[1]] <- quote(lattice::xyplot)
##     ans <- eval.parent(ccall)
##     ans$call <- ocall
##     ans
## }



xyplot.formula <-
    function(x,
             data = NULL,
             allow.multiple = is.null(groups) || outer,
             outer = !is.null(groups),
             auto.key = FALSE,
             aspect = "fill",
             panel = lattice.getOption("panel.xyplot"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales = list(),
             default.prepanel = lattice.getOption("prepanel.default.xyplot"),
             subscripts = !is.null(groups),
             subset = TRUE)
{
    formula <- x 
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(x))
    subset <- eval(substitute(subset), data, environment(x))
    if (!is.null(lattice.options))
    {
        oopt <- lattice.options(lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

    form <-
        latticeParseFormula(formula, data, subset = subset,
                            groups = groups, multiple = allow.multiple,
                            outer = outer, subscripts = TRUE,
                            drop = drop.unused.levels)
    groups <- form$groups

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)
    
    if ("subscripts" %in% names(formals(panel))) subscripts <- TRUE
    if (subscripts) subscr <- form$subscr
    cond <- form$condition
    y <- form$left
    x <- form$right

    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }

    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- form$left.name

    ## S-PLUS requires both x and y to be numeric, but we
    ## don't. Question is, should we give a warning ? Nope.

    ##if (!(is.numeric(x) && is.numeric(y)))
    ##    warning("x and y are not both numeric")

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <-
        do.call("trellis.skeleton",
                c(list(formula = formula,
                       cond = cond,
                       aspect = aspect,
                       strip = strip,
                       panel = panel,
                       xlab = xlab,
                       ylab = ylab,
                       xlab.default = form$right.name,
                       ylab.default = form$left.name,
                       lattice.options = lattice.options), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(xyplot)
    ## This is NOT OK: foo$call <- sys.call(); foo$call[[1]] <- quote(xyplot)

    ## Step 2: Compute scales.common (leaving out limits for now)

    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))

    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limits)) # override xlim
    {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limits
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limits))
    {
        have.ylim <- TRUE
        ylim <- foo$y.scales$limits
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog)
    {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (have.xlim) xlim <- logLimits(xlim, xbase)
    }
    if (have.ylog)
    {
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (have.ylim) ylim <- logLimits(ylim, ybase)
    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <- dots
    if (subscripts) foo$panel.args.common$groups <- groups

    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length)))
        stop("mismatch in number of packets")
    foo$panel.args <- vector(mode = "list", length = npackets)

    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1)
    {
        dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
        dimnames(foo$packet.sizes) <- lapply(foo$condlevels, as.character)
    }

    ## 
    cond.current.level <- rep(1, length(cond))
    for (packet.number in seq_len(npackets))
    {
        id <- compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)

        foo$panel.args[[packet.number]] <-
            list(x = x[id], y = y[id])
        if (subscripts)
            foo$panel.args[[packet.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    ## FIXME: make this adjustment everywhere else

    more.comp <-
        c(limits.and.aspect(default.prepanel,
                            prepanel = prepanel, 
                            have.xlim = have.xlim, xlim = xlim, 
                            have.ylim = have.ylim, ylim = ylim, 
                            x.relation = foo$x.scales$relation,
                            y.relation = foo$y.scales$relation,
                            panel.args.common = foo$panel.args.common,
                            panel.args = foo$panel.args,
                            aspect = aspect,
                            npackets = npackets,
                            x.axs = foo$x.scales$axs,
                            y.axs = foo$y.scales$axs),
          cond.orders(foo))
    foo[names(more.comp)] <- more.comp

    if (is.null(foo$legend) && needAutoKey(auto.key, groups))
    {
        foo$legend <-
            list(list(fun = "drawSimpleKey",
                      args =
                      updateList(list(text = levels(as.factor(groups)),
                                      points = TRUE,
                                      rectangles = FALSE,
                                      lines = FALSE), 
                                 if (is.list(auto.key)) auto.key else list())))
        foo$legend[[1]]$x <- foo$legend[[1]]$args$x
        foo$legend[[1]]$y <- foo$legend[[1]]$args$y
        foo$legend[[1]]$corner <- foo$legend[[1]]$args$corner

        names(foo$legend) <- 
            if (any(c("x", "y", "corner") %in% names(foo$legend[[1]]$args)))
                "inside"
            else
                "top"
        if (!is.null(foo$legend[[1]]$args$space))
            names(foo$legend) <- foo$legend[[1]]$args$space
    }


    class(foo) <- "trellis"
    foo
}








