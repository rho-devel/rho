

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



## from R 2.4.0 onwards, hist.default produces a warning when any
## 'unused' arguments are supplied.  Before then, all ... arguments
## used to be supplied to hist(), so arguments to hist() could be
## supplied that way.  This is no longer possible, so the following
## wrapper is being added to capture undesirable arguments.


hist.constructor <- 
    function(x, breaks, 
             include.lowest = TRUE,
             right = TRUE, ...)
{
    if (is.numeric(breaks) && length(breaks) > 1)
        hist(as.numeric(x), breaks = breaks, plot = FALSE,
             include.lowest = include.lowest,
             right = right)
    else
        hist(as.numeric(x), breaks = breaks, right = right, plot = FALSE)
}


prepanel.default.histogram <-
    function(x,
             breaks,
             equal.widths = TRUE,
             type = "density",
             nint = round(log2(length(x)) + 1),
             ...)
{
    if (length(x) < 1) prepanel.null()
    else
    {
        if (is.null(breaks))
        {
            breaks <-
                if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
                else if (equal.widths) do.breaks(range(x, finite = TRUE), nint)
                else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <-
            hist.constructor(x, breaks = breaks, ...)
        y <-
            switch(type,
                   count = h$counts,
                   percent = 100 * h$counts/length(x),
                   density = h$density)
        ## y <-
        ##     if (type == "count") h$counts
        ##     else if (type == "percent") 100 * h$counts / length(x)
        ##     else h$density
        list(xlim =
             if (is.factor(x)) levels(x)
             else scale.limits(c(x, h$breaks)),
             ## if (is.factor(x)) levels(x)
             ## else range(x, h$breaks, finite = TRUE),
             ylim = range(0, y, finite = TRUE),
             dx = 1,
             dy = 1)
    }
}



panel.histogram <-
    function(x,
             breaks,
             equal.widths = TRUE,
             type = "density",
             nint = round(log2(length(x)) + 1),
             alpha = plot.polygon$alpha,
             col = plot.polygon$col,
             border = plot.polygon$border,
             lty = plot.polygon$lty,
             lwd = plot.polygon$lwd,
             ...,
             identifier = "histogram")
{
    plot.polygon  <- trellis.par.get("plot.polygon")

    xscale <- current.panel.limits()$xlim
    panel.lines(x = xscale[1] + diff(xscale) * c(0.05, 0.95),
                y = c(0,0),
                col = border, lty = lty, lwd = lwd, alpha = alpha,
                identifier = paste(identifier, "baseline", sep = "."))
##     grid.lines(x = c(0.05, 0.95),
##                y = unit(c(0,0), "native"),
##                gp = gpar(col = border, lty = lty, lwd = lwd, alpha = alpha),
##                default.units = "npc")
        
    if (length(x) > 0)
    {
        if (is.null(breaks))
        {
            breaks <-
                if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
                else if (equal.widths) do.breaks(range(x, finite = TRUE), nint)
                else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- hist.constructor(x, breaks = breaks, ...)
        y <-
            switch(type,
                   count = h$counts,
                   percent = 100 * h$counts/length(x),
                   density = h$density)
        ## y <-
        ##     if (type == "count") h$counts
        ##     else if (type == "percent") 100 * h$counts/length(x)
        ##     else h$density
        breaks <- h$breaks

        nb <- length(breaks)
        if (length(y) != nb-1) warning("problem with 'hist' computations")

        if (nb > 1)
        {
            panel.rect(x = breaks[-nb],
                       y = 0,
                       height = y,
                       width = diff(breaks),
                       col = col, alpha = alpha,
                       border = border, lty = lty,
                       lwd = lwd,
                       just = c("left", "bottom"),
                       identifier = identifier)
        }
    }
}




histogram <- function(x, data, ...) UseMethod("histogram")


histogram.factor <- histogram.numeric <-
    function(x, data = NULL, xlab = deparse(substitute(x)), ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(histogram)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment() # list(x = x)
    ccall$xlab <- xlab
    ccall$x <- ~x
    ccall[[1]] <- quote(lattice::histogram)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}





histogram.formula <-
    function(x,
             data = NULL,
             allow.multiple = is.null(groups) || outer,
             outer = TRUE,
             auto.key = FALSE,
             aspect = "fill",
             panel = lattice.getOption("panel.histogram"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             type = c("percent", "count", "density"),
             nint = if (is.factor(x)) nlevels(x)
             else round(log2(length(x)) + 1),
             endpoints = extend.limits(range(as.numeric(x), finite = TRUE), prop = 0.04),
             breaks,
             equal.widths = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales = list(),
             default.prepanel = lattice.getOption("prepanel.default.histogram"),
             subscripts = !is.null(groups),
             subset = TRUE)
{
    formula <- x
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))
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
    x <- form$right
    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }
    if (missing(xlab)) xlab <- form$right.name
    if (missing(ylab)) ylab <- TRUE

    ##if(!(is.numeric(x) || is.factor(x)))
    ##    warning("x should be numeric")
    ##x <- as.numeric(x)

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
                       ylab.default = "dummy",
                       lattice.options = lattice.options), dots))
                          
    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(histogram)

    ## Step 2: Compute scales.common (leaving out limits for now)

    if (is.character(scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))

    ## Step 3: Decide if limits were specified in call:
    
    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limits))
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
        warning("Can't have log Y-scale")
        have.ylog <- FALSE
        foo$y.scales$log <- FALSE
    }

    ## should type default to density?  "Yes" when a relative
    ## frequency histogram is going to be misleading.

    if (missing(breaks)) # explicit NULL, or function, or character is fine
    {
        breaks <- lattice.getOption("histogram.breaks")
        if (is.null(breaks)) # nothing specified
            breaks <- # use nint and endpoints
                if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
                else do.breaks(as.numeric(endpoints), nint)
    }

    prefer.density <- 
        (is.function(breaks) || 
         (is.null(breaks) && !equal.widths) ||
         (is.numeric(breaks) && (length(breaks) > 1) && !isTRUE(all.equal(diff(range(diff(breaks))), 0)))
         )
    if (missing(type) && prefer.density)
        type <- "density"
    type <- match.arg(type)
    if (prefer.density && type != "density")
        warning(gettextf("type='%s' can be misleading in this context", type))

    ## this is normally done earlier (in trellis.skeleton), but in
    ## this case we needed to wait till type is determined

    foo$ylab.default <-
        switch(type,
               count   = gettext("Count"),
               percent = gettext("Percent of Total"),
               density = gettext("Density"))

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(breaks = breaks,
               type = type,
               equal.widths = equal.widths,
               nint = nint),
          dots)
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

    cond.current.level <- rep(1, length(cond))

    for (packet.number in seq_len(npackets))
    {
        id <- compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)

        foo$panel.args[[packet.number]] <- list(x = x[id])
        if (subscripts)
            foo$panel.args[[packet.number]]$subscripts <-
                subscr[id]

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

    more.comp <- c(limits.and.aspect(default.prepanel,
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
                                      points = FALSE,
                                      rectangles = TRUE,
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
