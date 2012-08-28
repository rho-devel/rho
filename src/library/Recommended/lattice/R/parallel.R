

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



prepanel.default.parallel <-
    function(x, y, z, ..., horizontal.axis = TRUE)
{
    if (horizontal.axis)
        list(xlim = c(0,1),
             ylim = extend.limits(c(1, ncol(as.data.frame(z))), prop = 0.03), 
             ##ylim = colnames(as.data.frame(z)),
             dx = 1,
             dy = 1)
    else
        list(xlim = extend.limits(c(1, ncol(as.data.frame(z))), prop = 0.03), 
             ylim = c(0,1),
             dx = 1,
             dy = 1)
}



panel.parallel <-
    function(x, y, z, subscripts,
             groups = NULL,
             col = superpose.line$col,
             lwd = superpose.line$lwd,
             lty = superpose.line$lty,
             alpha = superpose.line$alpha,
             common.scale = FALSE,
             lower = sapply(z, function(x) min(as.numeric(x), na.rm = TRUE)),
             upper = sapply(z, function(x) max(as.numeric(x), na.rm = TRUE)),
             ..., horizontal.axis = TRUE,
             identifier = "parallel")
{
    superpose.line <- trellis.par.get("superpose.line")
    reference.line <- trellis.par.get("reference.line")

    n.r <- ncol(z)
    n.c <- length(subscripts)
    if (is.null(groups))
    {
        col <- rep(col, length.out = n.c)
        lty <- rep(lty, length.out = n.c)
        lwd <- rep(lwd, length.out = n.c)
        alpha <- rep(alpha, length.out = n.c)
    }
    else
    {
        groups <- as.factor(groups)[subscripts]
        n.g <- nlevels(groups)
        gnum <- as.numeric(groups) ## probably unnecessary
        col <- rep(col, length.out = n.g)[gnum]
        lty <- rep(lty, length.out = n.g)[gnum]
        lwd <- rep(lwd, length.out = n.g)[gnum]
        alpha <- rep(alpha, length.out = n.g)[gnum]
    }

    if (is.function(lower)) lower <- sapply(z, lower)
    if (is.function(upper)) upper <- sapply(z, upper)
    if (common.scale)
    {
        lower <- min(lower)
        upper <- max(upper)
    }
    lower <- rep(lower, length.out = n.r)
    upper <- rep(upper, length.out = n.r)
    dif <- upper - lower

    if (n.r > 1)
        if (horizontal.axis)
            panel.segments(x0 = 0, x1 = 1,
                           y0 = seq_len(n.r),
                           y1 = seq_len(n.r),
                           col = reference.line$col,
                           lwd = reference.line$lwd,
                           lty = reference.line$lty,
                           identifier = paste(identifier, "reference",
                             sep = "."))
        else
            panel.segments(x0 = seq_len(n.r),
                           x1 = seq_len(n.r),
                           y0 = 0, y1 = 1,
                           col = reference.line$col,
                           lwd = reference.line$lwd,
                           lty = reference.line$lty,
                           identifier = paste(identifier, "reference",
                             sep = "."))
    else return(invisible())

    for (i in seq_len(n.r-1))
    {
        z0 <- (as.numeric(z[subscripts, i]) - lower[i])/dif[i]
        z1 <- (as.numeric(z[subscripts, i+1]) - lower[i+1])/dif[i+1]
        if (horizontal.axis)
            panel.segments(x0 = z0, y0 = i, x1 = z1, y1 = i + 1,
                           col = col,
                           lty = lty,
                           lwd = lwd,
                           alpha = alpha,
                           ...,
                           identifier = paste(identifier, i, sep = "."))
        else
            panel.segments(x0 = i, y0 = z0, x1 = i + 1, y1 = z1,
                           col = col,
                           lty = lty,
                           lwd = lwd,
                           alpha = alpha,
                           ...,
                           identifier = paste(identifier, i, sep = "."))
    }
    invisible()
}


parallelplot <- function(x, data, ...) UseMethod("parallelplot")

## {
##     ocall <- sys.call(sys.parent())
##     formula <- ocall$formula
##     if (!is.null(formula))
##     {
##         warning("The 'formula' argument has been renamed to 'x'. See ?xyplot")
##         ocall$formula <- NULL
##         if (!("x" %in% names(ocall))) ocall$x <- formula else warning("'formula' overridden by 'x'")
##         eval(ocall, parent.frame())
##     }
##     else UseMethod("parallelplot")
## }



parallelplot.matrix <-
parallelplot.data.frame <-
    function(x, data = NULL, ..., groups = NULL, subset = TRUE)
{
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$x <- ~x
    ccall$data <- environment()
    ##     ccall$data <-
    ##         list(x = x, groups = groups, subset = subset)
    ##     ccall$groups <- groups
    ##     ccall$subset <- subset
    ccall[[1]] <- quote(lattice::parallelplot)
    eval.parent(ccall)
}


parallelplot.formula <-
    function(x,
             data = NULL,
             auto.key = FALSE,
             aspect = "fill",
             between = list(x = 0.5, y = 0.5),
             panel = lattice.getOption("panel.parallel"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = NULL,
             xlim,
             ylab = NULL,
             ylim,
             varnames = NULL,
             horizontal.axis = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales = list(),
             default.prepanel = lattice.getOption("prepanel.default.parallel"),
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
    
    ## right.name <- deparse(substitute(x))
    ## x <- eval(substitute(x), data, environment(formula))
    form <-
        latticeParseFormula(formula, data,
                            subset = subset, groups = groups,
                            multiple = FALSE,
                            outer = FALSE, subscripts = TRUE,
                            drop = drop.unused.levels)


    ## We need to be careful with subscripts here. It HAS to be there,
    ## and it's to be used to index x, y, z (and not only groups,
    ## unlike in xyplot etc). This means we have to subset groups as
    ## well, which is about the only use for the subscripts calculated
    ## in latticeParseFormula, after which subscripts is regenerated
    ## as a straight sequence indexing the variables

    if (!is.null(form$groups)) groups <-  form$groups[form$subscr]
    subscr <- seq_len(nrow(form$right))

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)
    cond <- form$condition
    x <- as.data.frame(form$right)

    if (length(cond) == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, nrow(x))))
    }

    varnames <-
        if (is.null(varnames)) colnames(x)
        else varnames
    ## WAS eval(substitute(varnames), data, environment(formula)), but
    ## not sure why non-standard evaluation would be useful here
    if (length(varnames) != ncol(x)) stop("'varnames' has wrong length.")

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <-
        do.call("trellis.skeleton",
                c(list(formula = formula, 
                       cond = cond,
                       aspect = aspect,
                       between = between,
                       strip = strip,
                       panel = panel,
                       xlab = xlab,
                       ylab = ylab,
                       xlab.default = gettext("Parallel Coordinate Plot"),
                       lattice.options = lattice.options,
                       horizontal.axis = horizontal.axis),
                  dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(parallelplot)

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## overriding at and labels, maybe not necessary

    if (missing(default.scales))
    {
        default.scales <- 
            list(x = list(at = c(0, 1), labels = c("Min", "Max")),
                 y =
                 list(alternating = FALSE, axs = "i", tck = 0,
                      at = seq_len(ncol(x)), labels = varnames))
        if (!horizontal.axis) names(default.scales) <- c("y", "x")
    }
    
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
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        foo$x.scales$log <- FALSE
        ## This is because No further changes will be
        ## necessary while printing since x-axes are not
        ## marked (many x axes)
    }
    if (have.ylog) {
        warning("cannot have log y-scale")
        foo$y.scales$log <- FALSE
    }
    
    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(z = x, groups = groups, varnames = varnames), dots)


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

        foo$panel.args[[packet.number]] <-
            list(subscripts = subscr[id])

        cond.current.level <-
            cupdate(cond.current.level,
                    cond.max.level)
    }

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
                                      points = FALSE,
                                      rectangles = FALSE,
                                      lines = TRUE), 
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




parallel <- function(x, data, ...)
{
    .Deprecated("parallelplot")
    ## ccall <- match.call()
    ## ccall[[1]] <- quote(lattice::parallelplot)
    ## eval.parent(ccall)
    UseMethod("parallel")
}


parallel.formula <- parallelplot.formula
parallel.matrix <- parallelplot.matrix
parallel.data.frame <- parallelplot.data.frame


