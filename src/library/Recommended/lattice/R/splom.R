

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





prepanel.default.splom <-
    function(z, ...)
{
    list(xlim = c(.5, ncol(z) + .5),
         ylim = c(.5, ncol(z) + .5),
         dx = 1,
         dy = 1)
}

panel.splom <-
    function(...,
             identifier = "splom")
    panel.xyplot(..., identifier = identifier)


diag.panel.splom <-
    function(x = NULL,
             varname = NULL, limits, at = NULL, labels = NULL,
             draw = TRUE, tick.number = 5,

             varname.col = add.text$col,
             varname.cex = add.text$cex,
             varname.lineheight = add.text$lineheight,
             varname.font = add.text$font,
             varname.fontfamily = add.text$fontfamily,
             varname.fontface = add.text$fontface,

             axis.text.col = axis.text$col,
             axis.text.alpha = axis.text$alpha,
             axis.text.cex = axis.text$cex,
             axis.text.font = axis.text$font,
             axis.text.fontfamily = axis.text$fontfamily,
             axis.text.fontface = axis.text$fontface,
             axis.text.lineheight = axis.text$lineheight,

             axis.line.col = axis.line$col,
             axis.line.alpha = axis.line$alpha,
             axis.line.lty = axis.line$lty,
             axis.line.lwd = axis.line$lwd,
             axis.line.tck = 1,
             ...)
{
    add.text <- trellis.par.get("add.text")
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    if (!is.null(varname))
        grid.text(varname,
                  name = trellis.grobname("diag.text", type="panel"),
                  gp =
                  gpar(col = varname.col,
                       cex = varname.cex,
                       lineheight = varname.lineheight,
                       fontface = chooseFace(varname.fontface, varname.font),
                       fontfamily = varname.fontfamily))
    if (draw) ## plot axes
    {
        rot <- if (is.numeric(limits)) 0 else c(90, 0)
        axis.details <-
            formattedTicksAndLabels(limits,
                                    at = if (is.null(at)) TRUE else at,
                                    labels = if (is.null(labels)) TRUE else labels,
                                    logsc = FALSE,
                                    ..., n = tick.number)
        for (side in c("left", "top", "right", "bottom"))
            panel.axis(side = side,
                       at = axis.details$at,
                       labels = axis.details$labels,
                       check.overlap = axis.details$check.overlap,
                       ticks = TRUE,
                       half = TRUE,

                       tck = axis.line.tck,
                       rot = rot, 

                       text.col = axis.text.col,
                       text.alpha = axis.text.alpha,
                       text.cex = axis.text.cex,
                       text.font = axis.text.font,
                       text.fontfamily = axis.text.fontfamily,
                       text.fontface = axis.text.fontface,
                       text.lineheight = axis.text.lineheight,

                       line.col = axis.line.col,
                       line.alpha = axis.line.alpha,
                       line.lty = axis.line.lty,
                       line.lwd = axis.line.lwd)
    }
}






## FIXME: add alpha pars

panel.pairs <-
    function(z,
             panel = lattice.getOption("panel.splom"),
             lower.panel = panel,
             upper.panel = panel,
             diag.panel = "diag.panel.splom",
             as.matrix = FALSE,

             groups = NULL,
             panel.subscripts,
             subscripts,
             pscales = 5,

             ## prepanel.limits = function(x) if (is.factor(x)) levels(x) else
             ## extend.limits(range(as.numeric(x), finite = TRUE)),
             prepanel.limits = scale.limits,

             varnames = colnames(z),
             varname.col = add.text$col,
             varname.cex = add.text$cex,
             varname.font = add.text$font,
             varname.fontfamily = add.text$fontfamily,
             varname.fontface = add.text$fontface,

             axis.text.col = axis.text$col,
             axis.text.cex = axis.text$cex,
             axis.text.font = axis.text$font,
             axis.text.fontfamily = axis.text$fontfamily,
             axis.text.fontface = axis.text$fontface,
             axis.text.lineheight = axis.text$lineheight,

             axis.line.col = axis.line$col,
             axis.line.lty = axis.line$lty,
             axis.line.lwd = axis.line$lwd,
             axis.line.alpha = axis.line$alpha,
             axis.line.tck = 1,
             ...)
{
    lower.panel <- getFunctionOrName(lower.panel)
    upper.panel <- getFunctionOrName(upper.panel)
    diag.panel <- getFunctionOrName(diag.panel)
    add.text <- trellis.par.get("add.text")
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    n.var <- ncol(z)
    if (n.var == 0) return()

    lim <- vector("list", length = n.var)
    for (i in seq_len(n.var))
        lim[[i]] <-
            if (is.list(pscales) && !is.null(pscales[[i]]$lim))
                pscales[[i]]$lim
            else prepanel.limits(z[,i])
    
    ## maybe (ideally) this should be affected by scales

    if (length(subscripts))
    {
        draw <- is.list(pscales) || (is.numeric(pscales) && pscales!=0) # whether axes to be drawn
        splom.layout <- grid.layout(nrow = n.var, ncol = n.var)
        pushViewport(viewport(layout = splom.layout, name = "pairs"))

        for(i in 1:n.var)     ## i = row, j = col
            for(j in 1:n.var)
            {
                if (as.matrix)
                    pushViewport(viewport(layout.pos.row = i,
                                          layout.pos.col = j,
                                          name = paste("subpanel", j, i, sep = "."),
                                          clip = trellis.par.get("clip")$panel,
                                          xscale = as.numeric(extend.limits(lim[[j]])),
                                          yscale = as.numeric(extend.limits(lim[[i]]))))
                                          ## xscale = if (is.character(lim[[j]])) c(0, length(lim[[j]]) + 1)
                                          ## else as.numeric(extend.limits(lim[[j]])),
                                          ## yscale = if (is.character(lim[[i]])) c(0, length(lim[[i]]) + 1)
                                          ## else as.numeric(extend.limits(lim[[i]])) ))
                else
                    pushViewport(viewport(layout.pos.row = n.var - i + 1,
                                          layout.pos.col = j,
                                          name = paste("subpanel", j, i, sep = "."),
                                          clip = trellis.par.get("clip")$panel,
                                          xscale = as.numeric(extend.limits(lim[[j]])),
                                          yscale = as.numeric(extend.limits(lim[[i]]))))
                                          
                                          ## xscale = if (is.character(lim[[j]]))
                                          ## c(0, length(lim[[j]]) + 1) else lim[[j]],
                                          ## yscale = if (is.character(lim[[i]]))
                                          ## c(0, length(lim[[i]]) + 1) else lim[[i]]))
                if (i == j)
                {
                    ## axls <-
                    ##     if (is.list(pscales) && !is.null(pscales[[i]]$at))
                    ##         pscales[[i]]$at
                    ##     else if (is.character(lim[[i]]))
                    ##         seq_along(lim[[i]])
                    ##     else
                    ##         pretty(lim[[i]],
                    ##                n = if (is.numeric(pscales))
                    ##                pscales else 5)

                    ## labels <-
                    ##     if (is.list(pscales) && !is.null(pscales[[i]]$lab))
                    ##         pscales[[i]]$lab
                    ##     else if (is.character(lim[[i]]))
                    ##         lim[[i]]
                    ##     else
                    ##         NULL

                    ## if (is.numeric(lim[[i]]))
                    ## {
                    ##     axlims <- range(lim[[i]])
                    ##     axid <- axls > axlims[1] & axls < axlims[2]
                    ##     axls <- axls[axid]
                    ##     labels <- labels[axid]
                    ## }

                    diag.panel(x = z[subscripts, j],
                               varname = varnames[i],
                               limits = lim[[i]],
                               at = if (is.list(pscales)) pscales[[i]]$at else NULL, 
                               labels = if (is.list(pscales)) pscales[[i]]$lab else NULL,
                               draw = draw,
                               tick.number = if (is.numeric(pscales)) pscales else 5,

                               varname.col = varname.col,
                               varname.cex = varname.cex,
                               varname.font = varname.font,
                               varname.fontfamily = varname.fontfamily,
                               varname.fontface = varname.fontface,

                               axis.text.col = axis.text.col,
                               axis.text.cex = axis.text.cex,
                               axis.text.font = axis.text.font,
                               axis.text.fontfamily = axis.text.fontfamily,
                               axis.text.fontface = axis.text.fontface,
                               axis.text.lineheight = axis.text.lineheight,

                               axis.line.col = axis.line.col,
                               axis.line.lty = axis.line.lty,
                               axis.line.lwd = axis.line.lwd,
                               axis.line.alpha = axis.line.alpha,
                               axis.line.tck = axis.line.tck,

                               i = i, j= j,
                               ...)

                    grid.rect(name = trellis.grobname("pairs.border",
                                type="panel"),
                              gp =
                              gpar(col = axis.line.col,
                                   lty = axis.line.lty,
                                   lwd = axis.line.lwd,
                                   fill = "transparent"))

                }
                else
                {
                    pargs <-
                        if (!panel.subscripts)
                            c(list(x = z[subscripts, j],
                                   y = z[subscripts, i]),
                              list(...),
                              list(i = i, j = j))
                        else
                            c(list(x = z[subscripts, j],
                                   y = z[subscripts, i],
                                   groups = groups,
                                   subscripts = subscripts),
                              list(...),
                              list(i = i, j = j))

                    ## if (!("..." %in% names(formals(panel))))
                    ##     pargs <- pargs[intersect(names(pargs), names(formals(panel)))]

                    ## if (as.matrix)
                    ##     do.call(if (i > j) lower.panel else upper.panel,
                    ##             pargs)
                    ## else
                    ##     do.call(if (i < j) lower.panel else upper.panel,
                    ##             pargs)

                    if (as.matrix)
                        checkArgsAndCall(if (i > j) lower.panel else upper.panel, pargs)
                    else
                        checkArgsAndCall(if (i < j) lower.panel else upper.panel, pargs)
                    
                    grid.rect(name = trellis.grobname("pairs.border",
                                type="panel"),
                              gp =
                              gpar(col = axis.line.col,
                                   lty = axis.line.lty,
                                   lwd = axis.line.lwd,
                                   fill = "transparent"))
                }
                upViewport()
            }
        upViewport()
    }
}



splom <- function(x, data, ...) UseMethod("splom")



splom.matrix <-
splom.data.frame <-
    function(x, data = NULL, ..., groups = NULL, subset = TRUE)
{
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$x <- ~x
    ccall$data <- environment()
    ## WAS:
    ## ccall$data <- list(x = x, groups = groups, subset = subset)
    ## ccall$groups <- groups
    ## ccall$subset <- subset
    ccall[[1]] <- quote(lattice::splom)
    eval.parent(ccall)
}



splom.formula <-
    function(x,
             data = NULL,
             auto.key = FALSE,
             aspect = 1,
             between = list(x = 0.5, y = 0.5),
             panel = lattice.getOption("panel.splom"),
             ## panel = if (is.null(groups)) "panel.splom" else "panel.superpose",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab = gettext("Scatter Plot Matrix"),
             xlim,
             ylab = NULL,
             ylim,
             superpanel = lattice.getOption("panel.pairs"),
             pscales = 5,
             varnames = NULL,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales = list(draw = FALSE, relation = "same", axs = "i"),
             default.prepanel = lattice.getOption("prepanel.default.splom"),
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

    if (length(cond) == 0)
    {
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
                       panel = superpanel,
                       strip = strip,
                       xlab = xlab,
                       ylab = ylab,
                       xlab.default = gettext("Scatter Plot Matrix"),
                       lattice.options = lattice.options),
                  dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(splom)

    ## Step 2: Compute scales.common (leaving out limits for now)

    ## FIXME: It is not very clear exactly what effect scales is
    ## supposed to have. Not much in Trellis (probably), but there are
    ## certain components which are definitely relevant, and certain
    ## others (like log) which can be used in innovative
    ## ways. However, I'm postponing all that to later, if at all

    if (!is.list(scales)) scales <- list()
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
    
    ## Step 4: Decide if log scales are being used (has to be NO):

    ## have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    ## have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log

    ## Step 5: Process cond

    cond.max.level <- unlist(lapply(cond, nlevels))

    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(z = x,
               panel = panel,
               panel.subscripts = TRUE,
               groups = groups, # xscales = foo$x.scales, yscales = foo$y.scales,
               varnames = varnames,
               pscales = pscales),
          dots)

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

