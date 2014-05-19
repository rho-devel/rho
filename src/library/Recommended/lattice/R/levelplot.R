

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




level.colors <- function(x, at, col.regions, colors = TRUE, ...)
{
    ind.col <- cut(x, at, include.lowest = TRUE, labels = FALSE)
    if (!colors) 
        ind.col
    else 
    {
        if (missing(col.regions)) col.regions <- trellis.par.get("regions")$col
        nregions <- length(at) - 1
        if (is.function(col.regions)) col.regions <- col.regions(nregions)
        ncolor <- length(col.regions)
        col.regions <-
            if (ncolor <= nregions)
                rep(col.regions, length.out = nregions)
            else 
                col.regions[round(seq(1, ncolor, length.out = nregions))]
        col.regions[ind.col]
    }
}




prepanel.default.levelplot <-
    function(x, y, subscripts, ...)
{
    pad <- lattice.getOption("axis.padding")$numeric
    if (length(subscripts) > 0)
    {
        x <- x[subscripts]
        y <- y[subscripts]

        if (!is.factor(x)) ## (is.numeric(x)) 
        {
            ux <- sort(unique(x[is.finite(x)]))

            if ((ulen <- length(ux)) < 2) xlim <- ux + c(-1, 1)
            else
            {
                ## need to be careful here for DateTime classes
                diffs <- diff(as.numeric(ux))[c(1, ulen-1)]
                xlim <- c(ux[1] - diffs[1] / 2,
                          ux[ulen] + diffs[2] / 2)
            }
        }

        if (!is.factor(y)) ## (is.numeric(y))
        {
            uy <- sort(unique(y[is.finite(y)]))

            if ((ulen <- length(uy)) < 2) ylim <- uy + c(-1, 1)
            else
            {
                ## need to be careful here for DateTime classes
                diffs <- diff(as.numeric(uy))[c(1, ulen-1)]
                ylim <- c(uy[1] - diffs[1] / 2,
                          uy[ulen] + diffs[2] / 2)
            }
        }
        list(xlim =
             if (!is.factor(x)) { ## (is.numeric(x))
                 extend.limits(xlim, prop = -pad/(1 + 2*pad))
             } ##                    ^^ these get extended back later
             else levels(x),

             ylim = if (!is.factor(y)) { ## (is.numeric(y))
                 extend.limits(ylim, prop = -pad/(1 + 2*pad))
             }
             else levels(y),

             dx = if (is.numeric(x)) length(ux) else 1,
             dy = if (is.numeric(y)) length(uy) else 1)
    }
    else
        prepanel.null()
}
    


panel.contourplot <- function(...) panel.levelplot(...)


## version using contourLines, and hopefully works for missing matrix
## entries as well

panel.levelplot <-
    function(x, y, z, 
             subscripts,
             at = pretty(z),
             shrink,
             labels = FALSE,
             label.style = c("mixed", "flat", "align"),
             contour = FALSE,
             region = TRUE,
             col = add.line$col,
             lty = add.line$lty,
             lwd = add.line$lwd,
             border = "transparent",
             border.lty = 1,
             border.lwd = 0.1,
             ...,
             col.regions = regions$col,
             alpha.regions = regions$alpha,
             identifier = "levelplot")
{
    if (length(subscripts) == 0) return()
    regions <- trellis.par.get("regions")
    label.style <- match.arg(label.style)
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)

##     numcol <- length(at) - 1
##     numcol.r <- length(col.regions)
##     col.regions <-
##         if (numcol.r <= numcol)
##             rep(col.regions, length.out = numcol)
##         else
##             col.regions[round(seq(1, numcol.r, length.out = numcol))]
##     zcol <- cut(z, at, include.lowest = TRUE, labels = FALSE)

    zcol <-
        if (region) level.colors(z, at, col.regions, colors = TRUE)
        else "transparent"

    x <- x[subscripts]
    y <- y[subscripts]
    minXwid <- if (length(unique(x)) > 1) min(diff(sort(unique(x)))) else 1
    minYwid <- if (length(unique(y)) > 1) min(diff(sort(unique(y)))) else 1
    fullZrange <- range(as.numeric(z), finite = TRUE) # for shrinking
    z <- z[subscripts]
    if (region) zcol <- zcol[subscripts]

    if (hasGroupNumber())
        group <- list(...)$group.number
    else
        group <- 0

    ## Do we need a zlim-like argument ?

    shrinkx <- c(1, 1)
    shrinky <- c(1, 1)
    if (!missing(shrink)) {
        if (is.numeric(shrink)) {
            shrinkx <- rep(shrink, length.out = 2)
            shrinky <- rep(shrink, length.out = 2)
        }
        else if (is.list(shrink)) {
            shrinkx <- rep(shrink[[1]], length.out = 2)
            shrinky <- rep(shrink[[1]], length.out = 2)
            if ("x" %in% names(shrink)) shrinkx <- rep(shrink$x, length.out = 2)
            if ("y" %in% names(shrink)) shrinky <- rep(shrink$y, length.out = 2)
        }
        else warning("Invalid 'shrink' parameter ignored")
    }

    scaleWidth <- function(z, min = .8, max = .8, zl = range(z, finite = TRUE)) {
        if (diff(zl) == 0) rep(.5 * (min + max), length(z))
        else min + (max - min) * (z - zl[1]) / diff(zl)
    }

    if (x.is.factor)
    {
        ## unique values
        ux <- sort(unique(x[!is.na(x)]))
        ## dimension of rectangles
        lx <- rep(1, length(ux))
        ## centers of rectangles
        cx <- ux
    }
    else
    {
        ## sorted unique values of x 
        ux <- sort(unique(x[!is.na(x)]))
        ## actual box boundaries (x axis)
        bx <-
            if (length(ux) > 1)
                c(3 * ux[1] - ux[2],
                  ux[-length(ux)] + ux[-1],
                  3 * ux[length(ux)] - ux[length(ux)-1]) / 2
            else
                ux + c(-.5, .5) * minXwid
        ## dimension of rectangles
        lx <- diff(bx)
        ## centers of rectangles
        cx <- (bx[-1] + bx[-length(bx)])/2
    }

    ## same things for y
    if (y.is.factor)
    {
        ## unique values
        uy <- sort(unique(y[!is.na(y)]))
        ## dimension of rectangles
        ly <- rep(1, length(uy))
        ## centers of rectangles
        cy <- uy
    }
    else
    {
        uy <- sort(unique(y[!is.na(y)]))
        by <-
            if (length(uy) > 1)
                c(3 * uy[1] - uy[2],
                  uy[-length(uy)] + uy[-1],
                  3 * uy[length(uy)] - uy[length(uy)-1]) / 2
            else
                uy + c(-.5, .5) * minYwid
        ly <- diff(by)
        cy <- (by[-1] + by[-length(by)])/2
    }

    idx <- match(x, ux)
    idy <- match(y, uy)

    if (region) 
        grid.rect(x = cx[idx],
                  y = cy[idy],
                  width = lx[idx] * scaleWidth(z, shrinkx[1], shrinkx[2], fullZrange),
                  height = ly[idy] * scaleWidth(z, shrinky[1], shrinky[2], fullZrange),
                  default.units = "native",
                  name = trellis.grobname(paste(identifier, "rect", sep="."),
                    type = "panel", group = group),
                  gp =
                  gpar(fill = zcol,
                       col = border,
                       lwd = border.lwd,
                       lty = border.lty,
                       alpha = alpha.regions))

    if (contour)
    {
        ## calculate aspect ratio of panel to use in calculating label alignment
        cpl <- current.panel.limits(unit="cm")
        asp <- diff(cpl$ylim) / diff(cpl$xlim)

        ## Processing the labels argument
        if (is.logical(labels) && !labels) labels <- NULL
        else
        {
            if (is.characterOrExpression(labels)) labels <- list(labels = labels)
            text <- trellis.par.get("add.text")
            tmp <- list(col = text$col,
                        alpha = text$alpha,
                        cex = text$cex,
                        fontfamily = text$fontfamily,
                        fontface = text$fontface,
                        font = text$font)
            labels <- 
                if (is.list(labels)) updateList(tmp, labels)
                else tmp
            if (!is.characterOrExpression(labels$labels)) # NULL/TRUE
                labels$labels <- format(at, trim = TRUE)
        }

        add.line <- trellis.par.get("add.line")

        ## convert z into a matrix, with NA entries for those
        ## 'missing' from data frame. There's scope for ambiguity
        ## here, which can be avoided by the user.

        m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
        m[(idy - 1) * length(ux) + idx ] <- z

        clines <-
            contourLines(x = ux, y = uy, z = m,
                         nlevels = length(at), ## necessary ?
                         levels = at)

        ccount <- 0
        
        for (val in clines) {

            ccount <- ccount + 1
            
            ## each val looks like:

            ## $ :List of 3
            ##  ..$ level: num 170
            ##  ..$ x    : num [1:21] 0.535 0.534 0.534 0.534 0.535 ...
            ##  ..$ y    : num [1:21] 0.398 0.400 0.417 0.433 0.434 ...

            ## we don't know how to leave gap in lines for labels.

            llines(val, ## hopefully $levels won't matter
                   col = col, lty = lty, lwd = lwd,
                   identifier = paste(identifier, "line", ccount,
                     sep = "."))

            ## if too small, don't add label. How small is small ?
            ## Should depend on resolution. How ?

            if (length(val$x) > 5)
            {
                if (!is.null(labels))
                {
                    slopes <- diff(val$y) / diff(val$x)
                    ## slopes[is.na(slopes)] <- 0

                    if (label.style == "flat")
                    {
                        ## draw label at 'flattest' position along contour

                        textloc <- which.min(abs(slopes))
                        rotangle <- 0
                    }
                    else if (label.style == "align")
                    {

                        ## draw label at 'deepest' position along
                        ## contour, depth being min distance to either
                        ## of the four edges, scaled appropriately

                        rx <- range(ux)
                        ry <- range(uy)
                        depth <- pmin(pmin(val$x - rx[1], rx[2] - val$x) / diff(rx), 
                                      pmin(val$y - ry[1], ry[2] - val$y) / diff(ry))
                        textloc <- min(which.max(depth), length(slopes)) 
                                        # slopes has one less entry,
                                        # and textloc indexes slopes

                        rotangle <- atan(asp * slopes[textloc] * diff(rx) / diff(ry)) * 180 / base::pi
                    }
                    else if (label.style == "mixed")
                    {

                        ## mix both. align for contours whose flattest
                        ## portion is too close to edge

                        rx <- range(ux)
                        ry <- range(uy)
                        depth <- pmin(pmin(val$x - rx[1], rx[2] - val$x) / diff(rx), 
                                      pmin(val$y - ry[1], ry[2] - val$y) / diff(ry))
                        textloc <- which.min(abs(slopes))
                        rotangle <- 0

                        if (depth[textloc] < .05 ) {
                            textloc <- min(which.max(depth), length(slopes))
                            rotangle <- atan(asp * slopes[textloc] * diff(rx) / diff(ry)) * 180 / base::pi
                        }
                    }
                    else stop("Invalid label.style")

                    i <- match(val$level, at)

                    ltext(labels$labels[i],
                          adj = c(.5, 0),
                          srt = rotangle,
                          col = labels$col,
                          alpha = labels$alpha,
                          cex = labels$cex,
                          font = labels$font,
                          fontfamily = labels$fontfamily,
                          fontface = labels$fontface,
                          x = .5 * (val$x[textloc]+val$x[textloc + 1]),
                          y = .5 * (val$y[textloc]+val$y[textloc + 1]),
                          identifier = paste(identifier, "label", ccount,
                            sep = "."))
                }
            }
        }
    }
}


contourplot <- function(x, data, ...) UseMethod("contourplot")


contourplot.table <-
    function(x, data = NULL, aspect = "iso", ..., xlim, ylim)
{
    ## see comments in levelplot.table below
    if (!missing(data)) warning("explicit 'data' specification ignored")
    dn <- dimnames(x)
    ## if (!is.null(dn))
    dimnames(x) <- lapply(dn, make.unique)
    data <- as.data.frame.table(x)
    nms <- names(data)
    freq <- which(nms == "Freq")
    nms <- nms[-freq]
    form <- sprintf("Freq ~ %s + %s", nms[1], nms[2])
    nms <- nms[-c(1, 2)]
    len <- length(nms)
    if (len > 0)
    {
        rest <- paste(nms, collapse = "+")
        form <- paste(form, rest, sep = "|")
    }
    if (missing(xlim)) xlim <- dn[[1]]
    if (missing(ylim)) ylim <- dn[[2]]
    contourplot(as.formula(form), data,
                aspect = aspect, xlim = xlim, ylim = ylim, ...)
}


contourplot.matrix <-
    function(x, data = NULL, aspect = "iso", 
             ..., xlim, ylim,
             row.values = seq_len(nrow(x)),
             column.values = seq_len(ncol(x)))
{
    stopifnot(length(row.values) == nrow(x),
              length(column.values) == ncol(x))
    if (!missing(data)) warning("explicit 'data' specification ignored")
    form <- z ~ row * column
    data <- expand.grid(row = row.values, column = column.values)
    data$z <- as.vector(as.numeric(x))
    ## if rownames/colnames are non-null, use them to label
    if (missing(xlim))
        xlim <-
            if (!is.null(rownames(x))) rownames(x)
            else range(row.values, finite = TRUE) + c(-0.5, 0.5)
    if (missing(ylim))
        ylim <-
            if (!is.null(colnames(x))) colnames(x)
            else range(column.values, finite = TRUE) + c(-0.5, 0.5)
    contourplot(form, data, aspect = aspect, xlim = xlim, ylim = ylim, ...)
}


### FIXME: This would have been cleaner, except that as.table() forces
### row and column names, whereas we would prefer numeric axes if no
### names are present.  The same point could be made for arrays
### (although what should happen to conditioning variables is less
### clear in that case), but we will ignore that issue for now.

## contourplot.matrix <- function(x, data = NULL, xlab, ylab, ...)
## {
##     if (!missing(data)) warning("explicit 'data' specification ignored")
##     dns <- names(dimnames(x))
##     if (missing(xlab))
##         xlab <- if (is.null(dns)) "row" else dns[1]
##     if (missing(ylab))
##         ylab <- if (is.null(dns)) "column" else dns[2]
##     contourplot(as.table(x), xlab = xlab, ylab = ylab, ...)
## }

contourplot.array <- function(x, data = NULL, ...)
{
    if (!missing(data)) warning("explicit 'data' specification ignored")
    contourplot(as.table(x), ...)
}


contourplot.formula <-
    function(x,
             data = NULL,
             panel = lattice.getOption("panel.contourplot"),
             default.prepanel = lattice.getOption("prepanel.default.contourplot"),
             cuts = 7,
             labels = TRUE,
             contour = TRUE,
             pretty = TRUE,
             region = FALSE,
             ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(contourplot)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$cuts <- cuts
    ccall$labels <- labels
    ccall$contour <- contour
    ccall$pretty <- pretty
    ccall$region <- region
    ccall[[1]] <- quote(lattice::levelplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}




levelplot <- function(x, data, ...) UseMethod("levelplot")

levelplot.array <- function(x, data = NULL, ...)
{
    if (!missing(data)) warning("explicit 'data' specification ignored")
    levelplot(as.table(x), ...)
}

levelplot.table <-
    function(x, data = NULL, aspect = "iso", ..., xlim, ylim)
{
    if (!missing(data)) warning("explicit 'data' specification ignored")
    dn <- dimnames(x) ## cannot be NULL for tables

    ## We don't want to collapse duplicate names. We do prefer to use
    ## original names for labeling, and we do so for xlim and ylim
    ## below, but not for conditioning variables (too much work, plus
    ## may not even be possible)

    ## if (!is.null(dn))
    dimnames(x) <- lapply(dn, make.unique)
    data <- as.data.frame.table(x)
    nms <- names(data)
    freq <- which(nms == "Freq")
    nms <- nms[-freq]
    form <- sprintf("Freq ~ %s + %s", nms[1], nms[2])
    nms <- nms[-c(1, 2)]
    len <- length(nms)
    if (len > 0)
    {
        rest <- paste(nms, collapse = "+")
        form <- paste(form, rest, sep = "|")
    }
    ## if rownames/colnames are non-null, use them to label (not the unique versions)
    if (missing(xlim)) xlim <- dn[[1]]
    if (missing(ylim)) ylim <- dn[[2]]
    levelplot(as.formula(form), data,
              aspect = aspect, xlim = xlim, ylim = ylim, ...)
}


levelplot.matrix <-
    function(x, data = NULL, aspect = "iso",
             ..., xlim, ylim,
             row.values = seq_len(nrow(x)),
             column.values = seq_len(ncol(x)))
{
    stopifnot(length(row.values) == nrow(x),
              length(column.values) == ncol(x))
    if (!missing(data)) warning("explicit 'data' specification ignored")
    form <- z ~ row * column
    data <- expand.grid(row = row.values, column = column.values)
    data$z <- as.vector(as.numeric(x))
    ## if rownames/colnames are non-null, use them to label
    if (missing(xlim))
        xlim <-
            if (!is.null(rownames(x))) rownames(x)
            else range(row.values, finite = TRUE) + c(-0.5, 0.5)
    if (missing(ylim))
        ylim <-
            if (!is.null(colnames(x))) colnames(x)
            else range(column.values, finite = TRUE) + c(-0.5, 0.5)
    levelplot(form, data, aspect = aspect, xlim = xlim, ylim = ylim, ...)
}

### See FIXME for contourplot.matrix above

## levelplot.matrix <- function(x, data = NULL, xlab, ylab, ...)
## {
##     if (!missing(data)) warning("explicit 'data' specification ignored")
##     dns <- names(dimnames(x))
##     if (missing(xlab))
##         xlab <- if (is.null(dns)) "row" else dns[1]
##     if (missing(ylab))
##         ylab <- if (is.null(dns)) "column" else dns[2]
##     levelplot(as.table(x), xlab = xlab, ylab = ylab, ...)
## }




levelplot.formula <-
    function(x,
             data = NULL,
             allow.multiple = is.null(groups) || outer,
             outer = TRUE,
             aspect = "fill",
             panel = if (useRaster) lattice.getOption("panel.levelplot.raster")
                     else lattice.getOption("panel.levelplot"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,

             ## at, region etc should be ideally in panel.levelplot only, but is needed for colorkey
             at,
             cuts = 15,
             pretty = FALSE,
             region = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             useRaster = FALSE,
             lattice.options = NULL,
             default.scales = list(),
             default.prepanel = lattice.getOption("prepanel.default.levelplot"),
             colorkey = region,
             col.regions,
             alpha.regions,
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

    ## Step 1: Evaluate x, y, z etc. and do some preprocessing

    form <-
        latticeParseFormula(formula, data, dimension = 3,
                            subset = subset, groups = groups,
                            multiple = allow.multiple,
                            outer = outer, subscripts = TRUE,
                            drop = drop.unused.levels)

    ## We need to be careful with 'subscripts' here. It HAS to be
    ## there, and it's to be used to index x, y, z (and not only
    ## groups, unlike in xyplot etc). This means we have to subset
    ## groups as well, which is about the only use for the subscripts
    ## calculated in latticeParseFormula, after which subscripts is
    ## regenerated as a straight sequence indexing the variables

    if (!is.null(form$groups))
        groups <-
            if (is.matrix(form$groups)) as.vector(form$groups)[form$subscr]
            else if (is.data.frame(form$groups)) as.vector(as.matrix(form$groups))[form$subscr]
            else form$groups[form$subscr]

    subscr <- seq_len(length(form$left))

    cond <- form$condition
    z <- form$left
    x <- form$right.x
    y <- form$right.y

    if (useRaster)
    {
        ## Does device support raster images?
        devRaster <- dev.capabilities("rasterImage")$rasterImage
        if (is.na(devRaster)) 
        {
            warning("device support for raster images unknown, ignoring 'raster=TRUE'")
            useRaster <- FALSE
        }
        else if (devRaster == "no")
        {
            warning("device has no raster support, ignoring 'raster=TRUE'")
            useRaster <- FALSE
        }
        else if (devRaster == "non-missing" && any(is.na(z)))
        {
            warning("device does not support raster images with NA, ignoring 'raster=TRUE'")
            useRaster <- FALSE
        }
    }

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)
    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }
    if (missing(xlab)) xlab <- form$right.x.name
    if (missing(ylab)) ylab <- form$right.y.name

    zrng <- extend.limits(range(as.numeric(z), finite = TRUE))
    if (missing(at))
        at <-
            if (pretty) pretty(zrng, cuts)
            else seq(zrng[1], zrng[2], length.out = cuts + 2)
    

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
                       xlab.default = form$right.x.name,
                       ylab.default = form$right.y.name,
                       lattice.options = lattice.options), dots))

    
    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(levelplot)

    ## Step 2: Compute scales.common (excluding limits)

    if (is.character (scales)) scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo,
             do.call("construct.scales", scales))


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

    ## Most levelplot/contourplot specific code here

    if (is.logical(colorkey))
    {
        if (colorkey)
        {
            colorkey <- list(at = at, space = "right")
            if (useRaster) colorkey$raster <- TRUE
            if (!missing(col.regions)) colorkey$col <- col.regions
            if (!missing(alpha.regions)) colorkey$alpha <- alpha.regions
        }
        else colorkey <- NULL
    }
    else if (is.list(colorkey))
    {
        tmp <- ## FIXME: does the inside thing work? probably not 
            list(space = if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right",
                 at = at)
        if (!missing(col.regions)) tmp$col <- col.regions
        if (!missing(alpha.regions)) tmp$alpha <- alpha.regions
        if (useRaster) tmp$raster <- TRUE
        colorkey <- updateList(tmp, colorkey)
    }
    foo$legend <-
        construct.legend(foo$legend,
                         colorkey,
                         fun = "draw.colorkey")

    ## Step 6: Determine packets

    foo$panel.args.common <-
        c(list(x = x, y = y, z = z, at = at,
               region = region), dots)
    if (!missing(col.regions)) foo$panel.args.common$col.regions <- col.regions
    if (!missing(alpha.regions)) foo$panel.args.common$alpha.regions <- alpha.regions



# ############### premature calculation of col.regions
#     ## region
#     numcol <- length(at)-1
#     numcol.r <- length(col.regions)

#     col.regions <-
#         if (numcol.r <= numcol)
#             rep(col.regions, length.out = numcol)
#         else col.regions[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]

#     if (is.logical(colorkey))
#     {
#         if (colorkey) colorkey <-
#             list(space = "right", col = col.regions,
#                  at = at, tick.number = 7)
#         else colorkey <- NULL
#     }
#     else if (is.list(colorkey))
#     {
#         #foo$colorkey <- colorkey
#         if (is.null(colorkey$col)) colorkey$col <- col.regions
#         if (is.null(colorkey$at)) colorkey$at <- at
#         if (is.null(colorkey$space)) colorkey$space <-
#             if (any(c("x", "y", "corner") %in% names(colorkey))) "inside" else "right"
#     }
#     foo$legend <-
#         construct.legend(foo$legend,
#                          colorkey,
#                          fun = "draw.colorkey")

#     zcol <- rep(NA, length(z)) #numeric(length(z))
#     for (i in seq_along(col.regions))
#         zcol[!id.na & !is.na(z) & z>=at[i] & z<at[i+1]] <- i

#     foo$panel.args.common <-
#         c(list(x=x, y=y, z=z, at=at,
#                labels=labels,
#                region = region, contour = contour,
#                zcol=zcol,
#                col.regions=col.regions),
#           dots)
# ##############################


    if (!is.null(groups)) foo$panel.args.common$groups <- groups

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

    class(foo) <- "trellis"
    foo
}


## experimental version using grid.raster (R >= 2.11.0)

panel.levelplot.raster <-
    function(x, y, z, 
             subscripts,
             at = pretty(z),
             ...,
             col.regions = regions$col,
             alpha.regions = regions$alpha,
             interpolate = FALSE,
             identifier = "levelplot")
{
    if (length(subscripts) == 0) return()
    regions <- trellis.par.get("regions")
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)
    x <- x[subscripts]
    y <- y[subscripts]
    z <- z[subscripts]
    zcol <- zcol[subscripts]

    if (hasGroupNumber())
        group <- list(...)$group.number
    else
        group <- 0

    if (x.is.factor)
    {
        ## unique values (we want to keep missing levels in between)
        ux <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE))
        xwid <- 1L
    }
    else
    {
        ## sorted unique values of x 
        ux <- sort(unique(x[!is.na(x)]))
        ## complain if all ux are not equidistant
        ## if             (length(unique(diff(ux))) != 1) -- too strict
        if (!isTRUE(all.equal(diff(range(diff(ux))), 0)))
            warning("'x' values are not equispaced; output may be wrong")
        xwid <- mean(diff(ux))
    }
    ## same things for y
    if (y.is.factor)
    {
        ux <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE))
        ywid <- 1L
    }
    else
    {
        uy <- sort(unique(y[!is.na(y)]))
        if (!isTRUE(all.equal(diff(range(diff(uy))), 0)))
            warning("'y' values are not equispaced; output may be wrong")
        ywid <- mean(diff(uy))
    }
    ncolumns <- length(ux)
    nrows <- length(uy)
    xlow <- ux[1] - 0.5 * xwid
    xhigh <- ux[ncolumns] + 0.5 * xwid
    ylow <- uy[1] - 0.5 * ywid
    yhigh <- uy[nrows] + 0.5 * ywid
    ## create a suitable matrix of colors
    zmat <- rep("transparent", ncolumns * nrows)
    idx <- match(x, ux)
    idy <- match(y, rev(uy)) # image goes top to bottom
    id <- idy + nrows * (idx-1L)
    zmat[id] <- zcol
    dim(zmat) <- c(nrows, ncolumns)
    grid.raster(as.raster(zmat), interpolate = interpolate,
                x = xlow, y = ylow,
                width = xhigh - xlow, height = yhigh - ylow,
                just = c("left", "bottom"),
                default.units = "native",
                name = trellis.grobname(paste(identifier, "raster", sep="."),
                                        type = "panel", group = group))
}


