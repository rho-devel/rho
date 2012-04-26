

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
### Copyright (C) 2007 Felix Andrews <felix@nfrac.org> 
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




### the code here deals with interacting (via grid viewports) with a
### lattice plot after it is made




## utility used in panel.identify

getTextPosition <- function(x, y)
    ## returns position 1: below, 2: left, 3: above, 4: right (w.r.t
    ## origin).  Used as a tool in panel.identify.
{
    a <- abs(c(x, y))
    if (y <= 0 && a[1] <= -y) 1
    else if (x <= 0 && a[2] <= -x) 2
    else if (y >= 0 && a[1] <= y) 3
    else if (x >= 0 && a[2] <= x) 4
}



panel.identify <-
    function(x, y = NULL,
             subscripts = seq_along(x),
             labels = subscripts, 
             n = length(x), offset = 0.5,
             threshold = 18, ## in points, roughly 0.25 inches
             panel.args = trellis.panelArgs(),
             ...)
    ## ... goes to ltext
{
    if (missing(x))
    {
        x <- panel.args$x
        y <- panel.args$y
        if (missing(subscripts) && !is.null(panel.args$subscripts))
            subscripts <- panel.args$subscripts
    }
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    labels <- as.character(labels)
    if (length(labels) > length(subscripts))
        labels <- labels[subscripts]

    unmarked <- rep(TRUE, length(x))
    count <- 0

    while (count < n)
    {
        ll <- grid.locator(unit = "points")
        if (is.null(ll)) break ## non-left click
        lx <- convertX(ll$x, "points", TRUE)
        ly <- convertY(ll$y, "points", TRUE)
        pdists <- sqrt((px - lx)^2 + (py - ly)^2)
        if (min(pdists, na.rm = TRUE) > threshold)
            warning("no observations within ", threshold, " points")
        else
        {
            w <- which.min(pdists)
            if (unmarked[w])
            {
                pos <- getTextPosition(x = lx - px[w], y = ly - py[w])
                ltext(x[w], y[w], labels[w], pos = pos, offset = offset, ...,
                      identifier = "identify")
                unmarked[w] <- FALSE
                count <- count + 1
            }
            else
                warning("nearest observation already identified")
        }
    }
    subscripts[!unmarked]
}



## identify for cloud().  This is a lot more complicated because we
## need to redo all the projection calculations.  The first bit is a
## function that separates out the panel.identify() functionality.


panel.3didentify <-
    function(x, y, z, rot.mat = diag(4), distance,
             xlim.scaled,
             ylim.scaled,
             zlim.scaled,
             subscripts = seq_along(x),
             labels = subscripts, 
             n = length(x), offset = 0.5,
             threshold = 18, ## in points, roughly 0.25 inches
             ...)
    ## ... goes to ltext
{
    ## 2-D projection
    id <- ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) &
           (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) &
           (z >= zlim.scaled[1]) & (z <= zlim.scaled[2]) &
           !is.na(x) & !is.na(y) & !is.na(z))
    m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
    x <- m[1, ]
    y <- m[2, ]

    ## rest is like panel.identify
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    is.na(px) <- is.na(py) <- !id # make these unavailable
    labels <- as.character(labels)
    if (length(labels) > length(subscripts))
        labels <- labels[subscripts]

    unmarked <- rep(TRUE, length(x))
    count <- 0

    while (count < n)
    {
        ll <- grid.locator(unit = "points")
        if (is.null(ll)) break ## non-left click
        lx <- convertX(ll$x, "points", TRUE)
        ly <- convertY(ll$y, "points", TRUE)
        pdists <- sqrt((px - lx)^2 + (py - ly)^2)
        if (min(pdists, na.rm = TRUE) > threshold)
            warning("no observations within ", threshold, " points")
        else
        {
            w <- which.min(pdists)
            if (unmarked[w])
            {
                pos <- getTextPosition(x = lx - px[w], y = ly - py[w])
                ltext(x[w], y[w], labels[w], pos = pos, offset = offset, ...)
                unmarked[w] <- FALSE
                count <- count + 1
            }
            else
                warning("nearest observation already identified")
        }
    }
    subscripts[!unmarked]
}



## The more involved part is the wrapper that computes the projections
## etc.


panel.identify.cloud <-
    function(x = panel.args$x,
             y = panel.args$y,
             z = panel.args$z,
             subscripts = panel.args$subscripts,

             perspective = TRUE,
             distance = if (perspective) 0.2 else 0, 
             xlim = panel.args$xlim,
             ylim = panel.args$ylim,
             zlim = panel.args$zlim,

             screen = list(z = 40, x = -60),
             R.mat = diag(4),
             aspect = c(1, 1),

             scales.3d = panel.args$scales.3d,
             ...,

             panel.3d.identify = panel.3didentify,
             n = length(subscripts),
             offset = 0.5,
             threshold = 18, ## in points, roughly 0.25 inches
             labels = subscripts,
             panel.args = trellis.panelArgs())
{
    argOrDefault <- function(arg) {
        if (is.null(panel.args[[arg]])) get(arg) # default
        else panel.args[[arg]]
    }
    if (missing(perspective)) perspective <- argOrDefault("perspective")
    if (missing(distance)) distance <- argOrDefault("distance")
    if (missing(screen)) screen <- argOrDefault("screen")
    if (missing(R.mat)) R.mat <- argOrDefault("R.mat")
    if (missing(aspect)) aspect <- argOrDefault("aspect")
    ## if (missing())  <- argOrDefault("")

    if (is.factor(x)) x <- as.numeric(x)
    if (is.factor(y)) y <- as.numeric(y)
    if (is.factor(z)) z <- as.numeric(z)

    ## calculate rotation matrix:
    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)

    if (length(subscripts) == 0)  ## nothing to do
        return (integer(0))
    
    xlabelinfo <-
        calculateAxisComponents(xlim,
                                at = scales.3d$x$at,
                                num.limit = NULL,
                                labels = scales.3d$x$labels,
                                logsc = scales.3d$x$log,
                                abbreviate = scales.3d$x$abbreviate,
                                minlength = scales.3d$x$minlength,
                                format.posixt = scales.3d$x$format,
                                n = scales.3d$x$tick.number)

    ylabelinfo <-
        calculateAxisComponents(ylim,
                                at = scales.3d$y$at,
                                num.limit = NULL,
                                labels = scales.3d$y$labels,
                                logsc = scales.3d$y$log,
                                abbreviate = scales.3d$y$abbreviate,
                                minlength = scales.3d$y$minlength,
                                format.posixt = scales.3d$y$format,
                                n = scales.3d$y$tick.number)

    zlabelinfo <-
        calculateAxisComponents(zlim,
                                at = scales.3d$z$at,
                                num.limit = NULL,
                                labels = scales.3d$z$labels,
                                logsc = scales.3d$z$log,
                                abbreviate = scales.3d$z$abbreviate,
                                minlength = scales.3d$z$minlength,
                                format.posixt = scales.3d$z$format,
                                n = scales.3d$z$tick.number)

    xlim <- xlabelinfo$num.limit
    ylim <- ylabelinfo$num.limit
    zlim <- zlabelinfo$num.limit
    aspect <- rep(aspect, length.out = 2)
    x <- x[subscripts]
    y <- y[subscripts]
    z <- z[subscripts]
    corners <-
        data.frame(x = c(-1, 1, 1,-1,-1, 1, 1,-1),
                   y = c(-1,-1,-1,-1, 1, 1, 1, 1) * aspect[1],
                   z = c(-1,-1, 1, 1,-1,-1, 1, 1) * aspect[2])
    corners <- corners / (2 * max(corners)) ## contain in [-.5, .5] cube
    xlim.scaled <- range(corners$x)
    ylim.scaled <- range(corners$y)
    zlim.scaled <- range(corners$z)
    ## box ranges and lengths
    cmin <- lapply(corners, min)
    cmax <- lapply(corners, max)
    clen <- lapply(corners, function(x) diff(range(x, finite = TRUE)))
    ## scaled (to bounding box) data
    x <- cmin$x + clen$x * (x-xlim[1])/diff(xlim)
    y <- cmin$y + clen$y * (y-ylim[1])/diff(ylim)
    z <- cmin$z + clen$z * (z-zlim[1])/diff(zlim)

    panel.3d.identify(x, y, z,
                      rot.mat = rot.mat,
                      distance = distance,
                      xlim.scaled = xlim.scaled,
                      ylim.scaled = ylim.scaled,
                      zlim.scaled = zlim.scaled,
                      subscripts = subscripts,
                      labels = labels, 
                      n = length(x), offset = 0.5,
                      threshold = 18,
                      ...)
}



trellis.vpname <-
    function(name = 
             c("position", "split", "split.location", "toplevel", "figure",
               "panel", "strip", "strip.left", "legend", "legend.region",
               "main", "sub", "xlab", "ylab", "xlab.top", "ylab.right",
               "page"),
             column = lattice.getStatus("current.focus.column", prefix = prefix),
             row = lattice.getStatus("current.focus.row", prefix = prefix),
             side = c("left", "top", "right", "bottom", "inside"),
             clip.off = FALSE,
             prefix = lattice.getStatus("current.prefix"))
{
    name <- match.arg(name)
    side <- match.arg(side)

    paste(prefix, 
          switch(name,

                 position = "position.vp",
                 split = "split.vp",
                 split.location = "split.location.vp",
                 toplevel = "toplevel.vp",
                 figure = "figure.vp",

                 xlab = "xlab.vp",
                 ylab = "ylab.vp",
                 main = "main.vp",
                 sub  = "sub.vp",
                 xlab.top = "xlab.top.vp",
                 ylab.right = "ylab.right.vp",

                 panel =
                 if (clip.off) paste("panel", column, row, "off", "vp",  sep = ".")
                 else paste("panel", column, row, "vp", sep = "."), 

                 strip = 
                 if (clip.off) paste("strip", column, row, "off", "vp", sep = ".")
                 else paste("strip", column, row, "vp", sep = "."), 

                 strip.left =
                 if (clip.off) paste("strip.left", column, row, "off", "vp", sep = ".")
                 else paste("strip.left", column, row, "vp", sep = "."), 

                 legend = paste("legend", side, "vp", sep = "."),
                 legend.region = "legend.region.vp"),
          sep = ".")
}



trellis.grobname <-
    function(name,
             type = c("", "panel", "strip", "strip.left", "key", "colorkey"),
             group = 0,
             which.given = lattice.getStatus("current.which.given",
               prefix = prefix),
             which.panel = lattice.getStatus("current.which.panel",
               prefix = prefix),
             column = lattice.getStatus("current.focus.column",
               prefix = prefix),
             row = lattice.getStatus("current.focus.row",
               prefix = prefix),
             prefix = lattice.getStatus("current.prefix"))
{
    stripname <- function(striplab,
                          name, column, row, which.given, which.panel) {
        if (length(which.panel) > 1) {
            paste(name, "given", which.given, striplab, 
                  column, row, sep=".")            
        } else {
            paste(name, striplab, column, row, sep=".")
        }
    }
    if (group > 0)
        name <- paste(name, "group", group, sep=".")
    paste(prefix,
          switch(type,
                 panel=paste(name, type, column, row, sep="."),
                 strip=,
                 strip.left=stripname(type, name, column, row,
                   which.given, which.panel),
                 key=,
                 colorkey=paste(type, name, sep="."),
                 name),
          sep = ".")
}


### Primary user-level interface to `focus' on viewports created by
### plot.trellis(), so that existing plots can be augmented.  Multiple
### plots can exist on a given page, and all of them are usually
### accessible using different 'prefix' argument.  To keep code
### simple, many utility functions related to interaction assume that
### lattice.getStatus("current.prefix") gives the correct prefix when
### they are called.  Only trellis.focus() allows an explicit 'prefix'
### argument, and sets 'current.prefix' to 'prefix' when it is called.
### To switch to a not-currently-active trellis plot, one must thus
### use trellis.focus() before any other functions.


trellis.focus <-
    function(name,
             column = stop("column must be specified"),
             row = stop("row must be specified"),
             side = NULL,
             clip.off = FALSE,
             highlight = interactive(),
             ...,
             prefix,
             guess = TRUE,
             verbose = getOption("verbose"))
{
    trellis.unfocus()
    if (!missing(prefix)) lattice.setStatus(current.prefix = prefix)
    else prefix <- lattice.getStatus("current.prefix")
    
    if (missing(name) && missing(column) && missing(row))
        return(trellis.clickFocus(clip.off = clip.off,
                                  highlight = highlight,
                                  ...,
                                  guess = guess,
                                  verbose = verbose))

    if (name %in% c("panel", "strip", "strip.left"))
    {
        ll <- lattice.getStatus("current.panel.positions", prefix = prefix)
        if (column > 0 && row > 0 &&
            column <= ncol(ll) && row <= nrow(ll) &&
            ll[row, column] > 0) ## to disallow empty positions
        {
            lattice.setStatus(current.focus.column = column,
                              current.focus.row = row,
                              prefix = prefix)
        }
        else
            stop("panel position unspecified or invalid")
    }
    else ## this is for calls from trellis.switchFocus
    {
        if (!missing(row))
            lattice.setStatus(current.focus.row = row, prefix = prefix)
        if (!missing(column))
            lattice.setStatus(current.focus.column = column, prefix = prefix)
    }
    lattice.setStatus(vp.depth = downViewport(trellis.vpname(name, side = side,
                                                             clip.off = clip.off,
                                                             prefix = prefix)),
                      prefix = prefix)
    if (highlight)
    {
        lattice.setStatus(vp.highlighted = TRUE, prefix = prefix)
        gp <- do.call("gpar",
                      updateList(lattice.getOption("highlight.gpar"),
                                 list(...)))
        lvp <- rectGrob(name = "lvp.highlight", gp = gp)
        grid.draw(lvp)
    }
    else
    {
        lattice.setStatus(vp.highlighted = FALSE, prefix = prefix)
    }
    invisible()
}



trellis.switchFocus <-
    function(name,
             side = NULL,
             clip.off = FALSE,
             highlight,
             ...,
             prefix)
{
    if (!missing(prefix) &&  prefix != lattice.getStatus("current.prefix"))
        stop("'trellis.switchFocus' cannot be used to switch to a different 'prefix'.  Use 'trellis.focus' first")
    prefix <- lattice.getStatus("current.prefix")
    row <- lattice.getStatus("current.focus.row", prefix = prefix)
    column <- lattice.getStatus("current.focus.column", prefix = prefix)
    if (missing(highlight))
        highlight <- lattice.getStatus("vp.highlighted", prefix = prefix)

    ## have to evaluate these explicitly to avoid lazy evaluation
    ## inside trellis.focus

    trellis.focus(name = name,
                  row = row,
                  column = column,
                  side = side, clip.off = clip.off,
                  highlight = highlight,
                  ...)
}



trellis.unfocus <- function()
    ## mainly, undo highlighting
{
    prefix <- lattice.getStatus("current.prefix")
    if (lattice.getStatus("vp.highlighted", prefix = prefix))
    {
        grid.remove("lvp.highlight", warn = FALSE)
        lattice.setStatus(vp.highlighted = FALSE, prefix = prefix)
    }
    lattice.setStatus(current.focus.column = 0,
                      current.focus.row = 0,
                      prefix = prefix)
    if (lattice.getStatus("vp.depth", prefix = prefix) > 0)
        upViewport(lattice.getStatus("vp.depth", prefix = prefix))
    lattice.setStatus(vp.depth = 0, prefix = prefix)
    invisible()
}


trellis.panelArgs <-
    function(x, packet.number)
{
    if (missing(x))
    {
        x <- trellis.last.object()
        if (is.null(x))
            stop("Plot object was not saved, cannot retrieve panel data")
        if (lattice.getStatus("current.plot.multipage",
                              prefix = lattice.getStatus("current.prefix")))
            warning("Plot spans multiple pages, only last page can be updated")
    }
    if (missing(packet.number))
    {
        ## FIXME: workaround for unfortunate choice of names.  May
        ## require more extensive changes

        ## pn <- get("packet.number", mode = "function")
        ## packet.number <- pn()

        ## How about this?
        packet.number <- lattice::packet.number()
    }
    if (!length(packet.number)) ## should be 0x0 matrix otherwise
        stop("You have to first select a panel using trellis.focus()")
    c(x$panel.args[[packet.number]], x$panel.args.common)
}



### trellis.clickFocus() and panel.identify.qqmath() are based on
### contributions by Felix Andrews <felix@nfrac.org> (2007/06/21)

### Click on a panel to focus on it.  trellis.clickFocus() is not
### exported, but used by trellis.focus() when 'name' etc. is missing.


trellis.clickFocus <-
    function(clip.off = FALSE,
             highlight = interactive(),
             ...,
             guess = TRUE,
             verbose = TRUE)
{
    layoutMatrix <- trellis.currentLayout()
    if (guess && sum(layoutMatrix > 0) == 1)
    {
        ## there's only one panel, so just select it
        w <- which(layoutMatrix > 0)
        focusRow <- row(layoutMatrix)[w]
        focusCol <- col(layoutMatrix)[w]
        if (verbose) message(sprintf("Selecting panel at position (%g, %g)", focusRow, focusCol))
    }
    else if (all(layoutMatrix == 0))
    {
        warning("No panels available")
        return()
    }
    else 
    {
        if (verbose) message("Click on panel to focus")
        trellis.focus("figure", highlight = FALSE)

        clickLoc <- grid.locator("npc")
        if (is.null(clickLoc)) return()
        focusCol <- ceiling(as.numeric(clickLoc$x) * ncol(layoutMatrix))
        focusRow <- ceiling(as.numeric(clickLoc$y) * nrow(layoutMatrix))
        if (lattice.getStatus("as.table", prefix = lattice.getStatus("current.prefix")))
            focusRow <- nrow(layoutMatrix) - focusRow + 1
        trellis.unfocus()
    }
    if ((focusCol >= 1) && (focusCol <= ncol(layoutMatrix)) &&
        (focusRow >= 1) && (focusRow <= nrow(layoutMatrix)) &&
        layoutMatrix[focusRow, focusCol] > 0)
    {
        trellis.focus("panel", column = focusCol, row = focusRow,
                      clip.off = clip.off, highlight = highlight,
                      ...)
    }
    else
    {
        focusCol <- focusRow <- 0
    }
    invisible(list(col=focusCol, row=focusRow))
}


## old version: doesn't work with aspect != "fill"

## trellis.clickFocus <-
##     function(clip.off = FALSE,
##              highlight = interactive(),
##              ...,
##              guess = TRUE,
##              verbose = TRUE)
## {
##     layoutMatrix <- trellis.currentLayout()
##     if (guess && sum(layoutMatrix > 0) == 1)
##     {
##         ## there's only one panel, so just select it
##         w <- which(layoutMatrix > 0)
##         focusRow <- row(layoutMatrix)[w]
##         focusCol <- col(layoutMatrix)[w]
##         if (verbose) message(sprintf("Selecting panel at position (%g, %g)", focusRow, focusCol))
##     }
##     else if (all(layoutMatrix == 0))
##     {
##         warning("No panels available")
##         return()
##     }
##     else 
##     {
##         if (verbose) message("Click on panel to focus")
##         ## trellis.focus("toplevel", highlight = FALSE)
##         glayout <- lattice.getStatus("layout.details")
##         rowRange <- range(glayout$pos.heights$panel, glayout$pos.heights$strip)
##         colRange <- range(glayout$pos.widths$panel, glayout$pos.widths$strip.left)
##         layCols <-  glayout$page.layout$ncol
##         layRows <- glayout$page.layout$nrow
##         leftPad <- convertX(sum(glayout$page.layout$widths[1:(colRange[1]-1)]), "npc", valueOnly = TRUE)
##         rightPad <- convertX(sum(glayout$page.layout$widths[(colRange[2]+1):layCols]), "npc", valueOnly = TRUE)
##         topPad <- convertY(sum(glayout$page.layout$heights[1:(rowRange[1]-1)]), "npc", valueOnly = TRUE)
##         botPad <- convertY(sum(glayout$page.layout$heights[(rowRange[2]+1):layRows]), "npc", valueOnly = TRUE)
##         clickLoc <- grid.locator("npc")
##         if (is.null(clickLoc)) return()
##         clickXScaled <- (as.numeric(clickLoc$x) - leftPad) / (1 - leftPad - rightPad)
##         focusCol <- ceiling(clickXScaled * ncol(layoutMatrix))
##         clickYScaled <- (as.numeric(clickLoc$y) - botPad) / (1 - botPad - topPad)
##         focusRow <- ceiling(clickYScaled * nrow(layoutMatrix))
##         if (lattice.getStatus("as.table")) focusRow <- nrow(layoutMatrix) - focusRow + 1
##     }
##     if ((focusCol >= 1) && (focusCol <= ncol(layoutMatrix)) &&
##         (focusRow >= 1) && (focusRow <= nrow(layoutMatrix)) &&
##         layoutMatrix[focusRow, focusCol] > 0)
##     {
##         trellis.focus("panel", column = focusCol, row = focusRow,
##                       clip.off = clip.off, highlight = highlight,
##                       ...)
##     }
##     else
##     {
##         focusCol <- focusRow <- 0
##     }
##     invisible(list(col=focusCol, row=focusRow))
## }






### wrapper around panel.identify meant to work with qqmath.

panel.identify.qqmath <-
    function(x = panel.args$x,
             distribution = panel.args$distribution,
             groups = panel.args$groups, 
             subscripts = panel.args$subscripts,
             labels = subscripts, 
             panel.args = trellis.panelArgs(),
             ...)
{
    x <- as.numeric(x)
    if (is.null(subscripts)) subscripts <- seq_along(x)
    labels <- as.character(labels)
    if (length(labels) > length(subscripts))
        labels <- labels[subscripts]
    if (!is.null(panel.args$f.value)) warning("'f.value' not supported; ignoring")
    distribution <- getFunctionOrName(distribution)
    ## compute quantiles corresponding to given vector, possibly
    ## containing NA's.  The return value must correspond to the
    ## original order
    getq <- function(x)
    {
        ans <- x
        id <- !is.na(x)
        ord <- order(x[id])
        if (any(id)) ans[id] <- distribution(ppoints(sum(id)))[order(ord)]
        ans
    }
    if (is.null(groups))
    {
        ## panel.points(x = getq(x), y = x, pch = ".", col = "red", cex = 3)
        panel.identify(x = getq(x), y = x, labels = labels, ...)
    }
    else
    {
        allq <- rep(NA_real_, length(x))
        subg <- groups[subscripts]
        vals <- if (is.factor(groups)) levels(groups) else sort(unique(groups))
        for (i in seq_along(vals))
        {
            ok <- !is.na(subg) & (subg == vals[i])
            allq[ok] <- getq(x[ok])
        }
        panel.identify(x = allq, y = x, labels = labels, ...)
    }
}




### `link' for splom


panel.link.splom <-
    function(threshold = 18, verbose = getOption("verbose"), ...)
{
    ans <- numeric(0)
    repeat {
        new <- splom.linkPoint(threshold = threshold, verbose = verbose, ...)
        if (is.null(new)) break
        else ans[length(ans) + 1] <- new
    }
    ans
}

panel.brush.splom <- panel.link.splom # for back-compatibility


splom.linkPoint <-
    function(pargs = trellis.panelArgs(),
             threshold = 18,
             col = 'black', pch = 16, cex = 0.8, ...,
             verbose = getOption("verbose"))
{
    if (verbose) message("Click to choose one point to highlight")
    ll <- grid.locator(unit = "npc")
    if (is.null(ll)) return(NULL)
    nvars <- length(pargs$z)
    ## which subpanel
    colpos <- ceiling(convertUnit(ll$x, "npc", valueOnly = TRUE) * nvars)
    rowpos <- ceiling(convertUnit(ll$y, "npc", valueOnly = TRUE) * nvars)
    if (rowpos == colpos) return(numeric(0))
    subpanel.name <- paste("subpanel", colpos, rowpos, sep = ".")
    ## coordinates of click in subpanel
    ll$x <- nvars * (ll$x - unit((colpos-1) / nvars, "npc"))
    ll$y <- nvars * (ll$y - unit((rowpos-1) / nvars, "npc"))
    ## get to that viewport, so we can convert units
    depth <- downViewport(subpanel.name)
    xnative <- convertX(ll$x, "native", TRUE)
    ynative <- convertY(ll$y, "native", TRUE)
    ## find nearest point in data (replicate steps in panel.identify)
    xpoints <- convertX(unit(xnative, "native"), "points", TRUE)
    ypoints <- convertY(unit(ynative, "native"), "points", TRUE)
    data.xp <- convertX(unit(pargs$z[, colpos], "native"), "points", TRUE)
    data.yp <- convertY(unit(pargs$z[, rowpos], "native"), "points", TRUE)
    pdists <- sqrt((data.xp - xpoints)^2 + (data.yp - ypoints)^2)
    if (min(pdists, na.rm = TRUE) > threshold)
    {
        if (verbose) warning("no points within ", threshold, " points of click")
        upViewport(depth)
        return(numeric(0))
    }
    else
    {
        w <- which.min(pdists)
        if (verbose) print(pargs$z[w,])
        upViewport(depth)
        for (row in 1:nvars)
        for (column in 1:nvars)
            if (row != column)
            {
                subpanel.name <-
                    paste("subpanel",
                          column, row, sep = ".")
                depth <- downViewport(subpanel.name)
                panel.points(x = pargs$z[w, column],
                             y = pargs$z[w, row],
                             pch = pch, col = col, cex = cex,
                             ...,
                             identifier = "link")
                upViewport(depth)
            }
        return(w)
    }
}

