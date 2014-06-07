### Copyright (C) 2001-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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






prepanel.default.bwplot <-
    function(x, y, 
             horizontal = TRUE, nlevels,
             origin = NULL, stack = FALSE,
             ...)
{
    ## This function needs to work for all high level functions in the
    ## bwplot family, namely bwplot, dotplot, stripplot and
    ## barchart. For all but barchart, this is simply a question of
    ## getting the ranges. For stacked barcharts, things are slightly
    ## complicated.

    if (any(!is.na(x) & !is.na(y)))
    {
        if (horizontal)
        {
            if (!is.factor(y)) ## y came from a shingle
            {
                if (missing(nlevels)) nlevels <- length(unique(y))
                y <- factor(y, levels = 1:nlevels)
            }
            list(xlim =
                 if (stack) {
                     foo1 <-
                         if (any(x > 0))
                             range(tapply(x[x > 0], y[x > 0, drop = TRUE], sum, na.rm = TRUE), finite = TRUE) 
                         else 0
                     foo2 <-
                         if (any(x < 0))
                             range(tapply(x[x < 0], y[x < 0, drop = TRUE], sum, na.rm = TRUE), finite = TRUE) 
                         else 0
                     range(foo1, foo2)
                 }
                 ## else if (is.numeric(x)) range(x, origin, finite = TRUE)
                 ## else levels(x),
                 else scale.limits(c(x, origin)),
                 ylim = levels(y),
                 yat = sort(unique(as.numeric(y))),
                 dx = 1,
                 dy = 1)
        }
        else
        {
            if (!is.factor(x)) ## x came from a shingle
            {
                if (missing(nlevels)) nlevels <- length(unique(x))
                x <- factor(x, levels = 1:nlevels)
            }
            list(xlim = levels(x),
                 xat = sort(unique(as.numeric(x))),
                 ylim =
                 if (stack) {
                     foo1 <-
                         if (any(y > 0))
                             range(tapply(y[y > 0], x[y > 0], sum, na.rm = TRUE), finite = TRUE)
                         else 0
                     foo2 <-
                         if (any(y < 0))
                             range(tapply(y[y < 0], x[y < 0], sum, na.rm = TRUE), finite = TRUE)
                         else 0
                     range(foo1, foo2)
                 }
                 ## else if (is.numeric(y)) range(y, origin, finite = TRUE)
                 ## else levels(y),
                 else scale.limits(c(y, origin)),
                 dx = 1,
                 dy = 1)
        }
    }
    else prepanel.null()
}





panel.barchart <-
    function(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
             horizontal = TRUE,
             origin = NULL, reference = TRUE,
             stack = FALSE,
             groups = NULL, 
             col = if (is.null(groups)) plot.polygon$col else superpose.polygon$col,
             border = if (is.null(groups)) plot.polygon$border else superpose.polygon$border,
             lty = if (is.null(groups)) plot.polygon$lty else superpose.polygon$lty,
             lwd = if (is.null(groups)) plot.polygon$lwd else superpose.polygon$lwd,
             ...,
             identifier = "barchart")
{
    plot.polygon <- trellis.par.get("plot.polygon")
    superpose.polygon <- trellis.par.get("superpose.polygon")
    reference.line <- trellis.par.get("reference.line")

    ## this function doesn't have a subscripts argument (which would
    ## have made barchart always pass the subscripts to the trellis
    ## object, which is unnecessary when groups = NULL).  To work
    ## around this, we have to do some things that may seem a bit odd

    keep <- 
        (function(x, y, groups, subscripts, ...) {
            !is.na(x) & !is.na(y) &
            if (is.null(groups)) TRUE
            else !is.na(groups[subscripts])
        })(x = x, y = y, groups = groups, ...)

    if (!any(keep)) return()
    x <- as.numeric(x[keep])
    y <- as.numeric(y[keep])

    if (!is.null(groups))
    {
        groupSub <- function(groups, subscripts, ...)
            groups[subscripts[keep]]

        ## This is to make sure `levels' are calculated based on the
        ## whole groups vector and not just the values represented in
        ## this particular panel (which might make the key
        ## inconsistent and/or cause other problems)

        if (!is.factor(groups)) groups <- factor(groups)
        nvals <- nlevels(groups)
        groups <- as.numeric(groupSub(groups, ...))
    }


    if (horizontal)
    {
        ## No grouping
        if (is.null(groups))
        {
            if (is.null(origin))
            {
                origin <- current.panel.limits()$xlim[1]
                reference <- FALSE
            }
            height <- box.width # box.ratio / (1 + box.ratio)
        
            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd,
                             identifier = paste(identifier, "abline",
                               sep = "."))

            panel.rect(x = rep(origin, length(y)),
                       y = y,
                       height = rep(height, length(y)),
                       width = x - origin,
                       border = border, col = col,
                       lty = lty, lwd = lwd,
                       just = c("left", "centre"),
                       identifier = identifier)
        }

        ## grouped, with stacked bars

        else if (stack)
        {
            if (!is.null(origin) && origin != 0)
                warning("'origin' forced to 0 for stacked bars")
 
##             vals <- seq_len(nlevels(groups))
##             groups <- as.numeric(groupSub(groups, ...))
##             ## vals <- sort(unique(groups))
##             nvals <- length(vals)

            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)

            height <- box.width # box.ratio / (1 + box.ratio)

            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd,
                             identifier = paste(identifier, "abline",
                               sep = "."))

            for (i in unique(y))
            {
                ok <- y == i
                ord <- sort.list(groups[ok])
                pos <- x[ok][ord] > 0
                nok <- sum(pos, na.rm = TRUE)
                if (nok > 0)
                    panel.rect(x = cumsum(c(0, x[ok][ord][pos][-nok])),
                               y = rep(i, nok),
                               col = col[groups[ok][ord][pos]],
                               border = border[groups[ok][ord][pos]],
                               lty = lty[groups[ok][ord][pos]],
                               lwd = lwd[groups[ok][ord][pos]],
                               height = rep(height, nok),
                               width = x[ok][ord][pos],
                               just = c("left", "centre"),
                               identifier = paste(identifier, "pos", i,
                                 sep = "."))
                neg <- x[ok][ord] < 0
                nok <- sum(neg, na.rm = TRUE)
                if (nok > 0)
                    panel.rect(x = cumsum(c(0, x[ok][ord][neg][-nok])),
                               y = rep(i, nok),
                               col = col[groups[ok][ord][neg]],
                               border = border[groups[ok][ord][neg]],
                               lty = lty[groups[ok][ord][neg]],
                               lwd = lwd[groups[ok][ord][neg]],
                               height = rep(height, nok),
                               width = x[ok][ord][neg],
                               just = c("left", "centre"),
                               identifier = paste(identifier, "neg", i,
                                 sep = "."))
            }
        }

        ## grouped, with side by side bars

        else
        {
            if (is.null(origin))
            {
                origin <- current.panel.limits()$xlim[1]
                reference <- FALSE
            }
##             vals <- seq_len(nlevels(groups))
##             groups <- as.numeric(groupSub(groups, ...))
##             ## vals <- sort(unique(groups))
##             nvals <- length(vals)

            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)

            height <- box.width / nvals # box.ratio/(1 + nvals * box.ratio)
            if (reference)
                panel.abline(v = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd,
                             identifier = paste(identifier, "abline",
                               sep = "."))
            for (i in unique(y))
            {
                ok <- y == i
                nok <- sum(ok, na.rm = TRUE)
                panel.rect(x = rep(origin, nok), 
                           y = (i + height * (groups[ok] - (nvals + 1)/2)),
                           col = col[groups[ok]],
                           border = border[groups[ok]],
                           lty = lty[groups[ok]],
                           lwd = lwd[groups[ok]],
                           height = rep(height, nok),
                           width = x[ok] - origin,
                           just = c("left", "centre"),
                           identifier = paste(identifier, "y", i,
                               sep = "."))
            }
        }
    }
    
    ## if not horizontal

    else
    {
        if (is.null(groups))
        {
            if (is.null(origin))
            {
                origin <- current.panel.limits()$ylim[1]
                reference <- FALSE
            }
            width <- box.width # box.ratio/(1+box.ratio)

            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd,
                             identifier = paste(identifier, "abline",
                               sep = "."))

            panel.rect(x = x,
                       y = rep(origin, length(x)),
                       col = col, border = border,
                       lty = lty, lwd = lwd,
                       width = rep(width, length(x)),
                       height = y - origin,
                       just = c("centre", "bottom"),
                       identifier = identifier)
        }
        else if (stack)
        {

            if (!is.null(origin) && origin != 0)
                warning("'origin' forced to 0 for stacked bars")

##             vals <- seq_len(nlevels(groups))
##             groups <- as.numeric(groupSub(groups, ...))
##             ## vals <- sort(unique(groups))
##             nvals <- length(vals)

            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)

            width <- box.width # box.ratio/(1 + box.ratio)

            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd,
                             identifier = paste(identifier, "abline",
                               sep = "."))

            for (i in unique(x))
            {
                ok <- x == i
                ord <- sort.list(groups[ok])
                pos <- y[ok][ord] > 0
                nok <- sum(pos, na.rm = TRUE)
                if (nok > 0)
                    panel.rect(x = rep(i, nok),
                               y = cumsum(c(0, y[ok][ord][pos][-nok])),
                               col = col[groups[ok][ord][pos]],
                               border = border[groups[ok][ord][pos]],
                               lty = lty[groups[ok][ord][pos]],
                               lwd = lwd[groups[ok][ord][pos]],
                               width = rep(width, nok),
                               height = y[ok][ord][pos],
                               just = c("centre", "bottom"),
                               identifier = paste(identifier, "pos", i,
                                 sep = "."))
                neg <- y[ok][ord] < 0
                nok <- sum(neg, na.rm = TRUE)
                if (nok > 0)
                    panel.rect(x = rep(i, nok),
                               y = cumsum(c(0, y[ok][ord][neg][-nok])),
                               col = col[groups[ok][ord][neg]],
                               border = border[groups[ok][ord][neg]],
                               lty = lty[groups[ok][ord][neg]],
                               lwd = lwd[groups[ok][ord][neg]],
                               width = rep(width, nok),
                               height = y[ok][ord][neg],
                               just = c("centre", "bottom"),
                               identifier = paste(identifier, "neg", i,
                                 sep = "."))
            }
        }
        else
        {
            if (is.null(origin))
            {
                origin <- current.panel.limits()$ylim[1]
                reference = FALSE
            }
##             vals <- seq_len(nlevels(groups))
##             groups <- as.numeric(groupSub(groups, ...))
##             ## vals <- sort(unique(groups))
##             nvals <- length(vals)

            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)

            width <- box.width / nvals # box.ratio/(1 + nvals * box.ratio)
            if (reference)
                panel.abline(h = origin,
                             col = reference.line$col,
                             lty = reference.line$lty,
                             lwd = reference.line$lwd,
                             identifier = paste(identifier, "abline",
                               sep = "."))
            for (i in unique(x))
            {
                ok <- x == i
                nok <- sum(ok, na.rm = TRUE)
                panel.rect(x = (i + width * (groups[ok] - (nvals + 1)/2)),
                           y = rep(origin, nok), 
                           col = col[groups[ok]],
                           border = border[groups[ok]],
                           lty = lty[groups[ok]],
                           lwd = lwd[groups[ok]],
                           width = rep(width, nok),
                           height = y[ok] - origin,
                           just = c("centre", "bottom"),
                           identifier = paste(identifier, "x", i,
                               sep = "."))
            }
        }
    }
}



panel.dotplot <-
    function(x, y, horizontal = TRUE,
             pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch,
             col = if (is.null(groups)) dot.symbol$col else sup.symbol$col,
             lty = dot.line$lty,
             lwd = dot.line$lwd,
             col.line = dot.line$col,
             levels.fos = if (horizontal) unique(y) else unique(x),
             groups = NULL,
             ...,
             identifier = "dotplot")
{
    x <- as.numeric(x)
    y <- as.numeric(y)

    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")

    if (horizontal)
    {
        panel.abline(h = levels.fos,
                     col = col.line, lty = lty, lwd = lwd,
                     identifier = paste(identifier, "abline", sep="."))
        panel.xyplot(x = x, y = y,
                     col = col, pch = pch,
                     ## lty = lty, lwd = lwd,
                     groups = groups,
                     horizontal = horizontal, ...,
                     identifier = identifier)
    }
    else
    {
        panel.abline(v = levels.fos,
                     col = col.line, lty = lty, lwd = lwd,
                     identifier = paste(identifier, "abline", sep="."))
        panel.xyplot(x = x, y = y,
                     col = col, pch = pch,
                     ## lty = lty, lwd = lwd,
                     groups = groups,
                     horizontal = horizontal, ...,
                     identifier = identifier)
    }
}





panel.stripplot <-
    function(x, y, jitter.data = FALSE,
             factor = 0.5, amount = NULL,
             horizontal = TRUE, groups = NULL, ...,
             identifier = "stripplot")
{
    if (!any(is.finite(x) & is.finite(y))) return()
    panel.xyplot(x = x,
                 y = y,
                 jitter.x = jitter.data && !horizontal,
                 jitter.y = jitter.data &&  horizontal,
                 factor = factor, amount = amount,
                 groups = groups,
                 horizontal = horizontal, ...,
                 identifier = identifier)
}



## version that supports notches (based on patch from Mike Kay)

panel.bwplot <-
    function(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
             horizontal = TRUE,
             pch = box.dot$pch,
             col = box.dot$col,
             alpha = box.dot$alpha,
             cex = box.dot$cex,
             font = box.dot$font,
             fontfamily = box.dot$fontfamily,
             fontface = box.dot$fontface,
             fill = box.rectangle$fill,
             varwidth = FALSE,
             notch = FALSE,
             notch.frac = 0.5,
             ...,
             levels.fos = if (horizontal) sort(unique(y)) else sort(unique(x)),
             stats = boxplot.stats,
             coef = 1.5, do.out = TRUE,
             identifier = "bwplot")
{
    if (all(is.na(x) | is.na(y))) return()
    x <- as.numeric(x)
    y <- as.numeric(y)

    box.dot <- trellis.par.get("box.dot")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")

    fontsize.points <- trellis.par.get("fontsize")$points
    cur.limits <- current.panel.limits()
    xscale <- cur.limits$xlim
    yscale <- cur.limits$ylim

    if (!notch) notch.frac <- 0

    if (horizontal)
    {
        blist <-
            tapply(x, factor(y, levels = levels.fos),
                   stats,
                   coef = coef,
                   do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.height <- box.width # box.ratio / (1 + box.ratio)
        if (varwidth)
        {
            maxn <- max(table(y))
            blist.n <- sapply(blist, "[[", "n")
            blist.height <- sqrt(blist.n / maxn) * blist.height
        }

        ## start of major changes to support notches
        blist.conf <-
            if (notch)
                t(sapply(blist, "[[", "conf"))
            else
                blist.stats[ , c(2,4), drop = FALSE]

        xbnd <- cbind(blist.stats[, 3], blist.conf[, 2],
                      blist.stats[, 4], blist.stats[, 4],
                      blist.conf[, 2], blist.stats[, 3],
                      blist.conf[, 1], blist.stats[, 2],
                      blist.stats[, 2], blist.conf[, 1],
                      blist.stats[, 3])
        ytop <- levels.fos + blist.height / 2
        ybot <- levels.fos - blist.height / 2
        ybnd <- cbind(ytop - notch.frac * blist.height / 2,
                      ytop, ytop, ybot, ybot,
                      ybot + notch.frac * blist.height / 2,
                      ybot, ybot, ytop, ytop,
                      ytop - notch.frac * blist.height / 2)
        ## xs <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
        ## ys <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
        ## xs[seq(along.with = levels.fos, by = 2), ] <- xbnd[seq(along.with = levels.fos), ]
        ## ys[seq(along.with = levels.fos, by = 2), ] <- ybnd[seq(along.with = levels.fos), ]

        ## box

        ## append NA-s to demarcate between boxes
        xs <- cbind(xbnd, NA_real_)
        ys <- cbind(ybnd, NA_real_)

        panel.polygon(t(xs), t(ys),
                      lwd = box.rectangle$lwd,
                      lty = box.rectangle$lty,
                      col = fill,
                      alpha = box.rectangle$alpha,
                      border = box.rectangle$col,
                      identifier = paste(identifier, "box", sep="."))
        ## end of major changes to support notches


        ## whiskers

        panel.segments(c(blist.stats[, 2], blist.stats[, 4]),
                       rep(levels.fos, 2),
                       c(blist.stats[, 1], blist.stats[, 5]),
                       rep(levels.fos, 2),
                       col = box.umbrella$col,
                       alpha = box.umbrella$alpha,
                       lwd = box.umbrella$lwd,
                       lty = box.umbrella$lty,
                       identifier = paste(identifier, "whisker", sep="."))
        panel.segments(c(blist.stats[, 1], blist.stats[, 5]),
                       levels.fos - blist.height / 2,
                       c(blist.stats[, 1], blist.stats[, 5]),
                       levels.fos + blist.height / 2,
                       col = box.umbrella$col,
                       alpha = box.umbrella$alpha,
                       lwd = box.umbrella$lwd,
                       lty = box.umbrella$lty,
                       identifier = paste(identifier, "cap", sep="."))

        ## dot

        if (all(pch == "|"))
        {
            mult <- if (notch) 1 - notch.frac else 1
            panel.segments(blist.stats[, 3],
                           levels.fos - mult * blist.height / 2,
                           blist.stats[, 3],
                           levels.fos + mult * blist.height / 2,
                           lwd = box.rectangle$lwd,
                           lty = box.rectangle$lty,
                           col = box.rectangle$col,
                           alpha = alpha,
                           identifier = paste(identifier, "dot", sep="."))
        }
        else
        {
            panel.points(x = blist.stats[, 3],
                         y = levels.fos,
                         pch = pch,
                         col = col, alpha = alpha, cex = cex,
                         fontfamily = fontfamily,
                         fontface = chooseFace(fontface, font),
                         fontsize = fontsize.points,
                         identifier = paste(identifier, "dot", sep="."))
        }

        ## outliers

        panel.points(x = unlist(blist.out),
                     y = rep(levels.fos, sapply(blist.out, length)),
                     pch = plot.symbol$pch,
                     col = plot.symbol$col,
                     alpha = plot.symbol$alpha,
                     cex = plot.symbol$cex,
                     fontfamily = plot.symbol$fontfamily,
                     fontface = chooseFace(plot.symbol$fontface, plot.symbol$font),
                     fontsize = fontsize.points,
                     identifier = paste(identifier, "outlier", sep="."))
                     
    }
    else
    {
        blist <-
            tapply(y, factor(x, levels = levels.fos),
                   stats,
                   coef = coef,
                   do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.height <- box.width # box.ratio / (1 + box.ratio)
        if (varwidth)
        {
            maxn <- max(table(x))
            blist.n <- sapply(blist, "[[", "n")
            blist.height <- sqrt(blist.n / maxn) * blist.height
        }

        blist.conf <-
            if (notch)
                sapply(blist, "[[", "conf")
            else
                t(blist.stats[ , c(2,4), drop = FALSE])

        ybnd <- cbind(blist.stats[, 3], blist.conf[2, ],
                      blist.stats[, 4], blist.stats[, 4],
                      blist.conf[2, ], blist.stats[, 3],
                      blist.conf[1, ], blist.stats[, 2],
                      blist.stats[, 2], blist.conf[1, ],
                      blist.stats[, 3])
        xleft <- levels.fos - blist.height / 2
        xright <- levels.fos + blist.height / 2
        xbnd <- cbind(xleft + notch.frac * blist.height / 2,
                      xleft, xleft, xright, xright,
                      xright - notch.frac * blist.height / 2,
                      xright, xright, xleft, xleft,
                      xleft + notch.frac * blist.height / 2)
        ## xs <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
        ## ys <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
        ## xs[seq(along.with = levels.fos, by = 2), ] <- xbnd[seq(along.with = levels.fos), ]
        ## ys[seq(along.with = levels.fos, by = 2), ] <- ybnd[seq(along.with = levels.fos), ]

        ## box

        ## append NA-s to demarcate between boxes
        xs <- cbind(xbnd, NA_real_)
        ys <- cbind(ybnd, NA_real_)

        panel.polygon(t(xs), t(ys),
                      lwd = box.rectangle$lwd,
                      lty = box.rectangle$lty,
                      col = fill,
                      alpha = box.rectangle$alpha,
                      border = box.rectangle$col,
                      identifier = paste(identifier, "box", sep="."))

        ## whiskers

        panel.segments(rep(levels.fos, 2),
                       c(blist.stats[, 2], blist.stats[, 4]),
                       rep(levels.fos, 2),
                       c(blist.stats[, 1], blist.stats[, 5]),
                       col = box.umbrella$col,
                       alpha = box.umbrella$alpha,
                       lwd = box.umbrella$lwd,
                       lty = box.umbrella$lty,
                       identifier = paste(identifier, "whisker", sep="."))

        panel.segments(levels.fos - blist.height / 2,
                       c(blist.stats[, 1], blist.stats[, 5]),
                       levels.fos + blist.height / 2,
                       c(blist.stats[, 1], blist.stats[, 5]),
                       col = box.umbrella$col,
                       alpha = box.umbrella$alpha,
                       lwd = box.umbrella$lwd,
                       lty = box.umbrella$lty,
                       identifier = paste(identifier, "cap", sep="."))


        ## dot

        if (all(pch == "|"))
        {
            mult <- if (notch) 1 - notch.frac else 1
            panel.segments(levels.fos - mult * blist.height / 2,
                           blist.stats[, 3],
                           levels.fos + mult * blist.height / 2,
                           blist.stats[, 3],
                           lwd = box.rectangle$lwd,
                           lty = box.rectangle$lty,
                           col = box.rectangle$col,
                           alpha = alpha,
                           identifier = paste(identifier, "dot", sep="."))
        }
        else
        {
            panel.points(x = levels.fos,
                         y = blist.stats[, 3],
                         pch = pch,
                         col = col, alpha = alpha, cex = cex,
                         fontfamily = fontfamily,
                         fontface = chooseFace(fontface, font),
                         fontsize = fontsize.points,
                         identifier = paste(identifier, "dot", sep="."))
        }

        ## outliers

        panel.points(x = rep(levels.fos, sapply(blist.out, length)),
                     y = unlist(blist.out),
                     pch = plot.symbol$pch,
                     col = plot.symbol$col,
                     alpha = plot.symbol$alpha,
                     cex = plot.symbol$cex,
                     fontfamily = plot.symbol$fontfamily,
                     fontface = chooseFace(plot.symbol$fontface, plot.symbol$font),
                     fontsize = fontsize.points,
                     identifier = paste(identifier, "outlier", sep="."))
    }
}














panel.violin <-
    function(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
             horizontal = TRUE,

             alpha = plot.polygon$alpha,
             border = plot.polygon$border,
             lty = plot.polygon$lty,
             lwd = plot.polygon$lwd,
             col = plot.polygon$col,

             varwidth = FALSE,

             bw = NULL,
             adjust = NULL,
             kernel = NULL,
             window = NULL,
             width = NULL,
             n = 50,
             from = NULL,
             to = NULL,
             cut = NULL,
             na.rm = TRUE,
             
             ...,
             identifier = "violin")
{
    if (all(is.na(x) | is.na(y))) return()
    x <- as.numeric(x)
    y <- as.numeric(y)

    ##reference.line <- trellis.par.get("reference.line")
    plot.polygon <- trellis.par.get("plot.polygon")

    ## density doesn't handle unrecognized arguments (not even to
    ## ignore it).  A tedious but effective way to handle that is to
    ## have all arguments to density be formal arguments to this panel
    ## function, as follows:

    darg <- list()
    darg$bw <- bw
    darg$adjust <- adjust
    darg$kernel <- kernel
    darg$window <- window
    darg$width <- width
    darg$n <- n
    darg$from <- from
    darg$to <- to
    darg$cut <- cut
    darg$na.rm <- na.rm
    my.density <- function(x)
    {
        ans <- try(do.call("density", c(list(x = x), darg)), silent = TRUE)
        ## if (inherits(ans, "try-error")) list(x = numeric(0), y = numeric(0)) else ans
        if (inherits(ans, "try-error"))
            list(x = rep(x[1], 3),
                 y = c(0, 1, 0))
        else ans
    }
    numeric.list <- if (horizontal) split(x, factor(y)) else split(y, factor(x))
    levels.fos <- as.numeric(names(numeric.list))
    d.list <- lapply(numeric.list, my.density)
    ## n.list <- sapply(numeric.list, length)  UNNECESSARY
    dx.list <- lapply(d.list, "[[", "x")
    dy.list <- lapply(d.list, "[[", "y")

    max.d <- sapply(dy.list, max)
    if (varwidth) max.d[] <- max(max.d)

    ##str(max.d)
    
    xscale <- current.panel.limits()$xlim
    yscale <- current.panel.limits()$ylim
    height <- box.width # box.ratio / (1 + box.ratio)

    if (hasGroupNumber())
        group <- list(...)$group.number
    else
        group <- 0

    if (horizontal)
    {
        for (i in seq_along(levels.fos))
        {
            if (is.finite(max.d[i]))
            {
                pushViewport(viewport(y = unit(levels.fos[i], "native"),
                                      height = unit(height, "native"),
                                      yscale = c(max.d[i] * c(-1, 1)),
                                      xscale = xscale))
                grid.polygon(x = c(dx.list[[i]], rev(dx.list[[i]])),
                             y = c(dy.list[[i]], -rev(dy.list[[i]])),
                             default.units = "native",
                             name = trellis.grobname(identifier,
                               type = "panel", group = group),
                             gp = gpar(fill = col, col = border, lty = lty, lwd = lwd, alpha = alpha))
                popViewport()
            }
        }
    }
    else
    {
        for (i in seq_along(levels.fos))
        {
            if (is.finite(max.d[i]))
            {
                pushViewport(viewport(x = unit(levels.fos[i], "native"),
                                      width = unit(height, "native"),
                                      xscale = c(max.d[i] * c(-1, 1)),
                                      yscale = yscale))
                grid.polygon(y = c(dx.list[[i]], rev(dx.list[[i]])),
                             x = c(dy.list[[i]], -rev(dy.list[[i]])),
                             default.units = "native",
                             name = trellis.grobname(identifier,
                               type = "panel", group = group),
                             gp = gpar(fill = col, col = border, lty = lty, lwd = lwd, alpha = alpha))
                popViewport()
            }
        }
    }
    invisible()
}



### dotplot, barchart and stripplot: essentially wrappers to bwplot


dotplot <- function(x, data, ...) UseMethod("dotplot")


## dotplot.numeric <-
##     function(formula, data = NULL, xlab = deparse(substitute(formula)), ...)
## {
##     ## old version:
##     ## nm <- deparse(substitute(formula))
##     ## formula <- as.formula(paste("~", nm))
##     ## or formula <- eval(substitute(~foo, list(foo = substitute(formula))))
##     ## both have the problem that they don't evaluate the formula

## this last attempt had problems with evaluations
## (e.g. dotplot(x, groups = a):

##     if (!missing(data))
##         warning("explicit 'data' specification ignored")
##     dotplot(~x, data = list(x = formula),
##             xlab = xlab,
##             ...)
## }

dotplot.numeric <-
    function(x, data = NULL, xlab = deparse(substitute(x)), ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(dotplot)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment() # list(x = x)
    ccall$xlab <- xlab
    ccall$x <- ~x
    ccall[[1]] <- quote(lattice::dotplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}



dotplot.table <-
    function(x, data = NULL, groups = TRUE,
             ..., horizontal = TRUE)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(barchart)
    if (!is.null(data)) warning("explicit 'data' specification ignored")
    data <- as.data.frame(x)
    nms <- names(data)
    freq <- which(nms == "Freq")
    nms <- nms[-freq]
    form <- ## WAS paste(nms[1], "Freq", sep = "~")
        sprintf(if (horizontal) "%s ~ Freq" else "Freq ~ %s", nms[1])
    nms <- nms[-1]
    len <- length(nms)
    if (is.logical(groups) && groups && len > 0)
    {
        groups <- as.name(nms[len])
        nms <- nms[-len]
        len <- length(nms)
    }
    else groups <- NULL
    if (len > 0)
    {
        rest <- paste(nms, collapse = "+")
        form <- paste(form, rest, sep = "|")
    }
    modifyList(dotplot(as.formula(form), data,
                       groups = eval(groups),
                       ...), 
               list(call = ocall))
}


dotplot.default <- function(x, data = NULL, ...) dotplot(table(x), data, ...)
dotplot.array <- function(x, data = NULL, ...) dotplot(as.table(x), data, ...)
dotplot.matrix <- function(x, data = NULL, ...) dotplot(as.table(x), data, ...)


dotplot.formula <-
    function(x,
             data = NULL,
             panel = lattice.getOption("panel.dotplot"),
             default.prepanel = lattice.getOption("prepanel.default.dotplot"),
             ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(dotplot)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$default.prepanel <- default.prepanel
    ccall[[1]] <- quote(lattice::bwplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}


barchart <- function(x, data, ...) UseMethod("barchart")


barchart.numeric <-
    function(x, data = NULL, xlab = deparse(substitute(x)), ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(barchart)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment() # list(x = x)
    ccall$xlab <- xlab
    ccall$x <- ~x
    ccall[[1]] <- quote(lattice::barchart)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}




barchart.table <-
    function(x, data = NULL, groups = TRUE,
             origin = 0, stack = TRUE, ..., horizontal = TRUE)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(barchart)
    if (!is.null(data)) warning("explicit 'data' specification ignored")
    data <- as.data.frame(x)
    nms <- names(data)
    freq <- which(nms == "Freq")
    nms <- nms[-freq]
    form <- ## WAS paste(nms[1], "Freq", sep = "~")
        sprintf(if (horizontal) "%s ~ Freq" else "Freq ~ %s", nms[1])
    nms <- nms[-1]
    len <- length(nms)
    if (is.logical(groups) && groups && len > 0)
    {
        groups <- as.name(nms[len])
        nms <- nms[-len]
        len <- length(nms)
    }
    else groups <- NULL
    if (len > 0)
    {
        rest <- paste(nms, collapse = "+")
        form <- paste(form, rest, sep = "|")
    }
    modifyList(barchart(as.formula(form), data,
                        groups = eval(groups),
                        ##groups = groups,
                        origin = origin, stack = stack, 
                        ...),
               list(ocall = ocall))
}

barchart.default <- function(x, data = NULL, ...) barchart(table(x), data, ...)
barchart.array <- function(x, data = NULL, ...) barchart(as.table(x), data, ...)
barchart.matrix <- function(x, data = NULL, ...) barchart(as.table(x), data, ...)


barchart.formula <-
    function(x,
             data = NULL,
             panel = lattice.getOption("panel.barchart"),
             default.prepanel = lattice.getOption("prepanel.default.barchart"),
             box.ratio = 2, 
             ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(barchart)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$default.prepanel <- default.prepanel
    ccall$box.ratio <- box.ratio
    ccall[[1]] <- quote(lattice::bwplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}


stripplot <- function(x, data, ...)  UseMethod("stripplot")


stripplot.numeric <-
    function(x, data = NULL, xlab = deparse(substitute(x)), ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(stripplot)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment() # list(x = x)
    ccall$xlab <- xlab
    ccall$x <- ~x
    ccall[[1]] <- quote(lattice::stripplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}



stripplot.formula <-
    function(x,
             data = NULL,
             panel = lattice.getOption("panel.stripplot"),
             default.prepanel = lattice.getOption("prepanel.default.stripplot"),
             ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(stripplot)
    ccall <- match.call()
    ccall$data <- data
    ccall$panel <- panel
    ccall$default.prepanel <- default.prepanel
    ccall[[1]] <- quote(lattice::bwplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}




### bwplot (the workhorse)

bwplot <- function(x, data, ...) UseMethod("bwplot")



bwplot.numeric <-
    function(x, data = NULL, xlab = deparse(substitute(x)), ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(bwplot)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment() # list(x = x)
    ccall$xlab <- xlab
    ccall$x <- ~x
    ccall[[1]] <- quote(lattice::bwplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}


bwplot.formula <-
    function(x,
             data = NULL,
             allow.multiple = is.null(groups) || outer,
             outer = FALSE,
             auto.key = FALSE,
             aspect = "fill",
             panel = lattice.getOption("panel.bwplot"),
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             box.ratio = 1,
             horizontal = NULL,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             ...,
             lattice.options = NULL,
             default.scales =
                 if (horizontal) list(y = list(tck = 0, alternating = FALSE, rot = 0))
                 else list(x = list(tck = 0, alternating = FALSE)),
             default.prepanel = lattice.getOption("prepanel.default.bwplot"),
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

    ## step 0: hack to get appropriate legend with auto.key = TRUE in
    ## barchart (default panel only).  The usual default in bwplot is
    ## appropriate for dotplot and stripplot (groups is usually not
    ## meaningful in bwplot itself).

    is.standard.barchart <- is.character(panel) && panel == "panel.barchart"

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
    y <- form$left
    if (is.null(y))
    {
        y <- rep(if (is.null(names(x))) '' else names(x), length.out = length(x))
        y <- factor(y, levels = unique(y))
    }
    if (length(cond) == 0)
    {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }
    if (is.null(horizontal))
    {
        horizontal <-
            if ((is.factor(x) || is.shingle(x) || is.character(x)) && is.numeric(y)) FALSE
            else TRUE
    }
    if (horizontal)
    {
##         if (!(is.numeric(x)))
##         {
##             warning("x should be numeric")
##         }
        y <- as.factorOrShingle(y)
        is.f.y <- is.factor(y)  # used throughout the rest of the code
        num.l.y <- nlevels(y)
        if (missing(xlab)) xlab <- form$right.name
        if (missing(ylab)) ylab <- if (is.f.y) NULL else form$left.name
    }
    else
    {
##         if (!(is.numeric(y)))
##         {
##             warning("y should be numeric")
##         }
        x <- as.factorOrShingle(x)
        is.f.x <- is.factor(x)  # used throughout the rest of the code
        num.l.x <- nlevels(x)
        if (missing(ylab)) ylab <- form$left.name
        if (missing(xlab)) xlab <- if (is.f.x) NULL else form$right.name
    }

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
    foo$call <- sys.call(sys.parent()); foo$call[[1]] <- quote(bwplot)

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
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (have.xlim) xlim <- logLimits(xlim, xbase)
    }
    if (have.ylog) {
        ## warning("Are you sure you want log scale for y ?")
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
    foo$panel.args.common$box.ratio <- box.ratio
    foo$panel.args.common$horizontal <- horizontal
    if (subscripts) foo$panel.args.common$groups <- groups

    ## only used if shingle, important if some levels are missing
    if (horizontal)
    {
        if (!is.f.y) ## y shingle
            foo$panel.args.common$nlevels <- num.l.y
    }
    else
    {
        if (!is.f.x) ## x shingle
            foo$panel.args.common$nlevels <- num.l.x
    }

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

        if (horizontal)
        {
            if (is.f.y)
            {
                foo$panel.args[[packet.number]] <-
                    list(x = x[id],
                         y = y[id])
                if (subscripts)
                    foo$panel.args[[packet.number]]$subscripts <-
                        subscr[id]
            }
            else  # shingle
            {
                panel.x <- numeric(0)
                panel.y <- numeric(0)
                if (subscripts) panel.subscr <- numeric(0)
                for (k in seq_len(num.l.y))
                {
                    tid <- id & (y >= levels(y)[[k]][1]) & (y <= levels(y)[[k]][2])
                    panel.x <- c(panel.x, x[tid])
                    panel.y <- c(panel.y, rep(k,length(tid[tid])))
                    if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                }
                foo$panel.args[[packet.number]] <-
                    list(x = panel.x,
                         y = panel.y)
                if (subscripts)
                    foo$panel.args[[packet.number]]$subscripts <-
                        panel.subscr
            }
        }
        else
        {
            if (is.f.x)
            {
                foo$panel.args[[packet.number]] <-
                    list(x = x[id],
                         y = y[id])
                if (subscripts)
                    foo$panel.args[[packet.number]]$subscripts <-
                        subscr[id]
            }
            else   # shingle
            {
                panel.x <- numeric(0)
                panel.y <- numeric(0)
                if (subscripts) panel.subscr <- numeric(0)
                for (k in seq_len(num.l.x))
                {
                    tid <- id & (x >= levels(x)[[k]][1]) & (x <= levels(x)[[k]][2])
                    panel.y <- c(panel.y, y[tid])
                    panel.x <- c(panel.x, rep(k,length(tid[tid])))
                    if (subscripts) panel.subscr <- c(panel.subscr, subscr[tid])
                }
                foo$panel.args[[packet.number]] <-
                    list(x = panel.x,
                         y = panel.y)
                if (subscripts)
                    foo$panel.args[[packet.number]]$subscripts <-
                        panel.subscr
            }
        }
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
                                      points = if (is.standard.barchart) FALSE else TRUE,
                                      rectangles = if (is.standard.barchart) TRUE else FALSE,
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

