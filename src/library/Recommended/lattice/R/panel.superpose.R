
### Copyright (C) 2001-2012  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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

## previously part of panels.R (r676)


panel.superpose <-
    function(x, y = NULL, subscripts, groups,
             panel.groups = "panel.xyplot",
             ...,
             col = "black",
             col.line = superpose.line$col,
             col.symbol = superpose.symbol$col,
             pch = superpose.symbol$pch,
             cex = superpose.symbol$cex,
             fill = superpose.symbol$fill,
             font = superpose.symbol$font,
             fontface = superpose.symbol$fontface,
             fontfamily = superpose.symbol$fontfamily,
             lty = superpose.line$lty,
             lwd = superpose.line$lwd,
             alpha = superpose.symbol$alpha,
             type = "p", grid = FALSE,
             distribute.type = FALSE)
{
    if (distribute.type)
    {

        ## This implies a slightly different behaviour: the 'type'
        ## argument behaves like other graphical parameters, i.e., it
        ## is repeated to be as long as the number of groups, and one
        ## used for each group.  This is the default behaviour of
        ## panel.superpose in S-PLUS.  The lattice default
        ## (!distribute.type) is to use all type values concurrently
        ## for each group.  We accomplish this by transforming 'type'
        ## to a list in either case (but in different ways) and then
        ## use common code.

        ## have.type <- FALSE
        type <- as.list(type)
    }
    else
    {
        ## this is something of a hack, needed because without this,
        ## grouped displays with 'g' %in% type may draw a grid
        ## (courtesy of panel.groups) for each group, overwriting
        ## earlier ones.

        ## have.type <- TRUE
        type <- unique(type)
        wg <- match('g', type, nomatch = NA_character_)
        if (!is.na(wg))
        {
            if (missing(grid)) grid <- TRUE
            type <- type[-wg]
        }
        type <- list(type)
    }
    if (grid) panel.grid(h = -1, v = -1, x = x, y = y)
    x <- as.numeric(x)
    if (!is.null(y)) y <- as.numeric(y)
    if (length(x) > 0)
    {
        if (!missing(col))
        {
            if (missing(col.line)) col.line <- col
            if (missing(col.symbol)) col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        vals <-
            if (is.factor(groups)) levels(groups)
            else sort(unique(groups))
        nvals <- length(vals)
        col <- rep(col, length.out = nvals)
        col.line <- rep(col.line, length.out = nvals)
        col.symbol <- rep(col.symbol, length.out = nvals)
        pch <- rep(pch, length.out = nvals)
        fill <- rep(fill, length.out = nvals)
        lty <- rep(lty, length.out = nvals)
        lwd <- rep(lwd, length.out = nvals)
        alpha <- rep(alpha, length.out = nvals)
        cex <- rep(cex, length.out = nvals)
        font <- rep(font, length.out = nvals)
        ## The next two may be NULL (in fact, are so by default)
        if (!is.null(fontface)) fontface <- rep(fontface, length.out = nvals)
        if (!is.null(fontfamily)) fontfamily <- rep(fontfamily, length.out = nvals)
        type <- rep(type, length.out = nvals)

        panel.groups <- getFunctionOrName(panel.groups)

        subg <- groups[subscripts]
        ok <- !is.na(subg)
        for (i in seq_along(vals))
        {
            id <- ok & (subg == vals[i])
            if (any(id))
            {
                args <-
                    list(x = x[id],
                         ## groups = groups,
                         subscripts = subscripts[id],
                         pch = pch[[i]], cex = cex[[i]],
                         font = font[[i]],
                         fontface = fontface[[i]],
                         fontfamily = fontfamily[[i]],
                         col = col[[i]],
                         col.line = col.line[[i]],
                         col.symbol = col.symbol[[i]],
                         fill = fill[[i]],
                         lty = lty[[i]],
                         lwd = lwd[[i]],
                         alpha = alpha[[i]],
                         type = type[[i]],
                         group.number = i,
                         group.value = vals[i],
                         ...)
                ## if (have.type) args$type <- type
                if (!is.null(y)) args$y <- y[id]
                do.call(panel.groups, args)
            }
        }
    }
}



panel.superpose.2 <-
    function(..., distribute.type = TRUE)
{
    panel.superpose(...,
                    distribute.type = distribute.type)
}


## panel.superpose.2 <-
##     function(x, y = NULL, subscripts, groups,
##              panel.groups = "panel.xyplot",
##              col,
##              col.line = superpose.line$col,
##              col.symbol = superpose.symbol$col,
##              pch = superpose.symbol$pch,
##              cex = superpose.symbol$cex,
##              font = superpose.symbol$font,
##              fontface = superpose.symbol$fontface,
##              fontfamily = superpose.symbol$fontfamily,
##              lty = superpose.line$lty,
##              lwd = superpose.line$lwd,
##              alpha = superpose.symbol$alpha,
##              type = "p",
##              ...)
## {

##     ## This is a slightly different version of panel.superpose.  It
##     ## has an explicit type argument which behaves like other
##     ## graphical parameters, i.e., it is repeated to be as long as the
##     ## number of groups, and one used for each group.  This is the
##     ## default behaviour of panel.superpose in S-PLUS.

##     ## Original version contributed by Neil Klepeis

##     type <- as.list(type)

##     x <- as.numeric(x)
##     if (!is.null(y)) y <- as.numeric(y)

##     if (length(x)>0)
##     {
##         if (!missing(col))
##         {
##             if (missing(col.line)) col.line <- col
##             if (missing(col.symbol)) col.symbol <- col
##         }

##         superpose.symbol <- trellis.par.get("superpose.symbol")
##         superpose.line <- trellis.par.get("superpose.line")

##         vals <-
##             if (is.factor(groups)) levels(groups)
##             else sort(unique(groups))
##         nvals <- length(vals)
##         col.line <- rep(col.line, length.out = nvals)
##         col.symbol <- rep(col.symbol, length.out = nvals)
##         pch <- rep(pch, length.out = nvals)
##         lty <- rep(lty, length.out = nvals)
##         lwd <- rep(lwd, length.out = nvals)
##         alpha <- rep(alpha, length.out = nvals)
##         cex <- rep(cex, length.out = nvals)
##         font <- rep(font, length.out = nvals)
##         fontface <- rep(fontface, length.out = nvals)
##         fontfamily <- rep(fontfamily, length.out = nvals)
##         type <- rep(type, length.out = nvals)

##         panel.groups <-
##             if (is.function(panel.groups)) panel.groups
##             else if (is.character(panel.groups)) get(panel.groups)
##             else eval(panel.groups)

##         subg <- groups[subscripts]
##         ok <- !is.na(subg)
##         for (i in seq_along(vals))
##         {
##             id <- ok & (subg == vals[i])
##             if (any(id))
##             {
##                 args <-
##                     list(x=x[id],
##                          groups = groups,
##                          subscripts = subscripts[id],
##                          pch = pch[i], cex = cex[i],
##                          font = font[i],
##                          fontface = fontface[i],
##                          fontfamily = fontfamily[i],
##                          col.line = col.line[i],
##                          col.symbol = col.symbol[i],
##                          lty = lty[i],
##                          lwd = lwd[i],
##                          alpha = alpha[i],
##                          type = type[[i]], ...)
##                 if (!is.null(y)) args$y <- y[id]

##                 do.call(panel.groups, args)
##             }
##         }
##     }
## }





# panel.superpose.2 <-
#     function(x, y, subscripts, groups,
#              col, col.line = superpose.line$col,
#              col.symbol = superpose.symbol$col,
#              pch = superpose.symbol$pch,
#              cex = superpose.symbol$cex,
#              lty = superpose.line$lty,
#              lwd = superpose.line$lwd, type="p", ...)
# {

#     ## `panel.superpose.2' : This is a version of the
#     ## 'panel.superpose' Trellis panel function that allows the plot
#     ## `type' to change between superimposed (overlayed) data sets.
#     ## See the `panel.xyplot' function for details on the `type'
#     ## option which is usually a single character, but here is a
#     ## character vector with each element specifying the plot style of
#     ## each subsequently-overlayed plot.  --- Neil Klepeis,
#     ## 26-Dec-2001

#     x <- as.numeric(x)
#     y <- as.numeric(y)

#     if (length(x) > 0) {
#         if (!missing(col)) {
#             if (missing(col.line))
#                 col.line <- col
#             if (missing(col.symbol))
#                 col.symbol <- col
#         }
#         superpose.symbol <- trellis.par.get("superpose.symbol")
#         superpose.line <- trellis.par.get("superpose.line")
#         x <- as.numeric(x)
#         y <- as.numeric(y)
#         vals <-
#             if (is.factor(groups)) levels(groups)
#             else sort(unique(groups))
#         nvals <- length(vals)
#         col.line <- rep(col.line, length.out = nvals)
#         col.symbol <- rep(col.symbol, length.out = nvals)
#         pch <- rep(pch, length.out = nvals)
#         lty <- rep(lty, length.out = nvals)
#         lwd <- rep(lwd, length.out = nvals)
#         cex <- rep(cex, length.out = nvals)
#         type <- rep(type, length.out = nvals)      # new line here
#         for (i in seq_along(vals)) {
#             id <- (groups[subscripts] == vals[i])
#             if (any(id))
#                 panel.xyplot(x = x[id], y = y[id], pch = pch[i],
#                   cex = cex[i], col.line = col.line[i], col.symbol = col.symbol[i],
#                   lty = lty[i], lwd = lwd[i], type=type[i], ...)
#         }
#     }
# }


