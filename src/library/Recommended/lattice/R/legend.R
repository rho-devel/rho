

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







construct.legend <-
    function(legend = NULL, key = NULL, fun = "draw.key")
{
    if (is.null(legend) && is.null(key)) return(NULL)
    if (is.null(legend)) legend <- list()
    if (!is.null(key))
    {
        space <- key$space
        x <- y <- corner <- NULL

        if (is.null(space))
        {
            if (any(c("x", "y", "corner") %in% names(key)))
            {
                stopifnot(is.null(x) || (length(x) == 1 && x >= 0 && x <= 1))
                stopifnot(is.null(y) || (length(y) == 1 && y >= 0 && y <= 1))
                stopifnot(is.null(corner) ||
                          (length(corner) == 2 &&
                           all(corner %in% c(0, 1))))
                space <- "inside"
                x <- key$x
                y <- key$y
                corner <- key$corner
                ## check for valid values
            }
            else
                space <- "top"
        }
        if (space != "inside" && space %in% names(legend))
            stop(gettextf("component '%s' duplicated in key and legend", space))

        key.legend <- list(fun = fun, args = list(key = key, draw = FALSE))
        key.legend$x <- x
        key.legend$y <- y
        key.legend$corner <- corner

        legend <- c(list(key.legend), legend)
        names(legend)[1] <- space
    }
    legend
}







## A convenience function for deciding whether an ``automatic'' legend
## should be drawn following instructions contained in the 'auto.key'
## argument.

needAutoKey <- function(auto.key, groups = NULL)
{
    ((!is.null(groups) && (isTRUE(auto.key) || is.list(auto.key))) || 
     (is.list(auto.key) && !is.null(auto.key$text)))
    ## old behaviour was:
    ## !is.null(groups) && (is.list(auto.key) || isTRUE(auto.key))
}


## convenience function for auto.key
drawSimpleKey <- function(...)
    draw.key(simpleKey(...), draw = FALSE)





## convenience function for the most common type of key

simpleKey <-
    function(text, points = TRUE,
             rectangles = FALSE,
             lines = FALSE,
             col = add.text$col,
             cex = add.text$cex,
             alpha = add.text$alpha,
             font = add.text$font,
             fontface = add.text$fontface,
             fontfamily = add.text$fontfamily,
             lineheight = add.text$lineheight,
             ...)
{
    add.text <- trellis.par.get("add.text")
    foo <- seq_along(text)
    ans <-
        list(text = list(lab = text),
             col = col, cex = cex, alpha = alpha,
             font = font,
             fontface = fontface,
             fontfamily = fontfamily,
             ...)
    if (points) ans$points <-
        Rows(trellis.par.get("superpose.symbol"), foo)
    if (rectangles) ans$rectangles <- 
        Rows(trellis.par.get("superpose.polygon"), foo)
    if (lines) ans$lines <-
        updateList(Rows(trellis.par.get("superpose.symbol"), foo), ## for pch
                   Rows(trellis.par.get("superpose.line"), foo))
    ans
}
             


componentName <- function(name, x, y) {
    trellis.grobname(paste(name, x, y, sep="."), type="key")
}


draw.key <- function(key, draw = FALSE, vp = NULL, ...)
{
    if (!is.list(key)) stop("key must be a list")
    
    max.length <- 0

    ## maximum of the `row-lengths' of the above
    ## components. There is some scope for confusion
    ## here, e.g., if col is specified in key as a
    ## length 6 vector, and then lines=list(lty=1:3),
    ## what should be the length of that lines column ?
    ## If 3, what happens if lines=list() ?
    ## (Strangely enough, S+ accepts lines=list()
    ## if col (etc) is NOT specified outside, but not
    ## if it is)
    
    process.key <-
        function(reverse.rows = FALSE, ## invert rows (e.g. for barchart, stack = FALSE)
                 between = 2,
                 align = TRUE,
                 title = NULL,
                 rep = TRUE,
                 background = trellis.par.get("background")$col,
                 alpha.background = 1,
                 border = FALSE,
                 transparent = FALSE, 
                 col = "black",
                 alpha = 1,
                 lty = 1,
                 lwd = 1,
                 font = 1, 
                 fontface = NULL, 
                 fontfamily = NULL,
                 pch = 8,
                 cex = 1,
                 fill = "transparent",
                 adj = 0,
                 type = "l", 
                 size = 5,
                 height = 1,
                 angle = 0, 
                 density = -1,
                 cex.title = 1.5 * max(cex),                 
                 padding.text = 1,
                 lineheight = 1,
                 columns = 1,
                 divide = 3,
                 between.columns = 3,

                 ## to avoid partial matching, anything starting with
                 ## lines, points, rect, text must come after ...
                 
                 ...,
                 lines.title = 2)
        {
            list(reverse.rows = reverse.rows,
                 between = between,
                 align = align,
                 title = title,
                 rep = rep,
                 background = background,
                 alpha.background = alpha.background,
                 border = border,
                 transparent = transparent, 
                 col = col,
                 alpha = alpha,
                 lty = lty,
                 lwd = lwd,
                 font = font,
                 fontface = fontface,
                 fontfamily = fontfamily,
                 pch = pch,
                 cex = cex,
                 fill = fill,
                 adj = adj,
                 type = type, 
                 size = size,
                 height = height,
                 angle = angle, 
                 density = density,
                 cex.title = cex.title,
                 padding.text = padding.text,
                 lineheight = lineheight,
                 columns = columns,
                 divide = divide,
                 between.columns = between.columns,
                 lines.title = lines.title,
                 ...)
        }

    fontsize.points <- trellis.par.get("fontsize")$points
    key <- do.call("process.key", key)

    key.length <- length(key)
    key.names <- names(key)    # Need to update
    if (is.logical(key$border)) 
        key$border <-
            if (key$border) "black"
            else "transparent"

    components <- list()

    for(i in 1:key.length)
    {
        curname <- pmatch(key.names[i], c("text", "rectangles", "lines", "points"))

        if (is.na(curname))
        {
            ;## do nothing
        }
        else if (curname == 1) # "text"
        {
            if (!(is.characterOrExpression(key[[i]][[1]])))
                stop("first component of text must be vector of labels")
            pars <-
                list(labels = key[[i]][[1]],
                     col = key$col,
                     alpha = key$alpha,
                     adj = key$adj,
                     cex = key$cex,
                     lineheight = key$lineheight,
                     font = key$font,
                     fontface = key$fontface,
                     fontfamily = key$fontfamily)
            pars <- pars[!sapply(pars, is.null)] # remove NULL components
            key[[i]][[1]] <- NULL
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- length(pars$labels)
            for (j in 1:length(pars))
                if (is.character(pars))
                    pars[[j]] <- rep(pars[[j]], length.out = tmplen)
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "text", pars = pars, length = tmplen)
        }
        else if (curname == 2) # "rectangles"
        {
            pars <-
                list(col = key$col,
                     border = "black",
                     alpha = key$alpha,
                     size = key$size,
                     height = key$height,
                     angle = key$angle,
                     density = key$density)
            pars <- pars[!sapply(pars, is.null)] # remove NULL components
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "rectangles", pars = pars, length = tmplen)
        }
        else if (curname == 3) # "lines"
        {
            pars <-
                list(col = key$col,
                     alpha = key$alpha,
                     size = key$size,
                     lty = key$lty,
                     cex = key$cex,
                     pch = key$pch,
                     fill = key$fill,
                     lwd = key$lwd,
                     type = key$type)
            pars <- pars[!sapply(pars, is.null)] # remove NULL components
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "lines", pars = pars, length = tmplen)
        }
        else if (curname == 4) # "points"
        {
            pars <-
                list(col = key$col,
                     alpha = key$alpha,
                     cex = key$cex,
                     pch = key$pch,
                     lwd = key$lwd,
                     fill = key$fill,
                     font = key$font,
                     fontface = key$fontface,
                     fontfamily = key$fontfamily)
            pars <- pars[!sapply(pars, is.null)] # remove NULL components
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars,length)))
            max.length <- max(max.length, tmplen)
            components[[length(components)+1]] <-
                list(type = "points", pars = pars, length = tmplen)
        }
    }

    number.of.components <- length(components)
    ## number of components named one of "text",
    ## "lines", "rectangles" or "points"
    if (number.of.components == 0)
        stop("Invalid key, need at least one component named lines, text, rect or points")

    ## The next part makes sure all components have same length,
    ## except text, which should be as long as the number of labels

    ## Update (9/11/2003): but that doesn't always make sense --- Re:
    ## r-help message from Alexander.Herr@csiro.au (though it seems
    ## that's S+ behaviour on Linux at least). Each component should
    ## be allowed to have its own length (that's what the lattice docs
    ## suggest too, don't know why). Anyway, I'm adding a rep = TRUE
    ## argument to the key list, which controls whether each column
    ## will be repeated as necessary to have the same length.

    for (i in seq_len(number.of.components))
    {
        if (key$rep && (components[[i]]$type != "text"))
            components[[i]]$length <- max.length
        components[[i]]$pars <-
            lapply(components[[i]]$pars, rep, length.out = components[[i]]$length)
        if (key$reverse.rows) 
            components[[i]]$pars <- 
                lapply(components[[i]]$pars, rev)
##         {
##             if (key$rep) components[[i]]$length <- max.length
##             components[[i]]$pars <-
##                 lapply(components[[i]]$pars, rep, components[[i]]$length)
##         }
##         else
##         {
##             components[[i]]$pars <-
##                 c(components[[i]]$pars[1],
##                   lapply(components[[i]]$pars[-1], rep,
##                          length.out = components[[i]]$length))
##         }

    }
    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)
    if (column.blocks > max.length) warning("not enough rows for columns")
    key$between <- rep(key$between, length.out = number.of.components)
    
    if (key$align)
    {

        ## Setting up the layout

	## The problem of allocating space for text (character strings
	## or expressions) is dealt with as follows: 

	## Each row and column will take exactly as much space as
	## necessary. As steps in the construction, a matrix
	## textMatrix (of same dimensions as the layout) will contain
	## either 0, meaning that entry is not text, or n > 0, meaning
	## that entry has the text given by textList[[n]], where
	## textList is a list consisting of character strings or
	## expressions.

        n.row <- rows.per.block + 1
        n.col <- column.blocks * (1 + 3 * number.of.components) - 1

	textMatrix <- matrix(0, n.row, n.col)
	textList <- list()
	textCex <- numeric(0)

        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- vector(mode = "list", length = n.row)

        if (length(key$title) > 0)
        {
            stopifnot(length(key$title) == 1,
                      is.characterOrExpression(key$title))
            heights.x[1] <- key$lines.title * key$cex.title
            heights.units[1] <- "strheight"
            heights.data[[1]] <- key$title
        }
        else heights.x[1] <- 0

        widths.x <- rep(key$between.columns, n.col)
        widths.units <- rep("strwidth", n.col)
        widths.data <- as.list(rep("o", n.col))

        for (i in 1:column.blocks)
        {
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i-1] <-
                         key$between/2
            widths.x[(1:number.of.components-1)*3+1 +
                     (i-1)*3*number.of.components + i+1] <-
                         key$between/2
        }
        index <- 1
        for (i in 1:number.of.components)
        {
            cur <- components[[i]]
            id <- (1:column.blocks - 1) *
                (number.of.components * 3 + 1) + i * 3 - 1
            if (cur$type == "text")
            {
                for (j in 1:cur$length)
                {
                    colblck <- ceiling(j / rows.per.block)
                    xx <- (colblck - 1) *
                        (number.of.components * 3 + 1) + i * 3 - 1
                    yy <- j %% rows.per.block + 1
                    if (yy == 1) yy <- rows.per.block + 1
		    textMatrix[yy, xx] <- index
		    textList <- c(textList, list(cur$pars$labels[j]) )
		    textCex <- c(textCex, cur$pars$cex[j])
  		    index <- index + 1
		}
            } ## FIXME: do the same as above for those below
            else if (cur$type == "rectangles")
            {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "lines")
            {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "points")
            {
                widths.x[id] <- max(cur$pars$cex)
            }
        }

        ## Need to adjust the heights and widths 
        
        ## adjusting heights
        heights.insertlist.position <- 0
        heights.insertlist.unit <- unit(1, "null")
        for (i in seq_len(n.row))
        {
            textLocations <- textMatrix[i,]
            if (any(textLocations > 0))
            {
                textLocations <- textLocations[textLocations>0]
                strbar <- textList[textLocations]
                heights.insertlist.position <- c(heights.insertlist.position, i)
                heights.insertlist.unit <-
                    unit.c(heights.insertlist.unit,
                           unit(.2 * key$padding.text, "lines") +
                           max(unit(textCex[textLocations], "strheight", strbar)))
            }
        }

        layout.heights <- unit(heights.x, heights.units, data=heights.data)
        if (length(heights.insertlist.position)>1)
            for (indx in 2:length(heights.insertlist.position))
                layout.heights <-
                    rearrangeUnit(layout.heights, heights.insertlist.position[indx],
                                  heights.insertlist.unit[indx])

        ## adjusting widths
        widths.insertlist.position <- 0
        widths.insertlist.unit <- unit(1, "null")

        for (i in 1:n.col)
        {
            textLocations <- textMatrix[,i]
            if (any(textLocations > 0))
            {
                textLocations <- textLocations[textLocations>0]
                strbar <- textList[textLocations]
                widths.insertlist.position <- c(widths.insertlist.position, i)
                widths.insertlist.unit <-
                    unit.c(widths.insertlist.unit,
                           max(unit(textCex[textLocations], "strwidth", strbar)))
            }
        }

        layout.widths <- unit(widths.x, widths.units, data=widths.data)
        if (length(widths.insertlist.position)>1)
            for (indx in 2:length(widths.insertlist.position))
                layout.widths <-
                    rearrangeUnit(layout.widths, widths.insertlist.position[indx],
                                  widths.insertlist.unit[indx])

        key.layout <-
            grid.layout(nrow = n.row, ncol = n.col,
                        widths = layout.widths,
                        heights = layout.heights,
                        respect = FALSE,
                        just = if (is.null(key$just)) "center" else key$just)

        ## OK, layout set up, now to draw the key - no
        key.gf <- frameGrob(layout = key.layout, vp = vp,
                            name = trellis.grobname("frame", type="key"))
        if (!key$transparent)
            key.gf <-
                placeGrob(key.gf,
                          rectGrob(gp =
                                   gpar(fill = key$background,
                                        alpha = key$alpha.background,
                                        col = key$border),
                                   name = trellis.grobname("background",
                                     type="key")),
                          row = NULL, col = NULL)
        else
            key.gf <-
                placeGrob(key.gf,
                          rectGrob(gp=gpar(col=key$border),
                                   name = trellis.grobname("background",
                                     type="key")),
                          row = NULL, col = NULL)

        ## Title (FIXME: allow color, font, alpha-transparency here? 
        if (!is.null(key$title))
            key.gf <-
                placeGrob(key.gf, 
                          textGrob(label = key$title,
                                   gp = gpar(cex = key$cex.title,
                                     lineheight = key$lineheight),
                                   name = trellis.grobname("title",
                                     type="key")),
                          row = 1, col = NULL)
        
        for (i in 1:number.of.components)
        {
            cur <- components[[i]]
            for (j in seq_len(cur$length))
            {
                colblck <- ceiling(j / rows.per.block)
                xx <- (colblck - 1) *
                    (number.of.components*3 + 1) + i*3 - 1
                yy <- j %% rows.per.block + 1
                if (yy == 1) yy <- rows.per.block + 1
                componentx <- (colblck - 1)*(number.of.components) + i 
                componenty <- (j - 1) %% rows.per.block + 1
                if (cur$type == "text")
                {
                    key.gf <-
                        placeGrob(key.gf, 
                                  textGrob(x = cur$pars$adj[j],

                                           hjust = cur$pars$adj[j],

##                                            just = c(
##                                            if (cur$pars$adj[j] == 1) "right"
##                                            else if (cur$pars$adj[j] == 0) "left"
##                                            else "center",
##                                            "center"),

                                           label = cur$pars$labels[j],
                                           gp =
                                           gpar(col = cur$pars$col[j],
                                                alpha = cur$pars$alpha[j],
                                                lineheight = cur$pars$lineheight[j],
                                                fontfamily = cur$pars$fontfamily[j],
                                                fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                cex = cur$pars$cex[j]),
                                           name = componentName("text",
                                             componentx, componenty)),
                                  row = yy, col = xx)
                }
                else if (cur$type == "rectangles")
                {
                    key.gf <-
                        placeGrob(key.gf, 
                                  rectGrob(height = cur$pars$height[j],
                                           width = cur$pars$size[j] / max(cur$pars$size),
                                           default.units = "npc",
                                           ## centred, unlike S-PLUS, due to aesthetic reasons !
                                           gp = gpar(alpha = cur$pars$alpha[j],
                                                     fill = cur$pars$col[j],
                                                     col = cur$pars$border[j]),
                                           name = componentName("rect",
                                             componentx, componenty)),
                                  row = yy, col = xx)
                    ## FIXME: Need to make changes to support angle/density
                }
                else if (cur$type == "lines")
                {
                    if (cur$pars$type[j] == "l")
                    {
                        key.gf <-
                            placeGrob(key.gf,
                                      linesGrob(x = c(0,1) * cur$pars$size[j]/max(cur$pars$size),
                                                
                                                ## ^^ FIXME: this
                                                ## should be centered
                                                ## as well, but since
                                                ## the chances that
                                                ## someone would
                                                ## actually use this
                                                ## feature are
                                                ## astronomical, I'm
                                                ## leaving that for
                                                ## later.

                                                y = c(.5, .5),
                                                gp =
                                                gpar(col = cur$pars$col[j],
                                                     alpha = cur$pars$alpha[j],
                                                     lty = cur$pars$lty[j],
                                                     lwd = cur$pars$lwd[j]),
                                                name = componentName("lines",
                                                  componentx, componenty)),
                                      row = yy, col = xx)
                    }
                    else if (cur$pars$type[j] == "p")
                    {
                        key.gf <-
                            placeGrob(key.gf,
                                      pointsGrob(x=.5, y=.5, 
                                                 gp =
                                                 gpar(col = cur$pars$col[j],
                                                      alpha = cur$pars$alpha[j],
                                                      cex = cur$pars$cex[j],
                                                      fill = cur$pars$fill[j],
                                                      fontfamily = cur$pars$fontfamily[j],
                                                      fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                      fontsize = fontsize.points),
                                                 pch = cur$pars$pch[j],
                                                 name = componentName("points",
                                                   componentx, componenty)),
                                      row = yy, col = xx)
                    }
                    else # if (cur$pars$type[j] == "b" or "o") -- not differentiating
                    { 
                        key.gf <-
                            placeGrob(key.gf, 
                                      linesGrob(x = c(0,1) * cur$pars$size[j]/max(cur$pars$size),

                                                ## ^^ this should be
                                                ## centered as well,
                                                ## but since the
                                                ## chances that
                                                ## someone would
                                                ## actually use this
                                                ## feature are
                                                ## astronomical, I'm
                                                ## leaving that for
                                                ## later.

                                                y = c(.5, .5),
                                                gp =
                                                gpar(col = cur$pars$col[j],
                                                     alpha = cur$pars$alpha[j],
                                                     lty = cur$pars$lty[j],
                                                     lwd = cur$pars$lwd[j]),
                                                name = componentName("lines",
                                                  componentx, componenty)),
                                      row = yy, col = xx)
                        if (key$divide > 1)
                        {
                            key.gf <-
                                placeGrob(key.gf, 
                                          pointsGrob(x = (1:key$divide-1)/(key$divide-1),
                                                     y = rep(.5, key$divide),
                                                     gp =
                                                     gpar(col = cur$pars$col[j],
                                                          alpha = cur$pars$alpha[j],
                                                          cex = cur$pars$cex[j],
                                                          fill = cur$pars$fill[j],
                                                          fontfamily = cur$pars$fontfamily[j],
                                                          fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                          fontsize = fontsize.points),
                                                     pch = cur$pars$pch[j],
                                                     name = componentName("points",
                                                       componentx, componenty)),
                                          row = yy, col = xx)
                        }
                        else if (key$divide == 1)
                        {
                            key.gf <-
                                placeGrob(key.gf, 
                                          pointsGrob(x = 0.5, 
                                                     y = 0.5, 
                                                     gp =
                                                     gpar(col = cur$pars$col[j],
                                                          alpha = cur$pars$alpha[j],
                                                          cex = cur$pars$cex[j],
                                                          fill = cur$pars$fill[j],
                                                          fontfamily = cur$pars$fontfamily[j],
                                                          fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                          fontsize = fontsize.points),
                                                     pch = cur$pars$pch[j],
                                                     name = componentName("points",
                                                       componentx, componenty)),
                                          row = yy, col = xx)
                        }
                    }
                }
                else if (cur$type == "points")
                {
                    key.gf <-
                        placeGrob(key.gf,
                                  pointsGrob(x=.5, y=.5,
                                             gp =
                                             gpar(col = cur$pars$col[j],
                                                  alpha = cur$pars$alpha[j],
                                                  cex = cur$pars$cex[j],
                                                  lwd = cur$pars$lwd[j],
                                                  fill = cur$pars$fill[j],
                                                  fontfamily = cur$pars$fontfamily[j],
                                                  fontface = chooseFace(cur$pars$fontface[j], cur$pars$font[j]),
                                                  fontsize = fontsize.points),
                                             pch = cur$pars$pch[j],
                                             name = componentName("points",
                                               componentx, componenty)),
                                  row = yy, col = xx)
                }
            }
        }
    }
    else stop("Sorry, align=FALSE is not supported")
    if (draw) grid.draw(key.gf)
    key.gf
}









draw.colorkey <- function(key, draw = FALSE, vp = NULL)
{
    if (!is.list(key)) stop("key must be a list")
    
    process.key <-
        function(col = regions$col,
                 alpha = regions$alpha,
                 at,
                 tick.number = 7,
                 tck = 1,
                 width = 2,
                 height = 1,
                 space = "right",
                 raster = FALSE,
                 interpolate = FALSE,
                 axis.line = list(),
                 axis.text = list(),
                 ...)
        {
            regions <- trellis.par.get("regions")
            list(col = col,
                 alpha = alpha,
                 at = at,
                 tick.number = tick.number,
                 tck = tck,
                 width = width,
                 height = height,
                 space = space,
                 raster = raster,
                 interpolate = interpolate,
                 axis.line = axis.line,
                 axis.text = axis.text,
                 ...)
        }

    key <- do.call(process.key, key)

    axis.line <- updateList(trellis.par.get("axis.line"), key$axis.line)
    axis.text <- updateList(trellis.par.get("axis.text"), key$axis.text)

    ## made FALSE later if labels explicitly specified
    check.overlap <- TRUE
    

    ## Note: there are two 'at'-s here, one is key$at, which specifies
    ## the breakpoints of the rectangles, and the other is key$lab$at
    ## (optional) which is the positions of the ticks. We will use the
    ## 'at' variable for the latter, 'atrange' for the range of the
    ## former, and key$at explicitly when needed



    ## Getting the locations/dimensions/centers of the rectangles
    key$at <- sort(key$at) ## should check if ordered
    numcol <- length(key$at)-1
##     numcol.r <- length(key$col)
##     key$col <-
##         if (is.function(key$col)) key$col(numcol)
##         else if (numcol.r <= numcol) rep(key$col, length.out = numcol)
##         else key$col[floor(1+(1:numcol-1)*(numcol.r-1)/(numcol-1))]

    key$col <-
        level.colors(x = seq_len(numcol) - 0.5,
                     at = seq_len(numcol + 1) - 1,
                     col.regions = key$col,
                     colors = TRUE)


    ## FIXME: need to handle DateTime classes properly


    atrange <- range(key$at, finite = TRUE)
    scat <- as.numeric(key$at) ## problems otherwise with DateTime objects (?)

    if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 0)))
        warning("'at' values are not equispaced; output may be wrong")

    
    ## recnum <- length(scat)-1
    reccentre <- (scat[-1] + scat[-length(scat)]) / 2
    recdim <- diff(scat)

    cex <- axis.text$cex
    col <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface <- axis.text$fontface
    lineheight <- axis.text$lineheight
    rot <- 0

    ## The following code assumes names key$lab and key$lab$lab (which
    ## may have been used in user code), whereas documentation says
    ## key$labels and key$labels$labels.  To make both work without
    ## 'partial matching' warnings, we rename key$labels to key$lab
    ## etc.

    if (!is.null(key[["labels"]])) 
    {
        key[["lab"]] <- key[["labels"]]
        key[["labels"]] <- NULL
        if (is.list(key[["lab"]]) && !is.null(key[["lab"]][["labels"]])) 
        {
            key[["lab"]][["lab"]] <- key[["lab"]][["labels"]]
            key[["lab"]][["labels"]] <- NULL
        }
    }
    
    if (is.null(key$lab))
    {
        at <- lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- format(at, trim = TRUE)
    }
    else if (is.characterOrExpression(key$lab) && length(key$lab)==length(key$at))
    {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    }
    else if (is.list(key$lab))
    {
        at <- if (!is.null(key$lab$at)) key$lab$at else lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
        } else format(at, trim = TRUE)
        if (!is.null(key$lab$cex)) cex <- key$lab$cex
        if (!is.null(key$lab$col)) col <- key$lab$col
        if (!is.null(key$lab$font)) font <- key$lab$font
        if (!is.null(key$lab$fontface)) fontface <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) fontfamily <- key$lab$fontfamily
        if (!is.null(key$lab$lineheight)) lineheight <- key$lab$lineheight
        if (!is.null(key$lab$rot)) rot <- key$lab$rot
        
    }
    else stop("malformed colorkey")

    labscat <- at
    do.labels <- (length(labscat) > 0)

    if (key$space == "right")
    {
        labelsGrob <-
            if (do.labels)
                textGrob(label = labels,
                         x = rep(0, length(labscat)),
                         y = labscat,
                         vp = viewport(yscale = atrange),
                         default.units = "native",
                         check.overlap = check.overlap,
                         just = if (rot == -90) c("center", "bottom") else c("left", "center"),
                         rot = rot,
                         name = trellis.grobname("labels", type="colorkey"),
                         gp =
                         gpar(col = col,
                              cex = cex,
                              fontfamily = fontfamily,
                              fontface = chooseFace(fontface, font),
                              lineheight = lineheight))
            else nullGrob()

        heights.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        heights.units <- rep("null", 3)

        widths.x <- c(0.6 * key$width, do.labels * (0.3 + key$tck * 0.3), do.labels * 1)
        widths.units <- c("lines", "lines", "grobwidth")
        widths.data <- list(NULL, NULL, labelsGrob)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units),
                        widths = unit(widths.x, widths.units, data = widths.data),
                        respect = TRUE)

        key.gf <- frameGrob(layout = key.layout, vp = vp,
                            name = trellis.grobname("frame",
                              type="colorkey"))
        if (key$raster)
        {
            key.gf <- placeGrob(key.gf,
                                rasterGrob(matrix(rev(key$col), ncol = 1),
                                           width = 1, height = 1,
                                           vp = viewport(clip = "on"),
                                           name = trellis.grobname("raster",
                                                                   type="colorkey"),
                                           interpolate = key$interpolate),
                                row = 2, col = 1)
        }
        else
        {
            key.gf <- placeGrob(key.gf,
                                rectGrob(x = rep(.5, length(reccentre)), 
                                         y = reccentre,
                                         default.units = "native",
                                         vp = viewport(yscale = atrange),
                                         height = recdim, 
                                         name = trellis.grobname("image",
                                                                 type="colorkey"),
                                         gp =
                                         gpar(fill = key$col,
                                              col = "transparent",
                                              alpha = key$alpha)),
                                row = 2, col = 1)
        }
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(name = trellis.grobname("border",
                                       type="colorkey"),
                                     gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd,
                                          alpha = axis.line$alpha,
                                          fill = "transparent")),
                            row = 2, col = 1)
        if (do.labels)
        {
            if (key$tck != 0)
                key.gf <- placeGrob(frame = key.gf, 
                                    segmentsGrob(x0 = rep(0, length(labscat)),
                                                 y0 = labscat,
                                                 x1 = rep(key$tck / (1 + key$tck), length(labscat)),
                                                 y1 = labscat,
                                                 vp = viewport(yscale = atrange),
                                                 default.units = "native",
                                                 name = trellis.grobname("ticks",
                                                                         type="colorkey"),
                                                 gp =
                                                 gpar(col = axis.line$col,
                                                      lty = axis.line$lty,
                                                      lwd = axis.line$lwd)),
                                    row = 2, col = 2)
            key.gf <- placeGrob(key.gf,
                                labelsGrob, 
                                row = 2, col = 3)
        }
    }
    else if (key$space == "left")
    {
        labelsGrob <-
            if (do.labels)
                textGrob(label = labels,
                         x = rep(1, length(labscat)),
                         y = labscat,
                         vp = viewport(yscale = atrange),
                         default.units = "native",
                         check.overlap = check.overlap,
                         just = if (rot == 90) c("center", "bottom") else c("right", "center"),
                         rot = rot,
                         name = trellis.grobname("labels", type="colorkey"),
                         gp =
                         gpar(col = col,
                              cex = cex,
                              fontfamily = fontfamily,
                              fontface = chooseFace(fontface, font),
                              lineheight = lineheight))
            else nullGrob()

        heights.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        heights.units <- rep("null", 3)


        widths.x <- c(do.labels * 1, do.labels * (0.3 + key$tck * 0.3), 0.6 * key$width)
        widths.units <- c("grobwidth", "lines", "lines")
        widths.data <- list(labelsGrob, NULL, NULL)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units),
                        widths = unit(widths.x, widths.units, data = widths.data),
                        respect = TRUE)

        key.gf <- frameGrob(layout = key.layout, vp = vp,
                            name = trellis.grobname("frame",
                              type="colorkey"))
        if (key$raster)
        {
            key.gf <- placeGrob(key.gf,
                                rasterGrob(matrix(rev(key$col), ncol = 1),
                                           width = 1, height = 1,
                                           vp = viewport(clip = "on"),
                                           name = trellis.grobname("raster",
                                             type="colorkey"),
                                           interpolate = key$interpolate),
                                row = 2, col = 3)
        }
        else
        {
            key.gf <- placeGrob(key.gf,
                                rectGrob(x = rep(.5, length(reccentre)), 
                                         y = reccentre,
                                         default.units = "native",
                                         vp = viewport(yscale = atrange),
                                         height = recdim, 
                                         name = trellis.grobname("image",
                                           type="colorkey"),
                                         gp =
                                         gpar(fill = key$col,
                                              col = "transparent",
                                              alpha = key$alpha)),
                                row = 2, col = 3)
        }
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(name = trellis.grobname("border",
                                             type="colorkey"),
                                     gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd,
                                          alpha = axis.line$alpha,
                                          fill = "transparent")),
                            row = 2, col = 3)
        if (do.labels)
        {
            if (key$tck != 0)
                key.gf <- placeGrob(frame = key.gf, 
                                    segmentsGrob(x0 = rep(1, length(labscat)),
                                                 y0 = labscat,
                                                 x1 = rep(1 - key$tck / (1 + key$tck), length(labscat)),
                                                 y1 = labscat,
                                                 vp = viewport(yscale = atrange),
                                                 default.units = "native",
                                                 name = trellis.grobname("ticks",
                                                                         type="colorkey"),
                                                 gp =
                                                 gpar(col = axis.line$col,
                                                      lty = axis.line$lty,
                                                      lwd = axis.line$lwd)),
                                    row = 2, col = 2)
            key.gf <- placeGrob(key.gf,
                                labelsGrob, 
                                row = 2, col = 1)
        }
    }
    else if (key$space == "top")
    {
        labelsGrob <-
            if (do.labels)
                textGrob(label = labels,
                         y = rep(0, length(labscat)),
                         x = labscat,
                         vp = viewport(xscale = atrange),
                         default.units = "native",
                         check.overlap = check.overlap,
                         just = if (rot == 0) c("center","bottom") else c("left", "center"),
                         rot = rot,
                         name = trellis.grobname("labels", type="colorkey"),
                         gp =
                         gpar(col = col,
                              cex = cex,
                              fontfamily = fontfamily,
                              fontface = chooseFace(fontface, font),
                              lineheight = lineheight))
            else nullGrob()

        widths.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        widths.units <- rep("null", 3)

        heights.x <- c(do.labels * 1, do.labels * (0.3 + key$tck * 0.3), 0.6 * key$width)
        heights.units <- c("grobheight", "lines", "lines")
        heights.data <- list(labelsGrob, NULL, NULL)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units, data = heights.data),
                        widths = unit(widths.x, widths.units),
                        respect = TRUE)
        
        key.gf <- frameGrob(layout = key.layout, vp = vp,
                            name = trellis.grobname("frame",
                              type="colorkey"))
        if (key$raster)
        {
            key.gf <- placeGrob(key.gf,
                                rasterGrob(matrix(key$col, nrow = 1),
                                           width = 1, height = 1,
                                           vp = viewport(clip = "on"),
                                           name = trellis.grobname("raster",
                                             type="colorkey"),
                                           interpolate = key$interpolate),
                                row = 3, col = 2)
        }
        else
        {
            key.gf <- placeGrob(key.gf,
                                rectGrob(y = rep(.5, length(reccentre)), 
                                         x = reccentre,
                                         default.units = "native",
                                         vp = viewport(xscale = atrange),
                                         width = recdim, 
                                         name = trellis.grobname("image",
                                           type="colorkey"),
                                         gp =
                                         gpar(fill = key$col,
                                              col = "transparent",
                                              alpha = key$alpha)),
                                row = 3, col = 2)
        }
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(name = trellis.grobname("border",
                                             type="colorkey"),
                                     gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd,
                                          alpha = axis.line$alpha,
                                          fill = "transparent")),
                            row = 3, col = 2)
        if (do.labels)
        {
            if (key$tck != 0)
                key.gf <- placeGrob(frame = key.gf, 
                                    segmentsGrob(y0 = rep(0, length(labscat)),
                                                 x0 = labscat,
                                                 y1 = rep(key$tck / (1 + key$tck), length(labscat)),
                                                 x1 = labscat,
                                                 vp = viewport(xscale = atrange),
                                                 default.units = "native",
                                                 name = trellis.grobname("ticks",
                                                                         type="colorkey"),
                                                 gp =
                                                 gpar(col = axis.line$col,
                                                      lty = axis.line$lty,
                                                      lwd = axis.line$lwd)),
                                    row = 2, col = 2)
            key.gf <- placeGrob(key.gf,
                                labelsGrob, 
                                row = 1, col = 2)
        }
    }
    else if (key$space == "bottom")
    {
        labelsGrob <-
            if (do.labels)
                textGrob(label = labels,
                         y = rep(1, length(labscat)),
                         x = labscat,
                         vp = viewport(xscale = atrange),
                         default.units = "native",
                         check.overlap = check.overlap,
                         just = if (rot == 0) c("center", "top") else c("right", "center"),
                         rot = rot,
                         name = trellis.grobname("labels", type="colorkey"),
                         gp =
                         gpar(col = col,
                              cex = cex,
                              fontfamily = fontfamily,
                              fontface = chooseFace(fontface, font),
                              lineheight = lineheight))
            else nullGrob()

        widths.x <- c((1 - key$height) / 2, key$height, (1 - key$height) / 2)
        widths.units <- rep("null", 3)

        heights.x <- c(0.6 * key$width, do.labels * (0.3 + key$tck * 0.3), do.labels * 1)
        heights.units <- c("lines", "lines", "grobheight")
        heights.data <- list(NULL, NULL, labelsGrob)
        
        key.layout <-
            grid.layout(nrow = 3, ncol = 3,
                        heights = unit(heights.x, heights.units, data = heights.data),
                        widths = unit(widths.x, widths.units),
                        respect = TRUE)
        
        key.gf <- frameGrob(layout = key.layout, vp = vp,
                            name = trellis.grobname("frame",
                              type="colorkey"))
        if (key$raster)
        {
            key.gf <- placeGrob(key.gf,
                                rasterGrob(matrix(key$col, nrow = 1),
                                           width = 1, height = 1,
                                           vp = viewport(clip = "on"),
                                           name = trellis.grobname("raster",
                                             type="colorkey"),
                                           interpolate = key$interpolate),
                                row = 1, col = 2)
        }
        else
        {
            key.gf <- placeGrob(key.gf,
                                rectGrob(y = rep(.5, length(reccentre)), 
                                         x = reccentre,
                                         default.units = "native",
                                         vp = viewport(xscale = atrange),
                                         width = recdim, 
                                         name = trellis.grobname("image",
                                           type="colorkey"),
                                         gp =
                                         gpar(fill = key$col,
                                              col = "transparent",
                                              alpha = key$alpha)),
                                row = 1, col = 2)
        }
        key.gf <- placeGrob(frame = key.gf, 
                            rectGrob(name = trellis.grobname("image",
                                       type="colorkey"),
                                     gp =
                                     gpar(col = axis.line$col,
                                          lty = axis.line$lty,
                                          lwd = axis.line$lwd,
                                          alpha = axis.line$alpha,
                                          fill = "transparent")),
                            row = 1, col = 2)
        if (do.labels)
        {
            if (key$tck != 0)
                key.gf <- placeGrob(frame = key.gf, 
                                    segmentsGrob(y0 = rep(1, length(labscat)),
                                                 x0 = labscat,
                                                 y1 = rep(1 - key$tck / (1 + key$tck), length(labscat)),
                                                 x1 = labscat,
                                                 vp = viewport(xscale = atrange),
                                                 default.units = "native",
                                                 name = trellis.grobname("ticks",
                                                                         type="colorkey"),
                                                 gp =
                                                 gpar(col = axis.line$col,
                                                      lty = axis.line$lty,
                                                      lwd = axis.line$lwd)),
                                    row = 2, col = 2)
            key.gf <- placeGrob(key.gf,
                                labelsGrob, 
                                row = 3, col = 2)
        }
    }
    if (draw)
        grid.draw(key.gf)
    key.gf
}


