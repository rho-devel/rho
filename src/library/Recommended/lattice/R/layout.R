
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



## the pos'th entry in the unit vector x is replaced by the unit u.
## Essentially does what x[pos] <- u should have done, only u can only
## be a unit of length 1



rearrangeUnit <- function(x, pos, u)
{
    lx <- length(x)
    if (lx == 1)
        u
    else if (pos == 1)
        unit.c(u, x[(pos+1):lx])
    else if (pos == lx)
        unit.c(x[1:(pos-1)], u)
    else
        unit.c(x[1:(pos-1)], u, x[(pos+1):lx])
}











calculateGridLayout <-
    function(x,
             rows.per.page, cols.per.page,
             number.of.cond,
             panel.height = NULL, panel.width = NULL,

             main, sub,
             xlab, ylab, xlab.top, ylab.right,

             x.alternating, y.alternating,
             x.relation.same, y.relation.same,

             xaxis.rot, yaxis.rot,
             xaxis.cex, yaxis.cex,
             xaxis.lineheight, yaxis.lineheight,

             par.strip.text,

             legend)
    ## x: the trellis object
{


    ## The idea here is to create a layout with proper widths and
    ## heights (representing the requisite amounts of space required
    ## for different components of the plot -- see descriptions below)
    ## using the various units available in grid.

    ## Most of these components are fairly easy to define, with one
    ## exception -- namely those that involve axis labels. For
    ## instance, one (or more) _columns_ would usually contain the
    ## y-axis tick-labels. The width of this column is determined by
    ## ALL the y-labels; basically, the width of the column would be
    ## the MAXIMUM of the widths of the individual labels.

    ## This is in general not an easy problem, since relative width
    ## depends on the font used (also perhaps the device). Till
    ## lattice version 0.6, this was dealt with naively by treating
    ## the label with highest nchar() to be the widest. Unfortunately,
    ## this was no longer possible with labels that were
    ## expressions. So, after grid.text started supporting expression
    ## markups, the method of determining widths/heights for tick
    ## labels has changed. The new method essentially calculates the
    ## MAXIMUM of several grid UNIT objects (using calls like
    ## max(unit(...))) .

    ## The problem with this is that it is impossible to define the
    ## 'units' argument of those parts of the eventual layout when
    ## it's first being defined (it is not "null", "lines" or anything
    ## like that). So, those parts are calculated as separate units
    ## (via max.unit) and then inserted into the layout later.

    ## All this makes the code a bit difficult to follow. I just hope
    ## this gives some hints to whoever (probably me!) tries to
    ## decipher the following code on some later date.



    ## list giving positions (for indexing) of various components

    last.panel <- (rows.per.page - 1) * 4 + 10
    between.seq <- seq_len(rows.per.page-1)
    panel.seq <- seq_len(rows.per.page)

    if (!x$as.table)
    {
        between.seq <- rev(between.seq)
        panel.seq <- rev(panel.seq)
    }
    pos.heights <-
        list(top.padding       = 1,
             main              = 2,
             main.key.padding  = 3,
             key.top           = 4,
             xlab.top          = 5, #new
             key.axis.padding  = 6,
             axis.top          = 7,
             strip             = (panel.seq - 1) * 4 + 8,
             panel             = (panel.seq - 1) * 4 + 9,
             axis.panel        = (panel.seq - 1) * 4 + 10,
             between           = (between.seq - 1) * 4 + 11,
             axis.bottom       = last.panel + 1,
             axis.xlab.padding = last.panel + 2,
             xlab              = last.panel + 3,
             xlab.key.padding  = last.panel + 4,
             key.bottom        = last.panel + 5,
             key.sub.padding   = last.panel + 6,
             sub               = last.panel + 7,
             bottom.padding    = last.panel + 8)

    last.panel <- (cols.per.page - 1) * 4 + 9
    pos.widths <-
        list(left.padding      = 1,
             key.left          = 2,
             key.ylab.padding  = 3,
             ylab              = 4,
             ylab.axis.padding = 5,
             axis.left         = 6,
             axis.panel        = (seq_len(cols.per.page) - 1) * 4 + 7,
             strip.left        = (seq_len(cols.per.page) - 1) * 4 + 8,
             panel             = (seq_len(cols.per.page) - 1) * 4 + 9,
             between           = (seq_len(cols.per.page-1) - 1) * 4 + 10,
             axis.right        = last.panel + 1,
             axis.key.padding  = last.panel + 2,
             ylab.right        = last.panel + 3, # new
             key.right         = last.panel + 4,
             right.padding     = last.panel + 5)

    n.row <- sum(sapply(pos.heights, length))
    n.col <- sum(sapply(pos.widths, length))

    ## The next block applies when aspect is anything other than
    ## "fill", which means that the aspect ratio of panels are
    ## fixed. In grid terms, this means that the 'respect' argument
    ## has to be true for elements of the layout that correspond to
    ## panels.

    ## Earlier code used to set all respect entries to be TRUE in such
    ## cases (no need for matrices then), but that fails with the
    ## complicated layout necessitated by expressions (see above).

    layout.respect <- !x$aspect.fill

    if (layout.respect)
    {
        layout.respect <- matrix(0, n.row, n.col)
        layout.respect[pos.heights$panel, pos.widths$panel] <- 1
    }

    ## Shall now construct height and width components needed for the
    ## layout. See ?unit before trying to follow this.

    ## placeholders:
    heights.x <- numeric(n.row)
    heights.units <- character(n.row)
    heights.data <- vector(mode = "list", length = n.row)

    widths.x <- numeric(n.col)
    widths.units <- character(n.col)
    widths.data <- vector(mode = "list", length = n.col)

    ## default dimensions
    heights.defaults <- lattice.getOption("layout.heights")
    widths.defaults <- lattice.getOption("layout.widths")

    ## finally settings values, act as multipliers (usually all 1)
    heights.settings <- trellis.par.get("layout.heights")
    widths.settings <- trellis.par.get("layout.widths")

    ## want to convert this into a simple vector of multipliers
    #h.mult <- rep(1, n.row)
    #w.mult <- rep(1, n.col)
    #for (nm in names(heights.settings))
    #    h.mult[pos.heights[[nm]]] <- heights.settings[[nm]]
    #for (nm in names(widths.settings))
    #    w.mult[pos.widths[[nm]]] <- widths.settings[[nm]]


    ## set default units. These may be scaled by user controlled
    ## [heights/widths].settings (usually just 1)

    for (nm in names(heights.defaults))
    {
        heights.x[pos.heights[[nm]]] <-
            heights.settings[[nm]] * heights.defaults[[nm]]$x
        heights.units[pos.heights[[nm]]] <- heights.defaults[[nm]]$units
        if (!is.null(heights.defaults[[nm]]$data))
            heights.data[[ pos.heights[[nm]] ]] <- heights.defaults[[nm]]$data
    }
    for (nm in names(widths.defaults))
    {
        widths.x[pos.widths[[nm]]] <-
            widths.settings[[nm]] * widths.defaults[[nm]]$x
        widths.units[pos.widths[[nm]]] <- widths.defaults[[nm]]$units
        if (!is.null(widths.defaults[[nm]]$data))
            widths.data[[ pos.widths[[nm]] ]] <- widths.defaults[[nm]]$data
    }

    ## fine tuning:

    if (rows.per.page > 1)
        heights.x[pos.heights[["between"]]] <-
            rep(x$y.between, length.out = rows.per.page - 1) * heights.x[pos.heights[["between"]]]
    if (cols.per.page > 1)
        widths.x[pos.widths[["between"]]] <-
            rep(x$x.between, length.out = cols.per.page - 1) * widths.x[pos.widths[["between"]]]


    if (!is.null(panel.height))
    {
        heights.x[pos.heights[["panel"]]] <-
            heights.settings[["panel"]] * panel.height[[1]]
        heights.units[pos.heights[["panel"]]] <- panel.height[[2]]
        ##heights.data[[ pos.heights[["panel"]] ]] <- NULL
    }
    if (!is.null(panel.width))
    {
        widths.x[pos.widths[["panel"]]] <-
            widths.settings[["panel"]] * panel.width[[1]]
        widths.units[pos.widths[["panel"]]] <- panel.width[[2]]
        ##widths.data[[ pos.widths[["panel"]] ]] <- NULL
    }

    ## Update label data as necessary, but only if units are
    ## "grobwidth|grobheight" (the default).  Otherwise, the user
    ## wanted something different, so honor that.

    if (!is.null(main) && (heights.units[[ pos.heights[["main"]] ]] == "grobheight"))
    {
        heights.x[pos.heights[["main"]]] <- heights.settings[["main"]]
        heights.data[[ pos.heights[["main"]]  ]] <- main
    }
    if (!is.null(sub) && (heights.units[[ pos.heights[["sub"]] ]] == "grobheight"))
    {
        heights.x[pos.heights[["sub"]]] <- heights.settings[["sub"]]
        heights.data[[ pos.heights[["sub"]]  ]] <- sub
    }
    if (!is.null(xlab) && (heights.units[[ pos.heights[["xlab"]] ]] == "grobheight"))
    {
        heights.x[pos.heights[["xlab"]]] <- heights.settings[["xlab"]]
        heights.data[[ pos.heights[["xlab"]]  ]] <- xlab
    }
    if (!is.null(xlab.top) && (heights.units[[ pos.heights[["xlab.top"]] ]] == "grobheight"))
    {
        heights.x[pos.heights[["xlab.top"]]] <- heights.settings[["xlab.top"]]
        heights.data[[ pos.heights[["xlab.top"]]  ]] <- xlab.top
    }
    if (!is.null(ylab) && (widths.units[[ pos.widths[["ylab"]] ]] == "grobwidth"))
    {
        widths.x[pos.widths[["ylab"]]] <- widths.settings[["ylab"]]
        widths.data[[ pos.widths[["ylab"]]  ]] <- ylab
    }
    if (!is.null(ylab.right) && (widths.units[[ pos.widths[["ylab.right"]] ]] == "grobwidth"))
    {
        widths.x[pos.widths[["ylab.right"]]] <- widths.settings[["ylab.right"]]
        widths.data[[ pos.widths[["ylab.right"]]  ]] <- ylab.right
    }

    if (!is.null(legend))
    {
        ## Update data as necessary, but only if units are
        ## "grobwidth|grobheight" (the default).  Otherwise, the user
        ## wanted something different, so honor that.

        nl <- names(legend)
        if ("left" %in% nl && (widths.units[[ pos.widths[["key.left"]] ]] == "grobwidth"))
        {
            widths.x[ pos.widths[["key.left"]] ] <- widths.settings[["key.left"]]
            widths.data[[ pos.widths[["key.left"]] ]] <- legend$left$obj
        }
        if ("right" %in% nl && (widths.units[[ pos.widths[["key.right"]] ]] == "grobwidth"))
        {
            widths.x[ pos.widths[["key.right"]] ] <- widths.settings[["key.right"]]
            widths.data[[ pos.widths[["key.right"]] ]] <- legend$right$obj
        }
        if ("top" %in% nl && (heights.units[[ pos.heights[["key.top"]] ]] == "grobheight"))
        {
            heights.x[ pos.heights[["key.top"]] ] <- heights.settings[["key.top"]]
            heights.data[[ pos.heights[["key.top"]] ]] <- legend$top$obj
        }
        if ("bottom" %in% nl && (heights.units[[ pos.heights[["key.bottom"]] ]] == "grobheight"))
        {
            heights.x[ pos.heights[["key.bottom"]] ] <- heights.settings[["key.top"]]
            heights.data[[ pos.heights[["key.bottom"]] ]] <- legend$bottom$obj
        }
    }

    heights.x[pos.heights[["strip"]]] <-
        heights.x[pos.heights[["strip"]]] *
            if (is.logical(x$strip)) 0  # which means strip = F, strips not to be drawn
            else par.strip.text$cex * par.strip.text$lines * number.of.cond

    widths.x[pos.widths[["strip.left"]]] <-
        widths.x[pos.widths[["strip.left"]]] *
            if (is.logical(x$strip.left)) 0  # which means strip = F, strips not to be drawn
            else par.strip.text$cex * par.strip.text$lines * number.of.cond


    ## All the upcoming scary code about labels is only to determine
    ## how much space to leave for them. Much of these calculations
    ## will be repeated later before actually drawing them. 

    if (x$x.scales$draw)
    {
        axis.units <- lattice.getOption("axis.units")[["outer"]][c("top", "bottom")]
        axis.settings <- trellis.par.get("axis.components")[c("top", "bottom")]

        if (x.relation.same)
        {

            ## this means we need to allocate space for
            ## pos.heights[axis.top] and pos.heights[axis.bottom]

##             lab <- 
##                 calculateAxisComponents(x = x$x.limits,
##                                         at = x$x.scales$at,

##                                         used.at = x$x.used.at,
##                                         num.limit = x$x.num.limit,

##                                         labels = x$x.scales$lab,
##                                         logsc = x$x.scales$log,
##                                         abbreviate = x$x.scales$abbr,
##                                         minlength = x$x.scales$minl,
##                                         format.posixt = x$x.scales$format,
##                                         n = x$x.scales$tick.number)$lab

            lab.comps <- 
                x$xscale.components(x$x.limits,
                                    at = x$x.scales$at,
                                    used.at = x$x.used.at,
                                    num.limit = x$x.num.limit,
                                    labels = x$x.scales$labels,
                                    logsc = x$x.scales$log,
                                    abbreviate = x$x.scales$abbreviate,
                                    minlength = x$x.scales$minlength,
                                    format.posixt = x$x.scales$format,
                                    n = x$x.scales$tick.number)

            ## top

            if (is.logical(lab.comps$top) && !lab.comps$top)
            {
                axis.top.unit <- unit(0, "mm")
            }
            else
            {
                lab.comps.top <- 
                    if (is.logical(lab.comps$top)) # must be TRUE
                        lab.comps$bottom
                    else
                        lab.comps$top
                lab <- lab.comps.top$labels$labels

                tick.unit <-
                    unit(max(0, x$x.scales$tck[2] * axis.units$top$tick$x * axis.settings$top$tck * lab.comps.top$ticks$tck),
                         axis.units$top$tick$units)
                pad1.unit <-
                    unit(axis.units$top$pad1$x * axis.settings$top$pad1,
                         axis.units$top$pad1$units)
                pad2.unit <-
                    unit(axis.units$top$pad2$x * axis.settings$top$pad2,
                         axis.units$top$pad2$units)

                ## FIXME: dims may depend on other gpars like font (but
                ## implementing that maybe overkill)

                lab.grob <-
                    if (length(lab) > 0)
                        textGrob(label = lab,
                                 x = rep(0.5, length(lab)),
                                 y = rep(0.5, length(lab)),
                                 rot = xaxis.rot[2],
                                 gp = gpar(cex = xaxis.cex[2],
                                           lineheight = xaxis.lineheight))
                    else textGrob("")

                ## FIXME: this is slightly inefficient (refer to email
                ## exchange with Paul), in the sense that grid has to go
                ## through a lot of hoops to figure out that the
                ## dimensions of textGrob("") is 0x0.

                axis.top.unit <-
                    heights.settings[["axis.top"]] *
                        (unit(if (any(x.alternating==2 | x.alternating==3)) 1 else 0,
                              "grobheight", data = list(lab.grob)) + 
                         tick.unit + pad1.unit + pad2.unit)
            }

            
            ## bottom
            lab.comps.bottom <- lab.comps$bottom
            lab <- lab.comps.bottom$labels$labels

            tick.unit <-
                unit(max(0, x$x.scales$tck[1] * axis.units$bottom$tick$x * axis.settings$bottom$tck * lab.comps.bottom$ticks$tck),
                     axis.units$bottom$tick$units)
            pad1.unit <-
                unit(axis.units$bottom$pad1$x * axis.settings$bottom$pad1,
                     axis.units$bottom$pad1$units)
            pad2.unit <-
                unit(axis.units$bottom$pad2$x * axis.settings$bottom$pad2,
                     axis.units$bottom$pad2$units)

            lab.grob <- 
                if (length(lab) > 0)
                    textGrob(label = lab,
                             x = rep(0.5, length(lab)),
                             y = rep(0.5, length(lab)),
                             rot = xaxis.rot[1],
                             gp = gpar(cex = xaxis.cex[1],
                                       lineheight = xaxis.lineheight))
                else textGrob("")

            axis.bottom.unit <- heights.settings[["axis.bottom"]] *
                (unit(if (any(x.alternating==1 | x.alternating==3)) 1 else 0,
                      "grobheight", data = list(lab.grob)) + 
                 tick.unit + pad1.unit + pad2.unit)
        }
        else
        { # relation != same

            ## Basically need to allocate space for the tick labels.
            ## Technically, could have different heights for different
            ## rows, but don't want to go there (probably won't look
            ## good anyway). So, boils down to finding all the
            ## labels. Note that from R 2.0.0 onwards, the user can
            ## scale space for each axis individually, so such fine
            ## control is still possible

            tick.unit <-
                unit(max(0, x$x.scales$tck[1] * axis.units$bottom$tick$x *
                         axis.settings$bottom$tck),
                     axis.units$bottom$tick$units)
            pad1.unit <-
                unit(axis.units$bottom$pad1$x * axis.settings$bottom$pad1,
                     axis.units$bottom$pad1$units)
            pad2.unit <-
                unit(axis.units$bottom$pad2$x * axis.settings$bottom$pad2,
                     axis.units$bottom$pad2$units)

            ## we'll take max of ALL panels, even those that may not
            ## be present in this particular page or even this
            ## particular plot

            lab.groblist <-
                vector(mode = "list", length = length(x$x.limits))
            for (i in seq_along(x$x.limits))
            {
##                 lab <-
##                     calculateAxisComponents(x = x$x.limits[[i]],
##                                             at = if (is.list(x$x.scales$at))
##                                             x$x.scales$at[[i]] else x$x.scales$at,

##                                             used.at = x$x.used.at[[i]],
##                                             num.limit = x$x.num.limit[[i]],

##                                             labels = if (is.list(x$x.scales$lab))
##                                             x$x.scales$lab[[i]] else x$x.scales$lab,
##                                             logsc = x$x.scales$log,
##                                             abbreviate = x$x.scales$abbr,
##                                             minlength = x$x.scales$minl,
##                                             n = x$x.scales$tick.number,
##                                             format.posixt = x$x.scales$format)$lab

                lab.comps <-
                    x$xscale.components(x$x.limits[[i]],
                                        at = if (is.list(x$x.scales$at)) x$x.scales$at[[i]] else x$x.scales$at,
                                        used.at = x$x.used.at[[i]],
                                        num.limit = x$x.num.limit[[i]],
                                        labels = if (is.list(x$x.scales$labels))
                                        x$x.scales$labels[[i]] else x$x.scales$labels,
                                        logsc = x$x.scales$log,
                                        abbreviate = x$x.scales$abbreviate,
                                        minlength = x$x.scales$minlength,
                                        n = x$x.scales$tick.number,
                                        format.posixt = x$x.scales$format)

                lab <- lab.comps$bottom$labels$labels
                lab.groblist[[i]] <- 
                    if (length(lab) > 0)
                        textGrob(label = lab,
                                 x = rep(0.5, length(lab)),
                                 y = rep(0.5, length(lab)),
                                 rot = xaxis.rot[1],
                                 gp = gpar(cex = xaxis.cex[1],
                                           lineheight = xaxis.lineheight))
                    else textGrob("")
            }
            xaxis.panel.unit <-
### widthDetails(
                max(unit(rep(1, length(lab.groblist)), "grobheight", data = lab.groblist)) +
                    tick.unit + pad1.unit + pad2.unit
### )
        }
    }


    ## same for y-axes now

    if (x$y.scales$draw)
    {

        axis.units <- lattice.getOption("axis.units")[["outer"]][c("left", "right")]
        axis.settings <- trellis.par.get("axis.components")[c("left", "right")]

        if (y.relation.same)
        {

            ## this means we need to allocate space for
            ## pos.widths[axis.left] and pos.widths[axis.right]

##             lab <- 
##                 calculateAxisComponents(x = x$y.limits,
##                                         at = x$y.scales$at,
##                                         used.at = x$y.used.at,
##                                         num.limit = x$y.num.limit,
##                                         labels = x$y.scales$lab,
##                                         logsc = x$y.scales$log,
##                                         abbreviate = x$y.scales$abbr,
##                                         minlength = x$y.scales$minl,
##                                         format.posixt = x$y.scales$format,
##                                         n = x$y.scales$tick.number)$lab

            lab.comps <-
                x$yscale.components(x$y.limits,
                                    at = x$y.scales$at,
                                    used.at = x$y.used.at,
                                    num.limit = x$y.num.limit,
                                    labels = x$y.scales$labels,
                                    logsc = x$y.scales$log,
                                    abbreviate = x$y.scales$abbreviate,
                                    minlength = x$y.scales$minlength,
                                    format.posixt = x$y.scales$format,
                                    n = x$y.scales$tick.number)

            
            ## right

            if (is.logical(lab.comps$right) && !lab.comps$right)
            {
                axis.right.unit <- unit(0, "mm")
            }
            else
            {
                lab.comps.right <- 
                    if (is.logical(lab.comps$right)) # must be TRUE
                        lab.comps$left
                    else
                        lab.comps$right
                lab <- lab.comps.right$labels$labels

                tick.unit <-
                    unit(max(0, x$y.scales$tck[2] * axis.units$right$tick$x * axis.settings$right$tck * lab.comps.right$ticks$tck),
                         axis.units$right$tick$units)
                pad1.unit <-
                    unit(axis.units$right$pad1$x * axis.settings$right$pad1,
                         axis.units$right$pad1$units)
                pad2.unit <-
                    unit(axis.units$right$pad2$x * axis.settings$right$pad2,
                         axis.units$right$pad2$units)

                lab.grob <- 
                    if (length(lab) > 0)
                        textGrob(label = lab,
                                 x = rep(0.5, length(lab)),
                                 y = rep(0.5, length(lab)),
                                 rot = yaxis.rot[2],
                                 gp = gpar(cex = yaxis.cex[2],
                                           lineheight = yaxis.lineheight))
                    else textGrob("")

                axis.right.unit <- widths.settings[["axis.right"]] *
                    (unit(if (any(y.alternating==2 | y.alternating==3)) 1 else 0,
                          "grobwidth", data = list(lab.grob)) + 
                     tick.unit + pad1.unit + pad2.unit)
            }




            ## left
            lab.comps.left <- lab.comps$left
            lab <- lab.comps.left$labels$labels
            tick.unit <-
                unit(max(0, x$y.scales$tck[1] * axis.units$left$tick$x * axis.settings$left$tck * lab.comps.left$ticks$tck),
                     axis.units$left$tick$units)
            pad1.unit <-
                unit(axis.units$left$pad1$x * axis.settings$left$pad1,
                     axis.units$left$pad1$units)
            pad2.unit <-
                unit(axis.units$left$pad2$x * axis.settings$left$pad2,
                     axis.units$left$pad2$units)

            lab.grob <- 
                if (length(lab) > 0)
                    textGrob(label = lab,
                             x = rep(0.5, length(lab)),
                             y = rep(0.5, length(lab)),
                             rot = yaxis.rot[1],
                             gp = gpar(cex = yaxis.cex[1],
                                       lineheight = yaxis.lineheight))
                else textGrob("")

            axis.left.unit <- widths.settings[["axis.left"]] *
                (unit(if (any(y.alternating==1 | y.alternating==3)) 1 else 0,
                      "grobwidth", data = list(lab.grob)) + 
                 tick.unit + pad1.unit + pad2.unit)
        }
        else { # relation != same

            ## Basically need to allocate space for the tick labels.
            ## Technically, could have different heights for different
            ## rows, but don't want to go there (probably won't look
            ## good anyway). So, boils down to finding all the
            ## labels. Note that from R 2.0.0 onwards, the user can
            ## scale space for each axis individually, so such fine
            ## control is still possible

            tick.unit <-
                unit(max(0, x$y.scales$tck[1] * axis.units$left$tick$x *
                         axis.settings$left$tck),
                     axis.units$left$tick$units)
            pad1.unit <-
                unit(axis.units$left$pad1$x * axis.settings$left$pad1,
                     axis.units$left$pad1$units)
            pad2.unit <-
                unit(axis.units$left$pad2$x * axis.settings$left$pad2,
                     axis.units$left$pad2$units)

            ## we'll take max of ALL panels, even those that may not
            ## be present in this particular page or even this
            ## particular plot

            lab.groblist <-
                vector(mode = "list", length = length(x$y.limits))
            for (i in seq_along(x$y.limits))
            {
##                 lab <-
##                     calculateAxisComponents(x = x$y.limits[[i]],
##                                             at = if (is.list(x$y.scales$at))
##                                             x$y.scales$at[[i]] else x$y.scales$at,

##                                             used.at = x$y.used.at[[i]],
##                                             num.limit = x$y.num.limit[[i]],

##                                             labels = if (is.list(x$y.scales$lab))
##                                             x$y.scales$lab[[i]] else x$y.scales$lab,
##                                             logsc = x$y.scales$log,
##                                             abbreviate = x$y.scales$abbr,
##                                             minlength = x$y.scales$minl,
##                                             n = x$y.scales$tick.number,
##                                             format.posixt = x$y.scales$format)$lab

                lab.comps <-
                    x$yscale.components(x$y.limits[[i]],
                                        at = if (is.list(x$y.scales$at)) x$y.scales$at[[i]] else x$y.scales$at,
                                        used.at = x$y.used.at[[i]],
                                        num.limit = x$y.num.limit[[i]],
                                        labels = if (is.list(x$y.scales$labels))
                                        x$y.scales$labels[[i]] else x$y.scales$labels,
                                        logsc = x$y.scales$log,
                                        abbreviate = x$y.scales$abbreviate,
                                        minlength = x$y.scales$minlength,
                                        n = x$y.scales$tick.number,
                                        format.posixt = x$y.scales$format)

                lab <- lab.comps$left$labels$labels
                lab.groblist[[i]] <- 
                    if (length(lab) > 0)
                        textGrob(label = lab,
                                 x = rep(0.5, length(lab)),
                                 y = rep(0.5, length(lab)),
                                 rot = yaxis.rot[1],
                                 gp = gpar(cex = yaxis.cex[1],
                                           lineheight = yaxis.lineheight))
                    else textGrob("")
            }
            yaxis.panel.unit <-
### widthDetails(
                max(unit(rep(1, length(lab.groblist)), "grobwidth", data = lab.groblist)) +
                    tick.unit + pad1.unit + pad2.unit
###)
        }
    }


    ## Having determined heights and widths, now construct the layout:


    layout.heights <- unit(heights.x, heights.units, data = heights.data)
    layout.widths <- unit(widths.x, widths.units, data = widths.data)

    ## As mentioned above, the units for axes are 'composite' units
    ## calculated separately, and we'll now insert them in their
    ## proper place

    if (x$x.scales$draw)
    {
        if (x.relation.same)
        {
            layout.heights <-
                rearrangeUnit(layout.heights,
                              pos.heights[["axis.top"]],
                              trellis.par.get("layout.heights")[["axis.top"]] * axis.top.unit)
            layout.heights <-
                rearrangeUnit(layout.heights,
                              pos.heights[["axis.bottom"]],
                              trellis.par.get("layout.heights")[["axis.bottom"]] * axis.bottom.unit)
        }
        else
        {
            ## FIXME: this would be a good place to add custom
            ## multipliers for panel axes, once Paul fixes the grid
            ## bug

            pcol <- length(pos.heights[["axis.panel"]])
            axis.panel.mult <-
                rep(trellis.par.get("layout.heights")[["axis.panel"]],
                    length.out = pcol)
            for (i in seq_len(pcol))
                layout.heights <-
                    rearrangeUnit(layout.heights,
                                  pos.heights[["axis.panel"]][i],
                                  axis.panel.mult[i] * xaxis.panel.unit)
        }
    }
    if (x$y.scales$draw)
    {
        if (y.relation.same)
        {
            layout.widths <-
                rearrangeUnit(layout.widths,
                              pos.widths[["axis.left"]],
                              trellis.par.get("layout.widths")[["axis.left"]] * axis.left.unit)
            layout.widths <-
                rearrangeUnit(layout.widths,
                              pos.widths[["axis.right"]],
                              trellis.par.get("layout.widths")[["axis.right"]] * axis.right.unit)
        }
        else
        {
            prow <- length(pos.widths[["axis.panel"]])
            axis.panel.mult <-
                rep(trellis.par.get("layout.widths")[["axis.panel"]],
                    length.out = prow)
            for (i in seq_len(prow))
                layout.widths <-
                    rearrangeUnit(layout.widths,
                                  pos.widths[["axis.panel"]][i],
                                  axis.panel.mult[i] * yaxis.panel.unit)
        }
    }


    page.layout <-
        grid.layout(nrow = n.row, ncol = n.col,
                    widths = layout.widths,
                    heights = layout.heights,
#                    widths = w.mult * layout.widths,
#                    heights = h.mult * layout.heights,
                    respect = layout.respect)

#debug
#    assign("gridLayout", page.layout, .GlobalEnv)


    list(page.layout = page.layout,
         pos.heights = pos.heights,
         pos.widths = pos.widths)
}




## Note: the following is only meant to be used in print.trellis.  It
## supercedes the use of cupdate there.  However, cupdate will
## continue to be used by high level functions.


packet.panel.default <-
    function(layout, condlevels,
             page, row, column,
             skip, all.pages.skip = TRUE)
{
    ## assume layout has been standardized

    ## condlevels is a list, as long as the number of conditioning
    ## variables, each component being a vector of integer indices
    ## representing levels.  This must already have considered any
    ## effects of index.cond and perm.cond.

    panels.per.page <- layout[1] * layout[2]
    panels.per.row <- layout[1]
    panels.per.column <- layout[2]

    packet.order <- do.call(expand.grid, condlevels)

    ## Don't try to force the default behaviour of skip elsewhere,
    ## since this gives users the freedom to make it behave as they
    ## want.

    if (all.pages.skip)
    {
        skip <- rep(skip, length.out = panels.per.page * page)
    }
    else
    {
        skip <- rep(skip, length.out = panels.per.page)
        skip <- rep(skip, page)
    }
    ## This covers current page, which is all we care about. 
    
    panel.number <- 1 + 
        (page - 1) * panels.per.page +
            (row - 1) * panels.per.row +
                (column - 1)

    if (skip[panel.number]) return(NULL)
    panel.number <- panel.number - sum(head(skip, panel.number))
    if (panel.number > nrow(packet.order)) return(NULL)

    as.numeric(packet.order[panel.number, ])
}



