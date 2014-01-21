

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

col.whitebg <- function()
    list(background = list(col="transparent"),
         plot.polygon = list(col="#c8ffc8"),
         box.rectangle = list(col="darkgreen"),
         box.umbrella = list(col="darkgreen"),
         dot.line = list(col="#e8e8e8"),
         dot.symbol = list(col="darkgreen"),
         plot.line = list(col="darkgreen"),
         plot.symbol = list(col="darkgreen"),
         ##regions=list(col=rev(hsv(h=250:349/1000, v=30:129/150,s=.5,
         ##gamma = .6)))
         regions = list(col = heat.colors(100)),
         strip.shingle = list(col = c("#ff7f00", "#00ff00", "#00ffff",
                                      "#0080ff", "#ff00ff", "#ff0000", "#ffff00")),
         strip.background = list(col = c("#ffe5cc", "#ccffcc", "#ccffff",
                                         "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")),
         reference.line = list(col="#e8e8e8"),
         superpose.line = list(col = c("darkgreen","red","royalblue",
                                       "brown","orange","turquoise", "orchid"),
                               lty = 1:7),
         superpose.symbol = list(pch = c(1,3,6,0,5,16,17), cex = rep(.7, 7),
                                 col = c("darkgreen","red","royalblue",
                                         "brown","orange","turquoise", "orchid")))




## this function is used to make the superpose.polygon colors less
## saturated versions of the symbol and line colors

lower.saturation <-
    function(x, f = 0.2)
{
    RGB <- col2rgb(x)
    RGB[] <- 255 - RGB
    RGB[] <- round(f * RGB)
    RGB[] <- 255 - RGB
    rgb(RGB["red", ],
        RGB["green", ],
        RGB["blue", ],
        maxColorValue = 255)
}



standard.theme <- 
canonical.theme <- function(name = .Device, color = name != "postscript")
{
    ## For the purpose of this function, the only differences in the
    ## settings/themes arise from the difference in the default
    ## colors. So, I will first set up the appropriate colors
    ## according to 'name', and then use those to create the
    ## theme. The first 16 colors correspond to trellis.settings
    ## colors, the 17th is the background color.

    if (color)
    {
        ## color colors
        can.col <-
            if (name %in% c("windows", "X11"))
                c("#000000", "#00ffff", "#ff00ff", "#00ff00",
                  "#ff7f00", "#007eff", "#ffff00", "#ff0000",
                  "#c6ffff", "#ffc3ff", "#c8ffc8", "#ffd18f",
                  "#a9e2ff", "#ffffc3", "#ff8c8a", "#aaaaaa",
                  "#909090")
            else if (name %in% c("postscript", "pdf", "xfig"))
                c("#000000", "#00ffff", "#ff00ff", "#00ff00",
                  "#ff7f00", "#0080ff", "#ffff00", "#ff0000",
                  "#ccffff", "#ffccff", "#ccffcc", "#ffe5cc",
                  "#cce6ff", "#ffffcc", "#ffcccc", "#e6e6e6",
                  "transparent")
            else ## default, same as X11 for now
                c("#000000", "#00FFFF", "#FF00FF", "#00FF00",
                  "#FF7F00", "#007EFF", "#FFFF00", "#FF0000",
                  "#C6FFFF", "#FFC3FF", "#C8FFC8", "#FFD18F",
                  "#A9E2FF", "#FFFFC3", "#FF8C8A", "#AAAAAA",
                  "#909090")
    }
    else ## b&w colors, same for all devices (8:15 mostly unnecessary)
        can.col <-
            c("#000000", "#999999", "#4C4C4C", "#E6E6E6", "#F2F2F2",
              "#B2B2B2", "#000000", "#030303", "#050505", "#080808",
              "#0A0A0A", "#0D0D0D", "#0F0F0F", "#121212", "#151515",
              "#AAAAAA", "transparent")

    ## The following is the canonical definition of what elements are
    ## valid in any setting. Adding something here should be necessary
    ## and sufficient.

    ## Note: For any component with a $font entry, more general
    ## specifications using $fontfamily and $fontface is also allowed
    ## (see ?grid::gpar for details).  If set, fontsize$text will
    ## override get.gpar("fontsize") (which in turn usually defaults
    ## to the device pointsize).

    ## color settings, modified later if postscript or color = FALSE
    ans <-
        list(grid.pars        = list(), ## set globally at the beginning
             fontsize         = list(text = 12, points = 8),
             background       = list(alpha = 1, col = can.col[17]),
             panel.background = list(col = "transparent"),
             clip             = list(panel = "on", strip = "on"),
             add.line         = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1),
             add.text         = list(alpha = 1, cex = 1, col = can.col[1], font = 1, lineheight = 1.2),
             plot.polygon     = list(alpha = 1, col = can.col[2], border = "black", lty = 1, lwd = 1),
             box.dot          = list(alpha = 1, col = can.col[1], cex = 1, font = 1, pch = 16),
             box.rectangle    = list(alpha = 1, col = can.col[2], fill = "transparent", lty = 1, lwd = 1),
             box.umbrella     = list(alpha = 1, col = can.col[2], lty = 2, lwd = 1),
             dot.line         = list(alpha = 1, col = can.col[16], lty = 1, lwd = 1),
             dot.symbol       = list(alpha = 1, cex = 0.8, col = can.col[2], font = 1, pch = 16),
             plot.line        = list(alpha = 1, col = can.col[2], lty = 1, lwd = 1),
             plot.symbol      = list(alpha = 1, cex = 0.8, col = can.col[2], font = 1, pch = 1, fill = "transparent"),
             reference.line   = list(alpha = 1, col = can.col[16], lty = 1, lwd = 1),
             strip.background = list(alpha = 1, col = can.col[c(12, 11, 9, 13, 10, 15, 14)]),
             strip.shingle    = list(alpha = 1, col = can.col[c(5, 4, 2, 6, 3, 8, 7)]),
             strip.border     = list(alpha = 1, col = rep(can.col[1], 7), lty = rep(1, 7), lwd = rep(1, 7)),
             superpose.line   = list(alpha = 1, col = can.col[2:8], lty = rep(1, 7), lwd = rep(1, 7)),
             superpose.symbol = list(alpha = rep(1, 7), cex = rep(0.8, 7), col = can.col[2:8],
                                     fill = lower.saturation(can.col[2:8]),
                                     ## WAS: fill = rep("transparent", 7),
                                     font = rep(1, 7), pch = rep(1, 7)),
             superpose.polygon= list(alpha = rep(1, 7),
                                     col = lower.saturation(can.col[2:8]), ## WAS can.col[2:8]
                                     border = rep("black", 7), lty = rep(1, 7), lwd = rep(1, 7)),
             regions          = list(alpha = 1, col = rev(cm.colors(100))),
             shade.colors     = list(alpha = 1, palette = function(irr, ref, height, saturation = .9) {
                 hsv(h = height, s = 1 - saturation * (1 - (1-ref)^0.5), v = irr)
             }),
             axis.line        = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1),
             axis.text        = list(alpha = 1, cex = .8, col = can.col[1], font = 1, lineheight = 1),

             ## NEW: controls widths of tick marks and padding of labels
             axis.components  = list(left = list(tck = 1, pad1 = 1, pad2 = 1),
                                     top = list(tck = 1, pad1 = 1, pad2 = 1),
                                     right = list(tck = 1, pad1 = 1, pad2 = 1),
                                     bottom = list(tck = 1, pad1 = 1, pad2 = 1)),
             ## NEW: controls widths of basic layout's components
             layout.heights   = list(top.padding = 1,
                                     main = 1,
                                     main.key.padding = 1,
                                     key.top = 1,
                                     xlab.top = 1,
                                     key.axis.padding = 1,
                                     axis.top = 1,
                                     strip = 1,
                                     panel = 1, ## shouldn't be changed
                                     axis.panel = 1, ## can be useful
                                     between = 1,
                                     axis.bottom = 1,
                                     axis.xlab.padding = 1,
                                     xlab = 1,
                                     xlab.key.padding = 0,
                                     key.bottom = 1,
                                     key.sub.padding = 1,
                                     sub = 1,
                                     bottom.padding = 1),
             layout.widths    = list(left.padding = 1,
                                     key.left = 1,
                                     key.ylab.padding = 0,
                                     ylab = 1,
                                     ylab.axis.padding = 1,
                                     axis.left = 1,
                                     axis.panel = 1, ## can be useful
                                     strip.left = 1,
                                     panel = 1, ## shouldn't be changed
                                     between = 1,
                                     axis.right = 1,
                                     axis.key.padding = 1,
                                     ylab.right = 1,
                                     key.right = 1,
                                     right.padding = 1),

             box.3d           = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1),
             par.xlab.text    = list(alpha = 1, cex = 1, col = can.col[1], font = 1, lineheight = 1),
             par.ylab.text    = list(alpha = 1, cex = 1, col = can.col[1], font = 1, lineheight = 1),
             par.zlab.text    = list(alpha = 1, cex = 1, col = can.col[1], font = 1, lineheight = 1),
             par.main.text    = list(alpha = 1, cex = 1.2, col = can.col[1], font = 2, lineheight = 1),
             par.sub.text     = list(alpha = 1, cex = 1, col = can.col[1], font = 2, lineheight = 1))

    if (color)
    {
        if (name == "postscript" || name == "pdf")
        {
            ans$plot.symbol$col <- can.col[6]
            ans$plot.line$col <- can.col[6]
            ans$dot.symbol$col <- can.col[6]
            ans$box.rectangle$col <- can.col[6]
            ans$box.umbrella$col <- can.col[6]
            ans$superpose.symbol$col <- c(can.col[c(6, 3, 4, 8)],
                                          "orange", "darkgreen", "brown")
            ans$superpose.symbol$col[c(3, 6)] <- ans$superpose.symbol$col[c(6, 3)]
            ans$superpose.line$col <- ans$superpose.symbol$col
        }
    }
    else {
        ## black and white settings
        ans$plot.polygon$col <- can.col[5]
###        ans$box.dot$col <- can.col[1]
        ans$box.rectangle$col <- can.col[1]
        ans$box.umbrella$col <- can.col[1]
###        ans$box.umbrella$lty <- 2
        ans$dot.line$col <- can.col[4]
        ans$dot.symbol$col <- can.col[1]
###        ans$dot.symbol$cex <- 0.85
        ans$plot.line$col <- can.col[1]
        ans$plot.symbol$col <- can.col[1]
        ## changing this to be like barplot
        ## ans$regions$col <- gray(29:128/128)
        ans$regions$col <- grey(seq(0.3^2.2, 0.9^2.2, length.out = 100)^(1/2.2))
        ans$shade.colors$palette <-
            function(irr, ref, height, w = .5)
                grey(w * irr + (1 - w) * (1 - (1-ref)^.4))
###        ans$reference.line$col <- can.col[4]
        ans$strip.background$col <- can.col[rep(5, 7)]
        ans$strip.shingle$col <- can.col[rep(6, 7)]
        ans$superpose.line$col <- can.col[rep(1, 7)]
        ans$superpose.line$lty <- 1:7
        ans$superpose.symbol$col <- can.col[rep(1, 7)]
        ans$superpose.symbol$cex <- rep(0.7, 7)
        ans$superpose.symbol$pch <- c(1,3,6,0,5,16,17)
        ans$superpose.polygon$col <- grey( (c(6, 12, 7, 11, 8, 10, 9)/15)^.8 )
        ##ans$superpose.symbol$pch <- c("o","+",">","s","w","#","{")
    }
    ans
}






## trellis.par.grep <-
##     function(pattern,
##              theme = trellis.par.get(),
##              ...)
## {
##     nms <- names(theme)
##     id <- grep(pattern, nms)
##     ## if match, leave in return value, o.w. apply recursively
##     if (length(id)) id <- -id ## id now non-matches
##     for (nm in nms[id])
##     {
##         print(nm)
##         theme[[nm]] <-
##             if (is.list(theme[[nm]]))
##                 trellis.par.grep(pattern, theme[[nm]], ...)
##             else
##                 NULL
##     }
##     if (all(sapply(theme, is.null))) NULL else theme
## }




trellis.par.get <-
    function(name = NULL)
{
    ## the default device is opened if none already open
    if (is.null(dev.list())) trellis.device()
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)

    ## just in case settings for the current device haven't been
    ## created yet, which may happen if the device is opened by x11(),
    ## say, (i.e., not by trellis.device()) and no trellis object has
    ## been printed on this device yet.

    if (is.null(lattice.theme[[.Device]])) {
        trellis.device(device = .Device, new = FALSE)
        lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    }
    if (is.null(name))
        lattice.theme[[.Device]]
    else if (name %in% names(lattice.theme[[.Device]]))
        lattice.theme[[.Device]][[name]]
    else NULL
}



trellis.par.set <-
    function(name, value, ..., theme, warn = TRUE, strict = FALSE)
{
    ## the default device is opened if none already open
    if (is.null(dev.list()))
    {
        trellis.device()
        if (warn)
            warning("Note: The default device has been opened to honour attempt to modify trellis settings")
    }


    ## if (name %in% names(lattice.theme[[.Device]])) NEEDED as a safeguard ?
    ## if (!is.list(value)) stop("value must be a list")


    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    ## make sure a list for this device is present
    if (is.null(lattice.theme[[.Device]]))
    {
        trellis.device(device = .Device, new = FALSE)
        lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    }
    ## WAS: lattice.theme[[.Device]][[name]] <- value

    if (missing(theme))
    {
        if (!missing(value))
        {
            theme <- list(value)
            names(theme) <- name
        }
        else if (!missing(name) && is.list(name))
        {
            theme <- name
        }
        else theme <- list(...)
    }
    else
    {
        if (is.character(theme)) theme <- get(theme)
        if (is.function(theme)) theme <- theme()
        if (!is.list(theme))
        {
            warning("Invalid 'theme' specified")
            theme <- NULL
        }
    }

    if (strict)
    {
        if (strict > 1L) lattice.theme[[.Device]] <- theme
        else lattice.theme[[.Device]][names(theme)] <- theme
    }
    else
        lattice.theme[[.Device]] <- updateList(lattice.theme[[.Device]], theme)
    assign("lattice.theme", lattice.theme, envir = .LatticeEnv)
    invisible()
}



trellis.device <-
    function(device = getOption("device"),
             color = !(dev.name == "postscript"),
             theme = lattice.getOption("default.theme"),
             new = TRUE,
             retain = FALSE,
             ...)
{
    ## Get device function
    if (is.character(device))
    {
        ## to make sure this works even if package grDevices is not attached
        if (new || is.null(dev.list()))
        {
            device.call <- try(get(device), silent = TRUE)
            if (inherits(device.call, "try-error"))
                device.call <-
                    try(utils::getFromNamespace(device, "grDevices"),
                        silent = TRUE)
            if (inherits(device.call, "try-error"))
                stop(gettextf("Could not find device function '%s'", device))
        }
        dev.name <- device
    }
    else
    {
        device.call <- device
        dev.name <- deparse(substitute(device))
    }

    ## Start the new device if necessary.
    ## new = FALSE ignored if no devices open.


    ## FIXME: remove this warning in some future version
    if ("bg" %in% names(list(...)))
        warning("'trellis.device' has changed, 'bg' may not be doing what you think it is")

    if (new || is.null(dev.list()))
    {
        device.call(...)
        lattice.setStatus(print.more = FALSE)
    }

    ## In the olden days, the defaults were device specific, and given
    ## by 'canonical.theme(name = .Device, color = color)'.  From R
    ## 2.3.0, this was changed so that all (color) devices now have
    ## the same defaults, namely 'canonical.theme(name = "pdf", color
    ## = color)'.  The old default can be reinstated by putting
    ## 'options(lattice.theme = "canonical.theme")' during startup, or
    ## 'lattice.options(default.theme = "canonical.theme")' after
    ## loading lattice.


    ## Make sure there's an entry for this device in the theme list
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    if (!(.Device %in% names(lattice.theme)))
    {
        lattice.theme[[.Device]] <- canonical.theme(name = "pdf", color = color)
        assign("lattice.theme", lattice.theme, envir = .LatticeEnv)
    }

    ## If retain = FALSE, overwrite with default settings for device
    if (!retain) trellis.par.set(canonical.theme(name = "pdf", color=color))

    ## get theme as list
    if (!is.null(theme) && !is.list(theme))
    {
        if (is.character(theme)) theme <- get(theme)
        if (is.function(theme)) theme <- theme()
        if (!is.list(theme))
        {
            warning("Invalid 'theme' specified")
            theme <- NULL
        }
    }

    ## apply theme
    if (!is.null(theme)) trellis.par.set(theme)
    return(invisible())
}



lset <- function(theme = col.whitebg())
{
    .Defunct("trellis.par.set")
}


show.settings <- function(x = NULL)
{
    old.settings <- trellis.par.get()
    on.exit(trellis.par.set(old.settings))
    if (!is.null(x)) trellis.par.set(x)
    theme <- trellis.par.get()
    d <- c("superpose.symbol",
           "superpose.line",
           "strip.background",
           "strip.shingle",
           "dot.[symbol, line]",
           "box.[dot, rectangle, umbrella]",
           "add.[line, text]",
           "reference.line",
           "plot.[symbol, line]",
           "plot.shingle[plot.polygon]",
           "histogram[plot.polygon]",
           "barchart[plot.polygon]",
           "superpose.polygon",
           "regions")
    d <- factor(d, levels = d)

    ## We only draw a border box for some panels.  To do this, we make
    ## axis.line globally transparent, and then draw the border on a
    ## case-by-case basis.  But to do that, we need to store the
    ## original axis.line settings.
    par.box <- trellis.par.get("axis.line")
    panel.box <- function()
    {
        panel.fill(col = "transparent",
                   border = adjustcolor(par.box$col, par.box$alpha),
                   lty = par.box$lty,
                   lwd = par.box$lwd)
    }
    
    xyplot(d ~ d | d,
           prepanel = function(x, y) {
               list(ylim = c(0, 1),
                    xlim = as.character(x),
                    xat = 1)
           },
           panel = function(x, y) {
               cpl <- current.panel.limits() # ylim = c(0, 1)
               rsx <- function(u) # (ReScaleX) map from [0,1]
               {
                   cpl$xlim[1] + u * diff(cpl$xlim)
               }
               switch(as.character(x),
                      "superpose.symbol" = {
                          superpose.symbol <- trellis.par.get("superpose.symbol")
                          len <- max(2, sapply(superpose.symbol, length))
                          panel.superpose(x = rep(rsx(ppoints(len)), len),
                                          y = rep(ppoints(len), each = len),
                                          groups = gl(len, len),
                                          subscripts = 1:(len*len))
                      },
                      "superpose.line" = {
                          superpose.line <- trellis.par.get("superpose.line")
                          len <- max(2, sapply(superpose.line, length))
                          panel.superpose(x = rep(rsx(c(0,1)), len),
                                          y = rep(ppoints(1:len), each = 2),
                                          groups = gl(len, 2),
                                          subscripts = 1:(2*len),
                                          type = "l")
                      },
                      "strip.background" = {
                          strip.background <- trellis.par.get("strip.background")
                          strip.border <- trellis.par.get("strip.border")
                          len <-
                              max(sapply(strip.background, length),
                                  sapply(strip.border, length))
                          panel.rect(y = ppoints(len), height = 0.5 / len,
                                     xleft = cpl$xlim[1], xright = cpl$xlim[2],
                                     col = adjustcolor(strip.background$col, strip.background$alpha),
                                     border = strip.border$col,
                                     lty = strip.border$lty,
                                     lwd = strip.border$lwd)
                      },
                      "strip.shingle" = {
                          strip.shingle <- trellis.par.get("strip.shingle")
                          len <- max(sapply(strip.shingle, length))
                          panel.rect(y = ppoints(len), height = 0.5 / len,
                                     xleft = cpl$xlim[1], xright = cpl$xlim[2],
                                     col = adjustcolor(strip.shingle$col, strip.shingle$alpha),
                                     border = "transparent")
                      },
                      "dot.[symbol, line]" = {
                          panel.dotplot(x = rsx(ppoints(5, a = 0)), y = ppoints(5, a = 0))
                          panel.box()
                      },
                      "box.[dot, rectangle, umbrella]" = {
                          panel.bwplot(x = rsx(ppoints(5)), y = rep(0.5, 5), box.width = 0.15)
                          panel.box()
                      },
                      "add.[line, text]" = {
                          add.line <- trellis.par.get("add.line")
                          xx <- seq(0.1, 0.9, length.out = 50)
                          yy <- 0.5 + .45 * sin(0.1 + 11 * xx)
                          panel.lines(x = rsx(xx), y = yy,
                                      col = add.line$col, lty = add.line$lty, lwd = add.line$lwd)
                          panel.text(labels = c("Hello", "World"),
                                     x = rsx(c(.25, .75)), y = c(0.25, 0.75))
                          panel.box()
                      },
                      "reference.line" = {
                          panel.grid()
                          panel.box()
                      },
                      "plot.[symbol, line]" = {
                          ## plot.symbol <- trellis.par.get("plot.symbol")
                          ## plot.line <- trellis.par.get("plot.line")
                          ## x <- seq(.1, .9, length.out = 20)
                          ## y <- .9 * sin(.1+11*x)
                          xx <- seq(0.1, 0.9, length.out = 20)
                          yy <- 0.5 + .4 * sin(0.1 + 11 * xx)
                          panel.xyplot(x = rsx(xx + 0.05), y = yy + 0.01, type = "l")
                          panel.xyplot(x = rsx(xx - 0.05), y = yy - 0.01)
                          panel.box()
                      },
                      "plot.shingle[plot.polygon]" = {
                          xx <- seq(0.1, 0.4, length.out = 5)
                          yy <- ppoints(5)
                          panel.barchart(x = rsx(xx + 0.5), y = yy, origin = rsx(xx),
                                         reference = FALSE, horizontal = TRUE,
                                         box.width = 1/10)
                          panel.box()
                      },
                      "histogram[plot.polygon]" = {
                          xx <- ppoints(7, 0)
                          panel.barchart(x = rsx(xx), y = (2:8)/9, horizontal = FALSE,
                                         origin = 1/18, box.width = diff(rsx(xx))[1],
                                         reference = FALSE)
                          panel.box()
                      },
                      "barchart[plot.polygon]" = {
                          xx <- ppoints(6)
                          panel.barchart(x = rev(rsx(xx)), y = xx,
                                         origin = cpl$xlim[1], box.width = 1/12)
                          panel.box()
                      },
                      "superpose.polygon" = {
                          superpose.polygon <- trellis.par.get("superpose.polygon")
                          len <- max(2, sapply(superpose.polygon, length))
                          xx <- ppoints(len)
                          panel.barchart(x = rsx(rev(xx)), y = rep(0.5, len),
                                         groups = gl(len, 1),
                                         subscripts = seq_len(len),
                                         stack = FALSE,
                                         box.width = 0.9)
                          panel.box()
                      },
                      "regions" = {
                          panel.levelplot(x = do.breaks(cpl$xlim, 98),
                                          y = rep(0.5, 99),
                                          z = 1:99 + 0.5,
                                          at = 1:100,
                                          region = TRUE,
                                          subscripts = 1:99)
                          panel.box()
                      })
           },
           ## layout = c(4, 4),
           par.settings = modifyList(theme,
                                     list(axis.line = list(col = "transparent"),
                                          clip = list(panel = "off"))),
           as.table = TRUE, strip = FALSE, xlab = "", ylab = "",
           between = list(x = 1, y = 0.5),
           scales = list(relation = "free", 
                         y = list(draw = FALSE, axs = "i"),
                         x = list(tck = 0, axs = "r")))
}




show.settings.old <- function(x = NULL)
{
    old.settings <- trellis.par.get()
    on.exit(trellis.par.set(old.settings))

    if (!is.null(x)) trellis.par.set(x)
    theme <- trellis.par.get()

    n.row <- 13
    n.col <- 9
    heights.x <- rep(1, n.row)
    heights.units <- rep("lines", n.row)
    heights.units[c(2, 5, 8, 11)] <- "null"
    widths.x <- rep(1, n.row)
    widths.units <- rep("lines", n.row)
    widths.units[c(2, 4, 6, 8)] <- "null"
    page.layout <-
        grid.layout(nrow = n.row, ncol = n.col,
                    widths = unit(widths.x, widths.units),
                    heights = unit(heights.x, heights.units))
    if (!lattice.getStatus("print.more")) grid.newpage()
    lattice.setStatus(print.more = FALSE)
    grid.rect(gp = gpar(fill = theme$background$col,
              col = "transparent"))
    pushViewport(viewport(layout = page.layout,
                          gp = gpar(fontsize = theme$fontsize$text)))
    gp.box <-
        gpar(col = theme$axis.line$col,
             lty = theme$axis.line$lty,
             lwd = theme$axis.line$lwd,
             alpha = theme$axis.line$alpha,
             fill = "transparent")

    ## superpose.symbol
    superpose.symbol <- theme$superpose.symbol
    len <- max(2, sapply(superpose.symbol, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 2,
                          yscale = c(0,len+1),
                          xscale = c(0,len+1)))
    panel.superpose(x = rep(1:len, len),
                    y = rep(1:len, each = len),
                    groups = gl(len, len),
                    subscripts = 1:(len*len))
    popViewport()
    grid.text(label = "superpose.symbol",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 2))


    ## superpose.line
    superpose.line <- theme$superpose.line
    len <- max(2, sapply(superpose.line, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 4,
                          yscale = c(0,len+1),
                          xscale = c(0, 1)))
    panel.superpose(x = rep(c(0,1), len),
                    y = rep(1:len, each = 2),
                    groups = gl(len, 2),
                    subscripts = 1:(2*len),
                    type = "l")
    popViewport()
    grid.text(label = "superpose.line",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 4))

    ## strip.background
    strip.background <- theme$strip.background
    strip.border <- theme$strip.border
    len <-
        max(sapply(strip.background, length),
            sapply(strip.border, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 6,
                          yscale = c(0, len+1),
                          xscale = c(0, 1)))
    grid.rect(y = unit(1:len, "native"),
              height = unit(0.5, "native"),
              gp =
              gpar(fill = strip.background$col,
                   alpha = strip.background$alpha,
                   col = strip.border$col,
                   lty = strip.border$lty,
                   lwd = strip.border$lwd))
    popViewport()
    grid.text(label = "strip.background",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 6))

    ## strip.shingle
    strip.shingle <- theme$strip.shingle
    len <- max(sapply(strip.shingle, length))
    pushViewport(viewport(layout.pos.row = 2,
                          layout.pos.col = 8,
                          yscale = c(0,len+1),
                          xscale = c(0,1)))
    grid.rect(y = unit(1:len, "native"),
              height = unit(0.5, "native"),
              gp =
              gpar(fill = strip.shingle$col,
                   alpha = strip.shingle$alpha,
                   col = "transparent", lwd = 0.0001))
    popViewport()
    grid.text(label = "strip.shingle",
              vp = viewport(layout.pos.row = 3, layout.pos.col = 8))

    ## dot.[symbol, line]
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 2,
                          yscale = extend.limits(c(0,6)),
                          xscale = c(0,6)))
    panel.dotplot(x = 1:5, y = 1:5)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "dot.[symbol, line]",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 2))

    ## box.[dot, rectangle, umbrella]
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 4,
                          yscale = c(-2, 2),
                          xscale = c(0,6)))
    panel.bwplot(x = 1:5, y = rep(0, 5))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "box.[dot, rectangle, umbrella]",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 4))

    ## add.[line, text]
    add.text <- theme$add.text
    add.line <- theme$add.line
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 6,
                          yscale = c(-1,1),
                          xscale = c(0,1)))
    x <- seq(.1, .9, length.out = 50)
    y <- .9 * sin(.1+11*x)
    llines(x = x, y = y, type = "l", col = add.line$col,
           lty = add.line$lty, lwd = add.line$lwd)
    ltext(labels = c("Hello", "World"),
          x = c(.25, .75), y = c(-.5, .5))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "add.[line, text]",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 6))

    ## reference.line
    pushViewport(viewport(layout.pos.row = 5,
                          layout.pos.col = 8,
                          yscale = c(0,4),
                          xscale = c(0,4)))
    panel.grid()
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "reference.line",
              vp = viewport(layout.pos.row = 6, layout.pos.col = 8))

    ## plot.[symbol, line]
    plot.symbol <- theme$plot.symbol
    plot.line <- theme$plot.line
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 2,
                          yscale = c(-1.1,1.1),
                          xscale = c(-.1,1.1)))
    x <- seq(.1, .9, length.out = 20)
    y <- .9 * sin(.1+11*x)
    panel.xyplot(x = x+.05, y = y+.1, type = "l")
    panel.xyplot(x = x-.05, y = y-.1)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "plot.[symbol, line]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 2))

    ## plot.shingle[plot.polygon]
    plot.polygon <- theme$plot.polygon
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 4,
                          yscale = extend.limits(c(0,6)),
                          xscale = extend.limits(c(1,10))))
    grid.rect(x = c(3.5, 4.5, 5.5, 6.5, 7.5), width = rep(5,5),
              y = c(1,2,3,4,5), height = rep(.5, ,5),
              default.units = "native",
              gp =
              gpar(fill = plot.polygon$col,
                   col = plot.polygon$border,
                   alpha = plot.polygon$alpha,
                   lty = plot.polygon$lty,
                   lwd = plot.polygon$lwd))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "plot.shingle[plot.polygon]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 4))

    ## histogram[plot.polygon]
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 6,
                          yscale = extend.limits(c(0,7)),
                          xscale = extend.limits(c(0.5,7.5))))
    panel.histogram(x = rep(1:7, 1:7), breaks = 0:7 + 0.5, type = "count")
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "histogram[plot.polygon]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 6))

    ## barchart[plot.polygon]
    pushViewport(viewport(layout.pos.row = 8,
                          layout.pos.col = 8,
                          yscale = extend.limits(c(0.5,6.5)),
                          xscale = c(-1,7)))
    panel.barchart(x = 6:1, y = 1:6)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "barchart[plot.polygon]",
              vp = viewport(layout.pos.row = 9, layout.pos.col = 8))


    ## superpose.polygon
    superpose.polygon <- trellis.par.get("superpose.polygon")
    len <- max(2, sapply(superpose.polygon, length))
    pushViewport(viewport(layout.pos.row = 11,
                          layout.pos.col = 2,
                          yscale = extend.limits(c(-.45, .45)),
                          xscale = c(-1, len+1)))
    panel.barchart(x = len:1, y = rep(0, len),
                   groups = gl(len, 1),
                   subscripts = 1:len,
                   stack = FALSE)
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "superpose.polygon",
              vp = viewport(layout.pos.row = 12, layout.pos.col = 2))

    ## regions
    regions <- theme$regions
    len <- length(regions$col)
    pushViewport(viewport(layout.pos.row = 11,
                          layout.pos.col = 4,
                          xscale = c(0,len+1)))
    grid.rect(x = 1:len, width = 1,
              default.units = "native",
              gp =
              gpar(col = "transparent",
                   fill = regions$col,
                   alpha = regions$alpha))
    grid.rect(gp = gp.box)
    popViewport()
    grid.text(label = "regions",
              vp = viewport(layout.pos.row = 12, layout.pos.col = 4))
    invisible()
}









## non-graphical options and layout defaults (in terms of grid units)

lattice.getOption <- function(name)
{
    get("lattice.options", envir = .LatticeEnv)[[name]]
}


## FIXME: lattice.options(foo == 1) doesn't work?
lattice.options <- function(...)
{
    ## this would have been really simple if only form allowed were
    ## lattice.options("foo", "bar") and
    ## lattice.options(foo=1, bar=2). But it could also be
    ## lattice.options(foo=1, "bar"), which makes some juggling necessary

    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .LatticeEnv$lattice.options
    ## any reason to prefer get("lattice.options", envir = .LatticeEnv)?

    ## if no args supplied, returns full options list
    if (length(new) == 0) return(old)

    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)]) ## typically getting options, not setting
    isNamed <- nm != "" ## typically all named when setting, but could have mix
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

    ## so now everything has non-"" names, but only the isNamed ones should be set
    ## everything should be returned, however

    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    ## this used to be

    ## modified <- updateList(retVal[nm], new[nm])
    ## .LatticeEnv$lattice.options[names(modified)] <- modified

    ## but then calling lattice.options(foo = NULL) had no effect
    ## because foo would be missing from modified.  So, we now do:

    .LatticeEnv$lattice.options <- updateList(old, new[nm])

    ## return changed entries invisibly
    invisible(retVal)
}



.defaultLatticeOptions <- function()
    list(save.object = TRUE,
         panel.error = "panel.error",
         drop.unused.levels = list(cond = TRUE, data = TRUE),
         default.theme = getOption("lattice.theme"), ## for back compatibility, usually NULL
         legend.bbox = "panel", ## for key$space = "inside"

         banking = banking,

         histogram.breaks = NULL,

         default.args =
         list(as.table = FALSE,
              aspect = "fill",
              between = list(x=0, y=0),
              ##page = NULL,
              ##main = NULL,
              ##sub = NULL,
              ##par.strip.text = NULL,
              ##layout = NULL,
              skip = FALSE,
              strip = strip.default,
              xscale.components = xscale.components.default,
              yscale.components = yscale.components.default,
              axis = axis.default),

         ## extends limits by this amount, to provide padding for
         ## numeric and factor scales respectively. Note that the
         ## value for numeric is multiplicative, while factor is
         ## additive

         axis.padding = list(numeric = 0.07, factor = 0.6),

         ## ticks too close to the limits will not be drawn unless
         ## explicitly requested.  Limits will be contracted by this
         ## proportion, and anything outside will be skipped.

         skip.boundary.labels = 0.02,

         ## separator for interaction when generating artificial
         ## factor (see 'allow.multiple' argument in ?xyplot)

         interaction.sep = " * ",

         ## default panel functions

         panel.contourplot = "panel.contourplot",
         panel.levelplot = "panel.levelplot",
         panel.levelplot.raster = "panel.levelplot.raster",
         panel.parallel = "panel.parallel",
         panel.densityplot = "panel.densityplot",
         panel.splom = "panel.splom",
         panel.wireframe = "panel.wireframe",
         panel.dotplot = "panel.dotplot",
         panel.qq = "panel.qq",
         panel.stripplot = "panel.stripplot",
         panel.xyplot = "panel.xyplot",
         panel.qqmath = "panel.qqmath",
         panel.barchart = "panel.barchart",
         panel.bwplot = "panel.bwplot",
         panel.histogram = "panel.histogram",
         panel.cloud = "panel.cloud",
         panel.pairs = "panel.pairs",

         ## default prepanel functions

         prepanel.default.bwplot = "prepanel.default.bwplot",
         prepanel.default.cloud = "prepanel.default.cloud",
         prepanel.default.densityplot = "prepanel.default.densityplot",
         prepanel.default.histogram = "prepanel.default.histogram",
         prepanel.default.levelplot = "prepanel.default.levelplot",
         prepanel.default.parallel = "prepanel.default.parallel",
         prepanel.default.qq = "prepanel.default.qq",
         prepanel.default.qqmath = "prepanel.default.qqmath",
         prepanel.default.splom = "prepanel.default.splom",
         prepanel.default.xyplot = "prepanel.default.xyplot",

         prepanel.default.dotplot = "prepanel.default.bwplot",
         prepanel.default.barchart = "prepanel.default.bwplot",
         prepanel.default.wireframe = "prepanel.default.cloud",
         prepanel.default.contourplot = "prepanel.default.levelplot",
         
         ## Axis units.  Rather than messing with these, end-users
         ## should manipulate corresponding settings via
         ## trellis.par.set()

         axis.units =
         list(outer =
              list(left =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   top =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   right =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   bottom =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm"))),

              inner =
              list(left =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   top =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   right =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")),
                   bottom =
                   list(tick = list(x = 2, units = "mm"),
                        pad1 = list(x = 2, units = "mm"),
                        pad2 = list(x = 2, units = "mm")))),



##          axis.units =
##          list(outer =
##               list(left =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")),
##                    top =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")),
##                    right =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")),
##                    bottom =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc"))),

##               inner =
##               list(left =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")),
##                    top =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")),
##                    right =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")),
##                    bottom =
##                    list(tick = list(x = 0.01, units = "snpc"),
##                         pad1 = list(x = 0.01, units = "snpc"),
##                         pad2 = list(x = 0.01, units = "snpc")))),



         layout.heights =

##          list(top.padding = list(x = 2, units = "mm", data = NULL),
##               main = list(x = 0, units = "grobheight", data = textGrob(label="")),
##               main.key.padding = list(x = 2, units = "mm", data = NULL),
##               key.top = list(x = 0, units = "grobheight", data = textGrob(label="")),
##               key.axis.padding = list(x = 2, units = "mm", data = NULL),
##               axis.top = list(x = 0, units = "mm", data = NULL),
##               strip = list(x = 1, units = "lines", data = NULL),
##               panel = list(x = 1, units = "null", data = NULL),
##               axis.panel = list(x = 0, units = "mm", data = NULL),
##               between = list(x = 5, units = "mm", data = NULL),
##               axis.bottom = list(x = 0, units = "mm", data = NULL),
##               axis.xlab.padding = list(x = 2, units = "mm", data = NULL),
##               xlab = list(x = 0, units = "grobheight", data = textGrob(label="")),
##               xlab.key.padding = list(x = 2, units = "mm", data = NULL),
##               key.bottom = list(x = 0, units = "grobheight", data = textGrob(label="")),
##               key.sub.padding = list(x = 2, units = "mm", data = NULL),
##               sub = list(x = 0, units = "grobheight", data = textGrob(label="")),
##               bottom.padding = list(x = 2, units = "mm", data = NULL)),
##          layout.widths =
##          list(left.padding = list(x = 2, units = "mm", data = NULL),
##               key.left = list(x = 0, units = "grobwidth", data = textGrob(label="")),
##               key.ylab.padding = list(x = 2, units = "mm", data = NULL),

##               ## changed in 2.1.0
##               ylab = list(x = 0, units = "grobwidth", data = textGrob(label="")),


##               ylab.axis.padding = list(x = 2, units = "mm", data = NULL),
##               axis.left = list(x = 0, units = "mm", data = NULL),
##               axis.panel = list(x = 0, units = "mm", data = NULL),
##               panel = list(x = 1, units = "null", data = NULL),
##               between = list(x = 5, units = "mm", data = NULL),
##               axis.right = list(x = 0, units = "mm", data = NULL),
##               axis.key.padding = list(x = 2, units = "mm", data = NULL),
##               key.right = list(x = 0, units = "grobwidth", data = textGrob(label="")),
##               right.padding = list(x = 2, units = "mm", data = NULL)),



         list(top.padding = list(x = 0.01, units = "snpc", data = NULL),
              main = list(x = 0, units = "grobheight", data = textGrob(label="")),
              main.key.padding = list(x = 0.01, units = "snpc", data = NULL),
              key.top = list(x = 0, units = "grobheight", data = textGrob(label="")),
              xlab.top = list(x = 0, units = "grobheight", data = textGrob(label="")),
              key.axis.padding = list(x = 0.01, units = "snpc", data = NULL),
              axis.top = list(x = 0, units = "mm", data = NULL),
              strip = list(x = 1, units = "lines", data = NULL),
              panel = list(x = 1, units = "null", data = NULL),
              axis.panel = list(x = 0, units = "mm", data = NULL),
              between = list(x = 5, units = "mm", data = NULL),
              axis.bottom = list(x = 0, units = "mm", data = NULL),
              axis.xlab.padding = list(x = 0.01, units = "snpc", data = NULL),
              xlab = list(x = 0, units = "grobheight", data = textGrob(label="")),
              xlab.key.padding = list(x = 0.01, units = "snpc", data = NULL),
              key.bottom = list(x = 0, units = "grobheight", data = textGrob(label="")),
              key.sub.padding = list(x = 0.01, units = "snpc", data = NULL),
              sub = list(x = 0, units = "grobheight", data = textGrob(label="")),
              bottom.padding = list(x = 0.01, units = "snpc", data = NULL)),

         layout.widths =
         list(left.padding = list(x = 0.01, units = "snpc", data = NULL),
              key.left = list(x = 0, units = "grobwidth", data = textGrob(label="")),
              key.ylab.padding = list(x = 0.01, units = "snpc", data = NULL),
              ylab = list(x = 0, units = "grobwidth", data = textGrob(label="")),
              ylab.axis.padding = list(x = 0.01, units = "snpc", data = NULL),
              axis.left = list(x = 0, units = "mm", data = NULL),
              axis.panel = list(x = 0, units = "mm", data = NULL),
              strip.left = list(x = 1, units = "lines", data = NULL),
              panel = list(x = 1, units = "null", data = NULL),
              between = list(x = 5, units = "mm", data = NULL),
              axis.right = list(x = 0, units = "mm", data = NULL),
              axis.key.padding = list(x = 0.01, units = "snpc", data = NULL),
              ylab.right = list(x = 0, units = "grobwidth", data = textGrob(label="")),
              key.right = list(x = 0, units = "grobwidth", data = textGrob(label="")),
              right.padding = list(x = 0.01, units = "snpc", data = NULL)),

         highlight.gpar = list(col = "red", lwd = 2, fill = "transparent")

         )




## Interface to internal storage for use by plot.trellis,
## trellis.focus, etc.  The optional argument prefix allows one level
## of nesting for storing plot-specific settings (for example,
## multiple plots in a page, or the panel function of one plot calling
## plot.trellis() again).

lattice.getStatus <- function(name, prefix = NULL)
{
    if (is.null(prefix))
        get("lattice.status", envir = .LatticeEnv)[[name]]
    else
        get("lattice.status", envir = .LatticeEnv)[[prefix]][[name]]
}

lattice.setStatus <- function (..., prefix = NULL, clean.first = FALSE)
{
    ## if clean.first = TRUE, remove previously existing things.  This
    ## is done whenever a new page is started, as otherwise crud from
    ## previous calls may keep piling up.
    dots <- list(...)
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
        dots <- dots[[1]]
    if (length(dots) == 0) return()
    lattice.status <-
        if (clean.first) list()
        else get("lattice.status", envir = .LatticeEnv)
    if (is.null(prefix))
        lattice.status[names(dots)] <- dots
    else
        lattice.status[[prefix]][names(dots)] <- dots
    assign("lattice.status", lattice.status, envir = .LatticeEnv)
    invisible()
}


.defaultLatticeStatus <- function()
    list(print.more = FALSE,
         plot.index = 1) ## keeps track of multiple plots in a page

.defaultLatticePrefixStatus <- function()
    list(current.plot.saved = FALSE,
         current.plot.multipage = FALSE,
         current.focus.row = 0,
         current.focus.column = 0,
         vp.highlighted = FALSE) ## keeps track of multiple plots in a page


simpleTheme <-
    function(col, alpha, 
             cex, pch, lty, lwd, font, fill, border,
             col.points, col.line, 
             alpha.points, alpha.line)
{
    ans <-
        list(plot.symbol = list(),
             plot.line = list(),
             plot.polygon = list(),
             superpose.symbol = list(),
             superpose.line = list(),
             superpose.polygon = list())
    setValue <- function(value, name, targets)
    {
        for (t in targets) ans[[t]][[name]] <<- value
    }
    if (!missing(col)) setValue(col, "col", 1:6)
    if (!missing(alpha)) setValue(alpha, "alpha", 1:6)
    if (!missing(cex)) setValue(cex, "cex", c(1, 4))
    if (!missing(pch)) setValue(pch, "pch", c(1, 4))
    if (!missing(lty)) setValue(lty, "lty", 1:6)
    if (!missing(lwd)) setValue(lwd, "lwd", 1:6)
    if (!missing(font)) setValue(font, "font", c(1, 4))
    if (!missing(fill)) setValue(fill, "fill", c(1, 3, 4, 6))
    if (!missing(border)) setValue(border, "border", c(3, 6))
    if (!missing(col.points)) setValue(col.points, "col", c(1, 4))
    if (!missing(col.line)) setValue(col.line, "col", c(2, 5))
    if (!missing(alpha.points)) setValue(alpha.points, "alpha", c(1, 4))
    if (!missing(alpha.line)) setValue(alpha.line, "alpha", c(2, 5))
    ## ensure first three only have scalars
    for (nm in c("plot.symbol", "plot.line", "plot.polygon"))
        ans[[nm]] <- lapply(ans[[nm]], head, 1)
    ans
}



