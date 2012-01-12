
### Copyright (C) 2001-2011  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
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


panel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             lwd = plot.line$lwd,
             lty = plot.line$lty,
             col,
             col.line = plot.line$col,
             type, ## ignored
             horizontal = FALSE,
             ...,
             identifier = "loess")
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return()

    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal)
    {
        smooth <-
            loess.smooth(y[ok], x[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$y, y = smooth$x,
                    col = col.line, lty = lty, lwd = lwd, ...,
                    identifier = identifier)
    }
    else
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$x, y = smooth$y,
                    col = col.line, lty = lty, lwd = lwd, ...,
                    identifier = identifier)
    }
    smooth
}



prepanel.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             horizontal = FALSE,
             ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return(prepanel.null())
    if (horizontal)
    {
        smooth <-
            loess.smooth(y[ok], x[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        list(xlim = range(x, smooth$y, finite = TRUE),
             ylim = range(y, smooth$x, finite = TRUE),
             dx = diff(smooth$y),
             dy = diff(smooth$x))
    }
    else 
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        list(xlim = range(x, smooth$x, finite = TRUE),
             ylim = range(y, smooth$y, finite = TRUE),
             dx = diff(smooth$x),
             dy = diff(smooth$y))
    }
}



# panel.smooth <-
#     function(x, y, span = 2/3, degree = 1, zero.line = FALSE,
#              family = c("symmetric", "gaussian"),
#              evaluation = 50,
#              lwd = plot.line$lwd, lty = plot.line$lty,
#              col = plot.line$col, ...)
# {
#     if (zero.line) abline(h=0, ...)
#     panel.loess(x, y, span = span, family = family,
#                 degree = degree, evaluation = evaluation, ...)
#     panel.xyplot(x, y, ...)
# }
## base R function exists



## panel.loess <-
##     function(x, y, span = 2/3, degree = 1,
##              family = c("symmetric", "gaussian"),
##              evaluation = 50,
##              lwd = plot.line$lwd,
##              lty = plot.line$lty,
##              col,
##              col.line = plot.line$col,
##              type, ## ignored
##              horizontal = FALSE,
##              ...,
##              identifier = "loess")



panel.spline <-
    function(x, y, npoints = 101,
             lwd = plot.line$lwd,
             lty = plot.line$lty,
             col, col.line = plot.line$col,
             type, # ignored
             horizontal = FALSE, ...,
             keep.data = FALSE,
             identifier = "spline")
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return()
    if (!missing(col))
    {
        if (missing(col.line)) col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal)
    {
        args <- list(x = y[ok], y = x[ok], ..., keep.data = keep.data)
        smooth <- checkArgsAndCall(smooth.spline, args)
        yy <- do.breaks(range(y[ok]), npoints)
        p <- predict(smooth, x = yy)
        panel.lines(p$y, p$x, col = col.line, lty = lty, lwd = lwd,
                    ..., identifier = identifier)
    }
    else
    {
        args <- list(x = x[ok], y = y[ok], ..., keep.data = keep.data)
        smooth <- checkArgsAndCall(smooth.spline, args)
        xx <- do.breaks(range(x[ok]), npoints)
        p <- predict(smooth, x = xx)
        panel.lines(p$x, p$y, col = col.line, lty = lty, lwd = lwd,
                    ..., identifier = identifier)
    }
    smooth
}

prepanel.spline <-
    function(x, y, npoints = 101, 
             horizontal = FALSE, ...,
             keep.data = FALSE)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return(prepanel.null())
    if (horizontal)
    {
        args <- list(x = y[ok], y = x[ok], ..., keep.data = keep.data)
        smooth <- checkArgsAndCall(smooth.spline, args)
        yy <- do.breaks(range(y[ok]), npoints)
        p <- predict(smooth, x = yy)
        list(xlim = range(x, p$y, finite = TRUE),
             ylim = range(y, p$x, finite = TRUE),
             dx = diff(p$y),
             dy = diff(p$x))
    }
    else
    {
        args <- list(x = x[ok], y = y[ok], ..., keep.data = FALSE)
        smooth <- checkArgsAndCall(smooth.spline, args)
        xx <- do.breaks(range(x[ok]), npoints)
        p <- predict(smooth, x = xx)
        list(xlim = range(x, p$x, finite = TRUE),
             ylim = range(y, p$y, finite = TRUE),
             dx = diff(p$x),
             dy = diff(p$y))
    }
}



