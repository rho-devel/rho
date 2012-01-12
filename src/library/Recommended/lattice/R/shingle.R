

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




"[.shingle" <-
    function(x, subset, drop = FALSE)
{
    if (!is.shingle(x)) stop("x must be a shingle")
    ans <- as.numeric(x)[subset]
    attr(ans, "levels") <- levels(x)
    class(attr(ans, "levels")) <- "shingleLevel"
    if (drop) {
        xlvs <- levels(ans)
        dl <- logical(nlevels(ans))
        for (i in seq_along(dl))
            dl[i] <- any( ans >= xlvs[[i]][1] & ans <= xlvs[[i]][2] )
        attr(ans, "levels") <- xlvs[dl]
        class(attr(ans, "levels")) <- "shingleLevel"
    }
    class(ans) <- "shingle"
    ans
}





make.list.from.intervals <- function(x)
{
    if (ncol(x) != 2) stop("x must be matrix with 2 columns")
    ans <- vector(mode = "list", length = nrow(x))
    for (i in seq_len(nrow(x))) ans[[i]] <- x[i,]
    ans
}



equal.count <-
    function(x, ...)
{
    attr(x, "levels") <- make.list.from.intervals(co.intervals(x,...))
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}



shingle <-
    function(x, intervals=sort(unique(x)))
{
    if (ncol(as.matrix(intervals))==1)
        intervals <- cbind(intervals, intervals, deparse.level = 0)
    else if (ncol(as.matrix(intervals)) > 2)
        stop("bad value of 'intervals'")
    attr(x, "levels") <- make.list.from.intervals(intervals)
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}


as.data.frame.shingle <- as.data.frame.factor

is.shingle <-
    function(x) inherits(x, "shingle")


as.shingle <-
    function(x) if (is.shingle(x)) x else shingle(x)



summary.shingle <-
    function(object, showValues = FALSE, ...)
    print.shingle(object, showValues = showValues, ...)



as.character.shingleLevel <- function(x, ...)
{
    interval2string <- function(x)
    {
        stopifnot(length(x) == 2)
        if (x[1] == x[2]) paste("{ ", x[1], " }", sep = "")
        else paste("[ ", x[1], ", ", x[2], " ]", sep = "")
    }
    sapply(x, interval2string)
}


print.shingleLevel <- function(x, ...)
{
    print(do.call("rbind", x))
    invisible(x)
}


print.shingle <- function(x, showValues = TRUE, ...)
{
    if (showValues)
    {
        cat(gettext("\nData:\n"))
        print(as.numeric(x))
    }
    l <- levels(x)
    n <- nlevels(x)
    if (n < 1) cat(gettext("\nno intervals\n"))
    else {
        int <- data.frame(min = numeric(n), max = numeric(n), count = numeric(n))
        for (i in 1:n) {
            int$min[i] <- l[[i]][1]
            int$max[i] <- l[[i]][2]
            int$count[i] <- length(x[x>=l[[i]][1] & x<=l[[i]][2]])
        }
        cat(gettext("\nIntervals:\n"))
        print(int)
        olap <- numeric(n-1)
        for (i in seq_len(n - 1))
            olap[i] <-
                length(x[x >= l[[i]][1] & x <= l[[i]][2] &
                         x >= l[[i+1]][1] & x <= l[[i+1]][2]])
        cat(gettext("\nOverlap between adjacent intervals:\n"))
        print(olap)
    }
    invisible(x)
}



plot.shingle <-
    function(x, 
             panel = panel.shingle,
             xlab = gettext("Range"),
             ylab = gettext("Panel"),
             ...)
{
    ocall <- sys.call(sys.parent()); ocall[[1]] <- quote(plot)
    panel.shingle <-
        function(x, y,
                 col = plot.polygon$col,
                 lty = plot.polygon$lty,
                 lwd = plot.polygon$lwd,
                 alpha = plot.polygon$alpha,
                 border = plot.polygon$border,
                 ...)
        {
            plot.polygon <- trellis.par.get("plot.polygon")
            n <- nlevels(y)
            if (n > 0)
                lrect(xleft = x[1 + 2 * (0:(n-1))],
                      xright = x[2 + 2 * (0:(n-1))],
                      y = 1:n,
                      height = 0.5,
                      col = col,
                      lty = lty,
                      alpha = alpha,
                      border = border,
                      ...)
        }
    x <- levels(x)
    ans <-
        bwplot(factor(rep(seq_len(length(x)), each = 2)) ~ unlist(x),
               xlab = xlab, ylab = ylab,
               panel = panel, ...)
    ans$call <- ocall
    ans
}


## as.factor.shingle <- function(x)
## {
##     slevels <- unlist(levels(x))
##     if (is.unsorted(slevels))
##     {
##         ## Overlapping levels. Will use overlaps as additional levels
##         breaks <- sort(unique(slevels))
##     }
##     else if (any(slevels[c(TRUE, FALSE)] == slevels[c(FALSE, TRUE)]))
##     {
##         ## Some intervals are singleton points.
##         ## FIXME: This doesn't work
##         breaks <- unique(slevels)
##     }
##     else 
##     {
##         breaks <- unique(slevels)
##     }
##     cut(as.numeric(x), breaks = breaks,
##         include.lowest = TRUE, right = TRUE)[drop = TRUE]
## }

