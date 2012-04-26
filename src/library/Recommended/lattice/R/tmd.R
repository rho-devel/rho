

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




prepanel.tmd.default <-
    function(x, y, ...)
{
    prepanel.default.xyplot(x = (as.numeric(x) + as.numeric(y)) / 2,
                            y = (as.numeric(y) - as.numeric(x)),
                            ...)
}




prepanel.tmd.qqmath <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL,
             subscripts, ...)
{
    if (!is.numeric(x)) x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    getxx <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            distribution(ppoints(nobs))
        else
            distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            sort(x)
        else
            quantile(x, f.value(nobs),
                     names = FALSE,
                     type = qtype,
                     na.rm = TRUE)
    }
    if (!nobs)
        prepanel.null()
    else if (!is.null(groups))
    {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        meanlist <- difflist <-
            vector(mode = "list", length = length(sx))
        for (i in seq_along(sx))
        {
            meanlist[[i]] <- (xxlist[[i]] + yylist[[1]]) / 2
            difflist[[i]] <- (yylist[[i]] - xxlist[[1]])
        }
        list(xlim = range(unlist(meanlist), na.rm = TRUE),
             ylim = range(unlist(difflist), na.rm = TRUE),
             dx = unlist(lapply(meanlist, diff)),
             dy = unlist(lapply(difflist, diff)))
    }
    else 
    {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        tmd.mean <- (xx + yy) / 2
        tmd.diff <- (yy - xx)
        list(xlim = range(tmd.mean),
             ylim = range(tmd.diff),
             dx = diff(tmd.mean),
             dy = diff(tmd.diff))
    }
}





panel.tmd.default <-
    function(x, y, groups = NULL, ...,
             identifier = "tmd")
{
    panel.abline(h=0)
    if (is.null(groups))
        panel.xyplot(x = (as.numeric(x) + as.numeric(y)) / 2,
                     y = (as.numeric(y) - as.numeric(x)),
                     ...,
                     identifier = identifier)
    else
        panel.superpose(x = (as.numeric(x) + as.numeric(y)) / 2,
                        y = (as.numeric(y) - as.numeric(x)),
                        groups = groups, ...)
}


panel.tmd.qqmath <-
    function(x,
             f.value = NULL,
             distribution = qnorm,
             qtype = 7,
             groups = NULL, 
             subscripts, ...,
             identifier = "tmd")
{
    panel.abline(h=0)
    if (!is.numeric(x)) x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    getxx <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            distribution(ppoints(nobs))
        else
            distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL,
                      nobs = sum(!is.na(x)))
    {
        if (is.null(f.value))
            sort(x)
        else
            quantile(x, f.value(nobs),
                     names = FALSE,
                     type = qtype,
                     na.rm = TRUE)
    }
    if (!nobs)
        NULL
    else if (!is.null(groups))
    {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        xx <- unlist(xxlist)
        yy <- unlist(yylist)
        tmd.mean <- (xx + yy) / 2
        tmd.diff <- (yy - xx)
        tmd.groups <-
            gl(length(xxlist),
               sapply(xxlist, length),
               labels = names(xxlist))
        tmd.subscripts <- seq_along(xx)
        panel.superpose(x = tmd.mean, y = tmd.diff,
                        groups = tmd.groups,
                        subscripts = tmd.subscripts,
                        ...)
    }
    else 
    {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        tmd.mean <- (xx + yy) / 2
        tmd.diff <- (yy - xx)
        panel.xyplot(x = tmd.mean, y = tmd.diff, ...,
                     identifier = identifier)
    }
}




tmd <- function(object, ...)  UseMethod("tmd")

tmd.formula <-
    function(object, data = NULL, ...)
    tmd(xyplot(object, data = data, ...))

tmd.trellis <-
    function(object,
             xlab = "mean",
             ylab = "difference",
             panel = if (qqmath) panel.tmd.qqmath else panel.tmd.default,
             prepanel = if (qqmath) prepanel.tmd.qqmath else prepanel.tmd.default,
             ...)
{
    ## data x, y are not always in panel.args (they may be in
    ## panel.args.common), but they are for xyplot and qq, which are
    ## all this is supposed to work for (also qqmath, but see below).
    ## One special case is qqmath, which is treated differently.  May
    ## modify this for others if there's demand.

    qqmath <- object$call[[1]] == quote(qqmath) ## FIXME bad hack (use class(x) = c("trellis", "qqmath") instead?)
    object$xlab.default <- gettext("mean")
    object$ylab.default <- gettext("difference")

    update(object,
           xlab = xlab, ylab = ylab,
           panel = panel,
           prepanel = prepanel,
           ...)
}
                

