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


summary.trellis <-
    function(object, ...)
{
    ans <- 
        with(object,
             list(call = call,
                  packet.sizes = packet.sizes,
                  index.cond = index.cond,
                  perm.cond = perm.cond))
    class(ans) <- "summary.trellis"
    ans
}



print.summary.trellis <- function(x, ...)
{
    cat(gettext("\nCall:\n"))
    print(x$call)
    cat(gettext("\nNumber of observations:\n"))
    ps <-
        do.call("[",
                c(list(x$packet.sizes),
                  x$index.cond,
                  list(drop = FALSE)))
    if (!is.null(dim(ps)))
        ps <- aperm(ps, x$perm.cond)
    print(ps)
    invisible(x)
}




## summary.trellis.old <- function(object, ...)
## {
##     cat(gettext("\nCall:\n"))
##     print(object$call)
##     cat("\nY label:\n")
##     str(object$ylab)
##     cat("\nX label:\n")
##     str(object$xlab)
##     if (!is.null(names(object$condlevels)))
##     {
##         cat("\nLevels of Conditioning variables:")
##         for (i in seq_along(object$condlevels))
##         {
##             cat("\n<", i, "> ", names(object$condlevels)[i], "\n", sep = "")
##             print(object$condlevels[[i]])
##         }
##     }
##     cat("\n")
##     invisible()
## }




dim.trellis <- function(x)
{
    olevs <- lapply(sapply(x$condlevels, length), seq)
    as.integer(sapply(mapply("[", olevs, x$index.cond,
                             SIMPLIFY = FALSE),
                      length))
}


dimnames.trellis <- function(x)
    x$condlevels


"dimnames<-.trellis" <- 
    function (x, value)
{
    d <- dim(x)
    if (!is.list(value) || length(value) != length(d))
        stop("supplied 'dimnames' have wrong length")
    if (!all(d == sapply(value, length)))
        stop("some components of supplied 'dimnames' have wrong length")
    x$condlevels <- value
    x
}


