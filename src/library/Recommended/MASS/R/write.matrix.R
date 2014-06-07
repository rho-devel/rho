# file MASS/R/write.matrix.R
# copyright (C) 1994-2001 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
write.matrix <- function(x, file = "", sep = " ", blocksize)
{
    x <- as.matrix(x)
    p <- ncol(x)
    cn <- colnames(x)
    if(!missing(blocksize) && blocksize > 0L) {
        cat(cn, file=file, sep=c(rep(sep, p-1L), "\n"))
        nlines <- 0
        nr <- nrow(x)
        while (nlines < nr) {
            nb <- min(blocksize, nr - nlines)
            cat(format(t(x[nlines + (1L:nb), ])),
                file = file, append = TRUE,
                sep = c(rep(sep, p-1L), "\n"))
            nlines <- nlines + nb
        }
    } else
        cat(c(cn, format(t(x))), file=file, sep=c(rep(sep, p-1L), "\n"))
}
