### This file is part of the 'foreign' package for R.

# Copyright 2004 by Roger Bivand
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

read.systat <- function(file, to.data.frame=TRUE)
{
    if (length(file) != 1L) stop("only one file")
    if (!is.character(file)) stop("'file' must be character")
    res <- .Call(readSystat, as.character(file))
    if (to.data.frame) {
        comment <- NULL
        if (!is.null(attr(res, "comment")) &&
            nzchar(attr(res, "comment")))
            comment <- attr(res, "comment")
        res <- as.data.frame(res)
        if (!is.null(comment)) comment(res) <- comment
    }
    res
}
