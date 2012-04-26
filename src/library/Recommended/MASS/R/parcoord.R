# file MASS/R/R/parcoord.R
# copyright (C) 2002 W. N. Venables and B. D. Ripley
#
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

parcoord <- function(x, col = 1, lty = 1, var.label = FALSE, ...)
{
    rx <- apply(x, 2L, range, na.rm = TRUE)
    x <- apply(x, 2L, function(x) (x - min(x, na.rm=TRUE))/
        (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
    matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty,
            xlab="", ylab = "",
            axes = FALSE, ...)
    axis(1, at = 1L:ncol(x), labels = colnames(x))
    for(i in 1L:ncol(x)) {
        lines(c(i, i), c(0, 1), col = "grey70")
        if(var.label)
            text(c(i, i), c(0, 1), labels = format(rx[,i], digits=3),
                 xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
    }
    invisible()
}
