# file MASS/R/contr.sdif.R
# copyright (C) 1994-2013 W. N. Venables and B. D. Ripley
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
contr.sdif <- function(n, contrasts = TRUE, sparse = FALSE)
{
    # contrasts generator giving 'successive difference' contrasts.
    if(is.numeric(n) && length(n) == 1L) {
        if(n %% 1L || n < 2L) stop("invalid number of levels")
        lab <- as.character(seq(n))
    } else {
        lab <- as.character(n)
        n <- length(n)
        if(n < 2L) stop("invalid number of levels")
    }
    if(contrasts) {
        cont <- col(matrix(nrow = n, ncol = n - 1L))
        upper.tri <- !lower.tri(cont)
        cont[upper.tri] <- cont[upper.tri] - n
        structure(cont/n,
                  dimnames = list(lab, paste(lab[-1L], lab[-n], sep="-")))
    } else .Diag(lab, sparse = sparse)
}

.Diag <- function(nms, sparse)
{
    n <- length(nms)
    dn <- list(nms, nms)
    if(sparse) new("ddiMatrix", diag = "U", Dim = c(n, n), Dimnames = dn)
    else structure(diag(1, n, n), dimnames = dn)
}
