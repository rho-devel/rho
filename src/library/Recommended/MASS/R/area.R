# file MASS/R/area.R
# copyright (C) 1994-9 W. N. Venables and B. D. Ripley
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
"area"<-
function(f, a, b, ..., fa = f(a, ...), fb = f(b, ...), limit
	 = 10, eps = 1e-5)
{
    h <- b - a
    d <- (a + b)/2
    fd <- f(d, ...)
    a1 <- ((fa + fb) * h)/2
    a2 <- ((fa + 4 * fd + fb) * h)/6
    if(abs(a1 - a2) < eps)
        return(a2)
    if(limit == 0) {
        warning(gettextf("iteration limit reached near 'x = %f'", d),
                doman = NA)
        return(a2)
    }
    Recall(f, a, d, ..., fa = fa, fb = fd, limit = limit - 1,
           eps = eps) + Recall(f, d, b, ..., fa = fd, fb =
           fb, limit = limit - 1, eps = eps)
}
"fbeta"<-
function(x, alpha, beta)
{
    x^(alpha - 1) * (1 - x)^(beta - 1)
}
"print.abbrev"<-
function(x, ...)
{
    if(is.list(x))
        x <- unlist(x)
    NextMethod("print")
}
