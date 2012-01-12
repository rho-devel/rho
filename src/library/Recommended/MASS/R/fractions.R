# file MASS/R/fractions.R
# copyright (C) 1994-2005 W. N. Venables and B. D. Ripley
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
.rat <- function(x, cycles = 10, max.denominator = 2000)
{
  a0 <- rep(0, length(x))
  A <- matrix(b0 <- rep(1, length(x)))
  fin <- is.finite(x)
  B <- matrix(floor(x))
  r <- as.vector(x) - drop(B)
  len <- 0
  while(any(which <- fin & (r > 1/max.denominator)) &&
	(len <- len + 1) <= cycles) {
    a <- a0
    b <- b0
    a[which] <- 1
    r[which] <- 1/r[which]
    b[which] <- floor(r[which])
    r[which] <- r[which] - b[which]
    A <- cbind(A, a)
    B <- cbind(B, b)
  }
  pq1 <- cbind(b0, a0)
  pq <- cbind(B[, 1], b0)
  len <- 1
  while((len <- len + 1) <= ncol(B)) {
    pq0 <- pq1
    pq1 <- pq
    pq <- B[, len] * pq1 + A[, len] * pq0
  }
  pq[!fin, 1] <- x[!fin]
  list(rat = pq, x = x)
}

rational <- function(x, cycles = 10, max.denominator = 2000, ...)
{
  ans <- .rat(x, cycles, max.denominator)$rat
  do.call("structure", c(list(ans[,1]/ans[,2]), attributes(x)))
}

fractions <- function(x, cycles = 10, max.denominator = 2000, ...)
{
  ans <- .rat(x, cycles, max.denominator)
  ndc <- paste(ans$rat[, 1], ans$rat[, 2], sep = "/")
  int <- ans$rat[, 2] == 1
  ndc[int] <- as.character(ans$rat[int, 1])
  structure(ans$x, fracs = ndc, class = c("fractions", class(ans$x)))
}

t.fractions <- function(x)
{
  xt <- NextMethod()
  class(xt) <- class(x)
  attr(xt, "fracs") <- t(array(attr(x, "fracs"), dim(x)))
  xt
}

Math.fractions <- function(x, ...)
{
  x <- unclass(x)
  fractions(NextMethod())
}

Ops.fractions <- function(e1, e2)
{
  e1 <- unclass(e1)
  if(!missing(e2))
    e2 <- unclass(e2)
  fractions(NextMethod(.Generic))
}

Summary.fractions <- function(x, ..., na.rm)
{
  x <- unclass(x)
  fractions(NextMethod())
}

"[.fractions" <- function(x, ...)
{
  x <- unclass(x)
  fractions(NextMethod())
}

"[<-.fractions" <- function(x, ..., value)
{
  x <- unclass(x)
  fractions(NextMethod())
}

as.character.fractions <- function(x, ...)
    structure(attr(x, "fracs"), dim = dim(x), dimnames = dimnames(x))

as.fractions <- function(x)
    if(is.fractions(x)) x else fractions(x)

is.fractions <- function(f)
    inherits(f, "fractions")

print.fractions <- function(x, ...)
{
  y <- attr(x, "fracs")
  mc <- max(ncy <- nchar(y, "c"))
  if(any(small <- ncy < mc)) {
    blanks <- "    "
    while(nchar(blanks) < mc) blanks <- paste(blanks, blanks)
    blanks <- rep(blanks, sum(small))
    blanks <- substring(blanks, 1L, mc - ncy)
    y[small] <- paste(blanks[small], y[small], sep = "")
  }
  att <- attributes(x)
  att$fracs <- att$class <- NULL
  x <- do.call("structure", c(list(y), att))
  NextMethod("print", quote = FALSE)
}

