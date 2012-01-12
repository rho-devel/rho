###          Extract variance components of lme models.
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
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

VarCorr <- function( x, sigma = 1, rdig = 3) UseMethod("VarCorr")

VarCorr.lme <- function( x, sigma = 1., rdig = 3)
{
  sigma <- x$sigma
  m <- lapply( rev( x$modelStruct$reStruct ), VarCorr,
              sigma = sigma, rdig = rdig)
  Q <- length( m )
  if (Q <= 1) {
    nm <- names(m)
    m <- m[[1]]
    mm <- rbind(m, Residual = c(Variance = sigma^2, StdDev = sigma))
    v <- array( "", dim(mm), dimnames(mm) )
    v[, 1] <- format( mm[, 1] )
    v[, 2] <- format( mm[, 2] )
    if (!is.null( attr(m, "corr") ) ) {
      v <- cbind( v, rbind(attr(m, "corr"),
                           Residual = rep("", ncol(attr(m, "corr")))))
    }
    attr( v, "title" ) <-
      paste( nm, "=", attr( m, "formStr" ) )
    class( v ) <- "VarCorr.lme"
    return( v )
  }
  ## multiple nested levels case
  nrows <- sapply( m, nrow )
  trows <- 1 + c(0, cumsum(1 + nrows))[1:Q]
  bd <- rbind( do.call("rbind", m),
              c(Variance = sigma^2, StdDev = sigma) )
  corr <- lapply( m, attr, which = "corr")
  colnames <- colnames(bd)
  maxCorr <- 0
  if ( !all( Nulls <- sapply( corr, is.null ) ) ) {
    maxCorr <- max( sapply( corr[!Nulls], ncol ) )
    colnames <- c( colnames, "Corr", rep("", maxCorr - 1 ) )
  }
  v <- array("", c(sum(nrows) + Q + 1, 2 + maxCorr), list(NULL, colnames))
  v[-trows, 1] <- format(bd[, 1])
  v[-trows, 2] <- format(bd[, 2])
  v[trows, 1] <- sapply( m, attr, which = "formStr" )
  rownames <- rep("", sum(nrows) + Q)
  rownames[trows] <- paste( names( m ), "=" )
  rr <- 1
  for (i in seq_along( m ) ) {
    rownames[ rr + (1:nrows[i]) ] <- dimnames( m[[i]] )[[1]]
    if (!is.null( corr[[i]] )) {
      v[ rr + (1:nrows[i]), 2 + (1:ncol(corr[[i]])) ] <- corr[[i]]
    }
    rr <- rr + nrows[i] + 1
  }
  rownames(v) <- c(rownames, "Residual")
  class(v) <- "VarCorr.lme"
  v
}

print.VarCorr.lme <- function(x, ...)
{
  if (!is.null(attr(x, "title"))) {
    cat(attr( x, "title" ), "\n")
    attr(x, "title") <- NULL
  }
  print(unclass(x), ..., quote = FALSE )
  invisible(x)
}


VarCorr.pdMat <- function( x, sigma = 1., rdig = 3)
{
  sx <- summary( x )
  sd <- sigma * attr( sx, "stdDev" )
  var <- sd^2
  p <- dim(sx)[2]
  v <- array(c(var, sd), c(p, 2), list( names(sd), c( "Variance", "StdDev" )))
#   attr(v, "formStr") <- deparse(as.call(list(as.name(class(x)[[1]]),
#                                        as.vector(attr(x, "formula")))))
# ## puts in an extra blank.  We'll do it the clunky way instead
  attr(v, "formStr") <-
    if ( inherits( attr(x, "formula"), "listForm" ) ) {# an nlme'ism
      paste(class(x)[[1]], "(list(",
            paste( sapply( attr(x, "formula"),
                          function(x) as.character(deparse(x))),
                  collapse=","), "))", sep = "")
    } else {
      paste(class(x)[[1]], "(",
            substring(deparse(attr(x, "formula")), 2), ")", sep = "")
    }
      if ( attr(sx, "noCorrelation") | (p <= 1) ) {
    return(v)
  }
  ll <- lower.tri(sx)
  sx[ll] <- format(round(sx[ll], digits = rdig))
  sx[!ll] <- ""
  if (!is.null(colnames(sx))) {
    sx[1,] <- abbreviate(colnames(sx), minlength = rdig + 3)
  }
  dimnames(sx) <- list(names(sd), c("Corr", rep("", p - 1)))
  attr(v, "corr") <- sx[, -p, drop = FALSE ]
  v
}

VarCorr.pdBlocked <- function( x, sigma = 1., rdig = 3)
{
  m <- lapply( x, VarCorr, sigma = sigma, rdig = rdig)
  bd <- do.call("rbind", m)
## the following code does not appear to be used at all
##   corr <- lapply( m, attr, which = "corr")
##   maxCorr <- 0
##   if ( !all( Nulls <- sapply( corr, is.null ) ) ) {
##     maxCorr <- max( sapply( corr[!Nulls], ncol ) )

##   }
  attr(bd, "formStr") <-
    paste( sapply( m, attr, which = "formStr" ), collapse = ", ")
  bd
}

### Local variables:
### mode: S
### End:
