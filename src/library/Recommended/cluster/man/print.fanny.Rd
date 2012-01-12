\name{print.fanny}
\alias{print.fanny}
\alias{summary.fanny}
\alias{print.summary.fanny}
\title{Print and Summary Methods for FANNY Objects}
\description{
  Prints the objective function, membership coefficients and clustering vector
  of \code{fanny} object.

  This is a method for the function \code{\link{print}()} for objects
  inheriting from class \code{\link{fanny}}.
}
\usage{
\method{print}{fanny}(x, digits = getOption("digits"), \dots)
\method{summary}{fanny}(object, \dots)
\method{print}{summary.fanny}(x, digits = getOption("digits"), \dots)
}
\arguments{
  \item{x, object}{a \code{\link{fanny}} object.}
  \item{digits}{number of significant digits for printing, see
    \code{\link{print.default}}.}
  \item{\dots}{potential further arguments (required by generic).}
}
\seealso{
  \code{\link{fanny}}, \code{\link{fanny.object}}, \code{\link{print}},
  \code{\link{print.default}}.
}
\keyword{cluster}
\keyword{print}
