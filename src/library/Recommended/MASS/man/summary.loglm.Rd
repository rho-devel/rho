% file MASS/man/summary.loglm.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{summary.loglm}
\alias{summary.loglm}
\alias{print.summary.loglm}
\title{
Summary Method Function for Objects of Class 'loglm'
}
\description{
Returns a summary list for log-linear models fitted by
iterative proportional scaling using \code{loglm}.
}
\usage{
\method{summary}{loglm}(object, fitted = FALSE, \dots)
}
\arguments{
\item{object}{
a fitted loglm model object.
}
\item{fitted}{
if \code{TRUE} return observed and expected frequencies in the result.
Using \code{fitted = TRUE} may necessitate re-fitting the object.
}
\item{\dots}{
arguments to be passed to or from other methods.
}}
\value{
a list is returned for use by \code{print.summary.loglm}.
This has components

\item{formula}{
the formula used to produce \code{object}
}
\item{tests}{
the table of test statistics (likelihood ratio, Pearson) for the fit.
}
\item{oe}{
if \code{fitted = TRUE}, an array of the observed and expected frequencies,
otherwise \code{NULL}.
}}
\details{
This function is a method for the generic function
\code{summary()} for class \code{"loglm"}.
It can be invoked by calling \code{summary(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{summary.loglm(x)} regardless of the
class of the object.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{loglm}}, \code{\link{summary}}
}
\keyword{models}
