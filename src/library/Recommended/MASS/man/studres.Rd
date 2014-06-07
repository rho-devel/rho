% file MASS/man/studres.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{studres}
\alias{studres}
\title{
Extract Studentized Residuals from a Linear Model
}
\description{
The Studentized residuals.  Like standardized residuals, these are
normalized to unit variance, but the Studentized version is fitted
ignoring the current data point. (They are sometimes called jackknifed
residuals).
}
\usage{
studres(object)
}
\arguments{
\item{object}{
any object representing a linear model.
}}
\value{
The vector of appropriately transformed residuals.
}
\seealso{
\code{\link{residuals}}, \code{\link{stdres}}
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{models}
