% file MASS/man/stdres.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{stdres}
\alias{stdres}
\alias{lmwork}
\title{
Extract Standardized Residuals from a Linear Model
}
\description{
The standardized residuals.  These are normalized to unit
variance, fitted including the current data point.
}
\usage{
stdres(object)
}
\arguments{
\item{object}{
any object representing a linear model.
}}
\value{
The vector of appropriately transformed residuals.
}
\seealso{
\code{\link{residuals}}, \code{\link{studres}}
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{models}
