% File nlme/man/lmeScale.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{lmeScale}
\title{Scale for lme Optimization}
\usage{
lmeScale(start)
}
\alias{lmeScale}
\arguments{
  \item{start}{the starting values for the coefficients to be estimated.}
}
\description{
  This function calculates the scales to be used for each coefficient
  estimated through an \code{nlm} optimization in the \code{lme}
  function. If all initial values are zero, the scale is set to one for
  all coefficients; else, the scale for a coefficient with non-zero
  initial value is equal to the inverse of its initial value and the
  scale for the coefficients with initial value equal to zero is set to
  the median of the non-zero initial value coefficients.
}
\value{
  a vector with the scales to be used in \code{nlm} for estimating the
  coefficients. 
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{nlm}}}
\keyword{models}
