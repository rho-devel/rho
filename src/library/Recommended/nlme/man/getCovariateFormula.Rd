% File nlme/man/getCovariateFormula.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{getCovariateFormula}
\title{Extract Covariates Formula}
\usage{
getCovariateFormula(object)
}
\alias{getCovariateFormula}
\arguments{
 \item{object}{any object from which a formula can be extracted.}
}
\description{
  The right hand side of \code{formula(object)}, without any
  conditioning expressions (i.e. any expressions after a \code{|}
  operator) is returned as a one-sided formula.
}
\value{
  a one-sided formula describing the covariates associated with
  \code{formula(object)}. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getCovariate}}}

\examples{
getCovariateFormula(y ~ x | g)
getCovariateFormula(y ~ x)
}
\keyword{models}

