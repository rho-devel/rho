% File nlme/man/getResponseFormula.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{getResponseFormula}
\title{Extract Formula Specifying Response Variable}
\usage{
getResponseFormula(object)
}
\alias{getResponseFormula}
\arguments{
 \item{object}{any object from which a formula can be extracted.}
}
\description{
  The left hand side of \code{formula{object}} is returned as a
  one-sided formula. 
}
\value{
  a one-sided formula with the response variable associated with
  \code{formula{object}}. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getResponse}}}

\examples{
getResponseFormula(y ~ x | g)
}
\keyword{models}

