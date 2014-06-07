% File nlme/man/varWeights.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varWeights.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{varWeights}
\title{Extract Variance Function Weights}
\usage{
varWeights(object)
}
\alias{varWeights}
\alias{varWeights.varComb}
\alias{varWeights.varFunc}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc},
   representing a variance function structure.} 
}
\description{
The inverse of the standard deviations corresponding to the variance
function structure represented by \code{object} are returned.
}
\value{
  if \code{object} has a \code{weights} attribute, its value is
  returned; else \code{NULL} is returned.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{logLik.varFunc}}, 
  \code{\link{varWeights}}
}

\examples{
vf1 <- varPower(form=~age)
vf1 <- Initialize(vf1, Orthodont)
coef(vf1) <- 0.3
varWeights(vf1)[1:10]
}
\keyword{models}
