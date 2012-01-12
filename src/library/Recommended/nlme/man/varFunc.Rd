% File nlme/man/varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varFunc.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varFunc}
\title{Variance Function Structure}
\usage{
varFunc(object)
}
\alias{varFunc}
\arguments{
 \item{object}{either an one-sided formula specifying a variance
   covariate, or an object inheriting from class \code{varFunc},
   representing a variance function structure.} 
}
\description{
  If \code{object} is a one-sided formula, it is used as the argument to
  \code{varFixed} and the resulting object is returned. Else, if
  \code{object} inherits from class \code{varFunc}, it is returned
  unchanged. 
}
\value{
  an object from class \code{varFunc}, representing a variance function
  structure. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{summary.varFunc}},
  \code{\link{varFixed}},
  \code{\link{varWeights.varFunc}},
  \code{\link{coef.varFunc}}} 

\examples{
vf1 <- varFunc(~age)
}
\keyword{models}
