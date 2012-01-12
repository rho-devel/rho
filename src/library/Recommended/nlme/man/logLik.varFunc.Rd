% File nlme/man/logLik.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logLik.varFunc.Rd,v 1.7.2.1 2002/08/09 19:45:29 bates Exp $
\name{logLik.varFunc}
\title{Extract varFunc logLik}
\usage{
\method{logLik}{varFunc}(object, data, \dots)
}
\alias{logLik.varFunc}
\alias{logLik.varComb}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc},
   representing a variance function structure.} 
 \item{data}{this argument is included to make this method function
   compatible with other \code{logLik} methods and will be ignored.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the component of a Gaussian
  log-likelihood associated with the variance function structure
  represented by \code{object}, which is equal to the sum of the
  logarithms of the corresponding weights. 
}
\value{
  the sum of the logarithms of the weights corresponding to the variance
  function structure represented by \code{object}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{logLik.lme}}
}


\examples{
vf1 <- varPower(form = ~age)
vf1 <- Initialize(vf1, Orthodont)
coef(vf1) <- 0.1
logLik(vf1)
}
\keyword{models}
