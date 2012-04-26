% File nlme/man/logLik.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logLik.corStruct.Rd,v 1.7.2.1 2002/08/09 19:45:29 bates Exp $
\name{logLik.corStruct}
\title{Extract corStruct Log-Likelihood}
\usage{
\method{logLik}{corStruct}(object, data, \dots)
}
\alias{logLik.corStruct}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
 \item{data}{this argument is included to make this method function
   compatible with other \code{logLik} methods and will be ignored.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the component of a Gaussian
  log-likelihood associated with the correlation structure, which is
  equal to the negative of the logarithm of the determinant (or sum of
  the logarithms of the determinants) of the matrix (or matrices)
  represented by \code{object}.
}
\value{
  the negative of the logarithm of the determinant (or sum of
  the logarithms of the determinants) of the correlation matrix (or
  matrices) represented by \code{object}.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{logDet.corStruct}},
  \code{\link{logLik.lme}},
}

\examples{
cs1 <- corAR1(0.2)
cs1 <- Initialize(cs1, data = Orthodont)
logLik(cs1)
}
\keyword{models}
