% File nlme/man/logDet.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logDet.corStruct.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{logDet.corStruct}
\title{Extract corStruct Log-Determinant}
\usage{
\method{logDet}{corStruct}(object, covariate, \dots)
}
\alias{logDet.corStruct}
\alias{logDet.corIdent}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
 \item{covariate}{an optional covariate vector (matrix), or list of
   covariate vectors (matrices), at which values the correlation matrix,
   or list of correlation  matrices, are to be evaluated. Defaults to
   \code{getCovariate(object)}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the logarithm of the determinant of a
  square-root factor of the correlation matrix associated with
  \code{object}, or the sum of the log-determinants of square-root
  factors of the list of correlation matrices associated with
  \code{object}.  
}
\value{
  the log-determinant of a square-root factor of the correlation matrix
  associated with \code{object}, or the sum of the log-determinants of
  square-root factors of the list of correlation matrices associated
  with \code{object}. 
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{logLik.corStruct}},
  \code{\link{corMatrix.corStruct}},
  \code{\link{logDet}}
}

\examples{
cs1 <- corAR1(0.3)
logDet(cs1, covariate = 1:4)
}
\keyword{models}
