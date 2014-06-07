% File nlme/man/logDet.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logDet.pdMat.Rd,v 1.8.2.2 2003/04/17 22:29:35 bates Exp $
\name{logDet.pdMat}
\title{Extract Log-Determinant from a pdMat Object}
\usage{
\method{logDet}{pdMat}(object, \dots)
}
\alias{logDet.pdMat}
\alias{logDet.pdBlocked}
\alias{logDet.pdCompSymm}
\alias{logDet.pdDiag}
\alias{logDet.pdIdent}
\alias{logDet.pdNatural}
\alias{logDet.pdSymm}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the logarithm of the determinant of a
  square-root factor of the positive-definite matrix represented by
  \code{object}.  
}
\value{
  the log-determinant of a square-root factor of the positive-definite
  matrix represented by \code{object}. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{pdMat}},
  \code{\link{logDet}}
}

\examples{
pd1 <- pdSymm(diag(1:3))
logDet(pd1)
}
\keyword{models}
