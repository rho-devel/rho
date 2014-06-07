% File nlme/man/logDet.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logDet.reStruct.Rd,v 1.6 2001/10/30 20:51:15 bates Exp $
\name{logDet.reStruct}
\title{Extract reStruct Log-Determinants}
\usage{
\method{logDet}{reStruct}(object, \dots)
}
\alias{logDet.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Calculates, for each of the \code{pdMat} components of \code{object},
  the logarithm of the determinant of a square-root factor.  
}
\value{
  a vector with the log-determinants of square-root factors of the
  \code{pdMat} components of \code{object}.
}

\author{José Pinheiro}

\seealso{\code{\link{reStruct}},
  \code{\link{pdMat}}, \code{\link{logDet}}
}

\examples{
rs1 <- reStruct(list(A = pdSymm(diag(1:3), form = ~Score),
  B = pdDiag(2 * diag(4), form = ~Educ)))
logDet(rs1)
}
\keyword{models}
