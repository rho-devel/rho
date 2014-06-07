% File nlme/man/Dim.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Dim.pdMat.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{Dim.pdMat}
\title{Dimensions of a pdMat Object}
\usage{
\method{Dim}{pdMat}(object, \dots)
}
\alias{Dim.pdMat}
\alias{Dim.pdCompSymm}
\alias{Dim.pdDiag}
\alias{Dim.pdIdent}
\alias{Dim.pdNatural}
\alias{Dim.pdSymm}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat},
   representing a positive-definite matrix.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function returns the dimensions of the matrix represented
  by \code{object}. 
}
\value{
  an integer vector with the number of rows and columns of the
  matrix represented by \code{object}.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{Dim}}}
\examples{
Dim(pdSymm(diag(3)))
}
\keyword{models}
