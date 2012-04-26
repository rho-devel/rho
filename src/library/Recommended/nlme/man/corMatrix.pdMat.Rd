% File nlme/man/corMatrix.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corMatrix.pdMat.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{corMatrix.pdMat}
\title{Extract Correlation Matrix from a pdMat Object}
\usage{
\method{corMatrix}{pdMat}(object, \dots)
}
\alias{corMatrix.pdBlocked}
\alias{corMatrix.pdCompSymm}
\alias{corMatrix.pdDiag}
\alias{corMatrix.pdIdent}
\alias{corMatrix.pdMat}
\alias{corMatrix.pdSymm}

\arguments{
 \item{object}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}

\description{
  The correlation matrix corresponding to the positive-definite matrix
  represented by \code{object} is obtained. 
}
\value{
  the correlation matrix corresponding to the positive-definite matrix
  represented by \code{object}. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{as.matrix.pdMat}}, \code{\link{pdMatrix}}}

\examples{
pd1 <- pdSymm(diag(1:4))
corMatrix(pd1)
}
\keyword{models}
