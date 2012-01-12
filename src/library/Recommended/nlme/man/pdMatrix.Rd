% File nlme/man/pdMatrix.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdMatrix.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{pdMatrix}
\title{Extract Matrix or Square-Root Factor from a pdMat Object}
\usage{
pdMatrix(object, factor)
}
\alias{pdMatrix}
\alias{pdMatrix.pdBlocked}
\alias{pdMatrix.pdCompSymm}
\alias{pdMatrix.pdDiag}
\alias{pdMatrix.pdIdent}
\alias{pdMatrix.pdMat}
\alias{pdMatrix.pdSymm}
\alias{pdMatrix.pdNatural}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix.}
 \item{factor}{an optional logical value. If \code{TRUE}, a square-root
   factor of the positive-definite matrix represented by \code{object}
   is returned; else, if \code{FALSE}, the positive-definite matrix is
   returned. Defaults to \code{FALSE}.}
}
\description{
  The positive-definite matrix represented by \code{object}, or a
  square-root factor of it is obtained. Letting \eqn{\Sigma}{S} denote a
  positive-definite matrix, a square-root factor of \eqn{\Sigma}{S} is
  any square matrix \eqn{L}{L} such that \eqn{\Sigma = L'L}{S =
    L'L}. This function extracts \eqn{S} or \eqn{L}.
}
\value{
  if \code{fact} is \code{FALSE} the positive-definite matrix
  represented by \code{object} is returned; else a square-root of the
  positive-definite matrix is returned.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer, esp. p. 162.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{as.matrix.pdMat}},
  \code{\link{pdClasses}},
  \code{\link{pdFactor}},
  \code{\link{pdMat}},
  \code{\link{pdMatrix.reStruct}},
  \code{\link{corMatrix}}
}

\examples{
pd1 <- pdSymm(diag(1:4))
pdMatrix(pd1)
}
\keyword{models}
