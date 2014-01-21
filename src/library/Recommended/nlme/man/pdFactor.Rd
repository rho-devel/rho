% File nlme/man/pdFactor.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdFactor.Rd,v 1.8 2002/03/05 14:59:39 bates Exp $
\name{pdFactor}
\title{Square-Root Factor of a Positive-Definite Matrix}
\usage{
pdFactor(object)
}
\alias{pdFactor}
\alias{pdFactor.pdBlocked}
\alias{pdFactor.pdCompSymm}
\alias{pdFactor.pdDiag}
\alias{pdFactor.pdIdent}
\alias{pdFactor.pdMat}
\alias{pdFactor.pdNatural}
\alias{pdFactor.pdSymm}
\alias{pdFactor.pdLogChol}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix, which must have been initialized
   (i.e. \code{length(coef(object)) > 0}).} 
}
\description{
  A square-root factor of the positive-definite matrix represented by
  \code{object} is obtained. Letting \eqn{\Sigma}{S} denote a
  positive-definite matrix, a square-root factor of \eqn{\Sigma}{S} is
  any square matrix \eqn{L}{L} such that \eqn{\Sigma = L'L}{S =
    L'L}. This function extracts \eqn{L}.
}
\value{
  a vector with a square-root factor of the positive-definite matrix
  associated with \code{object} stacked column-wise.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This function is used intensively in optimization
  algorithms and its value is returned as a vector for efficiency
  reasons. The \code{pdMatrix} function can be used to obtain
  square-root factors in matrix form.
}
\seealso{\code{\link{pdMatrix}}}
\examples{
pd1 <- pdCompSymm(4 * diag(3) + 1)
pdFactor(pd1)
}
\keyword{models}
