% File nlme/man/solve.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: solve.pdMat.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{solve.pdMat}
\title{Calculate Inverse of a Positive-Definite Matrix}
\usage{
\method{solve}{pdMat}(a, b, \dots)
}
\alias{solve.pdMat}
\alias{solve.pdBlocked}
\alias{solve.pdDiag}
\alias{solve.pdIdent}
\alias{solve.pdLogChol}
\alias{solve.pdNatural}
\alias{solve.pdSymm}
\arguments{
 \item{a}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix.} 
 \item{b}{this argument is only included for consistency with the generic
   function and is not used in this method function.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The positive-definite matrix represented by \code{a} is inverted and
  assigned to \code{a}.
}
\value{
  a \code{pdMat} object similar to \code{a}, but with coefficients
  corresponding to the inverse of the positive-definite matrix
  represented by \code{a}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}


\seealso{\code{\link{pdMat}}}

\examples{
pd1 <- pdCompSymm(3 * diag(3) + 1)
solve(pd1)
}
\keyword{models}
