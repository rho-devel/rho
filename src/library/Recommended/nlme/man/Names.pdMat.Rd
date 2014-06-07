% File nlme/man/Names.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Names.pdMat.Rd,v 1.9.2.1 2003/08/09 22:45:17 bates Exp $
\name{Names.pdMat}
\title{Names of a pdMat Object}
\usage{
\method{Names}{pdMat}(object, \dots)
\method{Names}{pdMat}(object, \dots) <- value
}
\alias{Names.pdMat}
\alias{Names<-.pdMat}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat},
   representing a positive-definite matrix.}
 \item{value}{a character vector with the replacement values for the
   column and row names of the matrix represented by \code{object}. It
   must have length equal to the dimension of the matrix
   represented by \code{object} and, if names have been previously
   assigned to \code{object}, it must correspond to a permutation of the
   original names.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function returns the fist element of the \code{Dimnames}
  attribute of \code{object}, which contains the column names of the
  matrix represented by \code{object}. 
}
\value{
  if \code{object} has a \code{Dimnames} attribute then the first
  element of this attribute is returned; otherwise \code{NULL}. 
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the \code{Dimnames} attribute
  of \code{object} to \code{list(value, value)}.
}
\seealso{\code{\link{Names}}, \code{\link{Names.pdBlocked}}}

\examples{
pd1 <- pdSymm(~age, data = Orthodont)
Names(pd1)
}
\keyword{models}
