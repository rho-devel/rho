% File nlme/man/Extract.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Extract.pdMat.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{[.pdMat}
\title{Subscript a pdMat Object}
\usage{
\method{[}{pdMat}(x, i, j, drop = TRUE)
\method{[}{pdMat}(x, i, j) <- value
}
\alias{[.pdMat}
\alias{[.pdBlocked}
\alias{[<-.pdMat}
\arguments{
 \item{x}{an object inheriting from class \code{pdMat} representing a
   positive-definite matrix.}
 \item{i, j}{optional subscripts applying respectively to the rows
   and columns of the positive-definite matrix represented by
   \code{object}. When \code{i} (\code{j}) is omitted, all rows (columns)
   are extracted.}
 \item{drop}{a logical value. If \code{TRUE}, single rows or columns are
   converted to vectors. If \code{FALSE} the returned value retains its
   matrix representation.}
 \item{value}{a vector, or matrix, with the replacement values for the
   relevant piece of the matrix represented by \code{x}.}
}
\description{
  This method function extracts sub-matrices from the positive-definite
  matrix represented by \code{x}. 
}
\value{
  if \code{i} and \code{j} are identical, the returned value will be
  \code{pdMat} object with the same class as \code{x}. Otherwise, the
  returned value will be a matrix. In the case a single row (or column)
  is selected, the returned value may be converted to a vector,
  according to the rules above.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{[}}, \code{\link{pdMat}}}

\examples{
pd1 <- pdSymm(diag(3))
pd1[1, , drop = FALSE]
pd1[1:2, 1:2] <- 3 * diag(2)
}
\keyword{models}
