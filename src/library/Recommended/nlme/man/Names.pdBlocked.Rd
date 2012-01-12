% File nlme/man/Names.pdBlocked.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Names.pdBlocked.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{Names.pdBlocked}
\title{Names of a pdBlocked Object}
\usage{
\method{Names}{pdBlocked}(object, asList, \dots)
}
\alias{Names.pdBlocked}
\alias{Names<-.pdBlocked}
\arguments{
 \item{object}{an object inheriting from class \code{pdBlocked}
   representing a positive-definite matrix with block diagonal
   structure} 
 \item{asList}{a logical value. If \code{TRUE} a \code{list} with the
   names for each block diagonal element is returned. If \code{FALSE}
   a character vector with all column names is returned. Defaults to
   \code{FALSE}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the first element of the \code{Dimnames}
  attribute, which contains the column names, for each block diagonal
  element in the matrix represented by \code{object}.
}
\value{
  if \code{asList} is \code{FALSE}, a character vector with column names
  of the matrix represented by \code{object}; otherwise, if
  \code{asList} is \code{TRUE}, a list with components given by the
  column names of the individual block diagonal elements in the matrix
  represented by \code{object}.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{Names}},
  \code{\link{Names.pdMat}}}

\examples{
pd1 <- pdBlocked(list(~Sex - 1, ~age - 1), data = Orthodont)
Names(pd1)
}
\keyword{models}
