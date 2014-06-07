% File nlme/man/pdConstruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdConstruct.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{pdConstruct}
\title{Construct pdMat Objects}
\usage{
pdConstruct(object, value, form, nam, data, \dots)
}
\alias{pdConstruct}
\alias{pdConstruct.pdCompSymm}
\alias{pdConstruct.pdDiag}
\alias{pdConstruct.pdIdent}
\alias{pdConstruct.pdMat}
\alias{pdConstruct.pdNatural}
\alias{pdConstruct.pdSymm}
\alias{pdConstruct.pdLogChol}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix.} 
 \item{value}{an optional initialization value, which can be any of the
   following: a \code{pdMat} object, a positive-definite
   matrix, a one-sided linear formula (with variables separated by
   \code{+}), a vector of character strings, or a numeric
   vector. Defaults to \code{numeric(0)}, corresponding to an
   uninitialized object.} 
 \item{form}{an optional one-sided linear formula specifying the
   row/column names for the matrix represented by \code{object}. Because
   factors may be present in \code{form}, the formula needs to be
   evaluated on a data.frame to resolve the names it defines. This
   argument is ignored when \code{value} is a one-sided
   formula. Defaults to \code{NULL}.}
 \item{nam}{an optional vector of character strings specifying the
   row/column names for the matrix represented by object. It must have 
   length equal to the dimension of the underlying positive-definite
   matrix and unreplicated elements. This argument is ignored when
   \code{value} is a vector of character strings. Defaults to
   \code{NULL}.}
 \item{data}{an optional data frame in which to evaluate the variables
   named in \code{value} and \code{form}. It is used to
   obtain the levels for \code{factors}, which affect the
   dimensions and the row/column names of the underlying matrix. If
   \code{NULL}, no attempt is made to obtain information on 
   \code{factors} appearing in the formulas. Defaults to the
   parent frame from which the function was called.}
 \item{\dots}{optional arguments for some methods.}
}
\description{
  This function is an alternative constructor for the \code{pdMat}
  class associated with \code{object} and is mostly used internally in other
  functions. See the documentation on the principal constructor
  function, generally with the same name as the \code{pdMat} class of
  object. 
}
\value{
  a \code{pdMat} object representing a positive-definite matrix,
  inheriting from the same classes as \code{object}.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{pdCompSymm}}, \code{\link{pdDiag}},
  \code{\link{pdIdent}}, \code{\link{pdNatural}}, \code{\link{pdSymm}}}

\examples{
pd1 <- pdSymm()
pdConstruct(pd1, diag(1:4))
}
\keyword{models}
