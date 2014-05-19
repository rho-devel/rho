% File nlme/man/pdConstruct.pdBlocked.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdConstruct.pdBlocked.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{pdConstruct.pdBlocked}
\title{Construct pdBlocked Objects}
\usage{
\method{pdConstruct}{pdBlocked}(object, value, form, nam, data, pdClass,
\dots)
}
\alias{pdConstruct.pdBlocked}
\arguments{
 \item{object}{an object inheriting from class \code{pdBlocked},
   representing a positive definite block-diagonal matrix.} 
 \item{value}{an optional list with elements to be used as the
   \code{value} argument to other \code{pdMat} constructors. These
   include: \code{pdMat} objects, positive-definite
   matrices, one-sided linear formulas, vectors of character strings, or
   numeric vectors. All elements in the list must be similar (e.g. all
   one-sided formulas, or all numeric vectors). Defaults to
   \code{numeric(0)}, corresponding to an uninitialized object.}
 \item{form}{an optional list of one-sided linear formula specifying the
   row/column names for the block-diagonal elements of the matrix
   represented by \code{object}. Because factors may be present in
   \code{form}, the formulas needs to be evaluated on a data.frame to
   resolve the names they defines. This argument is ignored when
   \code{value} is a list of one-sided formulas. Defaults to \code{NULL}.}
 \item{nam}{an optional list of vector of character strings specifying the
   row/column names for the block-diagonal elements of the matrix
   represented by object. Each of its components must have  
   length equal to the dimension of the corresponding block-diagonal
   element and unreplicated elements. This argument is ignored when 
   \code{value} is a list of vector of character strings. Defaults to 
   \code{NULL}.}
 \item{data}{an optional data frame in which to evaluate the variables
   named in \code{value} and \code{form}. It is used to
   obtain the levels for \code{factors}, which affect the
   dimensions and the row/column names of the underlying matrix. If
   \code{NULL}, no attempt is made to obtain information on 
   \code{factors} appearing in the formulas. Defaults to the
   parent frame from which the function was called.}
 \item{pdClass}{an optional vector of character strings naming the
   \code{pdMat} classes to be assigned to the individual blocks in the
   underlying matrix. If a single class is specified, it is used for all
   block-diagonal elements. This argument will only be used when
   \code{value} is missing, or its elements are not \code{pdMat}
   objects. Defaults to \code{"pdSymm"}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This function give an alternative constructor for the \code{pdBlocked}
  class, representing a positive-definite block-diagonal matrix. Each
  block-diagonal element of the underlying  matrix is itself a
  positive-definite matrix and is represented internally as an
  individual \code{pdMat} object. When \code{value} is
  \code{numeric(0)}, a list of uninitialized \code{pdMat} objects, a
  list of one-sided formulas, or a list of vectors of character strings,
  \code{object} is returned as an uninitialized \code{pdBlocked} object
  (with just some of its attributes and its class defined) and needs to
  have its coefficients assigned later, generally using the \code{coef}
  or \code{matrix} replacement functions. If \code{value} is a list of  initialized
  \code{pdMat} objects, \code{object} will be constructed from the list
  obtained by applying \code{as.matrix} to each of the \code{pdMat}
  elements of \code{value}. Finally, if \code{value} is a list of
  numeric vectors, they are assumed to represent the unrestricted
  coefficients of the block-diagonal elements of the  underlying
  positive-definite matrix.  
}
\value{
  a \code{pdBlocked} object representing a positive-definite
  block-diagonal matrix, also inheriting from class \code{pdMat}.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}


\seealso{\code{\link{as.matrix.pdMat}},
  \code{\link{coef.pdMat}},
  \code{\link{pdBlocked}},
  \code{\link{pdClasses}},
  \code{\link{pdConstruct}},
  \code{\link{matrix<-.pdMat}}} 

\examples{
pd1 <- pdBlocked(list(c("A","B"), c("a1", "a2", "a3")))
pdConstruct(pd1, list(diag(1:2), diag(c(0.1, 0.2, 0.3))))
}
\keyword{models}
