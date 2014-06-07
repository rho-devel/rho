% File nlme/man/pdIdent.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdIdent.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{pdIdent}
\title{Multiple of the Identity Positive-Definite Matrix}
\usage{
pdIdent(value, form, nam, data)
}
\alias{pdIdent}
\arguments{
 \item{value}{an optional initialization value, which can be any of the
   following: a \code{pdMat} object, a positive-definite
   matrix, a one-sided linear formula (with variables separated by
   \code{+}), a vector of character strings, or a numeric
   value. Defaults to \code{numeric(0)}, corresponding
   to an uninitialized object.} 
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
}

\description{
  This function is a constructor for the \code{pdIdent} class,
  representing a multiple of the identity positive-definite matrix. 
  The matrix associated with \code{object} is represented by 1
  unrestricted parameter, given by the logarithm of the square-root of
  the diagonal value. When \code{value} is 
  \code{numeric(0)}, an uninitialized \code{pdMat} object, a one-sided
  formula, or a vector of character strings, \code{object} is returned
  as an uninitialized \code{pdIdent} object (with just some of its
  attributes and its class defined) and needs to have its coefficients
  assigned later, generally using the \code{coef} or \code{matrix} replacement
  functions. If \code{value} is an initialized \code{pdMat} object,
  \code{object} will be constructed from
  \code{as.matrix(value)}. Finally, if \code{value} is a numeric value,
  it is assumed to represent the unrestricted coefficient of the
  underlying positive-definite  matrix.  
}
\value{
  a \code{pdIdent} object representing a multiple of the identity
  positive-definite matrix, also inheriting from class \code{pdMat}. 
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{as.matrix.pdMat}},
  \code{\link{coef.pdMat}},
  \code{\link{pdClasses}},
  \code{\link{matrix<-.pdMat}}} 

\examples{
pd1 <- pdIdent(4 * diag(3), nam = c("A","B","C"))
pd1
}
\keyword{models}
