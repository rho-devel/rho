% File nlme/man/pdNatural.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdNatural.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{pdNatural}
\title{General Positive-Definite Matrix in Natural Parametrization}
\usage{
pdNatural(value, form, nam, data)
}
\alias{pdNatural}
\arguments{
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
}

\description{
  This function is a constructor for the \code{pdNatural} class,
  representing a general positive-definite matrix, using a natural
  parametrization . If the matrix associated with \code{object} is of
  dimension \eqn{n}, it is represented by \eqn{n(n+1)/2}{n*(n+1)/2}
  parameters. Letting \eqn{\sigma_{ij}}{S(i,j)} denote the \eqn{ij}-th
  element of the underlying positive definite matrix and
  \eqn{\rho_{ij}=\sigma_{i}/\sqrt{\sigma_{ii}\sigma_{jj}},\;i\neq j}{r(i,j) =
    S(i,j)/sqrt(S(i,i)S(j,j)), i not equal to j} denote the associated
  "correlations", the "natural" parameters are given by
  \eqn{\sqrt{\sigma_{ii}}, \;i=1,\ldots,n}{sqrt(Sii), i=1,..,n} and
  \eqn{\log((1+\rho_{ij})/(1-\rho_{ij})),\; i \neq
    j}{log((1+r(i,j))/(1-r(i,j))), i not equal to j}. Note that all
  natural parameters are individually unrestricted, but not jointly
  unrestricted (meaning that not all unrestricted vectors would give
  positive-definite matrices). Therefore, this parametrization should
  NOT be used for optimization. It is mostly used for deriving
  approximate confidence intervals on parameters following the
  optimization of an objective function. When \code{value} is
  \code{numeric(0)}, an uninitialized \code{pdMat} object, a one-sided
  formula, or a vector of character strings, \code{object} is returned
  as an uninitialized \code{pdSymm} object (with just some of its
  attributes and its class defined) and needs to have its coefficients
  assigned later, generally using the \code{coef} or \code{matrix} replacement
  functions. If \code{value} is an initialized \code{pdMat} object,
  \code{object} will be constructed from
  \code{as.matrix(value)}. Finally, if \code{value} is a numeric 
  vector, it is assumed to represent the natural parameters of the
  underlying positive-definite matrix.  
}
\value{
  a \code{pdNatural} object representing a general positive-definite
  matrix in natural parametrization, also inheriting from class
  \code{pdMat}.  
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer, esp. p. 162.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{as.matrix.pdMat}},
  \code{\link{coef.pdMat}},
  \code{\link{pdClasses}},
  \code{\link{matrix<-.pdMat}}} 
\examples{
pdNatural(diag(1:3))
}
\keyword{models}
