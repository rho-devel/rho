% File nlme/man/pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdMat.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{pdMat}
\title{Positive-Definite Matrix}
\usage{
pdMat(value, form, nam, data, pdClass)
}
\alias{pdMat}
\alias{plot.pdMat}
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
 \item{pdClass}{an optional character string naming the
   \code{pdMat} class to be assigned to the returned object. This
   argument will only be used when \code{value} is not a \code{pdMat} 
   object. Defaults to \code{"pdSymm"}.} 
}

\description{
  This function gives an alternative way of constructing an object
  inheriting from the \code{pdMat} class named in \code{pdClass}, or
  from \code{data.class(object)} if \code{object} inherits from
  \code{pdMat},  and is mostly used internally in other functions. See
  the documentation on the principal constructor function, generally
  with the same name as the \code{pdMat} class of object. 
}
\value{
  a \code{pdMat} object representing a positive-definite matrix,
  inheriting from the class named in \code{pdClass}, or from
  \code{class(object)}, if \code{object} inherits from \code{pdMat}.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{pdClasses}}, 
  \code{\link{pdCompSymm}}, \code{\link{pdDiag}},
  \code{\link{pdIdent}}, \code{\link{pdNatural}},
  \code{\link{pdSymm}},
  \code{\link{reStruct}},
  \code{\link{solve.pdMat}}, 
  \code{\link{summary.pdMat}}
}

\examples{
pd1 <- pdMat(diag(1:4), pdClass = "pdDiag")
pd1
}
\keyword{models}
