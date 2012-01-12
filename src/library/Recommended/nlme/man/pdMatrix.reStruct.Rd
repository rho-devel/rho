% File nlme/man/pdMatrix.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdMatrix.reStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{pdMatrix.reStruct}
\title{Extract Matrix or Square-Root Factor from Components of an
  reStruct Object} 
\usage{
\method{pdMatrix}{reStruct}(object, factor)
}
\alias{pdMatrix.reStruct}

\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{factor}{an optional logical value. If \code{TRUE}, square-root
   factors of the positive-definite matrices represented by the elements
   of \code{object} are returned; else, if \code{FALSE}, the
   positive-definite matrices are returned. Defaults to \code{FALSE}.} 
}
\description{
  This method function extracts the positive-definite  matrices
  corresponding to the \code{pdMat} elements of \code{object}, or
  square-root factors of the positive-definite matrices. 
}
\value{
  a list with components given by the positive-definite matrices
  corresponding to the elements of \code{object}, or square-root factors
  of the positive-definite matrices.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer, esp. p. 162.  
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{as.matrix.reStruct}},
  \code{\link{reStruct}},
  \code{\link{pdMat}}, 
  \code{\link{pdMatrix}}, 
  \code{\link{pdMatrix.pdMat}}
}

\examples{
rs1 <- reStruct(pdSymm(diag(3), ~age+Sex, data = Orthodont))
pdMatrix(rs1)
}
\keyword{models}
