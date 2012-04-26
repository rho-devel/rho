% File nlme/man/pdFactor.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pdFactor.reStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{pdFactor.reStruct}
\title{Extract Square-Root Factor from Components of an reStruct Object}
\usage{
\method{pdFactor}{reStruct}(object)
}
\alias{pdFactor.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
}
\description{
  This method function extracts square-root factors of the
  positive-definite matrices corresponding to the \code{pdMat} elements
  of \code{object}.  
}
\value{
  a vector with square-root factors of the positive-definite matrices
  corresponding to the elements of \code{object} stacked column-wise.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This function is used intensively in optimization
  algorithms and its value is returned as a vector for efficiency
  reasons. The \code{pdMatrix} function can be used to obtain
  square-root factors in matrix form.
}
\seealso{\code{\link{pdFactor}},
  \code{\link{pdMatrix.reStruct}},
  \code{\link{pdFactor.pdMat}}}
\examples{
rs1 <- reStruct(pdSymm(diag(3), ~age+Sex, data = Orthodont))
pdFactor(rs1)
}
\keyword{models}
