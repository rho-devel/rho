% File nlme/man/as.matrix.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{as.matrix.reStruct}
\title{Matrices of an reStruct Object}
\usage{
\method{as.matrix}{reStruct}(x, \dots)
}
\alias{as.matrix.reStruct}
\arguments{
 \item{x}{an object inheriting from class \code{reStruct}, representing
   a random effects structure and consisting of a list of \code{pdMat}
   objects.}
 \item{\dots}{further arguments passed from other methods.}
}
\description{
  This method function extracts the positive-definite matrices
  corresponding to the \code{pdMat} elements of \code{object}. 
}
\value{
  a list with components given by the positive-definite matrices
  corresponding to the elements of \code{object}.
}
\references{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{as.matrix.pdMat}}, \code{\link{reStruct}},
  \code{\link{pdMat}}}

\examples{
rs1 <- reStruct(pdSymm(diag(3), ~age+Sex, data = Orthodont))
as.matrix(rs1)
}
\keyword{models}
