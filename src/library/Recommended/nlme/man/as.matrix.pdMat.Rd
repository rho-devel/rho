% File nlme/man/as.matrix.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{as.matrix.pdMat}
\title{Matrix of a pdMat Object}
\usage{
\method{as.matrix}{pdMat}(x, \dots)
}
\alias{as.matrix.pdMat}
\arguments{
 \item{x}{an object inheriting from class \code{pdMat}, representing a
   positive-definite matrix.}
 \item{\dots}{further arguments passed from other methods.}
}
\description{
  This method function extracts the positive-definite matrix represented
  by \code{x}.
}
\value{
  a matrix corresponding to the positive-definite matrix represented by
  \code{x}.
}
\references{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{pdMat}}, \code{\link{corMatrix}}}

\examples{
as.matrix(pdSymm(diag(4)))
}

\keyword{models}
