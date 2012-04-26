% File nlme/man/as.matrix.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{as.matrix.corStruct}
\title{Matrix of a corStruct Object}
\usage{
\method{as.matrix}{corStruct}(x, \dots)
}
\alias{as.matrix.corStruct}
\arguments{
 \item{x}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
 \item{\dots}{further arguments passed from other methods.}
}
\description{
  This method function extracts the correlation matrix, or list of
  correlation matrices, associated with \code{object}.
}
\value{
  If the correlation structure includes a grouping factor, the returned
  value will be a list with components given by the correlation
  matrices for each group. Otherwise, the returned value will be a
  matrix representing the correlation structure associated with
  \code{object}. 
}
\references{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{corClasses}}, \code{\link{corMatrix}}}
\examples{
cst1 <- corAR1(form = ~1|Subject)
cst1 <- Initialize(cst1, data = Orthodont)
as.matrix(cst1)
}
\keyword{models}
