\name{mroot}
\alias{mroot}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Smallest square root of matrix}
\description{ Find a square root of a positive semi-definite matrix, 
having as few columns as possible. Uses either pivoted choleski 
decomposition or singular value decomposition to do this.  
}
\usage{
mroot(A,rank=NULL,method="chol")
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{A}{ The positive semi-definite matrix, a square root of which is 
to be found.}
\item{rank}{if the rank of the matrix \code{A} is known then it should 
be supplied. \code{NULL} or <1 imply that it should be estimated.}
\item{method}{ \code{"chol"} to use pivoted choloeski decompositon, 
which is fast but tends to over-estimate rank. \code{"svd"} to use 
singular value decomposition, which is slow, but is the most accurate way 
to estimate rank.}
}

\details{ The function uses SVD, or a pivoted 
Choleski routine. It is primarily of use for turning penalized regression 
problems into ordinary regression problems.} 
\value{ A matrix, \eqn{ {\bf B}}{B} with as many columns as the rank of 
\eqn{ {\bf A}}{A}, and such that \eqn{ {\bf A} = {\bf BB}^\prime}{A=BB'}.}
 

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\examples{
  require(mgcv)
  set.seed(0)
  a <- matrix(runif(24),6,4)
  A <- a\%*\%t(a) ## A is +ve semi-definite, rank 4
  B <- mroot(A) ## default pivoted choleski method
  tol <- 100*.Machine$double.eps
  chol.err <- max(abs(A-B\%*\%t(B)));chol.err
  if (chol.err>tol) warning("mroot (chol) suspect")
  B <- mroot(A,method="svd") ## svd method
  svd.err <- max(abs(A-B\%*\%t(B)));svd.err
  if (svd.err>tol) warning("mroot (svd) suspect")  
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









