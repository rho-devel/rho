\name{tensor.prod.model.matrix}
\alias{tensor.prod.model.matrix}
\alias{tensor.prod.penalties}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Utility functions for constructing tensor product smooths}
\description{
Produce model matrices or penalty matrices for a tensor product smooth from the model matrices or
penalty matrices for the marginal bases of the smooth.
}
\usage{
tensor.prod.model.matrix(X)
tensor.prod.penalties(S)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{X}{a list of model matrices for the marginal bases of a smooth} 
 \item{S}{a list of penalties for the marginal bases of a smooth.}
}
\details{ If \code{X[[1]]}, \code{X[[2]]} ... \code{X[[m]]} are the model matrices of the marginal bases of 
a tensor product smooth then the ith row of the model matrix for the whole tensor product smooth is given by
\code{X[[1]][i,]\%x\%X[[2]][i,]\%x\% ... X[[m]][i,]}, where \code{\%x\%} is the Kronecker product. Of course 
the routine operates column-wise, not row-wise!

If \code{S[[1]]}, \code{S[[2]]} ... \code{S[[m]]} are  the penalty matrices for the marginal bases, and 
\code{I[[1]]}, \code{I[[2]]} ... \code{I[[m]]} are corresponding identity matrices, each of the same 
dimension as its corresponding penalty, then the tensor product smooth has m associate penalties of the form:

\code{S[[1]]\%x\%I[[2]]\%x\% ... I[[m]]}, 

\code{I[[1]]\%x\%S[[2]]\%x\% ... I[[m]]} 

... 

\code{I[[1]]\%x\%I[[2]]\%x\% ... S[[m]]}. 

Of course it's important that the model matrices and penalty matrices are presented in the same order when 
constructing tensor product smooths.

}
\value{ Either a single model matrix for a tensor product smooth, or a list of penalty terms for a tensor
product smooth.   
}
\references{

Wood, S.N. (2006) Low rank scale invariant tensor product smooths for
Generalized Additive Mixed Models. Biometrics 62(4):1025-1036

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}} 

\seealso{  \code{\link{te}}, \code{\link{smooth.construct.tensor.smooth.spec}}  }

\examples{
require(mgcv)
X <- list(matrix(1:4,2,2),matrix(5:10,2,3))
tensor.prod.model.matrix(X)

S<-list(matrix(c(2,1,1,2),2,2),matrix(c(2,1,0,1,2,1,0,1,2),3,3))
tensor.prod.penalties(S)

}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




