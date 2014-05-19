\name{sp.vcov}
\alias{sp.vcov}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Extract smoothing parameter estimator covariance matrix from (RE)ML GAM fit}
\description{ Extracts the estimated covariance matrix for the log smoothing parameter
estimates from a (RE)ML estimated \code{gam} object, provided the fit was with a method 
that evaluated the required Hessian.
}
\usage{
sp.vcov(x)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{x}{ a fitted model object of class \code{gam} as produced by \code{gam()}.}
}
\details{ Just extracts the inverse of the hessian matrix of the negative (restricted) log likelihood w.r.t
the log smoothing parameters, if this has been obtained as part of fitting. 
}

\value{ A matrix corresponding to the estimated covariance matrix of the log smoothing parameter estimators,
if this can be extracted, otherwise \code{NULL}. If the scale parameter has been (RE)ML estimated (i.e. if the method was 
\code{"ML"} or \code{"REML"} and the scale parameter was unknown) then the 
last row and column relate to the log scale parameter.  
}

\author{Simon N. Wood \email{simon.wood@r-project.org}
}

\references{
Wood, S.N. (2006) On confidence intervals for generalized additive models based on penalized regression splines. Australian and New Zealand Journal of Statistics. 48(4): 445-464.
}


\seealso{ \code{\link{gam}}, \code{\link{gam.vcomp}}}


\examples{ 
require(mgcv)
n <- 100
x <- runif(n);z <- runif(n)
y <- sin(x*2*pi) + rnorm(n)*.2
mod <- gam(y~s(x,bs="cc",k=10)+s(z),knots=list(x=seq(0,1,length=10)),
           method="REML")
sp.vcov(mod)
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






