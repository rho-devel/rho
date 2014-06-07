\name{vcov.gam}
\alias{vcov.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Extract parameter (estimator) covariance matrix from GAM fit}
\description{ Extracts the Bayesian posterior covariance matrix of the
parameters or frequentist covariance matrix of the parameter estimators 
from a fitted \code{gam} object.
}
\usage{
\method{vcov}{gam}(object, freq = FALSE, dispersion = NULL, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{object}{ fitted model object of class \code{gam} as produced by \code{gam()}.}
\item{freq}{ \code{TRUE} to return the frequentist covariance matrix of the
parameter estimators, \code{FALSE} to return the Bayesian posterior covariance
matrix of the parameters.} 
\item{dispersion}{ a value for the dispersion parameter: not normally used.}
\item{...}{ other arguments, currently ignored.}
}
\details{ Basically, just extracts \code{object$Ve} or \code{object$Vp} from a \code{\link{gamObject}}.
}

\value{ A matrix corresponding to the estimated frequentist covariance matrix
of the model parameter estimators/coefficients, or the estimated posterior
covariance matrix of the parameters, depending on the argument \code{freq}.
}

\author{ Henric Nilsson. 
Maintained by Simon N. Wood \email{simon.wood@r-project.org}
}

\references{
Wood, S.N. (2006) On confidence intervals for generalized additive models based on penalized regression splines. Australian and New Zealand Journal of Statistics. 48(4): 445-464.
}


\seealso{ \code{\link{gam}}}


\examples{ 
require(mgcv)
n <- 100
x <- runif(n)
y <- sin(x*2*pi) + rnorm(n)*.2
mod <- gam(y~s(x,bs="cc",k=10),knots=list(x=seq(0,1,length=10)))
diag(vcov(mod))
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






