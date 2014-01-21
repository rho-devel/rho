\name{model.matrix.gam}
\alias{model.matrix.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Extract model matrix from GAM fit}
\description{Obtains the model matrix from a fitted \code{gam} object.
}
\usage{
\method{model.matrix}{gam}(object, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{object}{ fitted model object of class \code{gam} as produced by \code{gam()}.}

\item{...}{ other arguments, passed to \code{\link{predict.gam}}.}
}
\details{Calls \code{\link{predict.gam}} with no \code{newdata} argument and
\code{type="lpmatrix"} in order to obtain the model matrix of \code{object}.
}

\value{ A model matrix.
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}
}

\references{
Wood S.N. (2006b) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.

}


\seealso{ \code{\link{gam}}}


\examples{ 
require(mgcv)
n <- 15
x <- runif(n)
y <- sin(x*2*pi) + rnorm(n)*.2
mod <- gam(y~s(x,bs="cc",k=6),knots=list(x=seq(0,1,length=6)))
model.matrix(mod)
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






