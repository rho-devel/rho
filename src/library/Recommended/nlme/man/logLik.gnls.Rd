% File nlme/man/logLik.gnls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{logLik.gnls}
\title{Log-Likelihood of a gnls Object}
\usage{
\method{logLik}{gnls}(object, REML, \dots)
}
\alias{logLik.gnls}
\arguments{
 \item{object}{an object inheriting from class \code{gnls}, representing
   a generalized nonlinear least squares fitted model.}
 \item{REML}{an logical value for consistency with \code{logLik,gls},
   but only \code{FALSE} is accepted.. }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Returns the log-likelihood value of the nonlinear model represented by
  \code{object} evaluated at the estimated coefficients.  
}
\value{
  the log-likelihood of the linear model represented by
  \code{object} evaluated at the estimated coefficients.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{gnls}},
  \code{\link{logLik.lme}}
}
\examples{
fm1 <- gnls(weight ~ SSlogis(Time, Asym, xmid, scal), Soybean,
            weights = varPower())
logLik(fm1)
}
\keyword{models}
