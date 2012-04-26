% file MASS/man/fitdistr.Rd
% copyright (C) 2001-11 W. N. Venables and B. D. Ripley
%
\name{fitdistr}
\alias{fitdistr}
%\alias{print.fitdistr}
%\alias{coef.fitdistr}
%\alias{logLik.fitdistr}
\title{
  Maximum-likelihood Fitting of Univariate Distributions
}
\description{
  Maximum-likelihood fitting of univariate distributions, allowing
  parameters to be held fixed if desired.
}
\usage{
fitdistr(x, densfun, start, \dots)
}
\arguments{
\item{x}{
  A numeric vector of length at least one containing only \link{finite} values.
}
\item{densfun}{
  Either a character string or a function returning a density evaluated
  at its first argument.

  Distributions \code{"beta"}, \code{"cauchy"}, \code{"chi-squared"},
  \code{"exponential"}, \code{"f"}, \code{"gamma"}, \code{"geometric"},
  \code{"log-normal"}, \code{"lognormal"}, \code{"logistic"},
  \code{"negative binomial"}, \code{"normal"}, \code{"Poisson"},
  \code{"t"} and \code{"weibull"} are recognised, case being ignored.
}
\item{start}{
  A named list giving the parameters to be optimized with initial
  values.  This can be omitted for some of the named distributions and
  must be for others (see Details).
}
\item{\dots}{
  Additional parameters, either for \code{densfun} or for \code{optim}.
  In particular, it can be used to specify bounds via \code{lower} or
  \code{upper} or both.  If arguments of \code{densfun} (or the density
  function corresponding to a character-string specification) are included
  they will be held fixed.
}}
\value{
  An object of class \code{"fitdistr"}, a list with four components,

  \item{estimate}{the parameter estimates,}
  \item{sd}{the estimated standard errors,}
  \item{vcov}{the estimated variance-covariance matrix, and}
  \item{loglik}{the log-likelihood.}
}
\details{
  For the Normal, log-Normal, geometric, exponential and Poisson
  distributions the closed-form MLEs (and exact standard errors) are
  used, and \code{start} should not be supplied.

  For all other distributions, direct optimization of the log-likelihood
  is performed using \code{\link{optim}}.  The estimated standard
  errors are taken from the observed information matrix, calculated by a
  numerical approximation.  For one-dimensional problems the Nelder-Mead
  method is used and for multi-dimensional problems the BFGS method,
  unless arguments named \code{lower} or \code{upper} are supplied (when
  \code{L-BFGS-B} is used) or \code{method} is supplied explicitly.

  For the \code{"t"} named distribution the density is taken to be the
  location-scale family with location \code{m} and scale \code{s}.
  
  For the following named distributions, reasonable starting values will
  be computed if \code{start} is omitted or only partially specified:
  \code{"cauchy"}, \code{"gamma"}, \code{"logistic"},
  \code{"negative binomial"} (parametrized by \code{mu} and
  \code{size}), \code{"t"} and \code{"weibull"}.  Note that these
  starting values may not be good enough if the fit is poor: in
  particular they are not resistant to outliers unless the fitted
  distribution is long-tailed.

  There are \code{\link{print}}, \code{\link{coef}}, \code{\link{vcov}}
  and \code{\link{logLik}} methods for class \code{"fitdistr"}.
}
\note{
  Numerical optimization cannot work miracles: please note the comments
  in \code{\link{optim}} on scaling data.  If the fitted parameters are
  far away from one, consider re-fitting specifying the control
  parameter \code{parscale}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
## avoid spurious accuracy
op <- options(digits = 3)
set.seed(123)
x <- rgamma(100, shape = 5, rate = 0.1)
fitdistr(x, "gamma")
## now do this directly with more control.
fitdistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)

set.seed(123)
x2 <- rt(250, df = 9)
fitdistr(x2, "t", df = 9)
## allow df to vary: not a very good idea!
fitdistr(x2, "t")
## now do fixed-df fit directly with more control.
mydt <- function(x, m, s, df) dt((x-m)/s, df)/s
fitdistr(x2, mydt, list(m = 0, s = 1), df = 9, lower = c(-Inf, 0))

set.seed(123)
x3 <- rweibull(100, shape = 4, scale = 100)
fitdistr(x3, "weibull")

set.seed(123)
x4 <- rnegbin(500, mu = 5, theta = 4)
fitdistr(x4, "Negative Binomial")
options(op)
}
\keyword{distribution}
\keyword{htest}
