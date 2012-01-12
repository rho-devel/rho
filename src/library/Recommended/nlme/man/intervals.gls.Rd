% File nlme/man/intervals.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: intervals.gls.Rd,v 1.9 2002/03/05 14:59:39 bates Exp $
\name{intervals.gls}
\title{Confidence Intervals on gls Parameters}
\usage{
\method{intervals}{gls}(object, level, which, \dots)
}
\alias{intervals.gls}
\alias{print.intervals.gls}
\arguments{
 \item{object}{an object inheriting from class \code{gls}, representing
   a generalized least squares fitted linear model.}
 \item{level}{an optional numeric value for the interval confidence
   level. Defaults to 0.95.}
 \item{which}{an optional character string specifying the subset
   of parameters for which to construct the confidence
   intervals. Possible values are \code{"all"} for all parameters,
   \code{"var-cov"} for the variance-covariance parameters only, and
   \code{"coef"} for the linear model coefficients  only. Defaults to
   \code{"all"}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Approximate confidence intervals for the parameters in the linear
  model represented by \code{object} are obtained, using
  a normal approximation to the distribution of the (restricted)
  maximum likelihood estimators (the estimators are assumed to have a
  normal distribution centered at the true parameter values and with
  covariance matrix equal to the negative inverse Hessian matrix of the
  (restricted) log-likelihood evaluated at the estimated parameters).
  Confidence intervals are obtained in an unconstrained scale first,
  using the normal approximation, and, if necessary, transformed to the
  constrained scale.
}
\value{
  a list with components given by data frames with rows corresponding to
  parameters and columns \code{lower}, \code{est.}, and \code{upper}
  representing respectively lower confidence limits, the estimated
  values, and upper confidence limits for the parameters. Possible
  components are:
  \item{coef}{linear model coefficients, only present when \code{which}
    is not equal to \code{"var-cov"}.}
  \item{corStruct}{correlation parameters, only present when
    \code{which} is not equal to \code{"coef"} and a 
    correlation structure is used in \code{object}.}
  \item{varFunc}{variance function parameters, only present when
    \code{which} is not equal to \code{"coef"} and a variance function
    structure is used in \code{object}.} 
  \item{sigma}{residual standard error.}
}

\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gls}}, \code{\link{intervals}},
  \code{\link{print.intervals.gls}}
}

\examples{
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
intervals(fm1)
}
\keyword{models}
