% File nlme/man/intervals.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: intervals.lme.Rd,v 1.8 2002/03/05 14:59:39 bates Exp $
\name{intervals.lme}
\title{Confidence Intervals on lme Parameters}
\usage{
\method{intervals}{lme}(object, level, which, \dots)
}
\alias{intervals.lme}
\alias{print.intervals.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{level}{an optional numeric value with the confidence level for
   the intervals. Defaults to 0.95.} 
 \item{which}{an optional character string specifying the  subset
   of parameters for which to construct the confidence
   intervals. Possible values are \code{"all"} for all parameters,
   \code{"var-cov"} for the variance-covariance parameters only, and
   \code{"fixed"} for the fixed effects only. Defaults to \code{"all"}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Approximate confidence intervals for the parameters in the linear
  mixed-effects model represented by \code{object} are obtained, using
  a normal approximation to the distribution of the (restricted)
  maximum likelihood estimators (the estimators are assumed to have a
  normal distribution centered at the true parameter values and with
  covariance matrix equal to the negative inverse Hessian matrix of the
  (restricted) log-likelihood evaluated at the estimated parameters).
  Confidence intervals are obtained in an unconstrained scale first,
  using the normal approximation, and, if necessary, transformed to the
  constrained scale. The \code{pdNatural} parametrization is used for
  general positive-definite matrices.
}
\value{
  a list with components given by data frames with rows corresponding to
  parameters and columns \code{lower}, \code{est.}, and \code{upper}
  representing respectively lower confidence limits, the estimated
  values, and upper confidence limits for the parameters. Possible
  components are:
  \item{fixed}{fixed effects, only present when \code{which} is not
    equal to \code{"var-cov"}.}
  \item{reStruct}{random effects variance-covariance parameters, only
    present when \code{which} is not equal to \code{"fixed"}.}
  \item{corStruct}{within-group correlation parameters, only
    present when \code{which} is not equal to \code{"fixed"} and a
    correlation structure is used in \code{object}.}
  \item{varFunc}{within-group variance function parameters, only
    present when \code{which} is not equal to \code{"fixed"} and a
    variance function structure is used in \code{object}.}
  \item{sigma}{within-group standard deviation.}
}

\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}}, \code{\link{intervals}},
  \code{\link{print.intervals.lme}},
    \code{\link{pdNatural}}}

\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
intervals(fm1)
}
\keyword{models}

