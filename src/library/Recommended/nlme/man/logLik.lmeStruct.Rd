% File nlme/man/logLik.lmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logLik.lmeStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{logLik.lmeStruct}
\title{Log-Likelihood of an lmeStruct Object}
\usage{
\method{logLik}{lmeStruct}(object, Pars, conLin, \dots)
}
\alias{logLik.lmeStruct}
\alias{logLik.lmeStructInt}

\arguments{
 \item{object}{an object inheriting from class \code{lmeStruct},
   representing a list of linear mixed-effects model components, such as
   \code{reStruct}, \code{corStruct}, and \code{varFunc} objects.}
 \item{Pars}{the parameter values at which the (restricted)
   log-likelihood is to be evaluated.}
 \item{conLin}{an optional condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying lme model. Defaults to \code{attr(object, "conLin")}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  \code{Pars} is used to update the coefficients of the model components
  of \code{object} and the individual (restricted) log-likelihood
  contributions of each component are added together. The type of
  log-likelihood (restricted or not) is determined by the
  \code{settings} attribute of \code{object}.
}
\value{
  the (restricted) log-likelihood for the linear mixed-effects model
  described by \code{object}, evaluated at \code{Pars}.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}},
  \code{\link{lmeStruct}},
  \code{\link{logLik.lme}}
}

\keyword{models}
