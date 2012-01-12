% File nlme/man/logLik.gnlsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logLik.gnlsStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{logLik.gnlsStruct}
\title{Log-Likelihood of a gnlsStruct Object}
\usage{
\method{logLik}{gnlsStruct}(object, Pars, conLin, \dots)
}
\alias{logLik.gnlsStruct}

\arguments{
 \item{object}{an object inheriting from class \code{gnlsStruct},
   representing a list of model components, such as
   \code{corStruct} and \code{varFunc} objects, and attributes
   specifying the underlying nonlinear model and the response variable.}
 \item{Pars}{the parameter values at which the log-likelihood is to be
   evaluated.} 
 \item{conLin}{an optional condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying nonlinear model. Defaults to \code{attr(object,
     "conLin")}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  \code{Pars} is used to update the coefficients of the model components
  of \code{object} and the individual log-likelihood
  contributions of each component are added together.
}
\value{
  the log-likelihood for the linear model described by \code{object},
  evaluated at \code{Pars}.  
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gnls}}, \code{\link{gnlsStruct}},
  \code{\link{logLik.gnls}}
}

\keyword{models}
