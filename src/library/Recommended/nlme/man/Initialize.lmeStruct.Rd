% File nlme/man/Initialize.lmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Initialize.lmeStruct.Rd,v 1.1.2.1 2002/08/09 19:45:29 bates Exp $
\name{Initialize.lmeStruct}
\title{Initialize an lmeStruct Object}
\usage{
\method{Initialize}{lmeStruct}(object, data, groups, conLin, control, \dots)
}
\alias{Initialize.lmeStruct}
\arguments{
 \item{object}{an object inheriting from class \code{lmeStruct},
   representing a list of linear mixed-effects model components, such as
   \code{reStruct}, \code{corStruct}, and \code{varFunc} objects.}
 \item{data}{a data frame in which to evaluate the variables defined in
   \code{formula(object)}.}
 \item{groups}{a data frame with the grouping factors corresponding to
   the lme model associated with \code{object} as columns, sorted from
   innermost to outermost grouping level.}
 \item{conLin}{an optional condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying lme model. Defaults to \code{attr(object, "conLin")}.}
 \item{control}{an optional list with control parameters for the
   initialization and optimization algorithms used in
   \code{lme}. Defaults to \code{list(niterEM=20, gradHess=TRUE)},
   implying that 20 EM iterations are to be used in the derivation of
   initial estimates for the coefficients of the \code{reStruct}
   component of \code{object} and, if possible, numerical gradient
   vectors and Hessian matrices for the log-likelihood function are to
   be used in the optimization algorithm.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The individual linear mixed-effects model components of the
  \code{lmeStruct} list are initialized. 
}
\value{
  an \code{lmeStruct} object similar to \code{object}, but with
  initialized model components. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}}, \code{\link{Initialize.reStruct}},
  \code{\link{Initialize.corStruct}},
  \code{\link{Initialize.varFunc}}, \code{\link{Initialize}}
}

\keyword{models}
