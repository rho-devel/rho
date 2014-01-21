% File nlme/man/Initialize.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Initialize.reStruct.Rd,v 1.1.2.1 2002/08/09 19:45:29 bates Exp $
\name{Initialize.reStruct}
\title{Initialize reStruct Object}
\usage{
\method{Initialize}{reStruct}(object, data, conLin, control, \dots)
}
\alias{Initialize.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{data}{a data frame in which to evaluate the variables defined in
   \code{formula(object)}.}
 \item{conLin}{a condensed linear model object, consisting of a list
   with components \code{"Xy"}, corresponding to a regression matrix
   (\code{X}) combined with a response vector (\code{y}), and
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying model.} 
 \item{control}{an optional list with a single component \code{niterEM}
   controlling the number of iterations for the EM algorithm used to
   refine initial parameter estimates. It is given as a list for
   compatibility with other \code{Initialize} methods. Defaults to
   \code{list(niterEM = 20)}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Initial estimates for the parameters in the \code{pdMat} objects
  forming \code{object}, which have not yet been initialized, are
  obtained using the methodology described in Bates and Pinheiro
  (1998). These estimates may be refined using a series of EM
  iterations, as described in Bates and Pinheiro (1998). The number of
  EM iterations to be used is defined in \code{control}. 
}
\value{
  an \code{reStruct} object similar to \code{object}, but with all
  \code{pdMat} components initialized.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{reStruct}}, \code{\link{pdMat}},
  \code{\link{Initialize}}
}

\keyword{models}
