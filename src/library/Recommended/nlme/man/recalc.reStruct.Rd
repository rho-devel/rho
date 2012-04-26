% File nlme/man/recalc.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: recalc.reStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{recalc.reStruct}
\title{Recalculate for an reStruct Object}
\usage{
\method{recalc}{reStruct}(object, conLin, \dots)
}
\alias{recalc.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{conLin}{a condensed linear model object, consisting of a list
   with components \code{"Xy"}, corresponding to a regression matrix
   (\code{X}) combined with a response vector (\code{y}), and
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying model.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The log-likelihood, or restricted log-likelihood, of the
  Gaussian linear mixed-effects model represented by \code{object} and
  \code{conLin} (assuming spherical within-group covariance structure),
  evaluated at \code{coef(object)} is calculated and added to the
  \code{logLik} component of \code{conLin}. The \code{settings}
  attribute of \code{object} determines whether the log-likelihood, or
  the restricted log-likelihood, is to be calculated. The computational
  methods for the (restricted) log-likelihood calculations are described
  in Bates and Pinheiro (1998). 
}
\value{
  the condensed linear model with its \code{logLik} component updated.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{logLik}},
  \code{\link{lme}}, 
  \code{\link{recalc}},
  \code{\link{reStruct}}
} 

\keyword{models}
