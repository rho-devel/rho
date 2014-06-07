% File nlme/man/recalc.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: recalc.varFunc.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{recalc.varFunc}
\title{Recalculate for varFunc Object}
\usage{
\method{recalc}{varFunc}(object, conLin, \dots)
}
\alias{recalc.varFunc}
\alias{recalc.varIdent}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc},
   representing a variance function structure.} 
 \item{conLin}{a condensed linear model object, consisting of a list
   with components \code{"Xy"}, corresponding to a regression matrix
   (\code{X}) combined with a response vector (\code{y}), and
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying model.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function pre-multiples the \code{"Xy"} component of
  \code{conLin} by a diagonal matrix with diagonal elements given by the
  weights corresponding to the variance structure represented by
  \code{object}e and adds the log-likelihood contribution of
  \code{object}, given by \code{logLik(object)}, to the \code{"logLik"}
  component of \code{conLin}.  
}
\value{
  the recalculated condensed linear model object.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{This method function is only used inside model fitting functions,
    such as \code{lme} and \code{gls}, that allow heteroscedastic error
    terms.}
\seealso{
  \code{\link{recalc}},
  \code{\link{varWeights}},
  \code{\link{logLik.varFunc}}}
\keyword{models}
