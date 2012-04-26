% File nlme/man/recalc.modelStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: recalc.modelStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{recalc.modelStruct}
%\title{Recalculate Condensed Linear Model for a \code{modelStruct} Object}
\title{Recalculate for a modelStruct Object}
\usage{
\method{recalc}{modelStruct}(object, conLin, \dots)
}
\alias{recalc.modelStruct}
\arguments{
 \item{object}{an object inheriting from class \code{modelStruct},
   representing a list of model components, such as \code{corStruct} and
   \code{varFunc} objects.}
 \item{conLin}{an optional  condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying model. Defaults to \code{attr(object, "conLin")}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function recalculates the condensed linear model object
  using each element of \code{object} sequentially from last to first.
}
\value{
  the recalculated condensed linear model object.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{This method function is generally only used inside model fitting
  functions, such as \code{lme} and \code{gls}, that allow model
  components, such as correlated error terms and variance functions.
}
\seealso{\code{\link{recalc.corStruct}},
  \code{\link{recalc.reStruct}},
  \code{\link{recalc.varFunc}}}
\keyword{models}
