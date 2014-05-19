% File nlme/man/recalc.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: recalc.corStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{recalc.corStruct}
\title{Recalculate for corStruct Object}
\usage{
\method{recalc}{corStruct}(object, conLin, \dots)
}
\alias{recalc.corStruct}
\alias{recalc.corAR1}
\alias{recalc.corARMA}
\alias{recalc.corCAR1}
\alias{recalc.corCompSymm}
\alias{recalc.corHF}
\alias{recalc.corIdent}
\alias{recalc.corNatural}
\alias{recalc.corSpatial}
\alias{recalc.corSymm}

\arguments{
 \item{object}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
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
  \code{conLin} by the transpose square-root factor(s) of the
  correlation matrix (matrices) associated with \code{object} and adds
  the log-likelihood contribution of \code{object}, given by
  \code{logLik(object)}, to the \code{"logLik"} component of
  \code{conLin}. 
}
\value{
  the recalculated condensed linear model object.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{This method function is only used inside model
  fitting functions, such as \code{lme} and \code{gls},
  that allow correlated error terms.}
\seealso{\code{\link{corFactor}},
    \code{\link{logLik.corStruct}}
}
\keyword{models}
