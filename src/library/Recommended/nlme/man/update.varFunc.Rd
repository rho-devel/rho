% File nlme/man/update.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: update.varFunc.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{update.varFunc}
\title{Update varFunc Object}
\usage{
\method{update}{varFunc}(object, data, \dots)
}
\alias{update.varExp}
\alias{update.varFunc}
\alias{update.varComb}
\alias{update.varConstPower}
\alias{update.varExpon}
\alias{update.varPower}
\arguments{
  \item{object}{an object inheriting from class \code{varFunc},
    representing a variance function structure.} 
  \item{data}{a list with a component named \code{"."} with the current
    version of the fitted object (from which fitted values, coefficients,
    and residuals can be extracted) and, if necessary, other variables
    used to evaluate the variance covariate(s).}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  If the \code{formula(object)} includes a \code{"."} term, representing
  a fitted object, the variance covariate needs to be updated upon
  completion of an optimization cycle (in which the variance function
  weights are kept fixed). This method function allows a reevaluation of
  the variance covariate using the current fitted object and,
  optionally, other variables in the original data.
}
\value{
  if \code{formula(object)} includes a \code{"."} term, an
  \code{varFunc} object similar to \code{object}, but with the 
  variance covariate reevaluated at the current fitted object value;
  else \code{object} is returned unchanged.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{needUpdate}},
  \code{\link{covariate<-.varFunc}}
}
\keyword{models}
