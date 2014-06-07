% File nlme/man/update.modelStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: update.modelStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{update.modelStruct}
\title{Update a modelStruct Object}
\usage{
\method{update}{modelStruct}(object, data, \dots)
}
\alias{update.modelStruct}
\alias{update.corStruct}
\alias{update.reStruct}
\arguments{
  \item{object}{an object inheriting from class \code{modelStruct},
    representing a list of model components, such as \code{corStruct} and
    \code{varFunc} objects.}
  \item{data}{a data frame in which to evaluate the variables needed for
    updating the elements of \code{object}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
    This method function updates each element of \code{object}, allowing
    the access to \code{data}.
}
\value{
    an object similar to \code{object} (same class, length, and names),
    but with updated elements.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{reStruct}}
} 
\note{This method function is primarily used within model fitting
  functions, such as \code{lme} and \code{gls}, that allow model
  components such as variance functions.}
\keyword{models}
