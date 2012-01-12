% File nlme/man/residuals.gnlsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: residuals.gnlsStruct.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{residuals.gnlsStruct}
\title{Calculate gnlsStruct Residuals}
\usage{
\method{residuals}{gnlsStruct}(object, \dots)
}
\alias{residuals.gnlsStruct}
\arguments{
 \item{object}{an object inheriting from class \code{gnlsStruct},
   representing a list of model components, such as
   \code{corStruct} and \code{varFunc} objects, and attributes
   specifying the underlying nonlinear model and the response variable.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The residuals for the nonlinear model represented by \code{object}
  are extracted.
}
\value{
  a vector with the residuals for the nonlinear model represented by
  \code{object}. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This method function is primarily used inside \code{gnls} and 
  \code{residuals.gnls}.
}
\seealso{\code{\link{gnls}},
  \code{\link{residuals.gnls}},
  \code{\link{fitted.gnlsStruct}} } 
\keyword{models}
