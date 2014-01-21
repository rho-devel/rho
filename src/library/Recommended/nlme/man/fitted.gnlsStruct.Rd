% File nlme/man/fitted.gnlsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: fitted.gnlsStruct.Rd,v 1.6.2.1 2003/05/17 21:52:25 bates Exp $
\name{fitted.gnlsStruct}
\title{Calculate gnlsStruct Fitted Values}
\usage{
\method{fitted}{gnlsStruct}(object, \dots)
}
\alias{fitted.gnlsStruct}
\arguments{
 \item{object}{an object inheriting from class \code{gnlsStruct},
   representing a list of model components, such as
   \code{corStruct} and \code{varFunc} objects, and attributes
   specifying the underlying nonlinear model and the response variable.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The fitted values for the nonlinear model represented by \code{object}
  are extracted.
}
\value{
  a vector with the fitted values for the nonlinear model represented by
  \code{object}. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This method function is generally only used inside \code{gnls} and 
  \code{fitted.gnls}.
}

\seealso{\code{\link{gnls}}, %\code{\link{fitted.gnls}},
  \code{\link{residuals.gnlsStruct}} } 

\keyword{models}
