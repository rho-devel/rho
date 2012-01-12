% File nlme/man/fitted.glsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: fitted.glsStruct.Rd,v 1.6.2.2 2003/05/17 21:51:26 bates Exp $
\name{fitted.glsStruct}
\title{Calculate glsStruct Fitted Values}
\usage{
\method{fitted}{glsStruct}(object, glsFit, \dots)
}
\alias{fitted.glsStruct}
\arguments{
 \item{object}{an object inheriting from class \code{glsStruct},
   representing a list of linear model components, such as
   \code{corStruct} and \code{varFunc} objects.}
 \item{glsFit}{an optional list with components \code{logLik}
   (log-likelihood), \code{beta} (coefficients), \code{sigma} (standard
   deviation for error term), \code{varBeta} (coefficients' covariance
   matrix), \code{fitted} (fitted values), and \code{residuals}
   (residuals). Defaults to \code{attr(object, "glsFit")}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The fitted values for the linear model represented by \code{object}
  are extracted.
}
\value{
  a vector with the fitted values for the linear model represented by
  \code{object}. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This method function is generally only used inside \code{gls} and 
  \code{fitted.gls}.
}

\seealso{\code{\link{gls}}, %\code{\link{fitted.gls}},
  \code{\link{residuals.glsStruct}} } 

\keyword{models}
