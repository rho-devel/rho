% File nlme/man/varWeights.glsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varWeights.glsStruct.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varWeights.glsStruct}
\title{Variance Weights for glsStruct Object}
\usage{
\method{varWeights}{glsStruct}(object)
}
\alias{varWeights.glsStruct}
\arguments{
 \item{object}{an object inheriting from class \code{glsStruct},
   representing a list of linear model components, such as
   \code{corStruct} and \code{varFunc} objects.}
}
\description{
  If \code{object} includes a \code{varStruct} component, the inverse of
  the standard deviations of the variance function structure represented
  by the corresponding \code{varFunc} object are returned; else, a
  vector of ones of length equal to the number of observations in the
  data frame used to fit the associated linear model is returned. 
}
\value{
  if \code{object} includes a \code{varStruct} component, a vector with
  the corresponding variance weights; else, or a vector of ones.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{varWeights}}}

\keyword{models}
