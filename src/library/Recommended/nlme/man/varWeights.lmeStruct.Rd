% File nlme/man/varWeights.lmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varWeights.lmeStruct.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varWeights.lmeStruct}
\title{Variance Weights for lmeStruct Object}
\usage{
\method{varWeights}{lmeStruct}(object)
}
\alias{varWeights.lmeStruct}
\arguments{
  \item{object}{an object inheriting from class \code{lmeStruct},
    representing a list of linear mixed-effects model components, such as
    \code{reStruct}, \code{corStruct}, and \code{varFunc} objects.}
}
\description{
  If \code{object} includes a \code{varStruct} component, the inverse of
  the standard deviations of the variance function structure represented
  by the corresponding \code{varFunc} object are returned; else, a
  vector of ones of length equal to the number of observations in the
  data frame used to fit the associated linear mixed-effects model is
  returned. 
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
