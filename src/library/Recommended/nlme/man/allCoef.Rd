% File nlme/man/allCoef.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: allCoef.Rd,v 1.4.4.1 2003/04/17 22:28:45 bates Exp $
\name{allCoef}
\title{Extract Coefficients from a Set of Objects}
\usage{
allCoef(\dots, extract)
}
\alias{allCoef}
\arguments{
 \item{\dots}{objects to which \code{extract} will be applied. Generally
 these will be model components, such as \code{corStruct} and
 \code{varFunc} objects.}
 \item{extract}{an optional extractor function. Defaults to \code{coef}.}
}
\description{
  The extractor function is applied to each object in \code{\dots}, with
  the result being converted to a vector. A \code{map} attribute is
  included to indicate which pieces of the returned vector correspond to
  the original objects in \code{dots}.
}
\value{
  a vector with all elements, generally coefficients, obtained by
  applying \code{extract} to the objects in \code{\dots}. 
}
\author{Jose' Pinheiro and Douglas Bates}

\seealso{\code{\link{lmeStruct}},\code{\link{nlmeStruct}}}

\examples{
cs1 <- corAR1(0.1)
vf1 <- varPower(0.5)
allCoef(cs1, vf1)
}
\keyword{models}

