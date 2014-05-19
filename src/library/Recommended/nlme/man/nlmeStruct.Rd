% File nlme/man/nlmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: nlmeStruct.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{nlmeStruct}
\title{Nonlinear Mixed-Effects Structure}
\usage{
nlmeStruct(reStruct, corStruct, varStruct)
}
\alias{nlmeStruct}
\arguments{
 \item{reStruct}{a \code{reStruct} representing a random effects
   structure.}
 \item{corStruct}{an optional \code{corStruct} object, representing a
   correlation structure. Default is \code{NULL}.}
 \item{varStruct}{an optional \code{varFunc} object, representing a
   variance function structure. Default is \code{NULL}.}
}
\description{
  A nonlinear mixed-effects structure is a list of model components
  representing different sets of parameters in the nonlinear mixed-effects
  model. An \code{nlmeStruct} list must contain at least a
  \code{reStruct} object, but may also contain \code{corStruct} and
  \code{varFunc} objects. \code{NULL} arguments are not included in the
  \code{nlmeStruct} list. 
}
\value{
  a list of model components determining the parameters to be estimated
  for the associated nonlinear mixed-effects model.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{corClasses}},
  \code{\link{nlme}},
  \code{\link{residuals.nlmeStruct}},
  \code{\link{reStruct}},
  \code{\link{varFunc}} }

\examples{
nlms1 <- nlmeStruct(reStruct(~age), corAR1(), varPower())
}
\keyword{models}
