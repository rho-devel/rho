% File nlme/man/lmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: lmeStruct.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{lmeStruct}
\title{Linear Mixed-Effects Structure}
\usage{
lmeStruct(reStruct, corStruct, varStruct)
}
\alias{lmeStruct}
\arguments{
 \item{reStruct}{a \code{reStruct} representing a random effects
   structure.}
 \item{corStruct}{an optional \code{corStruct} object, representing a
   correlation structure. Default is \code{NULL}.}
 \item{varStruct}{an optional \code{varFunc} object, representing a
   variance function structure. Default is \code{NULL}.}
   
}
\description{
  A linear mixed-effects structure is a list of model components
  representing different sets of parameters in the linear mixed-effects
  model. An \code{lmeStruct} list must contain at least a
  \code{reStruct} object, but may also contain \code{corStruct} and
  \code{varFunc} objects. \code{NULL} arguments are not included in the
  \code{lmeStruct} list. 
}
\value{
  a list of model components determining the parameters to be estimated
  for the associated linear mixed-effects model.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{corClasses}},
  \code{\link{lme}},
  \code{\link{residuals.lmeStruct}},
  \code{\link{reStruct}},
  \code{\link{varFunc}} }

\examples{
lms1 <- lmeStruct(reStruct(~age), corAR1(), varPower())
}
\keyword{models}
