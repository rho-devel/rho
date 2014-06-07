% File nlme/man/Initialize.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Initialize.varFunc.Rd,v 1.1.2.2 2003/04/17 22:28:45 bates Exp $
\name{Initialize.varFunc}
\title{Initialize varFunc Object}
\usage{
\method{Initialize}{varFunc}(object, data, \dots)
}
\alias{Initialize.varFunc}
\alias{Initialize.varComb}
\alias{Initialize.varConstPower}
\alias{Initialize.varExp}
\alias{Initialize.varFixed}
\alias{Initialize.varIdent}
\alias{Initialize.varPower}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc},
   representing a variance function structure.} 
 \item{data}{a data frame in which to evaluate the variables named in
   \code{formula(object)}. }
 \item{\dots}{this argument is included to make this method compatible
   with the generic.}
}
\description{
  This method initializes \code{object} by evaluating its associated
  covariate(s) and grouping factor, if any is present, in \code{data};
  determining if the covariate(s) need to be updated when the
  values of the coefficients associated with \code{object} change;
  initializing the log-likelihood and the weights associated with
  \code{object}; and assigning initial values for the coefficients in
  \code{object}, if none were present. The covariate(s) will only be
  initialized if no update is needed when \code{coef(object)} changes. 
}
\value{
  an initialized object with the same class as \code{object}
  representing a variance function structure.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{Initialize}}
}
\examples{
vf1 <- varPower( form = ~ age | Sex )
vf1 <- Initialize( vf1, Orthodont )
}
\keyword{models}
