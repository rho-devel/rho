% File nlme/man/Initialize.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Initialize.corStruct.Rd,v 1.1.2.1 2002/08/09 19:45:29 bates Exp $
\name{Initialize.corStruct}
\title{Initialize corStruct Object}
\usage{
\method{Initialize}{corStruct}(object, data, \dots)
}
\alias{Initialize.corStruct}
\alias{Initialize.corAR1}
\alias{Initialize.corARMA}
\alias{Initialize.corCAR1}
\alias{Initialize.corCompSymm}
\alias{Initialize.corHF}
\alias{Initialize.corIdent}
\alias{Initialize.corLin}
\alias{Initialize.corNatural}
\alias{Initialize.corSpatial}
\alias{Initialize.corSpher}
\alias{Initialize.corSymm}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct}
   representing a correlation structure.}
 \item{data}{a data frame in which to evaluate the variables defined in
   \code{formula(object)}.}
 \item{\dots}{this argument is included to make this method compatible
   with the generic.}
}
\description{
  This method initializes \code{object} by evaluating its associated
  covariate(s) and grouping factor, if any is present, in \code{data},
  calculating various dimensions and constants used by optimization
  algorithms involving \code{corStruct} objects (see the appropriate
  \code{Dim} method documentation), and assigning initial values for
  the coefficients in \code{object}, if none were present.
}
\value{
  an initialized object with the same class as \code{object}
  representing a correlation structure.
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{Dim.corStruct}}}

\examples{
cs1 <- corAR1(form = ~ 1 | Subject)
cs1 <- Initialize(cs1, data = Orthodont)
}
\keyword{models}
