% File nlme/man/Variogram.corGaus.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Variogram.corGaus.Rd,v 1.7.2.1 2002/08/09 19:45:29 bates Exp $
\name{Variogram.corGaus}
\title{Calculate Semi-variogram for a corGaus Object}
\usage{
\method{Variogram}{corGaus}(object, distance, sig2, length.out, \dots)
}
\alias{Variogram.corGaus}
\arguments{
  \item{object}{an object inheriting from class \code{corGaus},
    representing an Gaussian spatial correlation structure.}
  \item{distance}{an optional numeric vector with the distances at
    which the semi-variogram is to be calculated. Defaults to
    \code{NULL}, in which case a sequence of length \code{length.out}
    between the minimum and maximum values of
    \code{getCovariate(object)} is used.}
  \item{sig2}{an optional numeric value representing the process
    variance. Defaults to \code{1}.}
  \item{length.out}{an optional integer specifying the length of the
    sequence of distances to be used for calculating the semi-variogram,
    when \code{distance = NULL}. Defaults to \code{50}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function calculates the semi-variogram values
  corresponding to the Gaussian correlation model, using the estimated
  coefficients corresponding to \code{object}, at the distances defined
  by \code{distance}.
}
\value{
  a data frame with columns \code{variog} and \code{dist} representing,
  respectively, the semi-variogram values and the corresponding
  distances. The returned value inherits from class \code{Variogram}. 
}
\references{
  Cressie, N.A.C. (1993), "Statistics for Spatial Data", J. Wiley & Sons.
}  
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{corGaus}},
  \code{\link{plot.Variogram}},
  \code{\link{Variogram}}
}
\examples{
cs1 <- corGaus(3, form = ~ Time | Rat)
cs1 <- Initialize(cs1, BodyWeight)
Variogram(cs1)[1:10,]
}
\keyword{models}
