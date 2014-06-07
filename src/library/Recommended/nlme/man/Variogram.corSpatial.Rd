% File nlme/man/Variogram.corSpatial.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Variogram.corSpatial.Rd,v 1.7.2.2 2003/04/17 22:28:45 bates Exp $
\name{Variogram.corSpatial}
\title{Calculate Semi-variogram for a corSpatial Object}
\usage{
\method{Variogram}{corSpatial}(object, distance, sig2, length.out, FUN, \dots)
}
\alias{Variogram.corSpatial}
\arguments{
  \item{object}{an object inheriting from class \code{corSpatial},
    representing spatial correlation structure.}
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
  \item{FUN}{a function of two arguments, the distance and the range
    corresponding to \code{object}, specifying the semi-variogram
    model.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function calculates the semi-variogram values
  corresponding to the model defined in \code{FUN}, using the estimated
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
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{corSpatial}},
  \code{\link{Variogram}},
  \code{\link{Variogram.default}},
  \code{\link{Variogram.corExp}},
  \code{\link{Variogram.corGaus}},
  \code{\link{Variogram.corLin}},
  \code{\link{Variogram.corRatio}},
  \code{\link{Variogram.corSpher}},
  \code{\link{plot.Variogram}}
}
\examples{
cs1 <- corExp(3, form = ~ Time | Rat)
cs1 <- Initialize(cs1, BodyWeight)
Variogram(cs1, FUN = function(x, y) (1 - exp(-x/y)))[1:10,]
}
\keyword{models}
