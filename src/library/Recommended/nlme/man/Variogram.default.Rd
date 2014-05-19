% File nlme/man/Variogram.default.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Variogram.default.Rd,v 1.8.2.1 2003/04/17 22:28:45 bates Exp $
\name{Variogram.default}
\title{Calculate Semi-variogram}
\usage{
\method{Variogram}{default}(object, distance, \dots)
}
\alias{Variogram.default}
\arguments{
  \item{object}{a numeric vector with the values to be used for
    calculating the semi-variogram, usually a residual vector from a
    fitted model.}
  \item{distance}{a numeric vector with the pairwise distances
    corresponding to the elements of \code{object}. The order of the
    elements in \code{distance} must correspond to the pairs
    \code{(1,2), (1,3), \dots, (n-1,n)}, with \code{n} representing the
    length of \code{object}, and must have length \code{n(n-1)/2}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function calculates the semi-variogram for an arbitrary
  vector \code{object}, according to the distances in \code{distance}.
  For each pair of elements \eqn{x,y} in \code{object}, the
  corresponding semi-variogram is \eqn{(x-y)^2/2}. The semi-variogram is
  useful for identifying and modeling spatial correlation structures in
  observations with constant expectation and constant variance.   
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
  \code{\link{Variogram}},
  \code{\link{Variogram.gls}},
  \code{\link{Variogram.lme}},
  \code{\link{plot.Variogram}}} 
\examples{
\dontrun{
fm1 <- lm(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), Ovary,
          subset = Mare == 1)
Variogram(resid(fm1), dist(1:29))[1:10,]
}
}
\keyword{models}
