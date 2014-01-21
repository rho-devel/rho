% File nlme/man/Variogram.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Variogram.gls.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{Variogram.gls}
\title{Calculate Semi-variogram for Residuals from a gls Object}
\usage{
\method{Variogram}{gls}(object, distance, form, resType, data,
      na.action, maxDist, length.out, collapse, nint, breaks,
      robust, metric, \dots)
}
\alias{Variogram.gls}
\arguments{
  \item{object}{an object inheriting from class \code{gls}, representing
    a generalized least squares fitted model.}
  \item{distance}{an optional numeric vector with the distances between
    residual pairs. If a grouping variable is present, only the
    distances between residual pairs within the same group should be
    given. If missing, the distances are calculated based on the
    values of the arguments \code{form}, \code{data}, and
    \code{metric}, unless \code{object} includes a \code{corSpatial}
    element, in which case the associated covariate (obtained with the
    \code{getCovariate} method) is used.}
  \item{form}{an optional one-sided formula specifying the covariate(s)
    to be used for calculating the distances between residual pairs and,
    optionally, a grouping factor for partitioning the residuals (which
    must appear to the right of a \code{|} operator in
    \code{form}). Default is \code{~1}, implying that the observation 
    order within the groups is used to obtain the distances.}
  \item{resType}{an optional character string specifying the type of
    residuals to be used. If \code{"response"}, the "raw" residuals
    (observed - fitted) are used; else, if \code{"pearson"}, the
    standardized residuals (raw residuals divided by the corresponding
    standard errors) are used; else, if \code{"normalized"}, the
    normalized residuals (standardized residuals pre-multiplied by the
    inverse square-root factor of the estimated error correlation
    matrix) are used. Partial matching of arguments is used, so only the
    first character needs to be provided. Defaults to \code{"pearson"}.} 
  \item{data}{an optional data frame in which to interpret the variables
    in \code{form}. By default, the same data used to fit \code{object}
    is used.}
  \item{na.action}{a function that indicates what should happen when the
    data contain \code{NA}s. The default action (\code{na.fail}) causes
    an error message to be printed and the function to terminate, if there
    are any incomplete observations.}
  \item{maxDist}{an optional numeric value for the maximum distance used
    for calculating the semi-variogram between two residuals. By default
    all residual pairs are included.}
  \item{length.out}{an optional integer value. When \code{object}
    includes a \code{corSpatial} element, its semi-variogram values are
    calculated and this argument is used as the \code{length.out}
    argument to the corresponding \code{Variogram} method. Defaults to
    \code{50}.}
  \item{collapse}{an optional character string specifying the type of
    collapsing to be applied to the individual semi-variogram values. If
    equal to \code{"quantiles"}, the semi-variogram values are split
    according to quantiles of the distance distribution, with equal
    number of observations per group, with possibly varying distance
    interval lengths. Else, if \code{"fixed"}, the semi-variogram values
    are divided according to distance intervals of equal lengths, with
    possibly different number of observations per interval. Else, if
    \code{"none"}, no collapsing is used and the individual
    semi-variogram values are returned. Defaults to \code{"quantiles"}.}
  \item{nint}{an optional integer with the number of intervals to be
    used when collapsing the semi-variogram values. Defaults to \code{20}.}
  \item{robust}{an optional logical value specifying if a robust
    semi-variogram estimator should be used when collapsing the
    individual values. If \code{TRUE} the robust estimator is
    used. Defaults to \code{FALSE}.}
  \item{breaks}{an optional numeric vector with the breakpoints for the
    distance intervals to be used in collapsing the semi-variogram
    values. If not missing, the option specified in \code{collapse} is
    ignored.}
  \item{metric}{an optional character string specifying the distance
    metric to be used. The currently available options are
    \code{"euclidean"} for the root sum-of-squares of distances;
    \code{"maximum"} for the maximum difference; and \code{"manhattan"}
    for the sum of the absolute differences. Partial matching of
    arguments is used, so only the first three characters need to be
    provided. Defaults to \code{"euclidean"}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function calculates the semi-variogram for the residuals
  from a \code{gls} fit. The semi-variogram values are calculated for
  pairs of residuals within the same group level, if a grouping factor
  is present. If \code{collapse} is different from \code{"none"}, the
  individual semi-variogram values are collapsed using either a robust
  estimator (\code{robust = TRUE}) defined in Cressie (1993), or the
  average of the values within the same distance interval. The
  semi-variogram is useful for modeling the error term correlation
  structure.  
}
\value{
  a data frame with columns \code{variog} and \code{dist} representing,
  respectively, the semi-variogram values and the corresponding
  distances. If the semi-variogram values are collapsed, an extra
  column, \code{n.pairs}, with the number of residual pairs used in each
  semi-variogram calculation, is included in the returned data frame. If
  \code{object} includes a \code{corSpatial} element, a data frame with
  its corresponding semi-variogram is included in the returned value, as
  an attribute \code{"modelVariog"}. The returned value inherits from
  class \code{Variogram}. 
} 
\references{
  Cressie, N.A.C. (1993), "Statistics for Spatial Data", J. Wiley & Sons.
}  

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{gls}},
  \code{\link{Variogram}},
  \code{\link{Variogram.default}},
  \code{\link{Variogram.lme}},
  \code{\link{plot.Variogram}}}
\examples{
\dontrun{
fm1 <- gls(weight ~ Time * Diet, BodyWeight)
Variogram(fm1, form = ~ Time | Rat)[1:10,]
}
}
\keyword{models}
