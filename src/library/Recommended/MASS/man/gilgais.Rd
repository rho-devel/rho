% file MASS/man/gilgais.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{gilgais}
\alias{gilgais}
\title{
Line Transect of Soil in Gilgai Territory
}
\description{
  This dataset was collected on a line transect survey in gilgai
  territory in New South Wales, Australia.  Gilgais are natural gentle
  depressions in otherwise flat land, and sometimes seem to be regularly
  distributed. The data collection was stimulated by the question: are
  these patterns reflected in soil properties?  At each of 365 sampling
  locations on a linear grid of 4 meters spacing, samples were taken at
  depths 0-10 cm, 30-40 cm and 80-90 cm below the surface. pH, electrical
  conductivity and chloride content were measured on a 1:5 soil:water
  extract from each sample.
}
\usage{
gilgais
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{pH00}}{
      pH at depth 0--10 cm.
    }
    \item{\code{pH30}}{
      pH at depth 30--40 cm.
    }
    \item{\code{pH80}}{
      pH at depth 80--90 cm.
    }
    \item{\code{e00}}{
      electrical conductivity in mS/cm  (0--10 cm).
    }
    \item{\code{e30}}{
      electrical conductivity in mS/cm (30--40 cm).
    }
    \item{\code{e80}}{
      electrical conductivity in mS/cm (80--90 cm).
    }
    \item{\code{c00}}{
      chloride content in ppm  (0--10 cm).
    }
    \item{\code{c30}}{
      chloride content in ppm (30--40 cm).
    }
    \item{\code{c80}}{
      chloride content in ppm (80--90 cm).
    }
  }
}
\source{
  Webster, R. (1977) Spectral analysis of gilgai soil.
  \emph{Australian Journal of Soil Research} \bold{15}, 191--204.

  Laslett, G. M. (1989)
  Kriging and splines: An empirical comparison of their
  predictive performance in some applications (with discussion).
  \emph{Journal of the American Statistical Association} \bold{89}, 319--409
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
