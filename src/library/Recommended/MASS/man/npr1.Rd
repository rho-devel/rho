% file MASS/man/npr1.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{npr1}
\alias{npr1}
\title{
  US Naval Petroleum Reserve No. 1 data
}
\description{
  Data on the locations, porosity and permeability (a measure of oil flow)
  on 104 oil wells in the US Naval Petroleum Reserve No. 1 in California.
}
\usage{
npr1
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{x, y}}{
      x and y coordinates, in miles from an arbitrary origin.
    }
    \item{\code{perm}}{
      permeability in milli-Darcies.
    }
    \item{\code{por}}{
      porosity (\%).
    }
  }
}
\source{
  Maher, J.C., Carter, R.D. and Lantz, R.J. (1975)
  Petroleum geology of Naval Petroleum Reserve No. 1, Elk Hills,
  Kern County, California.
  \emph{USGS Professional Paper} \bold{912}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
