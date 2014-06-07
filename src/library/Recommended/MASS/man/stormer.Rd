% file MASS/man/stormer.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{stormer}
\alias{stormer}
\title{
  The Stormer Viscometer Data
}
\description{
  The stormer viscometer measures the viscosity of a fluid by measuring the
  time taken for an inner cylinder in the mechanism to perform a fixed number
  of revolutions in response to an actuating weight.  The viscometer is
  calibrated by measuring the time taken with varying weights while the
  mechanism is suspended in fluids of accurately known viscosity.  The data
  comes from such a calibration, and theoretical considerations suggest a
  nonlinear relationship between time, weight and viscosity, of the form
  \code{Time = (B1*Viscosity)/(Weight - B2) + E}
  where \code{B1} and \code{B2}
  are unknown parameters to be estimated, and \code{E} is error.
}
\usage{
stormer
}
\format{
  The data frame contains the following components:
  \describe{
    \item{\code{Viscosity}}{
      viscosity of fluid.
    }
    \item{\code{Wt}}{
      actuating weight.
    }
    \item{\code{Time}}{
      time taken.
    }
  }
}
\source{
  E. J. Williams (1959) \emph{Regression Analysis.} Wiley.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
