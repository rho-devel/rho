% file MASS/man/michelson.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{michelson}
\alias{michelson}
\title{
  Michelson's Speed of Light Data
}
\description{
  Measurements of the speed of light in air, made between 5th June
  and 2nd July, 1879.  The data consists of five experiments, each
  consisting of 20 consecutive runs.  The response is the speed
  of light in km/s, less 299000.  The currently accepted value, on
  this scale of measurement, is 734.5.
}
\usage{
michelson
}
\format{
  The data frame contains the following components:
  \describe{
    \item{\code{Expt}}{
      The experiment number, from 1 to 5.
    }
    \item{\code{Run}}{
      The run number within each experiment.
    }
    \item{\code{Speed}}{
      Speed-of-light measurement.
    }
  }
}
\source{
A.J. Weekes (1986) \emph{A Genstat Primer.} Edward Arnold.

S. M. Stigler (1977) Do robust estimators work with real data?
\emph{Annals of Statistics} \bold{5}, 1055--1098.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
