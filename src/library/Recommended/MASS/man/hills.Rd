% file MASS/man/hills.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{hills}
\alias{hills}
\title{
  Record Times in Scottish Hill Races
}
\description{
  The record times in 1984 for 35 Scottish hill races.
}
\usage{
hills
}
\format{
  The components are:
  \describe{
    \item{\code{dist}}{
      distance in miles (on the map).
    }
    \item{\code{climb}}{
      total height gained during the route, in feet.
    }
    \item{\code{time}}{
      record time in minutes.
    }
  }
}
\source{
  A.C. Atkinson (1986) Comment: Aspects of diagnostic regression analysis.
  \emph{Statistical Science} \bold{1}, 397--402.

[A.C. Atkinson (1988) Transformations unmasked. \emph{Technometrics}
\bold{30}, 311--318 \dQuote{corrects} the time for Knock Hill from 78.65
to 18.65. It is unclear if this based on the original records.]
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
