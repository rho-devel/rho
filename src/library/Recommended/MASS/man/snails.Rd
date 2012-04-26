% file MASS/man/snails.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{snails}
\alias{snails}
\title{
  Snail Mortality Data
}
\description{
  Groups of 20 snails were held for periods of 1, 2, 3 or 4 weeks
  in carefully controlled conditions of temperature and relative
  humidity.  There were two species of snail, A and B, and the
  experiment was designed as a 4 by 3 by 4 by 2 completely randomized
  design.  At the end of the exposure time the snails were tested to see if
  they had survived; the process itself is fatal for the animals.  The
  object of the exercise was to model the probability of survival in terms of
  the stimulus variables, and in particular to test for differences between
  species.

  The data are unusual in that in most cases fatalities during the experiment
  were fairly small.
}
\usage{
snails
}
\format{
  The data frame contains the following components:
  \describe{
    \item{\code{Species}}{
      snail species A (\code{1}) or B (\code{2}).
    }
    \item{\code{Exposure}}{
      exposure in weeks.
    }
    \item{\code{Rel.Hum}}{
      relative humidity (4 levels).
    }
    \item{\code{Temp}}{
      temperature, in degrees Celsius (3 levels).
    }
    \item{\code{Deaths}}{
      number of deaths.
    }
    \item{\code{N}}{
      number of snails exposed.
    }
  }
}
\source{
  Zoology Department, The University of Adelaide.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
