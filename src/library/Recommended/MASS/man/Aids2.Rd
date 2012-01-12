% file MASS/man/Aids2.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Aids2}
\alias{Aids2}
\title{
Australian AIDS Survival Data
}
\description{
Data on patients diagnosed with AIDS in Australia before 1 July 1991.
}
\usage{
Aids2
}
\format{
  This data frame contains 2843 rows and the following columns:
  \describe{
    \item{\code{state}}{
      Grouped state of origin: \code{"NSW "}includes ACT and
      \code{"other"} is WA, SA, NT and TAS.
    }
    \item{\code{sex}}{
      Sex of patient.
    }
    \item{\code{diag}}{(Julian) date of diagnosis.}
    \item{\code{death}}{
      (Julian) date of death or end of observation.
    }
    \item{\code{status}}{
      \code{"A"} (alive) or \code{"D"} (dead) at end of observation.
    }
    \item{\code{T.categ}}{
      Reported transmission category.
    }
    \item{\code{age}}{
      Age (years) at diagnosis.
    }
  }
}
\note{
  This data set has been slightly jittered as a
  condition of its release, to ensure patient confidentiality.
}
\source{
  Dr P. J. Solomon and the Australian National Centre in HIV Epidemiology
  and Clinical Research.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
