% File nlme/man/Gun.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Gun}
\alias{Gun}
\title{Methods for firing naval guns}
\description{
  The \code{Gun} data frame has 36 rows and 4 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{rounds}{
      a numeric vector
    }
    \item{Method}{
      a factor with levels
      \code{M1} 
      \code{M2} 
    }
    \item{Team}{
      an ordered factor with levels
      \code{T1S} < \code{T3S} < \code{T2S} < \code{T1A} <
      \code{T2A} < \code{T3A} < \code{T1H} < \code{T3H} <
      \code{T2H}
    }
    \item{Physique}{
      an ordered factor with levels
      \code{Slight} < \code{Average} < \code{Heavy}
    }
  }
}
\details{
  Hicks (p.180, 1993) reports data from an experiment on
  methods for firing naval guns. Gunners of three different physiques
  (slight, average, and heavy) tested two firing methods. Both methods
  were tested twice by each of nine teams of three gunners with
  identical physique. The response was the number of rounds fired per
  minute.}
\source{
  Hicks, C. R. (1993), \emph{Fundamental Concepts in the Design of
    Experiments (4th ed)}, Harcourt Brace, New York.
}
%\examples{}
\keyword{datasets}
