% file MASS/man/coop.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{coop}
\alias{coop}
\title{
  Co-operative Trial in Analytical Chemistry
}
\description{
  Seven specimens were sent to 6 laboratories in 3 separate batches and
  each analysed for Analyte.  Each analysis was duplicated.
}
\usage{
coop
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Lab}}{
      Laboratory, \code{L1}, \code{L2}, \dots, \code{L6}.
    }
    \item{\code{Spc}}{
      Specimen, \code{S1}, \code{S2}, \dots, \code{S7}.
    }
    \item{\code{Bat}}{
      Batch, \code{B1}, \code{B2}, \code{B3} (nested within \code{Spc/Lab}),
    }
    \item{\code{Conc}}{
      Concentration of Analyte in \eqn{g/kg}.
    }
  }
}
\source{
  Analytical Methods Committee (1987)
  Recommendations for the conduct and
  interpretation of co-operative trials,
  \emph{The Analyst} \bold{112}, 679--686.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{chem}}, \code{\link{abbey}}.
}
\keyword{datasets}
