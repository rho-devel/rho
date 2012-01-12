% file MASS/man/crabs.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{crabs}
\alias{crabs}
\title{
  Morphological Measurements on Leptograpsus Crabs
}
\description{
  The \code{crabs} data frame has 200 rows and 8 columns, describing 5
  morphological measurements on 50 crabs each of two colour forms and
  both sexes, of the species \emph{Leptograpsus variegatus} collected at
  Fremantle, W. Australia.
}
\usage{
crabs
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{sp}}{
      \code{species} - \code{"B"} or \code{"O"} for blue or orange.
    }
    \item{\code{sex}}{
      as it says.
    }
    \item{\code{index}}{
      index \code{1:50} within each of the four groups.
    }
    \item{\code{FL}}{
      frontal lobe size (mm).
    }
    \item{\code{RW}}{
      rear width (mm).
    }
    \item{\code{CL}}{
      carapace length (mm).
    }
    \item{\code{CW}}{
      carapace width (mm).
    }
    \item{\code{BD}}{
      body depth (mm).
    }
  }
}
\source{
  Campbell, N.A. and Mahon, R.J. (1974) A multivariate
  study of variation in two species of rock crab of genus
  \emph{Leptograpsus.}
  \emph{Australian Journal of  Zoology} \bold{22}, 417--425.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
