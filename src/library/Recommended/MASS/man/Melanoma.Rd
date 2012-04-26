% file MASS/man/Melanoma.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Melanoma}
\alias{Melanoma}
\title{
  Survival from Malignant Melanoma
}
\description{
  The \code{Melanoma} data frame has data on 205 patients in Denmark
  with malignant melanoma.
}
\usage{
Melanoma
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{time}}{
      survival time in days, possibly censored.
    }
    \item{\code{status}}{
      \code{1} died from melanoma, \code{2} alive, \code{3} dead from
      other causes.
    }
    \item{\code{sex}}{
      \code{1} = male, \code{0} = female.
    }
    \item{\code{age}}{
      age in years.
    }
    \item{\code{year}}{
      of operation.
    }
    \item{\code{thickness}}{
      tumour thickness in mm.
    }
    \item{\code{ulcer}}{
      \code{1} = presence, \code{0} = absence.
    }
  }
}
\source{
  P. K. Andersen, O. Borgan, R. D. Gill and N. Keiding (1993)
  \emph{Statistical Models based on Counting Processes.}
  Springer.
}
\keyword{datasets}
