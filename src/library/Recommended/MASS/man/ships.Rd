% file MASS/man/ships.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{ships}
\alias{ships}
\title{
  Ships Damage Data
}
\description{
  Data frame giving the number of damage incidents and aggregate
  months of service by ship type, year of construction, and period of operation.
}
\usage{
ships
}
\format{
  \describe{
    \item{\code{type}}{
      type: \code{"A"} to \code{"E"}.
    }
    \item{\code{year}}{
      year of construction: 1960--64, 65--69, 70--74, 75--79
      (coded as \code{"60"}, \code{"65"}, \code{"70"}, \code{"75"}).
    }
    \item{\code{period}}{
      period of operation : 1960--74, 75--79.
    }
    \item{\code{service}}{
      aggregate months of service.
    }
    \item{\code{incidents}}{
      number of damage incidents.
    }
  }
}
\source{
  P. McCullagh and J. A. Nelder, (1983),
  \emph{Generalized Linear Models.} Chapman & Hall, section 6.3.2, page 137
}
\keyword{datasets}
