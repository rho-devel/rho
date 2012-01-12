% file MASS/man/fgl.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{fgl}
\alias{fgl}
\title{
Measurements of Forensic Glass Fragments
}
\description{
The \code{fgl} data frame has 214 rows and 10 columns.
It was collected by B. German on fragments of glass
collected in forensic work.
}
\usage{
fgl
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{RI}}{
      refractive index; more precisely the refractive index is 1.518xxxx.

      The next 8 measurements are percentages by weight of oxides.
    }
    \item{\code{Na}}{sodium.}
    \item{\code{Mg}}{manganese.}
    \item{\code{Al}}{aluminium.}
    \item{\code{Si}}{silicon.}
    \item{\code{K}}{potassium.}
    \item{\code{Ca}}{calcium.}
    \item{\code{Ba}}{barium.}
    \item{\code{Fe}}{iron.}
    \item{\code{type}}{
      The fragments were originally classed into seven types, one of which
      was absent in this dataset.  The categories which occur are
      window float glass (\code{WinF}: 70),
      window non-float glass (\code{WinNF}: 76),
      vehicle window glass (\code{Veh}: 17),
      containers (\code{Con}: 13),
      tableware (\code{Tabl}: 9) and
      vehicle headlamps (\code{Head}: 29).
    }
  }
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
