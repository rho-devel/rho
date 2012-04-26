% file MASS/man/anorexia.Rd
% copyright (C) 1999 W. N. Venables and B. D. Ripley
%
\name{anorexia}
\alias{anorexia}
\title{
  Anorexia Data on Weight Change
}
\description{
  The \code{anorexia} data frame has 72 rows and 3 columns.
  Weight change data for young female anorexia patients.
}
\usage{
anorexia
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Treat}}{
      Factor of three levels: \code{"Cont"} (control), \code{"CBT"}
      (Cognitive Behavioural treatment) and  \code{"FT"} (family
      treatment).
    }
    \item{\code{Prewt}}{
      Weight of patient before study period, in lbs.
    }
    \item{\code{Postwt}}{
      Weight of patient after study period, in lbs.
    }
  }
}
\source{
  Hand, D. J., Daly, F., McConway, K., Lunn, D. and Ostrowski, E. eds (1993)
  \emph{A Handbook of Small Data Sets.}
  Chapman & Hall, Data set 285 (p. 229)

(Note that the original source mistakenly says that weights are in kg.)
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
