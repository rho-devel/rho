% file MASS/man/cabbages.Rd
% copyright (C) 1999 W. N. Venables and B. D. Ripley
%
\name{cabbages}
\alias{cabbages}
\title{
  Data from a cabbage field trial
}
\description{
  The \code{cabbages} data set has 60 observations and 4 variables
}
\usage{
cabbages
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Cult}}{
      Factor giving the cultivar of the cabbage, two levels: \code{c39}
      and \code{c52}.
    }
    \item{\code{Date}}{
      Factor specifying one of three planting dates: \code{d16},
      \code{d20} or \code{d21}.
    }
    \item{\code{HeadWt}}{
      Weight of the cabbage head, presumably in kg.
    }
    \item{\code{VitC}}{
      Ascorbic acid content, in undefined units.
    }
  }
}
\source{
  Rawlings, J. O. (1988)
  \emph{Applied Regression Analysis: A Research Tool.}
  Wadsworth and Brooks/Cole.  Example 8.4, page 219.
  (Rawlings cites the original source as the files of the late
  Dr Gertrude M Cox.)
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
