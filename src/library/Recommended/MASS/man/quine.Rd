% file MASS/man/quine.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{quine}
\alias{quine}
\title{
  Absenteeism from School in Rural New South Wales
}
\description{
  The \code{quine} data frame has 146 rows and 5 columns.
  Children from Walgett, New South Wales, Australia, were classified by
  Culture, Age, Sex and Learner status and the number of days absent from
  school in a particular school year was recorded.
}
\usage{
quine
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Eth}}{
      ethnic background: Aboriginal or Not, (\code{"A"} or \code{"N"}).
    }
    \item{\code{Sex}}{
      sex: factor with levels (\code{"F"} or \code{"M"}).
    }
    \item{\code{Age}}{
      age group: Primary (\code{"F0"}), or forms \code{"F1,"}
      \code{"F2"} or \code{"F3"}.
    }
    \item{\code{Lrn}}{
      learner status: factor with levels Average or Slow learner, (\code{"AL"} or
      \code{"SL"}).
    }
    \item{\code{Days}}{
      days absent from school in the year.
    }
  }
}
\source{
  S. Quine, quoted in Aitkin, M. (1978) The analysis of unbalanced cross
  classifications (with discussion).
  \emph{Journal of the Royal Statistical Society series A} \bold{141}, 195--223.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
