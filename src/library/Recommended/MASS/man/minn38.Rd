% file MASS/man/minn38.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{minn38}
\alias{minn38}
\title{
  Minnesota High School Graduates of 1938
}
\description{
  The Minnesota high school graduates of 1938 were classified according to
  four factors, described below.  The \code{minn38} data frame has 168
  rows and 5 columns.
}
\usage{
minn38
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{hs}}{
      high school rank: \code{"L"}, \code{"M"} and \code{"U"} for lower,
      middle and upper third.
    }
    \item{\code{phs}}{
      post high school status: Enrolled in college, (\code{"C"}), enrolled in
      non-collegiate school, (\code{"N"}), employed full-time, (\code{"E"})
      and other, (\code{"O"}).
    }
    \item{\code{fol}}{
      father's occupational level, (seven levels, \code{"F1"}, \code{"F2"},
      \dots, \code{"F7"}).
    }
    \item{\code{sex}}{
      sex: factor with levels\code{"F"} or \code{"M"}.
    }
    \item{\code{f}}{
      frequency.
    }
  }
}
\source{
  From R. L. Plackett, (1974) \emph{The Analysis of Categorical
    Data.} London: Griffin

  who quotes the data from

  Hoyt, C. J., Krishnaiah, P. R. and Torrance, E. P. (1959) Analysis of
  complex contingency tables, \emph{J. Exp. Ed.} \bold{27}, 187--194.
}
\keyword{datasets}
