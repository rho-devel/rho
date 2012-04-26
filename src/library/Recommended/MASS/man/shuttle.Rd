% file MASS/man/shuttle.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{shuttle}
\alias{shuttle}
\title{
  Space Shuttle Autolander Problem
}
\description{
  The \code{shuttle} data frame has 256 rows and 7 columns.
  The first six columns are categorical variables giving example
  conditions; the seventh is the decision.  The first 253 rows are the
  training set, the last 3 the test conditions.
}
\usage{
shuttle
}
\format{
  This data frame contains the following factor columns:
  \describe{
    \item{\code{stability}}{
      stable positioning or not (\code{stab} / \code{xstab}).
    }
    \item{\code{error}}{
      size of error (\code{MM} / \code{SS} / \code{LX} / \code{XL}).
    }
    \item{\code{sign}}{
      sign of error, positive or negative (\code{pp} / \code{nn}).
    }
    \item{\code{wind}}{
      wind sign (\code{head} / \code{tail}).
    }
    \item{\code{magn}}{
      wind strength (\code{Light} / \code{Medium} / \code{Strong} /
      \code{Out of Range}).
    }
    \item{\code{vis}}{
      visibility (\code{yes} / \code{no}).
    }
    \item{\code{use}}{
      use the autolander or not. (\code{auto} / \code{noauto}.)
    }
  }
}
\source{
  D. Michie (1989)
  Problems of computer-aided concept formation. In
  \emph{Applications of Expert Systems 2},
  ed. J. R. Quinlan, Turing Institute Press / Addison-Wesley, pp. 310--333.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
