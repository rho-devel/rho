% file MASS/man/cpus.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{cpus}
\alias{cpus}
\title{
  Performance of Computer CPUs
}
\description{
  A relative performance measure and characteristics of 209 CPUs.
}
\usage{
cpus
}
\format{
  The components are:
  \describe{
    \item{\code{name}}{
      manufacturer and model.
    }
    \item{\code{syct}}{
      cycle time in nanoseconds.
    }
    \item{\code{mmin}}{
      minimum main memory in kilobytes.
    }
    \item{\code{mmax}}{
      maximum main memory in kilobytes.
    }
    \item{\code{cach}}{
      cache size in kilobytes.
    }
    \item{\code{chmin}}{
      minimum number of channels.
    }
    \item{\code{chmax}}{
      maximum number of channels.
    }
    \item{\code{perf}}{
      published performance on a benchmark mix relative to an IBM 370/158-3.
    }
    \item{\code{estperf}}{
      estimated performance (by Ein-Dor & Feldmesser).
    }
  }
}
\source{
  P. Ein-Dor and J. Feldmesser (1987)
  Attributes of the performance of central processing units: a relative
  performance prediction model.
  \emph{Comm. ACM.} \bold{30}, 308--317.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
