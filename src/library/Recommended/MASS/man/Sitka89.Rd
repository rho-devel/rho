% file MASS/man/Sitka89.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Sitka89}
\alias{Sitka89}
\title{
  Growth Curves for Sitka Spruce Trees in 1989
}
\description{
  The \code{Sitka89} data frame has 632 rows and 4 columns.  It gives repeated
  measurements on the log-size of 79 Sitka spruce trees, 54 of which
  were grown in ozone-enriched chambers and 25 were controls.  The size
  was measured eight times in 1989, at roughly monthly intervals.
}
\usage{
Sitka89
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{size}}{measured size (height times diameter squared) of
      tree, on log scale.}
    \item{\code{Time}}{time of measurement in  days since 1 January 1988.}
    \item{\code{tree}}{number of tree.}
    \item{\code{treat}}{either \code{"ozone"} for an ozone-enriched
      chamber or \code{"control"}.}
  }
}
\seealso{
\code{\link{Sitka}}
}
\source{
  P. J. Diggle, K.-Y. Liang and S. L. Zeger (1994)
  \emph{Analysis of Longitudinal Data.}
  Clarendon Press, Oxford
}
\keyword{datasets}
