% file MASS/man/synth.tr.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{synth.tr}
\alias{synth.tr}
\alias{synth.te}
\title{
Synthetic Classification Problem
}
\description{
The \code{synth.tr} data frame has 250 rows and 3 columns.
The \code{synth.te} data frame has 100 rows and 3 columns.
It is intended that \code{synth.tr} be used from training and
\code{synth.te} for testing.
}
\usage{
synth.tr
synth.te
}
\format{
These data frames contains the following columns:
\describe{
\item{\code{xs}}{
x-coordinate
}
\item{\code{ys}}{
y-coordinate
}
\item{\code{yc}}{
class, coded as 0 or 1.
}}}
\source{
Ripley, B.D. (1994)
Neural networks and related methods for
classification (with discussion).
\emph{Journal of the Royal Statistical Society series B}
\bold{56}, 409--456.


Ripley, B.D. (1996)
\emph{Pattern Recognition and Neural Networks.}
Cambridge: Cambridge University Press.
}
\keyword{datasets}
