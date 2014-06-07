\name{hist.scott}
\alias{hist.scott}
\alias{hist.FD}
\title{
  Plot a Histogram with Automatic Bin Width Selection
}
\description{
  Plot a histogram with automatic bin width selection, using the Scott
  or Freedman--Diaconis formulae.
}
\usage{
hist.scott(x, prob = TRUE, xlab = deparse(substitute(x)), ...)
hist.FD(x, prob = TRUE, xlab = deparse(substitute(x)), ...)
}
\arguments{
  \item{x}{A data vector}
  \item{prob}{Should the plot have unit area, so be a density estimate?}
  \item{xlab, \dots}{Further arguments to \code{hist}.}
}
\value{
For the \code{nclass.*} functions, the suggested number of classes.
}
\section{Side Effects}{
  Plot a histogram.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  Springer.
}
\seealso{
  \code{\link{hist}}
}
\keyword{hplot}
\keyword{dplot}
