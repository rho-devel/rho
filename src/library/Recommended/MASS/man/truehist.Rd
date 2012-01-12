% file MASS/man/truehist.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{truehist}
\alias{truehist}
\title{
Plot a Histogram
}
\description{
Creates a histogram on the current graphics device.
}
\usage{
truehist(data, nbins = "Scott", h, x0 = -h/1000,
         breaks, prob = TRUE, xlim = range(breaks),
         ymax = max(est), col = "cyan",
         xlab = deparse(substitute(data)), bty = "n", \dots)
}
\arguments{
\item{data}{
numeric vector of data for histogram.  Missing values (\code{NA}s)
are allowed and omitted.
}
\item{nbins}{
The suggested number of bins.  Either a positive integer, or a character string
naming a rule: \code{"Scott"} or \code{"Freedman-Diaconis"} or \code{"FD"}.  (Case is
ignored.)
}
\item{h}{
The bin width, a strictly positive number (takes precedence over \code{nbins}).
}
\item{x0}{
Shift for the bins - the breaks are at \code{x0 + h * (\dots, -1, 0, 1, \dots)}
}
\item{breaks}{
The set of breakpoints to be used. (Usually omitted, takes precedence
over \code{h} and \code{nbins}).
}
\item{prob}{
If true (the default) plot a true histogram.
The vertical axis has a
\emph{relative frequency density}
scale, so the product of the dimensions of any panel gives the
relative frequency.  Hence the total area under the histogram
is 1 and it is directly comparable with most other estimates
of the probability density function.
If false plot the counts in the bins.
}
\item{xlim}{
The limits for the x-axis.
}
\item{ymax}{
The upper limit for the y-axis.
}
\item{col}{
The colour for the bar fill: the default is colour 5 in the default \R palette.
}
\item{xlab}{
label for the plot x-axis. By default, this will be the name of \code{data}.
}
\item{bty}{
The box type for the plot - defaults to none.
}
\item{\dots}{
  additional arguments to \code{\link{rect}} or \code{\link{plot}}.
}}
\section{Side Effects}{
A histogram is plotted on the current device.
}
\details{
  This plots a true histogram, a density estimate of total area 1.  If
  \code{breaks} is specified, those breakpoints are used. Otherwise if
  \code{h} is specified, a regular grid of bins is used with width
  \code{h}.  If neither \code{breaks} nor \code{h} is specified,
  \code{nbins} is used to select a suitable \code{h}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{hist}}
}
\keyword{hplot}
\keyword{dplot}
