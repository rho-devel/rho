% file MASS/man/ldahist.Rd
% copyright (C) 1998-9 W. N. Venables and B. D. Ripley
%
\name{ldahist}
\alias{ldahist}
\title{
Histograms or Density Plots of Multiple Groups
}
\description{
Plot histograms or density plots of data on a single Fisher linear
discriminant.
}
\usage{
ldahist(data, g, nbins = 25, h, x0 = - h/1000, breaks,
        xlim = range(breaks), ymax = 0, width,
        type = c("histogram", "density", "both"),
        sep = (type != "density"),
        col = 5, xlab = deparse(substitute(data)), bty = "n", \dots)
}
\arguments{
\item{data}{
vector of data. Missing values (\code{NA}s) are allowed and omitted.
}
\item{g}{
factor or vector giving groups, of the same length as \code{data}.
}
\item{nbins}{
Suggested number of bins to cover the whole range of the data.
}
\item{h}{
The bin width (takes precedence over \code{nbins}).
}
\item{x0}{
Shift for the bins - the breaks are at \code{x0 + h * (\dots, -1, 0, 1, \dots)}
}
\item{breaks}{
The set of breakpoints to be used. (Usually omitted, takes precedence
over \code{h} and \code{nbins}).
}
\item{xlim}{
The limits for the x-axis.
}
\item{ymax}{
The upper limit for the y-axis.
}
\item{width}{
Bandwidth for density estimates. If missing, the Sheather-Jones
selector is used for each group separately.
}
\item{type}{
Type of plot.
}
\item{sep}{
Whether there is a separate plot for each group, or one combined plot.
}
\item{col}{
The colour number for the bar fill.
}
\item{xlab}{
label for the plot x-axis. By default, this will be the name of \code{data}.
}
\item{bty}{
The box type for the plot - defaults to none.
}
\item{\dots}{
additional arguments to \code{polygon}.
}}
\section{Side Effects}{
Histogram and/or density plots are plotted on the current device.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{plot.lda}}.
}
\keyword{dplot}
\keyword{hplot}
