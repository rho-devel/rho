% file class/man/somgrid.Rd
% copyright (C) 2002 W. N. Venables and B. D. Ripley
%
\name{somgrid}
\alias{somgrid}
\alias{plot.somgrid}
\alias{plot.SOM}
\title{
Plot SOM Fits
}
\description{
  Plotting functions for SOM results.
}
\usage{
somgrid(xdim = 8, ydim = 6, topo = c("rectangular", "hexagonal"))

\method{plot}{somgrid}(x, type = "p", ...)

\method{plot}{SOM}(x, ...)
}
\arguments{
  \item{xdim, ydim}{dimensions of the grid}
  \item{topo}{the topology of the grid.}
  \item{x}{an object inheriting from class \code{"somgrid"} or \code{"SOM"}.}
  \item{type, \dots}{graphical parameters.}
}
\value{
  For \code{somgrid}, an object of class \code{"somgrid"}, a list with
  components
  \item{pts}{a two-column matrix giving locations for the grid points.}
  \item{xdim, ydim, topo}{as in the arguments to \code{somgrid}.}
}
\details{
  The class \code{"somgrid"} records the coordinates of the grid to be
  used for (batch or on-line) SOM: this has a plot method.

  The plot method for class \code{"SOM"} plots a \code{\link{stars}}
  plot of the representative at each grid point.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  \code{\link{batchSOM}}, \code{\link{SOM}}
}
\keyword{classif}
