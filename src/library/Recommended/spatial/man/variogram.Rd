% file spatial/man/variogram.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{variogram}
\alias{variogram}
\title{
Compute Spatial Variogram
}
\description{
Compute spatial (semi-)variogram of spatial data or residuals.
}
\usage{
variogram(krig, nint, plotit = TRUE, \dots)
}
\arguments{
\item{krig}{
trend-surface or kriging object with columns \code{x}, \code{y}, and \code{z}
}
\item{nint}{
number of bins used
}
\item{plotit}{
logical for plotting
}
\item{\dots}{
parameters for the plot
}}
\value{
  \code{x} and \code{y} coordinates of the variogram and \code{cnt},
  the number of pairs averaged per bin.
}
\section{Side Effects}{
Plots the variogram if \code{plotit = TRUE}
}
\details{
Divides range of data into \code{nint} bins, and computes the average squared
difference for pairs with separation in each bin.  Returns results for
bins with 6 or more pairs.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{correlogram}}
}
\examples{
data(topo, package="MASS")
topo.kr <- surf.ls(2, topo)
variogram(topo.kr, 25)
}
\keyword{spatial}
