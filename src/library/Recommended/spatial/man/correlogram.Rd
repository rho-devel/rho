% file spatial/man/correlogram.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{correlogram}
\alias{correlogram}
\title{
Compute Spatial Correlograms
}
\description{
Compute spatial correlograms of spatial data or residuals.
}
\usage{
correlogram(krig, nint, plotit = TRUE,  \dots)
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
\code{x} and \code{y} coordinates of the correlogram, and \code{cnt}, the number of pairs
averaged per bin.
}
\section{Side Effects}{
Plots the correlogram if \code{plotit = TRUE}.
}
\details{
Divides range of data into \code{nint} bins, and computes the covariance for
pairs with separation in each bin, then divides by the variance.
Returns results for bins with 6 or more pairs.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{variogram}}
}
\examples{
data(topo, package="MASS")
topo.kr <- surf.ls(2, topo)
correlogram(topo.kr, 25)
d <- seq(0, 7, 0.1)
lines(d, expcov(d, 0.7))
}
\keyword{spatial}
