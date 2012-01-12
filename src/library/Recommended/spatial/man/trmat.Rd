% file spatial/man/trmat.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{trmat}
\alias{trmat}
\title{
Evaluate Trend Surface over a Grid
}
\description{
Evaluate trend surface over a grid.
}
\usage{
trmat(obj, xl, xu, yl, yu, n)
}
\arguments{
\item{obj}{
object returned by \code{surf.ls} or \code{surf.gls}
}
\item{xl}{
limits of the rectangle for grid
}
\item{xu}{
}
\item{yl}{
}
\item{yu}{
}
\item{n}{
use \code{n}  x  \code{n} grid within the rectangle
}}
\value{
list with components \code{x}, \code{y} and \code{z} suitable for \code{contour} and \code{image}.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{surf.ls}}, \code{\link{surf.gls}}
}
\examples{
data(topo, package="MASS")
topo.kr <- surf.ls(2, topo)
trsurf <- trmat(topo.kr, 0, 6.5, 0, 6.5, 50)
}
\keyword{spatial}
