% file spatial/man/prmat.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{prmat}
\alias{prmat}
\title{
Evaluate Kriging Surface over a Grid
}
\description{
Evaluate Kriging surface over a grid.
}
\usage{
prmat(obj, xl, xu, yl, yu, n)
}
\arguments{
\item{obj}{
object returned by \code{surf.gls}
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
\code{\link{surf.gls}}, \code{\link{trmat}}, \code{\link{semat}}
}
\examples{
data(topo, package="MASS")
topo.kr <- surf.gls(2, expcov, topo, d=0.7)
prsurf <- prmat(topo.kr, 0, 6.5, 0, 6.5, 50)
contour(prsurf, levels=seq(700, 925, 25))
}
\keyword{spatial}
