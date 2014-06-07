% file spatial/man/semat.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{semat}
\alias{semat}
\title{
Evaluate Kriging Standard Error of Prediction over a Grid
}
\description{
Evaluate Kriging standard error of prediction over a grid.
}
\usage{
semat(obj, xl, xu, yl, yu, n, se)
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
}
\item{se}{
standard error at distance zero as a multiple of the supplied
covariance. Otherwise estimated, and it assumed that a correlation
function was supplied.
}}
\value{
list with components x, y and z suitable for \code{contour} and \code{image}.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{surf.gls}}, \code{\link{trmat}}, \code{\link{prmat}}
}
\examples{
data(topo, package="MASS")
topo.kr <- surf.gls(2, expcov, topo, d=0.7)
prsurf <- prmat(topo.kr, 0, 6.5, 0, 6.5, 50)
contour(prsurf, levels=seq(700, 925, 25))
sesurf <- semat(topo.kr, 0, 6.5, 0, 6.5, 30)
contour(sesurf, levels=c(22,25))
}
\keyword{spatial}
