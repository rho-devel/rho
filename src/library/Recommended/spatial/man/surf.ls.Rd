% file spatial/man/surf.ls.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{surf.ls}
\alias{surf.ls}
\title{
Fits a Trend Surface by Least-squares
}
\description{
Fits a trend surface by least-squares.
}
\usage{
surf.ls(np, x, y, z)
}
\arguments{
\item{np}{
degree of polynomial surface
}
\item{x}{
x coordinates or a data frame with columns \code{x}, \code{y}, \code{z}
}
\item{y}{
y coordinates
}
\item{z}{
z coordinates. Will supersede \code{x$z}
}}
\value{
list with components

\item{beta}{
the coefficients
}
\item{x}{
}
\item{y}{
}
\item{z}{
and others for internal use only.
}}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{trmat}}, \code{\link{surf.gls}}
}
\examples{
library(MASS)  # for eqscplot
data(topo, package="MASS")
topo.kr <- surf.ls(2, topo)
trsurf <- trmat(topo.kr, 0, 6.5, 0, 6.5, 50)
eqscplot(trsurf, type = "n")
contour(trsurf, add = TRUE)
points(topo)

eqscplot(trsurf, type = "n")
contour(trsurf, add = TRUE)
plot(topo.kr, add = TRUE)
title(xlab= "Circle radius proportional to Cook's influence statistic")
}
\keyword{spatial}
