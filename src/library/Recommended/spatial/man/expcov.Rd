% file spatial/man/expcov.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{expcov}
\alias{expcov}
\alias{gaucov}
\alias{sphercov}
\title{
Spatial Covariance Functions
}
\description{
Spatial covariance functions for use with \code{surf.gls}.
}
\usage{
expcov(r, d, alpha = 0, se = 1)
gaucov(r, d, alpha = 0, se = 1)
sphercov(r, d, alpha = 0, se = 1, D = 2)
}
\arguments{
\item{r}{
vector of distances at which to evaluate the covariance
}
\item{d}{
range parameter
}
\item{alpha}{
proportion of nugget effect
}
\item{se}{
standard deviation at distance zero
}
\item{D}{
dimension of spheres.
}}
\value{
vector of covariance values.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{surf.gls}}
}
\examples{
data(topo, package="MASS")
topo.kr <- surf.ls(2, topo)
correlogram(topo.kr, 25)
d <- seq(0, 7, 0.1)
lines(d, expcov(d, 0.7))
}
\keyword{spatial}
