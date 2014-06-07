% file spatial/man/Kfn.Rd
% copyright (C) 1994-2009 W. N. Venables and B. D. Ripley
%
\name{Kfn}
\alias{Kfn}
\title{
Compute K-fn of a Point Pattern
}
\description{
Actually computes \eqn{L = \sqrt{K/\pi}}{L = sqrt(K/pi)}.
}
\usage{
Kfn(pp, fs, k=100)
}
\arguments{
\item{pp}{
a list such as a pp object, including components \code{x} and \code{y}
}
\item{fs}{
full scale of the plot
}
\item{k}{
number of regularly spaced distances in (0, \code{fs})
}}
\value{
A list with components

\item{x}{
vector of distances
}
\item{y}{
vector of L-fn values
}
\item{k}{
number of distances returned -- may be less than \code{k} if \code{fs} is too large
}
\item{dmin}{
minimum distance between pair of points
}
\item{lm}{
maximum deviation from L(t) = t
}}
\details{
relies on the domain D having been set by \code{ppinit} or \code{ppregion}.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{ppinit}}, \code{\link{ppregion}}, \code{\link{Kaver}}, \code{\link{Kenvl}}
}
\examples{
towns <- ppinit("towns.dat")
par(pty="s")
plot(Kfn(towns, 10), type="s", xlab="distance", ylab="L(t)")
}
\keyword{spatial}
