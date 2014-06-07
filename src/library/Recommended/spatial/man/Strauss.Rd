% file spatial/man/Strauss.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Strauss}
\alias{Strauss}
\title{
Simulates Strauss Spatial Point Process
}
\description{
Simulates Strauss spatial point process.
}
\usage{
Strauss(n, c=0, r)
}
\arguments{
\item{n}{
number of points
}
\item{c}{
parameter \code{c} in \eqn{[0, 1]}. \code{c = 0} corresponds to complete inhibition at distances up to \code{r}.
}
\item{r}{
inhibition distance
}}
\value{
list of vectors of \eqn{x} and \eqn{y} coordinates
}
\section{Side Effects}{
uses the random number generator
}
\details{
Uses spatial birth-and-death process for 4\code{n} steps, or for 40\code{n} steps
starting from a binomial pattern on the first call from an other function.
Uses the region set by \code{ppinit} or \code{ppregion}.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{Psim}}, \code{\link{SSI}}
}
\examples{
towns <- ppinit("towns.dat")
par(pty="s")
plot(Kfn(towns, 10), type="b", xlab="distance", ylab="L(t)")
lines(Kaver(10, 25, Strauss(69,0.5,3.5)))
}
\keyword{spatial}
