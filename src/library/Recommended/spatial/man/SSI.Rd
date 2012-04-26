% file spatial/man/SSI.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{SSI}
\alias{SSI}
\title{
Simulates Sequential Spatial Inhibition Point Process
}
\description{
Simulates SSI (sequential spatial inhibition) point process.
}
\usage{
SSI(n, r)
}
\arguments{
\item{n}{
number of points
}
\item{r}{
inhibition distance
}}
\value{
list of vectors of \code{x} and \code{y} coordinates
}
\details{
uses the region set by \code{ppinit} or \code{ppregion}.
}
\section{Side Effects}{
uses the random number generator.
}
\section{Warnings}{
  will never return if \code{r} is too large and it cannot place
  \code{n} points.
}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{Psim}}, \code{\link{Strauss}}
}
\examples{
towns <- ppinit("towns.dat")
par(pty = "s")
plot(Kfn(towns, 10), type = "b", xlab = "distance", ylab = "L(t)")
lines(Kaver(10, 25, SSI(69, 1.2)))
}
\keyword{spatial}
