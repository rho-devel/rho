% file spatial/man/Kenvl.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Kenvl}
\alias{Kenvl}
\title{
Compute Envelope and Average of Simulations of K-fns
}
\description{
Computes envelope (upper and lower limits) and average of simulations of K-fns
}
\usage{
Kenvl(fs, nsim, \dots)
}
\arguments{
\item{fs}{
full scale for K-fn
}
\item{nsim}{
number of simulations
}
\item{\dots}{
arguments to produce one simulation
}}
\value{
list with components

\item{x}{
distances
}
\item{lower}{
min of K-fns
}
\item{upper}{
max of K-fns
}
\item{aver}{
average of K-fns
}}
\references{
  Ripley, B. D. (1981) \emph{Spatial Statistics.} Wiley.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{Kfn}}, \code{\link{Kaver}}
}
\examples{
towns <- ppinit("towns.dat")
par(pty="s")
plot(Kfn(towns, 40), type="b")
plot(Kfn(towns, 10), type="b", xlab="distance", ylab="L(t)")
for(i in 1:10) lines(Kfn(Psim(69), 10))
lims <- Kenvl(10,100,Psim(69))
lines(lims$x,lims$lower, lty=2, col="green")
lines(lims$x,lims$upper, lty=2, col="green")
lines(Kaver(10,25,Strauss(69,0.5,3.5)), col="red")
}
\keyword{spatial}
