% file MASS/man/hubers.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{hubers}
\alias{hubers}
\title{
Huber Proposal 2 Robust Estimator of Location and/or Scale
}
\description{
Finds the Huber M-estimator for location with scale specified, scale
with location specified, or both if neither is specified.
}
\usage{
hubers(y, k = 1.5, mu, s, initmu = median(y), tol = 1e-06)
}
\arguments{
\item{y}{
vector y of data values
}
\item{k}{
Winsorizes at \code{k} standard deviations
}
\item{mu}{
specified location
}
\item{s}{
specified scale
}
\item{initmu}{
initial value of \code{mu}
}
\item{tol}{
convergence tolerance
}}
\value{
list of location and scale estimates

\item{mu}{
location estimate
}
\item{s}{
scale estimate
}}
\references{
Huber, P. J. (1981)
\emph{Robust Statistics.}
Wiley.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{huber}}
}
\examples{
hubers(chem)
hubers(chem, mu=3.68)
}
\keyword{robust}
