% file MASS/man/width.SJ.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{width.SJ}
\alias{width.SJ}
\title{
Bandwidth Selection by Pilot Estimation of Derivatives
}
\description{
Uses the method of Sheather & Jones (1991) to select the bandwidth of
a Gaussian kernel density estimator.
}
\usage{
width.SJ(x, nb = 1000, lower, upper, method = c("ste", "dpi"))
}
\arguments{
\item{x}{
a numeric vector
}
\item{nb}{
number of bins to use.
}
\item{upper, lower}{
range over which to search for solution if \code{method = "ste"}.
}
\item{method}{
  Either \code{"ste"} ("solve-the-equation") or \code{"dpi"}
  ("direct plug-in").
}}
\value{
a bandwidth.
}
\references{
Sheather, S. J. and Jones, M. C. (1991) A reliable data-based bandwidth
selection method for kernel density estimation.
\emph{Journal of the Royal Statistical Society series B}
\bold{53}, 683--690.

Scott, D. W. (1992)
\emph{Multivariate Density Estimation: Theory, Practice, and  Visualization.}
Wiley.

Wand, M. P. and Jones, M. C. (1995)
\emph{Kernel Smoothing.}
Chapman & Hall.
}
\seealso{
\code{\link{ucv}}, \code{\link{bcv}}, \code{\link{density}}
}
\examples{
width.SJ(geyser$duration, method = "dpi")
width.SJ(geyser$duration)

width.SJ(galaxies, method = "dpi")
width.SJ(galaxies)
}
\keyword{dplot}
