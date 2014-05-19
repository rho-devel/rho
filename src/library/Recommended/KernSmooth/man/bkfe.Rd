\name{bkfe}
\alias{bkfe}
\title{
Compute a Binned Kernel Functional Estimate 
}
\description{
Returns an estimate of a binned approximation to
the kernel estimate of the specified density functional. 
The kernel is the standard normal density.
}
\usage{
bkfe(x, drv, bandwidth, gridsize = 401L, range.x, binned = FALSE,
     truncate = TRUE)
}
\arguments{
\item{x}{
numeric vector of observations from the distribution whose density is to
be estimated.
Missing values are not allowed.
}
\item{drv}{
order of derivative in the density functional. Must be a
non-negative even integer.
}
\item{bandwidth}{
the kernel bandwidth smoothing parameter.  Must be supplied.
}
\item{gridsize}{
the number of equally-spaced points over which binning is
performed.
}
\item{range.x}{
vector containing the minimum and maximum values of \code{x}
at which to compute the estimate.
The default is the minimum and maximum data values, extended by the
support of the kernel.
}
\item{binned}{
logical flag: if \code{TRUE}, then \code{x} and \code{y} are taken to be grid counts
rather than raw data.
}
\item{truncate}{
logical flag: if \code{TRUE}, data with \code{x} values outside the
range specified by \code{range.x} are ignored.
}}
\value{
the (scalar) estimated functional.
}
\details{
The density functional of order \code{drv} is the integral of the
product of the density and its \code{drv}th derivative. 
The kernel estimates
of such quantities are computed using a binned implementation,
and the kernel is the standard normal density.
}
\section{Background}{
Estimates of this type were proposed by Sheather and
Jones (1991).
}
\references{
Sheather, S. J. and Jones, M. C. (1991).
A reliable data-based bandwidth selection method for
kernel density estimation.
\emph{Journal of the Royal Statistical Society, Series B},
\bold{53}, 683--690.

Wand, M. P. and Jones, M. C. (1995).
\emph{Kernel Smoothing.}
Chapman and Hall, London.
}
\examples{
data(geyser, package="MASS")
x <- geyser$duration
est <- bkfe(x, drv=4, bandwidth=0.3)
}
\keyword{smooth}
% Converted by Sd2Rd version 0.2-a5.
