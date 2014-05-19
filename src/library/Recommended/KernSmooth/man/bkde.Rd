\name{bkde}
\alias{bkde}
\title{
Compute a Binned Kernel Density Estimate
}
\description{
Returns x and y coordinates of the binned
kernel density estimate of the probability
density of the data.  
}
\usage{
bkde(x, kernel = "normal", canonical = FALSE, bandwidth,
     gridsize = 401L, range.x, truncate = TRUE)
}
\arguments{
\item{x}{
  numeric vector of observations from the distribution whose density is to
  be estimated.  Missing values are not allowed.
}
\item{bandwidth}{
  the kernel bandwidth smoothing parameter.  Larger values of
  \code{bandwidth} make smoother estimates, smaller values of
  \code{bandwidth} make less smooth estimates.  The default is a bandwidth
  computed from the variance of \code{x}, specifically the
  \sQuote{oversmoothed bandwidth selector} of Wand and Jones
  (1995, page 61).
}
\item{kernel}{
character string which determines the smoothing kernel.
\code{kernel} can be:
\code{"normal"} - the Gaussian density function (the default).
\code{"box"} - a rectangular box.
\code{"epanech"} - the centred beta(2,2) density.
\code{"biweight"} - the centred beta(3,3) density.
\code{"triweight"} - the centred beta(4,4) density.
This can be abbreviated to any unique abbreviation.
}
\item{canonical}{
logical flag: if \code{TRUE}, canonically scaled kernels are used.
}
\item{gridsize}{
the number of equally spaced points at which to estimate
the density.
}
\item{range.x}{
vector containing the minimum and maximum values of \code{x}
at which to compute the estimate.
The default is the minimum and maximum data values, extended by the
support of the kernel.
}
\item{truncate}{
logical flag: if \code{TRUE}, data with \code{x} values outside the
range specified by \code{range.x} are ignored.
}}
\value{
a list containing the following components:

\item{x}{
vector of sorted \code{x} values at which the estimate was computed.
}
\item{y}{
vector of density estimates
at the corresponding \code{x}.
}}
\details{
This is the binned approximation to the ordinary kernel density estimate.
Linear binning is used to obtain the bin counts.  
For each \code{x} value in the sample, the kernel is
centered on that \code{x} and the heights of the kernel at each datapoint are summed.
This sum, after a normalization, is the corresponding \code{y} value in the output.
}
\section{Background}{
Density estimation is a smoothing operation.
Inevitably there is a trade-off between bias in the estimate and the
estimate's variability: large bandwidths will produce smooth estimates that
may hide local features of the density; small bandwidths may introduce
spurious bumps into the estimate.
}
\references{
Wand, M. P. and Jones, M. C. (1995).
\emph{Kernel Smoothing.}
Chapman and Hall, London.
}
\seealso{
  \code{\link{density}}, \code{\link{dpik}}, \code{\link{hist}},
  \code{\link{ksmooth}}.
}
\examples{
data(geyser, package="MASS")
x <- geyser$duration
est <- bkde(x, bandwidth=0.25)
plot(est, type="l")
}
\keyword{distribution}
\keyword{smooth}
% Converted by Sd2Rd version 0.2-a5.
