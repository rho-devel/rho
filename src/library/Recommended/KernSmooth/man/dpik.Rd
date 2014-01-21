\name{dpik}
\alias{dpik}
\title{
Select a Bandwidth for Kernel Density Estimation
}
\description{
Use direct plug-in methodology to select the bandwidth
of a kernel density estimate.
}
\usage{
dpik(x, scalest = "minim", level = 2L, kernel = "normal",   
     canonical = FALSE, gridsize = 401L, range.x = range(x), 
     truncate = TRUE)
}
\arguments{
\item{x}{
numeric vector containing the sample on which the
kernel density estimate is to be constructed.
}
\item{scalest}{
estimate of scale.

 \code{"stdev"} - standard deviation is used.

 \code{"iqr"} - inter-quartile range divided by 1.349 is used.

 \code{"minim"} - minimum of \code{"stdev"} and \code{"iqr"} is used.
}
\item{level}{
number of levels of functional estimation used in the
plug-in rule.
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
logical flag: if \code{TRUE}, canonically scaled kernels are used
}
\item{gridsize}{
the number of equally-spaced points over which binning is 
performed to obtain kernel functional approximation. 
}
\item{range.x}{
vector containing the minimum and maximum values of \code{x}
at which to compute the estimate.
The default is the minimum and maximum data values.
}
\item{truncate}{
logical flag: if \code{TRUE}, data with \code{x} values outside the
range specified by \code{range.x} are ignored.
}}
\value{
the selected bandwidth.
}
\details{
The direct plug-in approach, where unknown functionals
that appear in expressions for the asymptotically
optimal bandwidths
are replaced by kernel estimates, is used.
The normal distribution is used to provide an
initial estimate.
}
\section{Background}{
This method for selecting the bandwidth of a kernel
density estimate was proposed by Sheather and
Jones (1991)
and is
described in Section 3.6 of Wand and Jones (1995). 
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
\seealso{
\code{\link{bkde}}, \code{\link{density}}, \code{\link{ksmooth}}
}
\examples{
data(geyser, package="MASS")
x <- geyser$duration
h <- dpik(x)
est <- bkde(x, bandwidth=h)
plot(est,type="l")
}
\keyword{smooth}
% Converted by Sd2Rd version 0.2-a5.
