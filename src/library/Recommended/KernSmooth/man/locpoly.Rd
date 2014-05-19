\name{locpoly}
\alias{locpoly}
\title{
Estimate Functions Using Local Polynomials
}
\description{
Estimates a probability density function,  
regression function or their derivatives
using local polynomials. A fast binned implementation
over an equally-spaced grid is used.
}
\usage{
locpoly(x, y, drv = 0L, degree, kernel = "normal", 
        bandwidth, gridsize = 401L, bwdisc = 25, 
        range.x, binned = FALSE, truncate = TRUE)
}
\arguments{
\item{x}{
numeric vector of x data.
Missing values are not accepted.
}
\item{bandwidth}{
the kernel bandwidth smoothing parameter.
It may be a single number or an array having
length \code{gridsize}, representing a bandwidth
that varies according to the location of
estimation.
}
\item{y}{
vector of y data.
This must be same length as \code{x}, and
missing values are not accepted.
}
\item{drv}{
order of derivative to be estimated.
}
\item{degree}{
degree of local polynomial used. Its value
must be greater than or equal to the value
of \code{drv}. The default value is of \code{degree} is
\code{drv} + 1.
}
\item{kernel}{
\code{"normal"} - the Gaussian density function.  Currently ignored.
}
\item{gridsize}{
number of equally-spaced grid points over which the 
function is to be estimated.
}
\item{bwdisc}{
number of logarithmically-equally-spaced bandwidths
on which \code{bandwidth} is discretised, to speed up
computation.
}
\item{range.x}{
vector containing the minimum and maximum values of \code{x} at which to
compute the estimate.
}
\item{binned}{
logical flag: if \code{TRUE}, then \code{x} and \code{y} are taken to be grid counts
rather than raw data. 
}
\item{truncate}{
logical flag: if \code{TRUE}, data with \code{x} values outside the range specified
by \code{range.x} are ignored.
}}
\value{
if \code{y} is specified, a local polynomial regression estimate of 
E[Y|X] (or its derivative) is computed.
If \code{y} is missing, a local polynomial estimate of the density
of \code{x} (or its derivative) is computed.


a list containing the following components:

\item{x}{
vector of sorted x values at which the estimate was computed.
}
\item{y}{
vector of smoothed estimates for either the density or the regression
at the corresponding \code{x}.
}}
\section{Details}{
Local polynomial fitting with a kernel weight is used to
estimate either a density, regression function or their
derivatives. In the case of density estimation, the 
data are binned and the local fitting procedure is applied to 
the bin counts. In either case, binned approximations over
an equally-spaced grid is used for fast computation. The
bandwidth may be either scalar or a vector of length
\code{gridsize}.
}
\references{
Wand, M. P. and Jones, M. C. (1995).
\emph{Kernel Smoothing.}
Chapman and Hall, London.
}
\seealso{
  \code{\link{bkde}}, \code{\link{density}}, \code{\link{dpill}},
  \code{\link{ksmooth}}, \code{\link{loess}}, \code{\link{smooth}},
  \code{\link{supsmu}}.
}
\examples{
data(geyser, package = "MASS")
# local linear density estimate
x <- geyser$duration
est <- locpoly(x, bandwidth = 0.25)
plot(est, type = "l")

# local linear regression estimate
y <- geyser$waiting
plot(x, y)
fit <- locpoly(x, y, bandwidth = 0.25)
lines(fit)
}
\keyword{smooth}
\keyword{regression}
% Converted by Sd2Rd version 0.2-a5.
