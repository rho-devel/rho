\name{dpill}
\alias{dpill}
\title{
Select a Bandwidth for Local Linear Regression
}
\description{
Use direct plug-in methodology to select the bandwidth
of a local linear Gaussian kernel regression estimate, as described
by Ruppert, Sheather and Wand (1995). 
}
\usage{
dpill(x, y, blockmax = 5, divisor = 20, trim = 0.01, proptrun = 0.05, 
      gridsize = 401L, range.x, truncate = TRUE)
}
\arguments{
\item{x}{
numeric vector of x data.
Missing values are not accepted.
}
\item{y}{
numeric vector of y data.
This must be same length as \code{x}, and
missing values are not accepted.
}
\item{blockmax}{
the maximum number of blocks of the data for construction
of an initial parametric estimate. 
}
\item{divisor}{
the value that the sample size is divided by to determine
a lower limit on the number of blocks of the data for
construction of an initial parametric estimate.
}
\item{trim}{
the proportion of the sample trimmed from each end in the
\code{x} direction before application of the plug-in methodology.
}
\item{proptrun}{
the proportion of the range of \code{x} at each end truncated in the
functional estimates.
}
\item{gridsize}{
number of equally-spaced grid points over which the
function is to be estimated.
}
\item{range.x}{
vector containing the minimum and maximum values of \code{x} at which to
compute the estimate.
For density estimation the default is the minimum and maximum data values
with 5\% of the range added to each end.
For regression estimation the default is the minimum and maximum data values.
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
The kernel is the standard normal density.
Least squares quartic fits over blocks of data are used to 
obtain an initial estimate. Mallow's \eqn{C_p}{Cp} is used to select
the number of blocks.
}
\section{Warning}{
If there are severe irregularities (i.e. outliers, sparse regions)
in the \code{x} values then the local polynomial smooths required for the
bandwidth selection algorithm may become degenerate and the function
will crash. Outliers in the \code{y} direction may lead to deterioration
of the quality of the selected bandwidth.
}
\references{
Ruppert, D., Sheather, S. J. and Wand, M. P. (1995).
An effective bandwidth selector for local least squares
regression.
\emph{Journal of the American Statistical Association},
\bold{90}, 1257--1270.

Wand, M. P. and Jones, M. C. (1995).
\emph{Kernel Smoothing.}
Chapman and Hall, London.
}
\seealso{
\code{\link{ksmooth}}, \code{\link{locpoly}}.
}
\examples{
data(geyser, package = "MASS")
x <- geyser$duration
y <- geyser$waiting
plot(x, y)
h <- dpill(x, y)
fit <- locpoly(x, y, bandwidth = h)
lines(fit)
}
\keyword{smooth}
% Converted by Sd2Rd version 0.2-a5.
