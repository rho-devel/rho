\name{dpih}
\alias{dpih}
\title{
Select a Histogram Bin Width 
}
\description{
Uses direct plug-in methodology to select the bin width of 
a histogram.
}
\usage{
dpih(x, scalest = "minim", level = 2L, gridsize = 401L, 
     range.x = range(x), truncate = TRUE)
}
\arguments{
\item{x}{
numeric vector containing the sample on which the
histogram is to be constructed.
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
\item{gridsize}{
number of grid points used in the binned approximations
to functional estimates.
}
\item{range.x}{
range over which functional estimates are obtained.
The default is the minimum and maximum data values.
}
\item{truncate}{
if \code{truncate} is \code{TRUE} then observations outside
of the interval specified by \code{range.x} are omitted.
Otherwise, they are used to weight the extreme grid points.
}}
\value{
the selected bin width.
}
\details{
The direct plug-in approach, where unknown functionals
that appear in expressions for the asymptotically
optimal bin width and bandwidths
are replaced by kernel estimates, is used.
The normal distribution is used to provide an
initial estimate.
}
\section{Background}{
This method for selecting the bin width of a histogram is
described in Wand (1995). It is an extension of the
normal scale rule of Scott (1979) and uses plug-in ideas
from bandwidth selection for kernel density estimation
(e.g. Sheather and Jones, 1991).
}
\references{
Scott, D. W. (1979). 
On optimal and data-based histograms.
\emph{Biometrika},
\bold{66}, 605--610.

Sheather, S. J. and Jones, M. C. (1991).
A reliable data-based bandwidth selection method for
kernel density estimation.
\emph{Journal of the Royal Statistical Society, Series B},
\bold{53}, 683--690. 

Wand, M. P. (1995).
Data-based choice of histogram binwidth.
\emph{The American Statistician}, \bold{51}, 59--64.
}
\seealso{
  \code{\link{hist}}
}
\examples{
data(geyser, package="MASS")
x <- geyser$duration
h <- dpih(x)
bins <- seq(min(x)-h, max(x)+h, by=h)
hist(x, breaks=bins)
}
\keyword{smooth}

