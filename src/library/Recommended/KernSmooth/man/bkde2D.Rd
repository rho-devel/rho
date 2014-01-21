\name{bkde2D}
\alias{bkde2D}
\title{
Compute a 2D Binned Kernel Density Estimate
}
\description{
Returns the set of grid points in each coordinate direction,
and the matrix of density estimates over the mesh induced by
the grid points. The kernel is the standard bivariate normal
density. 
}
\usage{
bkde2D(x, bandwidth, gridsize = c(51L, 51L), range.x, truncate = TRUE)
}
\arguments{
\item{x}{
a two-column numeric matrix containing the observations from the  
distribution whose density is to be estimated.
Missing values are not allowed.
}
\item{bandwidth}{
numeric vector oflength 2, containing the bandwidth to be used in each coordinate
direction.
}
\item{gridsize}{
vector containing the number of equally spaced points in each direction
over which the density is to be estimated.
}
\item{range.x}{
a list containing two vectors, where each vector 
contains the minimum and maximum values of \code{x}
at which to compute the estimate for each direction.
The default minimum in each direction is minimum
data value minus 1.5 times the bandwidth for
that direction. The default maximum is the maximum
data value plus 1.5 times the bandwidth for
that direction
}
\item{truncate}{
logical flag: if TRUE, data with \code{x} values outside the
range specified by \code{range.x} are ignored.
}}
\value{
a list containing the following components:

\item{x1}{
vector of values of the grid points in the first coordinate
direction at which the estimate was computed. 
}
\item{x2}{
vector of values of the grid points in the second coordinate
direction at which the estimate was computed. 
}
\item{fhat}{
matrix of density estimates 
over the mesh induced by \code{x1} and \code{x2}.
}}
\section{Details}{
This is the binned approximation to the 2D kernel density estimate.
Linear binning is used to obtain the bin counts and the
Fast Fourier Transform is used to perform the discrete convolutions.
For each \code{x1},\code{x2} pair the bivariate Gaussian kernel is
centered on that location and the heights of the 
kernel, scaled by the bandwidths, at each datapoint are summed.
This sum, after a normalization, is the corresponding 
\code{fhat} value in the output.
}
\references{
Wand, M. P. (1994).
Fast Computation of Multivariate Kernel Estimators.
\emph{Journal of Computational and Graphical Statistics,}
\bold{3}, 433-445.


Wand, M. P. and Jones, M. C. (1995).
\emph{Kernel Smoothing.}
Chapman and Hall, London.
}
\seealso{
  \code{\link{bkde}}, \code{\link{density}}, \code{\link{hist}}.
}
\examples{
data(geyser, package="MASS")
x <- cbind(geyser$duration, geyser$waiting)
est <- bkde2D(x, bandwidth=c(0.7, 7))
contour(est$x1, est$x2, est$fhat)
persp(est$fhat)
}
\keyword{distribution}
\keyword{smooth}
% Converted by Sd2Rd version 0.2-a5.
