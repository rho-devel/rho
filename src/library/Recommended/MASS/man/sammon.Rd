% file MASS/man/sammon.Rd
% copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
%
\name{sammon}
\alias{sammon}
\title{
Sammon's Non-Linear Mapping
}
\description{
One form of non-metric multidimensional scaling.
}
\usage{
sammon(d, y = cmdscale(d, k), k = 2, niter = 100, trace = TRUE,
       magic = 0.2, tol = 1e-4)
}
\arguments{
\item{d}{
distance structure of the form returned by \code{dist}, or a full, symmetric
matrix.  Data are assumed to be dissimilarities or relative distances,
but must be positive except for self-distance.  This can contain missing
values.
}
\item{y}{
  An initial configuration. If none is supplied, \code{cmdscale}
  is used to provide the classical solution.  (If there are missing
  values in \code{d}, an initial configuration must be provided.)  This
  must not have duplicates.
}
\item{k}{
The dimension of the configuration.
}
\item{niter}{
The maximum number of iterations.
}
\item{trace}{
Logical for tracing optimization. Default \code{TRUE}.
}
\item{magic}{
initial value of the step size constant in diagonal Newton method.
}
\item{tol}{
Tolerance for stopping, in units of stress.
}}
\value{
Two components:

\item{points}{
A two-column vector of the fitted configuration.
}
\item{stress}{
The final stress achieved.
}}
\section{Side Effects}{
If trace is true, the initial stress and the current stress are printed
out every 10 iterations.
}
\details{
This chooses a two-dimensional configuration to minimize the stress,
the sum of squared differences between the input distances and those
of the configuration, weighted by the distances, the whole sum being
divided by the sum of input distances to make the stress scale-free.

An iterative algorithm is used, which will usually converge in around
50 iterations.  As this is necessarily an \eqn{O(n^2)} calculation, it is slow
for large datasets.  Further, since the configuration is only determined
up to rotations and reflections (by convention the centroid is at the
origin), the result can vary considerably from machine to machine.
In this release the algorithm has been modified by adding a step-length
search (\code{magic}) to ensure that it always goes downhill.
}
\references{
  Sammon, J. W. (1969)
  A non-linear mapping for data structure analysis.
  \emph{IEEE Trans. Comput.}, \bold{C-18} 401--409.

  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks}. Cambridge University Press.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{cmdscale}}, \code{\link{isoMDS}}
}
\examples{
swiss.x <- as.matrix(swiss[, -1])
swiss.sam <- sammon(dist(swiss.x))
plot(swiss.sam$points, type = "n")
text(swiss.sam$points, labels = as.character(1:nrow(swiss.x)))
}
\keyword{multivariate}
