\name{lower.to.upper.tri.inds}
\alias{lower.to.upper.tri.inds}
\alias{upper.to.lower.tri.inds}
\title{Permute Indices for Triangular Matrices}
\description{
  Compute index vectors for extracting or reordering of lower or upper
  triangular matrices that are stored as contiguous vectors.
}
\usage{
lower.to.upper.tri.inds(n)
upper.to.lower.tri.inds(n)
}
\arguments{
  \item{n}{integer larger than 1.}
}
\value{
  integer vector containing a permutation of \code{1:N} where
  \eqn{N = n(n-1)/2}.
}
\note{
  these functions are mainly for internal use in the cluster package,
  and may not remain available (unless we see a good reason).
}
\seealso{\code{\link{upper.tri}}, \code{\link{lower.tri}} with a related
  purpose.}
\examples{
m5 <- matrix(NA,5,5)
m <- m5; m[lower.tri(m)] <- upper.to.lower.tri.inds(5); m
m <- m5; m[upper.tri(m)] <- lower.to.upper.tri.inds(5); m

stopifnot(lower.to.upper.tri.inds(2) == 1,
          lower.to.upper.tri.inds(3) == 1:3,
          upper.to.lower.tri.inds(3) == 1:3,
     sort(upper.to.lower.tri.inds(5)) == 1:10,
     sort(lower.to.upper.tri.inds(6)) == 1:15)
}
\keyword{array}
\keyword{utilities}
