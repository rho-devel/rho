\name{invPerm}
\alias{invPerm}
\title{Inverse Permutation Vector}
\description{
  From a permutation vector \code{p}, compute its \emph{inverse}
  permutation vector.
}
\usage{
invPerm(p, zero.p = FALSE, zero.res = FALSE)
}
\arguments{
  \item{p}{an integer vector of length, say, \code{n}.}
  \item{zero.p}{logical indicating if \code{p} contains values
    \code{0:(n-1)} or rather (by default, \code{zero.p = FALSE}) \code{1:n}.}
  \item{zero.res}{logical indicating if the result should contain values
    \code{0:(n-1)} or rather (by default, \code{zero.res = FALSE}) \code{1:n}.}
}
\value{
  an integer vector of the same length (\code{n}) as \code{p}.
  By default, (\code{zero.p = FALSE, zero.res = FALSE}),
  \code{invPerm(p)} is the same as \code{\link{order}(p)} or
  \code{\link{sort.list}(p)} and for that case, the function is
  equivalent to \code{invPerm. <- function(p) { p[p] <- seq_along(p) ; p }}.
}
\author{Martin Maechler}
\seealso{the class of permutation matrices, \code{\linkS4class{pMatrix}}.
}
\examples{
  p <- sample(10) # a random permutation vector
  ip <- invPerm(p)
  p[ip] # == 1:10
  ## they are indeed inverse of each other:
  stopifnot(
    identical(p[ip], 1:10),
    identical(ip[p], 1:10),
    identical(invPerm(ip), p)
  )
}
\keyword{arithmetic}
