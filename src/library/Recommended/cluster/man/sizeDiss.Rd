\name{sizeDiss}
\alias{sizeDiss}
\title{Sample Size of Dissimilarity Like Object}
\description{
  Returns the number of observations (\emph{sample size}) corresponding
  to a  dissimilarity like object, or equivalently,
  the number of rows or columns of a matrix
  when only the lower or upper triangular part (without diagonal) is given.

  It is nothing else but the inverse function of \eqn{f(n) = n(n-1)/2}.
}
\usage{
sizeDiss(d)
}
\arguments{
  \item{d}{any \R object with length (typically) \eqn{n(n-1)/2}.}
}
\value{
  a number; \eqn{n} if \code{length(d) == n(n-1)/2}, \code{NA} otherwise.
}
\seealso{\code{\link{dissimilarity.object}} and also
  \code{\link{as.dist}} for class \code{dissimilarity} and
  \code{dist} objects which have a \code{Size} attribute.}
\examples{
sizeDiss(1:10)# 5, since 10 == 5 * (5 - 1) / 2
sizeDiss(1:9) # NA

n <- 1:100
stopifnot(n == sapply( n*(n-1)/2, function(n) sizeDiss(logical(n))))
}
\keyword{utilities}
\keyword{arith}
