\name{KNex}
\alias{KNex}
\docType{data}
\title{Koenker-Ng Example Sparse Model Matrix and Response Vector}
\description{
  A model matrix \code{mm} and corresponding response vector \code{y}
  used in an example by Koenker and Ng.  The matrix \code{mm} is a sparse
  matrix with 1850 rows and 712 columns but only 8758 non-zero entries.
  It is a \code{"dgCMatrix"} object.  The vector \code{y} is just
  \code{\link{numeric}} of length 1850.
}
\usage{data(KNex)}
%\details{}
%\source{}
\references{
  Roger Koenker and Pin Ng (2003).
  SparseM: A sparse matrix package for R;
  \emph{J. of Statistical Software}, \bold{8} (6),
  \url{http://www.jstatsoft.org/}
}
\examples{
data(KNex)
class(KNex$mm)
  dim(KNex$mm)
image(KNex$mm)
str(KNex)

system.time( # a fraction of a second
  sparse.sol <- with(KNex, solve(crossprod(mm), crossprod(mm, y))))

head(round(sparse.sol,3))

## Compare with QR-based solution ("more accurate, but slightly slower"):
system.time(
  sp.sol2 <- with(KNex, qr.coef(qr(mm), y) ))

all.equal(sparse.sol, sp.sol2, tol = 1e-13) # TRUE
}
\keyword{datasets}
