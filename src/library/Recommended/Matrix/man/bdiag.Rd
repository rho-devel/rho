\name{bdiag}
\alias{bdiag}
\title{Construct a Block Diagonal Matrix}
\description{
  Build a block diagonal matrix given several building block matrices.
}
\usage{
bdiag(\dots)
}
\arguments{
  \item{\dots}{individual matrices or a \code{\link{list}} of matrices.}
}
% \details{
% }
\value{
  A \emph{sparse} matrix inheriting from class
  \code{\linkS4class{CsparseMatrix}}
  obtained by combining the arguments into a block diagonal matrix.
}
\author{Martin Maechler, built on a version posted by Berton Gunter to
  R-help; earlier versions have been posted by other authors, notably
  Scott Chasalow to S-news.  Doug Bates's faster implementation builds
  on \code{\linkS4class{TsparseMatrix}} objects.
}
\seealso{\code{\link{Diagonal}} for constructing matrices of
  class \code{\linkS4class{diagonalMatrix}}, or \code{\link{kronecker}}
  which also works for \code{"Matrix"} inheriting matrices.

  \code{\link{bandSparse}} constructs a \emph{banded} sparse matrix from
  its non-zero sub-/super - diagonals.

  Note that other CRAN \R packages have own versions of \code{bdiag()}
  which return traditional matrices.
}
\examples{
bdiag(matrix(1:4, 2), diag(3))
## combine "Matrix" class and traditional matrices:
bdiag(Diagonal(2), matrix(1:3, 3,4), diag(3:2))

mlist <- list(1, 2:3, diag(x=5:3), 27, cbind(1,3:6), 100:101)
bdiag(mlist)

stopifnot(identical(bdiag(mlist), % <- used to fail in earlier versions
                    bdiag(lapply(mlist, as.matrix))))
}
\keyword{array}
