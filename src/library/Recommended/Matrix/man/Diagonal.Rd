\name{Diagonal}
\alias{Diagonal}
\alias{.symDiagonal}
\title{Create Diagonal Matrix Object}
\description{
  Create a diagonal matrix object, i.e., an object inheriting from
  \code{\linkS4class{diagonalMatrix}}.
}
\usage{
Diagonal(n, x = NULL)
.symDiagonal(n, x = rep.int(1,n), uplo = "U")
}
\arguments{
  \item{n}{integer specifying the dimension of the (square) matrix.  If
    missing, \code{length(x)} is used.}
  \item{x}{numeric or logical; if missing, a \emph{unit} diagonal
    \eqn{n \times n}{n x n} matrix is created.}
  \item{uplo}{for \code{.symDiagonal}, the resulting sparse
    \code{\linkS4class{symmetricMatrix}} will have slot \code{uplo} set
    from this argument, either \code{"U"} or \code{"L"}.  Only rarely
    will it make sense to change this from the default.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  \code{Diagonal()} returns an object of class
  \code{\linkS4class{ddiMatrix}} or \code{\linkS4class{ldiMatrix}}
  (with \dQuote{superclass} \code{\linkS4class{diagonalMatrix}}).

  \code{.symDiagonal()} returns an object of class
  \code{\linkS4class{dsCMatrix}} or \code{\linkS4class{lsCMatrix}},
  i.e., a \emph{sparse} \emph{symmetric} matrix.  This can be
  more efficient than \code{Diagonal(n)} when the result is combined
  with further symmetric (sparse) matrices, however \emph{not} for
  matrix multiplications where \code{Diagonal()} is clearly preferred.
}
%\author{Martin Maechler}
\seealso{the generic function \code{\link{diag}} for \emph{extraction}
  of the diagonal from a matrix works for all \dQuote{Matrices}.

  \code{\link{bandSparse}} constructs a \emph{banded} sparse matrix from
  its non-zero sub-/super - diagonals.

  \code{\link{Matrix}} for general matrix construction;
  further, class \code{\linkS4class{diagonalMatrix}}.
}
\examples{
Diagonal(3)
Diagonal(x = 10^(3:1))
Diagonal(x = (1:4) >= 2)#-> "ldiMatrix"

## Use Diagonal() + kronecker() for "repeated-block" matrices:
M1 <- Matrix(0+0:5, 2,3)
(M <- kronecker(Diagonal(3), M1))

(S <- crossprod(Matrix(rbinom(60, size=1, prob=0.1), 10,6)))
(SI <- S + 10*.symDiagonal(6)) # sparse symmetric still
stopifnot(is(SI, "dsCMatrix"))
}
\keyword{array}
\keyword{algebra}
