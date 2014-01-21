\name{Diagonal}
\title{Create Diagonal Matrix Object}
\alias{Diagonal}
\alias{.sparseDiagonal}
\alias{.symDiagonal}
\description{
  Create a diagonal matrix object, i.e., an object inheriting from
  \code{\linkS4class{diagonalMatrix}}.
}
\usage{
Diagonal(n, x = NULL)

.symDiagonal(n, x = rep.int(1,n), uplo = "U")
.sparseDiagonal(n, x = 1, uplo = "U",
                shape = if(missing(cols)) "t" else "g",
	        unitri, kind, cols = if(n) 0:(n - 1L) else integer(0))
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
  \item{shape}{string of 1 character, one of \code{c("t","s","g")}, to
    chose a triangular, symmetric or general result matrix.}
  \item{unitri}{optional logical indicating if a triangular result
    should be \dQuote{unit-triangular}, i.e., with \code{diag = "U"}
    slot, if possible.  The default, \code{\link{missing}}, is the same
    as \code{\link{TRUE}}.}
  \item{kind}{string of 1 character, one of \code{c("d","l","n")}, to
    chose the storage mode of the result, from classes
    \code{\linkS4class{dsparseMatrix}},
    \code{\linkS4class{lsparseMatrix}}, or
    \code{\linkS4class{nsparseMatrix}}, respectively.}
  \item{cols}{integer vector with values from \code{0:(n-1)}, denoting
    the \emph{columns} to subselect conceptually, i.e., get the
    equivalent of  \code{Diagonal(n,*)[, cols + 1]}.}
}
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

  \code{.sparseDiagonal()}, the workhorse of \code{.symDiagonal} returns
  a \code{\linkS4class{CsparseMatrix}} (the resulting class depending
  on \code{shape} and \code{kind}) representation of \code{Diagonal(n)},
  or, when \code{cols} are specified, of \code{Diagonal(n)[, cols+1]}.
}
\author{Martin Maechler}
\seealso{the generic function \code{\link{diag}} for \emph{extraction}
  of the diagonal from a matrix works for all \dQuote{Matrices}.

  \code{\link{bandSparse}} constructs a \emph{banded} sparse matrix from
  its non-zero sub-/super - diagonals.  \code{\link{band}(A)} returns a
  band matrix containing some sub-/super - diagonals of \code{A}.

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
(I4 <- .sparseDiagonal(4, shape="t"))# now (2012-10) unitriangular
stopifnot(I4@diag == "U", all(I4 == diag(4)))
\dontshow{% checking some "unit-diagonality":
  L <- Diagonal(5, TRUE)
  stopifnot(L@diag == "U", identical(L, Diagonal(5) > 0))
}
}
\keyword{array}
\keyword{algebra}
