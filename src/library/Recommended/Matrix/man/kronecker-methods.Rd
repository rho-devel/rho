\name{kronecker-methods}
\docType{methods}
\title{Methods for Function 'kronecker()' in Package 'Matrix'}
\alias{kronecker-methods}
%
\alias{kronecker,ANY,Matrix-method}
\alias{kronecker,ANY,diagonalMatrix-method}
\alias{kronecker,ANY,sparseMatrix-method}
\alias{kronecker,Matrix,ANY-method}
\alias{kronecker,Matrix,diagonalMatrix-method}
\alias{kronecker,diagonalMatrix,ANY-method}
\alias{kronecker,diagonalMatrix,Matrix-method}
\alias{kronecker,indMatrix,indMatrix-method}
\alias{kronecker,sparseMatrix,ANY-method}
\alias{kronecker,sparseMatrix,TsparseMatrix-method}
%
\alias{kronecker,TsparseMatrix,sparseMatrix-method}
\alias{kronecker,TsparseMatrix,TsparseMatrix-method}
\alias{kronecker,dgTMatrix,dgTMatrix-method}
\alias{kronecker,dgTMatrix,dtTMatrix-method}
\alias{kronecker,dtTMatrix,dgTMatrix-method}
\alias{kronecker,dtTMatrix,dtTMatrix-method}
\alias{kronecker,dsparseMatrix,dsparseMatrix-method}
%
\description{
  Computes Kronecker products for objects inheriting from
  \code{"\linkS4class{Matrix}"}.

  In order to preserver sparseness, we treat \code{0 * NA} as \code{0},
  not as \code{\link{NA}} as usually in \R (and as used for the
  \pkg{base} function \code{\link[base]{kronecker}}).
}
\section{Methods}{
  \describe{

    \item{kronecker}{\code{signature(X = "Matrix", Y = "ANY")} .......}
    \item{kronecker}{\code{signature(X = "ANY", Y = "Matrix")} .......}
    \item{kronecker}{\code{signature(X = "diagonalMatrix", Y = "ANY")} .......}
    \item{kronecker}{\code{signature(X = "sparseMatrix", Y = "ANY")} .......}
    \item{kronecker}{\code{signature(X = "TsparseMatrix", Y = "TsparseMatrix")} .......}
    \item{kronecker}{\code{signature(X = "dgTMatrix", Y = "dgTMatrix")} .......}
    \item{kronecker}{\code{signature(X = "dtTMatrix", Y = "dtTMatrix")} .......}
    \item{kronecker}{\code{signature(X = "indMatrix", Y = "indMatrix")} .......}

  }
}
\examples{
(t1 <- spMatrix(5,4, x= c(3,2,-7,11), i= 1:4, j=4:1)) #  5 x  4
(t2 <- kronecker(Diagonal(3, 2:4), t1))               # 15 x 12

## should also work with special-cased logical matrices
l3 <- upper.tri(matrix(,3,3))
M <- Matrix(l3)
(N <- as(M, "nsparseMatrix"))
N2 <- as(N, "generalMatrix")
MM <- kronecker(M,M)
NN <- kronecker(N,N)
NN2 <- kronecker(N2,N2)
stopifnot(identical(NN,MM),
          is(NN, "triangularMatrix"))
}
\keyword{methods}
\keyword{array}

