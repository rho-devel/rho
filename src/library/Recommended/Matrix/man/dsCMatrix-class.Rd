\name{dsCMatrix-class}
\title{Numeric Symmetric Sparse (column compressed) Matrices}
\docType{class}
\alias{dsCMatrix-class}
\alias{dsTMatrix-class}
%
\alias{solve,dsCMatrix,dsparseMatrix-method}
\alias{solve,dsCMatrix,matrix-method}
\alias{solve,dsCMatrix,numeric-method}
\alias{solve,dsCMatrix,ddenseMatrix-method}
\alias{solve,dsCMatrix,denseMatrix-method}
\alias{t,dsCMatrix-method}
\alias{t,dsTMatrix-method}
\alias{coerce,dgeMatrix,dsCMatrix-method}
\alias{coerce,dgeMatrix,dsTMatrix-method}
\alias{coerce,dsCMatrix,dgCMatrix-method}
\alias{coerce,dsCMatrix,dgTMatrix-method}
\alias{coerce,dsCMatrix,dgeMatrix-method}
\alias{coerce,dsCMatrix,dsRMatrix-method}
\alias{coerce,dsCMatrix,dsTMatrix-method}
\alias{coerce,dsCMatrix,dsyMatrix-method}
\alias{coerce,dsCMatrix,lsCMatrix-method}
\alias{coerce,dsCMatrix,generalMatrix-method}
\alias{coerce,dsCMatrix,matrix-method}
\alias{coerce,dsCMatrix,nsCMatrix-method}
\alias{coerce,dsTMatrix,dgTMatrix-method}
\alias{coerce,dsTMatrix,dgeMatrix-method}
\alias{coerce,dsTMatrix,dsCMatrix-method}
\alias{coerce,dsTMatrix,dsyMatrix-method}
\alias{coerce,dsTMatrix,lsTMatrix-method}
\alias{coerce,dsTMatrix,matrix-method}
\alias{coerce,dsyMatrix,dsCMatrix-method}
\alias{coerce,dsyMatrix,dsTMatrix-method}
\alias{coerce,dtCMatrix,dtRMatrix-method}
\alias{coerce,matrix,dsCMatrix-method}
\alias{coerce,matrix,dsTMatrix-method}
\alias{coerce,graphNEL,dsCMatrix-method}
%% Group methods
\alias{Arith,dsCMatrix,dsCMatrix-method}
\alias{determinant,dsCMatrix,missing-method}
\alias{determinant,dsCMatrix,logical-method}
\description{The \code{dsCMatrix} class is a class of symmetric, sparse
  numeric matrices in the compressed, column-oriented format.  In this
  implementation the non-zero elements in the columns are sorted into
  increasing row order.\cr
  The \code{dsTMatrix} class is the class of
  symmetric, sparse numeric matrices in triplet format.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dsCMatrix",
    ...)} or \code{new("dsTMatrix", ...)} or (for \code{dsCMatrix}) also
  automatically from \code{\link{Matrix}(.)}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{A character object indicating if the upper
      triangle (\code{"U"}) or the lower triangle (\code{"L"}) is stored.}
    \item{\code{i}:}{Object of class \code{"integer"} of length nnZ
      (\emph{half} number of non-zero elements).  These are the row
      numbers for each non-zero element in the lower triangle of the matrix.}
    \item{\code{p}:}{(only in class \code{"dsCMatrix"}:) Object of class
      \code{"integer"} of pointers, one for each column, to the initial
      (zero-based) index of elements in the column.}
    \item{\code{j}:}{(only in class \code{"dsTMatrix"}:) Object of
      class \code{"integer"} of length nnZ (as \code{i}).  These are the
      column numbers for each non-zero element in the lower triangle of
      the matrix.}
    \item{\code{x}:}{Object of class \code{"numeric"} of length nnZ --
      the non-zero elements of the matrix (to be duplicated for full matrix).}
    \item{\code{factors}:}{Object of class \code{"list"} - a list
      of factorizations of the matrix. }
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
     of the matrix - must be an integer vector with exactly two
     non-negative values.}
  }
}
\section{Extends}{
  Both classes extend classes and \code{\linkS4class{symmetricMatrix}}
  \code{\linkS4class{dsparseMatrix}} directly;
  \code{dsCMatrix} further directly extends
  \code{\linkS4class{CsparseMatrix}}, where
  \code{dsTMatrix} does \code{\linkS4class{TsparseMatrix}}.
}
\section{Methods}{
  \describe{
    \item{solve}{\code{signature(a = "dsCMatrix", b = "dsparseMatrix")}: Solve
      a linear system of equations defined by \code{x} using a Cholesky
      decomposition.  All steps will be based on \emph{sparse}
      representations.}
    \item{solve}{\code{signature(a = "dsCMatrix", b = "matrix")}: Solve
      a linear system of equations defined by \code{x} using a Cholesky
      decomposition.}
    \item{chol}{\code{signature(x = "dsCMatrix", pivot = "logical")}:
      Returns (and stores) the Cholesky decomposition of \code{x}, see
      \code{\link{chol}}.}
    \item{Cholesky}{\code{signature(A = "dsCMatrix",...)}:
      Computes more flexibly Cholesky decompositions,
      see \code{\link{Cholesky}}.}
    \item{determinant}{\code{signature(x = "dsCMatrix", logarithm =
	"missing")}: Evaluate the determinant of \code{x} on the
      logarithm scale.  This creates and stores the Cholesky factorization.}
    \item{determinant}{\code{signature(x = "dsCMatrix", logarithm =
	"logical")}: Evaluate the determinant of \code{x} on the
      logarithm scale or not, according to the \code{logarithm}
      argument.  This creates and stores the Cholesky factorization.}
    \item{t}{\code{signature(x = "dsCMatrix")}: Transpose.  As for all
      symmetric matrices, a matrix for which the upper triangle is
      stored produces a matrix for which the lower triangle is stored
      and vice versa, i.e., the \code{uplo} slot is swapped, and the row
      and column indices are interchanged.}
    \item{t}{\code{signature(x = "dsTMatrix")}: Transpose.  The
      \code{uplo} slot is swapped from \code{"U"} to \code{"L"} or vice
      versa, as for a \code{"dsCMatrix"}, see above.}
    \item{coerce}{\code{signature(from = "dsCMatrix", to = "dgTMatrix")}}
    \item{coerce}{\code{signature(from = "dsCMatrix", to = "dgeMatrix")}}
    \item{coerce}{\code{signature(from = "dsCMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "dsTMatrix", to = "dgeMatrix")}}
    \item{coerce}{\code{signature(from = "dsTMatrix", to = "dsCMatrix")}}
    \item{coerce}{\code{signature(from = "dsTMatrix", to = "dsyMatrix")}}
    \item{coerce}{\code{signature(from = "dsTMatrix", to = "matrix")}}
  }
}
%\references{}
%\author{}
%\note{}
\seealso{
  Classes \code{\linkS4class{dgCMatrix}}, \code{\linkS4class{dgTMatrix}},
  \code{\linkS4class{dgeMatrix}} and those mentioned above.
}
\examples{
mm <- Matrix(toeplitz(c(10, 0, 1, 0, 3)), sparse = TRUE)
mm # automatically dsCMatrix
str(mm)

## how would we go from a manually constructed Tsparse* :
mT <- as(mm, "dgTMatrix")

## Either
(symM <- as(mT, "symmetricMatrix"))# dsT
(symC <- as(symM, "CsparseMatrix"))# dsC
## or
sC <- Matrix(mT, sparse=TRUE, forceCheck=TRUE)

sym2 <- as(symC, "TsparseMatrix")
\dontshow{
stopifnot(identical(symC, sC), identical(sym2, symM),
          class(sym2) == "dsTMatrix",
	  identical(sym2[1,], sC[1,]),
	  identical(sym2[,2], sC[,2]))
}
}
\keyword{classes}
\keyword{algebra}
