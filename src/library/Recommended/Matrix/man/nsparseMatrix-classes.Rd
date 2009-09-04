\name{nsparseMatrix-classes}
\title{Sparse "pattern" Matrices}
\docType{class}
\alias{nsparseMatrix-class}
\alias{ngCMatrix-class}
\alias{ntCMatrix-class}
\alias{nsCMatrix-class}
\alias{ngRMatrix-class}
\alias{ntRMatrix-class}
\alias{nsRMatrix-class}
\alias{ngTMatrix-class}
\alias{ntTMatrix-class}
\alias{nsTMatrix-class}
%
\alias{all,nsparseMatrix-method}
\alias{any,nsparseMatrix-method}
\alias{-,nsparseMatrix,missing-method}
\alias{!,nsparseMatrix-method}
\alias{\%*\%,ngCMatrix,ngCMatrix-method}
\alias{\%*\%,nsparseMatrix,nsparseMatrix-method}
\alias{\%*\%,nsparseMatrix,ndenseMatrix-method}
\alias{\%*\%,ndenseMatrix,nsparseMatrix-method}

\alias{coerce,nsparseMatrix,dsparseMatrix-method}
\alias{coerce,matrix,ngCMatrix-method}
\alias{coerce,matrix,ngTMatrix-method}
\alias{coerce,matrix,ntCMatrix-method}
\alias{coerce,matrix,ntTMatrix-method}
\alias{coerce,ngCMatrix,dMatrix-method}
\alias{coerce,ngCMatrix,dgCMatrix-method}
\alias{coerce,ngCMatrix,dsparseMatrix-method}
\alias{coerce,ngCMatrix,lMatrix-method}
\alias{coerce,ngCMatrix,lgCMatrix-method}
\alias{coerce,ngCMatrix,lsparseMatrix-method}
\alias{coerce,ngCMatrix,matrix-method}
\alias{coerce,ngCMatrix,ngTMatrix-method}
\alias{coerce,ngCMatrix,ngeMatrix-method}
\alias{coerce,ngCMatrix,ntCMatrix-method}
\alias{coerce,ngTMatrix,dMatrix-method}
\alias{coerce,ngTMatrix,dgTMatrix-method}
\alias{coerce,ngTMatrix,dsparseMatrix-method}
\alias{coerce,ngTMatrix,generalMatrix-method}
\alias{coerce,ngTMatrix,lMatrix-method}
\alias{coerce,ngTMatrix,lgTMatrix-method}
\alias{coerce,ngTMatrix,matrix-method}
\alias{coerce,ngTMatrix,ngCMatrix-method}
\alias{coerce,ngTMatrix,lgeMatrix-method}
\alias{coerce,ngTMatrix,ngeMatrix-method}
\alias{coerce,ngTMatrix,ntTMatrix-method}
\alias{coerce,ngTMatrix,triangularMatrix-method}
\alias{coerce,nsCMatrix,dMatrix-method}
\alias{coerce,nsCMatrix,dsCMatrix-method}
\alias{coerce,nsCMatrix,dsparseMatrix-method}
\alias{coerce,nsCMatrix,generalMatrix-method}
\alias{coerce,nsCMatrix,lMatrix-method}
\alias{coerce,nsCMatrix,lsCMatrix-method}
\alias{coerce,nsCMatrix,lsparseMatrix-method}
\alias{coerce,nsCMatrix,matrix-method}
\alias{coerce,nsCMatrix,ngCMatrix-method}
\alias{coerce,nsCMatrix,nsTMatrix-method}
\alias{coerce,nsTMatrix,dsTMatrix-method}
\alias{coerce,nsTMatrix,matrix-method}
\alias{coerce,nsTMatrix,ngCMatrix-method}
\alias{coerce,nsTMatrix,ngTMatrix-method}
\alias{coerce,nsTMatrix,nsCMatrix-method}
\alias{coerce,nsTMatrix,nsyMatrix-method}
\alias{coerce,ntCMatrix,dMatrix-method}
\alias{coerce,ntCMatrix,dsparseMatrix-method}
\alias{coerce,ntCMatrix,dtCMatrix-method}
\alias{coerce,ntCMatrix,lMatrix-method}
\alias{coerce,ntCMatrix,lsparseMatrix-method}
\alias{coerce,ntCMatrix,ltCMatrix-method}
\alias{coerce,ntCMatrix,matrix-method}
\alias{coerce,ntCMatrix,ngCMatrix-method}
\alias{coerce,ntCMatrix,TsparseMatrix-method}
\alias{coerce,ntTMatrix,dtTMatrix-method}
\alias{coerce,ntTMatrix,generalMatrix-method}
\alias{coerce,ntTMatrix,matrix-method}
\alias{coerce,ntTMatrix,ngCMatrix-method}
\alias{coerce,ntTMatrix,ngTMatrix-method}
\alias{coerce,ntTMatrix,ntCMatrix-method}
\alias{coerce,ntTMatrix,ntrMatrix-method}
%
\alias{crossprod,ngCMatrix,missing-method}
\alias{crossprod,ngTMatrix,missing-method}
\alias{crossprod,nsparseMatrix-method}
\alias{crossprod,nsparseMatrix,missing-method}
\alias{crossprod,nsparseMatrix,nsparseMatrix-method}
\alias{crossprod,nsparseMatrix,ndenseMatrix-method}
\alias{crossprod,ndenseMatrix,nsparseMatrix-method}
\alias{t,ngCMatrix-method}
\alias{t,ngTMatrix-method}
\alias{t,nsCMatrix-method}
\alias{t,ntCMatrix-method}
\alias{t,nsTMatrix-method}
\alias{t,ntTMatrix-method}
\alias{tcrossprod,ngCMatrix,missing-method}
\alias{tcrossprod,ngTMatrix,missing-method}
\alias{tcrossprod,nsparseMatrix,missing-method}
\alias{is.na,nsparseMatrix-method}
% Group
\alias{Ops,dsparseMatrix,nsparseMatrix-method}
\alias{Ops,nsparseMatrix,dsparseMatrix-method}
\alias{Ops,lsparseMatrix,nsparseMatrix-method}
\alias{Ops,nsparseMatrix,lsparseMatrix-method}
\alias{Ops,sparseMatrix,nsparseMatrix-method}
\alias{Ops,nsparseMatrix,sparseMatrix-method}
\alias{Arith,nsparseMatrix,Matrix-method}
\alias{Arith,Matrix,nsparseMatrix-method}
%
\description{The \code{nsparseMatrix} class is a virtual class of sparse
  \emph{\dQuote{pattern}} matrices, i.e., binary matrices conceptually
  with \code{TRUE}/\code{FALSE} entries.  Only the positions of the
  elements that are \code{TRUE} are stored.  These can be stored in the
  ``triplet'' form (classes \code{ngTMatrix}, \code{nsTMatrix}, and
  \code{ntTMatrix} which really contain pairs, not triplets) or in
  compressed column-oriented form (classes \code{ngCMatrix},
  \code{nsCMatrix}, and \code{ntCMatrix}) or in compressed row-oriented
  form (classes \code{ngRMatrix}, \code{nsRMatrix}, and
  \code{ntRMatrix}).  The second letter in the name of these non-virtual
  classes indicates \code{g}eneral, \code{s}ymmetric, or \code{t}riangular.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("ngCMatrix",
    ...)} and so on.  More frequently objects are created by coercion of
  a numeric sparse matrix to the pattern form for use in
  the symbolic analysis phase
  of an algorithm involving sparse matrices.  Such algorithms often
  involve two phases: a symbolic phase wherein the positions of the
  non-zeros in the result are determined and a numeric phase wherein the
  actual results are calculated.  During the symbolic phase only the
  positions of the non-zero elements in any operands are of interest,
  hence numeric sparse matrices can be treated as sparse pattern
  matrices.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower
      triangular. Present in the triangular and symmetric classes but not
      in the general class.}
    \item{\code{diag}:}{Object of class \code{"character"}. Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"} for non-unit.  The implicit diagonal elements are not
      explicitly stored when \code{diag} is \code{"U"}.  Present in the
      triangular classes only.}
    \item{\code{p}:}{Object of class \code{"integer"} of pointers, one
      for each column (row), to the initial (zero-based) index of elements in
      the column.  Present in compressed column-oriented and compressed
      row-oriented forms only.}
    \item{\code{i}:}{Object of class \code{"integer"} of length nnzero
      (number of non-zero elements).  These are the row numbers for
      each TRUE element in the matrix.  All other elements are FALSE.
      Present in triplet and compressed column-oriented forms only.}
    \item{\code{j}:}{Object of class \code{"integer"} of length nnzero
      (number of non-zero elements).  These are the column numbers for
      each TRUE element in the matrix.  All other elements are FALSE.
      Present in triplet and compressed column-oriented forms only.}
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
      of the matrix.}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "dgCMatrix", to =
	"ngCMatrix")}, and many similar ones; typically you should
      coerce to \code{"nsparseMatrix"} (or \code{"nMatrix"}).  Note that
      coercion to a sparse pattern matrix records all the potential
      non-zero entries, i.e., explicit (\dQuote{non-structural}) zeroes
      are coerced to \code{TRUE}, not \code{FALSE}, see the example.
    }
    \item{t}{\code{signature(x = "ngCMatrix")}: returns the transpose
      of \code{x}}
  }
}
%\references{}
%\author{}
%\note{}
\seealso{
  the class \code{\linkS4class{dgCMatrix}}
}
\examples{
(m <- Matrix(c(0,0,2:0), 3,5, dimnames=list(LETTERS[1:3],NULL)))
## ``extract the nonzero-pattern of (m) into an nMatrix'':
nm <- as(m, "nsparseMatrix") ## -> will be a "ngCMatrix"
str(nm) # no 'x' slot
nnm <- !nm     # no longer sparse
(nnm <- as(nnm, "sparseMatrix"))# "lgCMatrix"
## consistency check:
stopifnot(xor(as( nm, "matrix"),
              as(nnm, "matrix")))

## low-level way of adding "non-structural zeros" :
nnm@x[2:4] <- c(FALSE,NA,NA)
nnm
as(nnm, "nMatrix") # NAs *and* non-structural 0  |--->  'TRUE'

data(KNex)
nmm <- as(KNex $ mm, "ngCMatrix")
str(xlx <- crossprod(nmm))# "nsCMatrix"
stopifnot(isSymmetric(xlx))
image(xlx, main=paste("crossprod(nmm) : Sparse", class(xlx)))
}
\keyword{classes}
\keyword{algebra}
