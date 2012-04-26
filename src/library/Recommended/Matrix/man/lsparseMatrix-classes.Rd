\name{lsparseMatrix-classes}
\title{Sparse logical matrices}
\docType{class}
\alias{lsparseMatrix-class}
\alias{lgCMatrix-class}
\alias{ltCMatrix-class}
\alias{lsCMatrix-class}
\alias{lgRMatrix-class}
\alias{ltRMatrix-class}
\alias{lsRMatrix-class}
\alias{lgTMatrix-class}
\alias{ltTMatrix-class}
\alias{lsTMatrix-class}
% Group(like)
\alias{Ops,lsparseMatrix,lsparseMatrix-method}
\alias{Arith,lsparseMatrix,Matrix-method}
\alias{Arith,Matrix,lsparseMatrix-method}
\alias{Arith,lgCMatrix,lgCMatrix-method}
\alias{Arith,lgTMatrix,lgTMatrix-method}
\alias{Compare,lsparseMatrix,lsparseMatrix-method}
\alias{Logic,lsparseMatrix,lsparseMatrix-method}
\alias{Logic,lgCMatrix,lgCMatrix-method}
\alias{Logic,lgTMatrix,lgTMatrix-method}
\alias{Logic,lsCMatrix,lsCMatrix-method}
\alias{Logic,ltCMatrix,ltCMatrix-method}

\alias{-,lsparseMatrix,missing-method}
\alias{!,lsparseMatrix-method}
%
\alias{\%*\%,lgCMatrix,lgCMatrix-method}
\alias{\%*\%,lsparseMatrix,lsparseMatrix-method}
\alias{\%*\%,lsparseMatrix,ldenseMatrix-method}
\alias{\%*\%,ldenseMatrix,lsparseMatrix-method}

\alias{coerce,lsparseMatrix,matrix-method}
\alias{coerce,lsparseMatrix,dsparseMatrix-method}
\alias{coerce,lgCMatrix,dgCMatrix-method}
\alias{coerce,lgCMatrix,lgTMatrix-method}
\alias{coerce,lgCMatrix,lgeMatrix-method}
\alias{coerce,lgCMatrix,lsCMatrix-method}
\alias{coerce,lgCMatrix,ltCMatrix-method}
\alias{coerce,lgCMatrix,matrix-method}
\alias{coerce,lgTMatrix,dgTMatrix-method}
\alias{coerce,lgTMatrix,lgCMatrix-method}
\alias{coerce,lgTMatrix,lgeMatrix-method}
\alias{coerce,lgTMatrix,lsCMatrix-method}
\alias{coerce,lgTMatrix,ltTMatrix-method}
\alias{coerce,lgTMatrix,matrix-method}
\alias{coerce,lsCMatrix,dgTMatrix-method}
\alias{coerce,lsCMatrix,dsCMatrix-method}
\alias{coerce,lsCMatrix,generalMatrix-method}
\alias{coerce,lsCMatrix,lgCMatrix-method}
\alias{coerce,lsCMatrix,lgTMatrix-method}
\alias{coerce,lsCMatrix,lsTMatrix-method}
\alias{coerce,lsCMatrix,matrix-method}
\alias{coerce,lsTMatrix,dsTMatrix-method}
\alias{coerce,lsTMatrix,lgCMatrix-method}
\alias{coerce,lsTMatrix,lgTMatrix-method}
\alias{coerce,lsTMatrix,lsCMatrix-method}
\alias{coerce,lsTMatrix,lsyMatrix-method}
\alias{coerce,lsTMatrix,matrix-method}
\alias{coerce,ltCMatrix,dMatrix-method}
\alias{coerce,ltCMatrix,dtCMatrix-method}
\alias{coerce,ltCMatrix,lgCMatrix-method}
\alias{coerce,ltCMatrix,ltTMatrix-method}
\alias{coerce,ltCMatrix,matrix-method}
\alias{coerce,ltTMatrix,dtTMatrix-method}
\alias{coerce,ltTMatrix,generalMatrix-method}
\alias{coerce,ltTMatrix,lgCMatrix-method}
\alias{coerce,ltTMatrix,lgTMatrix-method}
\alias{coerce,ltTMatrix,ltCMatrix-method}
\alias{coerce,ltTMatrix,ltrMatrix-method}
\alias{coerce,ltTMatrix,matrix-method}

\alias{coerce,matrix,lgCMatrix-method}
\alias{coerce,matrix,lgTMatrix-method}
\alias{coerce,matrix,lsCMatrix-method}
\alias{coerce,matrix,ltCMatrix-method}
\alias{coerce,matrix,ltTMatrix-method}
%
\alias{crossprod,lgCMatrix,missing-method}
\alias{crossprod,lgTMatrix,missing-method}
\alias{crossprod,lsparseMatrix-method}
\alias{crossprod,lsparseMatrix,missing-method}
\alias{crossprod,lsparseMatrix,lsparseMatrix-method}
\alias{crossprod,lsparseMatrix,ldenseMatrix-method}
\alias{crossprod,ldenseMatrix,lsparseMatrix-method}
\alias{t,lgCMatrix-method}
\alias{t,lgTMatrix-method}
\alias{t,lsCMatrix-method}
\alias{t,ltCMatrix-method}
\alias{t,lsTMatrix-method}
\alias{t,ltTMatrix-method}
\alias{tcrossprod,lgCMatrix,missing-method}
\alias{tcrossprod,lgTMatrix,missing-method}
\alias{tcrossprod,lsparseMatrix,missing-method}
\alias{which,lgTMatrix-method}
\alias{which,lsparseMatrix-method}
\alias{which,lsparseVector-method}
\alias{which,lsTMatrix-method}
\alias{which,ltTMatrix-method}
%
\description{The \code{lsparseMatrix} class is a virtual class of sparse
  matrices with \code{TRUE}/\code{FALSE} entries.  Only the positions of the
  elements that are \code{TRUE} are stored.  These can be stored in the
  ``triplet'' form (classes \code{lgTMatrix}, \code{lsTMatrix}, and
  \code{ltTMatrix} which really contain pairs, not triplets) or in
  compressed column-oriented form (classes \code{lgCMatrix},
  \code{lsCMatrix}, and \code{ltCMatrix}) or in compressed row-oriented
  form (classes \code{lgRMatrix}, \code{lsRMatrix}, and
  \code{ltRMatrix}).  The second letter in the name of these non-virtual
  classes indicates \code{g}eneral, \code{s}ymmetric, or \code{t}riangular.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("lgCMatrix",
    ...)} and so on.  More frequently objects are created by coercion of
  a numeric sparse matrix to the logical form for use in
  the symbolic analysis phase
  of an algorithm involving sparse matrices.  Such algorithms often
  involve two phases: a symbolic phase wherein the positions of the
  non-zeros in the result are determined and a numeric phase wherein the
  actual results are calculated.  During the symbolic phase only the
  positions of the non-zero elements in any operands are of interest,
  hence any numeric sparse matrices can be treated as logical sparse
  matrices.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"logical"}, i.e., either
      \code{TRUE}, \code{\link{NA}}, or \code{FALSE}.}
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
    \item{coerce}{\code{signature(from = "dgCMatrix", to = "lgCMatrix")}}
    \item{t}{\code{signature(x = "lgCMatrix")}: returns the transpose
      of \code{x}}

    \item{which}{\code{signature(x = "lsparseMatrix")}, semantically
      equivalent to \pkg{base} function \code{\link{which}(x, arr.ind)};
      for details, see the \code{\linkS4class{lMatrix}} class documentation.}
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
(lm <- (m > 1)) # lgC
!lm     # no longer sparse
stopifnot(is(lm,"lsparseMatrix"),
          identical(!lm, m <= 1))

data(KNex)
str(mmG.1 <- (KNex $ mm) > 0.1)# "lgC..."
table(mmG.1@x)# however with many ``non-structural zeros''
## from logical to nz_pattern -- okay when there are no NA's :
nmG.1 <- as(mmG.1, "nMatrix") # <<< has "TRUE" also where mmG.1 had FALSE
## from logical to "double"
dmG.1 <- as(mmG.1, "dMatrix") # has '0' and back:
lmG.1 <- as(dmG.1, "lMatrix") # has no extra FALSE, i.e. drop0() included
stopifnot(identical(nmG.1, as((KNex $ mm) != 0,"nMatrix")),
          validObject(lmG.1), all(lmG.1@x),
          # same "logical" but lmG.1 has no 'FALSE' in x slot:
          all(lmG.1 == mmG.1))

class(xnx <- crossprod(nmG.1))# "nsC.."
class(xlx <- crossprod(mmG.1))# "dsC.." : numeric
is0 <- (xlx == 0)
mean(as.vector(is0))# 99.3\% zeros: quite sparse, but
table(xlx@x == 0)# more than half of the entries are (non-structural!) 0
stopifnot(isSymmetric(xlx), isSymmetric(xnx),
          ## compare xnx and xlx : have the *same* non-structural 0s :
          sapply(slotNames(xnx),
                 function(n) identical(slot(xnx, n), slot(xlx, n))))
}
\keyword{classes}
\keyword{algebra}
