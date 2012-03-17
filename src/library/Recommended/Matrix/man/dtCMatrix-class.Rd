\name{dtCMatrix-class}
\title{Triangular, (compressed) sparse column matrices}
\docType{class}
\alias{dtCMatrix-class}% C  and
\alias{dtTMatrix-class}% T
%
\alias{coerce,dtCMatrix,dgCMatrix-method}
\alias{coerce,dtCMatrix,dgeMatrix-method}
\alias{coerce,dtCMatrix,dgTMatrix-method}
\alias{coerce,dtCMatrix,dsCMatrix-method}
\alias{coerce,dtCMatrix,dtTMatrix-method}
\alias{coerce,dtCMatrix,dtrMatrix-method}
\alias{coerce,dtCMatrix,ltCMatrix-method}
\alias{coerce,dtCMatrix,ntCMatrix-method}
\alias{coerce,dtCMatrix,TsparseMatrix-method}
\alias{coerce,dtTMatrix,dgTMatrix-method}
\alias{coerce,dtTMatrix,dgeMatrix-method}
\alias{coerce,dtTMatrix,dtCMatrix-method}
\alias{coerce,dtTMatrix,dtrMatrix-method}
%no longer\alias{coerce,dtTMatrix,ltTMatrix-method}
%no longer\alias{coerce,dtTMatrix,ntTMatrix-method}
\alias{coerce,dtTMatrix,generalMatrix-method}
\alias{coerce,dtTMatrix,matrix-method}
\alias{coerce,matrix,dtCMatrix-method}
\alias{coerce,matrix,dtTMatrix-method}
%% Group methods
\alias{Arith,dtCMatrix,dtCMatrix-method}
%
\alias{solve,dtCMatrix,matrix-method}
\alias{solve,dtCMatrix,CsparseMatrix-method}
\alias{solve,dtCMatrix,dgeMatrix-method}
\alias{solve,dtCMatrix,missing-method}
\alias{solve,dtCMatrix,numeric-method}
\alias{t,dtCMatrix-method}
\alias{t,dtTMatrix-method}
\description{The \code{"dtCMatrix"} class is a class of triangular, sparse
  matrices in the compressed, column-oriented format.  In this
  implementation the non-zero elements in the columns are sorted into
  increasing row order.  The \code{"dtTMatrix"} class is a class of
  triangular, sparse matrices in triplet format.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dtCMatrix",
    ...)} or calls of the form  \code{new("dtTMatrix", ...)}, but more
  typically automatically via \code{\link{Matrix}()} or coercion such as
  \code{as(x, "dtCMatrix")}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.}
    \item{\code{diag}:}{Object of class \code{"character"}. Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"}; see \code{\linkS4class{triangularMatrix}}.}
    \item{\code{p}:}{(only present in \code{"dtCMatrix"}:) an
      \code{\link{integer}} vector for providing pointers, one for each
      column, see the detailed description in \code{\linkS4class{CsparseMatrix}}.}
    \item{\code{i}:}{Object of class \code{"integer"} of length nnzero
      (number of non-zero elements).  These are the row numbers for
      each non-zero element in the matrix.}
    \item{\code{j}:}{Object of class \code{"integer"} of length nnzero
      (number of non-zero elements).  These are the column numbers for
      each non-zero element in the matrix. (Only present in the
      \code{dtTMatrix} class.)}
    \item{\code{x}:}{Object of class \code{"numeric"} - the non-zero
      elements of the matrix.}
    \item{\code{Dim},\code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}),
      inherited from the \code{\linkS4class{Matrix}}, see there.}
  }
}
\section{Extends}{
  Class \code{"dgCMatrix"}, directly.
  Class \code{"triangularMatrix"}, directly.
  Class \code{"dMatrix"}, \code{"sparseMatrix"}, and more by class
  \code{"dgCMatrix"} etc, see the examples.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "dtCMatrix", to = "dgTMatrix")}}
    \item{coerce}{\code{signature(from = "dtCMatrix", to = "dgeMatrix")}}
    \item{coerce}{\code{signature(from = "dtTMatrix", to = "dgeMatrix")}}
    \item{coerce}{\code{signature(from = "dtTMatrix", to = "dtrMatrix")}}
    \item{coerce}{\code{signature(from = "dtTMatrix", to = "matrix")}}
    \item{solve}{\code{signature(a = "dtCMatrix", b = "matrix")}}
    \item{solve}{\code{signature(a = "dtCMatrix", b = "dgeMatrix")}}
    \item{solve}{\code{signature(a = "dtCMatrix", b = "missing")}}
    \item{solve}{\code{signature(a = "dtCMatrix", b = "numeric")}}
    \item{solve}{\code{signature(a = "dtCMatrix", b = "CsparseMatrix")}:
      fully sparse triangular solve, in traditional S/\R\ aka
      \code{\link{backsolve}}.}
    \item{t}{\code{signature(x = "dtCMatrix")}: returns the transpose of
      \code{x}}
    \item{t}{\code{signature(x = "dtTMatrix")}: returns the transpose of
      \code{x}}
  }
}
%\references{}
%\author{}
%\note{}
\seealso{
  Classes \code{\linkS4class{dgCMatrix}}, \code{\linkS4class{dgTMatrix}},
  \code{\linkS4class{dgeMatrix}}, and \code{\linkS4class{dtrMatrix}}.
}
\examples{
showClass("dtCMatrix")

showClass("dtTMatrix")
t1 <- new("dtTMatrix", x= c(3,7), i= 0:1, j=3:2, Dim= as.integer(c(4,4)))
t1
## from  0-diagonal to unit-diagonal {low-level step}:
tu <- t1 ; tu@diag <- "U"
tu
(cu <- as(tu, "dtCMatrix"))
str(cu)# only two entries in @i and @x
stopifnot(cu@i == 1:0,
          all(2 * symmpart(cu) == Diagonal(4) + forceSymmetric(cu)))

t1[1,2:3] <- -1:-2
diag(t1) <- 10*c(1:2,3:2)
t1 # still triangular
(it1 <- solve(t1))
t1. <- solve(it1)
all(abs(t1 - t1.) < 10 * .Machine$double.eps)
}
\keyword{classes}
\keyword{algebra}
