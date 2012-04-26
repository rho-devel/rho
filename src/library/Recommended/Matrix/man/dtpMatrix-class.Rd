\name{dtpMatrix-class}
\docType{class}
\alias{dtpMatrix-class}
\alias{\%*\%,dtpMatrix,ddenseMatrix-method}
\alias{\%*\%,dgeMatrix,dtpMatrix-method}
\alias{\%*\%,dtpMatrix,matrix-method}
\alias{\%*\%,matrix,dtpMatrix-method}
\alias{coerce,dtpMatrix,dtTMatrix-method}
\alias{coerce,dtpMatrix,dtrMatrix-method}
\alias{coerce,dtpMatrix,ltpMatrix-method}
\alias{coerce,dtpMatrix,matrix-method}
\alias{coerce,matrix,dtpMatrix-method}
\alias{determinant,dtpMatrix,missing-method}
\alias{determinant,dtpMatrix,logical-method}
\alias{diag,dtpMatrix-method}
\alias{norm,dtpMatrix,character-method}
\alias{norm,dtpMatrix,missing-method}
\alias{rcond,dtpMatrix,character-method}
\alias{rcond,dtpMatrix,missing-method}
\alias{solve,dtpMatrix,missing-method}
\alias{solve,dtpMatrix,matrix-method}
\alias{solve,dtpMatrix,ddenseMatrix-method}
\alias{t,dtpMatrix-method}
\alias{unpack,dtpMatrix-method}
\title{Packed triangular dense matrices}
\description{The \code{"dtpMatrix"} class is the class of triangular,
  dense, numeric matrices in packed storage.  The \code{"dtrMatrix"}
  class is the same except in nonpacked storage.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dtpMatrix",
    ...)} or by coercion from other classes of matrices.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.}
    \item{\code{diag}:}{Object of class \code{"character"}. Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"}; see \code{\linkS4class{triangularMatrix}}.}
    \item{\code{x}:}{Object of class \code{"numeric"}. The numeric
      values that constitute the matrix, stored in column-major order.
      For a packed square matrix of dimension \eqn{d \times d}{d * d},
      \code{length(x)} is of length \eqn{d(d+1)/2} (also when
      \code{diag == "U"}!).}
    \item{\code{Dim},\code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}),
      inherited from the \code{\linkS4class{Matrix}}, see there.}
  }
}
\section{Extends}{
  Class \code{"ddenseMatrix"}, directly.
  Class \code{"triangularMatrix"}, directly.
  Class \code{"dMatrix"} and more by class \code{"ddenseMatrix"} etc, see
  the examples.
}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature(x = "dtpMatrix", y = "dgeMatrix")}:
      Matrix multiplication; ditto for several other signature
      combinations, see \code{showMethods("\%*\%", class = "dtpMatrix")}.}
    \item{coerce}{\code{signature(from = "dtpMatrix", to = "dtrMatrix")}}
    \item{coerce}{\code{signature(from = "dtpMatrix", to = "matrix")}}
    \item{determinant}{\code{signature(x = "dtpMatrix", logarithm = "missing")}: ... }
    \item{determinant}{\code{signature(x = "dtpMatrix", logarithm = "logical")}: ... }
    \item{diag}{\code{signature(x = "dtpMatrix")}: ... }
    \item{norm}{\code{signature(x = "dtpMatrix", type = "character")}: ... }
    \item{norm}{\code{signature(x = "dtpMatrix", type = "missing")}: ... }
    \item{rcond}{\code{signature(x = "dtpMatrix", norm = "character")}: ... }
    \item{rcond}{\code{signature(x = "dtpMatrix", norm = "missing")}: ... }
    \item{solve}{\code{signature(a = "dtpMatrix", b = "missing")}: ... }
    \item{solve}{\code{signature(a = "dtpMatrix", b = "matrix")}: ... }
    \item{t}{\code{signature(x = "dtpMatrix")}: ... }
    \item{unpack}{\code{signature(x = "dtpMatrix")}: ... }
  }
}
\seealso{
  Class \code{\linkS4class{dtrMatrix}}
}
\examples{
showClass("dtrMatrix")

example("dtrMatrix-class")
(p1 <- as(T2, "dtpMatrix"))
str(p1)
(pp <- as(T, "dtpMatrix"))
stopifnot(length(p1@x) == 3, length(pp@x) == 3,
          p1 @ uplo == T2 @ uplo, pp @ uplo == T @ uplo)
}
\keyword{classes}
