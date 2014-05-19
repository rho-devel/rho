\name{dtpMatrix-class}
\title{Packed Triangular Dense Matrices - "dtpMatrix"}
\docType{class}
\alias{dtpMatrix-class}
\alias{crossprod,dtpMatrix,ddenseMatrix-method}
\alias{crossprod,dtpMatrix,matrix-method}
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
\alias{diag<-,dtpMatrix-method}
\alias{norm,dtpMatrix,character-method}
\alias{norm,dtpMatrix,missing-method}
\alias{rcond,dtpMatrix,character-method}
\alias{rcond,dtpMatrix,missing-method}
%\alias{solve,dtpMatrix,...-method}%--> solve-methods.Rd
\alias{t,dtpMatrix-method}
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
    \item{determinant}{\code{signature(x = "dtpMatrix", logarithm = "logical")}:
      the \code{\link{determinant}(x)} trivially is
      \code{prod(diag(x))}, but computed on log scale to prevent over-
      and underflow.}
    \item{diag}{\code{signature(x = "dtpMatrix")}: ... }
    \item{norm}{\code{signature(x = "dtpMatrix", type = "character")}: ... }
    \item{rcond}{\code{signature(x = "dtpMatrix", norm = "character")}: ... }
    \item{solve}{\code{signature(a = "dtpMatrix", b = "...")}:
      efficiently using internal backsolve or forwardsolve, see
      \code{\link{solve-methods}}.}
    \item{t}{\code{signature(x = "dtpMatrix")}: \code{t(x)} remains
      a \code{"dtpMatrix"}, lower triangular if \code{x} is upper
      triangular, and vice versa.}
  }
}
\seealso{
  Class \code{\linkS4class{dtrMatrix}}
}
\examples{
showClass("dtrMatrix")

example("dtrMatrix-class", echo=FALSE)
(p1 <- as(T2, "dtpMatrix"))
str(p1)
(pp <- as(T, "dtpMatrix"))
ip1 <- solve(p1)
stopifnot(length(p1@x) == 3, length(pp@x) == 3,
          p1 @ uplo == T2 @ uplo, pp @ uplo == T @ uplo,
	  identical(t(pp), p1), identical(t(p1), pp),
	  all((l.d <- p1 - T2) == 0), is(l.d, "dtpMatrix"),
	  all((u.d <- pp - T ) == 0), is(u.d, "dtpMatrix"),
	  l.d@uplo == T2@uplo, u.d@uplo == T@uplo,
	  identical(t(ip1), solve(pp)), is(ip1, "dtpMatrix"),
	  all.equal(as(solve(p1,p1), "diagonalMatrix"), Diagonal(2)))
}
\keyword{classes}
