\name{dtrMatrix-class}
\title{Triangular, dense, numeric matrices}
\docType{class}
\alias{\%*\%,dtrMatrix,dtrMatrix-method}
\alias{\%*\%,ddenseMatrix,dtrMatrix-method}
\alias{\%*\%,dtrMatrix,ddenseMatrix-method}
\alias{\%*\%,dtrMatrix,matrix-method}
\alias{\%*\%,matrix,dtrMatrix-method}
\alias{coerce,dgeMatrix,dtrMatrix-method}% or rather setIs?
\alias{coerce,dtrMatrix,dtpMatrix-method}
\alias{coerce,dtrMatrix,ltrMatrix-method}
\alias{coerce,dtrMatrix,matrix-method}
\alias{coerce,dtrMatrix,sparseMatrix-method}
\alias{coerce,dtrMatrix,CsparseMatrix-method}
\alias{coerce,matrix,dtrMatrix-method}
\alias{crossprod,dtrMatrix,ddenseMatrix-method}
\alias{crossprod,dtrMatrix,dtrMatrix-method}
\alias{crossprod,dtrMatrix,matrix-method}
\alias{crossprod,dtrMatrix,missing-method}
\alias{tcrossprod,ddenseMatrix,dtrMatrix-method}
\alias{tcrossprod,dtrMatrix,dtrMatrix-method}
\alias{tcrossprod,matrix,dtrMatrix-method}
\alias{determinant,dtrMatrix,logical-method}
\alias{determinant,dtrMatrix,missing-method}
\alias{diag,dtrMatrix-method}
\alias{norm,dtrMatrix,character-method}
\alias{norm,dtrMatrix,missing-method}
\alias{rcond,dtrMatrix,character-method}
\alias{rcond,dtrMatrix,missing-method}
\alias{show,dtrMatrix-method}
\alias{solve,dtrMatrix,ddenseMatrix-method}
\alias{solve,dtrMatrix,dMatrix-method}
\alias{solve,dtrMatrix,Matrix-method}
\alias{solve,dtrMatrix,matrix-method}
\alias{solve,dtrMatrix,missing-method}
\alias{t,dtrMatrix-method}
\alias{dtrMatrix-class}
\description{
  The \code{"dtrMatrix"} class is the class of triangular, dense,
  numeric matrices in nonpacked storage.  The \code{"dtpMatrix"} class
  is the same except in packed storage.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dtrMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.}
    \item{\code{diag}:}{Object of class \code{"character"}. Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"}; see \code{\linkS4class{triangularMatrix}}.}
    \item{\code{x}:}{Object of class \code{"numeric"}. The numeric
      values that constitute the matrix, stored in column-major order.}
    \item{\code{Dim}:}{Object of class \code{"integer"}. The dimensions
      of the matrix which must be a two-element vector of non-negative
      integers.}
  }
}
\section{Extends}{
  Class \code{"ddenseMatrix"}, directly.
  Class \code{"triangularMatrix"}, directly.
  Class \code{"Matrix"} and others, by class \code{"ddenseMatrix"}.
}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature(x = "dtrMatrix", y = "matrix")} and other
      signatures (use \code{showMethods("\%*\%", class="dtrMatrix")}):
      matrix multiplication.  Multiplication of (matching) triangular
      matrices now should remain triangular (in the sense of class
      \linkS4class{triangularMatrix}).}
    \item{crossprod,tcrossprod}{\code{signature(x = "dtrMatrix", y = "matrix")} and other
      signatures, see \code{"\%*\%"} above.}
    \item{coerce}{\code{signature(from = "dgeMatrix", to = "dtrMatrix")}}
    \item{coerce}{\code{signature(from = "dtrMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "dtrMatrix", to = "ltrMatrix")}}
    \item{coerce}{\code{signature(from = "dtrMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "matrix",    to = "dtrMatrix")}}

    \item{norm}{\code{signature(x = "dtrMatrix", type = "character")}}
    \item{rcond}{\code{signature(x = "dtrMatrix", norm = "character")}}
    \item{solve}{\code{signature(a = "dtrMatrix", b = "missing")}}
    \item{solve}{\code{signature(a = "dtrMatrix", b = "matrix")}}
  }
}
%\references{}
%\author{}
\seealso{
  Classes \code{\linkS4class{ddenseMatrix}}, \code{\linkS4class{dtpMatrix}},
  \code{\linkS4class{triangularMatrix}}
}
\examples{%% this is used from ./dtpMatrix-class.Rd (change with care!)
(m <- rbind(2:3, 0:-1))
(M <- as(m, "dgeMatrix"))

(T <- as(M, "dtrMatrix")) ## upper triangular is default
(T2 <- as(t(M), "dtrMatrix"))
stopifnot(T@uplo == "U", T2@uplo == "L", identical(T2, t(T)))
}
\keyword{classes}
