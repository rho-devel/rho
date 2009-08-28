\name{dgeMatrix-class}
\title{Class "dgeMatrix" of Dense Numeric (S4 Class) Matrices}
\docType{class}
\alias{dgeMatrix-class}
% METHODS:
% Group ones
\alias{Arith,dgeMatrix,dgeMatrix-method}
\alias{Arith,dgeMatrix,numeric-method}
\alias{Arith,numeric,dgeMatrix-method}
\alias{Math,dgeMatrix-method}
%\alias{Math2,dgeMatrix,numeric-method}
% Others
%
\alias{\%*\%,dgeMatrix,dgeMatrix-method}
\alias{\%*\%,dgeMatrix,matrix-method}
\alias{\%*\%,dgeMatrix,numeric-method}
\alias{\%*\%,matrix,dgeMatrix-method}
\alias{\%*\%,numeric,dgeMatrix-method}
\alias{as.vector,dgeMatrix,missing-method}
\alias{coerce,dgeMatrix,matrix-method}
\alias{coerce,dgeMatrix,triangularMatrix-method}
\alias{coerce,dgeMatrix,lgeMatrix-method}
\alias{coerce,matrix,dgeMatrix-method}
\alias{colMeans,dgeMatrix-method}
\alias{colSums,dgeMatrix-method}
\alias{crossprod,dgeMatrix,dgeMatrix-method}
\alias{crossprod,dgeMatrix,matrix-method}
\alias{crossprod,dgeMatrix,numeric-method}
\alias{crossprod,dgeMatrix,missing-method}
\alias{crossprod,matrix,dgeMatrix-method}
\alias{crossprod,numeric,dgeMatrix-method}
\alias{determinant,dgeMatrix,logical-method}
\alias{determinant,dgeMatrix,missing-method}
\alias{diag,dgeMatrix-method}
\alias{dim,dgeMatrix-method}
\alias{dimnames,dgeMatrix-method}
\alias{eigen,dgeMatrix,missing-method}
\alias{eigen,dgeMatrix,logical-method}
\alias{norm,dgeMatrix,missing-method}
\alias{norm,dgeMatrix,character-method}
\alias{rcond,dgeMatrix,missing-method}
\alias{rcond,dgeMatrix,character-method}
\alias{rowMeans,dgeMatrix-method}
\alias{rowSums,dgeMatrix-method}
\alias{solve,dgeMatrix,matrix-method}
\alias{solve,dgeMatrix,ddenseMatrix-method}
\alias{solve,dgeMatrix,sparseMatrix-method}
\alias{solve,dgeMatrix,missing-method}
\alias{t,dgeMatrix-method}
\alias{tcrossprod,dgeMatrix,missing-method}
\alias{tcrossprod,dgeMatrix,dgeMatrix-method}
\alias{tcrossprod,dgeMatrix,matrix-method}
\alias{tcrossprod,dgeMatrix,numeric-method}
\alias{tcrossprod,matrix,dgeMatrix-method}
\alias{tcrossprod,numeric,dgeMatrix-method}
%
\description{A general numeric dense matrix in the S4 Matrix
  representation.  \code{dgeMatrix} is the \emph{\dQuote{standard}}
  class for dense numeric matrices in the \pkg{Matrix} package.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dgeMatrix", ...)}
  or, more commonly, by coercion from the \code{Matrix} class (see
  \linkS4class{Matrix}) or by \code{\link{Matrix}(..)}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"numeric"} - the numeric
      values contained in the matrix, in column-major order.}
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
     of the matrix - must be an integer vector with exactly two
     non-negative values.}
    \item{\code{Dimnames}:}{a list of length two - inherited from class
      \code{\linkS4class{Matrix}}.}
    \item{\code{factors}:}{Object of class \code{"list"} - a list
      of factorizations of the matrix.}
  }
}
\section{Methods}{
  The are group methods (see, e.g., \code{\link{Arith}})
  \describe{
    \item{Arith}{\code{signature(e1 = "dgeMatrix", e2 = "dgeMatrix")}: ... }
    \item{Arith}{\code{signature(e1 = "dgeMatrix", e2 = "numeric")}: ... }
    \item{Arith}{\code{signature(e1 = "numeric", e2 = "dgeMatrix")}: ... }
    \item{Math}{\code{signature(x = "dgeMatrix")}: ... }
    \item{Math2}{\code{signature(x = "dgeMatrix", digits = "numeric")}: ...}
  }
  and other matrix methods available:
  \describe{
    \item{\%*\%}{\code{signature(x = "dgeMatrix", y = "dgeMatrix")}:
      Matrix multiplication; ditto for several other signature
      combinations, see \code{showMethods("\%*\%", class = "dgeMatrix")}.}
    \item{Schur}{\code{signature(x = "dgeMatrix", vectors = "logical")}: ... }
    \item{Schur}{\code{signature(x = "dgeMatrix", vectors = "missing")}: ... }
    \item{chol}{\code{signature(x = "dgeMatrix")}: see \code{\link{chol}}.}
    \item{coerce}{\code{signature(from = "dgeMatrix", to = "lgeMatrix")}: ... }
    \item{coerce}{\code{signature(from = "dgeMatrix", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "matrix", to = "dgeMatrix")}: ... }
    \item{colMeans}{\code{signature(x = "dgeMatrix")}: columnwise means (averages)}
    \item{colSums}{\code{signature(x = "dgeMatrix")}: columnwise sums}
    \item{crossprod}{\code{signature(x = "dgeMatrix", y = "dgeMatrix")}:
      ditto for several other signatures, use
      \code{showMethods("crossprod", class = "dgeMatrix")}, matrix
      crossproduct, an efficient version of \code{t(x) \%*\% y}.}
    \item{diag}{\code{signature(x = "dgeMatrix")}: ... }
    \item{dim}{\code{signature(x = "dgeMatrix")}: ... }
    \item{dimnames}{\code{signature(x = "dgeMatrix")}: ... }
    \item{eigen}{\code{signature(x = "dgeMatrix", only.values= "logical")}: ...}
    \item{eigen}{\code{signature(x = "dgeMatrix", only.values= "missing")}: ...}
    \item{norm}{\code{signature(x = "dgeMatrix", type = "character")}: ... }
    \item{norm}{\code{signature(x = "dgeMatrix", type = "missing")}: ... }
    \item{rcond}{\code{signature(x = "dgeMatrix", norm = "character")}: ... }
    \item{rcond}{\code{signature(x = "dgeMatrix", norm = "missing")}: ... }
    \item{rowMeans}{\code{signature(x = "dgeMatrix")}: rowwise means (averages)}
    \item{rowSums}{\code{signature(x = "dgeMatrix")}: rowwise sums}
    \item{solve}{\code{signature(a = "dgeMatrix", b = "dgeMatrix")}: ... }
    \item{solve}{\code{signature(a = "dgeMatrix", b = "missing")}: ... }
    \item{t}{\code{signature(x = "dgeMatrix")}: ... }
  }
}
%\references{}
%\author{}
%\note{}
\seealso{
  Classes \code{\linkS4class{Matrix}},
  \code{\linkS4class{dtrMatrix}}, and \code{\linkS4class{dsyMatrix}}.

}
%\examples{}
\keyword{classes}
\keyword{algebra}
