\name{dgCMatrix-class}
\docType{class}
\title{Compressed, sparse, column-oriented numeric matrices}
\alias{dgCMatrix-class}
\alias{\%*\%,dgCMatrix,dgeMatrix-method}
\alias{\%*\%,dgCMatrix,matrix-method}
\alias{\%*\%,numeric,dgCMatrix-method}
\alias{\%*\%,ddenseMatrix,dgCMatrix-method}
\alias{\%*\%,dgeMatrix,dgCMatrix-method}
\alias{\%*\%,matrix,dgCMatrix-method}
\alias{coerce,matrix,dgCMatrix-method}
\alias{coerce,dgeMatrix,dgCMatrix-method}
\alias{coerce,dgCMatrix,dgTMatrix-method}
\alias{coerce,dgCMatrix,dsCMatrix-method}% deprecated
\alias{coerce,dgCMatrix,dtCMatrix-method}
\alias{coerce,dgCMatrix,lgCMatrix-method}
\alias{coerce,dgCMatrix,ngCMatrix-method}
\alias{coerce,dgCMatrix,matrix-method}
\alias{coerce,dgCMatrix,dgeMatrix-method}
\alias{coerce,factor,dgCMatrix-method}
\alias{crossprod,dgCMatrix,missing-method}
\alias{crossprod,dgCMatrix,matrix-method}
\alias{crossprod,dgCMatrix,dgeMatrix-method}
\alias{diag,dgCMatrix-method}
\alias{dim,dgCMatrix-method}
\alias{lu,dgCMatrix-method}
\alias{t,dgCMatrix-method}
\alias{solve,dgCMatrix,matrix-method}
\alias{solve,dgCMatrix,missing-method}
\alias{solve,dgCMatrix,ddenseMatrix-method}
\alias{solve,dgCMatrix,dsparseMatrix-method}
%% Group methods --------- FIXME: These are not tested yet (or documented)
\alias{Arith,numeric,dgCMatrix-method}
\alias{Arith,dgCMatrix,numeric-method}
\alias{Arith,dgCMatrix,dgCMatrix-method}
%\alias{Math2,dgCMatrix,numeric-method}
\alias{Math,dgCMatrix-method}
% for silly reasons, need these 2+3 as well:
\alias{round,dgCMatrix,numeric-method}
\alias{signif,dgCMatrix,numeric-method}
\alias{log,dgCMatrix-method}
\alias{gamma,dgCMatrix-method}
\alias{lgamma,dgCMatrix-method}
%
\description{The \code{dgCMatrix} class is a class of sparse numeric
  matrices in the compressed, sparse, column-oriented format.  In this
  implementation the non-zero elements in the columns are sorted into
  increasing row order.  \code{dgCMatrix} is the
  \emph{\dQuote{standard}} class for sparse numeric matrices in the
  \pkg{Matrix} package.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("dgCMatrix", ...)} or
  often more easily via \code{\link{Matrix}(*, sparse = TRUE)}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"numeric"} - the non-zero
      elements of the matrix.}
    \item{\dots}{all other slots are inherited from the superclass
      \code{"\linkS4class{CsparseMatrix}"}.
    }
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "dgCMatrix")}}
    \item{coerce}{\code{signature(from = "dgCMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "dgCMatrix", to = "dgTMatrix")}}
    \item{crossprod}{\code{signature(x = "dgCMatrix", y = "missing")}
      returns \code{t(x) \%*\% x} as an \code{dsCMatrix} object.}
    \item{crossprod}{\code{signature(x = "dgCMatrix", y = "matrix")}: ... }
    \item{crossprod}{\code{signature(x = "dgCMatrix", y = "numeric")}: ... }
    \item{diag}{\code{signature(x = "dgCMatrix")}: returns the diagonal
      of \code{x}}
    \item{dim}{\code{signature(x = "dgCMatrix")}: returns the dimensions
      of \code{x}}
    \item{image}{\code{signature(x = "dgCMatrix")}: plots an image of
      \code{x} using the \code{\link[lattice]{levelplot}} function}
    \item{lu}{\code{signature(x = "dgCMatrix")}: computes the LU
      decomposition of a square \code{dgCMatrix} object}
  }
}
%\references{}
%\author{}
%\note{}
\seealso{
  Classes \code{\linkS4class{dsCMatrix}},
  \code{\linkS4class{dtCMatrix}}, \code{\link{lu}}
}
\examples{
(m <- Matrix(c(0,0,2:0), 3,5))
str(m)
m[,1]
\dontshow{## regression test: this must give a validity-check error:
stopifnot(inherits(try(new("dgCMatrix", i = 0:1, p = 0:2,
                           x = c(2,3), Dim = 3:4)),
          "try-error"))
}
}
\keyword{classes}
\keyword{algebra}
