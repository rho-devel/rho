\name{ldenseMatrix-class}
\docType{class}
\alias{ldenseMatrix-class}
% Group
\alias{!,ldenseMatrix-method}
\alias{Ops,ldenseMatrix,ldenseMatrix-method}
\alias{Logic,ldenseMatrix,lsparseMatrix-method}
\alias{Logic,lsparseMatrix,ldenseMatrix-method}
% purely "sparse" are in ./lsparseMatrix-classes.Rd
%
\alias{as.logical,ldenseMatrix-method}
\alias{as.vector,ldenseMatrix,missing-method}
\alias{coerce,matrix,ldenseMatrix-method}
\alias{coerce,ldenseMatrix,matrix-method}
\alias{diag,ldenseMatrix-method}
\alias{norm,ldenseMatrix,character-method}
\alias{which,ldenseMatrix-method}
%%-- These are already in ./lsparseMatrix-classes.Rd :
%\alias{\%*\%,lsparseMatrix,ldenseMatrix-method}
%\alias{\%*\%,ldenseMatrix,lsparseMatrix-method}
%\alias{crossprod,lsparseMatrix,ldenseMatrix-method}
%\alias{crossprod,ldenseMatrix,lsparseMatrix-method}
%
\title{Virtual Class "ldenseMatrix" of Dense Logical Matrices}
\description{
  \code{ldenseMatrix} is the virtual class of all dense \bold{l}ogical
  (S4) matrices.  It extends both \code{\linkS4class{denseMatrix}}
  and \code{\linkS4class{lMatrix}} directly.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{logical vector containing the entries of the matrix.}
    \item{\code{Dim}, \code{Dimnames}:}{see \code{\linkS4class{Matrix}}.}
  }
}
\section{Extends}{
Class \code{"lMatrix"}, directly.
Class \code{"denseMatrix"}, directly.
Class \code{"Matrix"}, by class \code{"lMatrix"}.
Class \code{"Matrix"}, by class \code{"denseMatrix"}.
}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature(x = "lsparseMatrix", y = "ldenseMatrix")}: ... }
    \item{\%*\%}{\code{signature(x = "ldenseMatrix", y = "lsparseMatrix")}: ... }
    \item{coerce}{\code{signature(from = "matrix", to = "ldenseMatrix")}: ... }
    \item{coerce}{\code{signature(from = "ldenseMatrix", to = "matrix")}: ... }
    \item{crossprod}{\code{signature(x = "lsparseMatrix", y = "ldenseMatrix")}: ... }
    \item{crossprod}{\code{signature(x = "ldenseMatrix", y = "lsparseMatrix")}: ... }
    \item{as.vector}{\code{signature(x = "ldenseMatrix", mode = "missing")}: ...}

    \item{which}{\code{signature(x = "ndenseMatrix")}, semantically
      equivalent to \pkg{base} function \code{\link{which}(x, arr.ind)};
      for details, see the \code{\linkS4class{lMatrix}} class documentation.}
  }
}
\seealso{
 Class \code{\linkS4class{lgeMatrix}} and the other subclasses.
}
\examples{
showClass("ldenseMatrix")

as(diag(3) > 0, "ldenseMatrix")
}
\keyword{classes}
