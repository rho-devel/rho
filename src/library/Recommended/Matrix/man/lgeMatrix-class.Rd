\name{lgeMatrix-class}
\docType{class}
\title{Class "lgeMatrix" of General Dense Logical Matrices}
\alias{lgeMatrix-class}
%
\alias{!,lgeMatrix-method}
\alias{Arith,lgeMatrix,lgeMatrix-method}
\alias{Compare,lgeMatrix,lgeMatrix-method}
\alias{Logic,lgeMatrix,lgeMatrix-method}
\alias{as.vector,lgeMatrix,missing-method}
\alias{coerce,matrix,lgeMatrix-method}
\alias{coerce,lgeMatrix,dgeMatrix-method}
\alias{coerce,lgeMatrix,matrix-method}
\alias{coerce,lgeMatrix,lgCMatrix-method}
\alias{coerce,lgeMatrix,lgTMatrix-method}
\alias{coerce,lgeMatrix,lsyMatrix-method}
\alias{coerce,lgeMatrix,ltrMatrix-method}
\alias{coerce,lgeMatrix,ltpMatrix-method}
\alias{coerce,lgeMatrix,lspMatrix-method}
\alias{diag,lgeMatrix-method}
\alias{diag<-,lgeMatrix-method}
\alias{t,lgeMatrix-method}
%
\description{This is the class of general dense \code{\link{logical}}
  matrices.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"logical"}. The logical
      values that constitute the matrix, stored in column-major order.}
    \item{\code{Dim},\code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}), see the
      \code{\linkS4class{Matrix}} class.}
    \item{\code{factors}:}{Object of class \code{"list"}.  A named
      list of factorizations that have been computed for the matrix.}
  }
}
\section{Extends}{
Class \code{"ldenseMatrix"}, directly.
Class \code{"lMatrix"}, by class \code{"ldenseMatrix"}.
Class \code{"denseMatrix"}, by class \code{"ldenseMatrix"}.
Class \code{"Matrix"}, by class \code{"ldenseMatrix"}.
Class \code{"Matrix"}, by class \code{"ldenseMatrix"}.
}
\section{Methods}{
  Currently, mainly \code{\link{t}()} and coercion methods (for
  \code{\link{as}(.)}); use, e.g.,
  \code{\link{showMethods}(class="lgeMatrix")} for details.
}
\seealso{
  Non-general logical dense matrix classes such as
  \code{\linkS4class{ltrMatrix}}, or \code{\linkS4class{lsyMatrix}};
  \emph{sparse} logical classes such as \code{\linkS4class{lgCMatrix}}.
}
\examples{
showClass("lgeMatrix")
str(new("lgeMatrix"))
set.seed(1)
(lM <- Matrix(matrix(rnorm(28), 4,7) > 0))# a simple random lgeMatrix
set.seed(11)
(lC <- Matrix(matrix(rnorm(28), 4,7) > 0))# a simple random lgCMatrix
as(lM, "lgCMatrix")
}
\keyword{classes}
